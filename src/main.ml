open Import
open Future

type setup =
  { build_system : Build_system.t
  ; stanzas      : (Path.t * Jbuild_types.Pkgs.t * Jbuild_types.Stanzas.t) list String_map.t
  ; contexts     : Context.t list
  ; packages     : Package.t String_map.t
  }

let package_install_file { packages; _ } pkg =
  match String_map.find pkg packages with
  | None -> Error ()
  | Some p -> Ok (Path.relative p.path (p.name ^ ".install"))

let setup ?(log=Log.no_log) ?filter_out_optional_stanzas_with_missing_deps
      ?workspace ?(workspace_file="jbuild-workspace")
      ?(use_findlib=true)
      ?only_packages
      ?extra_ignored_subtrees
      () =
  let conf = Jbuild_load.load ?extra_ignored_subtrees () in
  Option.iter only_packages ~f:(fun set ->
    String_set.iter set ~f:(fun pkg ->
      if not (String_map.mem pkg conf.packages) then
        die "@{<error>Error@}: I don't know about package %s \
             (passed through --only-packages/--release)%s"
          pkg (hint pkg (String_map.keys conf.packages))));
  let workspace =
    match workspace with
    | Some w -> w
    | None ->
      if Sys.file_exists workspace_file then
        Workspace.load workspace_file
      else
        { merlin_context = Some "default"; contexts = [Default] }
  in
  Future.all
    (List.map workspace.contexts ~f:(function
     | Workspace.Context.Default ->
       Context.default ~merlin:(workspace.merlin_context = Some "default")
         ~use_findlib ()
     | Opam { name; switch; root; merlin } ->
       Context.create_for_opam ~name ~switch ?root ~merlin ()))
  >>= fun contexts ->
  List.iter contexts ~f:(fun ctx ->
    Log.infof log "@[<1>Jbuilder context:@,%a@]@." Sexp.pp (Context.sexp_of_t ctx));
  Gen_rules.gen conf ~contexts
    ?only_packages
    ?filter_out_optional_stanzas_with_missing_deps
  >>= fun (rules, stanzas) ->
  let build_system = Build_system.create ~contexts ~file_tree:conf.file_tree ~rules in
  return { build_system
         ; stanzas
         ; contexts
         ; packages = conf.packages
         }

let external_lib_deps ?log ~packages () =
  Future.Scheduler.go ?log
    (setup () ~filter_out_optional_stanzas_with_missing_deps:false
     >>| fun setup ->
     let install_files =
       List.map packages ~f:(fun pkg ->
         match package_install_file setup pkg with
         | Ok path -> path
         | Error () -> die "Unknown package %S" pkg)
     in
     match String_map.find "default" setup.stanzas with
     | None -> die "You need to set a default context to use external-lib-deps"
     | Some stanzas ->
       let internals = Jbuild_types.Stanzas.lib_names stanzas in
       Path.Map.map
         (Build_system.all_lib_deps setup.build_system install_files)
         ~f:(String_map.filter ~f:(fun name _ ->
           not (String_set.mem name internals))))

let report_error ?(map_fname=fun x->x) ppf exn ~backtrace =
  match exn with
  | Loc.Error ({ start; stop }, msg) ->
    let start_c = start.pos_cnum - start.pos_bol in
    let stop_c  = stop.pos_cnum  - start.pos_bol in
    Format.fprintf ppf
      "@{<loc>File \"%s\", line %d, characters %d-%d:@}\n\
       @{<error>Error@}: %s\n"
      (map_fname start.pos_fname) start.pos_lnum start_c stop_c msg
  | Fatal_error "" -> ()
  | Fatal_error msg ->
    Format.fprintf ppf "%s\n" (String.capitalize_ascii msg)
  | Findlib.Package_not_found { package; required_by } ->
    Format.fprintf ppf
      "@{<error>Error@}: External library %S not found.\n" package;
    List.iter required_by ~f:(Format.fprintf ppf "-> required by %S\n");
    let cmdline_suggestion =
      (* CR-someday jdimino: this is ugly *)
      match Array.to_list Sys.argv with
      | prog :: "build" :: args ->
        prog :: "external-lib-deps" :: "--missing" :: args
      | _ ->
        ["jbuilder"; "external-lib-deps"; "--missing"]
    in
    Format.fprintf ppf
      "Hint: try: %s\n"
      (List.map cmdline_suggestion ~f:quote_for_shell |> String.concat ~sep:" ")
  | Findlib.External_dep_conflicts_with_local_lib
      { package; required_by; required_locally_in; defined_locally_in } ->
    Format.fprintf ppf
      "@{<error>Error@}: Conflict between internal and external version of library %S:\n\
       - it is defined locally in %s\n\
       - it is required by external library %S\n\
       - external library %S is required in %s\n\
       This cannot work.\n"
      package
      (Utils.jbuild_name_in ~dir:(Path.drop_build_context defined_locally_in))
      required_by
      required_by
      (Utils.jbuild_name_in ~dir:required_locally_in)
  | Code_error msg ->
    let bt = Printexc.raw_backtrace_to_string backtrace in
    Format.fprintf ppf "@{<error>Internal error, please report upstream \
                        including the contents of _build/log.@}\n\
                        Description: %s\n\
                        Backtrace:\n\
                        %s" msg bt
  | Unix.Unix_error (err, func, fname) ->
    Format.fprintf ppf "@{<error>Error@}: %s: %s: %s\n"
      func fname (Unix.error_message err)
  | _ ->
    let s = Printexc.to_string exn in
    let bt = Printexc.raw_backtrace_to_string backtrace in
    if String.is_prefix s ~prefix:"File \"" then
      Format.fprintf ppf "%s\nBacktrace:\n%s" s bt
    else
      Format.fprintf ppf "@{<error>Error@}: exception %s\nBacktrace:\n%s" s bt

let report_error ?map_fname ppf exn =
  match exn with
  | Build_system.Build_error.E err ->
    let module E = Build_system.Build_error in
    report_error ?map_fname ppf (E.exn err) ~backtrace:(E.backtrace err);
    if !Clflags.debug_dep_path then
      Format.fprintf ppf "Dependency path:\n    %s\n"
        (String.concat ~sep:"\n--> "
           (List.map (E.dependency_path err) ~f:Utils.describe_target))
  | exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    report_error ?map_fname ppf exn ~backtrace

let ignored_during_bootstrap =
  Path.Set.of_list
    (List.map ~f:Path.of_string
       [ "test"
       ; "example"
       ])

(* Called by the script generated by ../build.ml *)
let bootstrap () =
  Ansi_color.setup_err_formatter_colors ();
  let pkg = "jbuilder" in
  let main () =
    let anon s = raise (Arg.Bad (Printf.sprintf "don't know what to do with %s\n" s)) in
    let subst () =
      Future.Scheduler.go (Watermarks.subst () ~name:"jbuilder");
      exit 0
    in
    Arg.parse
      [ "-j"           , Set_int Clflags.concurrency, "JOBS concurrency"
      ; "--dev"        , Set Clflags.dev_mode       , " set development mode"
      ; "--debug-rules", Set Clflags.debug_rules    , " print out rules"
      ; "--verbose"    , Set Clflags.verbose        , " print detailed information about commands being run"
      ; "--subst"      , Unit subst                 , " substitute watermarks in source files"
      ]
      anon "Usage: boot.exe [-j JOBS] [--dev]\nOptions are:";
    let log = Log.create () in
    Future.Scheduler.go ~log
      (setup ~log ~workspace:{ merlin_context = Some "default"; contexts = [Default] }
         ~use_findlib:false
         ~extra_ignored_subtrees:ignored_during_bootstrap
         ()
       >>= fun { build_system = bs; _ } ->
       Build_system.do_build_exn bs [Path.(relative root) (pkg ^ ".install")])
  in
  try
    main ()
  with exn ->
    Format.eprintf "%a@?" (report_error ?map_fname:None) exn;
    exit 1

let setup = setup ~use_findlib:true ~extra_ignored_subtrees:Path.Set.empty

open Import

module Preds : sig
  type t

  val make : string list -> t
  val count : t -> int
  val is_subset : t -> subset:t -> bool
  val intersects : t -> t -> bool
end = struct
  type t = string list

  let make l = List.sort l ~cmp:String.compare

  let count = List.length

  let rec is_subset t ~subset =
    match t, subset with
    | _, [] -> true
    | [], _ :: _ -> false
    | x1 :: l1, x2 :: l2 ->
      let d = String.compare x1 x2 in
      if d = 0 then
        is_subset l1 ~subset:l2
      else if d < 0 then
        is_subset l1 ~subset
      else
        false

  let rec intersects a b =
    match a, b with
    | [], _ | _, [] -> false
    | x1 :: l1, x2 :: l2 ->
      let d = String.compare x1 x2 in
      if d = 0 then
        true
      else if d < 0 then
        intersects l1 b
      else
        intersects a l2
end

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required  : Preds.t
    ; preds_forbidden : Preds.t
    ; value           : string
    }

  let formal_predicates_count t =
    Preds.count t.preds_required + Preds.count t.preds_forbidden

  let matches t ~preds =
    Preds.is_subset preds ~subset:t.preds_required &&
    not (Preds.intersects preds t.preds_forbidden)


  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Inl x
        | Neg x -> Inr x)
    in
    { preds_required  = Preds.make preds_required
    ; preds_forbidden = Preds.make preds_forbidden
    ; value           = rule.value
    }
end

(* Set of rules for a given variable of a package *)
module Rules = struct
  (* To implement the algorithm described in [1], [set_rules] is sorted by decreasing
     number of formal predicates, then according to the order of the META
     file. [add_rules] are in the same order as in the META file.

     [1] http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html *)
  type t =
    { set_rules : Rule.t list
    ; add_rules : Rule.t list
    }

  let interpret t ~preds =
    let rec find_set_rule = function
      | [] -> ""
      | rule :: rules ->
        if Rule.matches rule ~preds then
          rule.value
        else
          find_set_rule rules
    in
    let v = find_set_rule t.set_rules in
    List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
      if Rule.matches rule ~preds then
        v ^ " " ^ rule.value
      else
        v)

  let of_meta_rules (rules : Meta.Simplified.Rules.t) =
    let add_rules = List.map rules.add_rules ~f:Rule.make in
    let set_rules =
      List.map rules.set_rules ~f:Rule.make
      |> List.stable_sort ~cmp:(fun a b ->
        compare (Rule.formal_predicates_count b) (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

module Vars = struct
  type t = Rules.t String_map.t

  let get (t : t) var preds =
    let preds = Preds.make preds in
    match String_map.find var t with
    | None -> ""
    | Some rules -> Rules.interpret rules ~preds

  let get_words t var preds = String.extract_comma_space_separated_words (get t var preds)
end

type package =
  { name             : string
  ; dir              : Path.t
  ; version          : string
  ; description      : string
  ; archives         : string list Mode.Dict.t
  ; plugins          : string list Mode.Dict.t
  ; jsoo_runtime     : string list
  ; requires         : package list
  ; ppx_runtime_deps : package list
  ; has_headers      : bool
  }

module Package_not_found = struct
  type t =
    { package     : string
    ; required_by : string list
    }
end

type present_or_absent =
  | Present of package
  | Absent  of Package_not_found.t

type t =
  { stdlib_dir  : Path.t
  ; path        : Path.t list
  ; packages    : (string, present_or_absent) Hashtbl.t
  ; has_headers : (Path.t, bool             ) Hashtbl.t
  }

let path t = t.path

let create ~stdlib_dir ~path =
  { stdlib_dir
  ; path
  ; packages    = Hashtbl.create 1024
  ; has_headers = Hashtbl.create 1024
  }

let has_headers t ~dir =
  match Hashtbl.find t.has_headers dir with
  | Some x -> x
  | None ->
    let x =
      match Path.readdir dir with
      | exception _ -> false
      | files ->
        List.exists files ~f:(fun fn -> Filename.check_suffix fn ".h")
    in
    Hashtbl.add t.has_headers ~key:dir ~data:x;
    x

module Pkg_step1 = struct
  type t =
    { package          : package
    ; requires         : string list
    ; ppx_runtime_deps : string list
    ; exists           : bool
    ; required_by      : string list
    }
end

let parse_package t ~name ~parent_dir ~vars ~required_by =
  let pkg_dir = Vars.get vars "directory" [] in
  let dir =
    if pkg_dir = "" then
      parent_dir
    else if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
      Path.relative t.stdlib_dir
        (String.sub pkg_dir ~pos:1 ~len:(String.length pkg_dir - 1))
    else if Filename.is_relative pkg_dir then
      Path.relative parent_dir pkg_dir
    else
      Path.absolute pkg_dir
  in
  let archives var preds =
    Mode.Dict.of_func (fun ~mode ->
      Vars.get_words vars var (Mode.findlib_predicate mode :: preds))
  in
  let jsoo_runtime = Vars.get_words vars "jsoo_runtime" [] in
  let preds = ["ppx_driver"; "mt"; "mt_posix"] in
  let pkg =
    { name
    ; dir
    ; has_headers = has_headers t ~dir
    ; version     = Vars.get vars "version" []
    ; description = Vars.get vars "description" []
    ; archives    = archives "archive" preds
    ; jsoo_runtime
    ; plugins     = Mode.Dict.map2 ~f:(@)
                      (archives "archive" ("plugin" :: preds))
                      (archives "plugin" preds)
    ; requires    = []
    ; ppx_runtime_deps = []
    }
  in
  let exists_if = Vars.get_words vars "exists_if" [] in
  let exists =
    List.for_all exists_if ~f:(fun fn ->
      Path.exists (Path.relative dir fn))
  in
  { Pkg_step1.
    package          = pkg
  ; requires         = Vars.get_words vars "requires"         preds
  ; ppx_runtime_deps = Vars.get_words vars "ppx_runtime_deps" preds
  ; exists           = exists
  ; required_by
  }

let parse_meta t ~dir ~required_by (meta : Meta.t) =
  let rec loop ~dir ~full_name ~acc (meta : Meta.Simplified.t) =
    let vars = String_map.map meta.vars ~f:Rules.of_meta_rules in
    let pkg = parse_package t ~name:full_name ~parent_dir:dir ~vars ~required_by in
    let dir = pkg.package.dir in
    List.fold_left meta.subs ~init:(pkg :: acc) ~f:(fun acc (meta : Meta.Simplified.t) ->
      loop ~dir ~full_name:(sprintf "%s.%s" full_name meta.name) ~acc meta)
  in
  loop ~dir ~full_name:meta.name (Meta.simplify meta) ~acc:[]

let root_package_name s =
  match String.index s '.' with
  | None -> s
  | Some i -> String.sub s ~pos:0 ~len:i

let rec load_meta_rec t ~fq_name ~packages ~required_by =
  let root_name = root_package_name fq_name in
  if String_map.mem root_name packages ||
     Hashtbl.mem t.packages root_name then
    packages
  else
    (* Search for a <package>/META file in the findlib search path *)
    let rec loop dirs : (Path.t * Meta.t) option =
      match dirs with
      | dir :: dirs ->
        let dir = Path.relative dir root_name in
        let fn = Path.relative dir "META" in
        if Path.exists fn then
          Some (dir,
                { name    = root_name
                ; entries = Meta.load (Path.to_string fn)
                })
        else
          loop dirs
      | [] ->
        match String_map.find root_name Meta.builtins with
        | Some meta -> Some (t.stdlib_dir, meta)
        | None ->
          let required_by =
            if root_name = fq_name then
              required_by
            else
              fq_name :: required_by
          in
          Hashtbl.add t.packages ~key:root_name
            ~data:(Absent { package = root_name
                          ; required_by
                          });
          None
    in
    match loop t.path with
    | None -> packages
    | Some (dir, meta) ->
      let new_packages = parse_meta t ~dir ~required_by meta in
      let packages =
        List.fold_left new_packages ~init:packages ~f:(fun acc (pkg : Pkg_step1.t) ->
          String_map.add acc ~key:pkg.package.name ~data:pkg)
      in
      let deps =
        List.fold_left new_packages ~init:String_map.empty
          ~f:(fun acc (pkg : Pkg_step1.t) ->
            if pkg.exists then
              let add_deps acc deps =
                List.fold_left deps ~init:acc ~f:(fun acc dep ->
                  String_map.add acc ~key:dep ~data:pkg.package.name)
              in
              add_deps (add_deps acc pkg.requires) pkg.ppx_runtime_deps
            else
              acc)
      in
      String_map.fold deps ~init:packages ~f:(fun ~key:dep ~data:package packages ->
        load_meta_rec t ~fq_name:dep ~packages ~required_by:(package :: required_by))

module Local_closure =
  Top_closure.Make
    (String)
    (struct
      type graph = Pkg_step1.t String_map.t
      type t = Pkg_step1.t
      let key (t : t) = t.package.name
      let deps (t : t) packages =
        List.filter_map t.requires ~f:(fun name ->
          String_map.find name packages) @
        List.filter_map t.ppx_runtime_deps ~f:(fun name ->
          String_map.find name packages)
    end)

let remove_dups_preserve_order pkgs =
  let rec loop seen pkgs acc =
    match pkgs with
    | [] -> List.rev acc
    | pkg :: pkgs ->
      if String_set.mem pkg.name seen then
        loop seen pkgs acc
      else
        loop (String_set.add pkg.name seen) pkgs (pkg :: acc)
  in
  loop String_set.empty pkgs []
;;

let load_meta t ~fq_name ~required_by =
  let packages = load_meta_rec t ~fq_name ~packages:String_map.empty ~required_by in
  match Local_closure.top_closure packages (String_map.values packages) with
  | Error cycle ->
    die "dependency cycle detected between external findlib packages:\n   %s"
      (List.map cycle ~f:(fun (pkg : Pkg_step1.t) -> pkg.package.name)
       |> String.concat ~sep:"\n-> ")
  | Ok ordering ->
    List.iter ordering ~f:(fun (pkg : Pkg_step1.t) ->
      if not pkg.exists then begin
        if !Clflags.debug_findlib then
          Printf.eprintf "findlib: package %S is hidden\n"
            pkg.package.name
      end else begin
        let resolve_deps deps missing_deps_acc =
          let deps, missing_deps =
            List.partition_map deps ~f:(fun name ->
              match Hashtbl.find t.packages name with
              | Some (Present pkg) -> Inl pkg
              | None | Some (Absent _) -> Inr name)
          in
          (deps, missing_deps @ missing_deps_acc)
        in
        let requires, missing_deps = resolve_deps pkg.requires [] in
        let ppx_runtime_deps, missing_deps =
          resolve_deps pkg.ppx_runtime_deps missing_deps
        in
        match missing_deps with
        | [] ->
          let requires =
            remove_dups_preserve_order
              (List.concat_map requires ~f:(fun pkg -> pkg.requires) @ requires)
          in
          let ppx_runtime_deps =
            remove_dups_preserve_order
              (List.concat
                 [ List.concat_map ppx_runtime_deps ~f:(fun pkg -> pkg.requires)
                 ; ppx_runtime_deps
                 ; List.concat_map requires ~f:(fun pkg -> pkg.ppx_runtime_deps)
                 ])
          in
          let pkg =
            { pkg.package with
              requires
            ; ppx_runtime_deps
            }
          in
          Hashtbl.add t.packages ~key:pkg.name ~data:(Present pkg)
        | _ ->
          let unknown_deps, hidden_deps =
            List.partition_map missing_deps ~f:(fun name ->
              match String_map.find name packages with
              | None -> Inl name
              | Some pkg -> Inr pkg)
          in
          match unknown_deps with
          | name :: _ ->
            Hashtbl.add t.packages ~key:name
              ~data:(Absent { package     = name
                            ; required_by = pkg.package.name :: pkg.required_by
                            })
          | [] ->
            (* We can be in this case for ctypes.foreign for instance *)
            if !Clflags.debug_findlib then
              Printf.eprintf "findlib: skipping %S has it has hidden dependencies: %s\n"
                pkg.package.name
                (String.concat ~sep:", "
                   (List.map hidden_deps
                      ~f:(fun (pkg : Pkg_step1.t) -> pkg.package.name)));
            assert (List.for_all hidden_deps
                      ~f:(fun (pkg : Pkg_step1.t) -> not pkg.exists))
      end
    )

exception Package_not_found of Package_not_found.t

let find_exn t ~required_by name =
  match Hashtbl.find t.packages name with
  | Some (Present x) -> x
  | Some (Absent pnf) -> raise (Package_not_found pnf)
  | None ->
    load_meta t ~fq_name:name ~required_by;
    match Hashtbl.find t.packages name with
    | Some (Present x) -> x
    | Some (Absent pnf) ->
      raise (Package_not_found pnf)
    | None ->
      let pnf =
        { Package_not_found.
          package     = name
        ; required_by
        }
      in
      Hashtbl.add t.packages ~key:name ~data:(Absent pnf);
      raise (Package_not_found pnf)

let find t ~required_by name =
  match find_exn t ~required_by name with
  | exception (Package_not_found _) -> None
  | x -> Some x

let available t ~required_by name =
  match find_exn t name ~required_by with
  | (_ : package) -> true
  | exception (Package_not_found _) -> false

module External_dep_conflicts_with_local_lib = struct
  type t =
    { package             : string
    ; required_by         : string
    ; required_locally_in : Path.t
    ; defined_locally_in  : Path.t
    }
end

exception External_dep_conflicts_with_local_lib of External_dep_conflicts_with_local_lib.t

let check_deps_consistency ~required_by ~local_public_libs pkg requires =
  List.iter requires ~f:(fun pkg' ->
    match String_map.find pkg'.name local_public_libs with
    | None -> ()
    | Some path ->
      raise (External_dep_conflicts_with_local_lib
               { package             = pkg'.name
               ; required_by         = pkg.name
               ; required_locally_in = required_by
               ; defined_locally_in  = path
               }))

let closure ~required_by ~local_public_libs pkgs =
  remove_dups_preserve_order
    (List.concat_map pkgs ~f:(fun pkg ->
       check_deps_consistency ~required_by ~local_public_libs pkg pkg.requires;
       pkg.requires)
     @ pkgs)

let closed_ppx_runtime_deps_of ~required_by ~local_public_libs pkgs =
  remove_dups_preserve_order
    (List.concat_map pkgs ~f:(fun pkg ->
       check_deps_consistency ~required_by ~local_public_libs pkg pkg.ppx_runtime_deps;
       pkg.ppx_runtime_deps))

let root_packages t =
  let pkgs =
    List.concat_map t.path ~f:(fun dir ->
      Sys.readdir (Path.to_string dir)
      |> Array.to_list
      |> List.filter ~f:(fun name ->
        Path.exists (Path.relative dir (name ^ "/META"))))
    |> String_set.of_list
  in
  let pkgs =
    String_set.union pkgs
      (String_set.of_list (String_map.keys Meta.builtins))
  in
  String_set.elements pkgs

let all_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    ignore (find_exn t pkg ~required_by:[] : package));
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:_ ~data acc ->
    match data with
    | Present p -> p :: acc
    | Absent  _ -> acc)
  |> List.sort ~cmp:(fun a b -> String.compare a.name b.name)


let stdlib_with_archives t =
  let x = find_exn t ~required_by:[] "stdlib" in
  let archives =
    { Mode.Dict.byte   = "stdlib.cma"  :: x.archives.byte
    ; Mode.Dict.native = "stdlib.cmxa" :: x.archives.native }
  in
  { x with archives }

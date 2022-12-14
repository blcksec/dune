open Import
open Jbuild_types
open Meta
open Build.O

module Pub_name = struct
  type t =
    | Dot of t * string
    | Id  of string

  let parse s =
    match String.split s ~on:'.' with
    | [] -> assert false
    | x :: l ->
      let rec loop acc l =
        match l with
        | [] -> acc
        | x :: l -> loop (Dot (acc, x)) l
      in
      loop (Id x) l

  let rec root = function
    | Dot (t, _) -> root t
    | Id n       -> n

  let to_list =
    let rec loop acc = function
      | Dot (t, n) -> loop (n :: acc) t
      | Id n       -> n :: acc
    in
    fun t -> loop [] t

  let to_string t = String.concat ~sep:"." (to_list t)
end

type item = Lib of Path.t * Pub_name.t * Library.t

let string_of_deps l =
  String.concat (List.sort l ~cmp:String.compare) ~sep:" "

let rule var predicates action value =
  Rule { var; predicates; action; value }
let requires ?(preds=[]) pkgs =
  rule "requires" preds Set (string_of_deps pkgs)
let ppx_runtime_deps ?(preds=[]) pkgs =
  rule "ppx_runtime_deps" preds Set (string_of_deps pkgs)
let description s = rule "description" []      Set s
let directory   s = rule "directory"   []      Set s
let archive preds s = rule "archive"   preds Set s
let plugin preds  s = rule "plugin"    preds Set s
let archives ?(preds=[]) name =
  [ archive (preds @ [Pos "byte"  ]) (name ^ ".cma" )
  ; archive (preds @ [Pos "native"]) (name ^ ".cmxa")
  ; plugin  (preds @ [Pos "byte"  ]) (name ^ ".cma" )
  ; plugin  (preds @ [Pos "native"]) (name ^ ".cmxs")
  ]

let gen_lib pub_name (lib : Library.t) ~lib_deps ~ppx_runtime_deps:ppx_rt_deps ~version =
  let desc =
    match lib.synopsis with
    | Some s -> s
    | None ->
      match (pub_name : Pub_name.t) with
      | Dot (p, "runtime-lib") -> sprintf "Runtime library for %s" (Pub_name.to_string p)
      | Dot (p, "expander"   ) -> sprintf "Expander for %s"        (Pub_name.to_string p)
      | _ -> ""
  in
  let preds =
    match lib.kind with
    | Normal -> []
    | Ppx_rewriter | Ppx_deriver -> [Pos "ppx_driver"]
  in
  List.concat
    [ version
    ; [ description desc
      ; requires ~preds lib_deps
      ]
    ; archives ~preds lib.name
    ; (match lib.kind with
       | Normal -> []
       | Ppx_rewriter | Ppx_deriver ->
         let sub_pkg_name = "deprecated-ppx-method" in
         [ Comment "This is what jbuilder uses to find out the runtime dependencies of"
         ; Comment "a preprocessor"
         ; ppx_runtime_deps ppx_rt_deps
         ; Comment "This line makes things transparent for people mixing preprocessors"
         ; Comment "and normal dependencies"
         ; requires ~preds:[Neg "ppx_driver"]
             [Pub_name.to_string (Dot (pub_name, sub_pkg_name))]
         ; Package
             { name    = sub_pkg_name
             ; entries =
                 List.concat
                   [ version
                   ; [ description "glue package for the deprecated method of using ppx"
                     ; requires ppx_rt_deps
                     ]
                   ; match lib.kind with
                   | Normal -> assert false
                   | Ppx_rewriter ->
                     [ rule "ppx" [Neg "ppx_driver"; Neg "custom_ppx"]
                         Set "./ppx.exe --as-ppx" ]
                   | Ppx_deriver ->
                     [ rule "requires" [Neg "ppx_driver"; Neg "custom_ppx"] Add
                         "ppx_deriving"
                     ; rule "ppxopt" [Neg "ppx_driver"; Neg "custom_ppx"] Set
                         ("ppx_deriving,package:" ^ Pub_name.to_string pub_name)
                     ]
                   ]
             }
         ])
    ; (match lib.buildable.js_of_ocaml with
       | { javascript_files = []; _ } -> []
       | { javascript_files = l ; _ } ->
         let root = Pub_name.root pub_name in
         [ rule "linkopts" [Pos "javascript"] Set
             (List.map l ~f:(fun fn ->
                sprintf "+%s/%s" root (Filename.basename fn))
              |> String.concat ~sep:" ")
         ; rule "jsoo_runtime" [] Set
             (List.map l ~f:Filename.basename
              |> String.concat ~sep:" ")
         ]
      )
    ]

let gen ~package ~version ~stanzas ~lib_deps ~ppx_runtime_deps =
  let items =
    List.filter_map stanzas ~f:(fun (dir, stanza) ->
      match (stanza : Stanza.t) with
      | Library ({ public = Some { name; package = p; _ }; _ } as lib)
        when p.name = package ->
        Some (Lib (dir, Pub_name.parse name, lib))
      | _ ->
        None)
  in
  (version >>| function
   | None -> []
   | Some s -> [rule "version" [] Set s])
  >>= fun version ->
  Build.all
    (List.map items ~f:(fun (Lib (dir, pub_name, lib)) ->
       Build.both
         (lib_deps ~dir         (Stanza.Library lib))
         (ppx_runtime_deps ~dir (Stanza.Library lib))
       >>| fun (lib_deps, ppx_runtime_deps) ->
       (pub_name,
        gen_lib pub_name lib ~lib_deps ~ppx_runtime_deps ~version)))
  >>| fun pkgs ->
  let pkgs =
    List.map pkgs ~f:(fun (pn, meta) ->
      match Pub_name.to_list pn with
      | [] -> assert false
      | _package :: path -> (path, meta))
  in
  let pkgs = List.sort pkgs ~cmp:(fun (a, _) (b, _) -> compare a b) in
  let rec loop name pkgs =
    let entries, sub_pkgs =
      List.partition_map pkgs ~f:(function
        | ([]    , entries) -> Inl entries
        | (x :: p, entries) -> Inr (x, (p, entries)))
    in
    let entries = List.concat entries in
    let subs =
      String_map.of_alist_multi sub_pkgs
      |> String_map.bindings
      |> List.map ~f:(fun (name, pkgs) ->
        let pkg = loop name pkgs in
        Package { pkg with
                  entries = directory name :: pkg.entries
                })
    in
    { name
    ; entries = entries @ subs
    }
  in
  loop package pkgs

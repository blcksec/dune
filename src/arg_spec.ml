open Import

module Pset = Path.Set

type t =
  | A        of string
  | As       of string list
  | S        of t list
  | Dep      of Path.t
  | Deps     of Path.t list
  | Dep_rel  of Path.t * string
  | Deps_rel of Path.t * string list
  | Path     of Path.t
  | Paths    of Path.t list

let rec add_deps ts set =
  List.fold_left ts ~init:set ~f:(fun set t ->
    match t with
    | Dep  fn  -> Pset.add fn set
    | Deps fns -> Pset.union set (Pset.of_list fns)
    | Dep_rel  (dir, fn) -> Pset.add (Path.relative dir fn) set
    | Deps_rel (dir, fns) ->
      List.fold_left fns ~init:set ~f:(fun set fn ->
        Pset.add (Path.relative dir fn) set)
    | S ts -> add_deps ts set
    | _ -> set)

let deps ts = add_deps ts Pset.empty

let expand ~dir ts =
  let rec loop = function
    | A s  -> [s]
    | As l -> l
    | Dep_rel (_, fn) -> [fn]
    | Deps_rel (_, fns) -> fns
    | (Dep fn | Path fn) -> [Path.reach fn ~from:dir]
    | (Deps fns | Paths fns) -> List.map fns ~f:(Path.reach ~from:dir)
    | S ts -> List.concat_map ts ~f:loop
  in
  List.concat_map ts ~f:loop

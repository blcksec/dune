open Import

module Deserialization_error = struct
  type t =
    | Version_mismatch of int
    | Parse_error
end

module type Sexpable = sig
  type t

  val to_sexp : t -> Sexp.t

  val of_sexp : Sexp.t -> (t, Deserialization_error.t) result
end

module type S = sig
  type t

  val conv : t Conv.value

  val version : int
end

module Make (TypeToSerialize : S) = struct
  open TypeToSerialize

  let conv =
    let open Conv in
    pair int conv

  let of_sexp sexp =
    match Conv.of_sexp Conv.(pair int sexp) ~version:(0, 0) sexp with
    | Error _ -> Error Deserialization_error.Parse_error
    | Ok (version, sexp) -> (
      match Int.equal version TypeToSerialize.version with
      | false -> Error (Deserialization_error.Version_mismatch version)
      | true -> (
        match Conv.of_sexp TypeToSerialize.conv ~version:(0, 0) sexp with
        | Error _ -> Error Deserialization_error.Parse_error
        | Ok v -> Ok v))

  let to_sexp t = Conv.to_sexp conv (TypeToSerialize.version, t)
end

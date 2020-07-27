(*
 * err.ml - An Error Monad
 *)

type 'a err =
  | Ok of 'a
  | Fail of string

module Err = struct

  (* Bind for the error monad *)
  let ( >>= ) m f =
    match m with
    | Ok a -> f a
    | Fail s -> Fail s

  let product e e' =
    match e , e' with
    | Ok x , Ok y -> Ok (x , y)
    | Fail s , _ -> Fail s
    | _ , Fail s' -> Fail s'

  let map e f =
    match e with
    | Ok x -> Ok (f x)
    | Fail s -> Fail s
    
  module Syntax = struct
    let (let+) e f = map e f
    let (and+) e e' = product e e'
    let (let*) m f = m >>= f
  end
  
end

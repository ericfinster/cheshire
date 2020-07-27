(*
 * err.ml - An Error Monad
 *)

open Monad
    
type 'a err =
  | Ok of 'a
  | Fail of string

module ErrMonad : Monad with type 'a t := 'a err =
  MakeMonad(struct

    type 'a t = 'a err
        
    let map f e =
      match e with
      | Ok x -> Ok (f x)
      | Fail s -> Fail s

    let pure a = Ok a

    let bind m f =
      match m with
      | Ok a -> f a
      | Fail s -> Fail s
                    
  end)

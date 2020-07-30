(*****************************************************************************)
(*                                                                           *)
(*                                   Lists                                   *)
(*                                                                           *)
(*****************************************************************************)

open Monad
open Traverse
open Applicative
    
module ListMonad : Monad with type 'a t := 'a list =
  MakeMonad(struct

    type 'a t = 'a list

    let map = List.map

    let pure a = [a]
                 
    let rec bind l f =
      match l with
      | [] -> []
      | x::xs -> f x @ bind xs f
                             
  end)

module ListTraverse(A : Applicative) : Traverse
  with type 'a t := 'a list
  with type 'a m := 'a A.t = struct

  open A.ApplicativeSyntax
  
  let rec traverse f l =
    match l with
    | [] -> A.pure []
    | x::xs ->
      let+ b = f x 
      and+ bs = traverse f xs in
      b::bs
                 
end




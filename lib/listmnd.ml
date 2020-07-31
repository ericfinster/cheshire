(*****************************************************************************)
(*                                                                           *)
(*                                   Lists                                   *)
(*                                                                           *)
(*****************************************************************************)

open Monad
open Applicative
    
module ListMonad = MakeMonad(struct

    type 'a t = 'a list

    let map = List.map

    let pure a = [a]
                 
    let rec bind l f =
      match l with
      | [] -> []
      | x::xs -> f x @ bind xs f
                             
  end)

module ListTraverse(A : Applicative) = struct

  type 'a t = 'a list
  type 'a m = 'a A.t
      
  open A.ApplicativeSyntax 
  
  let rec traverse f l =
    match l with
    | [] -> A.pure []
    | x::xs ->
      let+ b = f x 
      and+ bs = traverse f xs in
      b::bs
      
  (* let rec traverse f l =
   *   match l with
   *   | [] -> pure []
   *   | x::xs -> pure (fun b bs -> b::bs) <*> f x <*> traverse f xs *)

end



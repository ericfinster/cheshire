(*****************************************************************************)
(*                                                                           *)
(*                                   Lists                                   *)
(*                                                                           *)
(*****************************************************************************)

open Monad
open Traverse
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

module ListTraverse(A : Applicative) : Traverse = struct

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

end

module ListFold(M : Monad) = struct

  open M.MonadSyntax
         
  let rec fold_left_m f a l =
    match l with
    | [] -> M.pure a
    | b::bs ->
      let* r = f a b in
      fold_left_m f r bs

  let rec fold_right_m f a l =
    match l with
    | [] -> M.pure a
    | b::bs ->
      let* r = fold_right_m f a bs in
      f b r
      
end

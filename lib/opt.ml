(* 
 * opt.ml - Option implementation
 *)

open Option
open Monad
    
module OptMonad : Monad with type 'a t := 'a option
  = MakeMonad(struct

    type 'a t = 'a option

    let map f m =
      match m with
      | None -> None
      | Some a -> Some (f a)

    let pure a = Some a

    let bind m f =
      match m with
      | None -> None
      | Some a -> f a

  end)

open OptMonad.MonadSyntax

let head_opt l =
  match l with 
  | [] -> None
  | x::_ -> Some x

let tail_opt l =
  match l with
  | [] -> None
  | _::xs -> Some xs 
       
let opt_test l =
  let* x = head_opt l in
  let* xs = tail_opt l in
  let* y = head_opt xs in
  Some (x + y)

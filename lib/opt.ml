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


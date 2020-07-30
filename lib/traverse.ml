(*****************************************************************************)
(*                                                                           *)
(*                                  Traverse                                 *)
(*                                                                           *)
(*****************************************************************************)

(* open Functor *)
    
module type TraverseBase = sig
  type 'a t
  type 'a m
  val traverse : ('a -> 'b m) -> 'a t -> 'b t m
end

module type Traverse = sig
  include TraverseBase
  (* include Functor with type 'a t := 'a t  *)
end

(* module Make (S : sig type 'a s end)
 *             (F : functor (A: Applicative)
 *                  -> Base with type 'a m = 'a A.m
 *                     with type 'a t = 'a S.s)
 *             (A : Applicative) = struct
 * 
 *   include S
 *   include F(A)
 * 
 *   module MapInstance = F(Monad.Identity)
 * 
 *   let map : ('a -> 'b) -> 'a s -> 'b s =
 *     MapInstance.traverse
 *               
 * end *)


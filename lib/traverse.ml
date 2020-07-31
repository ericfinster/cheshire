(*****************************************************************************)
(*                                                                           *)
(*                                  Traverse                                 *)
(*                                                                           *)
(*****************************************************************************)

(* open Functor
 * open Applicative *)
    
module type TraverseBase = sig

  type 'a t
  type 'a m

  (* include FunctorBase with type 'a t := 'a t
   * include ApplicativeBase with type 'a t := 'a m *)
    
  val traverse : ('a -> 'b m) -> 'a t -> 'b t m
      
end



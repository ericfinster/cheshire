(*****************************************************************************)
(*                                                                           *)
(*                                  Functors                                 *)
(*                                                                           *)
(*****************************************************************************)

module type FunctorBase = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Functor = sig
  include FunctorBase
      
  module FunctorSyntax : sig
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module MakeFunctor(F: FunctorBase): Functor
  with type 'a t := 'a F.t = struct
  include F

  module FunctorSyntax = struct
    let (let+) a f = map f a 
  end
end

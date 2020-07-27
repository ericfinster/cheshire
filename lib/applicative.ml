(*
 *  applicative.ml - applicative functors
 *)

module type ApplicativeBase =
sig
  include Functor.Functor 
  val product : 'a t -> 'b t -> ('a * 'b) t 
  val pure : 'a -> 'a t
end

module MakeApplicative(A : ApplicativeBase) = 
struct
  include A

  module Syntax =
  struct
    let (let+) x f = map x f
    let (and+) x y = product x y
  end

  let apply : ('a -> 'b) t -> 'a t -> 'a t =
    fun ft xt -> map (fun (f , x) -> f x) @@ product ft xt 
        
  let ( <*> ) = apply
    
end


(*
 *  applicative.ml - applicative functors
 *)

open Functor
    
module type ApplicativeBase = sig
  include FunctorBase
  val product : 'a t -> 'b t -> ('a * 'b) t 
  val pure : 'a -> 'a t
end

module type Applicative = sig
  include ApplicativeBase
  include Functor with type 'a t := 'a t
  
  val apply : ('a -> 'b) t -> 'a t -> 'b t 
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  (* Can't seem to get this to automatically include
   * the functor syntax .... *)
  module ApplicativeSyntax : sig
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    val (and+) : 'a t -> 'b t -> ('a * 'b) t 
  end
end

module MakeApplicative(A : ApplicativeBase): Applicative 
  with type 'a t := 'a A.t  = struct
  
  include A

  let apply ft xt =
    map (fun (f , x) -> f x) @@ product ft xt 

  let ( <*> ) = apply

  module F : Functor with type 'a t := 'a t =
    MakeFunctor(struct
      include A 
    end)

  include F
      
  module ApplicativeSyntax = struct
    let (let+) a f = map f a
    let (and+) x y = product x y
  end
end


(*
 *  applicative.ml - applicative functors
 *)

open Functor
    
module type ApplicativeBase =
sig
  include Functor 
  val product : 'a t -> 'b t -> ('a * 'b) t 
  val pure : 'a -> 'a t
end

module type Applicative =
sig
  include ApplicativeBase
      
  val apply : ('a -> 'b) t -> 'a t -> 'a t 
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'a t

  (* let+ is really *functor* syntax *)
  module ApplicativeSyntax : sig
    val (let+) : ('a -> 'b) -> 'a t -> 'b t
    val (and+) : 'a t -> 'b t -> ('a * 'b) t 
  end
  
end

module MakeApplicative(A : ApplicativeBase) = 
struct
  include A

  let apply ft xt =
    map (fun (f , x) -> f x) @@ product ft xt 
        
  let ( <*> ) = apply

  module ApplicativeSyntax = struct
    let (let+) x f = map x f
    let (and+) x y = product x y
  end
  
end


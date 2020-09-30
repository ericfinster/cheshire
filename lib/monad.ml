(*****************************************************************************)
(*                                                                           *)
(*                                   Monads                                  *)
(*                                                                           *)
(*****************************************************************************)

open Functor
open Applicative
    
module type MonadBase = sig
  include FunctorBase
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pure : 'a -> 'a t
end

module type Monad = sig
  include MonadBase
  include Functor with type 'a t := 'a t 
  include Applicative with type 'a t := 'a t
      
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t

  module MonadSyntax : sig
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    val (and+) : 'a t -> 'b t -> ('a * 'b) t 
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end

end

module MakeMonad(M : MonadBase) = struct
  
  include M

  let ( >>= ) = bind
  let join m = m >>= (fun x -> x)

  module F : Functor with type 'a t := 'a t =
    MakeFunctor(struct
      include M 
    end)

  module A : Applicative with type 'a t := 'a t =
    MakeApplicative(struct
      include M
      let product at bt =
        at >>= fun a ->
        bt >>= fun b ->
        pure (a , b) 
    end)

  include F
  include A
      
  module MonadSyntax = struct
    include A.ApplicativeSyntax
    let (let*) m f = m >>= f
  end
                     
end

(*
 * Error monads
 *)

module type MonadErrorBase = sig

  type 'a t
  type e

  val throw : e -> 'a t 
  val catch : 'a t -> (e -> 'a t) -> 'a t

end 

module type MonadError = sig
  include Monad
  include MonadErrorBase with type 'a t := 'a t

  val ensure : bool -> e -> unit t
  val (<||>) : 'a t -> 'a t -> 'a t
      
end


module MakeMonadError(M : MonadBase)(E : MonadErrorBase with type 'a t := 'a M.t) = struct

  module Mnd = MakeMonad(M)

  include Mnd
  include E

  let ensure b s =
    if b then pure () else throw s 

  let (<||>) m n =
    catch m (fun _ -> n)
    
end

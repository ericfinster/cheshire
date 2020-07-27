(*
 * monad.ml - monads
 *)

module type MonadBase = sig
  include Functor.Functor
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pure: 'a -> 'a t
end

module type Monad = sig
  include MonadBase
  include Applicative.ApplicativeBase with type 'a t := 'a t
  val join : 'a t t -> 'a t
end

module MakeMonad(M : MonadBase) = struct
  include M

  let ( >>= ) = bind
  let join m = m >>= (fun x -> x)
  
  module Ap = Applicative.MakeApplicative(struct
      include M
      let product at bt =
        at >>= fun a ->
        bt >>= fun b ->
        pure (a , b) 
    end)
  
  module Syntax = struct
    include Ap.Syntax
    let (let*) m f = m >>= f
  end
                     
end

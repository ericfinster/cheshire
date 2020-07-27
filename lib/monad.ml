(*
 * monad.ml - monads
 *)

module type MonadBase = sig
  include Functor.Functor
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pure : 'a -> 'a t
end

module type Monad = sig
  include MonadBase
  include Applicative.ApplicativeBase with type 'a t := 'a t
      
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t

  module MonadSyntax : sig
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end

end

module MakeMonad(M : MonadBase) : Monad with type 'a t := 'a M.t = struct
  include M

  let ( >>= ) = bind
  let join m = m >>= (fun x -> x)
  
  module Ap : Applicative.ApplicativeBase with type 'a t := 'a t =
    Applicative.MakeApplicative(struct
      include M
      let product at bt =
        at >>= fun a ->
        bt >>= fun b ->
        pure (a , b) 
    end)

  include Ap
      
  module MonadSyntax = struct
    let (let*) m f = m >>= f
  end
                     
end

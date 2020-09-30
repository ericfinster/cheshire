(*****************************************************************************)
(*                             Monad Transformers                            *)
(*****************************************************************************)

module type Typ = sig 
  type t
end

module type Mnd = sig
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val pure : 'a -> 'a m
end

module type Traverse = sig
  type 'a t
  type 'a m
  val traverse : ('a -> 'b m) -> 'a t -> 'b t m
end

module type Applicative = sig
  type 'a t
  (* Separate out map? *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t 
  val pure : 'a -> 'a t
end

module MndToApp(M: Mnd) = struct
  let map f m = M.bind m (fun a -> M.pure (f a))
  let product x y = M.bind x (fun a -> M.bind y (fun b -> M.pure (a,b)))
  let pure = M.pure
end

module Identity = struct
  type 'a m = 'a
  let pure x = x
  let bind m f = f m
end

module ErrT(T: Typ)(M: Mnd) = struct
  type 'a err = Ok of 'a | Fail of T.t
  type 'a m = ('a err) M.m

  let pure x = M.pure (Ok x)
  let bind m f =
    M.bind m (function
        | Ok x -> f x
        | Fail s -> M.pure (Fail s))

  let lift m = M.bind m (fun x -> M.pure (Ok x))

  let throw e = M.pure (Fail e)
  let try_with m f =
    M.bind m (function
        | Ok x -> M.pure (Ok x)
        | Fail e -> f e)
      
end

module ReaderT(T: Typ)(M: Mnd) = struct
  type 'a m = T.t -> 'a M.m 

  let pure x _ = M.pure x
  let bind m f e = M.bind (m e) (fun a -> f a e)

  let ask env = M.pure env 
  let lift m _ = m
    
end  

(*****************************************************************************)
(*                            Syntax Constructors                            *)
(*****************************************************************************)

module ApplicativeSyntax(A: Applicative) = struct
    let (let+) a f = A.map f a
    let (and+) x y = A.product x y
end

module MonadSyntax(M: Mnd) = struct
  let (let+) m f = M.bind m (fun a -> M.pure (f a))
  let (and+) x y = M.bind x (fun a -> M.bind y (fun b -> M.pure (a,b)))
  let (let*) m f = M.bind m f      
end

(*****************************************************************************)
(*                            Examples and Testing                           *)
(*****************************************************************************)

type err_type = string
type outer_env = int
type inner_env = string

module Bleep = ReaderT(struct type t = inner_env end)(Identity)
module Blorp = ErrT(struct type t = err_type end)(Bleep)
module Blomp = ReaderT(struct type t = outer_env end)(Blorp)

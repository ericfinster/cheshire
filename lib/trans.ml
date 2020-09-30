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
        | Ok x -> M.pure (f x)
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
(*                            Examples and Testing                           *)
(*****************************************************************************)

(* type err_type = string
 * type outer_env = int
 * type inner_env = string
 * 
 * module Bleep = ErrT(struct type t = err_type end)(Identity)
 * module Blorp = ReaderT(struct type t = inner_env end)(Bleep)
 * module Blomp = ReaderT(struct type t = outer_env end)(Blorp)
 * 
 * type 'a mnd = 'a Blomp.m
 * let mnd_get_int : int mnd = Blomp.ask
 * let mnd_get_str : string mnd = Blomp.lift Blorp.ask 
 * 
 * let mnd_fail s = Blomp.lift (Blorp.lift (Bleep.throw s))     *)



(*
 * err.ml - An Error Monad
 *)

open Monad
    
type 'a err =
  | Ok of 'a
  | Fail of string

module ErrMonad = MakeMonadError(struct

    type 'a t = 'a err

    let map f e =
      match e with
      | Ok x -> Ok (f x)
      | Fail s -> Fail s

    let pure a = Ok a

    let bind m f =
      match m with
      | Ok a -> f a
      | Fail s -> Fail s

  end)(struct

    type e = string

    let throw s = Fail s
    let catch m h =
      match m with
      | Fail s -> h s
      | Ok a -> Ok a

  end)


module ErrT(M : Monad) = struct
  type 'a t = ('a err) M.t 

  let pure a = M.pure (Ok a)
  let bind m f = M.bind m
      (function Fail s -> M.pure (Fail s)
              | Ok v -> f v)
  
end
  
(* module ExceptionTransf(M: MONAD) = struct
 *   type 'a outcome = V of 'a | E of exn
 *   type 'a mon = ('a outcome) M.mon
 * 
 *   let ret x = M.ret (V x)
 *   let bind m f = 
 *     M.bind m (function E e -> M.ret (E e) | V v -> f v)
 *   let lift x = M.bind x (fun v -> M.ret (V v))
 * 
 *   type 'a result = 'a M.result
 * 
 *   let run m = M.run (M.bind m (function V x -> M.ret x))
 * 
 *   let raise e = M.ret (E e)
 *   let trywith m f =
 *     M.bind m (function E e -> f e | V v -> M.ret (V v))
 * end *)


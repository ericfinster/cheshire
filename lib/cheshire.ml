(*
 * Cheshire.ml - Top Level Module for Cheshire
 *)

open Option

module OptionMnd =
struct
  
  let product o o' =
    match (o , o') with
    | Some x , Some y -> Some (x , y)
    |  _ -> None
      

  module Syntax =
  struct
    let (let+) x f = map f x
    let (and+) o o' = product o o'
    let (let*) m f = bind m f
  end
  
end

let head_opt l =
  match l with 
  | [] -> None
  | x::_ -> Some x

let tail_opt l =
  match l with
  | [] -> None
  | _::xs -> Some xs 

open OptionMnd.Syntax
       
let test l =
  let* x = head_opt l in
  let* xs = tail_opt l in
  let* y = head_opt xs in
  Some (x + y)
  




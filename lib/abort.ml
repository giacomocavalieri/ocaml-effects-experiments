open Comp

module type Abort = sig 
  (** [abort] aborts the current computation *)
  val abort : 'a comp

  (** [to_option c] handles a computation with the [Abort] effect turning it
  into an option *)
  val to_option : ('a comp) -> 'a option

  (** [to_option_comp c] is a delayed computation with the same result as [to_option c] *)
  val to_option_comp : ('a comp) -> ('a option) comp 

  (** [from_option opt] turns an optional value into a computation that aborts
      if [opt = None] or returns [a] if [opt = Some a] *)
  val from_option : 'a option -> 'a comp
end

module Abort: Abort = struct 
  open Effect
  open Effect.Deep 

  type _ t += Abort : 'a t 
  let abort () = perform Abort
  let to_option comp = match_with comp () {
    retc = Option.some;
    exnc = raise;
    effc = fun (type a) (eff : a t) -> match eff with 
      | Abort -> Some (Fun.const None)
      | _     -> None 
  }
  let to_option_comp comp () = to_option comp
  let from_option = function 
    | Some a -> Fun.const a 
    | None   -> abort
end 
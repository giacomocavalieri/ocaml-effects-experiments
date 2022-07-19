open Comp 

module type State = sig 
  (** [s] is the type of the state *)
  type s

  (** [put s] replaces the old state with [s] *)
  val put : s -> unit 

  (** [get ()] is the current state *)
  val get : s comp

  (** [gets f] is [f (get ())] *)
  val gets : (s -> 'a) -> 'a

  (** [modify f] changes the current state applying [f] to it *)
  val modify : (s -> s) -> unit

  (** [with_initial_state s0 comp] is the value produced by the stateful 
      computation [comp]; the initial state is [s0] *)
  val with_initial_state : s -> ('a comp) -> 'a

  (** [with_initial_state_comp s0 comp] is a delayed computation with the 
      same result as [with_initial_state s0 comp] *)
  val with_initial_state_comp : s -> ('a comp) -> 'a comp
end

module MakeState (S: sig type t end) : State with type s = S.t = struct 
  open Effect 

  type s = S.t 
  type _ t += 
    | Put : s -> unit t 
    | Get : s t
  let put s = perform (Put s)
  let get () = perform Get
  
  let gets f = f (get ())
  let modify f = put (gets f)

  let with_initial_state s0 comp = 
    let open Effect.Shallow in 
    let rec handler s = {
      retc = Fun.id;
      exnc = raise;
      effc = fun (type a) (eff : a t) -> match eff with 
        | Put s' -> Some (fun (k : (a, _) continuation) -> continue_with k () (handler s'))
        | Get    -> Some (fun (k : (a, _) continuation) -> continue_with k s  (handler s))
        | _      -> None
    }
    in continue_with (fiber comp) () (handler s0)

  let with_initial_state_comp s0 comp () = with_initial_state s0 comp
end 

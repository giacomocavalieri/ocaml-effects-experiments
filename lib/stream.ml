open Comp 

module type Stream = sig 
  (** [elem] is the type of elements emitted by the stream *)
  type elem

  (** [emit a] has the effect of adding [a] to a stream of elements *)
  val emit : elem -> unit 

  (** [filter p c] removes all the elements emitted by computation [c]
      that do not satisfy the predicate [p] *)
  val filter : (elem -> bool) -> 'a comp -> 'a comp

  (** [map f c] applies [f] to all the elements emitted by computation [c] *)
  val map : (elem -> elem) -> 'a comp -> 'a comp 

  (** [for_each f c] applies to all elements emitted by computation [c]
      the function [f] *)
  val for_each : (elem -> 'b) -> 'a comp -> 'a comp 

  (** [to_list c] handles any emit effect used in the computation
      [c] accumulating all values into a list *)
  val to_list : 'a comp -> elem list 

  (** [to_res c] ignores any value emitted by the computation [c]
      and simply returns its result *)
  val to_res : 'a comp -> 'a 

  (** [to_res_list c] accumulates all elements emitted by the
      computation [c] and returns its result *)
  val to_res_list : 'a comp -> 'a * elem list

  (** [to_list_comp c] is a delayed computation with the same result as [to_list c] *)
  val to_list_comp : 'a comp -> (elem list) comp

  (** [to_res_comp c] is a delayed computation with the same result as [to_res c] *)
  val to_res_comp : 'a comp -> 'a comp 

  (** [to_res_list_comp c] is a delayed computation with the same result as [to_res_list c] *)
  val to_res_list_comp : 'a comp -> ('a * elem list) comp
end

module MakeStream (S : sig type t end)
  : Stream with type elem = S.t 
= struct 
  open Effect

  type elem = S.t
  type _ t += Emit : elem -> unit t 
  let emit e = perform (Emit e)

  let for_each f stream_comp () =
    let open Effect.Deep
    in try_with stream_comp () {
      effc = fun (type a) (eff : a t) -> match eff with 
        | Emit e -> Some (fun (k : (a, _) continuation) -> ignore (f e); continue k ())
        | _      -> None 
    }

  let filter p = for_each (fun e -> if p e then emit e else ())
  let map f = for_each (fun e -> emit (f e))

  let to_res stream_comp = 
    let open Effect.Deep
    in try_with stream_comp () {
      effc = fun (type a) (eff : a t) -> match eff with 
        | Emit _ -> Some (fun (k : (a, _) continuation) -> continue k ())
        | _      -> None
    }

  let to_res_list stream_comp =
    let open Effect.Shallow in
    let rec handler acc = {
      retc = (fun a -> (a, acc));
      exnc = raise;
      effc = fun (type a) (eff : a t) -> match eff with 
        | Emit e -> Some 
          (fun (k: (a, _) continuation) -> continue_with k () (handler (e::acc)))
        | _      -> None 
    } in
    let (a, res) = continue_with (fiber stream_comp) () (handler [])
    in (a, List.rev res)

  let to_list stream_comp = to_res_list stream_comp |> snd  
  let to_list_comp comp () = to_list comp
  let to_res_comp comp () = to_res comp 
  let to_res_list_comp comp () = to_res_list comp
end
open Comp

(* A very bare-bone debugger that can be defined in user space, allows one
   to pause a computation logging a string and going back and forth the 
   breakpoints.
   Inspired by Paul Chiusano's Debug ability 
   https://share.unison-lang.org/@pchiusano/code/latest/namespaces/public/stepwise/latest *)
module type Debug = sig 
  val pause : string -> unit 
  val debug : 'a comp -> 'a 
  val debug_comp : 'a comp -> 'a comp
end

(* Does not work since OCaml continuations are one-shot! Every captured continuation 
   must be resumed exactly once with a continue or discontinue; so going back to the
   previously stored breakpoint results in a Continuation_already_taken exception :( 
   Would be cool if the continuations were not linear allowing one to go back and
   forth the debug breakpoints. *)
module Debug : Debug = struct 
  open Effect 

  type _ t += Pause : string -> unit t

  let pause log = perform (Pause log)

  let debug comp = 
    let open Effect.Shallow in
    let rec ignore = {
      retc = Fun.id;
      exnc = raise;
      effc = fun (type a) (eff : a t) -> match eff with 
        | Pause _ -> Some (fun (k : (a, _) continuation) -> continue_with k () ignore)
        | _       -> None 
    } in 
    let rec handler (curr : (unit, 'a) continuation) breaks = {
      retc = Fun.id;
      exnc = raise;
      effc = fun (type a) (eff : a t) -> match eff with 
        | Pause log -> 
          Some (fun (k : (a, _) continuation) ->
            print_endline ("Debug: " ^ log);
            print_endline "(s)tep, (b)ack, (i)gnore breakpoints";
            match read_line () with
              | "s" -> continue_with k () (handler k (curr::breaks))
              | "i" -> continue_with k () ignore
              | "b" -> (match breaks with 
                | prev::prevs -> continue_with prev () (handler prev prevs)
                | [] -> continue_with curr () (handler k breaks))
              | _ -> continue_with k () ignore
          )
        | _ -> None 
    } in
    let comp_fiber = fiber comp
    in continue_with comp_fiber () (handler comp_fiber [])

    let debug_comp comp () = debug comp
end
module StringStream = Effects.Stream.Make (struct type t = string end)
module IntState = Effects.State.Make (struct type t = int end)
open Effects.Abort
open Effects.Comp
open StringStream
open IntState

(* Code can be written in an "imperative" style removing the need for monadic binding and complex types *)
let effectful_computation () = 
  emit "first";
  if ~!get mod 2 = 0 then emit "even" else emit "odd";
  modify (fun i -> i * 2);
  emit (string_of_int ~!get);
  put 1;
  if ~!get = 2 then ~!abort else true

(* The handlers give semantic to the dsl described by the effect.
   Since effects are not checked one may forgot to handle all possible effects, that 
   would result in a runtime error; moreover, it's not apparent from the type 
   signature which effects a computation may need to use, it's all up to the programmer's
   self-discipline *)
let _ = 
  let open Printf in
  let (res, emitted) = effectful_computation
    |> to_option_comp         (* now the computation won't abort and produces a [bool option] *)
    |> to_res_list_comp       (* now all the emitted elements are stored in a list producing a [bool option * int list] *)
    |> with_initial_state 2   (* inject the initial state needed to run the computation *)
  in
    printf "Emitted elements: ";
    List.iter (printf "%s ") emitted;
    printf "\n";
    match res with 
      | Some b -> printf "Computation ended with result: %B\n" b 
      | None   -> printf "Computation aborted\n"



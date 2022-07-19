type 'a comp = unit -> 'a
let eval f = f ()
let (~!) f = f ()
let (~!!) f = eval f |> ignore
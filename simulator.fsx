open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let servers = (Array.create 10 0.0) |> Array.mapi (fun i x -> (i, x))
let clients = 1000000
let mutable arrival = 0.0
let mutable nstop = 0
let mutable allBusyTime = 0.0
let random = new Random()

seq { 1..clients } 
|> Seq.iter(fun x -> 
                let sample1 = -1.0 * 0.25 * log (1.0 - random.NextDouble())
                let ia = sample1
                arrival <- arrival + ia
                let sample2 = -1.0 * 2.4 * log (1.0 - random.NextDouble()) 
                let serviceTime = sample2 |> float 
                let selectedServer = servers |> Array.minBy snd
                if arrival < (snd selectedServer) then
                    nstop <- nstop + 1
                    allBusyTime <- allBusyTime + (snd selectedServer) - arrival
                else 
                    let index = fst selectedServer
                    let busyUntil = arrival + serviceTime
                    servers.[index] <- (index, busyUntil) 
                ())

let computeErlangB s a =  
    if s = 0.0 then 0.0
    else 
      let sum = seq { 1..a } |> Seq.fold (fun acc item -> (1.0 + acc) * ((item |> float)/s)) 0.0       
      1.0/(1.0 + sum)

printfn "Blocked clients %d" nstop
printfn "Total clients %d" clients
printfn "Ration %f" ((nstop |> float)/(clients |> float))
printfn ""

printfn "Total busy time %f" allBusyTime 
printfn "Total time %f" arrival
printfn "Ration %f" (allBusyTime/arrival)
printfn ""

//4 * 2.4 = 9.6 erlangs
printfn "Erlang B : %f" (computeErlangB 9.6 10) 

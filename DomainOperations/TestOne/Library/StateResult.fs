module DomainOperations.TestOne.Library.StateResult

open FSharp.Core
open DomainOperations.TestOne.Library.State

/// A combination State and Result Monad.
type StateResult<'State, 'Ok, 'Error> = State<'State, Result<'Ok, 'Error>>

/// A combination State and Result Monad.
module StateResult =
    /// Sets the state.
    let put x = State (fun _ -> (x, Ok ()))
    
    /// Gets the state.
    let get = State (fun s -> s, s)
    
    let bind (f: 'Ok -> StateResult<'State, 'NewOk, 'Error>) (m: StateResult<'State, 'Ok, 'Error>) =
        m
        |> State.bind (fun r ->
            match r with
            | Ok x -> f x
            | Error e -> State.ret (Error e)
        )
        
    let map (f: 'Ok -> 'NewOk) (m: StateResult<'State, 'Ok, 'Error>) =
        m
        |> bind (fun x ->
            State (fun s -> (s, Ok (f x)))
        )
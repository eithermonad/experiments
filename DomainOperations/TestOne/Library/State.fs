module DomainOperations.TestOne.Library.State

/// The State Monad.
type State<'State, 'Output> = State of ('State -> 'State * 'Output)

module State =
    let run state (State f) = f state
    
    let ret output = State (fun state -> (state, output))
    
    let bind f state =
        let stateFunc = fun s ->
            let state', output = state |> run s
            f output |> run state'
            
        State stateFunc
            
    let map f state =
        let stateFunc = fun s ->
            let state', output = state |> run s
            f output |> ret |> run state'
            
        State stateFunc
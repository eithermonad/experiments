module DomainOperations.TestOne.Library.Domain

open DomainOperations.TestOne.Library.Aggregate
open DomainOperations.TestOne.Library.State
open DomainOperations.TestOne.Library.StateResult

/// Type Alias for the Domain Monad.
type Domain<'State, 'Events, 'Error> = StateResult<Aggregate<'State, 'Events>, 'State, 'Error>

/// The Domain Monad
module Domain =
    /// Requires that the given conditional to be true. Fails if not.
    let requireTrue msg x = if x then State.ret (Ok ()) else State.ret (Error [msg])
    
    // Applies and records the domain event.
    let inline record (apply: 'Events -> 'State -> 'State) (eventCtor: 'a -> 'Events) (payload: 'a) =
        let stateFunc = fun (aggregate: Aggregate<'State, 'Events>) ->
            let event = eventCtor payload
            let state' = apply event aggregate.State
            
            let aggregate' = {
                State = state'
                DomainEvents = aggregate.DomainEvents @ [event] 
            }
            
            (aggregate', Ok state')
            
        State stateFunc
        
    // Applies and records the domain event and returns unit.
    let record' apply eventCtor payload =
        record apply eventCtor payload |> StateResult.map (fun _ -> ())
    
    /// Conditionally applies and records the event.
    let recordWhen conditional apply eventCtor payload =
        let stateFunc = fun (aggregate: Aggregate<'State, 'Events>) ->
            let event = eventCtor payload
            let state' = apply event aggregate.State
            
            match conditional with
            | true ->
                let aggregate' = {
                    State = state'
                    DomainEvents = aggregate.DomainEvents @ [event] 
                }
                
                (aggregate', Ok state')
            | false -> (aggregate, Ok aggregate.State)
            
        State stateFunc 
    
    
    /// For DSLs. Conditionally applies and records the event.
    let thenRecord apply eventCtor payload conditional = recordWhen conditional apply eventCtor payload
    
    /// For DSLs. Conditionally applies and records the event and returns unit.
    let thenRecord' apply eventCtor payload conditional =
        thenRecord apply eventCtor payload conditional |> StateResult.map (fun _ -> ())
    
    /// Right Sequence operator.
    let ( *> ) (x: State<'State, Result<'Ok, 'Error list>>) (y: State<'State, Result<'OkNew, 'Error list>>) =
        x
        |> State.bind (fun xr ->
            match xr with
            | Ok _ ->
                y
                |> State.bind (fun yr ->
                    match yr with
                    | Ok v -> State.ret (Ok v)
                    | Error e -> State.ret (Error e)
                )
            | Error e ->
                y
                |> State.bind (fun yr ->
                    match yr with
                    | Ok _ -> State.ret (Error e)
                    | Error e' -> State.ret (Error (e @ e'))
                )
        )
        
    /// Combines multiple.
    let combine (list: State<'a, Result<'b, 'c list>> list) = List.fold ( *> ) (State.ret (Ok ())) list
        
/// Builder
type DomainBuilder<'State, 'Events, 'Error>(initial: 'State) =
    member this.Return x = State (fun s -> (s, Ok x))
    member this.ReturnFrom x = x
    member this.Bind (m: StateResult<Aggregate<'State, 'Events>, 'Ok, 'Error>, f) = StateResult.bind f m
    member this.Zero () = State (fun s -> (s, Ok ()))
    member this.Combine(statefulA, statefulB) = StateResult.bind (fun _ -> statefulB) statefulA
    member this.Delay f = f ()
    member this.Run state =
        let state', output = state |> State.run (Aggregate.Initial<'State, 'Events> initial)
        output |> Result.map (fun _ -> state')
        
let domain<'State, 'Events, 'Error> c = DomainBuilder<'State, 'Events, 'Error> c
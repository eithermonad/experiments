module DomainOperations.TestOne.Library.Aggregate

/// Represents a DDD Aggregate
type Aggregate<'State, 'Event> = {
    State: 'State
    DomainEvents: 'Event list
} with
    /// Constructs an empty aggregate with the given initial state.
    static member Initial<'State, 'Event> (state: 'State) = {
        State = state
        DomainEvents = List.empty<'Event> 
    }
    
module Aggregate =
    let bind (f: 'State -> Aggregate<'NewState, 'Events>) (aggregate: Aggregate<'State, 'Events>) =
        let aggregate' = f aggregate.State
        {
            State = aggregate'.State
            DomainEvents = List.append aggregate'.DomainEvents aggregate.DomainEvents 
        } 
module AggregateConflictHandling.Program

/// Represents a failure outcome for the failure side of the Result monad.
/// This is an internal type for the Conflict Handling Computation Expression.
type ErrorOutcome<'a> =
    | Conflict
    | NonConflict of 'a

/// Represents an aggregate save conflict-aware Result monad.
/// This is an internal type for the Conflict Handling Computation Expression.
type ConflictResult<'T, 'E> = ConflictResult of Result<'T, ErrorOutcome<'E>>
    
/// This is an internal type for the Conflict Handling Computation Expression.
module ConflictResult =
    let bind f (ConflictResult(m)) =
        match m with
        | Ok x -> f x
        | Error x -> ConflictResult (Error x)
        
    let fromResult m = ConflictResult m
    
    let safeUnwrap (ConflictResult(m)) = m

/// Save Result out of a Repository. This public type can be used in application code.
type SaveResult<'T> =
    | Saved of 'T
    | SaveConflict
        
/// Save Result out of a Repository. This public type can be used in application code.
module SaveResult =
    let toConflictResult =
        function
            | Saved x -> ConflictResult (Ok x)
            | SaveConflict -> ConflictResult (Error Conflict)

/// Conflict Handling Computation Expression
type ConflictHandler() =
    member this.Return x = Ok x |> ConflictResult.fromResult
    member this.ReturnFrom x = x
    member this.Zero _ = this.Return ()
    member this.Delay f = f
    
    /// Entry into the CE for regular results. No conflict can have occurred by this point.
    member this.Bind(x: Result<'a, 'e>, f: 'a -> ConflictResult<'b, 'e>) : ConflictResult<'b, 'e> =
        let x' = x |> Result.mapError NonConflict |> ConflictResult.fromResult
        ConflictResult.bind f x'
    
    /// Entry into the CE for save results. No conflict can have occurred by this point. A conflict may occur
    /// when `f` is invoked.
    member this.Bind (x: Result<'a, 'e>, f: 'a -> SaveResult<'b>) : ConflictResult<'b, 'e> =
        let x' = x |> Result.mapError NonConflict |> ConflictResult.fromResult
        let f' = fun a -> f a |> SaveResult.toConflictResult
        
        ConflictResult.bind f' x'
        
    /// Bind for Conflict Results.
    member this.Bind (x: ConflictResult<'a, 'e>, f: 'a -> ConflictResult<'b, 'e>) : ConflictResult<'b, 'e> =
        ConflictResult.bind f x
        
    /// Not used, but useful to have (probably) in case the first ever bind involves a save.
    member this.Bind (x: SaveResult<'a>, f: 'a -> ConflictResult<'b, 'e>) : ConflictResult<'b, 'e> =
        x
        |> SaveResult.toConflictResult
        |> ConflictResult.bind f
     
    /// Throws upon exhaustion of retries to not expose conflict type.
    member this.Run f =
        let rec loop n =
            match ConflictResult.safeUnwrap (f ()) with
            | Ok x -> Ok x
            | Error (NonConflict e) -> Error e
            | Error Conflict ->
                match n with
                | 0 -> failwith "Exhausted retries"
                | _ ->
                    printfn "Rerunning"
                    loop (n - 1)

        loop 3
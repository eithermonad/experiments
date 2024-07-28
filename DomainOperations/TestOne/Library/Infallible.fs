module DomainOperations.TestOne.Library.Infallible

/// Used for indicating that a computation can't fail. Cannot be instantiated.
type Infallible = class end

/// A result that can't fail.
type InfallibleResult<'a> = Result<'a, Infallible>

module Infallible =
    let unwrap (m: InfallibleResult<'a>) =
        match m with
        | Ok v -> v
        | _ -> failwith "Impossible"
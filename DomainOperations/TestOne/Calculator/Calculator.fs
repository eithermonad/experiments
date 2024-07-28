module DomainOperations.TestOne.Calculator.Calculator

open DomainOperations.TestOne.Library.Infallible
open DomainOperations.TestOne.Library.Domain

type Added = int
type Subtracted = int

type Events =
    | Added of Added
    | Subtracted of Subtracted

type CalculatorState = int

let apply event (state: CalculatorState) =
    match event with
    | Added x -> state + x
    | Subtracted x -> state - x
    
let record eventCtor = Domain.record apply eventCtor
let record' eventCtor = Domain.record' apply eventCtor
let thenRecord eventCtor = Domain.thenRecord apply eventCtor
let thenRecord' eventCtor = Domain.thenRecord' apply eventCtor

module Calculator =
    let private domain<'Error> = domain<CalculatorState, Events, 'Error>
    
    let happyTestOne calculator = domain<Infallible> calculator {
        do! record' Added 4
        do! record' Subtracted 3
    }
    
    let happyTestTwo shouldDouble calculator = domain<Infallible> calculator {
        let! value = record Added 4
        let! value = record Subtracted 3
        
        if shouldDouble then
            do! record' Added value
    }
    
    let complexTest shouldDouble shouldTriple calculator = domain calculator {
        let! value = record Added 2
        let! value = record Subtracted 1
        
        let! value = shouldDouble |> thenRecord Added value
        let! value = shouldTriple |> thenRecord Added (value * 2)
        
        do! (value > 10) |> Domain.requireTrue "Value should be greater than 10"
        
        do! Domain.combine [
            (value > 20) |> Domain.requireTrue "Must be greater than 11"
            (value % 2 = 0) |> Domain.requireTrue "Must be even"
        ]
        
        do! record' Added 5
    }
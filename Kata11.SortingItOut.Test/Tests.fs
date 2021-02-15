module Tests

open FsUnit.Xunit
open Kata11.SortingItOut
open Xunit

[<Fact>]
let ``Sorting Balls - sorts numbers as they are added`` () =
    Rack.create |> should be Empty
    
    Rack.create
    |> Rack.add 20
    |> should equal [20]
    
    Rack.create
    |> Rack.add 20
    |> Rack.add 10
    |> should equal [10; 20]
    
    Rack.create
    |> Rack.add 20
    |> Rack.add 10
    |> Rack.add 30
    |> should equal [10; 20; 30]
    
[<Fact>]
let ``Sorting Characters - sorts all characters in a text in lowercase`` () =
    let text = "When not studying nuclear physics, Bambi likes to play\nbeach volleyball."
    let expected = "aaaaabbbbcccdeeeeeghhhiiiiklllllllmnnnnooopprsssstttuuvwyyyy"
    
    HiddenMessage.sortLetters text |> should equal expected
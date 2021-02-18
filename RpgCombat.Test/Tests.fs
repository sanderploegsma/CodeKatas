module RpgCombat.Tests

open RpgCombat
open Xunit

[<Fact>]
let ``Character initial health is 1000`` () =
    let sut = Character.create()
    Assert.Equal(1000, sut.Health)
    
[<Fact>]
let ``Character initial level is 1`` () =
    let sut = Character.create()
    Assert.Equal(1, sut.Level)
    
[<Fact>]
let ``Character initial state is alive`` () =
    let sut = Character.create()
    Assert.Equal(Alive, sut.State)
    
[<Fact>]
let ``Damaging a character subtracts the damage from their health`` () =
    let sut =
        Character.create()
        |> Character.damage 100
        
    Assert.Equal(900, sut.Health)
    
[<Fact>]
let ``Damaging a character for more than their health means the character dies`` () =
    let sut =
        Character.create()
        |> Character.damage 1100
        
    Assert.Equal(Dead, sut.State)
    
[<Fact>]
let ``Damaging a character for more than their health results in a health of 0`` () =
    let sut =
        Character.create()
        |> Character.damage 1100
        
    Assert.Equal(0, sut.Health)
    
[<Fact>]
let ``Healing an alive character adds the healing points to their health`` () =
    let sut =
        Character.create()
        |> Character.damage 500
        |> Character.heal 100
        
    Assert.Equal(600, sut.Health)
    
[<Fact>]
let ``Healing an alive character never heals above 1000 health`` () =
    let sut =
        Character.create()
        |> Character.heal 100
        
    Assert.Equal(1000, sut.Health)
    
[<Fact>]
let ``Healing a dead character does nothing`` () =
    let sut =
        Character.create()
        |> Character.damage 1000
        |> Character.heal 100
        
    Assert.Equal(Dead, sut.State)
    Assert.Equal(0, sut.Health)
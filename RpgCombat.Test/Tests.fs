module RpgCombat.Tests

open RpgCombat
open Xunit

[<Fact>]
let ``Character initial health is 1000`` () =
    let sut = CombatEngine.createCharacter()
    Assert.Equal(1000, sut.Health)
    
[<Fact>]
let ``Character initial level is 1`` () =
    let sut = CombatEngine.createCharacter()
    Assert.Equal(1, sut.Level)
    
[<Fact>]
let ``Character initial state is alive`` () =
    let sut = CombatEngine.createCharacter()
    Assert.Equal(Alive, sut.State)

[<Fact>]
let ``Damaging a character only damages the target`` () =
    let source = { Id = 1; Health = 1000; Level = 1; State = Alive }
    let target = { Id = 2; Health = 1000; Level = 1; State = Alive }
    let source', _ = CombatEngine.damage 100 source target        
    Assert.Equal(source, source')
    
[<Fact>]
let ``Damaging a character subtracts the damage from their health`` () =
    let source = { Id = 1; Health = 1000; Level = 1; State = Alive }
    let target = { Id = 2; Health = 1000; Level = 1; State = Alive }
    let _, target' = CombatEngine.damage 100 source target        
    Assert.Equal(900, target'.Health)
    
[<Fact>]
let ``Damaging a character for more than their health means the character dies`` () =
    let source = { Id = 1; Health = 1000; State = Alive; Level = 1 }
    let target = { Id = 2; Health = 90; State = Alive; Level = 1 }
    let _, target' = CombatEngine.damage 100 source target   
    Assert.Equal(Dead, target'.State)
    
[<Fact>]
let ``Damaging a character for more than their health results in a health of 0`` () =
    let source = { Id = 1; Health = 1000; State = Alive; Level = 1 }
    let target = { Id = 2; Health = 90; State = Alive; Level = 1 }
    let _, target' = CombatEngine.damage 100 source target   
    Assert.Equal(0, target'.Health)
    
[<Fact>]
let ``Damaging a character that is 5 or more levels lower increases damage by 50%`` () =
    let source = { Id = 1; Health = 1000; Level = 6; State = Alive }
    let target = { Id = 2; Health = 1000; Level = 1; State = Alive }
    let _, target' = CombatEngine.damage 100 source target
    Assert.Equal(850, target'.Health)
    
[<Fact>]
let ``Damaging a character that is 5 or more levels higher reduces damage by 50%`` () =
    let source = { Id = 1; Health = 1000; Level = 1; State = Alive }
    let target = { Id = 2; Health = 1000; Level = 6; State = Alive }
    let _, target' = CombatEngine.damage 100 source target
    Assert.Equal(950, target'.Health)
    
[<Fact>]
let ``Characters cannot damage themselves`` () =
    let character = CombatEngine.createCharacter()
    let source, target = CombatEngine.damage 100 character character
    Assert.Equal(character, source)
    Assert.Equal(character, target)
    
[<Fact>]
let ``Healing an alive character adds the healing points to their health`` () =
    let sut = { Id = 1; Health = 500; Level = 1; State = Alive } |> CombatEngine.heal 100
    Assert.Equal(600, sut.Health)
    
[<Fact>]
let ``Healing an alive character never heals above 1000 health`` () =
    let sut = { Id = 1; Health = 1000; Level = 1; State = Alive } |> CombatEngine.heal 100
    Assert.Equal(1000, sut.Health)
    
[<Fact>]
let ``Healing a dead character does nothing`` () =
    let sut = { Id = 1; Health = 0; Level = 1; State = Dead } |> CombatEngine.heal 100
        
    Assert.Equal(Dead, sut.State)
    Assert.Equal(0, sut.Health)
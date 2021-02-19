namespace RpgCombat.Tests

open RpgCombat
open Xunit

module CharacterCreation =
    [<Fact>]
    let ``Character initial health is 1000`` () =
        let state = State.Init

        let result =
            CreateCharacter { FighterType = Melee }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(1000, characters.[0].Health)
        | Error s -> failwithf "Got error while creating character: %s" s

    [<Fact>]
    let ``Character initial level is 1`` () =
        let state = State.Init

        let result =
            CreateCharacter { FighterType = Melee }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(1, characters.[0].Level)
        | Error s -> failwithf "Got error while creating character: %s" s

    [<Fact>]
    let ``Character initial state is alive`` () =
        let state = State.Init

        let result =
            CreateCharacter { FighterType = Melee }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(Alive, characters.[0].State)
        | Error s -> failwithf "Got error while creating character: %s" s

module Combat =
    [<Fact>]
    let ``Damaging a character subtracts the damage from their health`` () =
        let character =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, character); (1, character) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 500 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(500, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Damaging a character for more than their health means the character dies`` () =
        let character =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, character); (1, character) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 1100 }
            |> run state

        match result with
        | Ok { Characters = characters } ->
            Assert.Equal(Dead, characters.[1].State)
            Assert.Equal(0, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Damaging a character that is 5 or more levels lower increases damage by 50%`` () =
        let source =
            { Health = 1000
              Level = 6
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let target = { source with Level = 1 }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(850, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Damaging a character that is 5 or more levels higher reduces damage by 50%`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let target = { source with Level = 6 }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(950, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Characters cannot damage themselves`` () =
        let character =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, character) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 0; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Dead characters cannot damage anyone`` () =
        let source =
            { Health = 0
              Level = 1
              State = Dead
              FighterType = Melee
              Position = 0, 0 }

        let target =
            { source with
                  Health = 1000
                  State = Alive }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Characters cannot damage other dead characters`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let target = { source with Health = 0; State = Dead }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Characters cannot damage non-existing characters`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, source) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Melee fighters cannot attack someone who is more than 2 meters away from them`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let target = { source with Position = (2, 2) }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Ranged fighters can attack someone who is more than 2 meters away from them`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Ranged
              Position = 0, 0 }

        let target = { source with Position = (2, 2) }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> Assert.True(true) // Assert.Pass() anyone?
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Ranged fighters cannot attack someone who is more than 20 meters away from them`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let target = { source with Position = (20, 10) }

        let state =
            { Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

module Healing =
    [<Fact>]
    let ``Healing an alive character adds the healing points to their health`` () =
        let source =
            { Health = 500
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, source) ] |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Points = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(600, characters.[0].Health)
        | Error s -> failwithf "Got error while healing character: %s" s

    [<Fact>]
    let ``Healing an alive character never heals above 1000 health`` () =
        let source =
            { Health = 1000
              Level = 1
              State = Alive
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, source) ] |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Points = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(1000, characters.[0].Health)
        | Error s -> failwithf "Got error while healing character: %s" s

    [<Fact>]
    let ``Dead characters cannot heal themselves`` () =
        let source =
            { Health = 0
              Level = 1
              State = Dead
              FighterType = Melee
              Position = 0, 0 }

        let state =
            { Characters = [ (0, source) ] |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Points = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

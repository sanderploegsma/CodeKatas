namespace RpgCombat.Tests

open System.Net.Http
open RpgCombat
open Xunit

module TestHelpers =
    let defaultCharacter =
        { Health = 1000
          Level = 1
          State = Alive
          FighterType = Melee
          Position = 0, 0
          Factions = Set.empty }

    let defaultProp: Prop =
        { Type = "Tree"
          Health = 200
          Position = 0, 0 }

    let defaultState = State.Init

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

    [<Fact>]
    let ``Character does not belong to any faction when created`` () =
        let state = State.Init

        let result =
            CreateCharacter { FighterType = Melee }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Empty(characters.[0].Factions)
        | Error s -> failwithf "Got error while creating character: %s" s

module Combat =
    open TestHelpers

    [<Fact>]
    let ``Damaging a character subtracts the damage from their health`` () =
        let state =
            { defaultState with
                  Characters =
                      [ (0, defaultCharacter)
                        (1, defaultCharacter) ]
                      |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 500 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(500, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Damaging a character for more than their health means the character dies`` () =
        let state =
            { defaultState with
                  Characters =
                      [ (0, defaultCharacter)
                        (1, defaultCharacter) ]
                      |> Map.ofList }

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
        let source = { defaultCharacter with Level = 6 }
        let target = { defaultCharacter with Level = 1 }

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(850, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Damaging a character that is 5 or more levels higher reduces damage by 50%`` () =
        let source = { defaultCharacter with Level = 1 }
        let target = { defaultCharacter with Level = 6 }

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(950, characters.[1].Health)
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Characters cannot damage themselves`` () =
        let state =
            { defaultState with
                  Characters = [ (0, defaultCharacter) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 0; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Dead characters cannot damage anyone`` () =
        let source =
            { defaultCharacter with
                  State = Dead
                  Health = 0 }

        let target = defaultCharacter

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Characters cannot damage other dead characters`` () =
        let source = defaultCharacter

        let target =
            { defaultCharacter with
                  Health = 0
                  State = Dead }

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Characters cannot damage non-existing characters`` () =
        let state =
            { defaultState with
                  Characters = [ (0, defaultCharacter) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Melee fighters cannot attack someone who is more than 2 meters away from them`` () =
        let source =
            { defaultCharacter with
                  Position = 0, 0 }

        let target =
            { defaultCharacter with
                  Position = 2, 2 }

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Ranged fighters can attack someone who is more than 2 meters away from them`` () =
        let source =
            { defaultCharacter with
                  FighterType = Ranged
                  Position = 0, 0 }

        let target =
            { defaultCharacter with
                  Position = 2, 2 }

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> Assert.True(true) // Assert.Pass() anyone?
        | Error s -> failwithf "Got error while damaging character: %s" s

    [<Fact>]
    let ``Ranged fighters cannot attack someone who is more than 20 meters away from them`` () =
        let source =
            { defaultCharacter with
                  FighterType = Ranged
                  Position = 0, 0 }

        let target =
            { defaultCharacter with
                  Position = 20, 10 }

        let state =
            { defaultState with
                  Characters = [ (0, source); (1, target) ] |> Map.ofList }

        let result =
            DamageCharacter { Actor = 0; Target = 1; Damage = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

module Healing =
    open TestHelpers

    [<Fact>]
    let ``Healing an alive character adds the healing points to their health`` () =
        let character = { defaultCharacter with Health = 500 }

        let state =
            { defaultState with
                  Characters = [ (0, character) ] |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Target = 0; Health = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(600, characters.[0].Health)
        | Error s -> failwithf "Got error while healing character: %s" s

    [<Fact>]
    let ``Healing an alive character never heals above 1000 health`` () =
        let state =
            { defaultState with
                  Characters = [ (0, defaultCharacter) ] |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Target = 0; Health = 100 }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Equal(1000, characters.[0].Health)
        | Error s -> failwithf "Got error while healing character: %s" s

    [<Fact>]
    let ``Dead characters cannot heal themselves`` () =
        let character =
            { defaultCharacter with
                  State = Dead
                  Health = 0 }

        let state =
            { defaultState with
                  Characters = [ (0, character) ] |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Target = 0; Health = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

    [<Fact>]
    let ``Characters cannot heal other characters`` () =
        let state =
            { defaultState with
                  Characters =
                      [ (0, defaultCharacter)
                        (1, defaultCharacter) ]
                      |> Map.ofList }

        let result =
            HealCharacter { Actor = 0; Target = 1; Health = 100 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected an error but got ok"
        | Error _ -> Assert.True(true) // Assert.Pass() anyone?

module Factions =
    open TestHelpers

    [<Fact>]
    let ``Characters can join a faction`` () =
        let state =
            { defaultState with
                  Characters = [ (0, defaultCharacter) ] |> Map.ofList }

        let result =
            JoinFaction { Actor = 0; Faction = "Foo Fighters" }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.Contains("Foo Fighters", characters.[0].Factions)
        | Error s -> failwithf "Got error while joining faction: %s" s

    [<Fact>]
    let ``It is not possible for a non-existing character to join a faction`` () =
        let state =
            { defaultState with
                  Characters = Map.empty }

        let result =
            JoinFaction { Actor = 0; Faction = "Foo Fighters" }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Characters can leave a faction`` () =
        let character =
            { defaultCharacter with
                  Factions = Set.ofList [ "Foo Fighters" ] }

        let state =
            { defaultState with
                  Characters = [ (0, character) ] |> Map.ofList }

        let result =
            LeaveFaction { Actor = 0; Faction = "Foo Fighters" }
            |> run state

        match result with
        | Ok { Characters = characters } -> Assert.DoesNotContain("Foo Fighters", characters.[0].Factions)
        | Error s -> failwithf "Got error while joining faction: %s" s

    [<Fact>]
    let ``It is not possible for a non-existing character to leave a faction`` () =
        let state =
            { defaultState with
                  Characters = Map.empty }

        let result =
            LeaveFaction { Actor = 0; Faction = "Foo Fighters" }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Characters belonging to the same faction cannot damage each other`` () =
        let character1 =
            { defaultCharacter with
                  Factions =
                      Set.ofList [ "Foo Fighters"
                                   "Bar Fighters" ] }

        let character2 =
            { defaultCharacter with
                  Factions =
                      Set.ofList [ "Foo Fighters"
                                   "Baz Fighters" ] }

        let state =
            { defaultState with
                  Characters = [ (0, character1); (1, character2) ] |> Map.ofList }

        match DamageCharacter { Actor = 0; Target = 1; Damage = 10 }
              |> run state with
        | Ok _ -> failwithf "Expected an error but got none"
        | Error _ -> Assert.True(true)

        match DamageCharacter { Actor = 1; Target = 0; Damage = 10 }
              |> run state with
        | Ok _ -> failwithf "Expected an error but got none"
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Characters belonging to the same faction can heal each other`` () =
        let character1 =
            { defaultCharacter with
                  Health = 500
                  Factions =
                      Set.ofList [ "Foo Fighters"
                                   "Bar Fighters" ] }

        let character2 =
            { defaultCharacter with
                  Health = 400
                  Factions =
                      Set.ofList [ "Foo Fighters"
                                   "Baz Fighters" ] }

        let state =
            { defaultState with
                  Characters = [ (0, character1); (1, character2) ] |> Map.ofList }

        match HealCharacter { Actor = 0; Target = 1; Health = 100 }
              |> run state with
        | Ok { Characters = characters } ->
            Assert.Equal(500, characters.[0].Health)
            Assert.Equal(500, characters.[1].Health)
        | Error s -> failwithf "Got error while healing character: %s" s

        match HealCharacter { Actor = 1; Target = 0; Health = 100 }
              |> run state with
        | Ok { Characters = characters } ->
            Assert.Equal(600, characters.[0].Health)
            Assert.Equal(400, characters.[1].Health)
        | Error s -> failwithf "Got error while healing character: %s" s

module Props =
    open TestHelpers

    [<Fact>]
    let ``Creating a new prop`` () =
        let result =
            CreateProp
                { Type = defaultProp.Type
                  Health = defaultProp.Health
                  Position = defaultProp.Position }
            |> run defaultState

        match result with
        | Ok { Props = props } -> Assert.Equal(defaultProp, props.[0])
        | Error s -> failwithf "Got error while creating prop: %s" s

    [<Fact>]
    let ``It's not possible to create a prop with a non-positive health`` () =
        let result =
            CreateProp
                { Type = defaultProp.Type
                  Health = 0
                  Position = defaultProp.Position }
            |> run defaultState

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

        let result =
            CreateProp
                { Type = defaultProp.Type
                  Health = -10
                  Position = defaultProp.Position }
            |> run defaultState

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Characters can damage props`` () =
        let state =
            { defaultState with
                  Characters = [ (0, defaultCharacter) ] |> Map.ofList
                  Props = [ (0, defaultProp) ] |> Map.ofList }

        let result =
            DamageProp { Actor = 0; Target = 0; Damage = 100 }
            |> run state

        match result with
        | Ok { Props = props } -> Assert.Equal(100, props.[0].Health)
        | Error s -> failwithf "Got error while damaging prop: %s" s

    [<Fact>]
    let ``Props are destroyed when their health reaches zero`` () =
        let state =
            { defaultState with
                  Characters = [ (0, defaultCharacter) ] |> Map.ofList
                  Props = [ (0, defaultProp) ] |> Map.ofList }

        let result =
            DamageProp { Actor = 0; Target = 0; Damage = 200 }
            |> run state

        match result with
        | Ok { Props = props } -> Assert.Empty(props)
        | Error s -> failwithf "Got error while damaging prop: %s" s

    [<Fact>]
    let ``Characters cannot damage props that are out of range`` () =
        let character =
            { defaultCharacter with
                  Position = 0, 0 }

        let prop = { defaultProp with Position = 10, 10 }

        let state =
            { defaultState with
                  Characters = [ (0, character) ] |> Map.ofList
                  Props = [ (0, prop) ] |> Map.ofList }

        let result =
            DamageProp { Actor = 0; Target = 0; Damage = 200 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Characters cannot damage props that do not exist`` () =
        let character =
            { defaultCharacter with
                  Position = 0, 0 }

        let state =
            { defaultState with
                  Characters = [ (0, character) ] |> Map.ofList
                  Props = Map.empty }

        let result =
            DamageProp { Actor = 0; Target = 0; Damage = 200 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``It is not possible for a non-existing character to damage a prop`` () =
        let state =
            { defaultState with
                  Characters = Map.empty
                  Props = [ (0, defaultProp) ] |> Map.ofList }

        let result =
            DamageProp { Actor = 0; Target = 0; Damage = 200 }
            |> run state

        match result with
        | Ok _ -> failwithf "Expected error but got none"
        | Error _ -> Assert.True(true)

module GameSequence =
    open TestHelpers

    [<Fact>]
    let ``Full game sequence`` () =
        let commands =
            [ CreateProp
                { Type = "Tree"
                  Health = 500
                  Position = 1, 1 }
              CreateProp
                  { Type = "Crate"
                    Health = 10
                    Position = 5, 5 }
              CreateCharacter { FighterType = Melee } // Id = 0
              CreateCharacter { FighterType = Melee } // Id = 1
              CreateCharacter { FighterType = Ranged } // Id = 2

              DamageProp { Actor = 0; Target = 0; Damage = 100 } // Tree health - 100
              DamageProp { Actor = 2; Target = 1; Damage = 10 } // Destroy crate
              DamageCharacter { Actor = 0; Target = 1; Damage = 200 } // Player 1 health - 200 (to 800)
              DamageCharacter { Actor = 0; Target = 2; Damage = 150 } // Player 2 health - 150 (to 850)
              HealCharacter { Actor = 2; Target = 2; Health = 500 } // Player 2 health + 500 (to 1000)
              JoinFaction { Actor = 1; Faction = "Muppets" }
              DamageCharacter { Actor = 1; Target = 2; Damage = 200 } // Player 2 health - 200 (to 800)
              JoinFaction { Actor = 2; Faction = "Muppets" }
              HealCharacter { Actor = 2; Target = 1; Health = 500 } // Player 1 health + 500 (to 1000)
              DamageCharacter { Actor = 1; Target = 0; Damage = 300 } // Player 0 health - 300 (to 700)
              DamageCharacter { Actor = 2; Target = 0; Damage = 800 } ]

        let expectedState =
            { Props =
                  Map.ofList [ 0,
                               { Type = "Tree"
                                 Health = 400
                                 Position = 1, 1 } ]
              Characters =
                  Map.ofList [ 0,
                               { defaultCharacter with
                                     FighterType = Melee
                                     Health = 0
                                     State = Dead }
                               1,
                               { defaultCharacter with
                                     FighterType = Melee
                                     Health = 1000
                                     Factions = Set.ofList [ "Muppets" ] }
                               2,
                               { defaultCharacter with
                                     FighterType = Ranged
                                     Health = 800
                                     Factions = Set.ofList [ "Muppets" ] } ] }

        let apply (stateResult: Result<State, string>) command =
            Result.bind (fun state -> run state command) stateResult

        let result =
            commands |> List.fold apply (Ok State.Init)

        match result with
        | Ok state -> Assert.Equal(expectedState, state)
        | Error s -> failwithf "Got error in game sequence: %s" s

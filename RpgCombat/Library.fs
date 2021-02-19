module RpgCombat

type CharacterId = int

type CharacterState =
    | Alive
    | Dead

type FighterType =
    | Melee
    | Ranged

type Character =
    { FighterType: FighterType
      Health: int
      Level: int
      State: CharacterState
      Position: int * int }

type CreateCharacterCommand = { FighterType: FighterType }

type DamageCharacterCommand =
    { Actor: CharacterId
      Target: CharacterId
      Damage: int }

type HealCharacterCommand = { Actor: CharacterId; Points: int }

type CharacterCreatedEvent = { FighterType: FighterType }

type CharacterDamagedEvent =
    { Actor: CharacterId
      Target: CharacterId
      Points: int }

type CharacterHealedEvent = { Actor: CharacterId; Points: int }

type Command =
    | CreateCharacter of CreateCharacterCommand
    | DamageCharacter of DamageCharacterCommand
    | HealCharacter of HealCharacterCommand

type Event =
    | CharacterCreated of CharacterCreatedEvent
    | CharacterDamaged of CharacterDamagedEvent
    | CharacterHealed of CharacterHealedEvent

type State =
    { Characters: Map<CharacterId, Character> }
    static member Init = { Characters = Map.empty }

let private execute state command =
    let characterExists characterId =
        state.Characters.ContainsKey(characterId)

    let isAlive characterId =
        state.Characters.TryFind(characterId)
        |> Option.map (fun c -> c.State = Alive)
        |> Option.defaultValue false

    let isInRange sourceId targetId =
        let { FighterType = fighterType
              Position = (x1, y1) } =
            state.Characters.[sourceId]

        let { Position = (x2, y2) } = state.Characters.[targetId]

        let distance =
            pown (x2 - x1) 2 + pown (y2 - y1) 2
            |> float
            |> sqrt

        match fighterType with
        | Melee -> distance <= 2.0
        | Ranged -> distance <= 20.0

    match command with
    | CreateCharacter cmd -> Ok([ CharacterCreated { FighterType = cmd.FighterType } ])

    | DamageCharacter cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | DamageCharacter cmd when not (characterExists cmd.Target) -> Error "Unknown character"
    | DamageCharacter cmd when not (isAlive cmd.Actor) -> Error "Dead characters cannot damage other characters"
    | DamageCharacter cmd when not (isAlive cmd.Target) -> Error "Cannot damage character who is already dead"
    | DamageCharacter cmd when cmd.Actor = cmd.Target -> Error "You cannot attack yourself"
    | DamageCharacter cmd when not (isInRange cmd.Actor cmd.Target) -> Error "Target is out of range"
    | DamageCharacter cmd ->
        Ok
            ([ CharacterDamaged
                { Actor = cmd.Actor
                  Target = cmd.Target
                  Points = cmd.Damage } ])

    | HealCharacter cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | HealCharacter cmd when not (isAlive cmd.Actor) -> Error "Dead characters cannot heal"
    | HealCharacter cmd ->
        Ok
            ([ CharacterHealed
                { Actor = cmd.Actor
                  Points = cmd.Points } ])

let private applyDamage source target points =
    let multiplier =
        if source.Level >= target.Level + 5 then 1.5
        else if source.Level <= target.Level - 5 then 0.5
        else 1.0

    let trueDamage = (float points) * multiplier |> int // TODO: should health be a float?


    if trueDamage < target.Health then
        { target with
              Health = target.Health - trueDamage }
    else
        { target with Health = 0; State = Dead }

let private applyHeal target points =
    let newHealth = min 1000 (target.Health + points)
    { target with Health = newHealth }

let private apply state event =
    match event with
    | CharacterCreated evt ->
        let id = Map.count state.Characters

        let character =
            { FighterType = evt.FighterType
              Health = 1000
              Level = 1
              State = Alive
              Position = 0, 0 }

        { state with
              Characters = Map.add id character state.Characters }
    | CharacterDamaged evt ->
        let character =
            applyDamage state.Characters.[evt.Actor] state.Characters.[evt.Target] evt.Points

        { state with
              Characters = Map.add evt.Target character state.Characters }
    | CharacterHealed evt ->
        let character =
            applyHeal state.Characters.[evt.Actor] evt.Points

        { state with
              Characters = Map.add evt.Actor character state.Characters }

let run state command =
    execute state command
    |> Result.map (List.fold apply state)

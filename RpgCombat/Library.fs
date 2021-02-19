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
      Position: int * int
      Factions: Set<string> }

type PropId = int

type Prop =
    { Type: string
      Health: int
      Position: int * int }

type CreateCharacterCommand = { FighterType: FighterType }

type CreatePropCommand =
    { Type: string
      Health: int
      Position: int * int }

type DamageCharacterCommand =
    { Actor: CharacterId
      Target: CharacterId
      Damage: int }

type DamagePropCommand =
    { Actor: CharacterId
      Target: PropId
      Damage: int }

type HealCharacterCommand =
    { Actor: CharacterId
      Target: CharacterId
      Health: int }

type JoinFactionCommand = { Actor: CharacterId; Faction: string }

type LeaveFactionCommand = { Actor: CharacterId; Faction: string }

type CharacterCreatedEvent = { FighterType: FighterType }

type PropCreatedEvent =
    { Type: string
      Health: int
      Position: int * int }

type CharacterDamagedEvent =
    { Actor: CharacterId
      Target: CharacterId
      Damage: int }

type PropDamagedEvent =
    { Actor: CharacterId
      Target: PropId
      Damage: int }

type CharacterHealedEvent =
    { Actor: CharacterId
      Target: CharacterId
      Health: int }

type CharacterJoinedFactionEvent = { Actor: CharacterId; Faction: string }

type CharacterLeftFactionEvent = { Actor: CharacterId; Faction: string }

type Command =
    | CreateCharacter of CreateCharacterCommand
    | CreateProp of CreatePropCommand
    | DamageCharacter of DamageCharacterCommand
    | DamageProp of DamagePropCommand
    | HealCharacter of HealCharacterCommand
    | JoinFaction of JoinFactionCommand
    | LeaveFaction of LeaveFactionCommand

type Event =
    | CharacterCreated of CharacterCreatedEvent
    | PropCreated of PropCreatedEvent
    | CharacterDamaged of CharacterDamagedEvent
    | PropDamaged of PropDamagedEvent
    | CharacterHealed of CharacterHealedEvent
    | CharacterJoinedFaction of CharacterJoinedFactionEvent
    | CharacterLeftFaction of CharacterLeftFactionEvent

type State =
    { Characters: Map<CharacterId, Character>
      Props: Map<PropId, Prop> }
    static member Init =
        { Characters = Map.empty
          Props = Map.empty }

let private execute state command =
    let characterExists characterId =
        state.Characters.ContainsKey(characterId)

    let propExists propId = state.Props.ContainsKey(propId)

    let isAlive characterId =
        state.Characters.TryFind(characterId)
        |> Option.map (fun c -> c.State = Alive)
        |> Option.defaultValue false

    let isInRange sourceId (x2, y2) =
        let { FighterType = fighterType
              Position = (x1, y1) } =
            state.Characters.[sourceId]

        let distance =
            pown (x2 - x1) 2 + pown (y2 - y1) 2
            |> float
            |> sqrt

        match fighterType with
        | Melee -> distance <= 2.0
        | Ranged -> distance <= 20.0

    let targetCharacterIsInRange sourceId targetId =
        let position = state.Characters.[targetId].Position
        isInRange sourceId position

    let targetPropIsInRange sourceId targetId =
        let position = state.Props.[targetId].Position
        isInRange sourceId position

    let areAllies sourceId targetId =
        let { Factions = sourceFactions } = state.Characters.[sourceId]
        let { Factions = targetFactions } = state.Characters.[targetId]

        Set.intersect sourceFactions targetFactions
        |> Set.count > 0

    match command with
    | CreateCharacter cmd -> Ok([ CharacterCreated { FighterType = cmd.FighterType } ])

    | CreateProp cmd when cmd.Health <= 0 -> Error "Prop must have positive health"
    | CreateProp cmd ->
        Ok
            ([ PropCreated
                { Type = cmd.Type
                  Health = cmd.Health
                  Position = cmd.Position } ])

    | DamageCharacter cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | DamageCharacter cmd when not (characterExists cmd.Target) -> Error "Unknown character"
    | DamageCharacter cmd when not (isAlive cmd.Actor) -> Error "Dead characters cannot damage other characters"
    | DamageCharacter cmd when not (isAlive cmd.Target) -> Error "Cannot damage character who is already dead"
    | DamageCharacter cmd when cmd.Actor = cmd.Target -> Error "You cannot attack yourself"
    | DamageCharacter cmd when not (targetCharacterIsInRange cmd.Actor cmd.Target) -> Error "Target is out of range"
    | DamageCharacter cmd when areAllies cmd.Actor cmd.Target -> Error "Allies cannot damage each other"
    | DamageCharacter cmd ->
        Ok
            ([ CharacterDamaged
                { Actor = cmd.Actor
                  Target = cmd.Target
                  Damage = cmd.Damage } ])

    | DamageProp cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | DamageProp cmd when not (propExists cmd.Target) -> Error "Unknown prop"
    | DamageProp cmd when not (isAlive cmd.Actor) -> Error "Dead characters cannot damage props"
    | DamageProp cmd when not (targetPropIsInRange cmd.Actor cmd.Target) -> Error "Target is out of range"
    | DamageProp cmd ->
        Ok
            ([ PropDamaged
                { Actor = cmd.Actor
                  Target = cmd.Target
                  Damage = cmd.Damage } ])

    | HealCharacter cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | HealCharacter cmd when not (isAlive cmd.Actor) -> Error "Dead characters cannot heal"
    | HealCharacter cmd when not (areAllies cmd.Actor cmd.Target)
                             && cmd.Actor <> cmd.Target -> Error "Characters can only heal themselves or their allies"
    | HealCharacter cmd ->
        Ok
            ([ CharacterHealed
                { Actor = cmd.Actor
                  Target = cmd.Target
                  Health = cmd.Health } ])

    | JoinFaction cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | JoinFaction cmd ->
        Ok
            ([ CharacterJoinedFaction
                { Actor = cmd.Actor
                  Faction = cmd.Faction } ])

    | LeaveFaction cmd when not (characterExists cmd.Actor) -> Error "Unknown character"
    | LeaveFaction cmd ->
        Ok
            ([ CharacterLeftFaction
                { Actor = cmd.Actor
                  Faction = cmd.Faction } ])

let private applyDamage source target damage =
    let multiplier =
        if source.Level >= target.Level + 5 then 1.5
        else if source.Level <= target.Level - 5 then 0.5
        else 1.0

    let trueDamage = (float damage) * multiplier |> int // TODO: should health be a float?


    if trueDamage < target.Health then
        { target with
              Health = target.Health - trueDamage }
    else
        { target with Health = 0; State = Dead }

let private applyHeal (target: Character) health =
    let newHealth = min 1000 (target.Health + health)
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
              Position = 0, 0
              Factions = Set.empty }

        { state with
              Characters = Map.add id character state.Characters }

    | PropCreated evt ->
        let id = Map.count state.Props

        let prop: Prop =
            { Type = evt.Type
              Health = evt.Health
              Position = evt.Position }

        { state with
              Props = Map.add id prop state.Props }

    | CharacterDamaged evt ->
        let character =
            applyDamage state.Characters.[evt.Actor] state.Characters.[evt.Target] evt.Damage

        { state with
              Characters = Map.add evt.Target character state.Characters }

    | PropDamaged evt ->
        let prop = state.Props.[evt.Target]
        let newHealth = prop.Health - evt.Damage

        if newHealth > 0 then
            { state with
                  Props = Map.add evt.Target { prop with Health = newHealth } state.Props }
        else
            { state with
                  Props = Map.remove evt.Target state.Props }

    | CharacterHealed evt ->
        let character =
            applyHeal state.Characters.[evt.Target] evt.Health

        { state with
              Characters = Map.add evt.Target character state.Characters }

    | CharacterJoinedFaction evt ->
        let character = state.Characters.[evt.Actor]

        let updatedCharacter =
            { character with
                  Factions = Set.add evt.Faction character.Factions }

        { state with
              Characters = Map.add evt.Actor updatedCharacter state.Characters }

    | CharacterLeftFaction evt ->
        let character = state.Characters.[evt.Actor]

        let updatedCharacter =
            { character with
                  Factions = Set.remove evt.Faction character.Factions }

        { state with
              Characters = Map.add evt.Actor updatedCharacter state.Characters }

let run state command =
    execute state command
    |> Result.map (List.fold apply state)

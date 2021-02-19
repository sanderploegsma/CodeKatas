module RpgCombat.Engine

open RpgCombat.Domain

type State =
    { Characters: Map<CharacterId, Character>
      Props: Map<PropId, Prop> }
    static member Init =
        { Characters = Map.empty
          Props = Map.empty }

    member this.containsCharacter id = this.Characters.ContainsKey(id)

    member this.containsProp id = this.Props.ContainsKey(id)

let private validateAction (state: State) actorId action =
    let isAlive characterId =
        state.Characters.[characterId].State = Alive

    let isInRange (x2, y2) =
        let { FighterType = fighterType
              Position = (x1, y1) } =
            state.Characters.[actorId]

        let distance =
            pown (x2 - x1) 2 + pown (y2 - y1) 2
            |> float
            |> sqrt

        match fighterType with
        | Melee -> distance <= 2.0
        | Ranged -> distance <= 20.0

    let targetCharacterIsInRange targetId =
        let position = state.Characters.[targetId].Position
        isInRange position

    let targetPropIsInRange targetId =
        let position = state.Props.[targetId].Position
        isInRange position

    let areAllies targetId =
        let { Factions = sourceFactions } = state.Characters.[actorId]
        let { Factions = targetFactions } = state.Characters.[targetId]

        Set.intersect sourceFactions targetFactions
        |> Set.count > 0

    match action with
    | DamageProp (id, _) when not (state.containsProp id) -> Error "Unknown prop"
    | DamageProp (id, _) when not (targetPropIsInRange id) -> Error "Prop is not in range"
    | DamageCharacter (id, _) when not (state.containsCharacter id) -> Error "Unknown character"
    | DamageCharacter (id, _) when actorId = id -> Error "Characters cannot damage themselves"
    | DamageCharacter (id, _) when not (isAlive id) -> Error "Cannot damage dead characters"
    | DamageCharacter (id, _) when not (targetCharacterIsInRange id) -> Error "Character is not in range"
    | DamageCharacter (id, _) when areAllies id -> Error "Allies cannot damage each other"
    | HealCharacter (id, _) when not (state.containsCharacter id) -> Error "Unknown character"
    | HealCharacter (id, _) when not (isAlive id) -> Error "Cannot heal dead characters"
    | HealCharacter (id, _) when actorId <> id && not (areAllies id) ->
        Error "Characters can only heal themselves or allies"
    | _ -> Ok([ ActionPerformed { Actor = actorId; Action = action } ])

let private execute state command =
    let isAlive characterId =
        state.Characters.[characterId].State = Alive

    match command with
    | CreateCharacter cmd -> Ok([ CharacterCreated { FighterType = cmd.FighterType } ])

    | CreateProp cmd when cmd.Health <= 0 -> Error "Prop must have positive health"
    | CreateProp cmd ->
        Ok
            ([ PropCreated
                { Type = cmd.Type
                  Health = cmd.Health
                  Position = cmd.Position } ])

    | PerformAction cmd when not (state.containsCharacter cmd.Actor) -> Error "Unknown character"
    | PerformAction cmd when not (isAlive cmd.Actor) -> Error "Dead characters cannot perform actions"
    | PerformAction cmd -> validateAction state cmd.Actor cmd.Action

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

let private applyAction state actorId action =
    let actor = state.Characters.[actorId]

    match action with
    | DamageProp (id, damage) ->
        let prop = state.Props.[id]
        let newHealth = prop.Health - damage

        if newHealth > 0 then
            { state with
                  Props = Map.add id { prop with Health = newHealth } state.Props }
        else
            { state with
                  Props = Map.remove id state.Props }
    | DamageCharacter (id, damage) ->
        let character =
            applyDamage actor state.Characters.[id] damage

        { state with
              Characters = Map.add id character state.Characters }
    | HealCharacter (id, health) ->
        let character = applyHeal state.Characters.[id] health

        { state with
              Characters = Map.add id character state.Characters }
    | JoinFaction faction ->
        let character =
            { actor with
                  Factions = Set.add faction actor.Factions }

        { state with
              Characters = Map.add actorId character state.Characters }
    | LeaveFaction faction ->
        let character =
            { actor with
                  Factions = Set.remove faction actor.Factions }

        { state with
              Characters = Map.add actorId character state.Characters }

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

    | ActionPerformed evt -> applyAction state evt.Actor evt.Action

let run state command =
    execute state command
    |> Result.map (List.fold apply state)

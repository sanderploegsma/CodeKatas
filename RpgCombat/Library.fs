module RpgCombat

type CharacterState =
    | Alive
    | Dead

type Character =
    { Id: int
      Health: int
      Level: int
      State: CharacterState }

[<RequireQualifiedAccess>]
module CombatEngine =
    let mutable characters = 0

    let createCharacter () =
        characters <- characters + 1

        { Id = characters
          Health = 1000
          Level = 1
          State = Alive }

    let damage (points: int) (source: Character) (target: Character) =
        let multiplier =
            if source.Level >= target.Level + 5 then 1.5
            else if source.Level <= target.Level - 5 then 0.5
            else 1.0

        let trueDamage = (float points) * multiplier |> int // TODO: should health be a float?

        if source.Id = target.Id then
            source, target
        else if trueDamage < target.Health then
            source,
            { target with
                  Health = target.Health - trueDamage }
        else
            source, { target with Health = 0; State = Dead }

    let heal (points: int) (target: Character) =
        match target.State with
        | Dead -> target
        | Alive ->
            { target with
                  Health = min 1000 (target.Health + points) }

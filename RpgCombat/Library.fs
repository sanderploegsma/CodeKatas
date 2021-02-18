module RpgCombat

type CharacterState = Alive | Dead

type Character =
    {
        Health: int
        Level: int
        State: CharacterState
    }
    
[<RequireQualifiedAccess>]
module Character =
    let create () =
        { Health = 1000; Level = 1; State = Alive }
        
    let damage points character =
        if points < character.Health then
            { character with Health = character.Health - points }
        else
            { character with Health = 0; State = Dead }
            
    let heal points character =
        match character.State with
        | Dead -> character
        | Alive -> { character with Health = min 1000 (character.Health + points) }
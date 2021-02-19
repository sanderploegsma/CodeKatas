module RpgCombat.Domain

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

type Action =
    | DamageCharacter of target: CharacterId * damage: int
    | DamageProp of target: PropId * damage: int
    | HealCharacter of target: CharacterId * health: int
    | JoinFaction of string
    | LeaveFaction of string

type ActionCommand =
    { Actor: CharacterId
      Action: Action }

type CharacterCreatedEvent = { FighterType: FighterType }

type PropCreatedEvent =
    { Type: string
      Health: int
      Position: int * int }

type ActionEvent =
    { Actor: CharacterId
      Action: Action }

type Command =
    | CreateCharacter of CreateCharacterCommand
    | CreateProp of CreatePropCommand
    | PerformAction of ActionCommand

type Event =
    | CharacterCreated of CharacterCreatedEvent
    | PropCreated of PropCreatedEvent
    | ActionPerformed of ActionEvent

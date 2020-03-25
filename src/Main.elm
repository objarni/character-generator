module Main exposing (main)

import Array
import Browser
import CharacterData
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Random
import Random.List


type alias Model =
    { character : Character
    , seed : Random.Seed
    }


type alias Character =
    { age : Int
    , occupation : String
    , sex : String
    , temperament : String
    , nameIndex : Int
    }


type Msg
    = SetCharacter Character


main : Program () Model Msg
main =
    Browser.element
        { init = initial
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initial () =
    let
        seed =
            Random.initialSeed 239847
    in
    ( { character =
            { age = 0
            , occupation = ""
            , sex = ""
            , temperament = ""
            , nameIndex = 0
            }
      , seed = seed
      }
    , Random.generate SetCharacter randomCharacter
    )



-- @remind https://en.wikipedia.org/wiki/Four_temperaments


view { character } =
    let
        { age, occupation, sex, temperament, nameIndex } =
            character

        nameList =
            if sex == "female" then
                femaleNames

            else
                maleNames

        name =
            nth nameList nameIndex
    in
    layout [ height fill ] <|
        column [ padding 50 ]
            [ text <| "Name: " ++ name
            , text <| "Age: " ++ String.fromInt age
            , text <| "Sex: " ++ sex
            , text <| "Occupation: " ++ occupation
            , text <| "Temperament: " ++ temperament
            ]


occupations =
    Array.fromList
        CharacterData.occupations


sexes =
    Array.fromList
        [ "female", "male" ]


temperaments =
    Array.fromList
        [ "sanguine"
        , "choleric"
        , "melancholic"
        , "phlegmatic"
        ]


femaleNames =
    Array.fromList
        CharacterData.femaleNames


maleNames =
    Array.fromList
        CharacterData.maleNames


nth : Array.Array String -> Int -> String
nth array ix =
    Maybe.withDefault "HUH?" (Array.get ix array)


randomCharacter : Random.Generator Character
randomCharacter =
    Random.map5
        (\age n m i j ->
            { age = age
            , occupation = nth occupations n
            , sex = nth sexes m
            , temperament = nth temperaments i
            , nameIndex = j
            }
        )
        (Random.int 5 75)
        (Random.int 0 (Array.length occupations - 1))
        (Random.int 0 1)
        (Random.int 0 (Array.length temperaments - 1))
        (Random.int 0 (Array.length femaleNames - 1))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCharacter character ->
            ( { model | character = character }
            , Cmd.none
            )

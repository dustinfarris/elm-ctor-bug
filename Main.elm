module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, hr, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { people : Dict String Person
    , newId : String
    , newName : String
    }


type alias Person =
    { id : String
    , name : String
    }



-- INIT


model : Model
model =
    { people = Dict.empty
    , newId = ""
    , newName = ""
    }



-- UPDATE


type Msg
    = SetNewId String
    | SetNewName String
    | CreatePerson


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNewId value ->
            { model | newId = value }

        SetNewName value ->
            { model | newName = value }

        CreatePerson ->
            let
                ( people, ids ) =
                    model.people
                        |> addPeople [ (Person model.newId model.newName) ]
            in
                { model
                    | people = people
                    , newId = ""
                    , newName = ""
                }


mergePeople : Person -> Maybe Person -> Maybe Person
mergePeople newPerson maybeOldPerson =
    case maybeOldPerson of
        Just oldPerson ->
            Just { oldPerson | name = newPerson.name }

        Nothing ->
            Just (Person newPerson.id newPerson.name)


{-| Combine two lists of Person, return the combined list and a list
    of ids that were upserted.
-}
addPeople : List Person -> Dict String Person -> ( Dict String Person, List String )
addPeople newPeople currentPeople =
    let
        ( people, ids ) =
            newPeople
                |> List.foldl
                    (\person ( people_____, ids ) ->
                        ( Dict.update person.id (mergePeople person) people
                        , person.id :: ids
                        )
                    )
                    ( currentPeople, [] )
    in
        ( people, ids )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "People" ]
        , ul [] <|
            List.map
                (\p -> li [] [ text (p.id ++ " / " ++ p.name) ])
                (Dict.values model.people)
        , hr [] []
        , h2 [] [ text "Add a person" ]
        , input [ placeholder "ID", value model.newId, onInput SetNewId ] []
        , input [ placeholder "Name", value model.newName, onInput SetNewName ] []
        , button [ onClick CreatePerson ] [ text "Create Person" ]
        ]

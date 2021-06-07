port module Client exposing (..)

-- show suggestions
-- send out suggestions when clicked

import Html
import Html.Attributes
import Html.Events
import Model
import Browser
import Main

import Http
import Json.Decode as Decode

main : Platform.Program () Model Msg
main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = always (gotReport GotReport)
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotServerResponse res ->
            ({model | serverConnected = if Result.toMaybe res == Nothing then False else True}, Cmd.none)
        GotReport val ->
            ( {model | report = Decode.decodeValue (Decode.field "report" Model.readReport) val
            |> Just}
            , Cmd.none
            )
        SuggestionChosen id ->
            (model
            , requestApplication id
            )

requestApplication : Model.Application -> Cmd Msg
requestApplication id =
    let
        body = Model.encodeApplication id
    in
        Http.post
            {url = "http://localhost:8787"
            , body = Http.jsonBody body
            , expect = Http.expectWhatever GotServerResponse
            }

connect : Cmd Msg
connect =
        Http.get
            {url = "http://localhost:8787"
            , expect = Http.expectWhatever GotServerResponse
            }


init : () -> (Model, Cmd Msg)
init flags = ({report = Nothing, serverConnected = False }, connect)

view : Model -> Html.Html Msg
view model =
    case model.serverConnected of
        False -> Html.text "Server disconnected"
        True ->
            case model.report of
                Nothing -> Html.text "No report available. Wait for the compiler to finish."
                Just (Err err) -> Html.text (Debug.toString err)
                Just (Ok r) -> viewReport r

type Msg =
    GotReport Decode.Value
    | GotServerResponse (Result Http.Error ())
    | SuggestionChosen Model.Application

type alias Model =
    { serverConnected : Bool
    , report : Maybe (Result Decode.Error Model.Report)
    }

type alias SuggestionId =
    {path : String, problemNumber : Int}

port gotReport : (Decode.Value -> msg) -> Sub msg

viewReport : Model.Report -> Html.Html Msg
viewReport r =
    case r of
        Model.NoMain -> Html.pre [] [Html.text "Main file not found.\nUse ELM_MAIN=src/MyMain.elm if src/Main.elm does not exist."]
        Model.Compiled -> Html.div [] [Html.text "COMPILED"]
        Model.Errored errors ->
            errors
            |> List.sortBy .path
            |> List.map viewError
            |> Html.div []

viewError : Model.CompileError -> Html.Html Msg
viewError ({problems, name, path} as file) =
    let
        renderedProblems =
            problems
            |> List.sortWith (\a b ->
                    case compare a.region.start.line a.region.start.line of
                        EQ -> compare a.region.start.column a.region.start.column
                        notEq -> notEq
                )
            |> List.map ((\p -> viewProblem file p |> Html.map (Main.applySuggestion file p >> SuggestionChosen)))
    in
    Html.div [] [
        Html.div [] [Html.text name]
      , Html.h3 [] [Html.text path]
      , Html.div [] renderedProblems
    ]

viewProblem : Model.CompileError -> Model.Problem -> Html.Html Model.Suggestion
viewProblem error p =
    let
        details =
            p.message
            |> List.map (viewMessageLine error p)
            |> Html.pre []
    in
    Html.div [] [
        Html.h3 [] [Html.text (Debug.toString p.type_) ]
      , details
    ]

viewMessageLine : Model.CompileError -> Model.Problem -> Model.MessagePart -> Html.Html Model.Suggestion
viewMessageLine error problem l =
    let
        style = (::) (Html.Attributes.class "message-span")  <| case l.formatting of
            Nothing -> []
            Just {bold, underline, color} ->
                let
                    fontStyle =
                        Html.Attributes.style "font-style" (if bold then "bold " else "" ++ if underline then "underline" else "")
                    fontColor =
                        case color of
                            Nothing -> []
                            Just c -> [Html.Attributes.attribute "style"  ("--color: " ++ String.toLower c)]
                in
                    fontStyle :: fontColor
    in
    case Main.findFixForMessage error problem l of
        Nothing -> Html.span style [Html.text l.string]
        Just suggestion ->
            suggestion
            |> List.map (\(subString, sug) ->
                Html.span (case sug of
                    Nothing -> []
                    Just s -> [Html.Attributes.title (Debug.toString s), Html.Attributes.class "has-suggestion" , Html.Events.onClick s]) [Html.text subString]
                )
            |> Html.span style

port module Main exposing (..)

import Browser
import Browser.Navigation
import Html exposing (Html, br, button, div, form, h1, h3, input, label, text)
import Html.Attributes exposing (for, id, name, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, andThen, bool, fail, field, int, list, map, map3, map4, map5, map6, string)
import Json.Encode as JE
import Url


type alias Model =
    { authToken : Maybe String
    , simpleSources : List SimpleSource
    , homes : List Home

    -- Sink form
    , influxSinks : List InfluxSink
    , influxSinkFormHost : String
    , influxSinkFormPort : String
    , influxSinkFormTLS : String
    , influxSinkFormUsername : String
    , influxSinkFormPassword : String

    -- Home form
    , homeDatakey : String

    -- Simple source form
    , simpleSourceDatakey : String
    , simpleSourceUrl : String
    , simpleSourceAuthHeader : String
    , counter : Int
    , tagMappings : List Mapping
    , fieldMappings : List Mapping
    }


type alias Mapping =
    { id : Int, key : String, value : String }


emptyMapping : Int -> Mapping
emptyMapping id =
    { id = id, key = "", value = "" }


type alias Home =
    { id : String, datakey : String, state : String, createdAt : String }


type alias InfluxSink =
    { id : String, influxHost : String, influxPort : Int, influxTLS : Bool, createdAt : String }


type alias SimpleSource =
    { id : String, datakey : String, createdAt : String, url : String, tagMappings : List (List String), fieldMappings : List (List String) }


main : Program (Maybe String) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


port wsReceiver : (JE.Value -> msg) -> Sub msg


initialState : Model
initialState =
    { authToken = Nothing
    , simpleSources = []
    , homes = []
    , influxSinks = []
    , influxSinkFormHost = ""
    , influxSinkFormPort = ""
    , influxSinkFormTLS = "true"
    , influxSinkFormUsername = ""
    , influxSinkFormPassword = ""
    , homeDatakey = ""
    , simpleSourceDatakey = ""
    , simpleSourceUrl = ""
    , simpleSourceAuthHeader = ""
    , counter = 0
    , tagMappings = []
    , fieldMappings = []
    }


init : Maybe String -> ( Model, Cmd Msg )
init token =
    case token of
        Just t ->
            ( { initialState | authToken = token }
            , Cmd.batch [ getHomes t, getSimpleSources t, getInfluxSinks t ]
            )

        Nothing ->
            ( initialState, Cmd.none )


type Msg
    = GotHomes (Result Http.Error (List Home))
    | GotInfluxSinks (Result Http.Error (List InfluxSink))
    | GotSimpleSources (Result Http.Error (List SimpleSource))
    | ValidWsMessageReceived WsMsg
    | InvalidWsMessageReceived String
      -- Influx sink form:
    | UpdateInfluxHost String
    | UpdateInfluxPort String
    | UpdateInfluxTLS Bool
    | UpdateInfluxUsername String
    | UpdateInfluxPassword String
    | AddInfluxSink
    | PostedInfluxSink (Result Http.Error InfluxSink)
      -- Home form:
    | UpdateHomeDatakey String
    | AddHome
    | PostedHome (Result Http.Error String)
      -- Simple source form:
    | AddTagMapping
    | AddFieldMapping
    | UpdateSimpleSourceDatakey String
    | UpdateSimpleSourceUrl String
    | UpdateSimpleSourceAuthHeader String
    | UpdateTagMappingKey Int String
    | UpdateTagMappingVal Int String
    | UpdateFieldMappingKey Int String
    | UpdateFieldMappingVal Int String
    | AddSimpleSource
    | PostedSimpleSource (Result Http.Error SimpleSource)


getHomes : String -> Cmd Msg
getHomes token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://hue.rolsdorph.io/homes"
        , body = Http.emptyBody
        , expect = Http.expectJson GotHomes (list homeDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


getInfluxSinks : String -> Cmd Msg
getInfluxSinks token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://hue.rolsdorph.io/sinks"
        , body = Http.emptyBody
        , expect = Http.expectJson GotInfluxSinks (list influxSinkDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


getSimpleSources : String -> Cmd Msg
getSimpleSources token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://hue.rolsdorph.io/simpleSources"
        , body = Http.emptyBody
        , expect = Http.expectJson GotSimpleSources (list simpleSourceDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg old =
    case msg of
        GotHomes res ->
            case res of
                Ok homes ->
                    ( { old | homes = homes }, Cmd.none )

                Err _ ->
                    ( old, Cmd.none )

        GotInfluxSinks res ->
            case res of
                Ok sinks ->
                    ( { old | influxSinks = sinks }, Cmd.none )

                Err _ ->
                    -- TODO: Handle all of these errors gracefully (or, more gracefully :))
                    ( old, Cmd.none )

        GotSimpleSources res ->
            case res of
                Ok sources ->
                    ( { old | simpleSources = sources }, Cmd.none )

                Err _ ->
                    ( old, Cmd.none )

        AddTagMapping ->
            ( { old | tagMappings = emptyMapping old.counter :: old.tagMappings, counter = old.counter + 1 }, Cmd.none )

        AddFieldMapping ->
            ( { old | fieldMappings = emptyMapping old.counter :: old.fieldMappings, counter = old.counter + 1 }, Cmd.none )

        UpdateInfluxHost host ->
            ( { old | influxSinkFormHost = host }, Cmd.none )

        UpdateInfluxPort newPort ->
            ( { old | influxSinkFormPort = newPort }, Cmd.none )

        UpdateInfluxTLS tls ->
            if tls == True then
                ( { old | influxSinkFormTLS = "true" }, Cmd.none )

            else
                ( { old | influxSinkFormTLS = "false" }, Cmd.none )

        UpdateInfluxUsername username ->
            ( { old | influxSinkFormUsername = username }, Cmd.none )

        UpdateInfluxPassword password ->
            ( { old | influxSinkFormPassword = password }, Cmd.none )

        AddInfluxSink ->
            case old.authToken of
                Just t ->
                    ( old, postInfluxSink old t )

                Nothing ->
                    ( old, Cmd.none )

        PostedInfluxSink res ->
            case res of
                Ok newSink ->
                    ( { old | influxSinks = newSink :: old.influxSinks }, Cmd.none )

                Err _ ->
                    ( old, Cmd.none )

        UpdateHomeDatakey newKey ->
            ( { old | homeDatakey = newKey }, Cmd.none )

        AddHome ->
            case old.authToken of
                Just t ->
                    ( old, postHome old t )

                Nothing ->
                    ( old, Cmd.none )

        PostedHome res ->
            case res of
                Ok url ->
                    ( old, Browser.Navigation.load url )

                Err _ ->
                    ( old, Cmd.none )

        UpdateSimpleSourceDatakey newKey ->
            ( { old | simpleSourceDatakey = newKey }, Cmd.none )

        UpdateSimpleSourceUrl newUrl ->
            ( { old | simpleSourceUrl = newUrl }, Cmd.none )

        UpdateSimpleSourceAuthHeader newAuthHeader ->
            ( { old | simpleSourceAuthHeader = newAuthHeader }, Cmd.none )

        UpdateTagMappingKey mappingId newKey ->
            ( { old | tagMappings = List.map (replaceKeyIfId mappingId newKey) old.tagMappings }, Cmd.none )

        UpdateTagMappingVal mappingId newVal ->
            ( { old | tagMappings = List.map (replaceValIfId mappingId newVal) old.tagMappings }, Cmd.none )

        UpdateFieldMappingKey mappingId newKey ->
            ( { old | fieldMappings = List.map (replaceKeyIfId mappingId newKey) old.fieldMappings }, Cmd.none )

        UpdateFieldMappingVal mappingId newVal ->
            ( { old | fieldMappings = List.map (replaceValIfId mappingId newVal) old.fieldMappings }, Cmd.none )

        AddSimpleSource ->
            case old.authToken of
                Just t ->
                    ( old, postSimpleSource old t )

                Nothing ->
                    ( old, Cmd.none )

        PostedSimpleSource res ->
            case res of
                Ok newSource ->
                    ( { old | simpleSources = newSource :: old.simpleSources }, Cmd.none )

                Err _ ->
                    ( old, Cmd.none )

        ValidWsMessageReceived a ->
            ( old, Cmd.none )

        InvalidWsMessageReceived err ->
            ( old, Cmd.none )


replaceKeyIfId : Int -> String -> Mapping -> Mapping
replaceKeyIfId id newKey oldMapping =
    let
        shouldReplace =
            id == oldMapping.id
    in
    if shouldReplace then
        { oldMapping | key = newKey }

    else
        oldMapping


replaceValIfId : Int -> String -> Mapping -> Mapping
replaceValIfId id newVal oldMapping =
    let
        shouldReplace =
            id == oldMapping.id
    in
    if shouldReplace then
        { oldMapping | value = newVal }

    else
        oldMapping


postInfluxSink : Model -> String -> Cmd Msg
postInfluxSink state token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://hue.rolsdorph.io/sinks"
        , body = sinkPayload state
        , expect = Http.expectJson PostedInfluxSink influxSinkDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


postHome : Model -> String -> Cmd Msg
postHome state token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://hue.rolsdorph.io/homes?redirectUrlInBody=true"
        , body = homePayload state
        , expect = Http.expectJson PostedHome string
        , timeout = Nothing
        , tracker = Nothing
        }


postSimpleSource : Model -> String -> Cmd Msg
postSimpleSource state token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://hue.rolsdorph.io/simpleSources"
        , body = simpleSourcePayload state
        , expect = Http.expectJson PostedSimpleSource simpleSourceDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


sinkPayload : Model -> Http.Body
sinkPayload state =
    urlEncode
        [ ( "influxHost", state.influxSinkFormHost )
        , ( "influxPort", state.influxSinkFormPort )
        , ( "influxTLS", state.influxSinkFormTLS )
        , ( "influxUsername", state.influxSinkFormUsername )
        , ( "influxPassword", state.influxSinkFormPassword )
        ]
        |> Http.stringBody "application/x-www-form-urlencoded"


homePayload : Model -> Http.Body
homePayload state =
    urlEncode
        [ ( "datakey", state.homeDatakey ) ]
        |> Http.stringBody "application/x-www-form-urlencoded"


simpleSourcePayload : Model -> Http.Body
simpleSourcePayload state =
    urlEncode
        [ ( "datakey", state.simpleSourceDatakey )
        , ( "url", state.simpleSourceUrl )
        , ( "authHeader", state.simpleSourceAuthHeader )
        , ( "tagMappings", jsonify state.tagMappings )
        , ( "fieldMappings", jsonify state.fieldMappings )
        ]
        |> Http.stringBody "application/x-www-form-urlencoded"


jsonify : List Mapping -> String
jsonify mappings =
    let
        listified =
            List.map (\m -> [ m.key, m.value ]) mappings
    in
    JE.encode 0 (JE.list (JE.list JE.string) listified)


urlEncode : List ( String, String ) -> String
urlEncode obj =
    obj
        |> List.map (\( k, v ) -> Url.percentEncode k ++ "=" ++ Url.percentEncode v)
        |> String.join "&"


addSinkForm : Html Msg
addSinkForm =
    div []
        [ h1 [] [ text "Add Influx Sink" ]
        , form [ id "sinkForm" ]
            [ label [ for "influxHost" ] [ text "Influx host" ]
            , input [ type_ "text", name "influxHost", id "influxHost", onInput UpdateInfluxHost ] []
            , br [] []
            , label [ for "Influx port:" ] [ text "Influx port:" ]
            , input [ type_ "text", name "influxPort", id "influxPort", onInput UpdateInfluxPort ] []
            , br [] []
            , label [ for "Use TLS:" ] [ text "Use TLS:" ]
            , input [ type_ "checkbox", name "influxTLS", id "influxTLS", onCheck UpdateInfluxTLS ] []
            , br [] []
            , label [ for "Influx username:" ] [ text "Influx username:" ]
            , input [ type_ "text", name "influxUsername", id "influxUsername", onInput UpdateInfluxUsername ] []
            , br [] []
            , label [ for "Influx password:" ] [ text "Influx password:" ]
            , input [ type_ "password", name "influxPassword", id "influxPassword", onInput UpdateInfluxPassword ] []
            , br [] []
            , br [] []
            , button [ type_ "button", onClick AddInfluxSink ] [ text "Add" ]
            ]
        ]


addHomeForm : Html Msg
addHomeForm =
    div []
        [ h1 [] [ text "Add Philips Hue Home" ]
        , form [ id "homeForm" ]
            [ label [ for "datakey" ] [ text "Data key" ]
            , input [ type_ "text", name "datakey", id "datakey", onInput UpdateHomeDatakey ] []
            , br [] []
            , br [] []
            , button [ type_ "button", onClick AddHome ] [ text "Add" ]
            ]
        ]


addSimpleSourceForm : Model -> Html Msg
addSimpleSourceForm model =
    div []
        [ h1 [] [ text "Add Simple Source" ]
        , form [ id "simpleSourcxeForm" ]
            [ label [ for "datakey" ] [ text "Data key" ]
            , input [ type_ "text", name "datakey", id "datakey", onInput UpdateSimpleSourceDatakey ] []
            , br [] []
            , label [ for "url" ] [ text "URL" ]
            , input [ type_ "text", name "url", id "url", onInput UpdateSimpleSourceUrl ] []
            , br [] []
            , label [ for "authHeader" ] [ text "Auth header" ]
            , input [ type_ "text", name "authHeader", id "authHeader", onInput UpdateSimpleSourceAuthHeader ] []
            , br [] []
            , h3 [] [ text "Tag mappings" ]
            , button [ type_ "button", onClick AddTagMapping ] [ text "+" ]
            , viewTagMappings model
            , h3 [] [ text "Field mappings" ]
            , button [ type_ "button", onClick AddFieldMapping ] [ text "+" ]
            , viewFieldMappings model
            , br [] []
            , br [] []
            , button [ type_ "button", onClick AddSimpleSource ] [ text "Add" ]
            ]
        ]


view : Model -> Html Msg
view state =
    div []
        [ h3 [] [ text "Homes" ]
        , div [] (List.map viewHome state.homes)
        , h3 [] [ text "Influx Sinks" ]
        , div [] (List.map viewSink state.influxSinks)
        , h3 [] [ text "Simple sources" ]
        , div [] (List.map viewSimpleSource state.simpleSources)
        , addSinkForm
        , addHomeForm
        , addSimpleSourceForm state
        ]


viewHome : Home -> Html a
viewHome home =
    div [] [ text (home.id ++ ", " ++ home.datakey) ]


viewSink : InfluxSink -> Html a
viewSink sink =
    div [] [ text (sink.id ++ ", " ++ sink.influxHost ++ ":" ++ String.fromInt sink.influxPort) ]


viewSimpleSource : SimpleSource -> Html a
viewSimpleSource source =
    div [] [ text (source.id ++ ", " ++ source.url) ]


viewTagMappings : Model -> Html Msg
viewTagMappings state =
    div [] (List.map tagMapping state.tagMappings)


viewFieldMappings : Model -> Html Msg
viewFieldMappings state =
    div [] (List.map fieldMapping state.fieldMappings)


tagMapping : Mapping -> Html Msg
tagMapping m =
    div []
        [ input [ type_ "text", name ("tagkey-" ++ String.fromInt m.id), onInput (UpdateTagMappingKey m.id), value m.key ] []
        , input [ type_ "text", name ("tagval-" ++ String.fromInt m.id), onInput (UpdateTagMappingVal m.id), value m.value ] []
        ]


fieldMapping : Mapping -> Html Msg
fieldMapping m =
    div []
        [ input [ type_ "text", name ("fieldkey-" ++ String.fromInt m.id), onInput (UpdateFieldMappingKey m.id), value m.key ] []
        , input [ type_ "text", name ("fieldval-" ++ String.fromInt m.id), onInput (UpdateFieldMappingVal m.id), value m.value ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    wsReceiver handleWsMessage


handleWsMessage : JE.Value -> Msg
handleWsMessage val =
    case Json.Decode.decodeValue wsMsgDecoder val of
        Ok res ->
            ValidWsMessageReceived res

        Err err ->
            InvalidWsMessageReceived (Json.Decode.errorToString err)


type alias WsMsgType =
    String


type alias WsMsgTime =
    String


type alias WsMsgSourceId =
    String


type alias WsMsgSinkId =
    String


type WsMsg
    = SourceCollectedMsg WsMsgTime WsMsgType WsMsgSourceId
    | SinkFedMsg WsMsgTime WsMsgType WsMsgSinkId
    | UnknownMessage


wsMsgDecoder : Decoder WsMsg
wsMsgDecoder =
    field "type" string
        |> andThen (\t -> parseWsMsg t)


parseWsMsg : String -> Decoder WsMsg
parseWsMsg msgType =
    case msgType of
        "SourceCollected" ->
            map3 SourceCollectedMsg (field "time" string) (field "type" string) (field "sourceId" string)

        "SinkFed" ->
            map3 SinkFedMsg (field "time" string) (field "type" string) (field "sinkId" string)

        _ ->
            fail ("Unknown msgType: " ++ msgType)


homeDecoder : Decoder Home
homeDecoder =
    map4 Home (field "id" string) (field "datakey" string) (field "state" string) (field "createdAt" string)


influxSinkDecoder : Decoder InfluxSink
influxSinkDecoder =
    map5 InfluxSink (field "id" string) (field "influxHost" string) (field "influxPort" int) (field "influxTLS" bool) (field "createdAt" string)


simpleSourceDecoder : Decoder SimpleSource
simpleSourceDecoder =
    map6 SimpleSource (field "id" string) (field "datakey" string) (field "createdAt" string) (field "url" string) (field "tagMappings" (list (list string))) (field "fieldMappings" (list (list string)))

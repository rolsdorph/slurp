module DynamicMappings exposing (DynamicMappings, Mapping, addMapping, asJson, empty, replaceKeyFor, replaceValFor, asMappingList)

import Json.Encode as JE


type DynamicMappings
    = DynamicMappings Int (List Mapping)


type alias Mapping =
    { id : Int, key : String, value : String }

asMappingList : DynamicMappings -> List Mapping
asMappingList (DynamicMappings _ mappings) = mappings

emptyMapping : Int -> Mapping
emptyMapping id =
    { id = id, key = "", value = "" }


empty : DynamicMappings
empty =
    DynamicMappings 0 []


addMapping : DynamicMappings -> DynamicMappings
addMapping (DynamicMappings counter mappingList) =
    DynamicMappings (counter + 1) (emptyMapping counter :: mappingList)


replaceKeyFor : Int -> String -> DynamicMappings -> DynamicMappings
replaceKeyFor id newKey (DynamicMappings counter mappings) =
    DynamicMappings counter (List.map (replaceKeyIfId id newKey) mappings)


replaceValFor : Int -> String -> DynamicMappings -> DynamicMappings
replaceValFor id newVal (DynamicMappings counter mappings) =
    DynamicMappings counter (List.map (replaceValIfId id newVal) mappings)


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


asJson : DynamicMappings -> String
asJson (DynamicMappings _ mappings) =
    let
        listified =
            List.map (\m -> [ m.key, m.value ]) mappings
    in
    JE.encode 0 (JE.list (JE.list JE.string) listified)

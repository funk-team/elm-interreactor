module Model exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Elm.Syntax.Pattern


type alias ResultsForFile =
    { path : String
    , code : String
    , suggestions : List (Result String (List Suggestion))
    }

type Suggestion =
    AddImportSuggestion String
    | UpdateLineSuggestion UpdateLineSuggestionParams
    | AddMissingPattern Elm.Syntax.Pattern.Pattern

type alias UpdateLineSuggestionParams =
    { line : Int
    , start : Int
    , end : Int
    , with : String
    }


problemTypeToString p =
    case p of
        ModuleNameMismatch -> " Module name mismatch"
        ModuleNotFound -> " Module not found"
        NamingError -> " Naming error"
        TooManyArgs -> " Too many args"
        TooFewArgs -> " Too few args"
        MissingArrow -> " Missing arrow"
        MissingPattern -> " Missing pattern"
        RedundantPattern -> " Redundant pattern"
        TypeMismatch -> " Type mismatch"
        UnfinishedLet -> " Unfinished let"
        ProblemInRecord -> " Problem in record"
        ProblemInDefinition -> " Problem in definition"
        ProblemInTypeAlias -> " Problem in type alias"
        UnfinishedDefinition -> " Unfinished definition"
        UnboundTypeVariable -> " Unbound type variable"
        Shadowing -> " Shadowing"
        WeirdDeclaration -> " Weird declaration"
        MissingColon -> " Missing colon"
        ProblemInExposing -> " Problem in exposing"
        UnfinishedRecordType -> " Unfinished record type"
        UnfinishedParentheses -> " Unfinished parentheses"
        Other string -> string



-- [generator-start]


type ProblemType =
    ModuleNameMismatch
    | ModuleNotFound
    | NamingError
    | TooManyArgs
    | TooFewArgs
    | MissingArrow
    | MissingPattern
    | RedundantPattern
    | TypeMismatch
    | UnfinishedLet
    | ProblemInRecord
    | ProblemInDefinition
    | ProblemInTypeAlias
    | UnfinishedDefinition
    | UnboundTypeVariable
    | Shadowing
    | WeirdDeclaration
    | MissingColon
    | ProblemInExposing
    | UnfinishedRecordType
    | UnfinishedParentheses
    | Other String


type alias Application =
    { path : String
    , code : String
    }

type alias ResolutionId =
    {path : String, problemNumber : Int}

type alias Position = {line : Int, column : Int}

type alias Region =
    {start : Position, end : Position}

type Report =
    Errored (List CompileError)
    | Compiled
    | NoMain

type alias CompileError =
    { path : String
    , name : String
    , code : String
    , problems : List Problem
    }
type alias Problem =
    { type_ : ProblemType
    , region : Region
    , message : List MessagePart
    }

type alias MessagePart =
    { string : String
    , formatting : Maybe Formatting
    }

type alias Formatting =
    { bold : Bool
    , underline : Bool
    , color : Maybe String
    }


-- [generator-generated-start] -- DO NOT MODIFY or remove this line
decodeApplication =
   Decode.map2
      Application
         ( Decode.field "path" Decode.string )
         ( Decode.field "code" Decode.string )

decodeCompileError =
   Decode.map4
      CompileError
         ( Decode.field "path" Decode.string )
         ( Decode.field "name" Decode.string )
         ( Decode.field "code" Decode.string )
         ( Decode.field "problems" (Decode.list decodeProblem) )

decodeFormatting =
   Decode.map3
      Formatting
         ( Decode.field "bold" Decode.bool )
         ( Decode.field "underline" Decode.bool )
         ( Decode.field "color" (Decode.maybe Decode.string) )

decodeMessagePart =
   Decode.map2
      MessagePart
         ( Decode.field "string" Decode.string )
         ( Decode.field "formatting" (Decode.maybe decodeFormatting) )

decodePosition =
   Decode.map2
      Position
         ( Decode.field "line" Decode.int )
         ( Decode.field "column" Decode.int )

decodeProblem =
   Decode.map3
      Problem
         ( Decode.field "type_" decodeProblemType )
         ( Decode.field "region" decodeRegion )
         ( Decode.field "message" (Decode.list decodeMessagePart) )

decodeProblemType =
   Decode.field "Constructor" Decode.string |> Decode.andThen decodeProblemTypeHelp

decodeProblemTypeHelp constructor =
   case constructor of
      "ModuleNameMismatch" ->
         Decode.succeed ModuleNameMismatch
      "ModuleNotFound" ->
         Decode.succeed ModuleNotFound
      "NamingError" ->
         Decode.succeed NamingError
      "TooManyArgs" ->
         Decode.succeed TooManyArgs
      "TooFewArgs" ->
         Decode.succeed TooFewArgs
      "MissingArrow" ->
         Decode.succeed MissingArrow
      "MissingPattern" ->
         Decode.succeed MissingPattern
      "RedundantPattern" ->
         Decode.succeed RedundantPattern
      "TypeMismatch" ->
         Decode.succeed TypeMismatch
      "UnfinishedLet" ->
         Decode.succeed UnfinishedLet
      "ProblemInRecord" ->
         Decode.succeed ProblemInRecord
      "ProblemInDefinition" ->
         Decode.succeed ProblemInDefinition
      "ProblemInTypeAlias" ->
         Decode.succeed ProblemInTypeAlias
      "UnfinishedDefinition" ->
         Decode.succeed UnfinishedDefinition
      "UnboundTypeVariable" ->
         Decode.succeed UnboundTypeVariable
      "Shadowing" ->
         Decode.succeed Shadowing
      "WeirdDeclaration" ->
         Decode.succeed WeirdDeclaration
      "MissingColon" ->
         Decode.succeed MissingColon
      "ProblemInExposing" ->
         Decode.succeed ProblemInExposing
      "UnfinishedRecordType" ->
         Decode.succeed UnfinishedRecordType
      "UnfinishedParentheses" ->
         Decode.succeed UnfinishedParentheses
      "Other" ->
         Decode.map
            Other
               ( Decode.field "A1" Decode.string )
      other->
         Decode.fail <| "Unknown constructor for type ProblemType: " ++ other

decodeRegion =
   Decode.map2
      Region
         ( Decode.field "start" decodePosition )
         ( Decode.field "end" decodePosition )

decodeReport =
   Decode.field "Constructor" Decode.string |> Decode.andThen decodeReportHelp

decodeReportHelp constructor =
   case constructor of
      "Errored" ->
         Decode.map
            Errored
               ( Decode.field "A1" (Decode.list decodeCompileError) )
      "Compiled" ->
         Decode.succeed Compiled
      "NoMain" ->
         Decode.succeed NoMain
      other->
         Decode.fail <| "Unknown constructor for type Report: " ++ other

decodeResolutionId =
   Decode.map2
      ResolutionId
         ( Decode.field "path" Decode.string )
         ( Decode.field "problemNumber" Decode.int )

encodeApplication a =
   Encode.object
      [ ("path", Encode.string a.path)
      , ("code", Encode.string a.code)
      ]

encodeCompileError a =
   Encode.object
      [ ("path", Encode.string a.path)
      , ("name", Encode.string a.name)
      , ("code", Encode.string a.code)
      , ("problems", (Encode.list encodeProblem) a.problems)
      ]

encodeFormatting a =
   Encode.object
      [ ("bold", Encode.bool a.bold)
      , ("underline", Encode.bool a.underline)
      , ("color", encodeMaybeString a.color)
      ]

encodeMaybeFormatting a =
   case a of
      Just b->
         encodeFormatting b
      Nothing->
         Encode.null

encodeMaybeString a =
   case a of
      Just b->
         Encode.string b
      Nothing->
         Encode.null

encodeMessagePart a =
   Encode.object
      [ ("string", Encode.string a.string)
      , ("formatting", encodeMaybeFormatting a.formatting)
      ]

encodePosition a =
   Encode.object
      [ ("line", Encode.int a.line)
      , ("column", Encode.int a.column)
      ]

encodeProblem a =
   Encode.object
      [ ("type_", encodeProblemType a.type_)
      , ("region", encodeRegion a.region)
      , ("message", (Encode.list encodeMessagePart) a.message)
      ]

encodeProblemType a =
   case a of
      ModuleNameMismatch ->
         Encode.object
            [ ("Constructor", Encode.string "ModuleNameMismatch")
            ]
      ModuleNotFound ->
         Encode.object
            [ ("Constructor", Encode.string "ModuleNotFound")
            ]
      NamingError ->
         Encode.object
            [ ("Constructor", Encode.string "NamingError")
            ]
      TooManyArgs ->
         Encode.object
            [ ("Constructor", Encode.string "TooManyArgs")
            ]
      TooFewArgs ->
         Encode.object
            [ ("Constructor", Encode.string "TooFewArgs")
            ]
      MissingArrow ->
         Encode.object
            [ ("Constructor", Encode.string "MissingArrow")
            ]
      MissingPattern ->
         Encode.object
            [ ("Constructor", Encode.string "MissingPattern")
            ]
      RedundantPattern ->
         Encode.object
            [ ("Constructor", Encode.string "RedundantPattern")
            ]
      TypeMismatch ->
         Encode.object
            [ ("Constructor", Encode.string "TypeMismatch")
            ]
      UnfinishedLet ->
         Encode.object
            [ ("Constructor", Encode.string "UnfinishedLet")
            ]
      ProblemInRecord ->
         Encode.object
            [ ("Constructor", Encode.string "ProblemInRecord")
            ]
      ProblemInDefinition ->
         Encode.object
            [ ("Constructor", Encode.string "ProblemInDefinition")
            ]
      ProblemInTypeAlias ->
         Encode.object
            [ ("Constructor", Encode.string "ProblemInTypeAlias")
            ]
      UnfinishedDefinition ->
         Encode.object
            [ ("Constructor", Encode.string "UnfinishedDefinition")
            ]
      UnboundTypeVariable ->
         Encode.object
            [ ("Constructor", Encode.string "UnboundTypeVariable")
            ]
      Shadowing ->
         Encode.object
            [ ("Constructor", Encode.string "Shadowing")
            ]
      WeirdDeclaration ->
         Encode.object
            [ ("Constructor", Encode.string "WeirdDeclaration")
            ]
      MissingColon ->
         Encode.object
            [ ("Constructor", Encode.string "MissingColon")
            ]
      ProblemInExposing ->
         Encode.object
            [ ("Constructor", Encode.string "ProblemInExposing")
            ]
      UnfinishedRecordType ->
         Encode.object
            [ ("Constructor", Encode.string "UnfinishedRecordType")
            ]
      UnfinishedParentheses ->
         Encode.object
            [ ("Constructor", Encode.string "UnfinishedParentheses")
            ]
      Other a1->
         Encode.object
            [ ("Constructor", Encode.string "Other")
            , ("A1", Encode.string a1)
            ]

encodeRegion a =
   Encode.object
      [ ("start", encodePosition a.start)
      , ("end", encodePosition a.end)
      ]

encodeReport a =
   case a of
      Errored a1->
         Encode.object
            [ ("Constructor", Encode.string "Errored")
            , ("A1", Encode.list encodeCompileError a1)
            ]
      Compiled ->
         Encode.object
            [ ("Constructor", Encode.string "Compiled")
            ]
      NoMain ->
         Encode.object
            [ ("Constructor", Encode.string "NoMain")
            ]

encodeResolutionId a =
   Encode.object
      [ ("path", Encode.string a.path)
      , ("problemNumber", Encode.int a.problemNumber)
      ] 
-- [generator-end]

readProblem =
   Decode.map3
      Problem
         ( Decode.field "title" problemTypeReader )
         ( Decode.field "region" decodeRegion )
         ( Decode.field "message" decodeMessage )


readMessagePart : Decode.Decoder MessagePart
readMessagePart =
    Decode.oneOf [
        Decode.string |> Decode.map (\string -> {string = string, formatting = Nothing})
      , Decode.map2 (\string formatting -> {string = string, formatting = Just formatting})
          (Decode.field "string" Decode.string)
          (decodeFormatting)
    ]


decodeMessage =
    Decode.list readMessagePart


readError =
   Decode.map4
      CompileError
         ( Decode.field "path" Decode.string )
         ( Decode.field "name" Decode.string )
         ( Decode.field "code" Decode.string )
         ( Decode.field "problems" (Decode.list readProblem) )


readReport : Decode.Decoder Report
readReport =
    Decode.oneOf
        [ Decode.string |> Decode.andThen (\simple ->
            if simple == "COMPILED"
                then Decode.succeed Compiled
                else Decode.fail "not compiled"
        )
        , (Decode.field "title" Decode.string) |> Decode.andThen (\msgOnly ->
            if msgOnly == "NO MAIN"
                then Decode.succeed NoMain
                else Decode.fail "not compiled"
        )
        , (Decode.field "errors" (Decode.list readError)) |> Decode.map Errored
        ]

problemTypeReader : Decode.Decoder ProblemType
problemTypeReader =
    Decode.string
    |> Decode.andThen (\string ->
        case string of
            "MODULE NAME MISMATCH" -> Decode.succeed ModuleNameMismatch
            "TYPE MISMATCH" -> Decode.succeed TypeMismatch
            "NAMING ERROR" -> Decode.succeed NamingError
            "TOO MANY ARGS" -> Decode.succeed TooManyArgs
            "TOO FEW ARGS" -> Decode.succeed TooFewArgs
            "MODULE NOT FOUND" -> Decode.succeed ModuleNotFound
            "MISSING PATTERNS" -> Decode.succeed MissingPattern
            "REDUNDANT PATTERN" -> Decode.succeed MissingPattern
            "UNFINISHED LET" -> Decode.succeed UnfinishedLet
            "UNBOUND TYPE VARIABLE" -> Decode.succeed UnboundTypeVariable
            "SHADOWING" -> Decode.succeed Shadowing
            "PROBLEM IN RECORD" -> Decode.succeed ProblemInRecord
            "PROBLEM IN DEFINITION" -> Decode.succeed ProblemInDefinition
            "PROBLEM IN TYPE ALIAS" -> Decode.succeed ProblemInTypeAlias
            "UNFINISHED DEFINITION" -> Decode.succeed UnfinishedDefinition
            "WEIRD DECLARATION" -> Decode.succeed WeirdDeclaration
            "PROBLEM IN EXPOSING" -> Decode.succeed ProblemInExposing
            "MISSING COLON?" -> Decode.succeed MissingColon
            "UNFINISHED RECORD TYPE" -> Decode.succeed UnfinishedRecordType
            "UNFINISHED PARENTHESES" -> Decode.succeed UnfinishedParentheses
            "MISSING ARROW" -> Decode.succeed MissingArrow
            other -> Decode.succeed (Other other)
            -- Decode.fail ("Problem type not recognized: " ++ other)
    )










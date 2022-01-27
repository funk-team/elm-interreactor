module Main exposing (..)
{-| Fix elm compiler problems automatically

BUGS
- elm-syntax stuff
- check whether fix is applicable
- import line not determined when no other imports exist

EASY: needs to allow adding lines
- create new file when not found in src directory
--> automatically add module definition
- add missing module definition if file is empty

URGENT
- prefix not found stuff with imports
--> necessary: allow producing additional lnies to error message
-- DEBUGGER OUTPUT: fix module name to top and make more visible
IDEAS
- batch occurences
- keep information about which fixes were accepted / which changes based on which problem + error + project combination
- search for Msg / Module / Api based on base-path of module
- ex:

--> add import
--> replace usage
how to find right import? convention vs string search in all dependencies


```

Compilation failed
Compiling ...-- NAMING ERROR --------------------------------------- Prospects/Import/Api.elm

I cannot find a `Prospects.Msg.ProspectsUploaded` variant:

19|     , msg = Prospects.Msg.ProspectsUploaded
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The `Prospects.Msg` module does not expose a `ProspectsUploaded` variant. These
names seem close though:

    Prospects.Model.Prospect
    Prospects.Model.SetState
    Prospects.Model.Model
    Prospects.Model.Phone

Hint: Read <https://elm-lang.org/0.19.1/imports> to see how `import`
declarations work in Elm.

```

-}
import Elm.Parser
import Json.Decode as Decode
import Elm.Syntax.Pattern
import Json.Encode as Encode
import List.Extra
import Elm.RawFile
import Elm.Syntax.File
import Elm.Syntax.Declaration
import Elm.Syntax.Node
import Elm.Writer
import Elm.Syntax.Import
import Elm.Syntax.Expression
import Elm.Syntax.Range
import Model exposing (..)
import Parser exposing ((|=), (|.))


findLastLineOfImport : Elm.Syntax.Import.Import -> Maybe Int
findLastLineOfImport {moduleName, moduleAlias, exposingList} =
    [Just (moduleName |> Elm.Syntax.Node.range)
    , moduleAlias |> Maybe.map (Elm.Syntax.Node.range)
    , exposingList |> Maybe.map (Elm.Syntax.Node.range)
    ]
    |> List.filterMap identity
    |> List.map (.end >> .row)
    |> List.maximum

applySuggestion ({path, code} as error) problem suggestion =
    case suggestion of
        UpdateLineSuggestion params ->
            (Application path (updateLine params code))
        AddImportSuggestion importName ->
            (Application path (addImport importName code))
        AddMissingPattern constructor ->
            (Application path (addPattern error problem constructor))
-- FIXES --

updateLine : UpdateLineSuggestionParams -> String -> String
updateLine replace currentCode =
            String.lines currentCode
            |> List.indexedMap (\lineNo currentLine -> if (lineNo + 1) == replace.line then
                let
                    before = String.left (replace.start - 1) currentLine
                    after = String.right (String.length currentLine - replace.end + 1) currentLine
                in
                    before ++ replace.with ++ after
                 else currentLine)
            |> String.join "\n"

type RangeMatch =
    Within
    | Exact
    | Outside

mapNodeByRegion : (Elm.Syntax.Expression.Expression -> Elm.Syntax.Expression.Expression) -> Region -> String -> String
mapNodeByRegion fn region code =
    case parseFile code of
        Err _ -> Debug.todo "Could not parse file"
        Ok ({declarations} as file) ->
            let
                descendDecl : Elm.Syntax.Declaration.Declaration -> Elm.Syntax.Declaration.Declaration
                descendDecl decl =
                    case decl of
                        Elm.Syntax.Declaration.FunctionDeclaration f ->
                            { f
                            | declaration =
                                f.declaration
                                |> traverseFunctionImpl
                            }
                            |> Elm.Syntax.Declaration.FunctionDeclaration
                        _ -> decl
                traverseFunctionImpl : Elm.Syntax.Node.Node Elm.Syntax.Expression.FunctionImplementation -> Elm.Syntax.Node.Node Elm.Syntax.Expression.FunctionImplementation
                traverseFunctionImpl fnDecl =
                    case matchRange fnDecl of
                        Outside -> fnDecl
                        Exact -> Debug.todo "function matching not supported (YET)"
                        Within -> 
                            fnDecl
                            |> Elm.Syntax.Node.map (\({expression} as fun) ->
                                    {fun | expression = expression
                                        |> traverseExpression
                                    })

                traverseExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                traverseExpression exprNode =
                    case matchRange exprNode of
                        Outside -> exprNode
                        Exact ->
                            Elm.Syntax.Node.map fn exprNode
                        Within ->
                            Debug.todo "traversing expressions not supported yet"
                matchRange decl =
                    let
                        codeRange = Elm.Syntax.Node.range decl
                        isRange =
                            codeRange.start.row == region.start.line
                            && codeRange.end.row == region.end.line
                        containsRows =
                            codeRange.start.row <= region.start.line
                            && codeRange.end.row >= region.end.line
                    in
                        Debug.log "range" <| if isRange
                            then Exact
                            else if containsRows
                            then Within
                            else Outside

                traverseDecl : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
                traverseDecl decl =
                    case matchRange decl of
                        Exact -> Debug.todo "declaration matching not supporte dyet"
                        Within -> Elm.Syntax.Node.map descendDecl decl
                            |> Debug.log "within"
                        Outside -> decl

                newDeclarations =
                    declarations
                    |> List.map traverseDecl
                -- if the declaration encloses the region,
            in
            Elm.Writer.writeFile
                {file
                | declarations = declarations
                }
            |> Elm.Writer.write
            |> Debug.log "file"

parseFile : String -> Result Decode.Error Elm.Syntax.File.File
parseFile code =
    case Elm.Parser.parse code of
        Err _ ->  Decode.decodeValue (Decode.fail "Elm file parser failed") Encode.null
        Ok rawFile ->
           rawFile
            |> Elm.RawFile.encode
            |> Decode.decodeValue Elm.Syntax.File.decoder

addImport : String -> String -> String
addImport importName sourceCode =
    case Elm.Parser.parse sourceCode of
        Err _ -> sourceCode
        Ok rawFile ->
            let
                lastLineOfModuleDefinition =
                    case (Elm.RawFile.encode rawFile)
                        |> Decode.decodeValue (Decode.at ["moduleDefinition", "range"] (Decode.list Decode.int)) of
                            Ok [_, _, endRow, _] ->
                                Just endRow
                            _ -> Nothing

                lastLineOfImports =
                    rawFile
                    |> Elm.RawFile.imports
                    |> List.map findLastLineOfImport
                    |> List.filterMap identity
                    |> List.maximum
            in
                case Maybe.map2 max lastLineOfModuleDefinition lastLineOfImports of
                    Nothing -> sourceCode
                    Just insertImportLine ->
                        let

                            (before, after) =
                                sourceCode
                                |> String.lines
                                |> List.Extra.splitAt insertImportLine
                        in
                            before ++ ["import " ++ importName] ++ after
                            |> String.join "\n"

findFixForMessage : CompileError -> Problem -> MessagePart -> Maybe (List (String, Maybe Suggestion))
findFixForMessage error problem message =
    let
        redArrowsMsgIndex =
            problem.message
            |> List.Extra.findIndex (\{formatting} -> case formatting of
                Nothing -> False
                Just {color} -> case color of
                    Just "RED" -> True
                    _ -> False
                )

        -- sometimes the red arrows mark an alternate problem range
        alternateRange =
            case redArrowsMsgIndex of
                Nothing -> Nothing
                Just i ->
                    case List.Extra.getAt (i - 1) problem.message of
                        Nothing -> Nothing
                        Just before ->
                            let
                                -- the arrows have some whitespace before them and that whitespace is offset by the line number above
                                emptySpacesParser =
                                    Parser.succeed ((++) " ")
                                    |. Parser.chompIf ((==) ' ')
                                    |= (Parser.chompWhile ((==) ' ') |> Parser.getChompedString)
                                parseEmptySpaces =
                                    Parser.run emptySpacesParser  >> Result.toMaybe
                                -- line number + "|"
                                whitespaceOffset =
                                    String.split "\n" before.string
                                    |> List.filterMap (parseLineNumberAndWhitespace)
                                    |> List.head
                                    |> Maybe.map (\(lineNo, _) -> 1 + (String.fromInt lineNo |> String.length))
                                whitespace =
                                    String.split "\n" before.string
                                    |> List.filterMap parseEmptySpaces
                                    |> List.head
                                    |> Maybe.map String.length
                            in
                            case Maybe.map2 (\w o -> w - o) whitespace whitespaceOffset of
                                Nothing -> Nothing
                                Just w ->
                                    case List.Extra.getAt i problem.message of
                                        Nothing -> Nothing
                                        Just {string} ->
                                            {end = w + (String.length string)
                                            , start = w
                                            }
                                            |> Just

    in
    case problem.type_ of
        MissingArrow -> Nothing
        NamingError ->
            -- fallback / oneOf
            case matchMissingImport message of
                Nothing -> matchNamingUpdate problem message
                suggestion -> suggestion
        ModuleNameMismatch ->
            deriveNameChangeFromGreenText Nothing problem message
        TooManyArgs ->
            removeArg error problem message
        MissingPattern ->
            findAddPattern message
        RedundantPattern ->
            Nothing -- Rename or remove pattern?
        ModuleNotFound ->
            matchNamingUpdate problem message
        TooFewArgs ->
            Nothing
        TypeMismatch ->
            case deriveNameChangeFromGreenText alternateRange problem message of
                Nothing -> suggestSimilarField alternateRange problem message
                suggestion -> suggestion
        UnfinishedLet ->
            Nothing
        UnfinishedParentheses ->
            Nothing
        UnboundTypeVariable ->
            Nothing
        ProblemInRecord ->
            Nothing
        ProblemInDefinition ->
            Nothing
        ProblemInTypeAlias ->
            suggestEqualsSign problem message
        UnfinishedDefinition ->
            Nothing
        WeirdDeclaration ->
            Nothing -- solve this problem when file is empty
        ProblemInExposing ->
            Nothing -- automatically add (..) when this occurs
        Shadowing ->
            Nothing
        MissingColon ->
            Nothing
        UnfinishedRecordType ->
            Nothing
        Other str ->
            Nothing

suggestEqualsSign problem message =
    let
        _ = Debug.log "problem" problem.region
        suggestion =
            (UpdateLineSuggestion
                { line = problem.region.start.line
                , with = " = "
                , start = problem.region.start.column
                , end = problem.region.end.column
                })
        parseEq =
            Parser.succeed (\bef token after ->
                [(bef, Nothing)
                ,(token, Just suggestion)
                ,(after, Nothing)
                ]
                )
            |= (Parser.chompUntil ("equals sign") |> Parser.getChompedString)
            |= (Parser.token ("equals sign") |> Parser.getChompedString)
            |= (Parser.chompWhile (always True) |> Parser.getChompedString)
    in
    Parser.run parseEq message.string
    |> Debug.log "result"
    |> Result.toMaybe

matcherFromString : String -> Maybe Elm.Syntax.Pattern.Pattern
matcherFromString str =
    case String.split " " str of
        qualifiedRef :: args ->
            case String.split "." qualifiedRef |> List.reverse of
                name :: moduleNameReversed ->
                    let
                        qRef : Elm.Syntax.Pattern.QualifiedNameRef
                        qRef =
                            {name = name
                            , moduleName = List.reverse moduleNameReversed
                            }
                        subPatterns =
                            args
                            |> List.map (always (node Elm.Syntax.Pattern.AllPattern))
                    in
                        Elm.Syntax.Pattern.NamedPattern
                            qRef
                            subPatterns
                        |> Just

                [] -> Nothing
        [] -> Nothing

addPattern : CompileError -> Problem -> Elm.Syntax.Pattern.Pattern -> String
addPattern error problem pattern =
    let
        addPatternToNode expr =
            case expr of
                Elm.Syntax.Expression.CaseExpression ({cases} as caseBlock) ->
                    let
                        newCase =
                            (node pattern
                            , node todoExpression
                            )
                        todoExpression =
                            Elm.Syntax.Expression.Application
                                [ node <| Elm.Syntax.Expression.FunctionOrValue [ "Debug" ] "todo"
                                , node <| Elm.Syntax.Expression.Literal "TODO"
                                ]
                    in
                        {caseBlock | cases = cases ++ [newCase]}
                        |> Elm.Syntax.Expression.CaseExpression
                _ -> Debug.todo "not a case expression"
        code =
            mapNodeByRegion
                addPatternToNode
                problem.region
                error.code
    in
        code

{-|
WARNING: THIS BREAKS WHEN THERE ARE MULTIPLE MISSING PATTERNS; IT SHOULD SPLIT BY LINE
-}
findAddPattern : MessagePart -> Maybe (List (String, Maybe Suggestion))
findAddPattern {string, formatting} =
        case formatting of
                Just {color} ->
                    case color of
                        (Just "yellow") ->
                            case matcherFromString string of
                                Nothing -> Nothing
                                Just pattern -> Just [(string, Just (AddMissingPattern pattern))]
                        _ -> Nothing
                Nothing -> Nothing

{-| -- TODO
-}
removeArg : CompileError -> Problem -> MessagePart -> Maybe (List (String, Maybe Suggestion))
removeArg error problem part =
    let
        lines =
            String.lines error.code
        expression =
            lines
            |> List.drop (problem.region.start.line - 1)
            |> List.take (problem.region.end.line - problem.region.start.line + 1)
--        sug =
--            if problem.region.start.line == problem.region.end.line
--                then Debug.todo ""
--                else Debug.todo "Support for multiline"
    in
        Nothing

matchMissingImport : MessagePart -> Maybe (List (String, Maybe Suggestion))
matchMissingImport {string} =
    case parseCannotFind string of
        Err _ -> Nothing
        Ok {before, missing, after, betweenMissingAndKind, kind} ->
            case kind of
                "import" ->
                    [ (before, Nothing)
                    , (missing, Just <| AddImportSuggestion missing)
                    , (betweenMissingAndKind, Nothing)
                    , (kind, Nothing)
                    , (after, Nothing)
                    ]
                    |> Just
                _ -> Nothing

{-|
    parseLineNumberAndWhitespace "1234|        " --> Just (1234, "        ")
-}
parseLineNumberAndWhitespace : String -> Maybe (Int, String)
parseLineNumberAndWhitespace =
    let
        parser =
            Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.token "|"
                |= (Parser.spaces |> Parser.getChompedString)
    in
        Parser.run parser
        >> Result.toMaybe


{-|
    parseCannotFind "I cannot find a `Prospects.Msg.Msg` type:\n\n10| uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\n                                                                              "
    --> Ok { before = "I cannot find a `"
    --> , missing = "Prospects.Msg.Msg"
    --> , betweenMissingAndKind = "` "
    --> , kind = "type"
    --> , after = ":\n\n10| uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\n                                                                              "
    --> }

    parseCannotFind "\nI cannot find a `Prospect.Import.Model` import. These names seem close though:\n\n    "
    --> Ok { before = "\nI cannot find a `"
    --> , missing = "Prospect.Import.Model"
    --> , betweenMissingAndKind = "` "
    --> , kind = "import"
    --> , after = ". These names seem close though:\n\n    "    --> Ok { before = "I cannot find a `"
    --> }
-}
parseCannotFind : String -> Result (List Parser.DeadEnd) ParsedMissing
parseCannotFind txt =
    let
        importParser =
            Parser.succeed ParsedMissing
            -- before
            |= (Parser.chompUntil ("`") |> Parser.getChompedString |> Parser.map (\str -> str ++ "`"))
            |. (Parser.chompIf ((==) '`') |> Parser.getChompedString)
            -- missing
            |= (Parser.chompUntil ("`") |> Parser.getChompedString)
            -- between
            |. (Parser.chompIf ((==) '`') |> Parser.getChompedString)
            |. (Parser.chompIf ((==) ' ') |> Parser.getChompedString)
            |= (Parser.succeed "` ")
            -- kind
            |= (Parser.chompUntil (".") |> Parser.getChompedString)
            |= (Parser.chompWhile (always True) |> Parser.getChompedString)
            |. Parser.end

        typeParser =
            Parser.succeed ParsedMissing
            -- before
            |= (Parser.chompUntil ("`") |> Parser.getChompedString |> Parser.map (\str -> str ++ "`"))
            |. (Parser.chompIf ((==) '`') |> Parser.getChompedString)
            -- missing
            |= (Parser.chompUntil ("`") |> Parser.getChompedString)
            -- between
            |. (Parser.chompIf ((==) '`') |> Parser.getChompedString)
            |. (Parser.chompIf ((==) ' ') |> Parser.getChompedString)
            |= (Parser.succeed "` ")
            -- kind
            |= (Parser.token ("type") |> Parser.getChompedString)
            |= (Parser.chompWhile (always True) |> Parser.getChompedString)
            |. Parser.end
    in
        case Parser.run typeParser txt of
            Err _ -> Parser.run importParser txt
            ok -> ok

type alias ParsedMissing =
    { before : String
    , missing : String
    , betweenMissingAndKind : String
    , kind : String
    , after : String
    }

type Text =
    Whitespace String
    | NonWhitespace String

{-|
    extractSuggestions "Some.Suggestion"
    --> Just [NonWhitespace "Some.Suggestion"]
    extractSuggestions "    Authentication.Ui\n    Framework.Router.Model\n    Authentication.Api\n    Framework.Request"
    --> Just [Whitespace "    ",NonWhitespace "Authentication.Ui",Whitespace "\n    ",NonWhitespace "Framework.Router.Model",Whitespace "\n    ",NonWhitespace "Authentication.Api",Whitespace "\n    ",NonWhitespace "Framework.Request"]
-}
extractSuggestions : String -> Maybe (List Text)
extractSuggestions str =
    let
        either soFar =
            Parser.oneOf [
              Parser.succeed (\firstLetter moreLetters -> soFar ++ [NonWhitespace (firstLetter ++ moreLetters)] |> Parser.Loop)
                |= (Parser.chompIf Char.isAlpha |> Parser.getChompedString)
                |= (Parser.chompUntilEndOr "\n" |> Parser.getChompedString)
            , Parser.succeed (\break spaces -> soFar ++ [Whitespace (break ++ spaces)] |> Parser.Loop)
                |= (Parser.chompIf ((==) '\n') |> Parser.getChompedString)
                |= (Parser.spaces |> Parser.getChompedString)
            , Parser.succeed (\break spaces -> soFar ++ [Whitespace (break ++ spaces)] |> Parser.Loop)
                |= (Parser.chompIf ((==) ' ') |> Parser.getChompedString)
                |= (Parser.spaces |> Parser.getChompedString)
            , Parser.succeed ()
               |. Parser.end
               |> Parser.map (\_ -> Parser.Done soFar)
            ]
        loop = Parser.loop [] either
    in
        Parser.run loop str
        |> Result.toMaybe

matchNamingUpdate : Problem -> MessagePart -> Maybe (List (String, Maybe Suggestion))
matchNamingUpdate problem {formatting, string} =
    case Maybe.andThen .color formatting of
        Just "yellow" ->
            case extractSuggestions string of
                Nothing -> Nothing
                Just parts ->
                    parts
                    |> List.map (\p -> case p of
                        Whitespace w -> (w, Nothing)
                        NonWhitespace n ->
                            (n, Just (UpdateLineSuggestion { line = problem.region.start.line
                            , with = n
                            , start = problem.region.start.column
                            , end = problem.region.end.column
                            })
                            )
                        )
                    |> Just
                
        _ -> Nothing

makeSuggestionForProblem : Problem -> Result String (List Suggestion)
makeSuggestionForProblem problem = Debug.todo "deprecated"

-- nothing to do with 4chan
deriveNameChangeFromGreenText : Maybe {start : Int, end : Int} -> Problem -> MessagePart -> Maybe (List (String, Maybe Suggestion))
deriveNameChangeFromGreenText rangeFromArrows problem {formatting, string} =
    case formatting of
        Just {color} ->
            case color of
                Just "GREEN" ->
                    let
                        details =
                            { line = problem.region.start.line
                            , with = string
                            , start = rangeFromArrows |> Maybe.map .start |> Maybe.withDefault problem.region.start.column
                            , end = rangeFromArrows |> Maybe.map .end |> Maybe.withDefault problem.region.end.column
                            }
                    in
                    UpdateLineSuggestion details
                    |> Just
                    |> Tuple.pair string
                    |> List.singleton
                    |> Just
                _ -> Nothing
        Nothing -> Nothing

{-|
    "\nThis is usually a typo. Here are the `model` fields that are most similar:\n\n    { csvFile : Maybe Prospects.Import.Model.ParsedFile\n    , csvHasHeader : Bool\n    , dropZone : DropZone.Model\n    , tagsDelimiter : Maybe Char\n    , ...\n    }\n\nSo maybe " |> String.lines |> List.filterMap parseSimilarField
    --> [("    { ","csvFile"," : Maybe Prospects.Import.Model.ParsedFile")
    --> ,("    , ","csvHasHeader"," : Bool")
    --> ,("    , ","dropZone"," : DropZone.Model")
    --> ,("    , ","tagsDelimiter"," : Maybe Char")]
-}

parseSimilarField : String -> Maybe (String, String, String)
parseSimilarField str =
    let
        recordRowStart =
            [ Parser.token "    { "
            , Parser.token "    , "
            ]
        parser =
            Parser.succeed (\before name after -> (before, name, after))
            |= (Parser.oneOf recordRowStart |> Parser.getChompedString)
            |= (Parser.chompUntil " " |> Parser.getChompedString)
            |= (Parser.chompUntilEndOr "\n" |> Parser.getChompedString)
            |. Parser.end
    in
        Parser.run parser str
        |> Result.toMaybe

suggestSimilarField : Maybe {start : Int, end : Int} -> Problem -> MessagePart -> Maybe (List (String, Maybe Suggestion))
suggestSimilarField rangeFromArrows problem {formatting, string} =
    string
    |> Debug.log "str"
    |> String.lines
    |> List.map (\line -> case parseSimilarField line of
        Nothing -> [(line, Nothing)]
        Just (before, newFieldName, after) ->
                    let
                        details =
                            { line = problem.region.start.line
                            , with = newFieldName
                            , start = rangeFromArrows |> Maybe.map .start |> Maybe.withDefault problem.region.start.column
                            , end = rangeFromArrows |> Maybe.map .end |> Maybe.withDefault problem.region.end.column
                            }
                    in
                        [ (before, Nothing)
                        , (newFieldName, Just (UpdateLineSuggestion details))
                        , (after, Nothing)
                        ]
    )
    |> List.intersperse [("\n", Nothing)]
    |> List.concatMap identity
    |> Just

out2 = """module Prospects.Import.Api
    exposing (..)

import Framework.Model
import Prospects.Msg
import Json.Encode as Encode
import Http
import Prospects.Model

uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg
uploadProspects prospects =
    let
        body =
            Encode.object [ ( "prospects", Encode.list Prospects.Model.encodeProspect prospects) ]
    in
    { headers = []
    , method = Framework.Model.Post (Http.jsonBody body)
    , endpoint = "/prospects/upload"
    , msg = Prospects.Msg.ProspectsUploaded
    }
"""

out1 = """module Prospects.Import.Api exposing (..)

import Framework.Model
import Prospects.Msg
import Json.Encode as Encode
import Http

uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg
uploadProspects prospects =
    let
        body =
            Encode.object [ ( "prospects", Encode.list Prospects.Model.encodeProspect prospects) ]
    in
    { headers = []
    , method = Framework.Model.Post (Http.jsonBody body)
    , endpoint = "/prospects/upload"
    , msg = Prospects.Msg.ProspectsUploaded
    }
"""

out3 = """module Prospects.Import.Api
    exposing (..)

import Framework.Model
import Prospects.Import.Msg
import Json.Encode as Encode
import Http
import Prospects.Model

uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Import.Msg.Msg
uploadProspects prospects =
    let
        body =
            Encode.object [ ( "prospects", Encode.list Prospects.Model.encodeProspect prospects) ]
    in
    { headers = []
    , method = Framework.Model.Post (Http.jsonBody body)
    , endpoint = "/prospects/upload"
    , msg = Prospects.Msg.ProspectsUploaded
    }
"""


{-|

test { errors = [{ code = "module Prospects.Api exposing (..)\n\nimport Framework.Model\nimport Prospects.Msg\nimport Json.Encode as Encode\nimport Http\n\nuploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\nuploadProspects prospects =\n    let\n        body =\n            Encode.object [ ( \"prospects\", Encode.list Prospects.Model.encodeProspect prospects) ]\n    in\n    { headers = []\n    , method = Framework.Model.Post (Http.jsonBody body)\n    , endpoint = \"/prospects/upload\"\n    , msg = Prospects.Msg.ProspectsUploaded\n    }\n", path = "/Users/username/project-name/frontend/Prospects/Import/Api.elm", problems = [{ message = [{ formatting = Nothing, string = "It looks like this module name is out of sync:\n\n1| module Prospects.Api exposing (..)\n          " },{ formatting = Just { bold = False, color = Just "RED", underline = False }, string = "^^^^^^^^^^^^^" },{ formatting = Nothing, string = "\nI need it to match the file path, so I was expecting to see\n`Prospects.Import.Api` here. Make the following change, and you should be all\nset!\n\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Prospects.Api" },{ formatting = Nothing, string = " -> " },{ formatting = Just { bold = False, color = Just "GREEN", underline = False }, string = "Prospects.Import.Api" },{ formatting = Nothing, string = "\n\n" },{ formatting = Just { bold = False, color = Nothing, underline = True }, string = "Note" },{ formatting = Nothing, string = ": I require that module names correspond to file paths. This makes it much\neasier to explore unfamiliar codebases! So if you want to keep the current\nmodule name, try renaming the file instead." }], region = { end = { column = 21, line = 1 }, start = { column = 8, line = 1 } }, type_ = ModuleNameMismatch }] }], type_ = CompileErrors } |> Maybe.map .code
--> Just (out1)

test { errors = [{ code = "module Prospects.Import.Api\n    exposing (..)\n\nimport Framework.Model\nimport Prospects.Msg\nimport Json.Encode as Encode\nimport Http\n\nuploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\nuploadProspects prospects =\n    let\n        body =\n            Encode.object [ ( \"prospects\", Encode.list Prospects.Model.encodeProspect prospects) ]\n    in\n    { headers = []\n    , method = Framework.Model.Post (Http.jsonBody body)\n    , endpoint = \"/prospects/upload\"\n    , msg = Prospects.Msg.ProspectsUploaded\n    }\n", path = "/Users/username/project-name/frontend/Prospects/Import/Api.elm", problems = [{ message = [{ formatting = Nothing, string = "I cannot find a `Prospects.Model.Prospect` type:\n\n9| uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\n                          " },{ formatting = Just { bold = False, color = Just "RED", underline = False }, string = "^^^^^^^^^^^^^^^^^^^^^^^^" },{ formatting = Nothing, string = "\nI cannot find a `Prospects.Model` import. These names seem close (though):\n\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Prospects.Msg.Msg" },{ formatting = Nothing, string = "\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Framework.Model.Model" },{ formatting = Nothing, string = "\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Framework.Model.Params" },{ formatting = Nothing, string = "\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Framework.Model.Flags" },{ formatting = Nothing, string = "\n\n" },{ formatting = Just { bold = False, color = Nothing, underline = True }, string = "Hint" },{ formatting = Nothing, string = ": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm." }], region = { end = { column = 48, line = 9 }, start = { column = 24, line = 9 } }, type_ = NamingError }] }], type_ = CompileErrors } |> Maybe.map .code
--> Just (out2)

test { errors = [{ code = "module Prospects.Import.Api\n    exposing (..)\n\nimport Framework.Model\nimport Prospects.Import.Msg\nimport Json.Encode as Encode\nimport Http\nimport Prospects.Model\n\nuploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\nuploadProspects prospects =\n    let\n        body =\n            Encode.object [ ( \"prospects\", Encode.list Prospects.Model.encodeProspect prospects) ]\n    in\n    { headers = []\n    , method = Framework.Model.Post (Http.jsonBody body)\n    , endpoint = \"/prospects/upload\"\n    , msg = Prospects.Msg.ProspectsUploaded\n    }\n", path = "/Users/username/project-name/frontend/Prospects/Import/Api.elm", problems = [{ message = [{ formatting = Nothing, string = "I cannot find a `Prospects.Msg.Msg` type:\n\n10| uploadProspects : List Prospects.Model.Prospect -> Framework.Model.Params Prospects.Msg.Msg\n                                                                              " },{ formatting = Just { bold = False, color = Just "RED", underline = False }, string = "^^^^^^^^^^^^^^^^^" },{ formatting = Nothing, string = "\nI cannot find a `Prospects.Msg` import. These names seem close (though):\n\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Prospects.Import.Msg.Msg" },{ formatting = Nothing, string = "\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Prospects.Model.Model" },{ formatting = Nothing, string = "\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Prospects.Model.Step" },{ formatting = Nothing, string = "\n    " },{ formatting = Just { bold = False, color = Just "yellow", underline = False }, string = "Prospects.Model.Field" },{ formatting = Nothing, string = "\n\n" },{ formatting = Just { bold = False, color = Nothing, underline = True }, string = "Hint" },{ formatting = Nothing, string = ": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm." }], region = { end = { column = 92, line = 10 }, start = { column = 75, line = 10 } }, type_ = NamingError }] }], type_ = CompileErrors } |> Maybe.map .code
--> Just (out3)

-}
test : Report -> Maybe Application
test = Debug.todo "improve testing"

dummyRange : Elm.Syntax.Range.Range
dummyRange =
    Elm.Syntax.Range.Range
        (Elm.Syntax.Range.Location 0 0)
        (Elm.Syntax.Range.Location 0 0)

node : a -> Elm.Syntax.Node.Node a
node =
    Elm.Syntax.Node.Node
        dummyRange

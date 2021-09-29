module Contentful.RichText exposing (decoder, toMarkdown)

import Json.Decode as Decode exposing (Decoder)



-- TYPES


type RichText
    = RichText (List TopLevelBlock)


type TopLevelBlock
    = MainBlock MainBlock
    | Table TableData



-- Block nodes


type MainBlock
    = ParagraphBlock Paragraph
    | Heading HeadingLevel InlineContent
    | ListBlock ListType (List (List MainBlock))
    | HorizontalRule
    | QuoteBlock (List Paragraph)
    | EmbeddedAsset Id
    | EmbeddedEntry Id


type Paragraph
    = Paragraph InlineContent


type HeadingLevel
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type ListType
    = Ordered
    | Unordered



-- Inline nodes


type alias InlineContent =
    List Inline


type Inline
    = Link Link
    | Text Text


type Link
    = Hyperlink Uri (List Text)
    | EntryHyperlink Id (List Text)
    | AssetHyperlink Id (List Text)
    | EntryInline Id


type alias Uri =
    String


type alias Id =
    String


type Text
    = WithMarks Marks String


type alias Marks =
    { bold : Bool
    , italic : Bool
    , underline : Bool
    , code : Bool
    }



-- Table nodes


type alias TableData =
    List Row


type alias Row =
    List Cell


type Cell
    = Cell CellType Spans (List Paragraph)


type CellType
    = Header
    | Data


type alias Spans =
    { colspan : Int
    , rowspan : Int
    }



-- DECODER


decoder : Decoder RichText
decoder =
    Decode.map RichText <|
        node "document" <|
            Decode.list <|
                Decode.oneOf
                    [ Decode.map MainBlock mainBlockDecoder
                    , Decode.map Table tableDataDecoder
                    ]


mainBlockDecoder : Decoder MainBlock
mainBlockDecoder =
    Decode.oneOf
        [ Decode.map ParagraphBlock paragraphDecoder
        , Decode.map (Heading One) (node "heading-1" inlineContentDecoder)
        , Decode.map (Heading Two) (node "heading-2" inlineContentDecoder)
        , Decode.map (Heading Three) (node "heading-3" inlineContentDecoder)
        , Decode.map (Heading Four) (node "heading-4" inlineContentDecoder)
        , Decode.map (Heading Five) (node "heading-5" inlineContentDecoder)
        , Decode.map (Heading Six) (node "heading-6" inlineContentDecoder)
        , Decode.map (ListBlock Ordered) (node "ordered-list" (Decode.list listItemDecoder))
        , Decode.map (ListBlock Unordered) (node "unordered-list" (Decode.list listItemDecoder))
        , Decode.map (always HorizontalRule) (node "hr" emptyList)
        , Decode.map QuoteBlock (node "blockquote" (Decode.list paragraphDecoder))
        , nodeWithData "embedded-asset-block" (always << EmbeddedAsset) (linkDataDecoder "Asset") emptyRecord
        , nodeWithData "embedded-entry-block" (always << EmbeddedEntry) (linkDataDecoder "Entry") emptyRecord
        ]


paragraphDecoder : Decoder Paragraph
paragraphDecoder =
    Decode.map Paragraph <|
        node "paragraph" inlineContentDecoder


listItemDecoder : Decoder (List MainBlock)
listItemDecoder =
    node "list-item" (Decode.lazy (\_ -> Decode.list mainBlockDecoder))


linkDataDecoder : String -> Decoder Id
linkDataDecoder typeField =
    Decode.at [ "target", "sys" ] <|
        Decode.map3 (\id _ _ -> id)
            (Decode.field "id" Decode.string)
            (Decode.field "type" (Decode.string |> Decode.andThen (checkDecodedStringIs typeField)))
            (Decode.field "type" (Decode.string |> Decode.andThen (checkDecodedStringIs "Entry")))


inlineContentDecoder : Decoder InlineContent
inlineContentDecoder =
    Decode.list <|
        Decode.oneOf
            [ Decode.map Text textDecoder
            , Decode.map Link linkDecoder
            ]


textDecoder : Decoder Text
textDecoder =
    Decode.field "nodeType" (Decode.string |> Decode.andThen (checkDecodedStringIs "text"))
        |> Decode.andThen (always (Decode.field "data" emptyRecord))
        |> Decode.andThen (always withMarksDecoder)


withMarksDecoder : Decoder Text
withMarksDecoder =
    Decode.map2 WithMarks
        marksDecoder
        (Decode.field "value" Decode.string)


marksDecoder : Decoder Marks
marksDecoder =
    Decode.list (Decode.field "type" Decode.string)
        |> Decode.andThen (marksDecoderHelper noMarks)
        |> Decode.field "marks"


linkDecoder : Decoder Link
linkDecoder =
    Decode.oneOf
        [ nodeWithData "hyperlink" Hyperlink (Decode.field "uri" Decode.string) (Decode.list textDecoder)
        , nodeWithData "entry-hyperlink" EntryHyperlink (linkDataDecoder "Entry") (Decode.list textDecoder)
        , nodeWithData "asset-hyperlink" AssetHyperlink (linkDataDecoder "Asset") (Decode.list textDecoder)
        , nodeWithData "embedded-entry-inline" (always << EntryInline) (linkDataDecoder "Entry") emptyList
        ]


tableDataDecoder : Decoder TableData
tableDataDecoder =
    node "table" <|
        Decode.list <|
            node "table-row" <|
                Decode.list <|
                    Decode.oneOf
                        [ nodeWithData "table-header-cell" (Cell Header) spansDecoder (Decode.list paragraphDecoder)
                        , nodeWithData "table-cell" (Cell Data) spansDecoder (Decode.list paragraphDecoder)
                        ]


spansDecoder : Decoder Spans
spansDecoder =
    Decode.map2 Spans
        (Decode.oneOf [ Decode.field "colspan" Decode.int, Decode.succeed 1 ])
        (Decode.oneOf [ Decode.field "rowspan" Decode.int, Decode.succeed 1 ])



-- DECODER HELPERS


node : String -> Decoder content -> Decoder content
node nodeType =
    nodeWithData nodeType (always identity) emptyRecord


nodeWithData : String -> (data -> content -> node) -> Decoder data -> Decoder content -> Decoder node
nodeWithData nodeType makeNode dataDecoder contentDecoder =
    Decode.map3 (always makeNode)
        (Decode.field "nodeType" (Decode.string |> Decode.andThen (checkDecodedStringIs nodeType)))
        (Decode.field "data" dataDecoder)
        (Decode.field "content" contentDecoder)


checkDecodedStringIs : String -> String -> Decoder ()
checkDecodedStringIs a b =
    if a == b then
        Decode.succeed ()

    else
        Decode.fail ("Was expecting ‘" ++ a ++ "’, got ‘" ++ b ++ "’.")


marksDecoderHelper : Marks -> List String -> Decoder Marks
marksDecoderHelper marks strings =
    case strings of
        "bold" :: rest ->
            marksDecoderHelper { marks | bold = True } rest

        "italic" :: rest ->
            marksDecoderHelper { marks | italic = True } rest

        "underline" :: rest ->
            marksDecoderHelper { marks | underline = True } rest

        "code" :: rest ->
            marksDecoderHelper { marks | code = True } rest

        _ :: _ ->
            Decode.fail "Encountered an unknown mark"

        [] ->
            Decode.succeed marks


noMarks : Marks
noMarks =
    { bold = False, italic = False, underline = False, code = False }


emptyRecord : Decoder ()
emptyRecord =
    Decode.dict (Decode.fail "Record has fields")
        |> Decode.map (always ())


emptyList : Decoder ()
emptyList =
    Decode.list (Decode.fail "List has items")
        |> Decode.map (always ())



-- MARKDOWN


toMarkdown : RichText -> String
toMarkdown (RichText topLevel) =
    List.indexedMap topLevelToMarkdown topLevel
        |> String.join "\n\n"


topLevelToMarkdown : Int -> TopLevelBlock -> String
topLevelToMarkdown blockNumber topLevelBlock =
    case topLevelBlock of
        MainBlock mainBlock ->
            mainBlockToMarkdown mainBlock
                |> String.join "\n\n"

        Table tableData ->
            tableDataToMarkdown blockNumber tableData


mainBlockToMarkdown : MainBlock -> List String
mainBlockToMarkdown mainBlock =
    case mainBlock of
        ParagraphBlock paragraph ->
            [ paragraphToMarkdown paragraph ]

        Heading level inlineContent ->
            [ headingToMarkdown level inlineContent ]

        ListBlock listType listItems ->
            listToMarkdown listType listItems

        HorizontalRule ->
            [ "---" ]

        QuoteBlock paragraphs ->
            quoteBlockToMarkdown paragraphs

        EmbeddedAsset id ->
            [ "{{ embeddedAsset " ++ id ++ " }}" ]

        EmbeddedEntry id ->
            [ "{{ embeddedEntry " ++ id ++ " }}" ]


paragraphToMarkdown : Paragraph -> String
paragraphToMarkdown (Paragraph inlineContent) =
    inlineContentToMarkdown inlineContent


headingToMarkdown : HeadingLevel -> InlineContent -> String
headingToMarkdown level inlineContent =
    hashes level ++ inlineContentToMarkdown inlineContent


hashes : HeadingLevel -> String
hashes level =
    case level of
        One ->
            "# "

        Two ->
            "## "

        Three ->
            "### "

        Four ->
            "#### "

        Five ->
            "##### "

        Six ->
            "###### "


listToMarkdown : ListType -> List (List MainBlock) -> List String
listToMarkdown listType =
    List.concat << List.map (listItem listType)


listItem : ListType -> List MainBlock -> List String
listItem listType items =
    let
        unindented =
            items
                |> List.map mainBlockToMarkdown
                |> List.intersperse [ "" ]
                |> List.concat
    in
    case unindented of
        head :: rest ->
            (listTypeToMarkdown listType ++ head) :: List.map ((++) "   ") rest

        [] ->
            []


listTypeToMarkdown : ListType -> String
listTypeToMarkdown listType =
    case listType of
        Ordered ->
            "1. "

        Unordered ->
            "*  "


quoteBlockToMarkdown : List Paragraph -> List String
quoteBlockToMarkdown =
    List.map paragraphToMarkdown
        >> List.intersperse ""
        >> List.map ((++) "> ")


inlineContentToMarkdown : InlineContent -> String
inlineContentToMarkdown =
    List.map inlineToMarkdown
        >> String.join ""


inlineToMarkdown : Inline -> String
inlineToMarkdown inline =
    case inline of
        Link link ->
            linkToMarkdown link

        Text text ->
            textToMarkdown text


textToMarkdown : Text -> String
textToMarkdown (WithMarks marks content) =
    let
        wrap =
            marksToMarkdown marks
    in
    wrap ++ content ++ String.reverse wrap


marksToMarkdown : Marks -> String
marksToMarkdown { bold, code, underline, italic } =
    let
        f bool str =
            if bool then
                str

            else
                ""
    in
    f bold "**" ++ f italic "*" ++ f underline "_" ++ f code "`"


linkToMarkdown : Link -> String
linkToMarkdown link =
    let
        f texts =
            String.concat (List.map textToMarkdown texts)
    in
    case link of
        Hyperlink uri content ->
            "[" ++ f content ++ "](" ++ uri ++ ")"

        EntryHyperlink id content ->
            "[" ++ f content ++ "]({{ entryLink " ++ id ++ " }})"

        AssetHyperlink id content ->
            "[" ++ f content ++ "]({{ assetLink " ++ id ++ " }})"

        EntryInline id ->
            "{{ inlineEntry " ++ id ++ " }}"


tableDataToMarkdown : Int -> TableData -> String
tableDataToMarkdown blockNumber tableData =
    "{{ table " ++ String.fromInt blockNumber ++ " }}"

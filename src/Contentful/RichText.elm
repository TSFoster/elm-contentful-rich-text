module Contentful.RichText exposing (Encoder, Marks, RichText, Spans, decoder, encode)

import Json.Decode as Decode exposing (Decoder)



-- TYPES


type RichText
    = RichText (List TopLevelBlock)


type TopLevelBlock
    = MainBlock MainBlock
    | TableBlock TableData



-- Block nodes


type MainBlock
    = ParagraphBlock Paragraph
    | Heading HeadingLevel InlineContent
    | ListBlock ListType (List ListItemData)
    | HorizontalRule
    | QuoteBlock (List Paragraph)
    | EmbeddedAsset Id
    | EmbeddedEntry Id


type Paragraph
    = Para InlineContent


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


type alias ListItemData =
    List MainBlock



-- Inline nodes


type alias InlineContent =
    List Inline


type Inline
    = Link Link
    | Text Text


type Link
    = Anchor Uri (List Text)
    | EntryLink Id (List Text)
    | AssetLink Id (List Text)
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
    { col : Int
    , row : Int
    }



-- DECODER


decoder : Decoder RichText
decoder =
    Decode.map RichText <|
        node "document" <|
            Decode.list <|
                Decode.oneOf
                    [ Decode.map MainBlock mainBlockDecoder
                    , Decode.map TableBlock tableDataDecoder
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
    Decode.map Para <|
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
        [ nodeWithData "hyperlink" Anchor (Decode.field "uri" Decode.string) (Decode.list textDecoder)
        , nodeWithData "entry-hyperlink" EntryLink (linkDataDecoder "Entry") (Decode.list textDecoder)
        , nodeWithData "asset-hyperlink" AssetLink (linkDataDecoder "Asset") (Decode.list textDecoder)
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



-- ENCODE


type alias Encoder inline block doc =
    { document : List block -> doc

    -- blocks
    , blockquote : Context -> List block -> block
    , embeddedAsset : Context -> String -> block
    , embeddedEntry : Context -> String -> block
    , heading1 : Context -> List inline -> block
    , heading2 : Context -> List inline -> block
    , heading3 : Context -> List inline -> block
    , heading4 : Context -> List inline -> block
    , heading5 : Context -> List inline -> block
    , heading6 : Context -> List inline -> block
    , hr : Context -> block
    , listItem : Context -> List block -> block
    , orderedList : Context -> List block -> block
    , unorderedList : Context -> List block -> block
    , paragraph : Context -> List inline -> block

    -- table blocks
    , table : Context -> List block -> block
    , tableRow : Context -> List block -> block
    , tableHeaderCell : Context -> Spans -> List block -> block
    , tableCell : Context -> Spans -> List block -> block

    -- inlines
    , text : Context -> Marks -> String -> inline
    , assetHyperlink : Context -> String -> List inline -> inline
    , entryHyperlink : Context -> String -> List inline -> inline
    , hyperlink : Context -> String -> List inline -> inline
    , embeddedEntryInline : Context -> String -> inline
    }


type alias Context =
    { depth : Int
    , container : Container
    , ancestors : List Ancestor
    , siblingsBefore : Int
    , siblingsAfter : Int
    }


type alias Ancestor =
    { container : Container
    , siblingsBefore : Int
    , siblingsAfter : Int
    }



-- type alias TableContext =
--     { table :
--         { siblingsBefore : Int
--         , siblingsAfter : Int
--         , rows : Int
--         , columns : Int
--         }
--     , colspan : Int
--     , rowspan : Int
--     , row : Int
--     , column : Int
--     }


type Container
    = Document
    | Paragraph
    | Heading1
    | Heading2
    | Heading3
    | Heading4
    | Heading5
    | Heading6
    | UnorderedList
    | OrderedList
    | ListItem
    | Blockquote
    | Hyperlink
    | EntryHyperlink
    | AssetHyperlink
    | Table
    | TableRow
    | TableCell
    | TableHeaderCell


initContext : Int -> Int -> Context
initContext total pos =
    { depth = 0
    , container = Document
    , ancestors = []
    , siblingsBefore = pos
    , siblingsAfter = total - pos
    }


pushContext : Context -> Container -> Int -> Int -> Context
pushContext parent parentType total pos =
    { depth = parent.depth + 1
    , container = parentType
    , ancestors = Ancestor parent.container parent.siblingsBefore parent.siblingsAfter :: parent.ancestors
    , siblingsBefore = pos
    , siblingsAfter = total - pos
    }


encode : Encoder inline block doc -> RichText -> doc
encode encoder (RichText topLevelBlocks) =
    List.indexedMap (encodeTopLevelBlock encoder << initContext (List.length topLevelBlocks)) topLevelBlocks
        |> encoder.document


encodeTopLevelBlock : Encoder inline block doc -> Context -> TopLevelBlock -> block
encodeTopLevelBlock encoder context topLevelBlock =
    case topLevelBlock of
        MainBlock mainBlock ->
            encodeMainBlock encoder context mainBlock

        TableBlock rows ->
            encodeTable encoder context rows


encodeMainBlock : Encoder inline block doc -> Context -> MainBlock -> block
encodeMainBlock encoder context mainBlock =
    case mainBlock of
        ParagraphBlock paragraph ->
            encodeParagraph encoder context paragraph

        Heading level inlineContent ->
            encodeHeading level encoder context inlineContent

        ListBlock listType listItems ->
            encodeListBlock listType encoder context listItems

        HorizontalRule ->
            encoder.hr context

        QuoteBlock paragraphs ->
            encodeQuoteBlock encoder context paragraphs

        EmbeddedAsset id ->
            encoder.embeddedAsset context id

        EmbeddedEntry id ->
            encoder.embeddedEntry context id


encodeParagraph : Encoder inline block doc -> Context -> Paragraph -> block
encodeParagraph encoder context (Para inlineContent) =
    encodeInlineContent encoder context Paragraph inlineContent
        |> encoder.paragraph context


encodeHeading : HeadingLevel -> Encoder inline block doc -> Context -> InlineContent -> block
encodeHeading level encoder context inlineContent =
    let
        ( container, encoderHeader ) =
            case level of
                One ->
                    ( Heading1, encoder.heading1 )

                Two ->
                    ( Heading2, encoder.heading2 )

                Three ->
                    ( Heading3, encoder.heading3 )

                Four ->
                    ( Heading4, encoder.heading4 )

                Five ->
                    ( Heading5, encoder.heading5 )

                Six ->
                    ( Heading6, encoder.heading6 )
    in
    encodeInlineContent encoder context container inlineContent
        |> encoderHeader context


encodeListBlock : ListType -> Encoder inline block doc -> Context -> List ListItemData -> block
encodeListBlock listType encoder context listItems =
    let
        ( container, encoderList ) =
            case listType of
                Unordered ->
                    ( UnorderedList, encoder.unorderedList )

                Ordered ->
                    ( OrderedList, encoder.orderedList )
    in
    List.indexedMap (encodeListItem encoder << pushContext context container (List.length listItems)) listItems
        |> encoderList context


encodeListItem : Encoder inline block doc -> Context -> ListItemData -> block
encodeListItem encoder context mainBlocks =
    List.indexedMap (encodeMainBlock encoder << pushContext context ListItem (List.length mainBlocks)) mainBlocks
        |> encoder.listItem context


encodeQuoteBlock : Encoder inline block doc -> Context -> List Paragraph -> block
encodeQuoteBlock encoder context paragraphs =
    List.indexedMap (encodeParagraph encoder << pushContext context Blockquote (List.length paragraphs)) paragraphs
        |> encoder.blockquote context


encodeInlineContent : Encoder inline block doc -> Context -> Container -> InlineContent -> List inline
encodeInlineContent encoder context container inlineContent =
    List.indexedMap (encodeInline encoder << pushContext context container (List.length inlineContent)) inlineContent


encodeInline : Encoder inline block doc -> Context -> Inline -> inline
encodeInline encoder context inline =
    case inline of
        Text text ->
            encodeText encoder context text

        Link link ->
            encodeLink encoder context link


encodeLink : Encoder inline block doc -> Context -> Link -> inline
encodeLink encoder context link =
    case link of
        Anchor uri texts ->
            List.indexedMap (encodeText encoder << pushContext context Hyperlink (List.length texts)) texts
                |> encoder.hyperlink context uri

        EntryLink id texts ->
            List.indexedMap (encodeText encoder << pushContext context EntryHyperlink (List.length texts)) texts
                |> encoder.entryHyperlink context id

        AssetLink id texts ->
            List.indexedMap (encodeText encoder << pushContext context AssetHyperlink (List.length texts)) texts
                |> encoder.assetHyperlink context id

        EntryInline id ->
            encoder.embeddedEntryInline context id


encodeText : Encoder inline block doc -> Context -> Text -> inline
encodeText encoder context (WithMarks marks string) =
    encoder.text context marks string


encodeTable : Encoder inline block doc -> Context -> TableData -> block
encodeTable encoder context rows =
    List.indexedMap (encodeRow encoder << pushContext context Table (List.length rows)) rows
        |> encoder.table context


encodeRow : Encoder inline block doc -> Context -> Row -> block
encodeRow encoder context row =
    List.indexedMap (encodeCell encoder << pushContext context TableRow (List.length row)) row
        |> encoder.tableRow context


encodeCell : Encoder inline block doc -> Context -> Cell -> block
encodeCell encoder context (Cell cellType spans paragraphs) =
    let
        ( container, encoderCell ) =
            case cellType of
                Header ->
                    ( TableHeaderCell, encoder.tableHeaderCell )

                Data ->
                    ( TableCell, encoder.tableCell )
    in
    List.indexedMap (encodeParagraph encoder << pushContext context container (List.length paragraphs)) paragraphs
        |> encoderCell context spans

# `Contentful.RichText`


This is a work in progress for an Elm package to deal with [Contentful's Rich
Text].

`decoder` and `encode` have only been lightly manually tested but are
understood to work as expected. As such, you should be successful in copying
`src/Contentful/RichText.elm` to your own projects. Any bug reports would be
very welcome.

The major tasks remaining before publishing are to settle on an API, and write
extensive tests and documentation for it. Benchmarking on large `RichText`s
should be done.

There is not currently a way to build or update `RichText`s. The only way to
traverse the text is via `encode`, which is designed with `Html.Html` and
Markdown in mind. `encode` may be renamed to `encodeWithContext` to allow for
`encode` without passing `Context` (mostly depending on performance and how
useful the context proves to be).

`RichText` and `decoder` are designed to [make impossible states impossible],
according to [Contentful's type definitions]. If it is to remain an opaque
type, it might make sense to simplify the data structure for performance and
maintainability reasons.

[Contentful's Rich Text]: https://www.contentful.com/developers/docs/concepts/rich-text/
[make impossible states impossible]: https://www.youtube.com/watch?v=IcgmSRJHu_8
[Contentful's type definitions]: https://github.com/contentful/rich-text/tree/master/packages/rich-text-types/src

## Example `Encoder`

### HTML

```elm
import Html exposing (Html)
import Html.Attributes exposing (rowspan, colspan)
import Contentful.RichText exposing (Encoder, Marks, Spans)

htmlEncoder : Encoder (Html msg) (Html msg) (Html msg)
htmlEncoder =
    { document = Html.div []
    , blockquote = always <| Html.blockquote []
    , embeddedAsset = always <| Html.div [] << (\id -> [ Html.text ("{{ EMBEDDED_ASSET " ++ id ++ " }}") ])
    , embeddedEntry = always <| Html.div [] << (\id -> [ Html.text ("{{ EMBEDDED_ENTRY " ++ id ++ " }}") ])
    , heading1 = always <| Html.h1 []
    , heading2 = always <| Html.h2 []
    , heading3 = always <| Html.h3 []
    , heading4 = always <| Html.h4 []
    , heading5 = always <| Html.h5 []
    , heading6 = always <| Html.h6 []
    , hr = always <| Html.hr [] []
    , listItem = always <| Html.li []
    , orderedList = always <| Html.ol []
    , unorderedList = always <| Html.ul []
    , paragraph = always <| Html.p []
    , table = always <| Html.table []
    , tableRow = always <| Html.tr []
    , tableHeaderCell = always <| Html.th << (\spans -> [ colspan spans.col, rowspan spans.row ])
    , tableCell = always <| Html.td << (\spans -> [ colspan spans.col, rowspan spans.row ])
    , text = always <| toText
    , assetHyperlink = always <| Html.a << List.singleton << Html.Attributes.href << (++) "/asset?id="
    , entryHyperlink = always <| Html.a << List.singleton << Html.Attributes.href << (++) "/entry?id="
    , hyperlink = always <| Html.a << List.singleton << Html.Attributes.href
    , embeddedEntryInline = always <| Html.span [] << (\id -> [ Html.text ("{{ EMBEDDED_ENTRY_INLINE " ++ id ++ " }}") ])
    }


toText : Marks -> String -> Html msg
toText marks string =
    List.foldl
        (\(hasMark, el) child -> if hasMark then el [ child ] else child)
        (Html.text string)
        [ ( marks.code, Html.code [] )
        , ( marks.bold, Html.strong [] )
        , ( marks.underline, Html.u [] )
        , ( marks.italic, Html.em [] )
        ]
```

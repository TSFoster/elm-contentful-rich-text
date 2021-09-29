module Main exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


example : String
example =
    """
    { "nodeType": "document"
    , "data": {}
    , "content":
        [ { "nodeType": "paragraph"
          , "data": {}
          , "content":
              [ { "nodeType": "text"
                , "value": "This text is "
                , "data": {}
                , "marks": []
                }
              , { "nodeType": "text"
                , "value": "important"
                , "data": {}
                , "marks": [ { "type": "bold" } ]
                }
              ]
          }
        ]
    }
    """



{-
   ## [Rules of Rich Text](https://www.contentful.com/developers/docs/concepts/rich-text#rules-of-rich-text)

   * The document must have only one root node with `nodeType: 'document'`.
   * Nested Documents are not allowed.
   * Nodes in the document can be of type `Block`, `Inline` or `Text`, see: [type definitions](https://github.com/contentful/rich-text/blob/master/packages/rich-text-types/src/types.ts#L15), list of supported [blocks](https://github.com/contentful/rich-text/blob/master/packages/rich-text-types/src/blocks.ts), [inlines](https://github.com/contentful/rich-text/blob/master/packages/rich-text-types/src/inlines.ts).
   * Document can contain only nodes type of `Block` as direct children.
   * Blocks can contain nodes of type `Block`, `Inline`, or `Text`.
   * Nested Blocks are whitelisted, see [the list of containers](https://github.com/contentful/rich-text/blob/master/packages/rich-text-types/src/schemaConstraints.ts#L47).
   * Void nodes are whitelisted, [the list of void nodes](https://github.com/contentful/rich-text/blob/master/packages/rich-text-types/src/schemaConstraints.ts#L42).
   * Inlines can contain nodes of type `Inline` or `Text`.
   * Text can have a list of `Marks` associated with it, see [list of supported marks](https://github.com/contentful/rich-text/blob/master/packages/rich-text-types/src/marks.ts).
   * Sibling `Text` nodes with equal marks should be grouped.
   * Custom node types, data properties and marks are not allowed.
-}

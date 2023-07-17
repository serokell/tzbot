-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.MessageBlocksSpec
  ( test_messageBlocksSpec
  , test_ignoreManuallySpec
  ) where

import TzPrelude

import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Data.String.Conversions
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (TestTree(..))
import Text.Interpolation.Nyan

import TzBot.ProcessEvents.Common (ignoreCodeBlocksManually)
import TzBot.Slack.API.MessageBlock

getLevel2Errors :: [ExtractError] -> [Text]
getLevel2Errors = mapMaybe f
  where
  f EEUnknownBlockElementLevel1Type {} = Nothing
  f (EEUnknownBlockElementLevel2 l2Err) = Just $ ubeType l2Err

test_ignoreManuallySpec :: TestTree
test_ignoreManuallySpec = TestGroup "Test `ignoreCodeBlocksManually`" $
  [ testCase "two big blocks" $ do
      let res = ignoreCodeBlocksManually "asd ```big code block1``` qwe ``` big code block2 ``` fda"
      res @?= ["asd "," qwe "," fda"]
  , testCase "bad big block" $ do
      let res = ignoreCodeBlocksManually "asd ```big code block1``` qwe ``` bad block"
      res @?= ["asd "," qwe ","` bad block"]
  , testCase "little and big blocks" $ do
      let res = ignoreCodeBlocksManually "asd `little block` ```big code block1 ```"
      res @?= ["asd "," "]
  , testCase "big and little blocks" $ do
      let res = ignoreCodeBlocksManually "``` code ``` foo `bar` baz"
      res @?= [" foo "," baz"]
  ]

test_messageBlocksSpec :: TestTree
test_messageBlocksSpec = TestGroup "Message blocks" $
  [ testCase "Correct extraction of all text pieces ignoring code blocks" $ do
      let justEverything = fromJust $ decode $ cs justEverythingRaw
          res = extractPieces justEverything
      fst res @?=
        [ "1plain "
        , " 1strike "
        , " bold 1strikeditalic\n"
        , "3.1quote block "
        , " 3.2quote block"
        , "4.1plain "
        , " 4.1strike "
        , " 4.1bold "
        , "4.2plain "
        , " 4.2strike "
        , "  4.2bold "
        , "between the lists\n"
        , "5.1plain "
        , " 5.1strike "
        , " 5.1bold"
        , "5.2something "
        , " 10am "
        , " 10"
        , "am I a human?"
        , "end!"
        ]
      getLevel2Errors (snd res) @?=
        [ "emoji", "link", "user", "broadcast"
        ]
  ]

{- | The original Slack message:
1plain `code` ~1strike~ `code` *bold* ~_1strikeditalic_~
```2big code block```
&gt; 3.1quote block `code` 3.2quote block
1. 4.1plain `code` ~4.1strike~ :slightly_smiling_face: *4.1bold*
2. 4.2plain `code` ~4.2strike~ <http://github.com|github>  *4.2bold*
between the lists
\8226 5.1plain `code` 5~.1strike~ `code` 5*.1bold*
\8226 5.2something <@U04FQH806E9> 10am <!here> 10
\8226 am I a human?
end!
 -}
justEverythingRaw :: ByteString
justEverythingRaw = [int||
[
    {
        "block_id": "tH8as",
        "elements": [
            {
                "elements": [
                    {
                        "text": "1plain ",
                        "type": "text"
                    },
                    {
                        "style": {
                            "code": true
                        },
                        "text": "code",
                        "type": "text"
                    },
                    {
                        "text": " ",
                        "type": "text"
                    },
                    {
                        "style": {
                            "strike": true
                        },
                        "text": "1strike",
                        "type": "text"
                    },
                    {
                        "text": " ",
                        "type": "text"
                    },
                    {
                        "style": {
                            "code": true
                        },
                        "text": "code",
                        "type": "text"
                    },
                    {
                        "text": " ",
                        "type": "text"
                    },
                    {
                        "style": {
                            "bold": true
                        },
                        "text": "bold",
                        "type": "text"
                    },
                    {
                        "text": " ",
                        "type": "text"
                    },
                    {
                        "style": {
                            "italic": true,
                            "strike": true
                        },
                        "text": "1strikeditalic",
                        "type": "text"
                    },
                    {
                        "text": "\\n",
                        "type": "text"
                    }
                ],
                "type": "rich_text_section"
            },
            {
                "border": 0,
                "elements": [
                    {
                        "text": "2big code block",
                        "type": "text"
                    }
                ],
                "type": "rich_text_preformatted"
            },
            {
                "elements": [
                    {
                        "text": "3.1quote block ",
                        "type": "text"
                    },
                    {
                        "style": {
                            "code": true
                        },
                        "text": "code",
                        "type": "text"
                    },
                    {
                        "text": " 3.2quote block",
                        "type": "text"
                    }
                ],
                "type": "rich_text_quote"
            },
            {
                "border": 0,
                "elements": [
                    {
                        "elements": [
                            {
                                "text": "4.1plain ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "code": true
                                },
                                "text": "code",
                                "type": "text"
                            },
                            {
                                "text": " ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "strike": true
                                },
                                "text": "4.1strike",
                                "type": "text"
                            },
                            {
                                "text": " ",
                                "type": "text"
                            },
                            {
                                "name": "slightly_smiling_face",
                                "style": {
                                    "bold": true
                                },
                                "type": "emoji",
                                "unicode": "1f642"
                            },
                            {
                                "text": " ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "bold": true
                                },
                                "text": "4.1bold ",
                                "type": "text"
                            }
                        ],
                        "type": "rich_text_section"
                    },
                    {
                        "elements": [
                            {
                                "text": "4.2plain ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "code": true
                                },
                                "text": "code",
                                "type": "text"
                            },
                            {
                                "text": " ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "strike": true
                                },
                                "text": "4.2strike",
                                "type": "text"
                            },
                            {
                                "text": " ",
                                "type": "text"
                            },
                            {
                                "text": "github",
                                "type": "link",
                                "url": "http://github.com"
                            },
                            {
                                "text": "  ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "bold": true
                                },
                                "text": "4.2bold ",
                                "type": "text"
                            }
                        ],
                        "type": "rich_text_section"
                    }
                ],
                "indent": 0,
                "style": "ordered",
                "type": "rich_text_list"
            },
            {
                "elements": [
                    {
                        "text": "between the lists\\n",
                        "type": "text"
                    }
                ],
                "type": "rich_text_section"
            },
            {
                "border": 0,
                "elements": [
                    {
                        "elements": [
                            {
                                "text": "5.1plain ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "code": true
                                },
                                "text": "code",
                                "type": "text"
                            },
                            {
                                "text": " 5",
                                "type": "text"
                            },
                            {
                               "style": {
                                    "strike": true
                                },
                                "text": ".1strike",
                                "type": "text"
                            },
                            {
                                "text": " ",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "code": true
                                },
                                "text": "code",
                                "type": "text"
                            },
                            {
                                "text": " 5",
                                "type": "text"
                            },
                            {
                                "style": {
                                    "bold": true
                                },
                                "text": ".1bold",
                                "type": "text"
                            }
                        ],
                        "type": "rich_text_section"
                    },
                    {
                        "elements": [
                            {
                                "text": "5.2something ",
                                "type": "text"
                            },
                            {
                                "type": "user",
                                "user_id": "U04FQH806E9"
                            },
                            {
                                "text": " 10am ",
                                "type": "text"
                            },
                            {
                                "range": "here",
                                "type": "broadcast"
                            },
                            {
                                "text": " 10",
                                "type": "text"
                            }
                        ],
                        "type": "rich_text_section"
                    },
                    {
                        "elements": [
                            {
                                "text": "am I a human?",
                                "type": "text"
                            }
                        ],
                        "type": "rich_text_section"
                    }
                ],
                "indent": 0,
                "style": "bullet",
                "type": "rich_text_list"
            },
            {
                "elements": [
                    {
                        "text": "end!",
                        "type": "text"
                    }
                ],
                "type": "rich_text_section"
            }
        ],
        "type": "rich_text"
    }
]

|]

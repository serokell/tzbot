-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.TzBot.MessageBlocksSpec
  ( test_messageBlocksSpec
  , test_ignoreManuallySpec
  ) where

import TzPrelude

import Data.Aeson (Value, parseJSON)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (TestTree(..))

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
      let justEverything = fromJust $ parseMaybe @_ @[MessageBlock] parseJSON messageBlocksJSON
          res = extractPieces justEverything
      fst res @?=
        [ "1plain "
        , " 1strike "
        , " bold 1strikeditalic\n"
        , "3.1quote block "
        , " 3.2quote block"
        , "4.1plain "
        , " 4.1strike  4.1bold"
        , "4.2plain "
        , " 4.2strike github  4.2bold"
        , "between the lists\n"
        , "5.1plain "
        , " 5 .1strike "
        , " 5 .1bold"
        , "5.2something "
        , " 10am "
        , " 10"
        , "am I a human?"
        , "end!"
        ]
      getLevel2Errors (snd res) @?= []
  ]

{- | The original Slack message (in markdown format):

@
1plain `code` ~1strike~ `code` *bold* _~1strikeditalic~_
```2big code block```
> 3.1quote block `code` 3.2quote block
1. 4.1plain `code` ~4.1strike~ *:slightly_smiling_face:* *4.1bold*
2. 4.2plain `code` ~4.2strike~ [github](http://github.com)  *4.2bold*
between the lists
* 5.1plain `code` 5 ~.1strike~ `code` 5 *.1bold*
* 5.2something @dc 10am @here 10
* am I a human?
end!
@

To obtain the corresponding block elements:

1. Paste this message into a slack channel
2. Right-click the message, "Copy link".
   The link should contain the channel ID and message timestamp,
   e.g.: https://diogotest.slack.com/archives/C02N85E82LV/p1694174625423609
3. Retrieve the block elements via the Web API.
   Use the command below, replacing the Bot Token, channel ID, and message timestamp.

@
curl 'https://slack.com/api/conversations.history' \
  -H 'Content-Type: application/json' \
  -H 'Authorization: Bearer <Bot Token>' \
  -d '{"channel": "C02N85E82LV", "latest": "1694174625.423609", "limit": 1, "inclusive": true}' \
  | jq '.messages[0].blocks'
@

-}
messageBlocksJSON :: Value
messageBlocksJSON = [aesonQQ|
[
  {
    "type": "rich_text",
    "block_id": "h35BL",
    "elements": [
      {
        "type": "rich_text_section",
        "elements": [
          {
            "type": "text",
            "text": "1plain "
          },
          {
            "type": "text",
            "text": "code",
            "style": {
              "code": true
            }
          },
          {
            "type": "text",
            "text": " "
          },
          {
            "type": "text",
            "text": "1strike",
            "style": {
              "strike": true
            }
          },
          {
            "type": "text",
            "text": " "
          },
          {
            "type": "text",
            "text": "code",
            "style": {
              "code": true
            }
          },
          {
            "type": "text",
            "text": " "
          },
          {
            "type": "text",
            "text": "bold",
            "style": {
              "bold": true
            }
          },
          {
            "type": "text",
            "text": " "
          },
          {
            "type": "text",
            "text": "1strikeditalic",
            "style": {
              "italic": true,
              "strike": true
            }
          },
          {
            "type": "text",
            "text": "\n"
          }
        ]
      },
      {
        "type": "rich_text_preformatted",
        "elements": [
          {
            "type": "text",
            "text": "2big code block"
          }
        ],
        "border": 0
      },
      {
        "type": "rich_text_quote",
        "elements": [
          {
            "type": "text",
            "text": "3.1quote block "
          },
          {
            "type": "text",
            "text": "code",
            "style": {
              "code": true
            }
          },
          {
            "type": "text",
            "text": " 3.2quote block"
          }
        ]
      },
      {
        "type": "rich_text_list",
        "elements": [
          {
            "type": "rich_text_section",
            "elements": [
              {
                "type": "text",
                "text": "4.1plain "
              },
              {
                "type": "text",
                "text": "code",
                "style": {
                  "code": true
                }
              },
              {
                "type": "text",
                "text": " "
              },
              {
                "type": "text",
                "text": "4.1strike",
                "style": {
                  "strike": true
                }
              },
              {
                "type": "text",
                "text": " "
              },
              {
                "type": "emoji",
                "name": "slightly_smiling_face",
                "unicode": "1f642",
                "style": {
                  "bold": true
                }
              },
              {
                "type": "text",
                "text": " "
              },
              {
                "type": "text",
                "text": "4.1bold",
                "style": {
                  "bold": true
                }
              }
            ]
          },
          {
            "type": "rich_text_section",
            "elements": [
              {
                "type": "text",
                "text": "4.2plain "
              },
              {
                "type": "text",
                "text": "code",
                "style": {
                  "code": true
                }
              },
              {
                "type": "text",
                "text": " "
              },
              {
                "type": "text",
                "text": "4.2strike",
                "style": {
                  "strike": true
                }
              },
              {
                "type": "text",
                "text": " "
              },
              {
                "type": "link",
                "url": "http://github.com",
                "text": "github"
              },
              {
                "type": "text",
                "text": "  "
              },
              {
                "type": "text",
                "text": "4.2bold",
                "style": {
                  "bold": true
                }
              }
            ]
          }
        ],
        "style": "ordered",
        "indent": 0,
        "border": 0
      },
      {
        "type": "rich_text_section",
        "elements": [
          {
            "type": "text",
            "text": "between the lists\n"
          }
        ]
      },
      {
        "type": "rich_text_list",
        "elements": [
          {
            "type": "rich_text_section",
            "elements": [
              {
                "type": "text",
                "text": "5.1plain "
              },
              {
                "type": "text",
                "text": "code",
                "style": {
                  "code": true
                }
              },
              {
                "type": "text",
                "text": " 5 "
              },
              {
                "type": "text",
                "text": ".1strike",
                "style": {
                  "strike": true
                }
              },
              {
                "type": "text",
                "text": " "
              },
              {
                "type": "text",
                "text": "code",
                "style": {
                  "code": true
                }
              },
              {
                "type": "text",
                "text": " 5 "
              },
              {
                "type": "text",
                "text": ".1bold",
                "style": {
                  "bold": true
                }
              }
            ]
          },
          {
            "type": "rich_text_section",
            "elements": [
              {
                "type": "text",
                "text": "5.2something "
              },
              {
                "type": "user",
                "user_id": "U02N85E78QM"
              },
              {
                "type": "text",
                "text": " 10am "
              },
              {
                "type": "broadcast",
                "range": "here"
              },
              {
                "type": "text",
                "text": " 10"
              }
            ]
          },
          {
            "type": "rich_text_section",
            "elements": [
              {
                "type": "text",
                "text": "am I a human?"
              }
            ]
          }
        ],
        "style": "bullet",
        "indent": 0,
        "border": 0
      },
      {
        "type": "rich_text_section",
        "elements": [
          {
            "type": "text",
            "text": "end!"
          }
        ]
      }
    ]
  }
]
|]

{- HLINT ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as Memory
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.QueryLike as Http
import qualified Network.Wai as Wai
import qualified Revolio
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Revolio" $ do

  Hspec.describe "Server" $ do

    Hspec.describe "authorizeRequest" $ do

      Hspec.it "authorizes a valid request" $ do
        -- https://api.slack.com/docs/verifying-requests-from-slack#step-by-step_walk-through_for_validating_a_request
        let
          secret = Revolio.textToSlackSigningSecret
            $ text "8f742231b10e8888abcd99yyyzzz85a5"
          header k v = (CaseInsensitive.mk $ utf8 k, utf8 v)
          request = Wai.defaultRequest
            { Wai.requestHeaders =
              [ header "X-Slack-Request-Timestamp" "1531420618"
              , header
                "X-Slack-Signature"
                "v0=a2114d57b48eac39b9ad189dd8316235a7b4a8d21a10bd27519666489c69b503"
              ]
            }
          body =
            utf8
              "token=xyzz0WbapA4vBCDEFasx0q6G&team_id=T1DC2JH3J&team_domain=testteamnow&channel_id=G8PSS9T3V&channel_name=foobar&user_id=U2CERLKJA&user_name=roadrunner&command=%2Fwebhook-collect&text=&response_url=https%3A%2F%2Fhooks.slack.com%2Fcommands%2FT1DC2JH3J%2F397700885554%2F96rGlfmibIGlgcZRskXaIFfN&trigger_id=398738663015.47445629121.803a0bc887a14d10d2c447fce8b6703c"
        Revolio.authorizeRequest secret request body `Hspec.shouldBe` Right ()

  Hspec.describe "Type" $ do

    Hspec.describe "Action" $ do

      Hspec.describe "textToAction" $ do

        Hspec.it "parses a help action" $ do
          Revolio.textToAction (text "help")
            `Hspec.shouldBe` Right Revolio.ActionHelp

        Hspec.it "parses a setup action" $ do
          let
            action = Revolio.ActionSetup Revolio.StratusTimeCredentials
              { Revolio.stratusTimeCredentialsLoginId = Revolio.textToStratusTimeLoginId $ text "username"
              , Revolio.stratusTimeCredentialsPassword = Revolio.textToStratusTimePassword $ text "password"
              }
          Revolio.textToAction (text "setup username password")
            `Hspec.shouldBe` Right action

        Hspec.it "parses a clock in action" $ do
          Revolio.textToAction (text "clock in")
            `Hspec.shouldBe` Right (Revolio.ActionClock Revolio.DirectionIn)

        Hspec.it "parses a clock out action" $ do
          Revolio.textToAction (text "clock out")
            `Hspec.shouldBe` Right (Revolio.ActionClock Revolio.DirectionOut)

        Hspec.it "rejects an invalid action" $ do
          Revolio.textToAction (text "invalid action")
            `Hspec.shouldSatisfy` Either.isLeft

      Hspec.describe "actionToText" $ do

        Hspec.it "converts an action into text" $ do
          Revolio.actionToText (Revolio.ActionClock Revolio.DirectionIn)
            `Hspec.shouldBe` text "clock in"
          Revolio.actionToText (Revolio.ActionClock Revolio.DirectionOut)
            `Hspec.shouldBe` text "clock out"
          Revolio.actionToText Revolio.ActionHelp `Hspec.shouldBe` text "help"
          Revolio.actionToText
              (Revolio.ActionSetup Revolio.StratusTimeCredentials
                { Revolio.stratusTimeCredentialsLoginId = Revolio.textToStratusTimeLoginId $ text "u"
                , Revolio.stratusTimeCredentialsPassword = Revolio.textToStratusTimePassword $ text "p"
                }
              )
            `Hspec.shouldBe` text "setup u p"

    Hspec.describe "Command" $ do

      Hspec.describe "textToCommand" $ do

        Hspec.it "parses a revolio command" $ do
          Revolio.textToCommand (text "/revolio")
            `Hspec.shouldBe` Right Revolio.CommandRevolio

        Hspec.it "rejects an invalid command" $ do
          Revolio.textToCommand (text "invalid command")
            `Hspec.shouldSatisfy` Either.isLeft

      Hspec.describe "commandToText" $ do

        Hspec.it "converts a command into text" $ do
          Revolio.commandToText Revolio.CommandRevolio
            `Hspec.shouldBe` text "/revolio"

    Hspec.describe "Config" $ do

      Hspec.it "returns the default config" $ do
        Revolio.getConfig "" []
          `Hspec.shouldBe` ("", Right Revolio.defaultConfig)

      Hspec.it "warns about unexpected arguments" $ do
        Revolio.getConfig "" ["it"] `Hspec.shouldNotSatisfy` (null . fst)

      Hspec.it "warns about unknown options" $ do
        Revolio.getConfig "" ["--it"] `Hspec.shouldNotSatisfy` (null . fst)

      Hspec.it "returns the help" $ do
        help <- either pure (fail . show) . snd $ Revolio.getConfig "" ["-?"]
        help `Hspec.shouldNotSatisfy` null

      Hspec.it "returns the version" $ do
        version <- either pure (fail . show) . snd $ Revolio.getConfig
          ""
          ["-v"]
        version `Hspec.shouldNotSatisfy` null

      Hspec.it "sets the Stratus Time client ID" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-c", "it"]
        Revolio.configClient config
          `Hspec.shouldBe` Revolio.textToStratusTimeClientId (text "it")

      Hspec.it "sets the host" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-h", "*"]
        Revolio.configHost config `Hspec.shouldBe` String.fromString "*"

      Hspec.it "sets the port" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-p", "80"]
        Revolio.configPort config `Hspec.shouldBe` 80

      Hspec.it "sets the Slack signing secret" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-s", "it"]
        Revolio.configSecret config
          `Hspec.shouldBe` Revolio.textToSlackSigningSecret (text "it")

      Hspec.it "sets the Stratus Time base URL" $ do
        config <- either fail pure . snd $ Revolio.getConfig
          ""
          ["-u", "http://test"]
        Revolio.configUrl config
          `Hspec.shouldBe` Revolio.textToStratusTimeBaseUrl
                             (text "http://test")

    Hspec.describe "Direction" $ do

      Hspec.it "can be used as a query value" $ do
        Http.toQueryValue Revolio.DirectionIn
          `Hspec.shouldBe` (Just $ utf8 "2")
        Http.toQueryValue Revolio.DirectionOut
          `Hspec.shouldBe` (Just $ utf8 "3")

      Hspec.describe "textToDirection" $ do

        Hspec.it "parses a valid direction" $ do
          Revolio.textToDirection (text "in")
            `Hspec.shouldBe` Right Revolio.DirectionIn
          Revolio.textToDirection (text "out")
            `Hspec.shouldBe` Right Revolio.DirectionOut

        Hspec.it "rejects an invalid direction" $ do
          Revolio.textToDirection (text "invalid direction")
            `Hspec.shouldSatisfy` Either.isLeft

      Hspec.describe "directionToText" $ do

        Hspec.it "converts a direction into text" $ do
          Revolio.directionToText Revolio.DirectionIn
            `Hspec.shouldBe` text "in"
          Revolio.directionToText Revolio.DirectionOut
            `Hspec.shouldBe` text "out"

    Hspec.describe "Payload" $ do

      Hspec.describe "queryToPayload" $ do

        let
          parse :: [(String, String)] -> Either String Revolio.Payload
          parse =
            Revolio.queryToPayload . Http.renderQuery False . Http.toQuery

        Hspec.it "parses a valid payload" $ do
          url <- either fail pure . Revolio.textToUrl $ text "http://test"
          let
            payload = Revolio.Payload
              { Revolio.payloadAction = Revolio.ActionHelp
              , Revolio.payloadCommand = Revolio.CommandRevolio
              , Revolio.payloadResponseUrl = url
              , Revolio.payloadUserId = Revolio.textToSlackUserId $ text "U1"
              }
          parse
              [ ("user_id", "U1")
              , ("command", "/revolio")
              , ("text", "help")
              , ("response_url", "http://test")
              ]
            `Hspec.shouldBe` Right payload

        Hspec.it "rejects a payload without required information" $ do
          parse [] `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "Slack" $ do

      Hspec.describe "Message" $ do

        Hspec.it "can be converted into JSON" $ do
          let
            expected = LazyByteString.fromStrict
              $ utf8 "{\"text\":\":wave:\",\"response_type\":\"ephemeral\"}"
          (Aeson.encode . Revolio.textToSlackMessage $ text ":wave:")
            `Hspec.shouldBe` expected

      Hspec.describe "SigningSecret" $ do

        Hspec.it "can be used as a byte array" $ do
          (Memory.length . Revolio.textToSlackSigningSecret $ text "123")
            `Hspec.shouldBe` 3

    Hspec.describe "StratusTime" $ do

      Hspec.describe "StratusTimeClientId" $ do

        Hspec.it "can be used as a query value" $ do
          (Http.toQueryValue . Revolio.textToStratusTimeClientId $ text "123")
            `Hspec.shouldBe` (Just $ utf8 "123")

      Hspec.describe "StratusTimeLoginId" $ do

        Hspec.it "can be used as a query value" $ do
          (Http.toQueryValue . Revolio.textToStratusTimeLoginId $ text "you")
            `Hspec.shouldBe` (Just $ utf8 "you")

      Hspec.describe "StratusTimePassword" $ do

        Hspec.it "can be used as a query value" $ do
          (Http.toQueryValue . Revolio.textToStratusTimePassword $ text "axe")
            `Hspec.shouldBe` (Just $ utf8 "axe")

    Hspec.describe "Url" $ do

      Hspec.describe "textToUrl" $ do

        Hspec.it "fails with an invalid URL" $ do
          Revolio.textToUrl (text "invalid URL")
            `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "success with a valid URL" $ do
          Revolio.textToUrl (text "http://test")
            `Hspec.shouldSatisfy` Either.isRight

      Hspec.describe "urlToText" $ do

        Hspec.it "renders a URL as text" $ do
          let expected = text "http://test"
          url <- either fail pure $ Revolio.textToUrl expected
          Revolio.urlToText url `Hspec.shouldBe` expected

  Hspec.describe "Version" $ do

    Hspec.describe "version" $ do

      Hspec.it "has four branches" $ do
        Version.versionBranch Revolio.version
          `Hspec.shouldSatisfy` ((== 4) . length)

      Hspec.it "starts branches with one" $ do
        Version.versionBranch Revolio.version
          `Hspec.shouldSatisfy` List.isPrefixOf [1]

      Hspec.it "has no tags" $ do
        versionTags Revolio.version `Hspec.shouldSatisfy` null

text :: String -> Text.Text
text = Text.pack

utf8 :: String -> ByteString.ByteString
utf8 = Encoding.encodeUtf8 . text

versionTags :: Version.Version -> [String]
versionTags (Version.Version _ tags) = tags

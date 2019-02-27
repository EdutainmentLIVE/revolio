{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as Memory
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.QueryLike as Http
import qualified Revolio
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Revolio" $ do

  Hspec.describe "Type" $ do

    Hspec.describe "Action" $ do

      Hspec.describe "textToAction" $ do

        Hspec.it "parses a help action" $ do
          Revolio.textToAction (Text.pack "help")
            `Hspec.shouldBe` Right Revolio.ActionHelp

        Hspec.it "parses a setup action" $ do
          let
            action = Revolio.ActionSetup
              (Revolio.textToPaychexLoginId $ Text.pack "username")
              (Revolio.textToPaychexPassword $ Text.pack "password")
          Revolio.textToAction (Text.pack "setup username password")
            `Hspec.shouldBe` Right action

        Hspec.it "parses a clock in action" $ do
          Revolio.textToAction (Text.pack "in")
            `Hspec.shouldBe` Right (Revolio.ActionClock Revolio.DirectionIn)

        Hspec.it "parses a clock out action" $ do
          Revolio.textToAction (Text.pack "out")
            `Hspec.shouldBe` Right (Revolio.ActionClock Revolio.DirectionOut)

        Hspec.it "rejects an invalid action" $ do
          Revolio.textToAction (Text.pack "invalid action")
            `Hspec.shouldSatisfy` Either.isLeft

      Hspec.describe "actionToText" $ do

        Hspec.it "converts an action into text" $ do
          Revolio.actionToText (Revolio.ActionClock Revolio.DirectionIn)
            `Hspec.shouldBe` Text.pack "in"
          Revolio.actionToText (Revolio.ActionClock Revolio.DirectionOut)
            `Hspec.shouldBe` Text.pack "out"
          Revolio.actionToText Revolio.ActionHelp
            `Hspec.shouldBe` Text.pack "help"
          Revolio.actionToText
              (Revolio.ActionSetup
                (Revolio.textToPaychexLoginId $ Text.singleton 'u')
                (Revolio.textToPaychexPassword $ Text.singleton 'p')
              )
            `Hspec.shouldBe` Text.pack "setup u p"

    Hspec.describe "Command" $ do

      Hspec.describe "textToCommand" $ do

        Hspec.it "parses a clock command" $ do
          Revolio.textToCommand (Text.pack "/clock")
            `Hspec.shouldBe` Right Revolio.CommandClock

        Hspec.it "rejects an invalid command" $ do
          Revolio.textToCommand (Text.pack "invalid command")
            `Hspec.shouldSatisfy` Either.isLeft

      Hspec.describe "commandToText" $ do

        Hspec.it "converts a command into text" $ do
          Revolio.commandToText Revolio.CommandClock
            `Hspec.shouldBe` Text.pack "/clock"

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

      Hspec.it "sets the Paychex client ID" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-c", "it"]
        Revolio.configClient config
          `Hspec.shouldBe` Revolio.textToPaychexClientId (Text.pack "it")

      Hspec.it "sets the host" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-h", "*"]
        Revolio.configHost config `Hspec.shouldBe` String.fromString "*"

      Hspec.it "sets the port" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-p", "80"]
        Revolio.configPort config `Hspec.shouldBe` 80

      Hspec.it "sets the Slack signing secret" $ do
        config <- either fail pure . snd $ Revolio.getConfig "" ["-s", "it"]
        Revolio.configSecret config
          `Hspec.shouldBe` Revolio.textToSlackSigningSecret (Text.pack "it")

    Hspec.describe "Direction" $ do

      Hspec.it "can be used as a query value" $ do
        Http.toQueryValue Revolio.DirectionIn
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.singleton '2')
        Http.toQueryValue Revolio.DirectionOut
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.singleton '3')

      Hspec.describe "textToDirection" $ do

        Hspec.it "parses a valid direction" $ do
          Revolio.textToDirection (Text.pack "in")
            `Hspec.shouldBe` Right Revolio.DirectionIn
          Revolio.textToDirection (Text.pack "out")
            `Hspec.shouldBe` Right Revolio.DirectionOut

        Hspec.it "rejects an invalid direction" $ do
          Revolio.textToDirection (Text.pack "invalid direction")
            `Hspec.shouldSatisfy` Either.isLeft

      Hspec.describe "directionToText" $ do

        Hspec.it "converts a direction into text" $ do
          Revolio.directionToText Revolio.DirectionIn
            `Hspec.shouldBe` Text.pack "in"
          Revolio.directionToText Revolio.DirectionOut
            `Hspec.shouldBe` Text.pack "out"

    Hspec.describe "PaychexClientId" $ do

      Hspec.it "can be used as a query value" $ do
        (Http.toQueryValue . Revolio.textToPaychexClientId $ Text.pack "123")
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.pack "123")

    Hspec.describe "PaychexLoginId" $ do

      Hspec.it "can be used as a query value" $ do
        (Http.toQueryValue . Revolio.textToPaychexLoginId $ Text.pack "you")
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.pack "you")

    Hspec.describe "PaychexPassword" $ do

      Hspec.it "can be used as a query value" $ do
        (Http.toQueryValue . Revolio.textToPaychexPassword $ Text.pack "axe")
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.pack "axe")

    Hspec.describe "Payload" $ do

      Hspec.describe "queryToPayload" $ do

        let
          parse :: [(String, String)] -> Either String Revolio.Payload
          parse =
            Revolio.queryToPayload . Http.renderQuery False . Http.toQuery

        Hspec.it "parses a valid payload" $ do
          let
            payload = Revolio.Payload
              { Revolio.payloadAction = Revolio.ActionHelp
              , Revolio.payloadCommand = Revolio.CommandClock
              , Revolio.payloadResponseUrl =
                either undefined id . Revolio.textToUrl $ Text.pack
                  "http://slack.test"
              , Revolio.payloadUserId = Revolio.textToSlackUserId
                $ Text.pack "U1"
              }
          parse
              [ ("user_id", "U1")
              , ("command", "/clock")
              , ("text", "help")
              , ("response_url", "http://slack.test")
              ]
            `Hspec.shouldBe` Right payload

        Hspec.it "rejects a payload without required information" $ do
          parse [] `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "SlackMessage" $ do

      Hspec.it "can be converted into JSON" $ do
        let
          expected =
            LazyByteString.fromStrict . Encoding.encodeUtf8 $ Text.pack
              "{\"text\":\":wave:\",\"response_type\":\"ephemeral\"}"
        (Aeson.encode . Revolio.textToSlackMessage $ Text.pack ":wave:")
          `Hspec.shouldBe` expected

    Hspec.describe "SlackSigningSecret" $ do

      Hspec.it "can be used as a byte array" $ do
        (Memory.length . Revolio.textToSlackSigningSecret $ Text.pack "123")
          `Hspec.shouldBe` 3

    Hspec.describe "Url" $ do

      Hspec.describe "textToUrl" $ do

        Hspec.it "fails with an invalid URL" $ do
          Revolio.textToUrl (Text.pack "invalid URL")
            `Hspec.shouldSatisfy` Either.isLeft

        Hspec.it "success with a valid URL" $ do
          Revolio.textToUrl (Text.pack "http://revolio.test")
            `Hspec.shouldSatisfy` Either.isRight

      Hspec.describe "urlToText" $ do

        Hspec.it "renders a URL as text" $ do
          let text = Text.pack "http://revolio.test"
          url <- either fail pure $ Revolio.textToUrl text
          Revolio.urlToText url `Hspec.shouldBe` text

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

versionTags :: Version.Version -> [String]
versionTags (Version.Version _ tags) = tags

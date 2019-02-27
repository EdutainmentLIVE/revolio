{- HLINT ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Data.ByteArray as Memory
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Network.HTTP.Types.QueryLike as Http
import qualified Revolio
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Revolio" $ do

  Hspec.describe "Type" $ do

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

    Hspec.describe "PaychexClientId" $ do

      Hspec.it "can be used as a query value" $ do
        (Http.toQueryValue . Revolio.textToPaychexClientId $ Text.pack "123")
          `Hspec.shouldBe` (Just . Encoding.encodeUtf8 $ Text.pack "123")

    Hspec.describe "SlackSigningSecret" $ do

      Hspec.it "can be used as a byte array" $ do
        (Memory.length . Revolio.textToSlackSigningSecret $ Text.pack "123")
          `Hspec.shouldBe` 3

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

{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.ParsingSpec (spec) where

import NeatInterpolation
import StaticLS.IDE.CodeActions.Parse
import StaticLS.IDE.CodeActions.InsertFields
import Test.Hspec (Spec, fdescribe, it, shouldBe)

spec :: Spec
spec = do
  fdescribe "parsing to support code actions" do
    it "parses fields not initialized" do
      let result =
            missingFields . normalize $
              [trimming|
                • Fields of ‘Person’ not initialised:
                    firstName :: String
                    middleName :: Maybe String
                    lastName :: String
                    parents :: [Person]
                • In the expression: Person {}
                In an equation for ‘person’: person = Person {}
            |]
      result `shouldBe` Just (normalize "Person", Nothing, fmap normalize ["firstName", "middleName", "lastName", "parents"])
    it "parses missing strict fields" do
      let result =
            missingFields . normalize $
              [trimming|
                • Constructor ‘Person’ does not have the required strict field(s):
                    firstName :: String
                    middleName :: Maybe String
                    lastName :: String
                    parents :: [Person]
                • In the second argument of ‘($)’, namely
                    ‘Person {}’
            |]
      result `shouldBe` Just (normalize "Person", Nothing, fmap normalize ["firstName", "middleName", "lastName", "parents"])
    it "handles partially incomplete records" do
      let result =
            missingFields . normalize $
              [trimming|
                • Fields of ‘Person’ not initialised:
                    middleName :: Maybe String
                    lastName :: String
                • In the expression: Person {firstName = _, parents = _}
                  In an equation for ‘a’: a = Person {firstName = _, parents = _}
            |]
      case result of
        Just (constructor, existingFields, missingFields) ->
          renderFields (getNormalText constructor) (fmap getNormalText existingFields) (fmap getNormalText missingFields) 0 `shouldBe` [trimming|
            Person
              { middleName = _
              , lastName = _
              , firstName = _
              , parents = _
              }
          |]
        Nothing -> fail "failed to parse"
    it "parses missing cases" do
      let result =
            nonExhaustivePatterns . normalize $
              [trimming|
                Pattern match(es) are non-exhaustive
                In a \case alternative:
                    Patterns of type ‘Maybe a’ not matched:
                        Nothing
                        Just _
            |]
      result `shouldBe` (Just $ fmap normalize ["Nothing", "Just _"])

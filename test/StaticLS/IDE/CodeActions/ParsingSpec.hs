{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.ParsingSpec (spec) where

import NeatInterpolation
import StaticLS.IDE.CodeActions.Parse
import Test.Hspec (Spec, fdescribe, it, shouldBe)

spec :: Spec
spec = do
  fdescribe "parsing to support code actions" do
    it "parses fields not initialized" do
      let result =
            fieldsNotInitialized . normalize $
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
            requiredStrictFields . normalize $
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
    it "parses missing cases 2" do
      let result =
            nonExhaustivePatterns . normalize $
              [trimming|
                Pattern match(es) are non-exhaustive
                In a \case alternative:
                    Patterns of type ‘ProposalType’ not matched:
                        ListCostProposal
                        ListCostCancellationProposal
                        PurchaseDealProposal
                        TobaccoRebateProposal
                        TariffProposal
            |]
      result `shouldBe` (Just $ fmap normalize ["ListCostProposal", "ListCostCancellationProposal", "PurchaseDealProposal", "TobaccoRebateProposal", "TariffProposal"])

{-# LANGUAGE OverloadedStrings #-}
module Text.ProtocolBuffers.Tests.AddressBook
  ( addressBookTests
  , addressBookQuickChecks
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?))
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck ()

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage

import qualified HSCodeGen.AddressBookProtos.AddressBook        as AddressBook'
import qualified HSCodeGen.AddressBookProtos.Person             as Person'
import qualified HSCodeGen.AddressBookProtos.Person.PhoneNumber as PhoneNumber'
import HSCodeGen.AddressBookProtos.AddressBook        (AddressBook(..))
import HSCodeGen.AddressBookProtos.Person             (Person(..))
import HSCodeGen.AddressBookProtos.Person.PhoneNumber (PhoneNumber(..))
import HSCodeGen.AddressBookProtos.Person.PhoneType   (PhoneType(..))

addressBookTests :: TestTree
addressBookTests = testGroup "Address book tests"
  [ testCase "Address book text-encoded then decoded identity 1" $
      roundTripTextEncodeDecode addressBook1 @? "text-encoded then decoded was not an identity"
  , testCase "Address book text-encoded then decoded identity 2" $
      roundTripTextEncodeDecode addressBook2 @? "text-encoded then decoded was not an identity"
  , testCase "Address book text-encoded then decoded identity 3" $
      roundTripTextEncodeDecode addressBook3 @? "text-encoded then decoded was not an identity"
  , testCase "Address book wire-encoded then decoded identity 1" $
      roundTripWireEncodeDecode addressBook1 @? "wire-encoded then decoded was not an identity"
  , testCase "Address book wire-encoded then decoded identity 2" $
      roundTripTextEncodeDecode addressBook2 @? "wire-encoded then decoded was not an identity"
  , testCase "Address book wire-encoded then decoded identity 3" $
      roundTripTextEncodeDecode addressBook3 @? "wire-encoded then decoded was not an identity"
  ]

addressBookQuickChecks :: TestTree
addressBookQuickChecks = testGroup "Address book QuickChecks"
  [ QC.testProperty "Address book wire-encoded then decoded identity" $ roundTripWireEncodeDecode
  , QC.testProperty "Address book text-encoded then decoded identity" $ roundTripTextEncodeDecode
  ]

roundTripTextEncodeDecode :: AddressBook -> Bool
roundTripTextEncodeDecode addressBook =
  let encoded = messagePutText addressBook
      decoded = case messageGetText $ LB.pack encoded of
                  Left _ -> False
                  Right result -> result == addressBook
  in decoded

roundTripWireEncodeDecode :: AddressBook -> Bool
roundTripWireEncodeDecode addressBook =
  let encoded = messagePut addressBook
      decoded = case messageGet encoded of
                  Right (result, "") -> result == addressBook
                  _ -> False
  in decoded

addressBook1 :: AddressBook
addressBook1 =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "Alice" (-1) (Just "alice@example.com") $ Seq.singleton ("123-456-7890", Just HOME)
      , mkPerson "Bob" 2 Nothing $ Seq.fromList [("1-800-123-4567", Just MOBILE), ("604-291-1234", Just WORK)]
      ]
  , AddressBook'.unknown'field = defaultValue
  }

addressBook2 :: AddressBook
addressBook2 =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "Nobody" 2 Nothing $ Seq.singleton ("111-111-1111", Nothing)
      ]
  , AddressBook'.unknown'field = defaultValue
  }

addressBook3 :: AddressBook
addressBook3 =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "" 1 (Just "\550252") $ Seq.singleton ("", Just HOME)
      ]
  , AddressBook'.unknown'field = defaultValue
  }

mkPerson :: String -> Int -> Maybe String -> Seq (String, Maybe PhoneType) -> Person
mkPerson name id' email phoneNumbers =
  Person {
    Person'.name = uFromString name
  , Person'.id = fromIntegral id'
  , Person'.email = uFromString <$> email
  , Person'.phone = mkPhoneNumbers phoneNumbers
  , Person'.unknown'field = defaultValue
  }

mkPhoneNumbers :: Seq (String, Maybe PhoneType) -> Seq PhoneNumber
mkPhoneNumbers = fmap mkPhoneNumbers' where
  mkPhoneNumbers' (num, phoneType) =
    PhoneNumber {
      PhoneNumber'.number = uFromString num
    , PhoneNumber'.type' = phoneType
    , PhoneNumber'.unknown'field = defaultValue
    }

instance Arbitrary AddressBook where
  arbitrary = AddressBook <$> arbitrary
                          <*> pure defaultValue

instance Arbitrary Person where
  arbitrary = Person <$> liftA uFromString arbitrary
                     <*> arbitrary
                     <*> frequency [ (2, liftA (Just . uFromString) arbitrary)
                                   , (1, pure Nothing)]
                     <*> liftA Seq.fromList (listOf arbitrary)
                     <*> pure defaultValue

instance Arbitrary PhoneNumber where
  arbitrary = PhoneNumber <$> liftA uFromString arbitrary
                          <*> liftA Just (elements [HOME, WORK, MOBILE])
                          -- <*> frequency [ (3, liftA Just $ elements [HOME, WORK, MOBILE])
                          --               , (1, pure Nothing)]
                          <*> pure defaultValue

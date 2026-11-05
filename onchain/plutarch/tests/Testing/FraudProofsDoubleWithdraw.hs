{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsDoubleWithdraw (tests) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptHash (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.MerkleTree.Helpers (pcombine, pnibble, pnibbles, psuffix)
import Plutarch.MerkleTree.Merkling (psparse_merkle_16)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.DoubleWithdraw
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Double-withdraw fraud proof"
  [ testCase "step 01 binds the first payable leaf" $
      psucceeds $ step01 $ step01Context fraudulent (bFirstProof fraudulent) (stateFromFirst fraudulent) threadName
  , testCase "step 01 binds the second payable leaf" $
      psucceeds $ step01 $ step01Context fraudulent (bSecondProof fraudulent) (stateFromSecond fraudulent) threadName
  , testCase "step 01 rejects a non-payable leaf" $
      pfails $ step01 $ step01Context honestDuplicate (bSecondProof honestDuplicate) (stateFromSecond honestDuplicate) threadName
  , testCase "step 01 rejects a forged withdrawals root" $
      pfails $ step01 $ step01Context fraudulent (bFirstProof distinctOutrefs) (stateFromFirst distinctOutrefs) threadName
  , testCase "step 01 rejects a wrong withdrawal count" $
      pfails $ step01 $ step01Context fraudulent (withCount 3 $ bFirstProof fraudulent) (stateFromFirst fraudulent) threadName
  , testCase "step 01 rejects a wrong header identity" $
      pfails $ step01 $ step01Context fraudulent (bFirstProof fraudulent) (stateFromFirst fraudulent) otherThreadName
  , testCase "step 01 cancels under the prover signature" $
      psucceeds $ step01 $ cancellationContext Nothing
  , testCase "step 01 rejects an unsigned cancellation" $
      pfails $ step01 $ withoutSignatories $ cancellationContext Nothing
  , testCase "step 02 accepts the exact fault from the first leaf" $
      psucceeds $ step02 $ step02Context fraudulent (stateFromFirst fraudulent) (bSecondProof fraudulent)
  , testCase "step 02 accepts the exact fault from the second leaf" $
      psucceeds $ step02 $ step02Context fraudulent (stateFromSecond fraudulent) (bFirstProof fraudulent)
  , testCase "step 02 rejects the same leaf twice" $
      pfails $ step02 $ step02Context fraudulent (stateFromFirst fraudulent) (bFirstProof fraudulent)
  , testCase "step 02 rejects distinct L2 output references" $
      pfails $ step02 $ step02Context distinctOutrefs (stateFromFirst distinctOutrefs) (bSecondProof distinctOutrefs)
  , testCase "step 02 rejects an honest non-payable duplicate" $
      pfails $ step02 $ step02Context honestDuplicate (stateFromFirst honestDuplicate) (bSecondProof honestDuplicate)
  , testCase "step 02 rejects forged membership" $
      pfails $ step02 $ step02Context fraudulent (stateFromFirst fraudulent) (bSecondProof distinctOutrefs)
  , testCase "step 02 rejects a wrong withdrawal count" $
      pfails $ step02 $ step02Context fraudulent (stateFromFirst fraudulent) (withCount 3 $ bSecondProof fraudulent)
  , testCase "step 02 rejects a carried header mismatch" $
      pfails $ step02 $ step02Context fraudulent (withHeaderHash (BS.replicate 28 0xbb) $ stateFromFirst fraudulent) (bSecondProof fraudulent)
  , testCase "step 02 cancels under the prover signature" $
      psucceeds $ step02 $ cancellationContext (Just $ stateFromFirst fraudulent)
  , testCase "step 02 rejects an unsigned cancellation" $
      pfails $ step02 $ withoutSignatories $ cancellationContext (Just $ stateFromFirst fraudulent)
  ]

data Block = Block
  { bRawRoot :: BS.ByteString
  , bCountedRoot :: BS.ByteString
  , bFirstId :: PD.Data
  , bFirstOutref :: (BS.ByteString, Integer)
  , bFirstProof :: PD.Data
  , bSecondId :: PD.Data
  , bSecondOutref :: (BS.ByteString, Integer)
  , bSecondProof :: PD.Data
  }

firstId, secondId :: PD.Data
firstId = inputData (hash32 0xc4, 0)
secondId = inputData (hash32 0xd4, 1)

sharedWithdrawalOutref, otherWithdrawalOutref :: (BS.ByteString, Integer)
sharedWithdrawalOutref = (hash32 0xb1, 0)
otherWithdrawalOutref = (hash32 0xb2, 1)

valid, spent :: PD.Data
valid = PD.Constr 0 []
spent = PD.Constr 2 [PD.B $ hash32 0xee]

fraudulent, honestDuplicate, distinctOutrefs :: Block
fraudulent = mkBlock sharedWithdrawalOutref valid sharedWithdrawalOutref valid
honestDuplicate = mkBlock sharedWithdrawalOutref valid sharedWithdrawalOutref spent
distinctOutrefs = mkBlock sharedWithdrawalOutref valid otherWithdrawalOutref valid

mkBlock :: (BS.ByteString, Integer) -> PD.Data -> (BS.ByteString, Integer) -> PD.Data -> Block
mkBlock firstOutref firstValidity secondOutref secondValidity = block
  where
    firstInfo = withdrawalInfoData firstOutref firstValidity
    secondInfo = withdrawalInfoData secondOutref secondValidity
    firstKeyBytes = serialise firstId
    secondKeyBytes = serialise secondId
    firstValueBytes = serialise firstInfo
    secondValueBytes = serialise secondInfo
    rawRoot = twoLeafRoot firstKeyBytes firstValueBytes secondKeyBytes secondValueBytes
    countedRoot = commitCountedRoot withdrawalsDomain rawRoot 2
    firstProof = rootMembership countedRoot rawRoot firstId firstInfo $
      leafProof firstKeyBytes secondKeyBytes secondValueBytes
    secondProof = rootMembership countedRoot rawRoot secondId secondInfo $
      leafProof secondKeyBytes firstKeyBytes firstValueBytes
    block = Block
      { bRawRoot = rawRoot
      , bCountedRoot = countedRoot
      , bFirstId = firstId
      , bFirstOutref = firstOutref
      , bFirstProof = firstProof
      , bSecondId = secondId
      , bSecondOutref = secondOutref
      , bSecondProof = secondProof
      }

rootMembership :: BS.ByteString -> BS.ByteString -> PD.Data -> PD.Data -> PD.Data -> PD.Data
rootMembership root rawRoot key value proof = PD.Constr 0
  [ PD.Constr withdrawalsDomain []
  , PD.B root
  , PD.B rawRoot
  , PD.I 2
  , key
  , value
  , proof
  ]

leafProof :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
leafProof ownKey neighborKey neighborValue =
  PD.List [PD.Constr 2 [PD.I skip, PD.B neighborPath, PD.B $ blake2b256 neighborValue]]
  where
    ownPath = blake2b256 ownKey
    neighborPath = blake2b256 neighborKey
    skip = commonNibblePrefix ownPath neighborPath

twoLeafRoot :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
twoLeafRoot firstKey firstValue secondKey secondValue = plift @PByteString $
  pcombine
    # (pnibbles # pconstant firstPath # 0 # pconstant skip)
    # ( psparse_merkle_16
          # (pnibble # pconstant firstPath # pconstant skip)
          # (pcombine # (psuffix # pconstant firstPath # pconstant nextCursor) # pconstant firstValueHash)
          # (pnibble # pconstant secondPath # pconstant skip)
          # (pcombine # (psuffix # pconstant secondPath # pconstant nextCursor) # pconstant secondValueHash)
      )
  where
    firstPath = blake2b256 firstKey
    secondPath = blake2b256 secondKey
    firstValueHash = blake2b256 firstValue
    secondValueHash = blake2b256 secondValue
    skip = commonNibblePrefix firstPath secondPath
    nextCursor = skip + 1

commonNibblePrefix :: BS.ByteString -> BS.ByteString -> Integer
commonNibblePrefix left right = go 0
  where
    go i
      | i >= 2 * min (BS.length left) (BS.length right) = toInteger i
      | nibbleAt left i == nibbleAt right i = go (i + 1)
      | otherwise = toInteger i

nibbleAt :: BS.ByteString -> Int -> Int
nibbleAt bytes i
  | even i = fromIntegral (BS.index bytes (i `div` 2) `shiftR` 4)
  | otherwise = fromIntegral (BS.index bytes (i `div` 2) .&. 0x0f)

stateFromFirst, stateFromSecond :: Block -> PD.Data
stateFromFirst block = stateData (BS.drop 4 threadName) (bFirstId block) (bFirstOutref block)
stateFromSecond block = stateData (BS.drop 4 threadName) (bSecondId block) (bSecondOutref block)

stateData :: BS.ByteString -> PD.Data -> (BS.ByteString, Integer) -> PD.Data
stateData headerHash withdrawalId' l2Outref =
  PD.Constr 0 [PD.B headerHash, withdrawalId', inputData l2Outref]

withHeaderHash :: BS.ByteString -> PD.Data -> PD.Data
withHeaderHash headerHash (PD.Constr 0 (_ : rest)) = PD.Constr 0 (PD.B headerHash : rest)
withHeaderHash _ _ = error "invalid double-withdraw state fixture"

withCount :: Integer -> PD.Data -> PD.Data
withCount count (PD.Constr 0 [domain, root, rawRoot, _, key, value, proof]) =
  PD.Constr 0 [domain, root, rawRoot, PD.I count, key, value, proof]
withCount _ _ = error "invalid root membership fixture"

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx = doubleWithdrawStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant ctx
step02 ctx = doubleWithdrawStep02Validator
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant ctx

step01Context :: Block -> PD.Data -> PD.Data -> BS.ByteString -> ScriptContext
step01Context block proof outputState assetName = spendContext
  (stepDatum Nothing)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 1, proof]])
  [threadInputWithName assetName]
  [stepOutputWithName nextScript (Just outputState) assetName]
  (referenceInputsWithWithdrawalsRoot (bCountedRoot block) 2)
  []
  mempty

step02Context :: Block -> PD.Data -> PD.Data -> ScriptContext
step02Context block state proof = spendContext
  (stepDatum $ Just state)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.I 1, proof]])
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  (referenceInputsWithWithdrawalsRoot (bCountedRoot block) 2)
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

cancellationContext :: Maybe PD.Data -> ScriptContext
cancellationContext state = spendContext
  (stepDatum state)
  cancelRedeemer
  [threadInput]
  []
  []
  [cancelMintEntry threadName]
  mempty

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo

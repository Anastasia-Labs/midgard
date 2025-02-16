{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.MidgardValidator (tests, dumpDebug) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Midgard.Utils (pand'List, pintToByteString)
import Testing.Eval (psucceeds, passert, toBuiltinHexString)
import Plutarch.Crypto 
import Midgard.Crypto (pethereumPubKeyToPubKeyHash, pcompressPublicKey)
import Plutarch.Prelude
import PlutusCore.Crypto.Hash qualified as Hash
import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value qualified as Value 
import PlutusTx.AssocMap qualified as AssocMap
import Data.ByteString (ByteString)
import Prettyprinter (Pretty (pretty), layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.String (renderString)
import Data.List (findIndices, sortOn)
import PlutusTx.Eq qualified
import Types.StateCommitment
import Types.Blocks
import qualified Data.ByteString as BS
import MerkleTree.MerklePatriciaForestry (MerklePatriciaForestry(..))


tests :: TestTree
tests = testGroup "AirdropGlobalValidator Tests"
  [ testCase "Bulk Claim" $ 
      assertBool "This pass" True
  ]

instance PlutusTx.Eq.Eq ScriptPurpose where
  (==) = (==)

divCeil :: Integral a => a -> a -> a
divCeil x y = 1 + div (x - 1) y

authCS :: CurrencySymbol
authCS = "deadbeef"

mockVestingStart :: POSIXTime
mockVestingStart = 1684449918000 -- May 19, 2023, 00:05:18 GMT

mockVestingEnd :: POSIXTime
mockVestingEnd = mockVestingStart + 30 * 24 * 60 * 60 * 1000 -- 30 days after start

mockCurrentTime :: POSIXTime
mockCurrentTime = mockVestingStart + 10 * 24 * 60 * 60 * 1000 -- 10 days after start

mockValidityRange :: Interval POSIXTime
mockValidityRange = Interval 
    { ivFrom = LowerBound (Finite mockCurrentTime) True
    , ivTo = UpperBound (Finite (mockCurrentTime + 60000)) False -- Valid for 1 minute
    }

globalLogicScriptContext :: ScriptContext 
globalLogicScriptContext = 
  ScriptContext 
    { scriptContextTxInfo = txInfoBase
    , scriptContextRedeemer = globalLogicRedeemer
    , scriptContextScriptInfo =  RewardingScript (ScriptCredential "deadbeef")
    }

globalLogicRedeemer :: Redeemer
globalLogicRedeemer = Redeemer {getRedeemer = toBuiltinData ()}

txInfoBase :: TxInfo
txInfoBase =
  TxInfo
    { -- Every tx info must contain a reference to the authorised script we are testing.
      txInfoReferenceInputs = []
    , txInfoInputs = []
    , txInfoRedeemers = AssocMap.unsafeFromList []
    , txInfoOutputs = []
    , txInfoFee = Lovelace 700_000
    , txInfoMint = Value $ AssocMap.safeFromList []
    , txInfoTxCerts = []
    , txInfoWdrl = AssocMap.unsafeFromList []
    , txInfoValidRange = mockValidityRange
    , txInfoSignatories = []
    , txInfoData = AssocMap.safeFromList []
    , txInfoId = TxId ""
    , txInfoVotes                 = AssocMap.unsafeFromList []
    , txInfoProposalProcedures    = [] 
    , txInfoCurrentTreasuryAmount = Nothing 
    , txInfoTreasuryDonation      = Nothing
    }

mockMidgardValidatorAddress :: Address 
mockMidgardValidatorAddress = 
  Address
    { addressCredential = ScriptCredential "11111111111111111111111111111111111111111111111111111111"
    , addressStakingCredential = Nothing
    }

mockPKH :: PubKeyHash 
mockPKH = PubKeyHash $ toBuiltinHexString "deadbeef"

sortValue :: Value -> Value
sortValue (Value m) = Value $ AssocMap.unsafeFromList $ sortOn (csToBS . fst) $ map sortInner $ AssocMap.toList m
  where
    csToBS :: CurrencySymbol -> BuiltinByteString
    csToBS (CurrencySymbol bs) = bs

    sortInner (cs, innerMap) = (cs, AssocMap.unsafeFromList $ sortOn (tnToBS . fst) $ AssocMap.toList innerMap)
    
    tnToBS :: TokenName -> BuiltinByteString
    tnToBS (TokenName bs) = bs

mockStateCommitmentSetNodeDatum :: BuiltinByteString -> BuiltinByteString -> StateCommitment -> StateCommitmentSetNode
mockStateCommitmentSetNodeDatum key next commitment = 
    MkStateCommitmentSetNode
      { key = key
      , next = next 
      , blockInfo = commitment
      }

midgardValidatorTxOut :: BuiltinByteString -> BuiltinByteString -> StateCommitment -> TxOut
midgardValidatorTxOut key next commitment =
  TxOut
    { txOutAddress = mockMidgardValidatorAddress
    , txOutValue = 
        sortValue (Value.singleton adaSymbol adaToken 2_000_000 
          <> Value.singleton midgardNodeCS (TokenName $ "FSN" <> key) 1)
    , txOutDatum = OutputDatum ( Datum {getDatum = toBuiltinData $ mockStateCommitmentSetNodeDatum key next commitment} )
    , txOutReferenceScript = Nothing
    }
  where 
    midgardNodeCS = "deadbeef"

midgardValidatorUTxO :: BuiltinByteString -> BuiltinByteString -> StateCommitment -> TxInInfo
midgardValidatorUTxO key next commitment = 
  TxInInfo 
    { txInInfoOutRef = TxOutRef{txOutRefId = "deadbeef", txOutRefIdx = 0}
    , txInInfoResolved = midgardValidatorTxOut key next commitment
    }

calculateFutureInstallments :: POSIXTime -> Integer -> Integer -> Integer
calculateFutureInstallments (POSIXTime currentTime) vestingEnd betweenTrenches =
    let vestingTimeRemaining = vestingEnd - currentTime
        futureInstallments = divCeil vestingTimeRemaining betweenTrenches
    in max 0 futureInstallments

createTestTxInfo :: [TxInInfo] -> TxInfo
createTestTxInfo inputs =
    let (POSIXTime _currentTime) = getLowerBoundTime (txInfoValidRange txInfoBase)
  
        -- create the expected outputs
        outputs = []
        
        -- Create redeemers for each input (Spending) and one for the global logic (Rewarding)
        allRedeemers = map (\i -> (Spending (txInInfoOutRef i), Redeemer $ toBuiltinData ())) inputs
    
    in txInfoBase
        { txInfoInputs = inputs
        , txInfoOutputs = outputs
        , txInfoValidRange = mockValidityRange
        , txInfoRedeemers = AssocMap.unsafeFromList allRedeemers
        }
  where    
    getLowerBoundTime (Interval lower _) = 
        case lower of
            LowerBound (Finite time) _ -> time
            _ -> error "Invalid lower bound"

    _getOutputDatum d = 
      case d of 
        OutputDatum d' -> getDatum d' 
        _ -> error "getOutputDatum: Expected InlineDatum"

-- Example MerklePatriciaForestry
exampleMPF :: MerklePatriciaForestry
exampleMPF = MerklePatriciaForestry $ toBuiltin $ BS.pack [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF]

-- Example Block
exampleBlock :: Block
exampleBlock = Block
  { txRoot = exampleMPF
  , depositEventRoot = exampleMPF
  , withdrawalEventRoot = exampleMPF
  , startTimestamp = POSIXTime 1625097600000 -- Example timestamp (2021-07-01 00:00:00 UTC)
  , endTimestamp = POSIXTime 1625184000000   -- Example timestamp (2021-07-02 00:00:00 UTC)
  , oldStateRoot = exampleMPF
  , newStateRoot = exampleMPF
  , transactionCount = 1000
  }

-- Example StateCommitment
exampleStateCommitment :: StateCommitment
exampleStateCommitment = StateCommitment
  { publisher = toBuiltin $ BS.pack [0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE]
  , block = exampleBlock
  , oldUtxoRoot = exampleMPF
  , newUtxoRoot = exampleMPF
  }

mockInputs :: [TxInInfo]
mockInputs = 
  [ midgardValidatorUTxO "key1" "key2" exampleStateCommitment
  , midgardValidatorUTxO "key2" "key3" exampleStateCommitment
  , midgardValidatorUTxO "key5" "key6" exampleStateCommitment
  ]

succeedsMidgardGlobalScriptContext :: ScriptContext  
succeedsMidgardGlobalScriptContext = 
  let txInfo'@TxInfo{txInfoRedeemers=oldRedeemers} = createTestTxInfo mockInputs
      redeemer = createGlobalLogicRedeemer txInfo
      txInfo = txInfo' {txInfoRedeemers = AssocMap.insert (Rewarding (ScriptCredential "deadbeef")) (Redeemer $ toBuiltinData redeemer) oldRedeemers}
  in  ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = redeemer
        , scriptContextScriptInfo = RewardingScript (ScriptCredential "deadbeef")
        }

createGlobalLogicRedeemer :: TxInfo -> Redeemer
createGlobalLogicRedeemer _txInfo = Redeemer {getRedeemer = glAct}
  where 
    glAct = toBuiltinData () 

findMidgardValidatorInputIndices :: TxInfo -> [Integer]
findMidgardValidatorInputIndices txInfo = 
    map fromIntegral $ findIndices isMidgardValidatorInput (txInfoInputs txInfo)
  where
    isMidgardValidatorInput :: TxInInfo -> Bool
    isMidgardValidatorInput txIn = 
      txOutAddress (txInInfoResolved txIn) == mockMidgardValidatorAddress

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty

dumpDebug :: IO () 
dumpDebug = do 
  putStrLn "dumpDebug:"
  -- let context = succeedsMidgardGlobalScriptContext
  --     txInfo = scriptContextTxInfo context
  -- putStrLn "Inputs:"
  -- mapM_ (print . txInInfoResolved) (txInfoInputs txInfo)
  -- putStrLn "\nOutputs:"
  -- mapM_ print (txInfoOutputs txInfo)
  -- putStrLn "\nRedeemers"
  -- print (txInfoRedeemers txInfo)
  -- putStrLn "\n"
  
  -- putStrLn $ renderPretty txInfo

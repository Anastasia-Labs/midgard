{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputValue (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusLedgerApi.V3 (Data (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CekData (PDataSummaryV1 (..), psemanticDataSummaryV1)
import Midgard.LedgerOutputCommitment (passetLeafHash)
import Midgard.LedgerOutputValue
import Midgard.ValidationMerkle (PFrontierPeak (..), pappendLeaf, pfrontierIsWellFormed)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.LedgerOutputValue"
  [ testCase "folds_the_exact_plutus_value_from_reverse_memberships" $ passertEvalNoTrace foldsExactValue
  , testCase "terminal_cross_language_control_decodes_canonically" $ passertEvalNoTrace terminalControlDecodes
  , testCase "maximum_nested_value_terminal_agrees_with_typescript" $ passertEvalNoTrace maximumTerminalAgrees
  , testCase "substituted_asset_facts_fail_closed" $ passertEvalNoTrace substitutedFactsFail
  , testCase "empty_value_map_is_supported" $ passertEvalNoTrace emptyValueSupported
  , testCase "maximum_cardano_value_terminal_fold_matches_typescript" $ passertEvalNoTrace maximumTerminalFold
  , testCase "maximum_cardano_value_policy_transition_matches_typescript" $ passertEvalNoTrace maximumPolicyTransition
  ]

foldsExactValue :: forall s. Term s PBool
foldsExactValue =
  plet assetPeaks $ \peaks ->
  plet (pinitialControlV1 # 3) $ \c0 ->
  plet (expectJust $ pstepV1 # c0 # 3 # peaks # 8_000_000 # assetWitness 34 (bytes "bbcc") pmaxUint64 pnil) $ \c1 ->
  plet (expectJust $ pstepV1 # c1 # 3 # peaks # 8_000_000 # assetWitness 17 (bytes "aa") 42 (plist [pdata firstLeaf])) $ \c2 ->
  plet (expectJust $ pstepV1 # c2 # 3 # peaks # 8_000_000 # assetWitness 17 (pconstant "") 1 (plist [pdata secondLeaf])) $ \c3 ->
  plet (expectJust $ pstepV1 # c3 # 3 # peaks # 8_000_000 # pcon PLedgerOutputValueNoWitness) $ \c4 ->
  plet (expectJust $ pstepV1 # c4 # 3 # peaks # 8_000_000 # pcon PLedgerOutputValueNoWitness) $ \terminal ->
    pfinalizeV1 # terminal #== pcon (PJust $ psemanticDataSummaryV1 # pconstant expectedValueData)
      #&& pencodeControlV1 # terminal #== terminalControl

terminalControlDecodes :: forall s. Term s PBool
terminalControlDecodes = plet (pdecodeControlV1 # terminalControl) $ \control -> pmatch control $ \c ->
  pfromData (pvalueControl'stage c) #== pstageTerminal
    #&& pencodeControlV1 # control #== terminalControl

maximumTerminalAgrees :: forall s. Term s PBool
maximumTerminalAgrees =
  plet (pdecodeControlV1 # maximumPreTerminalControl) $ \pre ->
  plet (expectJust $ pstepV1 # pre # 1_592 # maximumPeaks # 30_000_000 # pcon PLedgerOutputValueNoWitness) $ \terminal ->
  plet (pdecodeControlV1 # maximumTerminalControl) $ \decoded ->
  plet (expectJust $ pfinalizeV1 # terminal) $ \result ->
  pmatch pre $ \preFields -> pmatch result $ \summary ->
    pfrontierIsWellFormed # 1_592 # maximumPeaks
      #&& pfromData (pvalueControl'stage preFields) #== pstageFinalize
      #&& pfromData (pvalueControl'assetRemaining preFields) #== 0
      #&& pfromData (pvalueControl'currentPolicy preFields) #== policy 17
      #&& terminal #== decoded
      #&& pencodeControlV1 # terminal #== maximumTerminalControl
      #&& pfromData (psummary'root summary) #== maximumRoot
      #&& pfromData (psummary'cborLength summary) #== 5_002
      #&& pfromData (psummary'memory summary) #== 16_198
      #&& pfinalizeV1 # pre #== pcon PNothing
      #&& pnot # (pstepV1 # pre # 1_592 # maximumPeaks # 30_000_001 # pcon PLedgerOutputValueNoWitness #== pcon (PJust decoded))

substitutedFactsFail :: forall s. Term s PBool
substitutedFactsFail =
  pstepV1 # (pinitialControlV1 # 3) # 3 # assetPeaks # 8_000_000
    # assetWitness 34 (bytes "bbcc") (pmaxUint64 - 1) pnil
    #== pcon PNothing

emptyValueSupported :: forall s. Term s PBool
emptyValueSupported =
  plet (pinitialControlV1 # 0) $ \c0 ->
  plet (expectJust $ pstepV1 # c0 # 0 # pnil # 0 # pcon PLedgerOutputValueNoWitness) $ \c1 ->
  plet (expectJust $ pstepV1 # c1 # 0 # pnil # 0 # pcon PLedgerOutputValueNoWitness) $ \terminal ->
    pfinalizeV1 # terminal #== pcon (PJust $ psemanticDataSummaryV1 # pconstant (Map []))

maximumTerminalFold :: forall s. Term s PBool
maximumTerminalFold =
  plet (pdecodeControlV1 # maximumPreTerminalControl) $ \pre ->
  plet (expectJust $ pstepV1 # pre # 1_592 # maximumPeaks # 30_000_000 # pcon PLedgerOutputValueNoWitness) $ \post ->
  pmatch pre $ \preFields -> pmatch post $ \postFields ->
    pfromData (pvalueControl'stage preFields) #== pstageFinalize
      #&& pfromData (pvalueControl'assetRemaining preFields) #== 0
      #&& pfromData (pvalueControl'stage postFields) #== pstageTerminal
      #&& pencodeControlV1 # post #== maximumTerminalControl
      #&& pnot # (pfinalizeV1 # post #== pcon PNothing)

maximumPolicyTransition :: forall s. Term s PBool
maximumPolicyTransition =
  plet (pdecodeControlV1 # policyTransitionPreControl) $ \pre ->
  plet (expectJust $ pstepV1 # pre # 1_592 # maximumPeaks # 30_000_000 # maximumPolicyWitness 1) $ \post ->
  pmatch pre $ \preFields -> pmatch post $ \postFields ->
    pfromData (pvalueControl'assetRemaining preFields) #== 228
      #&& pfromData (pvalueControl'assetRemaining postFields) #== 227
      #&& pfromData (pvalueControl'currentPolicy postFields) #== policy 17
      #&& pencodeControlV1 # post #== policyTransitionPostControl
      #&& pstepV1 # pre # 1_592 # maximumPeaks # 30_000_000 # maximumPolicyWitness 2 #== pcon PNothing

assetWitness :: forall s. Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s (PBuiltinList (PAsData PByteString)) -> Term s PLedgerOutputValueWitnessV1
assetWitness policyByte assetName quantity siblings = pcon $ PLedgerOutputValueAsset
  (pdata $ policy policyByte) (pdata assetName) (pdata quantity) (pdata siblings)

maximumPolicyWitness :: forall s. Term s PInteger -> Term s PLedgerOutputValueWitnessV1
maximumPolicyWitness quantity = assetWitness 17 (bytes "e2") quantity $ plist $ map (pdata . bytes)
  [ "c8ff3191003d91361e84195924aba18210e02790385d36debdcbd8f70bbe30ce"
  , "e57595be0911f47b6ad9a4fc577e4dd80633ab1708ea007f1e10f2c046023271"
  , "9bab8e0b298f6ba239d749199354e3b5077454fdc4c46e9be6ee181b57620a36"
  , "ad95bca1382117e5c4150798dd59d0e50cb61f0ce24ae2cead5bb278d129863b"
  , "b741a1bc87ac41225567abf47022d28aff8bef9010b3f555dd95cc37606f19cc"
  , "4326fd84d7f7c1cb1c22a7d9be5cd7dd1e4e0cac784c895fe34a22252757e90f"
  , "e3aac35704e9bbab8073bd8db99057348328982a5d454a7af9be0891419510ef"
  , "16233275d44a0986f77f8f15153b441404e7248a0c844166968868f7d7bd996e"
  , "9f1efec8e1306f877d11aa735c34e3ff4491ee38d5b428bc5d3b03c0bd33b490"
  , "4ebf5ad0166b0ae660e33b8b4037a666ea07a55c72ffd3ec3b51e9cff53bdd1c"
  ]

assetPeaks :: forall s. Term s (PBuiltinList (PAsData PFrontierPeak))
assetPeaks = pappendLeaf # 2 # (pappendLeaf # 1 # (pappendLeaf # 0 # pnil # firstLeaf) # secondLeaf) # thirdLeaf

firstLeaf, secondLeaf, thirdLeaf :: forall s. Term s PByteString
firstLeaf = passetLeafHash # policy 17 # pconstant "" # 1
secondLeaf = passetLeafHash # policy 17 # bytes "aa" # 42
thirdLeaf = passetLeafHash # policy 34 # bytes "bbcc" # pmaxUint64

maximumPeaks :: forall s. Term s (PBuiltinList (PAsData PFrontierPeak))
maximumPeaks = plist $ zipWith peak [3, 4, 5, 9, 10] $ map bytes
  [ "d7186f5f4ba03f35771f5fe9a2bb1d98c1b6647fdb307a3a990674bdef9f44eb"
  , "3b64e0dee5d4fb8dea89b2e43cec29e9d042e70855235cdf5ee5dfba032370c8"
  , "99005b0619518750da865e276edd6ded5162e42e975b84ca5a0b39e2106d0c04"
  , "e140a7bcc4ae85c668a78b45e94afac7712d80519d9d4e1ad42c1e09afd56c71"
  , "d0f89d5bf0b4028db4ba39470ca2ff296bc5733936ddfef6beb77278f1003833"
  ]
  where peak height hashValue = pdata $ pcon $ PFrontierPeak (pdata height) (pdata hashValue)

expectedValueData :: Data
expectedValueData = Map
  [ (B "", Map [(B "", I 8_000_000)])
  , (B $ BS.replicate 28 17, Map [(B "", I 1), (B $ Base16.decodeLenient "aa", I 42)])
  , (B $ BS.replicate 28 34, Map [(B $ Base16.decodeLenient "bbcc", I 18_446_744_073_709_551_615)])
  ]

policy :: forall s. Term s PInteger -> Term s PByteString
policy byte = preplicateBS # 28 # (pintegerToByte # byte)

pmaxUint64 :: forall s. Term s PInteger
pmaxUint64 = 18_446_744_073_709_551_615

expectJust :: forall s a. Term s (PMaybe a) -> Term s a
expectJust value = pmatch value $ \case PNothing -> perror; PJust result -> result

plist :: forall s a. PIsListLike PBuiltinList a => [Term s a] -> Term s (PBuiltinList a)
plist = foldr (\item rest -> pcons # item # rest) pnil

terminalControl, maximumPreTerminalControl, maximumTerminalControl, maximumRoot, policyTransitionPreControl, policyTransitionPostControl :: forall s. Term s PByteString
terminalControl = bytes "8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f6d68f04ebaaf198c28e605965e60a233a8a46428e2284e19f344f6290e7464d18591888ff"
maximumPreTerminalControl = bytes "87010100581c1111111111111111111111111111111111111111111111111111111184582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80"
maximumTerminalControl = bytes "8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f83582035df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb419138a193f46ff"
maximumRoot = bytes "35df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb4"
policyTransitionPreControl = bytes "87010018e4581c1212121212121212121212121212121212121212121212121212121284582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820e6b1158ed70eadba4dd3edea999fbc8ce3f345de8a1c442c11d25f67245209b905190deb192d14d87a80"
policyTransitionPostControl = bytes "87010018e3581c11111111111111111111111111111111111111111111111111111111845820f3a575175904810deba1e2e614bb689c08dfd8797bfb1913783c8dd98ed55af501030a845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient

{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.EventV1Abi
Description : Common event-record ABI tests ported from
              @lib/midgard/event-v1-abi.test.ak@.

Every user event is a constructor-zero record whose first two fields are its
key and value. Shared minting and settlement code intentionally extracts those
fields without knowing the concrete event type, so this small common ABI is
load-bearing across deposits, withdrawals, and transaction orders.
-}
module Testing.EventV1Abi (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Midgard.LedgerState (
  PDepositEvent (..),
  PTxOrderEventV1 (..),
  PWithdrawalEvent (..),
  punsafeEventToIdData,
  punsafeEventToKeyValuePair,
 )
import Midgard.OperatorDirectory qualified as Directory
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RegisteredOperators qualified as Registered
import Midgard.OperatorDirectory.RetiredOperators qualified as Retired
import Midgard.Payout qualified as Payout
import Midgard.Reserve qualified as Reserve
import Midgard.Settlement qualified as Settlement
import Midgard.UserEvents qualified as UserEvents
import Midgard.UserEvents.Witness qualified as Witness
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Event V1 ABI"
    [ testCase "DepositEvent is constructor zero with id then info" $
        passertEval $ eventEncoding depositEvent
    , testCase "WithdrawalEvent is constructor zero with id then info" $
        passertEval $ eventEncoding withdrawalEvent
    , testCase "TxOrderEventV1 is constructor zero with id then payload" $
        passertEval $ eventEncoding txOrderEvent
    , testCase "unsafe_event_to_id_data returns the first field and ignores the rest" $
        passertEval $ punsafeEventToIdData rawEvent #== pconstant eventId
    , testCase "unsafe_event_to_key_value_pair returns the first two fields and ignores the rest" $
        passertEval keyValueExtraction
    , testCase "unsafe_event_to_id_data rejects an event with no fields" $
        pfails $ punsafeEventToIdData (pconstant (PD.Constr 0 [])) #== pconstant eventId
    , testCase "unsafe_event_to_key_value_pair rejects an event with no value field" $
        pfails oneFieldKeyValueExtraction
    , testCase "canonical event, reserve, and payout V1 constructor ABI is exact" $
        passertEval eventReservePayoutAbi
    , testCase "canonical settlement V1 constructor ABI is exact" $
        passertEval settlementAbi
    , testCase "canonical operator-directory V1 constructor ABI is exact" $
        passertEval operatorDirectoryAbi
    , testCase "rejects adjacent user-event mint constructor tag" $
        pfails malformedUserEventMintTag
    , testCase "rejects adjacent user-event witness constructor tag" $
        pfails malformedWitnessTag
    , testCase "rejects wrong deposit datum arity" $
        pfails malformedDepositDatumArity
    , testCase "rejects decorative reserve version field" $
        pfails malformedReserveArity
    , testCase "rejects adjacent active-operator mint constructor tag" $
        pfails malformedActiveMintTag
    , testCase "rejects malformed active-operator link" $
        pfails malformedActiveLink
    , testCase "rejects arbitrary retired-operator slashing payload" $
        pfails malformedRetiredSlashPayload
    , testCase "rejects adjacent settlement event constructor tag" $
        pfails malformedSettlementEventTag
    , testCase "deposit and transaction datum decoders stay distinct" $
        passertEval depositAndTransactionDatumDecodersStayDistinct
    , testCase "accepts a well-formed deposit datum" $
        passertEval wellFormedDepositDatum
    , testCase "rejects a deposit datum with an inline-datum tag" $
        pfails inlineDatumAsDepositDatum
    ]

eventId, eventInfo, trailingField, expectedEvent :: PD.Data
eventId = PD.Constr 0 [PD.B ("event-id" :: BS.ByteString), PD.I 1]
eventInfo = PD.Constr 0 [PD.B ("event-info" :: BS.ByteString)]
trailingField = PD.I 99
expectedEvent = PD.Constr 0 [eventId, eventInfo]

rawEvent :: forall s. Term s PData
rawEvent = pconstant (PD.Constr 7 [eventId, eventInfo, trailingField])

asData :: forall a s. PD.Data -> Term s (PAsData a)
asData = punsafeCoerce . pconstant @PData

asTerm :: forall a s. PD.Data -> Term s a
asTerm = punsafeCoerce . pconstant @PData

field :: Integer -> PD.Data
field = PD.I

fields :: Int -> [PD.Data]
fields count = map (field . fromIntegral) [1 .. count]

encodes :: forall a s. PIsData a => Term s a -> Integer -> [PD.Data] -> Term s PBool
encodes value tag values =
  pforgetData (pdata value) #== pconstant (PD.Constr tag values)

depositEvent :: forall s. Term s PDepositEvent
depositEvent =
  pcon $
    PDepositEvent
      { pdepositEvent'id = asData eventId
      , pdepositEvent'info = asData eventInfo
      }

withdrawalEvent :: forall s. Term s PWithdrawalEvent
withdrawalEvent =
  pcon $
    PWithdrawalEvent
      { pwithdrawalEvent'id = asData eventId
      , pwithdrawalEvent'info = asData eventInfo
      }

txOrderEvent :: forall s. Term s PTxOrderEventV1
txOrderEvent =
  pcon $
    PTxOrderEventV1
      { ptxOrderEvent'id = asData eventId
      , ptxOrderEvent'tx = asData eventInfo
      }

eventEncoding :: forall a s. PIsData a => Term s a -> Term s PBool
eventEncoding event = pforgetData (pdata event) #== pconstant expectedEvent

keyValueExtraction :: forall s. Term s PBool
keyValueExtraction =
  let (key, value) = punsafeEventToKeyValuePair rawEvent
   in key #== pconstant eventId #&& value #== pconstant eventInfo

oneFieldKeyValueExtraction :: forall s. Term s PBool
oneFieldKeyValueExtraction =
  let (key, value) =
        punsafeEventToKeyValuePair (pconstant (PD.Constr 0 [eventId]))
   in key #== pconstant eventId #&& value #== pconstant eventInfo

--------------------------------------------------------------------------------
-- Canonical constructor ABI
--------------------------------------------------------------------------------

eventReservePayoutAbi :: forall s. Term s PBool
eventReservePayoutAbi =
  pand'List
    [ encodes (pcon $ UserEvents.PAuthenticateEvent (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4)) 0 (fields 4)
    , encodes (pcon $ UserEvents.PBurnEventNFT (asData $ field 1) (asData $ field 2)) 1 (fields 2)
    , encodes (pcon $ Witness.PMintOrBurn (asData $ field 1)) 0 (fields 1)
    , encodes (pcon $ Witness.PRegisterToProveNotRegistered (asData $ field 1)) 1 (fields 1)
    , encodes (pcon $ Witness.PUnregisterToProveNotRegistered (asData $ field 1)) 2 (fields 1)
    , encodes (pcon $ Reserve.PSpend (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4)) 0 (fields 4)
    , encodes (pcon $ Payout.PAddFunds (asData $ field 1) (asData $ field 2) (asData $ field 3) (asTerm $ field 4) (asData $ field 5) (asData $ field 6) (asData $ field 7)) 0 (fields 7)
    , encodes (pcon $ Payout.PConcludeWithdrawal (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4)) 1 (fields 4)
    , encodes (pcon $ Payout.PMintPayout (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4)) 0 (fields 4)
    , encodes (pcon $ Payout.PBurnPayout (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4)) 1 (fields 4)
    , encodes (pcon $ Payout.PPayoutDatum (asData $ field 1) (asData $ field 2) (asTerm $ field 3)) 0 (fields 3)
    ]

settlementAbi :: forall s. Term s PBool
settlementAbi =
  pand'List
    [ encodes (pcon $ Settlement.PResolutionClaim (asData $ field 1) (asData $ field 2)) 0 (fields 2)
    , encodes (pcon $ Settlement.PSettlementDatum (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asTerm $ field 5)) 0 (fields 5)
    , encodes (pcon $ Settlement.PAttachResolutionClaim (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6) (asData $ field 7)) 0 (fields 7)
    , encodes (pcon $ Settlement.PDisproveResolutionClaim (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6) (asData $ field 7) (asData $ field 8) (asData $ field 9) (asData $ field 10) (asData $ field 11)) 1 (fields 11)
    , encodes (pcon $ Settlement.PResolve (asData $ field 1)) 2 (fields 1)
    , encodes (pcon $ Settlement.PSpawn (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4)) 0 (fields 4)
    , encodes (pcon $ Settlement.PRemove (asData $ field 1) (asData $ field 2) (asData $ field 3)) 1 (fields 3)
    , encodes (pcon Settlement.PDeposit) 0 []
    , encodes (pcon $ Settlement.PWithdrawal (asData $ field 1)) 1 (fields 1)
    , encodes (pcon $ Settlement.PTxOrder (asData $ field 1)) 2 (fields 1)
    , encodes (pcon $ Settlement.PDepositMembership (asData $ field 1)) 0 (fields 1)
    , encodes (pcon $ Settlement.PWithdrawalMembership (asData $ field 1)) 1 (fields 1)
    , encodes (pcon $ Settlement.PTxOrderMembership (asData $ field 1)) 2 (fields 1)
    ]

operatorDirectoryAbi :: forall s. Term s PBool
operatorDirectoryAbi =
  pand'List
    [ encodes (pcon $ Directory.PSlashOperatorForBadState (asData $ field 1)) 0 (fields 1)
    , encodes (pcon $ Directory.PSlashOperatorForBadSettlement (asData $ field 1) (asData $ field 2)) 1 (fields 2)
    , encodes (pcon $ Directory.PSlashingArguments (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5)) 0 (fields 5)
    , encodes (pcon $ Active.PNodeData (asTerm $ field 1) (asData $ field 2)) 0 (fields 2)
    , encodes (pcon $ Active.PShowOperatorIsInactive (asData $ field 1)) 0 (fields 1)
    , encodes (pcon $ Active.PShowSchedulerIsAdvancing (asData $ field 1) (asData $ field 2) (asTerm $ field 3) (asData $ field 4)) 1 (fields 4)
    , encodes (pcon Active.PListStateTransition) 0 []
    , encodes (pcon $ Active.PUpdateBondHoldNewState (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5)) 1 (fields 5)
    , encodes (pcon $ Active.PUpdateBondHoldNewSettlement (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6) (asData $ field 7)) 2 (fields 7)
    , encodes (pcon $ Active.PStrikeForInactivity (asData $ field 1) (asData $ field 2) (asData $ field 3) (asTerm $ field 4) (asData $ field 5) (asData $ field 6) (asData $ field 7)) 3 (fields 7)
    , encodes (pcon $ Active.PInit (asData $ field 1)) 0 (fields 1)
    , encodes (pcon Active.PDeinit) 1 []
    , encodes (pcon $ Active.PActivateOperator (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5)) 2 (fields 5)
    , encodes (pcon $ Active.PRetireOperator (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6) (asData $ field 7)) 3 (fields 7)
    , encodes (pcon $ Active.PSlashOperator (asData $ field 1) (asData $ field 2)) 4 (fields 2)
    , encodes (pcon $ Registered.PNodeData (asData $ field 1)) 0 (fields 1)
    , encodes (pcon Registered.PDuplicateIsRegistered) 0 []
    , encodes (pcon $ Registered.PDuplicateIsActive (asData $ field 1)) 1 (fields 1)
    , encodes (pcon Registered.PDuplicateIsRetired) 2 []
    , encodes (pcon $ Registered.PInit (asData $ field 1)) 0 (fields 1)
    , encodes (pcon Registered.PDeinit) 1 []
    , encodes (pcon $ Registered.PRegisterOperator (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6)) 2 (fields 6)
    , encodes (pcon $ Registered.PActivateOperator (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6)) 3 (fields 6)
    , encodes (pcon $ Registered.PDeregisterOperator (asData $ field 1) (asData $ field 2) (asData $ field 3)) 4 (fields 3)
    , encodes (pcon $ Registered.PSlashDuplicateOperator (asData $ field 1) (asData $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5)) 5 (fields 5)
    , encodes (pcon $ Retired.PNodeData (asTerm $ field 1)) 0 (fields 1)
    , encodes (pcon $ Retired.PInit (asData $ field 1)) 0 (fields 1)
    , encodes (pcon Retired.PDeinit) 1 []
    , encodes (pcon $ Retired.PRetireOperator (asData $ field 1) (asTerm $ field 2) (asData $ field 3) (asData $ field 4) (asData $ field 5) (asData $ field 6)) 2 (fields 6)
    , encodes (pcon $ Retired.PRecoverOperatorBond (asData $ field 1) (asData $ field 2) (asData $ field 3)) 3 (fields 3)
    , encodes (pcon $ Retired.PSlashOperator (asData $ field 1)) 4 (fields 1)
    ]

--------------------------------------------------------------------------------
-- Malformed constructor rejection
--------------------------------------------------------------------------------

isTagArity :: forall s. Term s PData -> Integer -> Integer -> Term s PBool
isTagArity source expectedTag expectedArity =
  pmatch (pasConstr # source) $ \(PBuiltinPair tag constructorFields) ->
    tag #== pconstant expectedTag
      #&& plength # constructorFields #== pconstant expectedArity

expectTagArities :: forall s. Term s PData -> [(Integer, Integer)] -> Term s PBool
expectTagArities source expected =
  pif
    (foldr (#||) (pconstant False) [isTagArity source tag arity | (tag, arity) <- expected])
    (pconstant True)
    perror

malformedUserEventMintTag :: forall s. Term s PBool
malformedUserEventMintTag =
  expectTagArities (pconstant $ PD.Constr 2 []) [(0, 4), (1, 2)]

malformedWitnessTag :: forall s. Term s PBool
malformedWitnessTag =
  expectTagArities (pconstant $ PD.Constr 3 []) [(0, 1), (1, 1), (2, 1)]

malformedDepositDatumArity :: forall s. Term s PBool
malformedDepositDatumArity =
  expectTagArities (pconstant $ PD.Constr 0 []) [(0, 3)]

malformedReserveArity :: forall s. Term s PBool
malformedReserveArity =
  expectTagArities (pconstant $ PD.Constr 0 (fields 5)) [(0, 4)]

malformedActiveMintTag :: forall s. Term s PBool
malformedActiveMintTag =
  expectTagArities
    (pconstant $ PD.Constr 5 [])
    [(0, 1), (1, 0), (2, 5), (3, 7), (4, 2)]

malformedActiveLink :: forall s. Term s PBool
malformedActiveLink =
  let source =
        pconstant $
          PD.Constr
            3
            [field 1, field 2, PD.B (BS.replicate 28 0x11), field 4, field 5, field 6, field 7]
   in pif
        (isTagArity source 3 7)
        ( pmatch (pasConstr # (pelemAt # 3 #$ psndBuiltin # (pasConstr # source))) $
            \(PBuiltinPair tag linkFields) ->
              pif
                (tag #== 0 #&& plength # linkFields #== 1)
                (plengthBS # (pasByteStr # (phead # linkFields)) #>= 0)
                (pif (tag #== 1 #&& pnull # linkFields) (pconstant True) perror)
        )
        perror

malformedRetiredSlashPayload :: forall s. Term s PBool
malformedRetiredSlashPayload =
  let source = pconstant $ PD.Constr 4 [field 1]
   in pif
        (isTagArity source 4 1)
        (expectTagArities (phead #$ psndBuiltin # (pasConstr # source)) [(0, 5)])
        perror

malformedSettlementEventTag :: forall s. Term s PBool
malformedSettlementEventTag =
  expectTagArities (pconstant $ PD.Constr 3 []) [(0, 0), (1, 1), (2, 1)]

depositAndTransactionDatumDecodersStayDistinct :: forall s. Term s PBool
depositAndTransactionDatumDecodersStayDistinct =
  withDecoded depositDatumCbor $ \depositData ->
    withDecoded noDatumCbor $ \noDatumData ->
      withDecoded inlineDatumCbor $ \inlineDatumData ->
        pand'List
          [ isTagArity depositData 0 3
          , isTagArity noDatumData 0 0
          , isTagArity inlineDatumData 2 1
          , pnot # (isTagArity noDatumData 0 3)
          , pnot # (isTagArity inlineDatumData 0 3)
          , pserialiseData # depositData #== pconstant depositDatumCbor
          , pserialiseData # noDatumData #== pconstant noDatumCbor
          , pserialiseData # inlineDatumData #== pconstant inlineDatumCbor
          ]

wellFormedDepositDatum :: forall s. Term s PBool
wellFormedDepositDatum =
  withDecoded depositDatumCbor $ \depositData ->
    pif
      (isTagArity depositData 0 3)
      (pserialiseData # depositData #== pconstant depositDatumCbor)
      perror

inlineDatumAsDepositDatum :: forall s. Term s PBool
inlineDatumAsDepositDatum =
  withDecoded inlineDatumCbor $ \inlineDatumData ->
    pif
      (isTagArity inlineDatumData 0 3)
      (pserialiseData # inlineDatumData #== pconstant inlineDatumCbor)
      perror

withDecoded :: forall s. BS.ByteString -> (Term s PData -> Term s PBool) -> Term s PBool
withDecoded source continuation =
  pmatch (pdeserialise # pconstant source) $ \case
    PNothing -> perror
    PJust value -> continuation value

depositDatumCbor, noDatumCbor, inlineDatumCbor :: BS.ByteString
depositDatumCbor = Base16.decodeLenient $
  "d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ff"
    <> "d8799fd8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ff00d87a80ffff"
    <> "02581c22222222222222222222222222222222222222222222222222222222ff"
noDatumCbor = Base16.decodeLenient "d87980"
inlineDatumCbor = Base16.decodeLenient "d87b9f182aff"

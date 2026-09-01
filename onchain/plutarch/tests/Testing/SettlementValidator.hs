{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.SettlementValidator
Description : Tests for the Plutarch port of @validators/settlement.ak@.

The first group carries the names and encodings of the five Aiken @test@ blocks
covering @decode_mint_redeemer@.

The handler groups mirror the newer Aiken
@validators/settlement-handlers.test.ak@ suite. Their names are kept verbatim so
the same production guard can be selected on both sides.
-}
module Testing.SettlementValidator (tests) where

import Numeric (showHex)

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript, SpendingScript),
  Credential (ScriptCredential),
  ScriptPurpose (Minting, Rewarding, Spending),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  scriptContextTxInfo,
  toBuiltinData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
  txInfoSignatories,
  txInfoValidRange,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusTx.Builtins (
  BuiltinData,
  blake2b_256,
  builtinDataToData,
  dataToBuiltinData,
  fromBuiltin,
  serialiseData,
  toBuiltin,
 )
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.Settlement (PMintRedeemer (..), pdecodeMintRedeemer)
import Midgard.Validators.Settlement (settlementMintValidator, settlementSpendValidator)
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Settlement Validator Tests"
    [ testGroup
        "decodeMintRedeemer (mirrors the Aiken test blocks)"
        [ testCase "settlement_mint_redeemer_parser_preserves_spawn_field_order" $
            passertEval $
              pmatch
                (pdecodeMintRedeemer # pconstant @PData (PD.Constr 0 [PD.B "aa", PD.I 1, PD.I 2, PD.I 3]))
                $ \case
                  PSpawn a b c d ->
                    pand'List
                      [ pto (pfromData a) #== pconstant ("aa" :: BS.ByteString)
                      , pfromData b #== 1
                      , pfromData c #== 2
                      , pfromData d #== 3
                      ]
                  _ -> pconstant False
        , testCase "settlement_mint_redeemer_parser_preserves_remove_field_order" $
            passertEval $
              pmatch
                (pdecodeMintRedeemer # pconstant @PData (PD.Constr 1 [PD.B "bb", PD.I 4, PD.I 5]))
                $ \case
                  PRemove a b c ->
                    pand'List
                      [ pto (pfromData a) #== pconstant ("bb" :: BS.ByteString)
                      , pfromData b #== 4
                      , pfromData c #== 5
                      ]
                  _ -> pconstant False
        , testCase "settlement_mint_redeemer_parser_rejects_unknown_tag" $
            pfails $ decodeTotal (PD.Constr 2 [])
        , testCase "settlement_mint_redeemer_parser_rejects_spawn_wrong_arity" $
            pfails $ decodeTotal (PD.Constr 0 [PD.B "aa", PD.I 1, PD.I 2])
        , testCase "settlement_mint_redeemer_parser_rejects_remove_wrong_type" $
            pfails $ decodeTotal (PD.Constr 1 [PD.B "bb", PD.B "04", PD.I 5])
        ]
    , testGroup
        "mint / Spawn"
        [ testCase "settlement_handler_mint_spawn_accepts_honest_merge" $
            psucceeds $ runSpawn spawnDefaults
        , testCase "settlement_handler_mint_spawn_rejects_unauthentic_hub_oracle_reference_input" $
            pfails $ runSpawn spawnDefaults {spAuthenticHub = False}
        , testCase "settlement_handler_mint_spawn_rejects_merge_redeemer_at_a_foreign_policy" $
            pfails $ runSpawn spawnDefaults {spMergeAtStateQueuePolicy = False}
        , testCase "settlement_handler_mint_spawn_rejects_non_merge_state_queue_redeemer" $
            pfails $ runSpawn spawnDefaults {spMergeTag = 2}
        , testCase "settlement_handler_mint_spawn_rejects_merge_redeemer_of_the_wrong_arity" $
            pfails $ runSpawn spawnDefaults {spMergeArity = 17}
        , testCase "settlement_handler_mint_spawn_rejects_settlement_id_that_is_not_the_merged_header_hash" $
            pfails $ runSpawn spawnDefaults {spHeaderKey = Just "zz"}
        , -- Absence of the optional index would let an empty block through.
          testCase "settlement_handler_mint_spawn_rejects_merge_without_a_settlement_redeemer_index" $
            pfails $ runSpawn spawnDefaults {spSettlementIndexPresent = False}
        , testCase "settlement_handler_mint_spawn_rejects_output_at_a_foreign_address" $
            pfails $ runSpawn spawnDefaults {spOutputAtSettlementAddress = False}
        , testCase "settlement_handler_mint_spawn_rejects_output_without_the_settlement_nft" $
            pfails $ runSpawn spawnDefaults {spOutputHasSettlementNft = False}
        , testCase "settlement_handler_mint_spawn_rejects_output_carrying_a_reference_script" $
            pfails $ runSpawn spawnDefaults {spOutputHasReferenceScript = True}
        , -- The roots come from the state queue's redeemer, so the settlement
          -- cannot claim roots the merge did not commit to.
          testCase "settlement_handler_mint_spawn_rejects_deposits_root_not_from_the_merged_block" $
            pfails $ runSpawn spawnDefaults {spDepositsRoot = Just (root 0xee)}
        , testCase "settlement_handler_mint_spawn_rejects_produced_datum_with_a_resolution_claim" $
            pfails $ runSpawn spawnDefaults {spWithClaim = True}
        , testCase "settlement_handler_mint_spawn_rejects_a_second_token_under_the_settlement_policy" $
            pfails $ runSpawn spawnDefaults {spExtraMint = True}
        , testCase "settlement_handler_mint_spawn_rejects_a_mint_quantity_other_than_one" $
            pfails $ runSpawn spawnDefaults {spMintQuantity = 2}
        ]
    , testGroup
        "mint / Remove"
        [ testCase "settlement_handler_mint_remove_accepts_resolved_settlement" $
            psucceeds $ runRemove removeDefaults
        , testCase "settlement_handler_mint_remove_rejects_input_without_the_settlement_nft" $
            pfails $ runRemove removeDefaults {rmInputHasSettlementNft = False}
        , testCase "settlement_handler_mint_remove_rejects_spend_redeemer_that_is_not_resolve" $
            pfails $ runRemove removeDefaults {rmResolveRedeemer = False}
        , testCase "settlement_handler_mint_remove_rejects_spend_redeemer_at_a_foreign_purpose" $
            pfails $ runRemove removeDefaults {rmSpendPurposeMatches = False}
        , testCase "settlement_handler_mint_remove_rejects_settlement_id_mismatch_across_redeemers" $
            pfails $ runRemove removeDefaults {rmSpendId = Just "zz"}
        , testCase "settlement_handler_mint_remove_rejects_settlement_without_a_resolution_claim" $
            pfails $ runRemove removeDefaults {rmHasClaim = False}
        , testCase "settlement_handler_mint_remove_rejects_missing_operator_signature" $
            pfails $ runRemove removeDefaults {rmSigned = False}
        , testCase "settlement_handler_mint_remove_rejects_lower_bound_before_the_resolution_time" $
            pfails $ runRemove removeDefaults {rmValidFrom = 500}
        , -- Additional identity check beyond the Aiken named matrix.
          testCase "rejects removal signed by somebody else" $
            pfails $ runRemove removeDefaults {rmSigner = Just "zz"}
        ]
    , testGroup
        "spend / AttachResolutionClaim"
        [ testCase "settlement_handler_spend_attach_accepts_honest_claim" $
            psucceeds $ runAttach attachDefaults
        , testCase "settlement_handler_spend_rejects_missing_datum" $
            pfails $ runAttach attachDefaults {atOwnDatumPresent = False}
        , testCase "settlement_handler_spend_attach_rejects_preexisting_resolution_claim" $
            pfails $ runAttach attachDefaults {atExistingClaim = True}
        , testCase "settlement_handler_spend_attach_rejects_missing_operator_signature" $
            pfails $ runAttach attachDefaults {atSigned = False}
        , testCase "settlement_handler_spend_attach_rejects_input_index_not_the_spent_utxo" $
            pfails $ runAttach attachDefaults {atOwnOutRefMatchesInput = False}
        , testCase "settlement_handler_spend_attach_rejects_output_at_a_foreign_address" $
            pfails $ runAttach attachDefaults {atOutputAtSettlementAddress = False}
        , testCase "settlement_handler_spend_attach_rejects_output_without_the_settlement_nft" $
            pfails $ runAttach attachDefaults {atOutputHasSettlementNft = False}
        , testCase "settlement_handler_spend_attach_rejects_output_carrying_a_reference_script" $
            pfails $ runAttach attachDefaults {atOutputHasReferenceScript = True}
        , testCase "settlement_handler_spend_attach_rejects_unauthentic_hub_oracle_reference_input" $
            pfails $ runAttach attachDefaults {atAuthenticHub = False}
        , testCase "settlement_handler_spend_attach_rejects_wrong_active_operators_redeemer_constructor" $
            pfails $ runAttach attachDefaults {atActiveRedeemerIsBondHold = False}
        , testCase "settlement_handler_spend_attach_rejects_active_operators_input_at_a_foreign_address" $
            pfails $ runAttach attachDefaults {atActiveInputAtExpectedAddress = False}
        , testCase "settlement_handler_spend_attach_rejects_operator_mismatch_with_active_operators_redeemer" $
            pfails $ runAttach attachDefaults {atActiveOperator = Just "zz"}
        , -- The resolution time comes from the bond hold, not from here.
          testCase "settlement_handler_spend_attach_rejects_resolution_time_not_from_active_operators_redeemer" $
            pfails $ runAttach attachDefaults {atOutputTime = Just 9_999}
        , testCase "settlement_handler_spend_attach_rejects_mutated_deposits_root_in_continuing_datum" $
            pfails $ runAttach attachDefaults {atOutputDepositsRoot = Just (root 0xee)}
        , testCase "settlement_handler_spend_attach_rejects_scheduler_without_an_active_operator" $
            pfails $ runAttach attachDefaults {atSchedulerIsActive = False}
        , -- The scheduler must currently appoint the claiming operator.
          testCase "settlement_handler_spend_attach_rejects_operator_that_is_not_the_current_scheduler_operator" $
            pfails $ runAttach attachDefaults {atSchedulerOperator = Just "zz"}
        ]
    , testGroup
        "spend / Resolve"
        [ testCase "settlement_handler_spend_resolve_accepts_exact_burn" $
            psucceeds $ runResolve resolveDefaults
        , testCase "settlement_handler_spend_resolve_rejects_absent_burn" $
            pfails $ runResolve resolveDefaults {rsBurnQuantity = 0}
        , testCase "settlement_handler_spend_resolve_rejects_burn_of_a_different_settlement_id" $
            pfails $ runResolve resolveDefaults {rsBurnId = Just "other"}
        , testCase "settlement_handler_spend_resolve_rejects_burn_of_the_wrong_quantity" $
            pfails $ runResolve resolveDefaults {rsBurnQuantity = -2}
        ]
    , testGroup
        "spend / DisproveResolutionClaim"
        [ testCase "settlement_handler_spend_disprove_accepts_honest_active_operator_disproof" $
            psucceeds $ runDisprove disproveDefaults
        , testCase "settlement_handler_spend_disprove_accepts_honest_retired_operator_disproof" $
            psucceeds $ runDisprove disproveDefaults {dsOperatorIsActive = False}
        , testCase "settlement_handler_spend_disprove_rejects_settlement_without_a_resolution_claim" $
            pfails $ runDisprove disproveDefaults {dsHasClaim = False}
        , testCase "settlement_handler_spend_disprove_rejects_operator_mismatch_with_the_claim" $
            pfails $ runDisprove disproveDefaults {dsClaimOperator = Just "zz"}
        , testCase "settlement_handler_spend_disprove_rejects_input_index_not_the_spent_utxo" $
            pfails $ runDisprove disproveDefaults {dsOwnOutRefMatchesInput = False}
        , testCase "settlement_handler_spend_disprove_rejects_continuing_datum_that_keeps_the_claim" $
            pfails $ runDisprove disproveDefaults {dsOutputKeepsClaim = True}
        , testCase "settlement_handler_spend_disprove_rejects_unauthentic_hub_oracle_reference_input" $
            pfails $ runDisprove disproveDefaults {dsAuthenticHub = False}
        , testCase "settlement_handler_spend_disprove_rejects_event_reference_input_of_the_wrong_script" $
            pfails $ runDisprove disproveDefaults {dsEventAtDepositPolicy = False}
        , testCase "settlement_handler_spend_disprove_rejects_membership_witness_for_a_foreign_root" $
            pfails $ runDisprove disproveDefaults {dsMembershipRoot = Just (root 0xdf)}
        , testCase "settlement_handler_spend_disprove_rejects_phas_withdraw_redeemer_that_does_not_match" $
            pfails $ runDisprove disproveDefaults {dsPhasRedeemerRoot = Just (root 0xdf)}
        , -- The dispute must land inside the claim's own deadline.
          testCase "settlement_handler_spend_disprove_rejects_upper_bound_at_or_after_the_resolution_time" $
            pfails $ runDisprove disproveDefaults {dsValidTo = attachResolutionTime}
        , testCase "settlement_handler_spend_disprove_rejects_slashing_redeemer_for_another_operator" $
            pfails $ runDisprove disproveDefaults {dsSlashedOperator = Just "zz"}
        , testCase "settlement_handler_spend_disprove_rejects_retired_slashing_redeemer_for_another_operator" $
            pfails $
              runDisprove
                disproveDefaults
                  { dsOperatorIsActive = False
                  , dsSlashedOperator = Just "zz"
                  }
        , -- The slash must be *for this reason*, not merely happening.
          testCase "settlement_handler_spend_disprove_rejects_slashing_for_a_reason_other_than_settlement" $
            pfails $ runDisprove disproveDefaults {dsSlashReasonIsSettlement = False}
        , -- Additional event/value binding beyond the Aiken named matrix.
          testCase "rejects an event whose datum differs from the membership value" $
            pfails $ runDisprove disproveDefaults {dsProvenInfo = Just (PD.Constr 0 [addrData auxiliaryPolicy, PD.I 9, PD.Constr 1 []])}
        ]
    ]

{- | Forces the decode and is 'True' for every value the type can hold.

The Aiken original explains the shape: a rejection test is only meaningful if
the body cannot itself be false, so the only way to fail is for the decoder to
reject.
-}
decodeTotal :: forall s. PD.Data -> Term s PBool
decodeTotal d =
  pmatch (pdecodeMintRedeemer # pconstant @PData d) $ \case
    PSpawn {} -> pconstant True
    PRemove {} -> pconstant True

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

hubOraclePolicy, settlementPolicy, stateQueuePolicy, auxiliaryPolicy :: CurrencySymbol
hubOraclePolicy = repeatedByte 0x11
settlementPolicy = repeatedByte 0x22
stateQueuePolicy = repeatedByte 0x33
auxiliaryPolicy = repeatedByte 0x55

repeatedByte :: Int -> CurrencySymbol
repeatedByte b = currencySymbolFromHex (concat (replicate 28 h))
  where
    h = let x = showHex b "" in if length x == 1 then '0' : x else x

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

-- | The merged block's header hash, which is also the settlement's asset name.
headerKey :: BS.ByteString
headerKey = "hh"

root :: Int -> BS.ByteString
root b = BS.replicate 32 (fromIntegral b)

depositsRoot, withdrawalsRoot, forcedRoot, txsRoot :: BS.ByteString
depositsRoot = root 0xa1
withdrawalsRoot = root 0xa2
forcedRoot = root 0xa3
txsRoot = root 0xa4

addrData :: CurrencySymbol -> PD.Data
addrData cs =
  PD.Constr 0 [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol cs))], PD.Constr 1 []]

{- | The hub oracle: @state_queue@ (policy 4) and @settlement_addr@
(address 10) are the fields this validator reads.
-}
hubRefIn :: TxInInfo
hubRefIn =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubOraclePolicy)))
        (mkAdaValue 2_000_000 <> singleton hubOraclePolicy hubAssetName 1)
        (OutputDatum (Datum hubDatum))
        Nothing
    )

hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( replicate 4 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol stateQueuePolicy))] -- 4: state_queue
          <> replicate 7 auxPolicyData
          <> replicate 10 (addrData auxiliaryPolicy)
          <> [addrData settlementPolicy] -- address 10: settlement_addr
          <> replicate 2 (addrData auxiliaryPolicy)
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

--------------------------------------------------------------------------------
-- Spawn
--------------------------------------------------------------------------------

data Spawn = Spawn
  { spAuthenticHub :: Bool
  , spMergeAtStateQueuePolicy :: Bool
  , spOutputAtSettlementAddress :: Bool
  , spOutputHasSettlementNft :: Bool
  , spOutputHasReferenceScript :: Bool
  , spMintQuantity :: Integer
  , spDepositsRoot :: Maybe BS.ByteString
  , spWithClaim :: Bool
  , spHeaderKey :: Maybe BS.ByteString
  , spMergeTag :: Integer
  , spMergeArity :: Int
  , spSettlementIndexPresent :: Bool
  , spExtraMint :: Bool
  }

spawnDefaults :: Spawn
spawnDefaults =
  Spawn
    { spAuthenticHub = True
    , spMergeAtStateQueuePolicy = True
    , spOutputAtSettlementAddress = True
    , spOutputHasSettlementNft = True
    , spOutputHasReferenceScript = False
    , spMintQuantity = 1
    , spDepositsRoot = Nothing
    , spWithClaim = False
    , spHeaderKey = Nothing
    , spMergeTag = 4
    , spMergeArity = 18
    , spSettlementIndexPresent = True
    , spExtraMint = False
    }

runSpawn :: forall s. Spawn -> Term s PUnit
runSpawn sp =
  settlementMintValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    settlementId = TokenName (toBuiltin headerKey)
    outputSettlementId =
      if spOutputHasSettlementNft sp
        then settlementId
        else TokenName "other"
    outputPolicy =
      if spOutputAtSettlementAddress sp
        then settlementPolicy
        else auxiliaryPolicy
    mergePolicy =
      if spMergeAtStateQueuePolicy sp
        then stateQueuePolicy
        else auxiliaryPolicy

    hubReferenceInput
      | spAuthenticHub sp = hubRefIn
      | otherwise = case hubRefIn of
          TxInInfo ref (TxOut address _ datum referenceScript) ->
            TxInInfo
              ref
              ( TxOut
                  address
                  (mkAdaValue 2_000_000 <> singleton auxiliaryPolicy hubAssetName 1)
                  datum
                  referenceScript
              )

    {- The state queue's @MergeToConfirmedStateV1@ redeemer. Only fields 0, 3
    and 4..7 are read; the rest are padding, but the arity check means they
    have to be there. -}
    mergeRedeemer =
      dataToBuiltinData $
        PD.Constr
          (spMergeTag sp)
          ( take
              (spMergeArity sp)
              ( [ PD.B (maybe headerKey id (spHeaderKey sp)) -- 0: header node key
                , PD.I 0
                , PD.I 0
                , if spSettlementIndexPresent sp
                    then PD.Constr 0 [PD.I 0] -- 3: Some(index)
                    else PD.Constr 1 [] -- None
                , PD.B withdrawalsRoot -- 4
                , PD.B forcedRoot -- 5
                , PD.B txsRoot -- 6
                , PD.B depositsRoot -- 7
                ]
                  <> replicate 10 (PD.I 0)
              )
          )

    settlementDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.B (maybe depositsRoot id (spDepositsRoot sp))
          , PD.B withdrawalsRoot
          , PD.B forcedRoot
          , PD.B txsRoot
          , if spWithClaim sp
              then PD.Constr 0 [PD.Constr 0 [PD.I 1, PD.B "op"]]
              else PD.Constr 1 []
          ]

    -- @Spawn { settlement_id, output_index, merge_redeemer_index, hub_index }@.
    mintRedeemer =
      dataToBuiltinData (PD.Constr 0 [PD.B headerKey, PD.I 0, PD.I 0, PD.I 0])

    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoReferenceInputs = [hubReferenceInput]
        , txInfoOutputs =
            [ TxOut
                (scriptHashAddress (ScriptHash (unCurrencySymbol outputPolicy)))
                (mkAdaValue 2_000_000 <> singleton settlementPolicy outputSettlementId 1)
                (OutputDatum (Datum settlementDatum))
                ( if spOutputHasReferenceScript sp
                    then Just (ScriptHash (unCurrencySymbol auxiliaryPolicy))
                    else Nothing
                )
            ]
        , txInfoMint =
            toMint
              ( singleton settlementPolicy settlementId (spMintQuantity sp)
                  <> (if spExtraMint sp then singleton settlementPolicy (TokenName "x") 1 else mempty)
              )
        , txInfoRedeemers =
            Map.unsafeFromList [(Minting mergePolicy, Redeemer mergeRedeemer)]
        }
    ctx = ScriptContext txInfo (Redeemer mintRedeemer) (MintingScript settlementPolicy)

--------------------------------------------------------------------------------
-- Remove
--------------------------------------------------------------------------------

data Remove = Remove
  { rmInputHasSettlementNft :: Bool
  , rmSpendPurposeMatches :: Bool
  , rmValidFrom :: Integer
  , rmSigned :: Bool
  , rmSigner :: Maybe BS.ByteString
  , rmHasClaim :: Bool
  , rmResolveRedeemer :: Bool
  , rmSpendId :: Maybe BS.ByteString
  }

removeDefaults :: Remove
removeDefaults =
  Remove
    { rmInputHasSettlementNft = True
    , rmSpendPurposeMatches = True
    , rmValidFrom = 2_000
    , rmSigned = True
    , rmSigner = Nothing
    , rmHasClaim = True
    , rmResolveRedeemer = True
    , rmSpendId = Nothing
    }

-- | The operator that attached the resolution claim.
claimOperator :: BS.ByteString
claimOperator = "op"

-- | The claimed resolution time; removal may not precede it.
resolutionTime :: Integer
resolutionTime = 1_000

runRemove :: forall s. Remove -> Term s PUnit
runRemove rm =
  settlementMintValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    settlementId = TokenName (toBuiltin headerKey)

    settlementDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.B depositsRoot
          , PD.B withdrawalsRoot
          , PD.B forcedRoot
          , PD.B txsRoot
          , if rmHasClaim rm
              then PD.Constr 0 [PD.Constr 0 [PD.I resolutionTime, PD.B claimOperator]]
              else PD.Constr 1 []
          ]

    settlementIn =
      TxInInfo
        (outRefN 0)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol settlementPolicy)))
            ( mkAdaValue 2_000_000
                <> singleton
                  settlementPolicy
                  (if rmInputHasSettlementNft rm then settlementId else TokenName "other")
                  1
            )
            (OutputDatum (Datum settlementDatum))
            Nothing
        )

    -- @Resolve@ is constructor 2 of the settlement spend redeemer.
    spendRedeemer
      | rmResolveRedeemer rm =
          dataToBuiltinData (PD.Constr 2 [PD.B (maybe headerKey id (rmSpendId rm))])
      | otherwise = dataToBuiltinData (PD.Constr 0 (replicate 7 (PD.I 0)))

    -- @Remove { settlement_id, input_index, spend_redeemer_index }@.
    mintRedeemer =
      dataToBuiltinData (PD.Constr 1 [PD.B headerKey, PD.I 0, PD.I 0])

    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [settlementIn]
        , txInfoReferenceInputs = [hubRefIn]
        , txInfoMint = toMint (singleton settlementPolicy settlementId (-1))
        , txInfoSignatories =
            [PubKeyHash (toBuiltin (maybe claimOperator id (rmSigner rm))) | rmSigned rm]
        , txInfoValidRange =
            Interval
              (LowerBound (Finite (POSIXTime (rmValidFrom rm))) True)
              (UpperBound PosInf True)
        , txInfoRedeemers =
            Map.unsafeFromList
              [ ( Spending (if rmSpendPurposeMatches rm then outRefN 0 else outRefN 1)
                , Redeemer spendRedeemer
                )
              ]
        }
    ctx = ScriptContext txInfo (Redeemer mintRedeemer) (MintingScript settlementPolicy)

--------------------------------------------------------------------------------
-- Spend
--------------------------------------------------------------------------------

activeOperatorsPolicy, schedulerPolicy :: CurrencySymbol
activeOperatorsPolicy = repeatedByte 0x44
schedulerPolicy = repeatedByte 0x66

claimOperatorKey :: BS.ByteString
claimOperatorKey = "op"

attachResolutionTime :: Integer
attachResolutionTime = 5_000

data Attach = Attach
  { atOwnDatumPresent :: Bool
  , atOwnOutRefMatchesInput :: Bool
  , atOutputAtSettlementAddress :: Bool
  , atOutputHasSettlementNft :: Bool
  , atOutputHasReferenceScript :: Bool
  , atAuthenticHub :: Bool
  , atActiveRedeemerIsBondHold :: Bool
  , atActiveInputAtExpectedAddress :: Bool
  , atSchedulerIsActive :: Bool
  , atSigned :: Bool
  , atExistingClaim :: Bool
  , atSchedulerOperator :: Maybe BS.ByteString
  , atActiveOperator :: Maybe BS.ByteString
  , atOutputTime :: Maybe Integer
  , atOutputDepositsRoot :: Maybe BS.ByteString
  }

attachDefaults :: Attach
attachDefaults =
  Attach
    { atOwnDatumPresent = True
    , atOwnOutRefMatchesInput = True
    , atOutputAtSettlementAddress = True
    , atOutputHasSettlementNft = True
    , atOutputHasReferenceScript = False
    , atAuthenticHub = True
    , atActiveRedeemerIsBondHold = True
    , atActiveInputAtExpectedAddress = True
    , atSchedulerIsActive = True
    , atSigned = True
    , atExistingClaim = False
    , atSchedulerOperator = Nothing
    , atActiveOperator = Nothing
    , atOutputTime = Nothing
    , atOutputDepositsRoot = Nothing
    }

{- | An @AttachResolutionClaim@ transaction.

Inputs: the settlement at 0, the active-operators node at 1. Reference inputs:
hub at 0, scheduler at 1. Redeemers: the active-operators spend at 0.
-}
runAttach :: forall s. Attach -> Term s PUnit
runAttach at =
  settlementSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pdata (pconstant (ScriptHash (unCurrencySymbol settlementPolicy)))
    # pconstant ctx
  where
    settlementId = TokenName (toBuiltin headerKey)
    settlementValue = mkAdaValue 2_000_000 <> singleton settlementPolicy settlementId 1
    outputSettlementId =
      if atOutputHasSettlementNft at
        then settlementId
        else TokenName "other"
    outputAddressPolicy =
      if atOutputAtSettlementAddress at
        then settlementPolicy
        else auxiliaryPolicy
    activeInputAddressPolicy =
      if atActiveInputAtExpectedAddress at
        then activeOperatorsPolicy
        else auxiliaryPolicy
    ownOutRef =
      if atOwnOutRefMatchesInput at
        then outRefN 0
        else outRefN 99

    claimData t = PD.Constr 0 [PD.Constr 0 [PD.I t, PD.B claimOperatorKey]]

    datumWith deposits claim =
      dataToBuiltinData $
        PD.Constr 0 [PD.B deposits, PD.B withdrawalsRoot, PD.B forcedRoot, PD.B txsRoot, claim]

    inputDatum = datumWith depositsRoot (if atExistingClaim at then claimData 1 else PD.Constr 1 [])
    outputDatum =
      datumWith
        (maybe depositsRoot id (atOutputDepositsRoot at))
        (claimData (maybe attachResolutionTime id (atOutputTime at)))

    settlementInput =
      TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol settlementPolicy)))
        settlementValue
        (OutputDatum (Datum inputDatum))
        Nothing

    settlementOutput =
      TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol outputAddressPolicy)))
        (mkAdaValue 2_000_000 <> singleton settlementPolicy outputSettlementId 1)
        (OutputDatum (Datum outputDatum))
        ( if atOutputHasReferenceScript at
            then Just (ScriptHash (unCurrencySymbol auxiliaryPolicy))
            else Nothing
        )

    activeNodeIn =
      TxInInfo
        (outRefN 1)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol activeInputAddressPolicy)))
            (mkAdaValue 2_000_000)
            NoOutputDatum
            Nothing
        )

    -- @UpdateBondHoldNewSettlement@ is constructor 2 of the active spend
    -- redeemer; operator is field 0 and resolution_time field 6.
    activeRedeemer
      | atActiveRedeemerIsBondHold at =
          dataToBuiltinData $
            PD.Constr
              2
              [ PD.B (maybe claimOperatorKey id (atActiveOperator at))
              , PD.I 0
              , PD.I 0
              , PD.I 0
              , PD.I 0
              , PD.I 0
              , PD.I attachResolutionTime
              ]
      | otherwise = dataToBuiltinData (PD.Constr 0 [])

    schedulerRefIn =
      TxInInfo
        (outRefN 7)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol schedulerPolicy)))
            (mkAdaValue 2_000_000 <> singleton schedulerPolicy (TokenName "MIDGARD_SCHEDULER") 1)
            ( OutputDatum
                ( Datum
                    ( dataToBuiltinData
                        ( if atSchedulerIsActive at
                            then
                              PD.Constr
                                1
                                [PD.B (maybe claimOperatorKey id (atSchedulerOperator at)), PD.I 0]
                            else PD.Constr 0 []
                        )
                    )
                )
            )
            Nothing
        )

    spendRedeemer =
      dataToBuiltinData
        (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.I 0, PD.B claimOperatorKey, PD.I 1])

    base = buildScriptContext mempty
    hubReferenceInput
      | atAuthenticHub at = hubSpendRefIn
      | otherwise = case hubSpendRefIn of
          TxInInfo ref (TxOut address _ datum referenceScript) ->
            TxInInfo
              ref
              ( TxOut
                  address
                  (mkAdaValue 2_000_000 <> singleton auxiliaryPolicy hubAssetName 1)
                  datum
                  referenceScript
              )
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [TxInInfo (outRefN 0) settlementInput, activeNodeIn]
        , txInfoReferenceInputs = [hubReferenceInput, schedulerRefIn]
        , txInfoOutputs = [settlementOutput]
        , txInfoSignatories = [PubKeyHash (toBuiltin claimOperatorKey) | atSigned at]
        , txInfoRedeemers = Map.unsafeFromList [(Spending (outRefN 1), Redeemer activeRedeemer)]
        }
    ctx =
      ScriptContext
        txInfo
        (Redeemer spendRedeemer)
        ( SpendingScript
            ownOutRef
            (if atOwnDatumPresent at then Just (Datum inputDatum) else Nothing)
        )

data Resolve = Resolve
  { rsBurnId :: Maybe BS.ByteString
  , rsBurnQuantity :: Integer
  }

resolveDefaults :: Resolve
resolveDefaults = Resolve {rsBurnId = Nothing, rsBurnQuantity = -1}

-- | A @Resolve@ spend: exactly this settlement's NFT must be burnt once.
runResolve :: forall s. Resolve -> Term s PUnit
runResolve resolve =
  settlementSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pdata (pconstant (ScriptHash (unCurrencySymbol settlementPolicy)))
    # pconstant ctx
  where
    burnedId = TokenName (toBuiltin (maybe headerKey id (rsBurnId resolve)))
    inputDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [PD.B depositsRoot, PD.B withdrawalsRoot, PD.B forcedRoot, PD.B txsRoot, PD.Constr 1 []]
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoMint =
            toMint (singleton settlementPolicy burnedId (rsBurnQuantity resolve))
        }
    ctx =
      ScriptContext
        txInfo
        (Redeemer (dataToBuiltinData (PD.Constr 2 [PD.B headerKey])))
        (SpendingScript (outRefN 0) (Just (Datum inputDatum)))

{- | The hub oracle as the spend path reads it.

@active_operators@ (4), @scheduler@ (3) and @active_operators_addr@ (address 1)
are what the attach branch needs.
-}
hubSpendRefIn :: TxInInfo
hubSpendRefIn =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubOraclePolicy)))
        (mkAdaValue 2_000_000 <> singleton hubOraclePolicy hubAssetName 1)
        (OutputDatum (Datum spendHubDatum))
        Nothing
    )

spendHubDatum :: BuiltinData
spendHubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( [auxPolicyData]
          <> [PD.B (fromBuiltin (unCurrencySymbol activeOperatorsPolicy))] -- 1: active_operators
          <> [PD.B (fromBuiltin (unCurrencySymbol retiredOperatorsPolicy))] -- 2: retired
          <> [PD.B (fromBuiltin (unCurrencySymbol schedulerPolicy))] -- 3: scheduler
          <> replicate 3 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol depositEventPolicy))] -- 7: deposit
          <> [auxPolicyData] -- 8: withdrawal
          <> [auxPolicyData] -- 9: tx_order
          <> replicate 2 auxPolicyData
          <> [addrData auxiliaryPolicy]
          <> [addrData activeOperatorsPolicy] -- address 1: active_operators_addr
          <> replicate 11 (addrData auxiliaryPolicy)
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))

--------------------------------------------------------------------------------
-- DisproveResolutionClaim
--------------------------------------------------------------------------------

depositEventPolicy :: CurrencySymbol
depositEventPolicy = repeatedByte 0x77

retiredOperatorsPolicy :: CurrencySymbol
retiredOperatorsPolicy = repeatedByte 0x88

phasValidatorHash :: BS.ByteString
phasValidatorHash =
  either (error "bad hex") id
    (Base16.decode (BS8.pack "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"))

disprovePhasRoot :: BS.ByteString
disprovePhasRoot = BS.replicate 32 0xaa

disproveCount :: Integer
disproveCount = 2

-- | @commit_counted_root@ for the deposits domain (tag 3).
disproveDepositsRoot :: BS.ByteString
disproveDepositsRoot =
  fromBuiltin $
    blake2b_256
      ( toBuiltin ("MidgardRootCountV1" :: BS.ByteString)
          <> serialiseData (dataToBuiltinData (PD.Constr 3 []))
          <> toBuiltin disprovePhasRoot
          <> serialiseData (dataToBuiltinData (PD.I disproveCount))
      )

serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = fromBuiltin . serialiseData . dataToBuiltinData

eventAssetName :: TokenName
eventAssetName = TokenName "evt"

disproveEventId :: PD.Data
disproveEventId = builtinDataToData (toBuiltinData (outRefN 4))

disproveDepositInfo :: PD.Data
disproveDepositInfo = PD.Constr 0 [addrData auxiliaryPolicy, PD.I 0, PD.Constr 1 []]

data Disprove = Disprove
  { dsOwnOutRefMatchesInput :: Bool
  , dsAuthenticHub :: Bool
  , dsEventAtDepositPolicy :: Bool
  , dsMembershipRoot :: Maybe BS.ByteString
  , dsPhasRedeemerRoot :: Maybe BS.ByteString
  , dsOperatorIsActive :: Bool
  , dsHasClaim :: Bool
  , dsClaimOperator :: Maybe BS.ByteString
  , dsSlashedOperator :: Maybe BS.ByteString
  , dsSlashReasonIsSettlement :: Bool
  , dsValidTo :: Integer
  , dsOutputKeepsClaim :: Bool
  , dsProvenInfo :: Maybe PD.Data
  }

disproveDefaults :: Disprove
disproveDefaults =
  Disprove
    { dsOwnOutRefMatchesInput = True
    , dsAuthenticHub = True
    , dsEventAtDepositPolicy = True
    , dsMembershipRoot = Nothing
    , dsPhasRedeemerRoot = Nothing
    , dsOperatorIsActive = True
    , dsHasClaim = True
    , dsClaimOperator = Nothing
    , dsSlashedOperator = Nothing
    , dsSlashReasonIsSettlement = True
    , dsValidTo = 1_000
    , dsOutputKeepsClaim = False
    , dsProvenInfo = Nothing
    }

{- | A @DisproveResolutionClaim@ transaction.

Inputs: the settlement at 0. Reference inputs: hub at 0, the deposit event at 1.
Redeemers: the operator set's slash at 0, the @phas@ withdrawal at 1.
-}
runDisprove :: forall s. Disprove -> Term s PUnit
runDisprove ds =
  settlementSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pdata (pconstant (ScriptHash (unCurrencySymbol settlementPolicy)))
    # pconstant ctx
  where
    settlementId = TokenName (toBuiltin headerKey)
    settlementValue = mkAdaValue 2_000_000 <> singleton settlementPolicy settlementId 1
    ownOutRef =
      if dsOwnOutRefMatchesInput ds
        then outRefN 0
        else outRefN 99
    eventPolicy =
      if dsEventAtDepositPolicy ds
        then depositEventPolicy
        else auxiliaryPolicy

    claimOperator = maybe claimOperatorKey id (dsClaimOperator ds)
    claimData =
      PD.Constr 0 [PD.Constr 0 [PD.I attachResolutionTime, PD.B claimOperator]]

    datumWith claim =
      dataToBuiltinData $
        PD.Constr
          0
          [PD.B disproveDepositsRoot, PD.B withdrawalsRoot, PD.B forcedRoot, PD.B txsRoot, claim]

    inputDatum = datumWith (if dsHasClaim ds then claimData else PD.Constr 1 [])
    outputDatum = datumWith (if dsOutputKeepsClaim ds then claimData else PD.Constr 1 [])

    settlementUtxo d =
      TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol settlementPolicy)))
        settlementValue
        (OutputDatum (Datum d))
        Nothing

    eventRefIn =
      TxInInfo
        (outRefN 4)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol eventPolicy)))
            (mkAdaValue 2_000_000 <> singleton eventPolicy eventAssetName 1)
            (OutputDatum (Datum (dataToBuiltinData (PD.Constr 0 [disproveEventId, disproveDepositInfo]))))
            Nothing
        )

    provenInfo = maybe disproveDepositInfo id (dsProvenInfo ds)

    -- @DepositMembership { witness }@ — constructor 0.
    membershipProof =
      PD.Constr
        0
        [ PD.Constr
            0
            [ PD.Constr 3 [] -- DepositsRootDomain
            , PD.B (maybe disproveDepositsRoot id (dsMembershipRoot ds))
            , PD.B disprovePhasRoot
            , PD.I disproveCount
            , PD.B (serialisedOf disproveEventId)
            , PD.B (serialisedOf provenInfo)
            , PD.List [PD.B "step"]
            ]
        ]

    phasRedeemer =
      dataToBuiltinData $
        PD.List
          [ PD.B (maybe disprovePhasRoot id (dsPhasRedeemerRoot ds))
          , PD.B (serialisedOf disproveEventId)
          , PD.B (serialisedOf provenInfo)
          , PD.List [PD.B "step"]
          ]

    -- @SlashingArguments { slashed_operator, hub_index, anchor_outref,
    -- anchor_output_index, slashing_reason }@.
    slashingArguments =
      PD.Constr
        0
        [ PD.B (maybe claimOperatorKey id (dsSlashedOperator ds))
        , PD.I 0
        , builtinDataToData (toBuiltinData (outRefN 0))
        , PD.I 0
        , if dsSlashReasonIsSettlement ds
            then PD.Constr 1 [PD.I 0, PD.I 0] -- SlashOperatorForBadSettlement
            else PD.Constr 0 [PD.I 0] -- SlashOperatorForBadState
        ]

    -- Active @SlashOperator@ is constructor 4 (two fields); retired
    -- @SlashOperator@ is constructor 4 (one field).
    slashRedeemer
      | dsOperatorIsActive ds = dataToBuiltinData (PD.Constr 4 [slashingArguments, PD.I 0])
      | otherwise = dataToBuiltinData (PD.Constr 4 [slashingArguments])

    slashPolicy =
      if dsOperatorIsActive ds then activeOperatorsPolicy else retiredOperatorsPolicy

    spendRedeemer =
      dataToBuiltinData $
        PD.Constr
          1
          [ PD.I 0 -- settlement_input_index
          , PD.I 0 -- settlement_output_index
          , PD.I 0 -- hub_ref_input_index
          , PD.I 0 -- operators_redeemer_index
          , PD.B claimOperatorKey
          , PD.Constr (if dsOperatorIsActive ds then 1 else 0) [] -- operator_is_active
          , PD.I 1 -- unresolved_event_ref_input_index
          , PD.B "evt"
          , PD.Constr 0 [] -- event_type: Deposit
          , membershipProof
          , PD.I 1
          ]

    base = buildScriptContext mempty
    hubReferenceInput
      | dsAuthenticHub ds = hubSpendRefIn
      | otherwise = case hubSpendRefIn of
          TxInInfo ref (TxOut address _ datum referenceScript) ->
            TxInInfo
              ref
              ( TxOut
                  address
                  (mkAdaValue 2_000_000 <> singleton auxiliaryPolicy hubAssetName 1)
                  datum
                  referenceScript
              )
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [TxInInfo (outRefN 0) (settlementUtxo inputDatum)]
        , txInfoReferenceInputs = [hubReferenceInput, eventRefIn]
        , txInfoOutputs = [settlementUtxo outputDatum]
        , txInfoValidRange =
            Interval
              (LowerBound NegInf True)
              (UpperBound (Finite (POSIXTime (dsValidTo ds))) True)
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Minting slashPolicy, Redeemer slashRedeemer)
              ,
                ( Rewarding (ScriptCredential (ScriptHash (toBuiltin phasValidatorHash)))
                , Redeemer phasRedeemer
                )
              ]
        }
    ctx =
      ScriptContext
        txInfo
        (Redeemer spendRedeemer)
        (SpendingScript ownOutRef (Just (Datum inputDatum)))

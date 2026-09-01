{- |
Module      : Midgard.UserEvents
Description : Plutarch port of @lib/midgard/user-events.ak@.

The shared minting policy behind all three user events — deposits, withdrawals
and transaction orders. Each has its own validator, but they all create and
destroy their event NFTs through 'pvalidateMint', supplying only the hub oracle
field naming their address and a predicate over the produced UTxO.

The mechanism, once, since it recurs in all three: the event's identity is a
nonce, the blake2b-256 of a spent UTxO's serialised output reference. That
nonce is the NFT's token name, and it also parameterises a witness staking
script whose registration the same transaction must carry. The nonce cannot
repeat, because the UTxO it names is consumed.

Only the mint side is ported. The spend side of each event is where it is
consumed into the L2 ledger, which needs settlement's counted membership proofs.
-}
module Midgard.UserEvents (
  PMintRedeemer (..),
  poutRefToNonce,
  pvalidateMint,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pasConstr, pserialiseData)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PMintValue,
  POutputDatum (..),
  PRedeemer,
  PScriptHash (..),
  PScriptPurpose,
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.V3 qualified as LedgerV3
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.ParameterValidation (papplyPrehashedParam)
import Midgard.Common.Utils (pgetInclusiveUpperBoundOfInterval)
import Midgard.Env qualified as Env
import Midgard.HubOracle (PHubOracleDatum)
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (punsafeEventToKeyValuePair)
import Midgard.UserEvents.Witness (pvalidateWitnessRedeemer)

{- | Aiken @user_events.MintRedeemer@.

Tags: @AuthenticateEvent@ 0, @BurnEventNFT@ 1.
-}
data PMintRedeemer (s :: S)
  = PAuthenticateEvent
      { pauthenticate'nonceInputIndex :: Term s (PAsData PInteger)
      , pauthenticate'eventOutputIndex :: Term s (PAsData PInteger)
      , pauthenticate'hubRefInputIndex :: Term s (PAsData PInteger)
      , pauthenticate'witnessRegistrationRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PBurnEventNFT
      { pburnEvent'nonceAssetName :: Term s (PAsData PTokenName)
      , pburnEvent'witnessUnregistrationRedeemerIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @user_events.out_ref_to_nonce@.

@cbor.serialise(out_ref) |> blake2b_256@ — Aiken's @cbor.serialise@ is the
@serialiseData@ builtin, so this is the hash of the output reference's own
@Data@ encoding.
-}
poutRefToNonce :: forall (s :: S). Term s (PAsData PTxOutRef :--> PTokenName)
poutRefToNonce = phoistAcyclic $
  plam $ \outRef ->
    pcon (PTokenName (pblake2b_256 #$ pserialiseData # pforgetData outRef))

{- | Aiken @user_events.validate_mint@.

@event_address_getter@ picks this event's address out of the hub datum, and
@event_validator@ is the per-event check on the produced UTxO. Both are Haskell
functions over Plutarch terms, matching the Aiken original's function arguments.
-}
pvalidateMint ::
  forall (s :: S).
  Term s (PAsData PScriptHash) ->
  (Term s PHubOracleDatum -> Term s (PAsData PAddress)) ->
  Term s PMintRedeemer ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PInterval LedgerV3.PPosixTime) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s (PAsData PTokenName) ->
    Term s PData ->
    Term s PData ->
    Term s PBool
  ) ->
  Term s PBool
pvalidateMint
  hubOracle
  eventAddressGetter
  redeemer
  ownPolicy
  inputs
  outputs
  referenceInputs
  validityRange
  mint
  redeemers
  eventValidator =
    pmatch redeemer $ \case
      PAuthenticateEvent
        { pauthenticate'nonceInputIndex
        , pauthenticate'eventOutputIndex
        , pauthenticate'hubRefInputIndex
        , pauthenticate'witnessRegistrationRedeemerIndex
        } ->
          pauthenticateNewEvent
            ( eventAddressGetter
                ( Hub.pgetDatum
                    # referenceInputs
                    # hubOracle
                    # pfromData pauthenticate'hubRefInputIndex
                )
            )
            (pfromData pauthenticate'nonceInputIndex)
            (pfromData pauthenticate'eventOutputIndex)
            (pfromData pauthenticate'witnessRegistrationRedeemerIndex)
            ownPolicy
            inputs
            outputs
            validityRange
            mint
            redeemers
            eventValidator
      PBurnEventNFT
        { pburnEvent'nonceAssetName
        , pburnEvent'witnessUnregistrationRedeemerIndex
        } ->
          pvalidateEventNftBurn
            pburnEvent'nonceAssetName
            (pfromData pburnEvent'witnessUnregistrationRedeemerIndex)
            ownPolicy
            mint
            redeemers

{- | Aiken @user_events.authenticate_new_event@.

Creates an event. The eight numbered checks of the original, in order: the NFT
is minted once under the nonce; the witness script derived from that nonce is
being registered with the right redeemer; the produced UTxO has an inline datum
and no reference script; its event id is the spent output reference; it sits at
this event's address; its inclusion time is @valid_to + event_wait_duration@;
its recorded witness is the derived hash; and the caller's own predicate holds.

The datum is read positionally — the first three fields of every event datum are
its event, inclusion time and witness — which is what lets one function serve
three different event types.
-}
pauthenticateNewEvent ::
  forall (s :: S).
  Term s (PAsData PAddress) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PInterval LedgerV3.PPosixTime) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s (PAsData PTokenName) ->
    Term s PData ->
    Term s PData ->
    Term s PBool
  ) ->
  Term s PBool
pauthenticateNewEvent
  eventAddress
  nonceInputIndex
  eventOutputIndex
  witnessRegistrationRedeemerIndex
  ownPolicy
  inputs
  outputs
  validityRange
  mint
  redeemers
  eventValidator = P.do
    validTo <- plet $ pgetInclusiveUpperBoundOfInterval # validityRange
    PTxInInfo {ptxInInfo'outRef} <-
      pmatch $ pfromData (pelemAt # nonceInputIndex # inputs)
    nonceOutRef <- plet $ pdata ptxInInfo'outRef
    nonce <- plet $ pdata (poutRefToNonce # nonceOutRef)
    expectedWitnessScriptHash <-
      plet $
        pdata
          ( papplyPrehashedParam
              # Env.pplutusVersion
              # Env.puserEventsWitnessScriptPrefix
              # pto (pfromData nonce)
          )
    PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
      pmatch $ pfromData (pelemAt # eventOutputIndex # outputs)
    eventDatumData <-
      plet $ pmatch ptxOut'datum $ \case
        POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
        _ -> perror
    datumFields <- plet $ psndBuiltin # (pasConstr # eventDatumData)
    eventData <- plet $ phead # datumFields
    let (eventId, eventInfo) = punsafeEventToKeyValuePair eventData
    pand'List
      [ Value.pvalueOf # pto mint # pfromData ownPolicy # pfromData nonce #== 1
      , pvalidateWitnessRedeemer
          expectedWitnessScriptHash
          witnessRegistrationRedeemerIndex
          True
          ownPolicy
          redeemers
      , pmatch ptxOut'referenceScript $ \case
          PDNothing -> pconstant True
          PDJust _ -> pconstant False
      , eventId #== pforgetData nonceOutRef
      , pdata ptxOut'address #== eventAddress
      , pfromData (punsafeCoerce @(PAsData PInteger) (phead #$ ptail # datumFields))
          #== validTo + Env.peventWaitDuration
      , (phead #$ ptail #$ ptail # datumFields) #== pforgetData expectedWitnessScriptHash
      , eventValidator nonce (pforgetData (pdata (pfromData ptxOut'value))) eventInfo
      ]

{- | Aiken @user_events.validate_event_nft_burn@.

Destroys an event. The burn must be the /only/ mint or burn under this policy —
so one transaction cannot quietly create an event while destroying another — and
the corresponding witness credential must be unregistered.
-}
pvalidateEventNftBurn ::
  forall (s :: S).
  Term s (PAsData PTokenName) ->
  Term s PInteger ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pvalidateEventNftBurn
  nonceAssetName
  witnessUnregistrationRedeemerIndex
  ownPolicy
  mint
  redeemers =
    pand'List
      [ pmatch (AssocMap.plookup # pfromData ownPolicy # pto (pto mint)) $ \case
          PNothing -> pconstant False
          PJust tokenMap ->
            pto (pto tokenMap)
              #== (psingleton # (ppairDataBuiltin # nonceAssetName # pdata (-1)))
      , pvalidateWitnessRedeemer
          ( pdata
              ( papplyPrehashedParam
                  # Env.pplutusVersion
                  # Env.puserEventsWitnessScriptPrefix
                  # pto (pfromData nonceAssetName)
              )
          )
          witnessUnregistrationRedeemerIndex
          False
          ownPolicy
          redeemers
      ]

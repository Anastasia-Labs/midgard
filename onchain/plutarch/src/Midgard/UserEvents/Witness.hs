{- |
Module      : Midgard.UserEvents.Witness
Description : Plutarch port of @lib/midgard/user-events/witness.ak@.

Every user event is paired with a witness staking script whose hash is derived
from the event's own nonce. Creating an event registers that credential;
burning the event's NFT unregisters it.

That pairing is what makes an event unforgeable and unrepeatable: the nonce
comes from a spent UTxO, so the witness hash is unique, and the ledger itself
refuses to register an already-registered credential. Nothing here needs to
scan for duplicates.
-}
module Midgard.UserEvents.Witness (
  PPublishRedeemer (..),
  pvalidateWitnessRedeemer,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (
  PCredential (..),
  PCurrencySymbol,
  PRedeemer (..),
  PScriptHash,
  PScriptPurpose (..),
  PTxCert (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

{- | Aiken @witness.PublishRedeemer@.

Tags: @MintOrBurn@ 0, @RegisterToProveNotRegistered@ 1,
@UnregisterToProveNotRegistered@ 2.

Only @MintOrBurn@ is constructed here; the other two belong to the witness
script's own "prove this event was never created" path, which is a separate
slice.
-}
data PPublishRedeemer (s :: S)
  = PMintOrBurn {pmintOrBurn'targetPolicy :: Term s (PAsData PCurrencySymbol)}
  | PRegisterToProveNotRegistered
      {pregisterToProve'registrationCertificateIndex :: Term s (PAsData PInteger)}
  | PUnregisterToProveNotRegistered
      {punregisterToProve'registrationCertificateIndex :: Term s (PAsData PInteger)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPublishRedeemer)

{- | Aiken @witness.validate_witness_redeemer@.

Checks that the redeemer at @witness_redeemer_index@ publishes a certificate
registering (or unregistering) exactly @expected_witness_script_hash@, and that
the redeemer supplied with it is @MintOrBurn@ naming this event policy.

Both halves matter. The certificate proves the right credential is being moved;
the redeemer equality stops the witness script being spent through one of its
other endpoints — or on behalf of a different event policy — while this script
counts it as the registration.

Aiken's @Publish { at, certificate }@ is Plutus V3's @Certifying@; the
certificate index is not checked, matching the original's explicit note.
-}
pvalidateWitnessRedeemer ::
  forall (s :: S).
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Bool ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pvalidateWitnessRedeemer
  expectedWitnessScriptHash
  witnessRedeemerIndex
  forRegistration
  eventPolicyId
  redeemers = P.do
    redeemerPair <- plet $ pelemAt # witnessRedeemerIndex # redeemers
    presentHash <-
      plet $ pmatch (pfromData (pfstBuiltin # redeemerPair)) $ \case
        PCertifying _ cert -> pcertifiedScriptHash forRegistration cert
        _ -> perror
    pand'List
      [ presentHash #== expectedWitnessScriptHash
      , pto (pfromData (psndBuiltin # redeemerPair))
          #== pforgetData (pdata (pcon (PMintOrBurn eventPolicyId)))
      ]

{- | The script hash a staking certificate registers or unregisters.

@forRegistration@ is a /Haskell/ 'Bool', so it picks the constructor at compile
time and each 'pmatch' below is left with a single explicit arm. Written as one
match over both certificate constructors, the two arms would be identical — the
shape that trips the branch-selection fault described in
'Midgard.Validators.ActiveOperators'.
-}
pcertifiedScriptHash ::
  forall (s :: S). Bool -> Term s PTxCert -> Term s (PAsData PScriptHash)
pcertifiedScriptHash True cert =
  pmatch cert $ \case
    PTxCertRegStaking credential _ -> pcredentialScriptHash credential
    _ -> perror
pcertifiedScriptHash False cert =
  pmatch cert $ \case
    PTxCertUnRegStaking credential _ -> pcredentialScriptHash credential
    _ -> perror

-- | The script hash of a credential; a key credential is a rejection.
pcredentialScriptHash ::
  forall (s :: S). Term s PCredential -> Term s (PAsData PScriptHash)
pcredentialScriptHash credential =
  pmatch credential $ \case
    PScriptCredential h -> h
    PPubKeyCredential _ -> perror

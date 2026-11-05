{- |
Module      : Midgard.HubOracle
Description : Plutarch port of @lib/midgard/hub-oracle.ak@.

The hub oracle is the protocol's address book: a single authenticated UTxO whose
inline datum names the minting policy and address of every other Midgard script.
Consumers read it as a reference input.
-}
module Midgard.HubOracle (
  passetName,
  PHubOracleDatum (..),
  pgetDatum,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  POutputDatum (..),
  PScriptHash,
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetAuthenticInputOf)

{- | Aiken @hub_oracle.asset_name@.

The token name of the NFT that authenticates the hub oracle UTxO.
-}
passetName :: forall (s :: S). Term s (PAsData PTokenName)
passetName = pdata (pcon (PTokenName (pconstant "MIDGARD_HUB_ORACLE")))

{- | Aiken @hub_oracle.Datum@.

Field order is load-bearing: it fixes the on-chain data encoding, and the Aiken
original reads the constructor's fields positionally. Do not reorder without
changing @lib/midgard/hub-oracle.ak@ in lockstep.

The twelve policy ids come first, then thirteen addresses, then the reserve
observer script hash — note that @reserve@ has an address but no policy id, and
@reserve_observer@ is a bare script hash, so the two runs are not parallel.
-}
data PHubOracleDatum (s :: S) = PHubOracleDatum
  { phubOracle'registeredOperators :: Term s (PAsData PCurrencySymbol)
  , phubOracle'activeOperators :: Term s (PAsData PCurrencySymbol)
  , phubOracle'retiredOperators :: Term s (PAsData PCurrencySymbol)
  , phubOracle'scheduler :: Term s (PAsData PCurrencySymbol)
  , phubOracle'stateQueue :: Term s (PAsData PCurrencySymbol)
  , phubOracle'fraudProofCatalogue :: Term s (PAsData PCurrencySymbol)
  , phubOracle'fraudProof :: Term s (PAsData PCurrencySymbol)
  , phubOracle'deposit :: Term s (PAsData PCurrencySymbol)
  , phubOracle'withdrawal :: Term s (PAsData PCurrencySymbol)
  , phubOracle'txOrder :: Term s (PAsData PCurrencySymbol)
  , phubOracle'settlement :: Term s (PAsData PCurrencySymbol)
  , phubOracle'payout :: Term s (PAsData PCurrencySymbol)
  , phubOracle'registeredOperatorsAddr :: Term s (PAsData PAddress)
  , phubOracle'activeOperatorsAddr :: Term s (PAsData PAddress)
  , phubOracle'retiredOperatorsAddr :: Term s (PAsData PAddress)
  , phubOracle'schedulerAddr :: Term s (PAsData PAddress)
  , phubOracle'stateQueueAddr :: Term s (PAsData PAddress)
  , phubOracle'fraudProofCatalogueAddr :: Term s (PAsData PAddress)
  , phubOracle'fraudProofAddr :: Term s (PAsData PAddress)
  , phubOracle'depositAddr :: Term s (PAsData PAddress)
  , phubOracle'withdrawalAddr :: Term s (PAsData PAddress)
  , phubOracle'txOrderAddr :: Term s (PAsData PAddress)
  , phubOracle'settlementAddr :: Term s (PAsData PAddress)
  , phubOracle'reserveAddr :: Term s (PAsData PAddress)
  , phubOracle'payoutAddr :: Term s (PAsData PAddress)
  , phubOracle'reserveObserver :: Term s (PAsData PScriptHash)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PHubOracleDatum)

{- | Aiken @hub_oracle.get_datum@.

Resolves the authentic hub oracle UTxO from @reference_inputs@ at the supplied
index and returns its inline datum.

The datum is a @Constr 0@ on the wire — Aiken's default encoding for a
single-constructor record, and what @builtin.un_constr_data@ in the original
reads. That is 'DeriveAsDataStruct', not 'DeriveAsDataRec': the latter encodes a
record as a bare @List@, which is right for Aiken types with a hand-written
@ToData@ (as @Neighbor@ has) but wrong here.

The Aiken original hand-destructures the constructor and unwraps each field with
@builtin.un_b_data@ rather than writing @expect datum: Datum = ...@. That is a
cost optimisation: it skips the full structural check that @expect@ would emit.
'DeriveAsDataRec' gives the same property here — fields are read positionally
out of the underlying @Data@ on demand, with no upfront validation — so the
'punsafeCoerce' below is the faithful equivalent, not a shortcut past a check
the original performs.

The authenticity guarantee comes entirely from 'pgetAuthenticInputOf': the UTxO
must sit at the hub oracle's own script address and carry exactly one non-Ada
asset, that asset being the hub oracle NFT.
-}
pgetDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PScriptHash
        :--> PInteger
        :--> PHubOracleDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs hubOracleScriptHash hubRefInputIndex -> P.do
    hubInput <-
      plet $
        pgetAuthenticInputOf
          # referenceInputs
          # hubOracleScriptHash
          # passetName
          # hubRefInputIndex
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData hubInput
    PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
    pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} ->
        pfromData (punsafeCoerce @(PAsData PHubOracleDatum) (pto poutputDatum'outputDatum))
      _ -> perror

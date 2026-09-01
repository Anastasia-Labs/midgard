{- |
Module      : Midgard.Validators.Deposit
Description : Partial Plutarch port of @validators/user-events/deposit.ak@.

The deposit event: L1 funds queued for L2. The minting policy authenticates a
new deposit UTxO and, on the way out, burns its NFT.

Both sides are ported. The @spend@ side moves the deposit's funds into the
reserve, and is gated on the referenced settlement having already absorbed this
deposit into its L2 ledger — proved by a counted membership proof against the
settlement's deposits root.
-}
module Midgard.Validators.Deposit (
  depositMintValidator,
  depositSpendValidator,
) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  POutputDatum (..),
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.SingularUtxoIndexer (poneToOne)
import Midgard.Common.Utils (pgetRedeemerAt)
import Midgard.Env qualified as Env
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (punsafeEventToKeyValuePair)
import Midgard.Settlement (PSettlementDatum (..), pvalidCountedMembership)
import Midgard.Settlement qualified as Settlement
import Midgard.TransitionTrace (PRootDomain (..))
import Midgard.UserEvents (PMintRedeemer (..), pvalidateMint)
import Midgard.UserEvents.Deposit (PDepositDatum (..), PSpendRedeemer (..))

{- | Aiken @validators/user-events/deposit.ak@ — @mint@.

Delegates to "Midgard.UserEvents".'Midgard.UserEvents.pvalidateMint', supplying
the hub oracle's @deposit_addr@ and the deposit-specific check on the produced
UTxO: it must carry exactly one NFT under this policy, named for the event's
nonce, and no more than @max_tokens_allowed_in_deposits@ other assets counting
Ada.

That ceiling is why the value is walked rather than queried. Aiken folds over
every policy in the deposit's value, treating the first entry under its own
policy as the authentication NFT and counting the token names of everything
else; the total must stay within the protocol parameter, so a single deposit
cannot make its L2 ledger entry unboundedly large.
-}
depositMintValidator ::
  forall (s :: S).
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
depositMintValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownPolicy <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'outputs
    , ptxInfo'referenceInputs
    , ptxInfo'mint
    , ptxInfo'redeemers
    , ptxInfo'validRange
    } <-
    pmatch pscriptContext'txInfo
  redeemer <-
    plet $
      pfromData (punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer))
  pif
    ( pvalidateMint
        hubOracle
        (\hubDatum -> pmatch hubDatum $ \(PHubOracleDatum {phubOracle'depositAddr}) -> phubOracle'depositAddr)
        redeemer
        ownPolicy
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pfromData ptxInfo'referenceInputs)
        ptxInfo'validRange
        (pfromData ptxInfo'mint)
        (pto (pto (pfromData ptxInfo'redeemers)))
        (pdepositEventValidator ownPolicy)
    )
    (pconstant ())
    perror

{- | The deposit-specific half of the mint check.

Aiken's @list.foldl2@ over the value's policy entries, carrying "have I seen a
valid authentication NFT" and "how many other token names have I counted".
-}
pdepositEventValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PData ->
  Term s PData ->
  Term s PBool
pdepositEventValidator ownPolicy nonce outputValueData _depositInfo = P.do
  outputValue <-
    plet $ pfromData (punsafeCoerce @(PAsData Value.PLedgerValue) outputValueData)
  entries' <- plet $ pto (pto outputValue)
  entries <- plet $ pto (pto entries')
  -- The NFT under this policy must be exactly (nonce, 1) and nothing else.
  nftIsValid <-
    plet $ pmatch (AssocMap.plookup # pfromData ownPolicy # entries') $ \case
      PNothing -> pconstant False
      PJust tokenMap ->
        pto (pto tokenMap) #== (psingleton # (ppairDataBuiltin # nonce # pdata 1))
  -- Every other policy contributes its token-name count; Ada counts as one.
  otherTokenCount <-
    plet $
      pfoldr
        # plam
          ( \entry acc ->
              pif
                (pfstBuiltin # entry #== ownPolicy)
                acc
                (acc + (plength #$ pto (pto (pfromData (psndBuiltin # entry)))))
          )
        # 0
        # entries
  pand'List
    [ nftIsValid
    , otherTokenCount #<= Env.pmaxTokensAllowedInDeposits
    ]

{- | Aiken @validators/user-events/deposit.ak@ — @spend@.

Moves a deposit's funds to the reserve. Six checks, in the original's order:

  1. the deposit's NFT is being burnt in this transaction, read out of this
     policy's own @BurnEventNFT@ mint redeemer;
  2. the produced output carries no reference script;
  3. its value is the input's minus that NFT;
  4. the referenced settlement's deposits root contains this deposit event;
  5. the output has no datum; and
  6. it goes to the reserve address named by the hub oracle.

Check 4 is what makes this safe to do at all: the funds may only leave once the
L2 ledger has already accounted for them. Note the @True@ passed for
@double_satisfaction_prevented@ — the original's reasoning is that the witness
staking script must be unregistered in the same transaction, which cannot happen
twice, so the burn is the uniqueness guarantee.
-}
depositSpendValidator ::
  forall (s :: S).
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
depositSpendValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownOutRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  ownDatum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum -> mDatum
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

  PSpendRedeemer
    { pdepositSpend'inputIndex
    , pdepositSpend'outputIndex
    , pdepositSpend'hubRefInputIndex
    , pdepositSpend'settlementRefInputIndex
    , pdepositSpend'mintRedeemerIndex
    , pdepositSpend'membershipProof
    , pdepositSpend'inclusionProofScriptWithdrawRedeemerIndex
    } <-
    pmatch (pfromData (punsafeCoerce @(PAsData PSpendRedeemer) (pto pscriptContext'redeemer)))

  PHubOracleDatum {phubOracle'deposit, phubOracle'settlement, phubOracle'reserveAddr} <-
    pmatch $
      Hub.pgetDatum
        # referenceInputs
        # hubOracle
        # pfromData pdepositSpend'hubRefInputIndex

  PSettlementDatum {psettlement'depositsRoot} <-
    pmatch $
      Settlement.pgetDatum
        # referenceInputs
        # phubOracle'settlement
        # pfromData pdepositSpend'settlementRefInputIndex

  -- The deposit's own datum, carried by the UTxO being spent.
  PDepositDatum {pdepositDatum'event} <-
    pmatch $
      pmatch ownDatum $ \case
        PDJust d -> pfromData (punsafeCoerce @(PAsData PDepositDatum) (pto (pfromData d)))
        PDNothing -> perror

  burnAssetName <-
    plet $
      pmatch
        ( pfromData
            ( punsafeCoerce @(PAsData PMintRedeemer)
                ( pto
                    ( pfromData
                        ( pgetRedeemerAt
                            # redeemerList
                            # pdata (pcon (PMinting phubOracle'deposit))
                            # pfromData pdepositSpend'mintRedeemerIndex
                        )
                    )
                )
            )
        )
        $ \case
          PBurnEventNFT {pburnEvent'nonceAssetName} -> pburnEvent'nonceAssetName
          _ -> perror

  let (depositId, depositInfo) =
        punsafeEventToKeyValuePair (pforgetData pdepositDatum'event)

  pif
    ( poneToOne
        (pfromData pdepositSpend'inputIndex)
        (pfromData pdepositSpend'outputIndex)
        ownOutRef
        inputs
        outputs
        -- Double satisfaction is prevented by the witness unregistration that
        -- the NFT burn forces; see the module note.
        (pconstant True)
        ( \input output -> P.do
            PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData input
            PTxOut {ptxOut'value = ownValue} <- pmatch ptxInInfo'resolved
            PTxOut
              { ptxOut'address = outAddress
              , ptxOut'value = outValue
              , ptxOut'datum = outDatum
              , ptxOut'referenceScript = outRefScript
              } <-
              pmatch output
            pand'List
              [ pmatch outRefScript $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , -- Aiken writes @output_value == own_value - NFT@. Rearranged to
                -- @output_value + NFT == own_value@ because Plutarch's value
                -- union does not drop the zero entry that subtraction leaves
                -- behind, while Aiken's @assets.add@ does. Equivalent on
                -- well-formed values, and it avoids depending on normalisation.
                ( pto (pfromData outValue)
                    <> ( Value.psingletonSortedValue
                          # pfromData phubOracle'deposit
                          # pfromData burnAssetName
                          # 1
                       )
                )
                  #== pto (pfromData ownValue)
              , pvalidCountedMembership
                  (pdata (pcon PDepositsRootDomain))
                  (pfromData psettlement'depositsRoot)
                  (pfromData pdepositSpend'membershipProof)
                  depositId
                  depositInfo
                  redeemerList
              , pmatch outDatum $ \case
                  PNoOutputDatum -> pconstant True
                  _ -> pconstant False
              , outAddress #== pfromData phubOracle'reserveAddr
              ]
        )
    )
    (pconstant ())
    perror

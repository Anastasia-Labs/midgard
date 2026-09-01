{- |
Module      : Midgard.Validators.FieldPreimageCertificate
Description : Plutarch port of @validators/field-preimage-certificate.ak@.

The permissionless field-preimage chunk-certificate validator — @docs/spec/midgard-tx.md@
§8.4 through §8.7.

Tier 3 of the carriage ladder is the only tier that needs on-chain certification,
and it needs it for one reason: a flat field commitment authenticates the whole
preimage and nothing smaller. Once a preimage is split across publications there
is no other way to verify an individual chunk before reconstruction, so the
certificate supplies that binding — a mint-verified @total_length@ and per-chunk
digest vector a consuming step can check one chunk against.

__One script, two handlers (§8.6).__ Aiken's @mint@ and @spend@ are handlers of
one validator, so the policy id and the spend credential are one script hash.
Plutarch has no handler syntax, so the two arrive here as two terms —
'fieldPreimageCertificateMintValidator' and 'fieldPreimageCertificateSpendValidator'
— compiled to one script by the deployment layer, exactly as every other
two-handler port in this tree. Each reads its own purpose out of the script info
and errors on any other, which is Aiken's @else(_) { fail }@ applied per handler.

__Nothing here is privileged (§8.7).__ The policy checks content and never
identity — no operator role, no signature at mint, no allowlist of publishers.
That is what makes a failed or malicious publication healable: because the §8.4
split is a pure function of the preimage bytes, an unrelated party who obtains
the same preimage republishes byte-identical chunks and certifies from them, and
the certificate they get is interchangeable with the one that was yanked.

__What this validator does not decide.__ It never looks at a game, a thread, a
block, or a step. A certificate is a statement about one L2 transaction's one
field, good indefinitely and for every dispute over that transaction; who may use
it is the consuming step's question, answered by
'Midgard.NativeTxFieldAccess.pauthenticatedFieldView' against the thread's
already-authenticated transaction.

The content rules live in "Midgard.NativeTxCarriage" so a focused test can drive
them without building a transaction; this module is the transaction-shape wrapper
— which output, which token, which burn.

=== The one divergence

Aiken's @expect certificate: FieldPreimageCertificateV1 = certificate_data@ is a
structural check of the datum against the type. The port coerces instead, as
every datum read in this tree does, and lets the fields refuse: the owner's
length is checked, the tx-id is hashed as bytes, the field index is bounded by
the asset-name derivation, and the digest vector is compared against digests
computed from the chunks. A datum that is not a certificate fails at the first of
those rather than at the coercion — the same refusal, one step later.
-}
module Midgard.Validators.FieldPreimageCertificate (
  fieldPreimageCertificateMintValidator,
  fieldPreimageCertificateSpendValidator,
) where

import Data.Kind (Type)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  POutputDatum (..),
  PScriptContext (..),
  PScriptInfo (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Value (PSortedValue, pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (phasSigned)
import Midgard.NativeTxCarriage (
  PFieldPreimageCertificateMintRedeemerV1 (..),
  prawChunkBytes,
  pverifyFieldPreimageCertificateV1,
 )
import Midgard.NativeTxFieldAccess (
  PFieldPreimageCertificateV1 (..),
  pfieldPreimageCertificateAssetName,
  pmaxTier3ChunkCount,
 )

--------------------------------------------------------------------------------
-- The minting handler
--------------------------------------------------------------------------------

{- | Aiken @validators/field-preimage-certificate.ak@ — @mint@.

Two redeemers with opposite jobs. @Certify@ proves content and brings exactly one
token into existence; @Retire@ proves nothing and may only take tokens out of it.

The parameterless signature is the point of §8.7: there is no operator, no hub
oracle and no deployment-time argument, so the policy id is a function of the
compiled code alone and any two deployments of it are the same policy.
-}
fieldPreimageCertificateMintValidator ::
  forall (s :: S).
  Term s (PScriptContext :--> PUnit)
fieldPreimageCertificateMintValidator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  -- `else(_) { fail }`: no purpose but `mint` reaches the dispatch.
  ownPolicy <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript policy -> policy
      _ -> perror
  PTxInfo {ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  -- Aiken's `tx.mint |> assets.tokens(own_policy_id) |> dict.to_pairs`, read
  -- once because both branches want it.
  mintedTokens <- plet $ ppolicyTokenPairs (pto (pfromData ptxInfo'mint)) ownPolicy
  redeemer <-
    plet $
      pfromData $
        punsafeCoerce @(PAsData PFieldPreimageCertificateMintRedeemerV1) (pto pscriptContext'redeemer)
  pif
    ( pmatch redeemer $ \case
        PCertify
          { pcertify'compactCbor
          , pcertify'witnessSetCompactCbor
          , pcertify'chunkRefInputIndices
          , pcertify'outputIndex
          } ->
            pcertify
              ownPolicy
              (pfromData ptxInfo'outputs)
              (pfromData ptxInfo'referenceInputs)
              mintedTokens
              (pfromData pcertify'compactCbor)
              (pfromData pcertify'witnessSetCompactCbor)
              (pfromData pcertify'chunkRefInputIndices)
              (pfromData pcertify'outputIndex)
        -- Burning. The owner's authority is checked by the `spend` handler that
        -- releases the token's UTxO; all this branch owes is that a burn
        -- redeemer cannot mint. Every quantity of this policy in the
        -- transaction must be negative, so `Retire` can never bring a
        -- certificate into existence without the content proof above.
        PRetire ->
          pall # plam (\entry -> pfromData (ppairQuantity entry) #< 0) # mintedTokens
    )
    (pconstant ())
    perror

{- | Aiken's @Certify@ branch.

The numbered steps are the Aiken source's, in its order, and the order is part of
the meaning: the output has to exist and be at this script's own address before
anything is read out of it, and the token has to be the certificate's own content
address before a single chunk is resolved.

Everything here refuses by aborting, because every step of the original is an
@expect@ — every step but the last, which is
'pverifyFieldPreimageCertificateV1' returning a boolean the caller turns into a
refusal. Both reach the same verdict; the difference is only visible to a test
that distinguishes them, and the tests follow the original.
-}
pcertify ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PTokenQtyList ->
  Term s PByteString ->
  Term s PByteString ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger ->
  Term s PBool
pcertify
  ownPolicy
  outputs
  referenceInputs
  mintedTokens
  compactCbor
  witnessSetCompactCbor
  chunkRefInputIndices
  outputIndex = P.do
    -- 1. The certificate output is named positionally, so the index has to
    --    resolve to a real output. Aiken's `list.at` is total — it answers
    --    `None` past the end and for a negative index — and both ends are then
    --    refused by the `expect` around it. `pelemAt` errors past the end
    --    directly, which is the same refusal; the negative end still needs its
    --    own guard.
    pif (0 #<= outputIndex) `flip` perror $ P.do
      PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
        pmatch (pfromData (pelemAt # outputIndex # outputs))
      PAddress {paddress'credential, paddress'stakingCredential} <- pmatch ptxOut'address
      -- 2. §8.5: the certificate is authenticated data, so it lands at this
      --    script's own address — the same hash as the policy that just proved
      --    it.
      pif (pisOwnScriptCredential paddress'credential ownPolicy) `flip` perror $
        -- 3. And at exactly one shape of that address. Nothing reads the stake
        --    credential, so pinning it costs nothing and buys two things: the
        --    minter cannot point the deposit's staking rights at a stranger's
        --    reward account on the way past, and an indexer looking for
        --    certificates has one address to enumerate rather than a family of
        --    them sharing a payment credential.
        pif (pisNothingData paddress'stakingCredential) `flip` perror $
          -- 4. No reference script: no consumer reads one, and refusing it
          --    keeps the output's shape exactly what §8.6 describes.
          pif (pisNothingData ptxOut'referenceScript) `flip` perror $ P.do
            -- 5. An inline datum, because a consuming step reads the manifest
            --    out of a reference input and a datum hash would make that
            --    impossible.
            certificate <-
              plet $ pmatch ptxOut'datum $ \case
                POutputDatum {poutputDatum'outputDatum} ->
                  pfromData $
                    punsafeCoerce @(PAsData PFieldPreimageCertificateV1) (pto poutputDatum'outputDatum)
                _ -> perror
            PFieldPreimageCertificateV1 {pcert'txId, pcert'fieldIndex} <- pmatch certificate
            -- 6. §8.6's deterministic name, derived from the certificate's own
            --    `(tx_id, field_index)` rather than from anything the redeemer
            --    says. The derivation is what bounds `field_index` to 0..8 and
            --    `tx_id` to 32 bytes, so an out-of-range certificate fails here
            --    before its content is ever looked at.
            assetName <-
              plet $
                pfieldPreimageCertificateAssetName
                  # pfromData pcert'txId
                  # pfromData pcert'fieldIndex
            -- 7. Exactly one token of this policy in the whole transaction, and
            --    it is the one the certificate names. Without the single-pair
            --    check a second, unexamined asset name of this policy could
            --    ride along on a valid certification and land wherever its
            --    minter liked. One pair also means one certificate per
            --    transaction and no batching — the deliberate trade the Aiken
            --    source argues at length.
            ponlyEntry mintedTokens $ \minted ->
              pif (ptokenNameIs (ppairName minted) assetName) `flip` perror $
                pif (pfromData (ppairQuantity minted) #== 1) `flip` perror $
                  -- 8. And the certificate output carries that token and nothing
                  --    else of this policy. Stated as the whole per-policy pair
                  --    list rather than as a quantity of the one name: a
                  --    quantity says nothing about the names it was not asked
                  --    about, so a second name of this policy arriving from an
                  --    input could ride into the proved output and be read there
                  --    by anything that trusts "a token of the certificate
                  --    policy" without also checking which one.
                  ponlyEntry (ppolicyTokenPairs (pto (pfromData ptxOut'value)) ownPolicy) $ \carried ->
                    pif (ptokenNameIs (ppairName carried) assetName) `flip` perror $
                      pif (pfromData (ppairQuantity carried) #== 1) `flip` perror $
                        -- 9. §8.3 bounds the ladder at three chunks; refusing a
                        --    longer index list up front keeps a redeemer from
                        --    making the policy resolve reference inputs it could
                        --    never legally need.
                        pif (plength # chunkRefInputIndices #<= pmaxTier3ChunkCount) `flip` perror $
                          -- 10. Everything §8.6 requires of the certificate's
                          --     content — the tx-id binding, the split shape,
                          --     the digest vector, the reconstruction against
                          --     the positionally-extracted field commitment.
                          pverifyFieldPreimageCertificateV1
                            # certificate
                            # compactCbor
                            # witnessSetCompactCbor
                            # ( pmap
                                  # plam (\index -> pdata (prawChunkBytes # referenceInputs # pfromData index))
                                  # chunkRefInputIndices
                              )

--------------------------------------------------------------------------------
-- The spending handler
--------------------------------------------------------------------------------

{- | Aiken @validators/field-preimage-certificate.ak@ — @spend@.

Min-Ada reclaim (§8.5/§8.7). Cleanup is owner-discretionary: no time-lock, no
forced cleanup, and a mid-game yank is self-healing, so the only question here is
whether the owner authorised it and whether the token stops existing.
-}
fieldPreimageCertificateSpendValidator ::
  forall (s :: S).
  Term s (PScriptContext :--> PUnit)
fieldPreimageCertificateSpendValidator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  ownRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  ownDatum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum -> mDatum
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'signatories} <- pmatch pscriptContext'txInfo
  -- 1. A datumless UTxO at this address is not a certificate — nothing names the
  --    asset to burn or the owner who may authorise it — so there is nothing
  --    this handler could check and it refuses instead of guessing.
  certificate <-
    plet $ pmatch ownDatum $ \case
      PDJust d ->
        pfromData $
          punsafeCoerce @(PAsData PFieldPreimageCertificateV1) (pto (pfromData d))
      PDNothing -> perror
  -- 2. The spent UTxO is resolved from the ledger's own `own_ref` rather than
  --    from a redeemer index, so the address checked below is the one actually
  --    being consumed.
  ownInput <-
    plet $
      pmatch
        ( pfind
            # plam
              ( \input ->
                  pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef} ->
                    ptxInInfo'outRef #== ownRef
              )
            # pfromData ptxInfo'inputs
        )
        $ \case
          PJust input -> pfromData input
          PNothing -> perror
  -- 3. Mint and spend are handlers of one validator, so the script hash guarding
  --    this UTxO *is* the certificate policy id.
  ownPolicy <-
    plet $ pmatch ownInput $ \PTxInInfo {ptxInInfo'resolved} ->
      pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'address} ->
        pmatch ptxOut'address $ \PAddress {paddress'credential} ->
          pmatch paddress'credential $ \case
            PScriptCredential scriptHash -> punsafeCoerce @PCurrencySymbol (pfromData scriptHash)
            _ -> perror
  PFieldPreimageCertificateV1 {pcert'owner, pcert'txId, pcert'fieldIndex} <- pmatch certificate
  -- 4. The same §8.6 derivation the mint handler used, over the datum this UTxO
  --    actually carries.
  assetName <-
    plet $
      punsafeCoerce @PTokenName $
        pfieldPreimageCertificateAssetName # pfromData pcert'txId # pfromData pcert'fieldIndex
  -- 5. The token must not survive the transaction. Stated over outputs rather
  --    than over `tx.mint`, because that is the form that cannot be
  --    double-satisfied: a burn count is a sum over the whole transaction, so
  --    two certificates of the same name spent together could each point at the
  --    same single `-1` and let one token survive — and a surviving token is a
  --    certificate whose datum the minting policy never saw, i.e. a forgery.
  --    "No output carries it" is per-token and order-independent: if every input
  --    holding the name is consumed and no output holds it, the ledger's own
  --    value balance says all of them were burnt.
  pif
    ( pall
        # plam
          ( \output ->
              pmatch (pfromData output) $ \PTxOut {ptxOut'value} ->
                pvalueOf # pto (pfromData ptxOut'value) # ownPolicy # assetName #== 0
          )
        # pfromData ptxInfo'outputs
    )
    `flip` perror
    $ pif
      (phasSigned # pcert'owner # pfromData ptxInfo'signatories)
      (pconstant ())
      perror

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | The token-name/quantity pair list inside one currency-symbol entry.
type PTokenQtyList = PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))

{- | Aiken @assets.tokens(value, policy) |> dict.to_pairs@.

A policy with no entry answers the empty list rather than aborting, exactly as
the Aiken pipeline does — every caller here refuses an empty list anyway, and it
refuses it for the reason it states rather than for a missing key.
-}
ppolicyTokenPairs ::
  forall (s :: S).
  Term s PSortedValue ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PTokenQtyList
ppolicyTokenPairs value policy =
  pmatch (AssocMap.plookup # pfromData policy # pto value) $ \case
    PNothing -> pnil
    PJust tokens -> pto (pto tokens)

-- | Aiken @expect [entry] = …@ — one entry and no more, aborting otherwise.
ponlyEntry ::
  forall (s :: S) (a :: S -> Type) (r :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PBuiltinList a) ->
  (Term s a -> Term s r) ->
  Term s r
ponlyEntry entries k =
  pelimList (\entry rest -> pif (pnull # rest) (k entry) perror) perror entries

-- | Whether this credential is the script whose hash is @policy@.
pisOwnScriptCredential ::
  forall (s :: S).
  Term s PCredential ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PBool
pisOwnScriptCredential credential policy =
  pmatch credential $ \case
    PScriptCredential scriptHash -> pto (pfromData scriptHash) #== pto (pfromData policy)
    _ -> pconstant False

-- | Aiken's @== None@ over the ledger's data-encoded optional.
pisNothingData ::
  forall (s :: S) (a :: S -> Type).
  Term s (PMaybeData a) ->
  Term s PBool
pisNothingData maybeData =
  pmatch maybeData $ \case
    PDNothing -> pconstant True
    PDJust _ -> pconstant False

-- | The asset name of a token-map entry.
ppairName ::
  forall (s :: S).
  Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
  Term s (PAsData PTokenName)
ppairName entry = pmatch entry $ \(PBuiltinPair name _) -> name

-- | The quantity of a token-map entry.
ppairQuantity ::
  forall (s :: S).
  Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
  Term s (PAsData PInteger)
ppairQuantity entry = pmatch entry $ \(PBuiltinPair _ quantity) -> quantity

-- | Whether a token-name key holds exactly these bytes.
ptokenNameIs ::
  forall (s :: S).
  Term s (PAsData PTokenName) ->
  Term s PByteString ->
  Term s PBool
ptokenNameIs name bytes = pto (pfromData name) #== bytes

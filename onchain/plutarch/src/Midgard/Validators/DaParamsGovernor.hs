{- |
Module      : Midgard.Validators.DaParamsGovernor
Description : Plutarch port of @validators/da-params-governor.ak@.

Governance over the data-availability parameters: which committee may attest,
how many of them a valid attestation needs, and which owners may change either.

Almost all of this validator is one pure predicate — 'pvalidDatum' — and the
two handlers are thin shells around it. That shape is the point: the same
invariant is applied to the datum being created, the datum being spent, and the
datum being produced, so a parameter set that could not have been minted also
cannot be arrived at by update.

/The floor is what makes single-key capture unrepresentable./ Both thresholds
must be at least @max(2, ceil(2 * set_len / 3))@. The clamp at two stops a
one-signature quorum outright; the two-thirds term stops a quorum voting itself
down to a minority slice of its own set. Neither threshold may exceed its set,
so the parameters are always satisfiable.

/Sets are proved sorted and unique as they are measured./ The committee is a
packed bytestring of 32-byte keys and the owners a list of 28-byte hashes; both
are walked strictly ascending, which rules out duplicates without a second pass.
A duplicate would otherwise let one key count twice towards a threshold.
-}
module Midgard.Validators.DaParamsGovernor (
  daParamsGovernorMintValidator,
  daParamsGovernorSpendValidator,
  pvalidDatum,
  pgovernedThresholdFloor,
  psortedUniquePackedLenAtMost,
  psortedUniqueLenAtMost,
  pownerQuorumMet,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  PLedgerValue,
  POutputDatum (..),
  PPubKeyHash,
  PScriptContext (..),
  PScriptInfo (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.DaAttestation (
  PDaParamsDatum (..),
  pdaParamsAssetName,
  pmaxIndexedSignerCount,
  pverificationKeyByteCount,
 )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Aiken @verification_key_hash_byte_count@ — Blake2b-224, as everywhere else.
pverificationKeyHashByteCount :: forall (s :: S). Term s PInteger
pverificationKeyHashByteCount = 28

{- | Aiken @min_governed_threshold@.

The lower clamp of the governed floor. Two, and this is the number that makes
single-key capture of either threshold unrepresentable.
-}
pminGovernedThreshold :: forall (s :: S). Term s PInteger
pminGovernedThreshold = 2

{- | Aiken @min_owner_count@.

The smallest owner set the governor represents.

The Aiken source is careful to say this is /redundant/ under the current floor —
a one-owner set is already unrepresentable, because its @update_threshold@ would
have to be both at least @governed_threshold_floor(1) == 2@ and at most one — and
that the redundancy is an artifact of the arithmetic rather than a property of
the governor. It is kept, here as there, as declared defence in depth: if the
two-thirds term were ever weakened to something whose value at one is one, a
single-owner set with @update_threshold == 1@ would become representable and
single-key governance capture would return.

It stays a separate constant from 'pminGovernedThreshold' even though both are
two: one bounds a set size and the other a threshold, and a change to either
must not silently move the other.
-}
pminOwnerCount :: forall (s :: S). Term s PInteger
pminOwnerCount = 2

{- | Aiken @governed_threshold_floor@ — @max(2, ceil(2 * set_len / 3))@.

@ceil(2n/3)@ is written @(2n + 2) / 3@ under integer division.
-}
pgovernedThresholdFloor :: forall (s :: S). Term s (PInteger :--> PInteger)
pgovernedThresholdFloor = phoistAcyclic $
  plam $ \setLen ->
    plet (pdiv # (2 * setLen + 2) # 3) $ \twoThirdsCeiling ->
      pif
        (pminGovernedThreshold #< twoThirdsCeiling)
        twoThirdsCeiling
        pminGovernedThreshold

--------------------------------------------------------------------------------
-- Measuring the two sets
--------------------------------------------------------------------------------

{- | Aiken @sorted_unique_packed_len_at_most@.

Walks a packed bytestring of fixed-width keys, returning how many there are, and
failing unless every key is strictly greater than the one before it and the
count stays within @max@.

Strict ascent does double duty: it is the sort order /and/ the uniqueness proof,
since equal adjacent keys are not strictly increasing. A duplicate committee key
would otherwise count twice towards @da_threshold@, so one signer could satisfy
a threshold of two.
-}
psortedUniquePackedLenAtMost ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PInteger)
psortedUniquePackedLenAtMost = phoistAcyclic $
  plam $ \keys maxLen byteCount -> P.do
    keysLen <- plet $ plengthBS # keys
    pif
      (maxLen #<= 0 #|| keysLen #< byteCount)
      perror
      $ plet (psliceBS # 0 # byteCount # keys)
      $ \first ->
        pgo # keys # byteCount # first # 1 # maxLen # byteCount # keysLen
  where
    pgo ::
      forall (s' :: S).
      Term
        s'
        ( PByteString
            :--> PInteger
            :--> PByteString
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
        )
    pgo = phoistAcyclic $
      pfix $ \self ->
        plam $ \keys offset prev len maxLen byteCount keysLen ->
          pif
            (offset #== keysLen)
            len
            ( pif
                (keysLen #< offset + byteCount)
                perror
                $ plet (psliceBS # offset # byteCount # keys)
                $ \key ->
                  plet (len + 1) $ \nextLen ->
                    pif
                      (pnot # (prev #< key) #|| maxLen #< nextLen)
                      perror
                      (self # keys # (offset + byteCount) # key # nextLen # maxLen # byteCount # keysLen)
            )

{- | Aiken @sorted_unique_len_at_most@.

The same walk over a list of keys rather than a packed bytestring, with each
key's width checked too. Owners are held as a list because governance is
authorised by transaction signatories, which are hashes, not by positional
index into a committee.
-}
psortedUniqueLenAtMost ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PPubKeyHash)
        :--> PInteger
        :--> PInteger
        :--> PInteger
    )
psortedUniqueLenAtMost = phoistAcyclic $
  plam $ \keys maxLen byteCount ->
    pelimList
      ( \first rest ->
          plet (pto (pfromData first)) $ \firstBytes ->
            pif
              (pnot # (plengthBS # firstBytes #== byteCount))
              perror
              (pgo # firstBytes # rest # 1 # maxLen # byteCount)
      )
      perror
      keys
  where
    pgo ::
      forall (s' :: S).
      Term
        s'
        ( PByteString
            :--> PBuiltinList (PAsData PPubKeyHash)
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
        )
    pgo = phoistAcyclic $
      pfix $ \self ->
        plam $ \prev keys len maxLen byteCount ->
          pelimList
            ( \key rest ->
                plet (pto (pfromData key)) $ \keyBytes ->
                  plet (len + 1) $ \nextLen ->
                    pif
                      ( pand'List
                          [ plengthBS # keyBytes #== byteCount
                          , prev #< keyBytes
                          , nextLen #<= maxLen
                          ]
                      )
                      (self # keyBytes # rest # nextLen # maxLen # byteCount)
                      perror
            )
            len
            keys

--------------------------------------------------------------------------------
-- The invariant
--------------------------------------------------------------------------------

{- | Aiken @valid_datum@.

Everything a governed parameter set must satisfy, applied identically wherever
one appears. Its conditions, in order:

  * the bounds themselves are sane — a positive committee cap no larger than the
    256 the bitmap encoding admits, and a positive owner cap;
  * the committee is sorted, unique and within its cap;
  * @committee_signers_hash@ is the hash of the committee bytes, which is what
    lets an attestation pin the committee it was made under without carrying it;
  * @da_threshold@ sits between the governed floor and the committee size;
  * the owners are sorted, unique, within their cap and at least two; and
  * @update_threshold@ sits between the governed floor and the owner count.

The upper bounds matter as much as the floors: a threshold above its set size
would be unsatisfiable, freezing attestation or governance permanently.
-}
pvalidDatum ::
  forall (s :: S).
  Term s (PDaParamsDatum :--> PInteger :--> PInteger :--> PBool)
pvalidDatum = phoistAcyclic $
  plam $ \datum maxCommitteeSize maxOwnerCount -> pmatch datum $
    \PDaParamsDatum
      { pdaParams'committee
      , pdaParams'committeeSignersHash
      , pdaParams'daThreshold
      , pdaParams'owners
      , pdaParams'updateThreshold
      } -> P.do
        _ <-
          plet $
            pif
              ( pand'List
                  [ 0 #< maxCommitteeSize
                  , maxCommitteeSize #<= pmaxIndexedSignerCount
                  , 0 #< maxOwnerCount
                  ]
              )
              (pconstant @PUnit ())
              perror
        committee <- plet $ pfromData pdaParams'committee
        committeeLen <-
          plet $
            psortedUniquePackedLenAtMost
              # committee
              # maxCommitteeSize
              # pverificationKeyByteCount
        ownerLen <-
          plet $
            psortedUniqueLenAtMost
              # pfromData pdaParams'owners
              # maxOwnerCount
              # pverificationKeyHashByteCount
        daThreshold <- plet $ pfromData pdaParams'daThreshold
        updateThreshold <- plet $ pfromData pdaParams'updateThreshold
        -- Aiken writes every one of these as an `expect`, so `valid_datum`
        -- either returns True or fails; it never returns False. Both handlers
        -- call it under an `expect` too, so the two shapes agree at the
        -- validator level — but the exported predicate must reject the same way
        -- the original does, or a future caller reading it as a boolean would
        -- silently get a different contract.
        pif
          ( pand'List
              [ pfromData pdaParams'committeeSignersHash #== (pblake2b_256 # committee)
              , (pgovernedThresholdFloor # committeeLen) #<= daThreshold
              , daThreshold #<= committeeLen
              , pminOwnerCount #<= ownerLen
              , (pgovernedThresholdFloor # ownerLen) #<= updateThreshold
              , updateThreshold #<= ownerLen
              ]
          )
          (pconstant True)
          perror

{- | Aiken @owner_quorum_met@.

Counts how many owners signed, stopping as soon as the threshold is reached.
Counting over the /owners/ rather than the signatories is what makes a
non-owner's signature worthless, and the sorted-unique invariant on the owner
list is what stops one signature being counted twice.
-}
pownerQuorumMet ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PPubKeyHash)
        :--> PBuiltinList (PAsData PPubKeyHash)
        :--> PInteger
        :--> PBool
    )
pownerQuorumMet = phoistAcyclic $
  pfix $ \self ->
    plam $ \owners signers threshold ->
      pif
        (threshold #<= 0)
        (pconstant True)
        ( pelimList
            ( \owner rest ->
                pif
                  (pelem # owner # signers)
                  (self # rest # signers # (threshold - 1))
                  (self # rest # signers # threshold)
            )
            (pconstant False)
            owners
        )

--------------------------------------------------------------------------------
-- Transaction-shape helpers
--------------------------------------------------------------------------------

{- | Aiken @only_output_with_nft@.

The single output carrying the params NFT. Two such outputs is a failure rather
than a choice — otherwise an update could produce a well-formed decoy alongside
the real continuation and let a consumer read whichever it found first.
-}
ponlyOutputWithNft ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PTxOut
ponlyOutputWithNft outputs policyId =
  pmatch
    ( pfoldr
        # plam
          ( \out found ->
              plet (pmatch (pfromData out) $ \PTxOut {ptxOut'value} -> pquantityOf ptxOut'value policyId pdaParamsAssetName) $
                \quantity ->
                  pif
                    (quantity #== 0)
                    found
                    ( pif
                        (pnot # (quantity #== 1))
                        perror
                        ( pmatch found $ \case
                            PNothing -> pcon (PJust (pfromData out))
                            PJust _ -> perror
                        )
                    )
          )
        # pcon PNothing
        # outputs
    )
    $ \case
      PNothing -> perror
      PJust output -> output

-- | Aiken @assets.quantity_of@; zero when the pair is absent.
pquantityOf ::
  forall (s :: S).
  Term s (PAsData PLedgerValue) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PInteger
pquantityOf value policyId tokenName =
  pmatch (AssocMap.plookup # pfromData policyId # pto (pto (pfromData value))) $ \case
    PNothing -> 0
    PJust tokenMap ->
      pmatch (AssocMap.plookup # pfromData tokenName # tokenMap) $ \case
        PNothing -> 0
        PJust quantity -> quantity

{- | Aiken @params_output_datum_at_script@ / @..._at_address@, merged.

Reads the continuation's inline datum and applies 'pvalidDatum' to it. The
address check differs between the two callers — the mint pins the /script hash/
because there is no input to copy from, the spend pins the input's whole address
so a continuation cannot quietly change its staking part — so it is passed in.
-}
pparamsOutputDatum ::
  forall (s :: S).
  Term s PTxOut ->
  (Term s PAddress -> Term s PBool) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PDaParamsDatum
pparamsOutputDatum output addressOk maxCommitteeSize maxOwnerCount = P.do
  PTxOut {ptxOut'address, ptxOut'datum, ptxOut'referenceScript} <- pmatch output
  datumData <-
    plet $ pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
      _ -> perror
  datum <- plet $ pfromData (punsafeCoerce @(PAsData PDaParamsDatum) datumData)
  pif
    ( pand'List
        [ addressOk ptxOut'address
        , pmatch ptxOut'referenceScript $ \case
            PDNothing -> pconstant True
            PDJust _ -> pconstant False
        , pvalidDatum # datum # maxCommitteeSize # maxOwnerCount
        ]
    )
    datum
    perror

{- | Aiken @value_not_drained@.

Every asset in the input must reappear in the output at no lower a quantity.
Cleanup is not a governance action, so an update must not be a way to walk the
UTxO's Ada — or anything else — out of the script.
-}
pvalueNotDrained ::
  forall (s :: S).
  Term s (PAsData PLedgerValue) ->
  Term s (PAsData PLedgerValue) ->
  Term s PBool
pvalueNotDrained inputValue outputValue =
  pall
    # plam
      ( \policyEntry ->
          pall
            # plam
              ( \tokenEntry ->
                  pfromData (psndBuiltin # tokenEntry)
                    #<= pquantityOf outputValue (pfstBuiltin # policyEntry) (pfstBuiltin # tokenEntry)
              )
            # pto (pto (pfromData (psndBuiltin # policyEntry)))
      )
    # pto (pto (pto (pto (pfromData inputValue))))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

{- | Aiken @validators/da-params-governor.ak@ — @mint@.

Creates the parameters. Three parameters fix the deployment: the outref that
must be consumed, and the two caps.

@init_ref@ is what makes this one-shot. The governor has no @Deinit@ and no
authority check at mint, so without a specific UTxO being consumed anyone could
mint a second parameter set at any time and the protocol would have two answers
to "who may attest".
-}
daParamsGovernorMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PTxOutRef -- init ref
        :--> PInteger -- max committee size
        :--> PInteger -- max owner count
        :--> PScriptContext
        :--> PUnit
    )
daParamsGovernorMintValidator = plam $
  \initRef maxCommitteeSize maxOwnerCount ctx -> P.do
    PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    policyId <-
      plet $ pmatch pscriptContext'scriptInfo $ \case
        PMintingScript cs -> cs
        _ -> perror
    PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
    mintEntry <-
      plet $
        pmatch (AssocMap.plookup # pfromData policyId # pto (pto (pfromData ptxInfo'mint))) $ \case
          PNothing -> perror
          PJust tokenMap ->
            plet (pto (pto tokenMap)) $ \entries ->
              pif (pnull # (ptail # entries)) (phead # entries) perror
    _ <-
      plet $
        pparamsOutputDatum
          (ponlyOutputWithNft (pfromData ptxInfo'outputs) policyId)
          ( \address ->
              pmatch address $ \PAddress {paddress'credential} ->
                pmatch paddress'credential $ \case
                  PScriptCredential h -> pto (pfromData h) #== pto (pfromData policyId)
                  PPubKeyCredential _ -> pconstant False
          )
          maxCommitteeSize
          maxOwnerCount
    pif
      ( pand'List
          [ pfstBuiltin # mintEntry #== pdaParamsAssetName
          , pfromData (psndBuiltin # mintEntry) #== 1
          , pany
              # plam
                ( \input ->
                    pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef} ->
                      pdata ptxInInfo'outRef #== initRef
                )
              # pfromData ptxInfo'inputs
          ]
      )
      (pconstant ())
      perror

{- | Aiken @validators/da-params-governor.ak@ — @spend@.

Updates the parameters. The owners' quorum authorises the change, and the same
'pvalidDatum' invariant constrains what they may change it /to/ — a quorum
cannot vote the protocol into a state the mint would have refused to create.

The input's own datum is re-validated as well as the output's. That is not
redundant: it means a parameter set that somehow became invalid is frozen rather
than being a base to update from, so the invariant cannot be escaped by first
reaching a bad state and then stepping out of it.
-}
daParamsGovernorSpendValidator ::
  forall (s :: S).
  Term
    s
    ( PInteger -- max committee size
        :--> PInteger -- max owner count
        :--> PScriptContext
        :--> PUnit
    )
daParamsGovernorSpendValidator = plam $ \maxCommitteeSize maxOwnerCount ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  ownRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  inputDatum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust d -> pfromData (punsafeCoerce @(PAsData PDaParamsDatum) (pto (pfromData d)))
          PDNothing -> perror
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'signatories} <- pmatch pscriptContext'txInfo

  ownInput <-
    plet $
      pmatch
        ( pfoldr
            # plam
              ( \input found ->
                  pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} ->
                    pif (ptxInInfo'outRef #== ownRef) (pcon (PJust ptxInInfo'resolved)) found
              )
            # pcon PNothing
            # pfromData ptxInfo'inputs
        )
        $ \case
          PNothing -> perror
          PJust resolved -> resolved
  PTxOut
    { ptxOut'address = inputAddress
    , ptxOut'value = inputValue
    , ptxOut'datum = inputOutputDatum
    , ptxOut'referenceScript = inputRefScript
    } <-
    pmatch ownInput
  policyId <-
    plet $ pmatch inputAddress $ \PAddress {paddress'credential} ->
      pmatch paddress'credential $ \case
        PScriptCredential h -> punsafeCoerce h
        PPubKeyCredential _ -> perror
  continuedOutput <- plet $ ponlyOutputWithNft (pfromData ptxInfo'outputs) policyId
  _ <-
    plet $
      pparamsOutputDatum
        continuedOutput
        (\address -> pdata address #== pdata inputAddress)
        maxCommitteeSize
        maxOwnerCount
  PTxOut {ptxOut'value = outputValue} <- pmatch continuedOutput

  pif
    ( pand'List
        [ pvalidDatum # inputDatum # maxCommitteeSize # maxOwnerCount
        , -- The resolved input's datum must be the one the ledger handed us.
          pmatch inputOutputDatum $ \case
            POutputDatum {poutputDatum'outputDatum} ->
              pto poutputDatum'outputDatum #== pforgetData (pdata inputDatum)
            _ -> pconstant False
        , pmatch inputRefScript $ \case
            PDNothing -> pconstant True
            PDJust _ -> pconstant False
        , pquantityOf inputValue policyId pdaParamsAssetName #== 1
        , pmatch inputDatum $ \PDaParamsDatum {pdaParams'owners, pdaParams'updateThreshold} ->
            pownerQuorumMet
              # pfromData pdaParams'owners
              # pfromData ptxInfo'signatories
              # pfromData pdaParams'updateThreshold
        , pvalueNotDrained inputValue outputValue
        ]
    )
    (pconstant ())
    perror

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- |
Module      : LinkedList.Internal
Description : Plutarch port of @aiken-design-patterns/linked-list/internal.ak@
              (v1.7.0).

Shared machinery for the linked-list modules. As in the Aiken original, these
are building blocks rather than a contract-facing API — prefer "LinkedList".

The Aiken original reaches for @builtin.un_map_data@ and walks the value's
@Data@ encoding directly rather than using the @Value@ API. That is a budget
optimisation and it relies on two ledger guarantees: a UTxO value always lists
its Ada entry first, and a well-formed list element holds nothing but Ada and
its single element NFT. The port keeps the same shape, because the checks it
performs — and the ones it deliberately skips — only make sense against those
guarantees.
-}
module LinkedList.Internal (
  pgetLovelaceAndSingleNftName,
  pauthenticateElementUtxoAndGetInfoHelper,
  pisOnlyMintUnderPolicy,
  pvalidateListNftAndGetOtherListAssets,
  pvalidateNoReservedListAssetChanges,
  pkeyFitsBetween,
  POrdering (..),
  pvalidateSingularAuthenticInputHelper,
  pvalidateDualAuthenticInputsHelper,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential,
  PCurrencySymbol,
  PLedgerValue,
  PMintValue,
  POutputDatum (..),
  PScriptHash,
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value (padaSymbol)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import LinkedList.Types (PNodeKeyPrefix, PNodeKeyPrefixLength, PRootKey)

{- | Aiken @utils.get_lovelace_and_single_nft_name@ (from
@aiken-design-patterns/utils.ak@).

Pops the Ada entry, then requires exactly one remaining policy holding exactly
one asset name at quantity one, and that the policy is @nft_policy_id@. Returns
the Lovelace quantity and the NFT's asset name.

The strictness is the point: a list element UTxO holds Ada and its own NFT and
nothing else, which is what lets the cheaper scans elsewhere in this module
classify inputs by value shape alone.
-}
pgetLovelaceAndSingleNftName ::
  forall (s :: S) (r :: S -> Type).
  Term s PLedgerValue ->
  Term s (PAsData PCurrencySymbol) ->
  (Term s PInteger -> Term s (PAsData PTokenName) -> Term s r) ->
  Term s r
pgetLovelaceAndSingleNftName value nftPolicyId k = P.do
  entries <- plet $ pto (pto (pto (pto value)))
  adaEntry <- plet $ phead # entries
  restOfPolicies <- plet $ ptail # entries
  -- `dict.expect_pop(ada_policy_id)` — the Ada entry sorts first, and its
  -- absence is a failure rather than a zero.
  lovelace <-
    plet $
      pfromData
        ( psndBuiltin
            #$ phead
            #$ pto (pto (pfromData (psndBuiltin # adaEntry)))
        )
  nftEntry <- plet $ pheadSingleton # restOfPolicies
  nameQty <- plet $ pheadSingleton #$ pto (pto (pfromData (psndBuiltin # nftEntry)))
  pif
    ( pand'List
        [ pfromData (pfstBuiltin # adaEntry) #== padaSymbol
        , pfstBuiltin # nftEntry #== nftPolicyId
        , pfromData (psndBuiltin # nameQty) #== 1
        ]
    )
    (k lovelace (pfstBuiltin # nameQty))
    perror

{- | Aiken @internal.authenticate_element_utxo_and_get_info_helper@.

Authenticates an inline-datum singleton list-element UTxO and hands the
continuation its address, Lovelace, NFT asset name (the element key at the
caller layer), raw datum, and optional reference script.

This authenticates the 'PTxOut' it is given; it does /not/ prove that output
came from the transaction body. Callers select the output.
-}
pauthenticateElementUtxoAndGetInfoHelper ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s PAddress ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PData ->
    Term s (PMaybeData PScriptHash) ->
    Term s r
  ) ->
  Term s r
pauthenticateElementUtxoAndGetInfoHelper elementUtxo nftPolicyId k = P.do
  PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
    pmatch elementUtxo
  datumData <-
    plet $ pmatch ptxOut'datum $ \case
      POutputDatum d -> pto d
      _ -> perror
  pgetLovelaceAndSingleNftName (pfromData ptxOut'value) nftPolicyId $
    \lovelace nftName ->
      k ptxOut'address lovelace nftName datumData ptxOut'referenceScript

{- | Aiken @internal.is_only_mint_under_policy@.

Exactly one asset name changes under @policy_id@, it is @asset_name@, and its
quantity is @expected_quantity@. Used by the strict mint helpers — root init,
deinit, and the default node insert/remove.
-}
pisOnlyMintUnderPolicy ::
  forall (s :: S).
  Term
    s
    ( PMintValue
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PBool
    )
pisOnlyMintUnderPolicy = phoistAcyclic $
  plam $ \txMint policyId assetName expectedQuantity ->
    pmatch (AssocMap.plookup # pfromData policyId # pto (pto txMint)) $ \case
      PNothing -> perror
      PJust nameQtyMap -> P.do
        entry <- plet $ pheadSingleton #$ pto (pto nameQtyMap)
        pand'List
          [ pfstBuiltin # entry #== assetName
          , pfromData (psndBuiltin # entry) #== expectedQuantity
          ]

{- | Aiken @internal.validate_list_nft_and_get_other_list_assets@.

Pops @expected_nft_name@ from @expected_nft_policy@, requires its quantity to be
@expected_nft_qty@, and returns the same-policy remainder.

The remainder is /not/ judged here. Advanced callers pass it to
'pvalidateNoReservedListAssetChanges' and then on to application callbacks, so
unrelated same-policy assets can be permitted without weakening the root/node
namespace invariants.
-}
pvalidateListNftAndGetOtherListAssets ::
  forall (s :: S).
  Term
    s
    ( PMintValue
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PTokenQtyList
    )
pvalidateListNftAndGetOtherListAssets = phoistAcyclic $
  plam $ \v expectedNftPolicy expectedNftName expectedNftQty ->
    pmatch (AssocMap.plookup # pfromData expectedNftPolicy # pto (pto v)) $ \case
      PNothing -> perror
      PJust nameQtyMap ->
        pexpectPop # pto (pto nameQtyMap) # expectedNftName # expectedNftQty

-- | The token-name/quantity pair list inside one currency-symbol entry.
type PTokenQtyList = PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))

{- | @dict.expect_pop@ over a token-name map: remove the named entry, check its
quantity, and return the rest.

Fails if the name is absent, matching @expect_pop@.
-}
pexpectPop ::
  forall (s :: S).
  Term
    s
    ( PTokenQtyList
        :--> PAsData PTokenName
        :--> PInteger
        :--> PTokenQtyList
    )
pexpectPop = phoistAcyclic $
  plam $ \entries0 name expectedQty ->
    plet
      ( pfix $ \self -> plam $ \entries ->
          pelimList
            ( \entry rest ->
                pif
                  (pfstBuiltin # entry #== name)
                  ( pif
                      (pfromData (psndBuiltin # entry) #== expectedQty)
                      rest
                      perror
                  )
                  (pcons @PBuiltinList # entry # (self # rest))
            )
            perror
            entries
      )
      (\go -> go # entries0)

{- | Aiken @internal.validate_no_reserved_list_asset_changes@.

Extra same-policy mints or burns must not touch the root NFT and must stay
outside the node namespace. Rejecting both here is what lets advanced callers
allow unrelated same-policy assets at all.
-}
pvalidateNoReservedListAssetChanges ::
  forall (s :: S).
  Term
    s
    ( PTokenQtyList
        :--> PAsData PRootKey
        :--> PNodeKeyPrefix
        :--> PNodeKeyPrefixLength
        :--> PBool
    )
pvalidateNoReservedListAssetChanges = phoistAcyclic $
  plam $ \otherAssets rootKey nodeKeyPrefix nodeKeyPrefixLength ->
    pall
      # plam
        ( \entry -> P.do
            name <- plet $ pfstBuiltin # entry
            pand'List
              [ pnot # (name #== rootKey)
              , pnot
                  # ( psliceBS
                        # 0
                        # nodeKeyPrefixLength
                        # pto (pfromData name)
                        #== nodeKeyPrefix
                    )
              ]
        )
      # otherAssets

{- | The three-way result of @bytearray.compare@.

Plutarch has no @Ordering@, and the Aiken API takes the required ordering as a
value (@Less@ for ascending inserts, @Greater@ for descending), so it has to be
represented explicitly.
-}
data POrdering (s :: S) = PLess | PEqual | PGreater
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq, PShow)
  deriving (PlutusType) via (DeriveAsSOPStruct POrdering)

{- | Aiken @internal.key_fits_between@.

For 'PLess', requires @previous < new@ and @new < next@; for 'PGreater', the
reverse. A missing neighbour is accepted and represents a list boundary.

Comparison is bytestring-lexicographic, so callers encoding numeric keys must
use a fixed width if numeric ordering is what they mean.
-}
pkeyFitsBetween ::
  forall (s :: S).
  Term
    s
    ( POrdering
        :--> PMaybeData PByteString
        :--> PByteString
        :--> PMaybeData PByteString
        :--> PBool
    )
pkeyFitsBetween = phoistAcyclic $
  plam $ \requiredOrdering previousKey newKey nextKey -> P.do
    let ordered a b =
          pmatch requiredOrdering $ \case
            PLess -> a #< b
            PGreater -> b #< a
            PEqual -> a #== b
    pand'List
      [ pmatch previousKey $ \case
          PDNothing -> pconstant True
          PDJust k -> ordered (pfromData k) newKey
      , pmatch nextKey $ \case
          PDNothing -> pconstant True
          PDJust k -> ordered newKey (pfromData k)
      ]

-- | The head of a list that must have exactly one element; fails otherwise.
pheadSingleton ::
  forall (a :: S -> Type) (s :: S).
  (PIsListLike PBuiltinList a) =>
  Term s (PBuiltinList a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \ell ->
    pif (pnull # (ptail # ell)) (phead # ell) perror

--------------------------------------------------------------------------------
-- Authentic-input scanners
--------------------------------------------------------------------------------

{- | Aiken @internal.internal_input_processor_validator@.

Given one currency-symbol entry's name/quantity map, requires exactly one token
name at quantity one, reads the Lovelace out of the value's leading Ada entry,
and requires an inline datum.
-}
pinternalInputProcessor ::
  forall (s :: S) (r :: S -> Type).
  Term s POutputDatum ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PSortedMap PTokenName PInteger)))) ->
  Term s (PAsData (AssocMap.PSortedMap PTokenName PInteger)) ->
  (Term s PInteger -> Term s (PAsData PTokenName) -> Term s PData -> Term s r) ->
  Term s r
pinternalInputProcessor currentDatum currentValuePairs namesQtyPairs k = P.do
  nameQty <- plet $ pheadSingleton #$ pto (pto (pfromData namesQtyPairs))
  lovelace <-
    plet $
      pfromData
        ( psndBuiltin
            #$ phead
            #$ pto (pto (pfromData (psndBuiltin # (phead # currentValuePairs))))
        )
  datumData <-
    plet $ pmatch currentDatum $ \case
      POutputDatum d -> pto d
      _ -> perror
  pif
    (pfromData (psndBuiltin # nameQty) #== 1)
    (k lovelace (pfstBuiltin # nameQty) datumData)
    perror

{- | Aiken @internal.validate_singular_authentic_input_helper@.

Folds right over the inputs, running @return@ at the first authentic list
element and then rejecting any /later/ input that has the candidate element
shape under the same policy.

The narrow shape check is deliberate, and the Aiken comment spells out why: a
valid element UTxO holds only Ada plus its one NFT, and input values always list
Ada first. So once the authentic input is found, a later input can only be
another candidate if its value is exactly Ada plus one non-Ada policy. Anything
else is not a list UTxO under this API's invariants, and paying for a broader
scan would be wasted budget.
-}
pvalidateSingularAuthenticInputHelper ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PData ->
    Term s PBool
  ) ->
  Term s PBool
pvalidateSingularAuthenticInputHelper inputs nftPolicyId return_ =
  pfoldr
    # plam
      ( \inputD inputFound -> P.do
          PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData inputD
          PTxOut {ptxOut'value, ptxOut'datum} <- pmatch ptxInInfo'resolved
          valuePairs <- plet $ pto (pto (pto (pto (pfromData ptxOut'value))))
          nonAda <- plet $ ptail # valuePairs
          isSingleNonAda <-
            plet $
              pif
                (pnull # nonAda)
                (pconstant False)
                (pnull # (ptail # nonAda))
          pif
            inputFound
            -- Already found: a later single-non-Ada-policy input must not be
            -- another element of this list policy.
            ( pif
                isSingleNonAda
                ( pif
                    (pfstBuiltin # (phead # nonAda) #== nftPolicyId)
                    perror
                    inputFound
                )
                inputFound
            )
            ( pif
                isSingleNonAda
                ( pif
                    (pfstBuiltin # (phead # nonAda) #== nftPolicyId)
                    ( pinternalInputProcessor
                        ptxOut'datum
                        valuePairs
                        (psndBuiltin # (phead # nonAda))
                        ( \lovelace nftName datumData ->
                            pif
                              (return_ inputD lovelace nftName datumData)
                              (pconstant True)
                              perror
                        )
                    )
                    inputFound
                )
                inputFound
            )
      )
    # pconstant False
    # inputs

{- | The three states of the dual-input scan.

Aiken encodes this as a Scott-encoded continuation triple; a Plutarch sum with
the same three shapes says the same thing more directly and stays off the data
encoding.
-}
data PDualState (s :: S)
  = PNoneFound
  | POneFound
      (Term s (PAsData PTxInInfo))
      (Term s PCredential)
      (Term s PInteger)
      (Term s (PAsData PTokenName))
      (Term s PData)
  | PTwoFound (Term s PCredential)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PDualState)

{- | Aiken @internal.validate_dual_authentic_inputs_helper@.

Requires exactly two authentic list inputs, selects the anchor by
@anchor_input_outref@, and passes anchor-first to @with@.

This rests on an API precondition the Aiken docs state plainly: the list payment
credential must be dedicated to list UTxOs. Once one authentic input has fixed
the credential, a later input at that same credential /must/ be the second list
input and must authenticate as such; after the pair is accepted, any further
input at that credential is rejected. It is not a classifier for arbitrary
same-credential inputs, and using it as one would be a mistake.
-}
pvalidateDualAuthenticInputsHelper ::
  forall (s :: S).
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PData ->
    Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PData ->
    Term s PBool
  ) ->
  Term s PBool
pvalidateDualAuthenticInputsHelper anchorInputOutref inputs nftPolicyId with_ =
  pmatch
    ( pfoldr
        # plam
          ( \inputD st -> P.do
              PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch $ pfromData inputD
              PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum} <- pmatch ptxInInfo'resolved
              PAddress {paddress'credential} <- pmatch ptxOut'address
              valuePairs <- plet $ pto (pto (pto (pto (pfromData ptxOut'value))))
              nonAda <- plet $ ptail # valuePairs
              pmatch st $ \case
                PNoneFound ->
                  pif
                    ( pif
                        (pnull # nonAda)
                        (pconstant False)
                        (pnull # (ptail # nonAda))
                    )
                    ( pif
                        (pfstBuiltin # (phead # nonAda) #== nftPolicyId)
                        ( pinternalInputProcessor
                            ptxOut'datum
                            valuePairs
                            (psndBuiltin # (phead # nonAda))
                            ( \lovelace nftName datumData ->
                                pcon (POneFound inputD paddress'credential lovelace nftName datumData)
                            )
                        )
                        (pcon PNoneFound)
                    )
                    (pcon PNoneFound)
                POneFound firstInput firstCred firstLovelace firstName firstDatum ->
                  pif
                    (firstCred #== paddress'credential)
                    ( P.do
                        entry <- plet $ pheadSingleton # nonAda
                        pif
                          (pfstBuiltin # entry #== nftPolicyId)
                          ( pinternalInputProcessor
                              ptxOut'datum
                              valuePairs
                              (psndBuiltin # entry)
                              ( \lovelace nftName datumData -> P.do
                                  PTxInInfo {ptxInInfo'outRef = firstOutRef} <-
                                    pmatch $ pfromData firstInput
                                  accepted <-
                                    plet $
                                      pif
                                        (firstOutRef #== anchorInputOutref)
                                        ( with_
                                            firstInput
                                            firstLovelace
                                            firstName
                                            firstDatum
                                            inputD
                                            lovelace
                                            nftName
                                            datumData
                                        )
                                        ( pif
                                            (ptxInInfo'outRef #== anchorInputOutref)
                                            ( with_
                                                inputD
                                                lovelace
                                                nftName
                                                datumData
                                                firstInput
                                                firstLovelace
                                                firstName
                                                firstDatum
                                            )
                                            perror
                                        )
                                  pif accepted (pcon (PTwoFound firstCred)) perror
                              )
                          )
                          perror
                    )
                    (pcon (POneFound firstInput firstCred firstLovelace firstName firstDatum))
                PTwoFound listCred ->
                  pif
                    (paddress'credential #== listCred)
                    perror
                    (pcon (PTwoFound listCred))
          )
        # pcon PNoneFound
        # inputs
    )
    $ \case
      PTwoFound _ -> pconstant True
      _ -> pconstant False

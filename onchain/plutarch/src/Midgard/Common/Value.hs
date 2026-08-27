{- |
Module      : Midgard.Common.Value
Description : Plutarch ports of the @cardano/assets@ operations Midgard uses
              that Plutarch's @Value@ API does not already cover.

Midgard's L2 value is a @ValuePairs@ — a bare list of policy/token/quantity
pairs — and turning one into a ledger @Value@ is a /validating/ operation, not a
cast. The checks it performs are load-bearing, so they are spelled out here
rather than approximated.

A note on the representations, since they are easy to confuse:
@PAssocMap k v@ wraps the builtin pair list directly, while @PSortedMap@ and
@PUnsortedMap@ each wrap a @PAssocMap@. So reaching the list costs one 'pto'
from a @PAssocMap@ and two from either of the others.
-}
module Midgard.Common.Value (
  pfromAssetList,
  pvalueIsNonNegative,
  pnegateValue,
  pmergeValues,
  pvalueIsPositive,
  pvalueWithoutNft,
  pvalueWithoutAsset,
  pnoChangeForStillNeededAssets,
  pquantityOfValue,
) where

import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTokenName)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

import Midgard.Common.Types (PValuePairs)

-- | The token map of one policy, as it appears inside a 'PValuePairs'.
type PTokenPairs = AssocMap.PAssocMap PTokenName PInteger

-- | The sorted map a 'Value.PSortedValue' is built from.
type PSortedTokens = AssocMap.PSortedMap PTokenName PInteger

{- | Aiken @assets.from_asset_list@ (from @aiken-lang/stdlib@).

@
xs |> list.foldr(dict.empty, fn(Pair(policy, assets), acc) {
  when assets is {
    [] -> fail @"from_asset_list: empty assets"
    _ -> assets
      |> dict.from_ascending_pairs_with(fn(v) { 0 != v })
      |> dict.insert_with(acc, policy, _, dict_strategy.expect_no_duplicate())
  }
})
@

Three ways this fails, all reproduced:

  * a policy whose token list is empty;
  * token names not in strictly ascending order, or a zero quantity —
    @from_ascending_pairs_with@ enforces both; and
  * the same policy appearing twice.

None is cosmetic. The result is the /target/ that a payout accumulator must not
exceed, so a malformed list quietly collapsing to a smaller value would weaken
that bound.

Note what is deliberately /not/ required: the policy entries themselves need not
be ascending. Aiken inserts them into a dict one at a time, so any order is
accepted and the result comes out sorted. Requiring sorted policies here would
reject transactions Aiken accepts.
-}
pfromAssetList :: forall (s :: S). Term s (PValuePairs :--> Value.PSortedValue)
pfromAssetList = phoistAcyclic $
  plam $ \pairs ->
    punsafeDowncast $
      pfoldr
        # plam
          ( \entry acc ->
              plet (pfromData (psndBuiltin # entry)) $ \tokens ->
                plet (pfromData (pfstBuiltin # entry)) $ \policy ->
                  pif
                    (pnull # pto tokens)
                    (ptraceInfoError "from_asset_list: empty assets")
                    ( pif
                        (pisAscendingNonZero # tokens)
                        ( pmatch (AssocMap.plookup # policy # acc) $ \case
                            PJust _ -> ptraceInfoError "from_asset_list: duplicate policy"
                            PNothing ->
                              AssocMap.pinsert
                                # policy
                                # (punsafeDowncast tokens :: Term _ PSortedTokens)
                                # acc
                        )
                        (ptraceInfoError "from_asset_list: unsorted or zero quantity")
                    )
          )
        # (AssocMap.pempty :: Term _ (AssocMap.PSortedMap PCurrencySymbol PSortedTokens))
        # pto pairs

{- | Aiken @dict.from_ascending_pairs_with(fn(v) { 0 != v })@, as a predicate.

Strictly ascending keys — which also rules out duplicates — and every quantity
non-zero.
-}
pisAscendingNonZero :: forall (s :: S). Term s (PTokenPairs :--> PBool)
pisAscendingNonZero = phoistAcyclic $
  plam $ \tokens ->
    pfix
      ( \self ->
          plam
            ( \xs ->
            pelimList
              ( \x rest ->
                  pand'
                    # (pnot # (pfromData (psndBuiltin # x) #== 0))
                    #$ pelimList
                      ( \y _ ->
                          pand'
                            # ( pto (pfromData (pfstBuiltin # x))
                                  #< pto (pfromData (pfstBuiltin # y))
                              )
                            # (self # rest)
                      )
                      (pconstant True)
                      rest
              )
                  (pconstant True)
                  xs
            )
      )
      # pto tokens

-- | Aiken @assets.negate@ — every quantity's sign flipped.
pnegateValue :: forall (s :: S). Term s (Value.PSortedValue :--> Value.PSortedValue)
pnegateValue = phoistAcyclic $
  plam $ \value ->
    -- Negation leaves every key untouched, so the sortedness the input carried
    -- still holds; only the amounts change. Plutarch's 'Value.pmapAmounts' is
    -- typed on the raw representation, hence the round trip.
    punsafeCoerce (Value.pmapAmounts # plam (\q -> 0 - q) # Value.pforgetSorted value)

{- | Aiken @assets.merge@ — pointwise addition, absent entries treated as zero.

Plutarch's 'Value.punionWith' already has that behaviour for non-colliding
entries, so this is only a name. It is spelled out because the /combination/
@merge(a, negate(b))@ is what Aiken uses for subtraction, and writing
@punionWith (-)@ instead would be wrong: entries present only in @b@ would come
through with the wrong sign.
-}
pmergeValues ::
  forall (s :: S).
  Term s (Value.PSortedValue :--> Value.PSortedValue :--> Value.PSortedValue)
pmergeValues = phoistAcyclic $
  plam $ \a b -> pdropZeroEntries #$ Value.punionWith # plam (+) # a # b

{- | Drop every zero quantity, and every policy left holding nothing.

Aiken's @assets.merge@ maintains the invariant that a value contains no zero
quantities; Plutarch's 'Value.punionWith' does not, so @x + (-x)@ leaves a
@{policy: {name: 0}}@ behind. A value carrying that compares unequal to the same
value without it, and every consumer of 'pmergeValues' feeds the result into an
equality — so without this, subtracting a value from itself produces something
that equals nothing.
-}
pdropZeroEntries ::
  forall (s :: S). Term s (Value.PSortedValue :--> Value.PSortedValue)
pdropZeroEntries = phoistAcyclic $
  plam $ \value ->
    punsafeDowncast
      ( punsafeDowncast
          ( punsafeDowncast
              ( pmap
                  # plam
                    ( \policyEntry ->
                        ppairDataBuiltin
                          # (pfstBuiltin # policyEntry)
                          # pdata
                            ( punsafeDowncast
                                ( punsafeDowncast
                                    ( pfilter
                                        # plam (\t -> pnot # (pfromData (psndBuiltin # t) #== 0))
                                        # pto (pto (pfromData (psndBuiltin # policyEntry)))
                                    )
                                )
                            )
                    )
                  #$ pfilter
                    # plam
                      ( \policyEntry ->
                          pnot
                            #$ pnull
                            #$ pfilter
                              # plam (\t -> pnot # (pfromData (psndBuiltin # t) #== 0))
                              # pto (pto (pfromData (psndBuiltin # policyEntry)))
                      )
                    # pto (pto (pto value))
              )
          )
      )

{- | @value_is_nonnegative@, local to @validators/user-events/withdrawal.ak@.

Every quantity in the flattened value must be at least zero.
-}
pvalueIsNonNegative :: forall (s :: S). Term s (Value.PSortedValue :--> PBool)
pvalueIsNonNegative = phoistAcyclic $
  plam $ \value ->
    pall
      # plam
        ( \policyEntry ->
            pall
              # plam (\tokenEntry -> pfromData (psndBuiltin # tokenEntry) #>= 0)
              # pto (pto (pfromData (psndBuiltin # policyEntry)))
        )
      # pto (pto (pto value))

{- | @value_is_positive@, local to @validators/payout.ak@.

/Some/ quantity is above zero — not every one. The payout's @AddFunds@ branch
uses it to require that a collection actually collected something, which a value
that is entirely zeroes would not.
-}
pvalueIsPositive :: forall (s :: S). Term s (Value.PSortedValue :--> PBool)
pvalueIsPositive = phoistAcyclic $
  plam $ \value ->
    pany
      # plam
        ( \policyEntry ->
            pany
              # plam (\tokenEntry -> pfromData (psndBuiltin # tokenEntry) #> 0)
              # pto (pto (pfromData (psndBuiltin # policyEntry)))
        )
      # pto (pto (pto value))

{- | Aiken @value |> assets.add(policy_id, asset_name, -1)@ for a held NFT.

/Written as a deletion, not a subtraction./ Aiken's @assets.add@ drops an entry
whose quantity reaches zero; Plutarch's value arithmetic leaves a @{policy:
{name: 0}}@ behind, and a value carrying that compares unequal to the same value
without it. Every use of this feeds an equality against a target value, so the
difference is the difference between accepting a correct payout and rejecting
every payout.

The caller has already established the policy holds exactly this one name, so
dropping the name and then the policy is exactly what Aiken's arithmetic does
here.
-}
pvalueWithoutNft ::
  forall (s :: S).
  Term s (Value.PSortedValue :--> PAsData PCurrencySymbol :--> PAsData PTokenName :--> Value.PSortedValue)
pvalueWithoutNft = phoistAcyclic $
  plam $ \value policyId assetName ->
    punsafeDowncast
      ( punsafeDowncast
          ( punsafeDowncast
              ( pfilter
                  # plam
                    ( \policyEntry ->
                        pif
                          (pfstBuiltin # policyEntry #== policyId)
                          -- The policy survives only if a name other than this
                          -- one remains under it.
                          ( pnot
                              #$ pnull
                              #$ pfilter
                                # plam (\t -> pnot # (pfstBuiltin # t #== assetName))
                                # pto (pto (pfromData (psndBuiltin # policyEntry)))
                          )
                          (pconstant True)
                    )
                  # pto (pto (pto value))
              )
          )
      )

{- | Aiken @value |> assets.add(policy_id, asset_name, -1)@ for an entry held
exactly once, /without/ 'pvalueWithoutNft's assumption about the policy.

The two are not interchangeable. 'pvalueWithoutNft' drops the whole policy entry
when this name is the only one under it and otherwise leaves the entry alone —
including the name it was asked to remove — which is correct only where the
caller has already established the policy holds nothing else. This one removes
the name wherever it sits and drops the policy only if that emptied it, which is
what @assets.add@ does in general.

The caller still owes the quantity: @add@ with @-1@ removes an entry only when
the sum reaches zero, so this is the right port of it exactly when the entry
being removed holds one. Every call site checks @quantity_of == 1@ first, which
is also what makes the one divergence unreachable — asked for an asset the value
does not hold, @add@ leaves a @-1@ behind and this returns the value unchanged.
-}
pvalueWithoutAsset ::
  forall (s :: S).
  Term s (Value.PSortedValue :--> PAsData PCurrencySymbol :--> PAsData PTokenName :--> Value.PSortedValue)
pvalueWithoutAsset = phoistAcyclic $
  plam $ \value policyId assetName ->
    punsafeDowncast
      ( punsafeDowncast
          ( punsafeDowncast
              ( pfoldr
                  # plam
                    ( \policyEntry acc ->
                        pif
                          (pfstBuiltin # policyEntry #== policyId)
                          ( plet
                              ( pfilter
                                  # plam (\t -> pnot # (pfstBuiltin # t #== assetName))
                                  # pto (pto (pfromData (psndBuiltin # policyEntry)))
                              )
                              $ \remaining ->
                                pif
                                  (pnull # remaining)
                                  acc
                                  ( pcons
                                      # ( ppairDataBuiltin
                                            # policyId
                                            #$ pdata
                                            $ punsafeDowncast (punsafeDowncast remaining)
                                        )
                                      # acc
                                  )
                          )
                          (pcons # policyEntry # acc)
                    )
                  # pcon PNil
                  # pto (pto (pto value))
              )
          )
      )

{- | Aiken @no_change_for_still_needed_assets@, local to @validators/payout.ak@.

No asset the payout still needs may appear positively in the reserve's change
output. Without it a collection could route a needed asset back to the reserve
and count it as collected in the same breath.
-}
pnoChangeForStillNeededAssets ::
  forall (s :: S).
  Term s (Value.PSortedValue :--> Value.PSortedValue :--> PBool)
pnoChangeForStillNeededAssets = phoistAcyclic $
  plam $ \change stillNeeded ->
    pall
      # plam
        ( \policyEntry ->
            pall
              # plam
                ( \tokenEntry ->
                    pif
                      (pfromData (psndBuiltin # tokenEntry) #> 0)
                      ( pquantityOfValue
                          # stillNeeded
                          # (pfstBuiltin # policyEntry)
                          # (pfstBuiltin # tokenEntry)
                          #== 0
                      )
                      (pconstant True)
                )
              # pto (pto (pfromData (psndBuiltin # policyEntry)))
        )
      # pto (pto (pto change))

-- | Aiken @assets.quantity_of@ over a value; zero when the pair is absent.
pquantityOfValue ::
  forall (s :: S).
  Term s (Value.PSortedValue :--> PAsData PCurrencySymbol :--> PAsData PTokenName :--> PInteger)
pquantityOfValue = phoistAcyclic $
  plam $ \value policyId assetName ->
    pmatch (AssocMap.plookup # pfromData policyId # pto value) $ \case
      PNothing -> 0
      PJust tokenMap ->
        pmatch (AssocMap.plookup # pfromData assetName # tokenMap) $ \case
          PNothing -> 0
          PJust quantity -> quantity

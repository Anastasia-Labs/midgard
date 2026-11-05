{- |
Module      : DesignPatterns.SingularUtxoIndexer
Description : Plutarch port of @aiken-design-patterns/singular-utxo-indexer.ak@
              (v1.7.0).

Redeemer-supplied indices let a spending validator reach its own input and the
corresponding output in constant time instead of scanning. Only the parts
Midgard uses are ported: @one_to_one@.
-}
module DesignPatterns.SingularUtxoIndexer (poneToOne) where

import Plutarch.LedgerApi.V3 (PTxInInfo (..), PTxOut, PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

{- | Aiken @singular_utxo_indexer.one_to_one@.

Resolves the input at @input_index@ and the output at @output_index@, requires
the resolved input to be the one actually being spent (@own_ref@), and hands
both to @validation_logic@.

@double_satisfaction_prevented@ is the Aiken original's deliberate nag: it is a
required argument precisely so a caller cannot forget that this pattern does
/not/ prevent double satisfaction. Whatever the caller passes must be a real
check, not a literal.
-}
poneToOne ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PBool ->
  (Term s (PAsData PTxInInfo) -> Term s PTxOut -> Term s PBool) ->
  Term s PBool
poneToOne inputIndex outputIndex ownRef inputs outputs doubleSatisfactionPrevented validationLogic = P.do
  inInput <- plet $ pelemAt # inputIndex # inputs
  outUtxo <- plet $ pfromData (pelemAt # outputIndex # outputs)
  PTxInInfo {ptxInInfo'outRef} <- pmatch $ pfromData inInput
  pif
    (pand' # doubleSatisfactionPrevented # (ptxInInfo'outRef #== ownRef))
    (validationLogic inInput outUtxo)
    perror

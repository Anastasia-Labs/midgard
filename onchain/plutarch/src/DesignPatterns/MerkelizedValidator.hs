{- |
Module      : DesignPatterns.MerkelizedValidator
Description : Plutarch port of @aiken-design-patterns/merkelized-validator.ak@
              (v1.7.0).

Off-loads an expensive computation into a withdrawal script so the calling
script stays inside Cardano's size limit. The caller re-reads the delegate's
redeemer and trusts its result only after checking the input it was given
matches. Only the part Midgard uses is ported: @delegated_compute@.
-}
module DesignPatterns.MerkelizedValidator (
  PComputationRedeemer (..),
  pgetWithdrawScriptsRedeemerAt,
  pdelegatedCompute,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PCredential (..),
  PRedeemer,
  PScriptHash,
  PScriptPurpose (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

{- | Aiken @merkelized_validator.ComputationRedeemer<a, b>@.

The delegate's redeemer: the argument it was given, and the result it produced.
Both stay raw 'PData' here, as in the Aiken original, and the caller coerces.
-}
data PComputationRedeemer (s :: S) = PComputationRedeemer
  { pcomputation'inputArg :: Term s PData
  , pcomputation'result :: Term s PData
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PComputationRedeemer)

{- | Aiken @utils.get_withdraw_scripts_redeemer_at@.

Reads the redeemer at @redeemer_index@ and requires it to belong to the
withdrawal purpose of @withdraw_script_hash@. The index is a caller hint, so the
purpose check is what stops it being aimed at an unrelated redeemer.
-}
pgetWithdrawScriptsRedeemerAt ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))
        :--> PInteger
        :--> PData
    )
pgetWithdrawScriptsRedeemerAt = phoistAcyclic $
  plam $ \withdrawScriptHash redeemers redeemerIndex -> P.do
    rdmrPair <- plet $ pelemAt # redeemerIndex # redeemers
    pif
      ( pfstBuiltin # rdmrPair
          #== pdata (pcon (PRewarding (pcon (PScriptCredential withdrawScriptHash))))
      )
      (pto (pfromData (psndBuiltin # rdmrPair)))
      perror

{- | Aiken @merkelized_validator.delegated_compute@.

Reads the delegate's @ComputationRedeemer@, checks that the argument recorded
there is the one this script actually wants computed, and returns the coerced
result.

That equality check is the whole security of the pattern: without it a caller
could point at a redeemer whose result was computed for different inputs. The
delegate script itself is responsible for the result being correct /for/ that
argument.

Note the Aiken original matches @Withdraw(Script(hash))@; in Plutus V3 that
purpose is @Rewarding@ with a script credential, which is what this checks.
-}
pdelegatedCompute ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PEq a) =>
  Term s a ->
  Term s (PAsData PScriptHash) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PInteger ->
  (Term s PData -> Term s a) ->
  (Term s PData -> Term s b) ->
  Term s b
pdelegatedCompute functionInput stakingValidator redeemers redeemerIndex inputCoercer outputCoercer = P.do
  PComputationRedeemer {pcomputation'inputArg, pcomputation'result} <-
    pmatch $
      pfromData $
        punsafeCoerceComputation $
          pgetWithdrawScriptsRedeemerAt # stakingValidator # redeemers # redeemerIndex
  pif
    (inputCoercer pcomputation'inputArg #== functionInput)
    (outputCoercer pcomputation'result)
    perror

-- | @expect ComputationRedeemer { .. }: ComputationRedeemer<Data, Data> = ...@
punsafeCoerceComputation ::
  forall (s :: S). Term s PData -> Term s (PAsData PComputationRedeemer)
punsafeCoerceComputation = punsafeCoerce

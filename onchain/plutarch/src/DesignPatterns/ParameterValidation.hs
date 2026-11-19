{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DesignPatterns.ParameterValidation
Description : Plutarch port of @aiken-design-patterns/parameter-validation.ak@
              (v1.7.0).

Lets a script verify on-chain that some other script hash really is a given
parameterised script applied to given parameters, by reassembling the applied
script's CBOR from a fixed prefix and re-hashing it. Only the part Midgard uses
is ported: @apply_prehashed_param@.
-}
module DesignPatterns.ParameterValidation (papplyPrehashedParam) where

import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.LedgerApi.V3 (PScriptHash (..))
import Plutarch.Prelude

{- | Aiken @parameter_validation.postfix@.

The two bytes closing an applied-script CBOR envelope.
-}
ppostfix :: forall (s :: S). Term s PByteString
ppostfix = phexByteStr "0001"

{- | Aiken @parameter_validation.apply_prehashed_param@.

@
builtin.integer_to_bytearray(True, 1, version)
  |> bytearray.concat(prefix)
  |> bytearray.concat(param)
  |> bytearray.concat(postfix)
  |> blake2b_224
@

For parameters that are already hashes and so need no further hashing —
@apply_param@ is the variant that blake2b-224s the parameter first. Because the
parameter is inserted verbatim, its length does not matter: the caller's
@prefix@ already encodes it.

@version@ is the Plutus language version (1, 2 or 3) as a single big-endian
byte. @prefix@ must come from a single CBOR-encoded instance of the
parameterised script, generated off-chain with dummy parameters.
-}
papplyPrehashedParam ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PByteString :--> PScriptHash)
papplyPrehashedParam = phoistAcyclic $
  plam $ \version prefix param ->
    pcon . PScriptHash $
      pblake2b_224
        -- `integer_to_bytearray(True, 1, version)`: big-endian, one byte wide.
        #$ (pintegerToByteString # pmostSignificantFirst # 1 # version)
        <> prefix
        <> param
        <> ppostfix

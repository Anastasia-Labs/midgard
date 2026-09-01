{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxCompact
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/compact.ak@.

The compact form is what a fault proof carries in place of a whole L2
transaction: every fixed scalar in full, one 32-byte hash per collection. These
tests carry an independent Haskell encoder for that format and check the port's
bytes against it, then exercise the verifiers that bind those bytes to a
transaction id.

Two properties are worth naming.

The **byte-for-byte** check: `verify_native_tx_compact_v1` does not merely decode
what the prover supplied — it re-encodes what it holds and compares the bytes. So
a prover cannot supply bytes that decode to the right value but are not the bytes
the encoder would have produced.

The **script/address transposition**: the lengths encoder writes script witnesses
before address witnesses, which is the opposite of the record's field order. The
fixture below gives those two different values so a one-sided transposition shows
up — in the reference-byte comparison, and again in the round trip.
-}
module Testing.NativeTxCompact (tests) where

import Data.ByteString qualified as BS
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxCompactV1,
  pdecodeNativeTxFieldPreimageLengthsV1,
  pdecodeNativeTxWitnessSetCompact,
  pencodeNativeTxCompactV1,
  pencodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxProofSourceV1,
  pencodeNativeTxWitnessSetCompact,
  pnativeTxCanonicalSizeV1,
  pnativeTxFullHashV1,
  pnativeTxIdForVersion,
  pnativeTxProofCommitmentV1,
  pverifyNativeTxCompactCborV1,
  pverifyNativeTxCompactV1,
  pverifyNativeTxProofSourceV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Native Tx Compact Tests"
    [ testGroup
        "encoding"
        [ testCase "the compact transaction matches an independent encoding" $
            holds $ (pencodeNativeTxCompactV1 # compactT defaultTx) #== pconstant (encodeCompact defaultTx)
        , testCase "the witness set matches an independent encoding" $
            holds $
              (pencodeNativeTxWitnessSetCompact # witnessSetT defaultWs)
                #== pconstant (encodeWitnessSet defaultWs)
        , -- The wire puts script witnesses before address witnesses; the
          -- fixture gives them different values so a transposition shows.
          testCase "the field lengths match an independent encoding" $
            holds $
              (pencodeNativeTxFieldPreimageLengthsV1 # lengthsT defaultLengths)
                #== pconstant (encodeLengths defaultLengths)
        , testCase "the proof source matches an independent encoding" $
            holds $
              ( pencodeNativeTxProofSourceV1
                  # pconstant (encodeCompact defaultTx)
                  # pconstant (encodeWitnessSet defaultWs)
                  # pconstant (encodeLengths defaultLengths)
              )
                #== pconstant defaultProofSource
        , testCase "the proof commitment matches an independent recomputation" $
            holds $
              ( pnativeTxProofCommitmentV1
                  # pconstant (encodeCompact defaultTx)
                  # pconstant (encodeWitnessSet defaultWs)
                  # pconstant (encodeLengths defaultLengths)
              )
                #== pconstant defaultCommitment
        , -- All three components are inside the commitment, so none can be
          -- swapped for another transaction's.
          testCase "the proof commitment changes with each of its three parts" $
            holds $
              pallDistinctT
                [ pnativeTxProofCommitmentV1
                  # pconstant c
                  # pconstant w
                  # pconstant l
                | (c, w, l) <-
                    [ (encodeCompact defaultTx, encodeWitnessSet defaultWs, encodeLengths defaultLengths)
                    , (encodeCompact otherTx, encodeWitnessSet defaultWs, encodeLengths defaultLengths)
                    , (encodeCompact defaultTx, encodeWitnessSet otherWs, encodeLengths defaultLengths)
                    , (encodeCompact defaultTx, encodeWitnessSet defaultWs, encodeLengths otherLengths)
                    ]
                ]
        , testCase "the body encoder rejects a hash that is not 32 bytes" $
            pfails $
              pencodeNativeTxCompactV1
                # compactT defaultTx {tSpendInputsHash = BS.replicate 31 0x01}
        , testCase "the body encoder rejects a negative fee" $
            pfails $ pencodeNativeTxCompactV1 # compactT defaultTx {tFee = -1}
        , testCase "the body encoder rejects a network id that is not 0, 1 or 255" $
            pfails $ pencodeNativeTxCompactV1 # compactT defaultTx {tNetworkId = 2}
        , testCase "the compact encoder rejects an out-of-range validity code" $
            pfails $ pencodeNativeTxCompactV1 # compactT defaultTx {tValidityCode = 6}
        , testCase "the lengths encoder rejects a negative length" $
            pfails $
              pencodeNativeTxFieldPreimageLengthsV1
                # lengthsT defaultLengths {lRedeemers = -1}
        ]
    , testGroup
        "decoding"
        [ testCase "the compact transaction round-trips" $
            holds $
              ( pencodeNativeTxCompactV1
                  #$ pdecodeNativeTxCompactV1
                  # pconstant (encodeCompact defaultTx)
              )
                #== pconstant (encodeCompact defaultTx)
        , testCase "the witness set round-trips" $
            holds $
              ( pencodeNativeTxWitnessSetCompact
                  #$ pdecodeNativeTxWitnessSetCompact
                  # pconstant (encodeWitnessSet defaultWs)
              )
                #== pconstant (encodeWitnessSet defaultWs)
        , -- If either side of the script/address transposition were dropped,
          -- this round trip would swap the two lengths and fail.
          testCase "the field lengths round-trip with script and address distinct" $
            holds $
              ( pencodeNativeTxFieldPreimageLengthsV1
                  #$ pdecodeNativeTxFieldPreimageLengthsV1
                  # pconstant (encodeLengths defaultLengths)
              )
                #== pconstant (encodeLengths defaultLengths)
        , testCase "the decoded field lengths carry each value in its own slot" $
            holds $
              pmatch
                (pdecodeNativeTxFieldPreimageLengthsV1 # pconstant (encodeLengths defaultLengths))
                ( \PNativeTxFieldPreimageLengthsV1
                     {plengths'scriptWitnesses, plengths'addressWitnesses} ->
                      plengths'scriptWitnesses
                        #== pconstant (lScriptWitnesses defaultLengths)
                        #&& plengths'addressWitnesses
                        #== pconstant (lAddressWitnesses defaultLengths)
                )
        , -- Nothing may ride along after the structure the decoder expects.
          testCase "the compact decoder rejects trailing bytes" $
            pfails $ pdecodeNativeTxCompactV1 # pconstant (encodeCompact defaultTx <> "\x00")
        , testCase "v1_n11_noncanonical_compact_validity_code_rejects" $
            pfails $
              pdecodeNativeTxCompactV1
                # pconstant (BS.init (encodeCompact defaultTx) <> "\x18\x03")
        , testCase "v1_n11_out_of_range_compact_validity_code_rejects" $
            pfails $
              pdecodeNativeTxCompactV1
                # pconstant (BS.init (encodeCompact defaultTx) <> "\x06")
        , testCase "the witness set decoder rejects trailing bytes" $
            pfails $
              pdecodeNativeTxWitnessSetCompact # pconstant (encodeWitnessSet defaultWs <> "\x00")
        , testCase "the lengths decoder rejects trailing bytes" $
            pfails $
              pdecodeNativeTxFieldPreimageLengthsV1 # pconstant (encodeLengths defaultLengths <> "\x00")
        , testCase "v1_n08_adjacent_eight_field_length_tuple_rejects" $
            pfails $
              pdecodeNativeTxFieldPreimageLengthsV1
                # pconstant "\x88\x01\x01\x01\x01\x01\x01\x01\x01"
        , testCase "the compact decoder rejects a wrong array header" $
            pfails $ pdecodeNativeTxCompactV1 # pconstant ("\x85" <> BS.drop 1 (encodeCompact defaultTx))
        , testCase "the compact decoder rejects an unsupported version byte" $
            pfails $
              pdecodeNativeTxCompactV1
                # pconstant (BS.take 1 (encodeCompact defaultTx) <> "\x02" <> BS.drop 2 (encodeCompact defaultTx))
        , -- 32-byte hashes are pinned to the `58 20` header, so the same bytes
          -- written in a wider form are not accepted.
          testCase "the witness set decoder rejects a non-pinned hash header" $
            pfails $
              pdecodeNativeTxWitnessSetCompact
                # pconstant ("\x83" <> "\x59\x00\x20" <> hash 0xa1 <> defBytes32 (hash 0xa2) <> defBytes32 (hash 0xa3))
        ]
    , testGroup
        "identity and size"
        [ testCase "v1_transaction_id_matches_typescript_domain_vector" $
            holds $
              (pnativeTxIdForVersion # 1 # pconstant "\x80")
                #== pconstant "\x7d\x3e\x96\x63\x0c\xaf\x36\xe8\x8e\x59\xbd\x85\x69\x20\x50\xf6\xf6\x9c\x0b\xe4\xb2\x85\x50\xe1\x67\x1b\x9f\xd1\x94\x2b\xdd\x0c"
        , testCase "the transaction id matches an independent recomputation" $
            holds $
              (pnativeTxIdForVersion # 1 # pconstant (encodeBody defaultTx))
                #== pconstant (txIdOf defaultTx)
        , testCase "the transaction id rejects an unsupported version" $
            pfails $ pnativeTxIdForVersion # 2 # pconstant (encodeBody defaultTx)
        , testCase "the full-transaction hash matches an independent recomputation" $
            holds $
              (pnativeTxFullHashV1 # pconstant "some canonical bytes")
                #== pconstant (fullHash "some canonical bytes")
        , -- The size of a transaction nobody holds in full, from the compact
          -- form and the nine committed lengths.
          testCase "the canonical size matches an independent computation" $
            holds $
              (pnativeTxCanonicalSizeV1 # compactT defaultTx # lengthsT defaultLengths)
                #== pconstant (canonicalSize defaultTx defaultLengths)
        , testCase "the canonical size tracks each length across its header boundary" $
            holds $
              pall'
                [ (pnativeTxCanonicalSizeV1 # compactT defaultTx # lengthsT defaultLengths {lOutputs = n})
                    #== pconstant (canonicalSize defaultTx defaultLengths {lOutputs = n})
                | n <- [0, 23, 24, 255, 256, 65535, 65536]
                ]
        ]
    , testGroup
        "verification"
        [ testCase "the value verifier accepts a transaction matching its id and bytes" $
            holds $
              pmatch
                ( pverifyNativeTxCompactV1
                    # pconstant (txIdOf defaultTx)
                    # compactT defaultTx
                    # pconstant (encodeCompact defaultTx)
                )
                (\PVerifiedMidgardNativeTxCompact {pverified'txId} -> pverified'txId #== pconstant (txIdOf defaultTx))
        , testCase "the value verifier rejects a wrong transaction id" $
            pfails $
              pverifyNativeTxCompactV1
                # pconstant (txIdOf otherTx)
                # compactT defaultTx
                # pconstant (encodeCompact defaultTx)
        , -- The byte-for-byte half: the value is right, the bytes are not.
          testCase "the value verifier rejects bytes that are not its own encoding" $
            pfails $
              pverifyNativeTxCompactV1
                # pconstant (txIdOf defaultTx)
                # compactT defaultTx
                # pconstant (encodeCompact otherTx)
        , testCase "the byte verifier accepts bytes matching their id" $
            holds $
              pmatch
                (pverifyNativeTxCompactCborV1 # pconstant (txIdOf defaultTx) # pconstant (encodeCompact defaultTx))
                (\PVerifiedMidgardNativeTxCompact {pverified'txId} -> pverified'txId #== pconstant (txIdOf defaultTx))
        , testCase "the byte verifier rejects a wrong transaction id" $
            pfails $
              pverifyNativeTxCompactCborV1 # pconstant (txIdOf otherTx) # pconstant (encodeCompact defaultTx)
        , testCase "the byte verifier rejects trailing bytes" $
            pfails $
              pverifyNativeTxCompactCborV1
                # pconstant (txIdOf defaultTx)
                # pconstant (encodeCompact defaultTx <> "\x00")
        , testCase "the proof source verifier accepts a consistent triple" $
            holds $
              pmatch
                (verifySource (encodeCompact defaultTx) (encodeWitnessSet defaultWs) (encodeLengths defaultLengths))
                (\(PPair _ ws) -> pmatch ws (\PNativeTxWitnessSetCompact {pwitnessSetCompact'addrTxWitsHash} -> pfromData pwitnessSetCompact'addrTxWitsHash #== pconstant (wAddr defaultWs)))
        , -- The witness set is bound by a hash carried *inside* the compact
          -- bytes, so another transaction's cannot be substituted.
          testCase "the proof source verifier rejects another transaction's witness set" $
            pfails $
              verifySource (encodeCompact defaultTx) (encodeWitnessSet otherWs) (encodeLengths defaultLengths)
        , testCase "the proof source verifier rejects a wrong transaction id" $
            pfails $
              pverifyNativeTxProofSourceV1
                # pconstant (txIdOf otherTx)
                # pconstant (encodeCompact defaultTx)
                # pconstant (encodeWitnessSet defaultWs)
                # pconstant (encodeLengths defaultLengths)
        , testCase "the proof source verifier rejects non-canonical length bytes" $
            pfails $
              verifySource
                (encodeCompact defaultTx)
                (encodeWitnessSet defaultWs)
                -- `5` written in the one-byte form instead of packed.
                ("\x89\x18\x05" <> BS.drop 2 (encodeLengths defaultLengths))
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

pallDistinctT :: forall s. [Term s PByteString] -> Term s PBool
pallDistinctT xs =
  pall' [pnot #$ a #== b | (i, a) <- zip [0 :: Int ..] xs, (j, b) <- zip [0 ..] xs, i < j]

verifySource ::
  forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> Term s (PPair PVerifiedMidgardNativeTxCompact PNativeTxWitnessSetCompact)
verifySource compactCbor wsCbor lengthsCbor =
  pverifyNativeTxProofSourceV1
    # pconstant (txIdOf defaultTx)
    # pconstant compactCbor
    # pconstant wsCbor
    # pconstant lengthsCbor

-- | A compact transaction, as a record so a test can move one field at a time.
data Tx = Tx
  { tSpendInputsHash :: BS.ByteString
  , tReferenceInputsHash :: BS.ByteString
  , tOutputsHash :: BS.ByteString
  , tFee :: Integer
  , tValidityStart :: Integer
  , tValidityEnd :: Integer
  , tRequiredObserversHash :: BS.ByteString
  , tRequiredSignersHash :: BS.ByteString
  , tMintHash :: BS.ByteString
  , tScriptIntegrityHash :: BS.ByteString
  , tAuxiliaryDataHash :: BS.ByteString
  , tNetworkId :: Integer
  , tWitnessSetHash :: BS.ByteString
  , tValidityCode :: Integer
  }

{- | Every scalar takes a different CBOR width, so a transposed field shows up as
a length change rather than only a value change.
-}
defaultTx :: Tx
defaultTx =
  Tx
    { tSpendInputsHash = hash 0x01
    , tReferenceInputsHash = hash 0x02
    , tOutputsHash = hash 0x03
    , tFee = 1000000 -- four-byte form
    , tValidityStart = -5 -- negative, packed
    , tValidityEnd = 65536 -- four-byte form
    , tRequiredObserversHash = hash 0x04
    , tRequiredSignersHash = hash 0x05
    , tMintHash = hash 0x06
    , tScriptIntegrityHash = hash 0x07
    , tAuxiliaryDataHash = hash 0x08
    , tNetworkId = 1
    , tWitnessSetHash = wsHashOf defaultWs
    , tValidityCode = 3
    }

otherTx :: Tx
otherTx = defaultTx {tFee = 999999, tWitnessSetHash = wsHashOf otherWs}

data Ws = Ws
  { wAddr :: BS.ByteString
  , wScript :: BS.ByteString
  , wRedeemer :: BS.ByteString
  }

defaultWs, otherWs :: Ws
defaultWs = Ws (hash 0x11) (hash 0x12) (hash 0x13)
otherWs = Ws (hash 0x21) (hash 0x22) (hash 0x23)

data Lengths = Lengths
  { lSpendInputs :: Integer
  , lReferenceInputs :: Integer
  , lOutputs :: Integer
  , lRequiredObservers :: Integer
  , lRequiredSigners :: Integer
  , lMint :: Integer
  , lAddressWitnesses :: Integer
  , lScriptWitnesses :: Integer
  , lRedeemers :: Integer
  }

-- | Address and script witnesses differ, which is what makes the transposition visible.
defaultLengths :: Lengths
defaultLengths = Lengths 5 6 300 8 9 10 70 71 12

otherLengths :: Lengths
otherLengths = defaultLengths {lMint = 11}

defaultProofSource :: BS.ByteString
defaultProofSource =
  "\x83"
    <> definiteBytes (encodeCompact defaultTx)
    <> definiteBytes (encodeWitnessSet defaultWs)
    <> definiteBytes (encodeLengths defaultLengths)

defaultCommitment :: BS.ByteString
defaultCommitment =
  blake2b256 ("MidgardNativeTxProofSourceV1" <> cborInt 1 <> defaultProofSource)

--------------------------------------------------------------------------------
-- The reference encoder
--------------------------------------------------------------------------------

encodeBody :: Tx -> BS.ByteString
encodeBody t =
  BS.concat
    [ "\x8c"
    , defBytes32 (tSpendInputsHash t)
    , defBytes32 (tReferenceInputsHash t)
    , defBytes32 (tOutputsHash t)
    , cborInt (tFee t)
    , cborInt (tValidityStart t)
    , cborInt (tValidityEnd t)
    , defBytes32 (tRequiredObserversHash t)
    , defBytes32 (tRequiredSignersHash t)
    , defBytes32 (tMintHash t)
    , defBytes32 (tScriptIntegrityHash t)
    , defBytes32 (tAuxiliaryDataHash t)
    , cborInt (tNetworkId t)
    ]

encodeCompact :: Tx -> BS.ByteString
encodeCompact t =
  BS.concat
    ["\x84", cborInt 1, encodeBody t, defBytes32 (tWitnessSetHash t), cborInt (tValidityCode t)]

txIdOf :: Tx -> BS.ByteString
txIdOf t = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> encodeBody t)

fullHash :: BS.ByteString -> BS.ByteString
fullHash b = blake2b256 ("MidgardNativeTxFullV1" <> cborInt 1 <> b)

encodeWitnessSet :: Ws -> BS.ByteString
encodeWitnessSet w =
  BS.concat ["\x83", defBytes32 (wAddr w), defBytes32 (wScript w), defBytes32 (wRedeemer w)]

wsHashOf :: Ws -> BS.ByteString
wsHashOf = blake2b256 . encodeWitnessSet

-- | Script before address, matching the wire and not the record.
encodeLengths :: Lengths -> BS.ByteString
encodeLengths l =
  BS.concat $
    "\x89"
      : map
        cborInt
        [ lSpendInputs l
        , lReferenceInputs l
        , lOutputs l
        , lRequiredObservers l
        , lRequiredSigners l
        , lMint l
        , lScriptWitnesses l
        , lAddressWitnesses l
        , lRedeemers l
        ]

canonicalSize :: Tx -> Lengths -> Integer
canonicalSize t l =
  1 + scalar 1 + bodySize + witnessSetSize + scalar (tValidityCode t)
  where
    scalar = fromIntegral . BS.length . cborInt
    size n
      | n < 24 = 1 + n
      | n <= 255 = 2 + n
      | n <= 65535 = 3 + n
      | otherwise = 5 + n
    bodySize =
      1
        + size (lSpendInputs l)
        + size (lReferenceInputs l)
        + size (lOutputs l)
        + scalar (tFee t)
        + scalar (tValidityStart t)
        + scalar (tValidityEnd t)
        + size (lRequiredObservers l)
        + size (lRequiredSigners l)
        + size (lMint l)
        + 34
        + 34
        + scalar (tNetworkId t)
    witnessSetSize =
      1 + size (lAddressWitnesses l) + size (lScriptWitnesses l) + size (lRedeemers l)

--------------------------------------------------------------------------------
-- Reference CBOR
--------------------------------------------------------------------------------

-- | Minimal CBOR for an integer of either sign, over the widths used here.
cborInt :: Integer -> BS.ByteString
cborInt n
  | n >= 0 = major 0 n
  | otherwise = major 1 (-1 - n)
  where
    major base v
      | v <= 23 = BS.pack [fromIntegral (base * 32 + v)]
      | v <= 255 = BS.pack [fromIntegral (base * 32 + 24), fromIntegral v]
      | v <= 65535 = BS.pack [fromIntegral (base * 32 + 25)] <> be 2 v
      | v <= 4294967295 = BS.pack [fromIntegral (base * 32 + 26)] <> be 4 v
      | otherwise = BS.pack [fromIntegral (base * 32 + 27)] <> be 8 v
    be w v = BS.pack [fromIntegral (v `div` (256 ^ i) `mod` 256) | i <- [w - 1, w - 2 .. 0 :: Integer]]

-- | The pinned two-byte header for a 32-byte hash.
defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 h = "\x58\x20" <> h

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | len <= 65535 = BS.pack [0x59, fromIntegral (len `div` 256), fromIntegral (len `mod` 256)] <> bytes
  | otherwise = error "reference definiteBytes: out of fixture range"
  where
    len = BS.length bytes

hash :: Int -> BS.ByteString
hash n = blake2b256 (BS.pack [fromIntegral n])

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

--------------------------------------------------------------------------------
-- Building the Plutarch values
--------------------------------------------------------------------------------

compactT :: forall s. Tx -> Term s PNativeTxCompact
compactT t =
  pcon $
    PNativeTxCompact
      { pcompact'body =
          pcon $
            PNativeTxBodyCompact
              { pbodyCompact'spendInputsHash = pconstant (tSpendInputsHash t)
              , pbodyCompact'referenceInputsHash = pconstant (tReferenceInputsHash t)
              , pbodyCompact'outputsHash = pconstant (tOutputsHash t)
              , pbodyCompact'fee = pconstant (tFee t)
              , pbodyCompact'validityIntervalStart = pconstant (tValidityStart t)
              , pbodyCompact'validityIntervalEnd = pconstant (tValidityEnd t)
              , pbodyCompact'requiredObserversHash = pconstant (tRequiredObserversHash t)
              , pbodyCompact'requiredSignersHash = pconstant (tRequiredSignersHash t)
              , pbodyCompact'mintHash = pconstant (tMintHash t)
              , pbodyCompact'scriptIntegrityHash = pconstant (tScriptIntegrityHash t)
              , pbodyCompact'auxiliaryDataHash = pconstant (tAuxiliaryDataHash t)
              , pbodyCompact'networkId = pconstant (tNetworkId t)
              }
      , pcompact'witnessSetHash = pconstant (tWitnessSetHash t)
      , pcompact'validityCode = pconstant (tValidityCode t)
      }

witnessSetT :: forall s. Ws -> Term s PNativeTxWitnessSetCompact
witnessSetT w =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant (wAddr w))
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant (wScript w))
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant (wRedeemer w))
      }

lengthsT :: forall s. Lengths -> Term s PNativeTxFieldPreimageLengthsV1
lengthsT l =
  pcon $
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs = pconstant (lSpendInputs l)
      , plengths'referenceInputs = pconstant (lReferenceInputs l)
      , plengths'outputs = pconstant (lOutputs l)
      , plengths'requiredObservers = pconstant (lRequiredObservers l)
      , plengths'requiredSigners = pconstant (lRequiredSigners l)
      , plengths'mint = pconstant (lMint l)
      , plengths'addressWitnesses = pconstant (lAddressWitnesses l)
      , plengths'scriptWitnesses = pconstant (lScriptWitnesses l)
      , plengths'redeemers = pconstant (lRedeemers l)
      }

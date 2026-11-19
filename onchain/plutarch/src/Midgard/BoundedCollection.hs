{-# LANGUAGE OverloadedStrings #-}

module Midgard.BoundedCollection (
  PItemProofV1 (..),
  pboundedCollectionVersion,
  pmaxTxSizeDerivedItemCount,
  phashBoundedCollectionItem,
  pboundedCollectionCommitment,
  pverifyBoundedCollectionItem,
) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.LedgerState (PItemProofV1 (..))
import Midgard.NativeTxFieldAccess (pfieldCount)
import Midgard.ValidationMerkle (PFrontierPeak, pfrontierCommitment, pfrontierIsWellFormed, pverifyMembership)
import Midgard.ValidationTrace (pcborInt)

pboundedCollectionVersion :: forall (s :: S). Term s PInteger
pboundedCollectionVersion = 1

pmaxTxSizeDerivedItemCount :: forall (s :: S). Term s PInteger
pmaxTxSizeDerivedItemCount = 16_384

pitemDomain, pcommitmentDomain :: forall (s :: S). Term s PByteString
pitemDomain = pconstant "MidgardBoundedCollectionItemV1"
pcommitmentDomain = pconstant "MidgardBoundedCollectionCommitmentV1"

pfieldIndexIsValid :: forall (s :: S). Term s PInteger -> Term s PBool
pfieldIndexIsValid fieldIndex = fieldIndex #>= 0 #&& fieldIndex #< pfieldCount

phashBoundedCollectionItem ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PByteString :--> PByteString)
phashBoundedCollectionItem = phoistAcyclic $ plam $ \fieldIndex itemIndex itemLength itemCommitment ->
  pif
    ( pfieldIndexIsValid fieldIndex
        #&& itemIndex #>= 0
        #&& itemLength #>= 0
        #&& plengthBS # itemCommitment #== 32
    )
    (pblake2b_256 #$
      pitemDomain
        <> (pencodeDefiniteArrayHeader # 5)
        <> pcborInt pboundedCollectionVersion
        <> pcborInt fieldIndex
        <> pcborInt itemIndex
        <> pcborInt itemLength
        <> (pencodeDefiniteBytes # itemCommitment))
    perror

pboundedCollectionCommitment ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PByteString)
pboundedCollectionCommitment = phoistAcyclic $ plam $ \fieldIndex itemCount frontier ->
  pif
    (pfieldIndexIsValid fieldIndex #&& itemCount #>= 0 #&& pfrontierIsWellFormed # itemCount # frontier)
    (pblake2b_256 #$
      pcommitmentDomain
        <> (pencodeDefiniteArrayHeader # 4)
        <> pcborInt pboundedCollectionVersion
        <> pcborInt fieldIndex
        <> pcborInt itemCount
        <> (pencodeDefiniteBytes #$ pfrontierCommitment # itemCount # frontier))
    perror

pverifyBoundedCollectionItem ::
  forall (s :: S). Term s (PByteString :--> PItemProofV1 :--> PBool)
pverifyBoundedCollectionItem = phoistAcyclic $ plam $ \expectedCommitment proof -> pmatch proof $ \p ->
  plet (pfromData $ pitemProof'fieldIndex p) $ \fieldIndex ->
  plet (pfromData $ pitemProof'itemCount p) $ \itemCount ->
  plet (pfromData $ pitemProof'itemIndex p) $ \itemIndex ->
  plet (pfromData $ pitemProof'itemLength p) $ \itemLength ->
  plet (pfromData $ pitemProof'itemCommitment p) $ \itemCommitment ->
  plet (pfromData $ pitemProof'frontier p) $ \frontier ->
    pif
      ( pand'List
          [ pfromData (pitemProof'version p) #== pboundedCollectionVersion
          , pfieldIndexIsValid fieldIndex
          , itemCount #> 0
          , itemLength #>= 0
          , plengthBS # itemCommitment #== 32
          ]
      )
      ( (plengthBS # expectedCommitment #== 32)
          #&& (itemIndex #>= 0)
          #&& (itemIndex #< itemCount)
          #&& ( pverifyMembership # itemCount # frontier # itemIndex
                  # (phashBoundedCollectionItem # fieldIndex # itemIndex # itemLength # itemCommitment)
                  # pfromData (pitemProof'siblings p)
              )
          -- The commitment constructor is partial on a malformed frontier.
          -- Aiken's conjunction guards it with the membership check above.
          #&& (pboundedCollectionCommitment # fieldIndex # itemCount # frontier #== expectedCommitment)
      )
      (pconstant False)

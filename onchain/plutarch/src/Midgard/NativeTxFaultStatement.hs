{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.NativeTxFaultStatement
Description : Plutarch port of @native-tx-fault-statement-v1.ak@.

Witness-minimal fault statements and their adjudication machinery (§12).
-}
module Midgard.NativeTxFaultStatement (
  PFaultStatementV1 (..),
  pfaultItemPredicate,
  pfaultAssetConservation,
  pfaultStatementFrameBytes,
  pitemFaultStatementBytes,
  pencodeFaultStatement,
  pdecodeFaultStatement,
  pfaultStatementHash,
  pproveItemFault,
  POutputUnitSweepV1,
  popenOutputUnitSweep,
  poutputSweepCheckpoint,
  poutputSweepIsFinal,
  poutputSweepQuantity,
  paccumulateOutputUnit,
  PMintUnitSweepV1,
  popenMintUnitSweep,
  pmintSweepCheckpoint,
  pmintSweepIsFinal,
  pmintSweepQuantity,
  psweepMintUnit,
  pmintUnitQuantity,
  passetConservationFaultIsProven,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (
  pbyteStringToInteger,
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.IntraItemBytes (pbyteIn, pheadAt, psliceExact)
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldCount, pmaxFieldItemCount)
import Midgard.NativeTxIntraItem (
  pcanonicalKeyPrecedesV1,
  pmaxAssetNameBytes,
  popenValueBookmark,
  ppolicyIdBytes,
  pvalueQuantityOf,
 )
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  pwalkFieldIndex,
  pwalkIsComplete,
  pwalkNext,
  pwalkNextItemIndex,
  pwalkSkip,
  pwalkTxId,
 )

pfaultItemPredicate, pfaultAssetConservation :: forall s. Term s PInteger
pfaultItemPredicate = 1
pfaultAssetConservation = 2

poutputFieldIndex :: forall s. Term s PInteger
poutputFieldIndex = 2

pmintFieldIndex :: forall s. Term s PInteger
pmintFieldIndex = 5

pmaxClaimedMagnitude :: forall s. Term s PInteger
pmaxClaimedMagnitude = 18446744073709551615

pstatementDomain :: forall s. Term s PByteString
pstatementDomain = phexByteStr "4d6964676172644e617469766554784661756c7453746174656d656e745631"

data PFaultStatementV1 (s :: S) = PFaultStatementV1
  { pfault'txId :: Term s (PAsData PByteString)
  , pfault'code :: Term s (PAsData PInteger)
  , pfault'fieldIndex :: Term s (PAsData PInteger)
  , pfault'itemIndex :: Term s (PAsData PInteger)
  , pfault'policyId :: Term s (PAsData PByteString)
  , pfault'assetName :: Term s (PAsData PByteString)
  , pfault'claimed :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFaultStatementV1)

pfaultStatementFrameBytes, pitemFaultStatementBytes :: forall s. Term s PInteger
pfaultStatementFrameBytes = 53
pitemFaultStatementBytes = pfaultStatementFrameBytes + 2

pstatementShapeIsExact :: forall s. Term s (PFaultStatementV1 :--> PBool)
pstatementShapeIsExact = phoistAcyclic $ plam $ \statement -> pmatch statement $ \s ->
  plet (pfromData $ pfault'txId s) $ \txId ->
  plet (pfromData $ pfault'code s) $ \code ->
  plet (pfromData $ pfault'fieldIndex s) $ \fieldIndex ->
  plet (pfromData $ pfault'itemIndex s) $ \itemIndex ->
  plet (pfromData $ pfault'policyId s) $ \policyId ->
  plet (pfromData $ pfault'assetName s) $ \assetName ->
  plet (pfromData $ pfault'claimed s) $ \claimed ->
    plengthBS # txId #== 32
      #&& fieldIndex #>= 0
      #&& fieldIndex #< pfieldCount
      #&& itemIndex #>= 0
      #&& itemIndex #<= pmaxFieldItemCount
      #&& pif
        (code #== pfaultAssetConservation)
        ( plengthBS # policyId #== ppolicyIdBytes
            #&& plengthBS # assetName #<= pmaxAssetNameBytes
            #&& fieldIndex #== poutputFieldIndex
            #&& itemIndex #== 0
        )
        ( code #== pfaultItemPredicate
            #&& policyId #== pconstant ""
            #&& assetName #== pconstant ""
            #&& claimed #== 0
        )

pencodeFaultStatement :: forall s. Term s (PFaultStatementV1 :--> PByteString)
pencodeFaultStatement = phoistAcyclic $ plam $ \statement ->
  pexpecting (pstatementShapeIsExact # statement) $ pmatch statement $ \s ->
    plet (pfromData $ pfault'claimed s) $ \claimed ->
    plet (pif (claimed #< 0) (0 - claimed) claimed) $ \magnitude ->
    pexpecting (magnitude #<= pmaxClaimedMagnitude) $
      pconstant "\x87\x58\x20"
        <> pfromData (pfault'txId s)
        <> pconstant "\x41"
        <> pbigEndian 1 (pfromData $ pfault'code s)
        <> pconstant "\x41"
        <> pbigEndian 1 (pfromData $ pfault'fieldIndex s)
        <> pconstant "\x43"
        <> pbigEndian 3 (pfromData $ pfault'itemIndex s)
        <> (pencodeName # pfromData (pfault'policyId s))
        <> (pencodeName # pfromData (pfault'assetName s))
        <> pconstant "\x49"
        <> pbigEndian 1 (pif (claimed #< 0) 1 0)
        <> pbigEndian 8 magnitude

pencodeName :: forall s. Term s (PByteString :--> PByteString)
pencodeName = phoistAcyclic $ plam $ \name -> plet (plengthBS # name) $ \len ->
  pexpecting (len #<= pmaxAssetNameBytes) $
    pif
      (len #<= 23)
      (pbigEndian 1 (0x40 + len) <> name)
      (pconstant "\x58" <> pbigEndian 1 len <> name)

pdecodeFaultStatement :: forall s. Term s (PByteString :--> PFaultStatementV1)
pdecodeFaultStatement = phoistAcyclic $ plam $ \bytes ->
  pmatch (pheadAt # bytes # 43 # 2) $ \(PPair policyOffset policyLen) ->
    plet (policyOffset + policyLen) $ \nameHeadOffset ->
    pmatch (pheadAt # bytes # nameHeadOffset # 2) $ \(PPair nameOffset nameLen) ->
      plet (nameOffset + nameLen) $ \quantityOffset ->
      pexpecting (pbyteIn # bytes # quantityOffset #== 0x49) $
      plet (pbyteIn # bytes # (quantityOffset + 1)) $ \sign ->
      pexpecting (sign #== 0 #|| sign #== 1) $
      plet (pbeInt bytes (quantityOffset + 2) 8) $ \magnitude ->
      plet
        ( pcon $ PFaultStatementV1
            { pfault'txId = pdata $ psliceExact # bytes # 3 # 32
            , pfault'code = pdata $ pbyteIn # bytes # 36
            , pfault'fieldIndex = pdata $ pbyteIn # bytes # 38
            , pfault'itemIndex = pdata $ pbeInt bytes 40 3
            , pfault'policyId = pdata $ psliceExact # bytes # policyOffset # policyLen
            , pfault'assetName = pdata $ psliceExact # bytes # nameOffset # nameLen
            , pfault'claimed = pdata $ pif (sign #== 1) (0 - magnitude) magnitude
            }
        )
        $ \statement -> pexpecting ((pencodeFaultStatement # statement) #== bytes) statement

pfaultStatementHash :: forall s. Term s (PFaultStatementV1 :--> PByteString)
pfaultStatementHash = phoistAcyclic $ plam $ \statement ->
  pblake2b_256 #$ pstatementDomain <> (pencodeFaultStatement # statement)

pproveItemFault :: forall s.
  Term s
    ( PFieldViewV1
        :--> PFieldWalkCheckpointV1
        :--> PFaultStatementV1
        :--> (PInteger :--> PByteString :--> PBool)
        :--> PPair PBool PFieldWalkCheckpointV1
    )
pproveItemFault = phoistAcyclic $ plam $ \view checkpoint statement predicate ->
  pmatch statement $ \s ->
    pexpecting
      ( pfromData (pfault'code s) #== pfaultItemPredicate
          #&& pstatementShapeIsExact # statement
          #&& pwalkTxId # checkpoint #== pfromData (pfault'txId s)
          #&& pwalkFieldIndex # checkpoint #== pfromData (pfault'fieldIndex s)
      ) $
      plet
        ( pwalkSkip
            # view
            # checkpoint
            # (pfromData (pfault'itemIndex s) - pwalkNextItemIndex # checkpoint)
        )
        $ \advanced -> pmatch (pwalkNext # view # advanced) $ \(PPair item next) ->
          pcon $ PPair (pnot #$ predicate # pfromData (pfault'itemIndex s) # item) next

data POutputUnitSweepV1 (s :: S) = POutputUnitSweepV1
  { poutputSweep'txId :: Term s PByteString
  , poutputSweep'policyId :: Term s PByteString
  , poutputSweep'assetName :: Term s PByteString
  , poutputSweep'checkpoint :: Term s PFieldWalkCheckpointV1
  , poutputSweep'quantity :: Term s PInteger
  , poutputSweep'isFinal :: Term s PBool
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec POutputUnitSweepV1)

popenOutputUnitSweep :: forall s.
  Term s
    ( PFieldWalkCheckpointV1
        :--> PByteString
        :--> PByteString
        :--> POutputUnitSweepV1
    )
popenOutputUnitSweep = phoistAcyclic $ plam $ \checkpoint policyId assetName ->
  pexpecting (psweepMayOpen checkpoint poutputFieldIndex policyId assetName) $
    pcon $ POutputUnitSweepV1
      { poutputSweep'txId = pwalkTxId # checkpoint
      , poutputSweep'policyId = policyId
      , poutputSweep'assetName = assetName
      , poutputSweep'checkpoint = checkpoint
      , poutputSweep'quantity = 0
      , poutputSweep'isFinal = pwalkIsComplete # checkpoint
      }

psweepMayOpen :: forall s.
  Term s PFieldWalkCheckpointV1 -> Term s PInteger -> Term s PByteString ->
  Term s PByteString -> Term s PBool
psweepMayOpen checkpoint fieldIndex policyId assetName =
  pwalkFieldIndex # checkpoint #== fieldIndex
    #&& pwalkNextItemIndex # checkpoint #== 0
    #&& plengthBS # policyId #== ppolicyIdBytes
    #&& plengthBS # assetName #<= pmaxAssetNameBytes

poutputSweepCheckpoint :: forall s.
  Term s (POutputUnitSweepV1 :--> PFieldWalkCheckpointV1)
poutputSweepCheckpoint = phoistAcyclic $ plam $ \sweep ->
  pmatch sweep poutputSweep'checkpoint

poutputSweepIsFinal :: forall s. Term s (POutputUnitSweepV1 :--> PBool)
poutputSweepIsFinal = phoistAcyclic $ plam $ \sweep -> pmatch sweep poutputSweep'isFinal

poutputSweepQuantity :: forall s. Term s (POutputUnitSweepV1 :--> PInteger)
poutputSweepQuantity = phoistAcyclic $ plam $ \sweep -> pmatch sweep $ \s ->
  pexpecting (poutputSweep'isFinal s) (poutputSweep'quantity s)

paccumulateOutputUnit :: forall s.
  Term s (PFieldViewV1 :--> POutputUnitSweepV1 :--> PInteger :--> POutputUnitSweepV1)
paccumulateOutputUnit = phoistAcyclic $ plam $ \view sweep budget ->
  pexpecting (budget #>= 0) (pfoldOutputUnit # view # sweep # budget)

pfoldOutputUnit :: forall s.
  Term s (PFieldViewV1 :--> POutputUnitSweepV1 :--> PInteger :--> POutputUnitSweepV1)
pfoldOutputUnit = pfix $ \self -> plam $ \view sweep budget -> pmatch sweep $ \s ->
  pif
    (pwalkIsComplete # poutputSweep'checkpoint s)
    ( pcon $ POutputUnitSweepV1
        { poutputSweep'txId = poutputSweep'txId s
        , poutputSweep'policyId = poutputSweep'policyId s
        , poutputSweep'assetName = poutputSweep'assetName s
        , poutputSweep'checkpoint = poutputSweep'checkpoint s
        , poutputSweep'quantity = poutputSweep'quantity s
        , poutputSweep'isFinal = pconstant True
        }
    ) $
  pif (budget #== 0) sweep $
    pmatch (pwalkNext # view # poutputSweep'checkpoint s) $ \(PPair item advanced) ->
      pmatch
        ( pvalueQuantityOf
            # (popenValueBookmark # item)
            # poutputSweep'policyId s
            # poutputSweep'assetName s
        )
        $ \(PPair quantity _) ->
          self
            # view
            # pcon
              ( POutputUnitSweepV1
                  { poutputSweep'txId = poutputSweep'txId s
                  , poutputSweep'policyId = poutputSweep'policyId s
                  , poutputSweep'assetName = poutputSweep'assetName s
                  , poutputSweep'checkpoint = advanced
                  , poutputSweep'quantity = poutputSweep'quantity s + quantity
                  , poutputSweep'isFinal = poutputSweep'isFinal s
                  }
              )
            # (budget - 1)

data PMintUnitSweepV1 (s :: S) = PMintUnitSweepV1
  { pmintSweep'txId :: Term s PByteString
  , pmintSweep'policyId :: Term s PByteString
  , pmintSweep'assetName :: Term s PByteString
  , pmintSweep'checkpoint :: Term s PFieldWalkCheckpointV1
  , pmintSweep'previousPolicy :: Term s PByteString
  , pmintSweep'quantity :: Term s PInteger
  , pmintSweep'isFinal :: Term s PBool
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PMintUnitSweepV1)

popenMintUnitSweep :: forall s.
  Term s
    ( PFieldWalkCheckpointV1
        :--> PByteString
        :--> PByteString
        :--> PMintUnitSweepV1
    )
popenMintUnitSweep = phoistAcyclic $ plam $ \checkpoint policyId assetName ->
  pexpecting (psweepMayOpen checkpoint pmintFieldIndex policyId assetName) $
    pcon $ PMintUnitSweepV1
      { pmintSweep'txId = pwalkTxId # checkpoint
      , pmintSweep'policyId = policyId
      , pmintSweep'assetName = assetName
      , pmintSweep'checkpoint = checkpoint
      , pmintSweep'previousPolicy = pconstant ""
      , pmintSweep'quantity = 0
      , pmintSweep'isFinal = pwalkIsComplete # checkpoint
      }

pmintSweepCheckpoint :: forall s. Term s (PMintUnitSweepV1 :--> PFieldWalkCheckpointV1)
pmintSweepCheckpoint = phoistAcyclic $ plam $ \sweep -> pmatch sweep pmintSweep'checkpoint

pmintSweepIsFinal :: forall s. Term s (PMintUnitSweepV1 :--> PBool)
pmintSweepIsFinal = phoistAcyclic $ plam $ \sweep -> pmatch sweep pmintSweep'isFinal

pmintSweepQuantity :: forall s. Term s (PMintUnitSweepV1 :--> PInteger)
pmintSweepQuantity = phoistAcyclic $ plam $ \sweep -> pmatch sweep $ \s ->
  pexpecting (pmintSweep'isFinal s) (pmintSweep'quantity s)

psweepMintUnit :: forall s.
  Term s (PFieldViewV1 :--> PMintUnitSweepV1 :--> PInteger :--> PMintUnitSweepV1)
psweepMintUnit = phoistAcyclic $ plam $ \view sweep budget ->
  pexpecting (budget #>= 0) $
    pmatch sweep $ \s ->
      pif (pmintSweep'isFinal s) sweep (psweepMintPolicies # view # sweep # budget)

psweepMintPolicies :: forall s.
  Term s (PFieldViewV1 :--> PMintUnitSweepV1 :--> PInteger :--> PMintUnitSweepV1)
psweepMintPolicies = pfix $ \self -> plam $ \view sweep budget -> pmatch sweep $ \s ->
  pif
    (pwalkIsComplete # pmintSweep'checkpoint s)
    (pmakeMintSweep s (pmintSweep'checkpoint s) (pmintSweep'previousPolicy s)
      (pmintSweep'quantity s) (pconstant True)) $
  pif (budget #== 0) sweep $
    pmatch (pwalkNext # view # pmintSweep'checkpoint s) $ \(PPair item advanced) ->
    pmatch (pmintItemHead # item) $ \(PPair groupPolicy assetsOffset) ->
      pexpecting
        ( pmintSweep'previousPolicy s #== pconstant ""
            #|| pcanonicalKeyPrecedesV1 # pmintSweep'previousPolicy s # groupPolicy
        ) $
        plet (pmakeMintSweep s advanced groupPolicy (pmintSweep'quantity s) (pmintSweep'isFinal s)) $ \stepped ->
          pif
            (groupPolicy #== pmintSweep'policyId s)
            ( pmakeMintSweep s advanced groupPolicy
                (pmintItemAssetQuantity # item # assetsOffset # pmintSweep'assetName s)
                (pconstant True)
            ) $
          pif
            (pcanonicalKeyPrecedesV1 # groupPolicy # pmintSweep'policyId s)
            (self # view # stepped # (budget - 1))
            (pmakeMintSweep s advanced groupPolicy (pmintSweep'quantity s) (pconstant True))

pmakeMintSweep :: forall s.
  PMintUnitSweepV1 s -> Term s PFieldWalkCheckpointV1 -> Term s PByteString ->
  Term s PInteger -> Term s PBool -> Term s PMintUnitSweepV1
pmakeMintSweep s checkpoint previousPolicy quantity isFinal = pcon $ PMintUnitSweepV1
  { pmintSweep'txId = pmintSweep'txId s
  , pmintSweep'policyId = pmintSweep'policyId s
  , pmintSweep'assetName = pmintSweep'assetName s
  , pmintSweep'checkpoint = checkpoint
  , pmintSweep'previousPolicy = previousPolicy
  , pmintSweep'quantity = quantity
  , pmintSweep'isFinal = isFinal
  }

pmintUnitQuantity :: forall s.
  Term s
    ( PFieldViewV1
        :--> PFieldWalkCheckpointV1
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PPair PInteger PFieldWalkCheckpointV1
    )
pmintUnitQuantity = phoistAcyclic $ plam $ \view checkpoint policyId assetName budget ->
  plet
    (psweepMintUnit # view # (popenMintUnitSweep # checkpoint # policyId # assetName) # budget)
    $ \swept -> pcon $ PPair (pmintSweepQuantity # swept) (pmintSweepCheckpoint # swept)

pmintItemHead :: forall s. Term s (PByteString :--> PPair PByteString PInteger)
pmintItemHead = phoistAcyclic $ plam $ \item ->
  pexpecting (pbyteIn # item # 0 #== 0x82) $
    pmatch (pheadAt # item # 1 # 2) $ \(PPair policyOffset policyLen) ->
      pexpecting (policyLen #== ppolicyIdBytes) $
        pcon $ PPair (psliceExact # item # policyOffset # policyLen) (policyOffset + policyLen)

pmintItemAssetQuantity :: forall s.
  Term s (PByteString :--> PInteger :--> PByteString :--> PInteger)
pmintItemAssetQuantity = phoistAcyclic $ plam $ \item mapOffset assetName ->
  pmatch (pheadAt # item # mapOffset # 5) $ \(PPair afterMap count) ->
    pexpecting (count #> 0) $
      pscanMintAssets # item # afterMap # count # assetName # pconstant "" # pconstant False

pscanMintAssets :: forall s.
  Term s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PBool
        :--> PInteger
    )
pscanMintAssets = pfix $ \self -> plam $ \item offset remaining wanted previous seen ->
  pif (remaining #<= 0) 0 $
    pmatch (pheadAt # item # offset # 2) $ \(PPair nameOffset nameLen) ->
      pexpecting (nameLen #<= pmaxAssetNameBytes) $
      plet (psliceExact # item # nameOffset # nameLen) $ \name ->
      pexpecting (pnot # seen #|| pcanonicalKeyPrecedesV1 # previous # name) $
        pmatch (pmintQuantityAt # item # (nameOffset + nameLen)) $ \(PPair after quantity) ->
          pexpecting (quantity #/= 0) $
            pif (name #== wanted) quantity $
              pif
                (pcanonicalKeyPrecedesV1 # name # wanted)
                (self # item # after # (remaining - 1) # wanted # name # pconstant True)
                0

pmintQuantityAt :: forall s. Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pmintQuantityAt = phoistAcyclic $ plam $ \item offset ->
  pif
    (pdiv # (pbyteIn # item # offset) # 32 #== 0)
    (pheadAt # item # offset # 0) $
    pmatch (pheadAt # item # offset # 1) $ \(PPair after magnitude) ->
      pcon $ PPair after (-1 - magnitude)

passetConservationFaultIsProven :: forall s.
  Term s (PFaultStatementV1 :--> POutputUnitSweepV1 :--> PMintUnitSweepV1 :--> PBool)
passetConservationFaultIsProven = phoistAcyclic $ plam $ \statement outputs mint ->
  pmatch statement $ \st -> pmatch outputs $ \out -> pmatch mint $ \mi ->
    pexpecting
      ( pfromData (pfault'code st) #== pfaultAssetConservation
          #&& pstatementShapeIsExact # statement
          #&& psweepAnswersStatement st
            (poutputSweep'txId out)
            (poutputSweep'policyId out)
            (poutputSweep'assetName out)
            (poutputSweep'isFinal out)
          #&& psweepAnswersStatement st
            (pmintSweep'txId mi)
            (pmintSweep'policyId mi)
            (pmintSweep'assetName mi)
            (pmintSweep'isFinal mi)
      ) $
      poutputSweepQuantity # outputs - pmintSweepQuantity # mint
        #/= pfromData (pfault'claimed st)

psweepAnswersStatement :: forall s.
  PFaultStatementV1 s -> Term s PByteString -> Term s PByteString ->
  Term s PByteString -> Term s PBool -> Term s PBool
psweepAnswersStatement statement txId policyId assetName isFinal =
  txId #== pfromData (pfault'txId statement)
    #&& policyId #== pfromData (pfault'policyId statement)
    #&& assetName #== pfromData (pfault'assetName statement)
    #&& isFinal

pexpecting :: forall (a :: S -> Type) s. Term s PBool -> Term s a -> Term s a
pexpecting condition value = pif condition value perror

pbigEndian :: forall s. Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width value = pintegerToByteString # pmostSignificantFirst # width # value

pbeInt :: forall s. Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PInteger
pbeInt bytes offset width =
  pbyteStringToInteger # pmostSignificantFirst #$ psliceExact # bytes # offset # width

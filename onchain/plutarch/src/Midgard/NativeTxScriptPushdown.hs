{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.NativeTxScriptPushdown
Description : Plutarch port of @native-tx-script-pushdown-v1.ak@.

The checkpointable evaluator for Cardano native scripts.  Recursion in the
script is represented by an explicit stack of small frames, allowing a walk to
stop after a bounded number of node/fold steps and resume from an authenticated
cursor in a later transaction.
-}
module Midgard.NativeTxScriptPushdown (
  PNativeScriptFrameV1 (..),
  PNativeScriptContextV1 (..),
  PNativeScriptWalkV1,
  psignatureNode,
  pallNode,
  panyNode,
  patLeastNode,
  pafterNode,
  pbeforeNode,
  ppendingNone,
  ppendingFalse,
  ppendingTrue,
  pnativeScriptCursorBytes,
  pmaxNativeScriptFrames,
  punsatisfiableRequired,
  popenNativeScriptWalk,
  pnativeScriptWalkIsComplete,
  pnativeScriptVerdict,
  pnativeScriptRun,
  pencodeNativeScriptFrame,
  pencodeNativeScriptCursor,
  pnativeScriptCursorHash,
  presumeNativeScriptWalkFromCommitment,
  pnativeScriptWalkFrames,
  pnativeScriptNodesVisited,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (
  pbyteStringToInteger,
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude

import Midgard.IntraItemBytes (pheadAt, psliceExact)
import Midgard.NativeScript (pmaxNativeScriptDepth, pmaxNativeScriptNodeCount)

psignatureNode, pallNode, panyNode, patLeastNode, pafterNode, pbeforeNode :: forall s. Term s PInteger
psignatureNode = 0
pallNode = 1
panyNode = 2
patLeastNode = 3
pafterNode = 4
pbeforeNode = 5

ppendingNone, ppendingFalse, ppendingTrue :: forall s. Term s PInteger
ppendingNone = 0
ppendingFalse = 1
ppendingTrue = 2

pnativeScriptCursorBytes, pmaxNativeScriptFrames, punsatisfiableRequired :: forall s. Term s PInteger
pnativeScriptCursorBytes = 87
pmaxNativeScriptFrames = pmaxNativeScriptDepth - 1
punsatisfiableRequired = pmaxNativeScriptNodeCount + 1

pcursorDomain, pframeDomain :: forall s. Term s PByteString
pcursorDomain = pconstant "MidgardNativeScriptWalkV1"
pframeDomain = pconstant "MidgardNativeScriptFrameV1"

data PNativeScriptFrameV1 (s :: S) = PNativeScriptFrameV1
  { pnativeFrame'kind :: Term s (PAsData PInteger)
  , pnativeFrame'remaining :: Term s (PAsData PInteger)
  , pnativeFrame'satisfied :: Term s (PAsData PInteger)
  , pnativeFrame'required :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptFrameV1)

data PNativeScriptContextV1 (s :: S) = PNativeScriptContextV1
  { pnativeContext'signers :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pnativeContext'validityIntervalStart :: Term s (PAsData PInteger)
  , pnativeContext'validityIntervalEnd :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptContextV1)

-- The constructor is deliberately not exported: open and authenticated resume
-- are the only ways external code can obtain a walk.
data PNativeScriptWalkV1 (s :: S) = PNativeScriptWalkV1
  { pnativeWalk'scriptDigest :: Term s (PAsData PByteString)
  , pnativeWalk'scriptLength :: Term s (PAsData PInteger)
  , pnativeWalk'offset :: Term s (PAsData PInteger)
  , pnativeWalk'frames :: Term s (PAsData (PBuiltinList (PAsData PNativeScriptFrameV1)))
  , pnativeWalk'roots :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pnativeWalk'nodesVisited :: Term s (PAsData PInteger)
  , pnativeWalk'pending :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptWalkV1)

pmakeWalk ::
  forall s.
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PNativeScriptFrameV1)) ->
  Term s (PBuiltinList (PAsData PByteString)) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PNativeScriptWalkV1
pmakeWalk digest len offset frames roots visited pending =
  pcon $ PNativeScriptWalkV1
    (pdata digest)
    (pdata len)
    (pdata offset)
    (pdata frames)
    (pdata roots)
    (pdata visited)
    (pdata pending)

popenNativeScriptWalk :: forall s. Term s (PByteString :--> PNativeScriptWalkV1)
popenNativeScriptWalk = phoistAcyclic $ plam $ \payload ->
  plet (plengthBS # payload) $ \len ->
    pif (len #> 0 #&& len #<= 0xffffff)
      (pmakeWalk (pblake2b_256 # payload) len 0 pnil pnil 0 ppendingNone)
      perror

pemptyStackRoot :: forall s. Term s PByteString
pemptyStackRoot = pblake2b_256 # pframeDomain

pstackRoot :: forall s. Term s PNativeScriptWalkV1 -> Term s PByteString
pstackRoot walk = pmatch walk $ \w ->
  pmatch (pfromData $ pnativeWalk'roots w) $ \case
    PNil -> pemptyStackRoot
    PCons root _ -> pfromData root

pnativeScriptWalkIsComplete :: forall s. Term s (PNativeScriptWalkV1 :--> PBool)
pnativeScriptWalkIsComplete = phoistAcyclic $ plam $ \walk -> pmatch walk $ \w ->
  pmatch (pfromData $ pnativeWalk'frames w) $ \case
    PNil -> pfromData (pnativeWalk'pending w) #/= ppendingNone
    PCons _ _ -> pconstant False

pnativeScriptVerdict :: forall s. Term s (PNativeScriptWalkV1 :--> PMaybe PBool)
pnativeScriptVerdict = phoistAcyclic $ plam $ \walk ->
  pif (pnativeScriptWalkIsComplete # walk)
    (pmatch walk $ \w ->
      pif (pfromData (pnativeWalk'offset w) #== pfromData (pnativeWalk'scriptLength w))
        (pcon $ PJust $ pfromData (pnativeWalk'pending w) #== ppendingTrue)
        perror)
    (pcon PNothing)

pnativeScriptRun ::
  forall s.
  Term s (PNativeScriptWalkV1 :--> PByteString :--> PNativeScriptContextV1 :--> PInteger :--> PNativeScriptWalkV1)
pnativeScriptRun = phoistAcyclic $ plam $ \walk payload context budget ->
  pmatch walk $ \w ->
    pif (budget #>= 0 #&& pblake2b_256 # payload #== pfromData (pnativeWalk'scriptDigest w))
      (prunSteps # walk # payload # context # budget)
      perror

prunSteps ::
  forall s.
  Term s (PNativeScriptWalkV1 :--> PByteString :--> PNativeScriptContextV1 :--> PInteger :--> PNativeScriptWalkV1)
prunSteps = pfix $ \self -> plam $ \walk payload context budget ->
  pif (budget #== 0 #|| pnativeScriptWalkIsComplete # walk)
    walk
    (self # (pnativeScriptStep # walk # payload # context) # payload # context # (budget - 1))

pnativeScriptStep ::
  forall s.
  Term s (PNativeScriptWalkV1 :--> PByteString :--> PNativeScriptContextV1 :--> PNativeScriptWalkV1)
pnativeScriptStep = phoistAcyclic $ plam $ \walk payload context -> pmatch walk $ \w ->
  pif (pfromData (pnativeWalk'pending w) #/= ppendingNone)
    (pfoldIntoParent # walk)
    (preadNode # walk # payload # context)

pfoldIntoParent :: forall s. Term s (PNativeScriptWalkV1 :--> PNativeScriptWalkV1)
pfoldIntoParent = phoistAcyclic $ plam $ \walk -> pmatch walk $ \w ->
  pmatch (pfromData $ pnativeWalk'frames w) $ \case
    PNil -> perror
    PCons frameData restFrames -> pmatch (pfromData $ pnativeWalk'roots w) $ \case
      PNil -> perror
      PCons _ restRoots -> pmatch (pfromData frameData) $ \frame ->
        plet
          (pfromData (pnativeFrame'satisfied frame)
            + pif (pfromData (pnativeWalk'pending w) #== ppendingTrue) 1 0)
          $ \satisfied ->
            plet (pfromData (pnativeFrame'remaining frame) - 1) $ \remaining ->
              pif (remaining #>= 0)
                (pif (remaining #== 0)
                  (pmakeWalk
                    (pfromData $ pnativeWalk'scriptDigest w)
                    (pfromData $ pnativeWalk'scriptLength w)
                    (pfromData $ pnativeWalk'offset w)
                    restFrames restRoots
                    (pfromData $ pnativeWalk'nodesVisited w)
                    (pverdictOf $ satisfied #>= pfromData (pnativeFrame'required frame)))
                  (ppushFrame
                    (pmakeWalk
                      (pfromData $ pnativeWalk'scriptDigest w)
                      (pfromData $ pnativeWalk'scriptLength w)
                      (pfromData $ pnativeWalk'offset w)
                      restFrames restRoots
                      (pfromData $ pnativeWalk'nodesVisited w)
                      ppendingNone)
                    (pcon $ PNativeScriptFrameV1
                      (pnativeFrame'kind frame)
                      (pdata remaining)
                      (pdata satisfied)
                      (pnativeFrame'required frame))))
                perror

pverdictOf :: forall s. Term s PBool -> Term s PInteger
pverdictOf valid = pif valid ppendingTrue ppendingFalse

ppushFrame :: forall s. Term s PNativeScriptWalkV1 -> Term s PNativeScriptFrameV1 -> Term s PNativeScriptWalkV1
ppushFrame walk frame = pmatch walk $ \w ->
  plet (pfromData $ pnativeWalk'frames w) $ \frames ->
    pif (plength # frames #< pmaxNativeScriptFrames)
      (plet (pchainFrame (pstackRoot walk) frame) $ \root ->
        pmakeWalk
          (pfromData $ pnativeWalk'scriptDigest w)
          (pfromData $ pnativeWalk'scriptLength w)
          (pfromData $ pnativeWalk'offset w)
          (pcons # pdata frame # frames)
          (pcons # pdata root # pfromData (pnativeWalk'roots w))
          (pfromData $ pnativeWalk'nodesVisited w)
          (pfromData $ pnativeWalk'pending w))
      perror

pchainFrame :: forall s. Term s PByteString -> Term s PNativeScriptFrameV1 -> Term s PByteString
pchainFrame below frame = pblake2b_256 #$ pframeDomain <> below <> (pencodeNativeScriptFrame # frame)

pencodeNativeScriptFrame :: forall s. Term s (PNativeScriptFrameV1 :--> PByteString)
pencodeNativeScriptFrame = phoistAcyclic $ plam $ \frame -> pmatch frame $ \f ->
  plet (pfromData $ pnativeFrame'kind f) $ \kind ->
    plet (pfromData $ pnativeFrame'remaining f) $ \remaining ->
      plet (pfromData $ pnativeFrame'satisfied f) $ \satisfied ->
        plet (pfromData $ pnativeFrame'required f) $ \required ->
          pif
            (kind #>= pallNode #&& kind #<= patLeastNode
              #&& remaining #> 0 #&& remaining #<= pmaxNativeScriptNodeCount
              #&& satisfied #>= 0 #&& satisfied #<= pmaxNativeScriptNodeCount
              #&& required #>= 0 #&& required #<= punsatisfiableRequired)
            (pbigEndian 1 kind <> pbigEndian 3 remaining <> pbigEndian 3 satisfied <> pbigEndian 3 required)
            perror

preadNode ::
  forall s.
  Term s (PNativeScriptWalkV1 :--> PByteString :--> PNativeScriptContextV1 :--> PNativeScriptWalkV1)
preadNode = phoistAcyclic $ plam $ \walk payload context -> pmatch walk $ \w ->
  plet (pfromData (pnativeWalk'nodesVisited w) + 1) $ \visited ->
    pif (visited #<= pmaxNativeScriptNodeCount)
      (pmatch (pheadAt # payload # pfromData (pnativeWalk'offset w) # 4) $ \(PPair afterArray arity) ->
        pmatch (pheadAt # payload # afterArray # 0) $ \(PPair afterTag tag) ->
          pif (arity #== pif (tag #== patLeastNode) 3 2)
            (preadTaggedNode w payload context visited afterTag tag)
            perror)
      perror

preadTaggedNode ::
  forall s.
  PNativeScriptWalkV1 s ->
  Term s PByteString ->
  Term s PNativeScriptContextV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PNativeScriptWalkV1
preadTaggedNode w payload context visited afterTag tag =
  pif (tag #== psignatureNode)
    (pmatch (pheadAt # payload # afterTag # 2) $ \(PPair keyOffset keyLength) ->
      pif (keyLength #== 28)
        (plet (psliceExact # payload # keyOffset # keyLength) $ \keyHash ->
          pmatch context $ \ctx ->
            pmakeWalkFrom w (keyOffset + keyLength) visited
              (pverdictOf $ pelem # pdata keyHash # pfromData (pnativeContext'signers ctx)))
        perror)
    (pif (tag #== pafterNode #|| tag #== pbeforeNode)
      (pmatch (pheadAt # payload # afterTag # 0) $ \(PPair next slot) ->
        pmatch context $ \ctx ->
          pmakeWalkFrom w next visited $ pverdictOf $
            pif (tag #== pafterNode)
              (pfromData (pnativeContext'validityIntervalStart ctx) #>= 0
                #&& pfromData (pnativeContext'validityIntervalStart ctx) #>= slot)
              (pfromData (pnativeContext'validityIntervalEnd ctx) #>= 0
                #&& pfromData (pnativeContext'validityIntervalEnd ctx) #<= slot))
      (pif (tag #== patLeastNode)
        (pmatch (pheadAt # payload # afterTag # 0) $ \(PPair afterRequired required) ->
          pmatch (pheadAt # payload # afterRequired # 4) $ \(PPair childrenOffset childCount) ->
            popenCompound
              (pmakeWalkFrom w (pfromData $ pnativeWalk'offset w) visited ppendingNone)
              childrenOffset tag childCount required)
        (pif (tag #== pallNode #|| tag #== panyNode)
          (pmatch (pheadAt # payload # afterTag # 4) $ \(PPair childrenOffset childCount) ->
            popenCompound
              (pmakeWalkFrom w (pfromData $ pnativeWalk'offset w) visited ppendingNone)
              childrenOffset tag childCount
              (pif (tag #== pallNode) childCount 1))
          perror)))

pmakeWalkFrom ::
  forall s.
  PNativeScriptWalkV1 s ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PNativeScriptWalkV1
pmakeWalkFrom w offset visited pending =
  pmakeWalk
    (pfromData $ pnativeWalk'scriptDigest w)
    (pfromData $ pnativeWalk'scriptLength w)
    offset
    (pfromData $ pnativeWalk'frames w)
    (pfromData $ pnativeWalk'roots w)
    visited
    pending

popenCompound ::
  forall s.
  Term s PNativeScriptWalkV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PNativeScriptWalkV1
popenCompound walk offset kind childCount required =
  pif (childCount #>= 0 #&& childCount #<= pmaxNativeScriptNodeCount #&& required #>= 0)
    (pmatch walk $ \w ->
      plet (pmakeWalkFrom w offset (pfromData $ pnativeWalk'nodesVisited w) (pfromData $ pnativeWalk'pending w)) $ \positioned ->
        pif (childCount #== 0)
          (pmatch positioned $ \positionedFields ->
            pmakeWalkFrom positionedFields offset
              (pfromData $ pnativeWalk'nodesVisited positionedFields)
              (pverdictOf $ 0 #>= required))
          (ppushFrame positioned $ pcon $ PNativeScriptFrameV1
            (pdata kind)
            (pdata childCount)
            (pdata 0)
            (pdata $ pif (required #> pmaxNativeScriptNodeCount) punsatisfiableRequired required)))
    perror

pencodeNativeScriptCursor :: forall s. Term s (PNativeScriptWalkV1 :--> PByteString)
pencodeNativeScriptCursor = phoistAcyclic $ plam $ \walk -> pmatch walk $ \w ->
  plet (pfromData $ pnativeWalk'frames w) $ \frames ->
    plet (plength # frames) $ \depth ->
      plet (pfromData $ pnativeWalk'scriptDigest w) $ \digest ->
        plet (pfromData $ pnativeWalk'scriptLength w) $ \scriptLength ->
          plet (pfromData $ pnativeWalk'offset w) $ \offset ->
            plet (pfromData $ pnativeWalk'nodesVisited w) $ \visited ->
              plet (pfromData $ pnativeWalk'pending w) $ \pending ->
                pif
                  (plengthBS # digest #== 32
                    #&& scriptLength #> 0 #&& scriptLength #<= 0xffffff
                    #&& offset #>= 0 #&& offset #<= scriptLength
                    #&& depth #>= 0 #&& depth #<= pmaxNativeScriptFrames
                    #&& plength # pfromData (pnativeWalk'roots w) #== depth
                    #&& visited #>= 0 #&& visited #<= pmaxNativeScriptNodeCount
                    #&& pending #>= ppendingNone #&& pending #<= ppendingTrue)
                  (pconstant "\x87\x58\x20" <> digest
                    <> pconstant "\x58\x20" <> pstackRoot walk
                    <> pconstant "\x43" <> pbigEndian 3 scriptLength
                    <> pconstant "\x43" <> pbigEndian 3 offset
                    <> pconstant "\x43" <> pbigEndian 3 depth
                    <> pconstant "\x43" <> pbigEndian 3 visited
                    <> pconstant "\x41" <> pbigEndian 1 pending)
                  perror

pnativeScriptCursorHash :: forall s. Term s (PNativeScriptWalkV1 :--> PByteString)
pnativeScriptCursorHash = phoistAcyclic $ plam $ \walk ->
  pblake2b_256 #$ pcursorDomain <> (pencodeNativeScriptCursor # walk)

prootsOf ::
  forall s.
  Term s (PBuiltinList (PAsData PNativeScriptFrameV1) :--> PBuiltinList (PAsData PByteString))
prootsOf = pfix $ \self -> plam $ \frames -> pmatch frames $ \case
  PNil -> pnil
  PCons frameData rest ->
    plet (self # rest) $ \below ->
      plet
        (pmatch below $ \case
          PNil -> pemptyStackRoot
          PCons root _ -> pfromData root)
        $ \belowRoot ->
          pcons # pdata (pchainFrame belowRoot (pfromData frameData)) # below

pdecodeNativeScriptCursor ::
  forall s.
  Term s PByteString ->
  Term s (PBuiltinList (PAsData PNativeScriptFrameV1)) ->
  Term s PNativeScriptWalkV1
pdecodeNativeScriptCursor bytes frames =
  pif (plengthBS # bytes #== pnativeScriptCursorBytes)
    (plet
      (pmakeWalk
        (psliceBS # 3 # 32 # bytes)
        (preadBigEndian bytes 70 3)
        (preadBigEndian bytes 74 3)
        frames
        (prootsOf # frames)
        (preadBigEndian bytes 82 3)
        (pindexBS' # bytes # 86))
      $ \walk -> pif (pencodeNativeScriptCursor # walk #== bytes) walk perror)
    perror

presumeNativeScriptWalkFromCommitment ::
  forall s.
  Term s
    ( PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PNativeScriptFrameV1)
        :--> PByteString
        :--> PNativeScriptWalkV1
    )
presumeNativeScriptWalkFromCommitment = phoistAcyclic $ plam $ \committed cursorBytes frames payload ->
  plet (pdecodeNativeScriptCursor cursorBytes frames) $ \walk -> pmatch walk $ \w ->
    pif
      (pnativeScriptCursorHash # walk #== committed
        #&& pblake2b_256 # payload #== pfromData (pnativeWalk'scriptDigest w)
        #&& plengthBS # payload #== pfromData (pnativeWalk'scriptLength w))
      walk
      perror

pnativeScriptWalkFrames ::
  forall s.
  Term s (PNativeScriptWalkV1 :--> PBuiltinList (PAsData PNativeScriptFrameV1))
pnativeScriptWalkFrames = phoistAcyclic $ plam $ \walk -> pmatch walk $ \w -> pfromData (pnativeWalk'frames w)

pnativeScriptNodesVisited :: forall s. Term s (PNativeScriptWalkV1 :--> PInteger)
pnativeScriptNodesVisited = phoistAcyclic $ plam $ \walk -> pmatch walk $ \w -> pfromData (pnativeWalk'nodesVisited w)

pbigEndian :: forall s. Integer -> Term s PInteger -> Term s PByteString
pbigEndian width value = pintegerToByteString # pmostSignificantFirst # pconstant width # value

preadBigEndian :: forall s. Term s PByteString -> Integer -> Integer -> Term s PInteger
preadBigEndian bytes offset width =
  pbyteStringToInteger # pmostSignificantFirst #$ psliceBS # pconstant offset # pconstant width # bytes

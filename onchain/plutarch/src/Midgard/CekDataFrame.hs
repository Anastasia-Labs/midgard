{- |
Module      : Midgard.CekDataFrame
Description : Plutarch port of @lib/midgard/cek-data-frame-v1.ak@.

One frame of a content-addressed traversal over a @Data@ value: the state a
proof carries while it walks a constructor's fields, a list's items or a map's
entries, one child at a time, without ever holding the whole node.

=== Two phases, and a cursor that separates them

A frame is filled and then folded, never both at once. In the __append__ phase
children are added to a Merkle frontier, one leaf each, and @fold_cursor@ is
zero. In the __fold__ phase no more children may arrive — @child_count@ must
already equal @expected_children@ — and children are read back /right to left/,
each proved against the frontier by a membership path, and prepended to the
sequence summary.

Right to left is not an implementation detail. The sequence summaries in
"Midgard.CekData" are built by prepending, so folding from the last child
backwards is what makes the resulting root equal the one a whole-value
commitment would produce. 'pfoldListChildV1' pins the order by computing the
index it expects — @expected_children - fold_cursor - 1@ — and refusing any
other.

=== A map's children are counted in halves

A map frame stores @2n@ children — key, value, key, value — but folds @n@ times,
so its cursor maximum is @expected_children / 2@ and @expected_children@ must be
even. Both are in 'pframeIsWellFormedV1', and 'pfoldMapPairV1' proves /two/
membership paths per step, at @2i@ and @2i + 1@.

=== The large-constructor alternative never appears

A constructor index above 127 travels as its authenticated scalar summary — a
root, a CBOR length and a memory — never as the integer. That is what keeps this
preimage bounded no matter how large the alternative is, and it is why
'pconstructorIsWellFormed' insists the small form leaves those three fields
empty and the large form leaves @constructor@ at zero.
-}
module Midgard.CekDataFrame (
  -- * Frame kinds
  pconstrSmallFrame,
  pconstrLargeFrame,
  plistFrame,
  pmapFrame,

  -- * The frame
  PDataFrameV1 (..),
  pframeIsWellFormedV1,
  pencodeFrameV1,
  phashFrameV1,

  -- * Children
  pchildLeafHashV1,

  -- * Building
  pinitialSmallConstrFrameV1,
  pinitialLargeConstrFrameV1,
  pinitialListFrameV1,
  pinitialMapFrameV1,
  pappendChildV1,

  -- * Folding
  pfoldListChildV1,
  pfoldMapPairV1,
  pfinalizedSummaryV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.CekData (
  PDataSequenceSummaryV1 (..),
  PDataSummaryV1 (..),
  pemptyDataListSummaryV1,
  pemptyDataPairSummaryV1,
  plargeConstrDataSummaryFromCborV1,
  plistDataSummaryV1,
  pmapDataSummaryV1,
  pprependDataListSummaryV1,
  pprependDataPairSummaryV1,
  psmallConstrDataSummaryV1,
 )
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
 )
import Midgard.ValidationMerkle (
  PFrontierPeak,
  pappendLeaf,
  pemptyFrontier,
  pencodeFrontier,
  pfrontierIsWellFormed,
  pverifyMembership,
 )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

pframeDomain, pchildDomain :: forall (s :: S). Term s PByteString
pframeDomain = pconstant "MidgardCekDataFrameV1"
pchildDomain = pconstant "MidgardCekDataFrameChildV1"

puint32Max, puint64Max :: forall (s :: S). Term s PInteger
puint32Max = 4294967295
puint64Max = 18446744073709551615

-- | Aiken @constr_small_frame@ and its three siblings.
pconstrSmallFrame, pconstrLargeFrame, plistFrame, pmapFrame ::
  forall (s :: S). Term s PInteger
pconstrSmallFrame = 0
pconstrLargeFrame = 1
plistFrame = 2
pmapFrame = 3

type PPeakList = PBuiltinList (PAsData PFrontierPeak)

--------------------------------------------------------------------------------
-- The frame
--------------------------------------------------------------------------------

-- | Aiken @DataFrameV1@.
data PDataFrameV1 (s :: S) = PDataFrameV1
  { pframe'kind :: Term s (PAsData PInteger)
  , pframe'constructor :: Term s (PAsData PInteger)
  , pframe'constructorCborRoot :: Term s (PAsData PByteString)
  , pframe'constructorCborLength :: Term s (PAsData PInteger)
  , pframe'constructorMemory :: Term s (PAsData PInteger)
  , pframe'tail :: Term s (PAsData PByteString)
  , pframe'expectedChildren :: Term s (PAsData PInteger)
  , pframe'childCount :: Term s (PAsData PInteger)
  , pframe'childPeaks :: Term s (PAsData PPeakList)
  , pframe'foldCursor :: Term s (PAsData PInteger)
  , pframe'sequence :: Term s (PAsData PDataSequenceSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataFrameV1)

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

puint32IsWellFormed, puint64IsWellFormed :: forall (s :: S). Term s (PInteger :--> PBool)
puint32IsWellFormed = phoistAcyclic $ plam $ \v -> 0 #<= v #&& v #<= puint32Max
puint64IsWellFormed = phoistAcyclic $ plam $ \v -> 0 #<= v #&& v #<= puint64Max

{- | Aiken @optional_hash_is_well_formed@.

The empty string is the absent tail — a frame at the bottom of the stack — so a
tail is either nothing at all or a full 32 bytes, never something in between.
-}
poptionalHashIsWellFormed :: forall (s :: S). Term s (PByteString :--> PBool)
poptionalHashIsWellFormed = phoistAcyclic $
  plam $ \value ->
    plet (plengthBS # value) $ \len -> len #== 0 #|| len #== 32

{- | Aiken @constructor_is_well_formed@.

Each kind owns exactly one of the two constructor representations and must leave
the other empty. Without that, a small frame could carry a large frame's summary
and be hashed under both readings.
-}
pconstructorIsWellFormed :: forall (s :: S). Term s (PDataFrameV1 :--> PBool)
pconstructorIsWellFormed = phoistAcyclic $
  plam $ \frame ->
    pmatch frame $ \f ->
      plet (pfromData (pframe'kind f)) $ \kind ->
        plet (pfromData (pframe'constructor f)) $ \constructor ->
          plet (pfromData (pframe'constructorCborRoot f)) $ \cborRoot ->
            plet (pfromData (pframe'constructorCborLength f)) $ \cborLength ->
              plet (pfromData (pframe'constructorMemory f)) $ \memory ->
                pif
                  (kind #== pconstrSmallFrame)
                  ( pand'List
                      [ 0 #<= constructor
                      , constructor #<= 127
                      , plengthBS # cborRoot #== 0
                      , cborLength #== 0
                      , memory #== 0
                      ]
                  )
                  $ pif
                    (kind #== pconstrLargeFrame)
                    ( pand'List
                        [ constructor #== 0
                        , plengthBS # cborRoot #== 32
                        , 0 #< cborLength
                        , puint32IsWellFormed # cborLength
                        , 5 #<= memory
                        , puint64IsWellFormed # memory
                        ]
                    )
                    ( pand'List
                        [ constructor #== 0
                        , plengthBS # cborRoot #== 0
                        , cborLength #== 0
                        , memory #== 0
                        ]
                    )

-- | Aiken @empty_sequence_for@ — a map folds pairs, everything else folds items.
pemptySequenceFor ::
  forall (s :: S). Term s PInteger -> Term s PDataSequenceSummaryV1
pemptySequenceFor kind =
  pif (kind #== pmapFrame) pemptyDataPairSummaryV1 pemptyDataListSummaryV1

{- | Aiken @frame_is_well_formed@.

The last clause is the phase separator: while the cursor is zero the sequence
must still be /exactly/ the canonical empty one for this kind, and the moment it
is not, every child must already be in the frontier. There is no state in which
a frame is both accepting children and folding them.
-}
pframeIsWellFormedV1 :: forall (s :: S). Term s (PDataFrameV1 :--> PBool)
pframeIsWellFormedV1 = phoistAcyclic $
  plam $ \frame ->
    pmatch frame $ \f ->
      plet (pfromData (pframe'kind f)) $ \kind ->
        plet (pfromData (pframe'expectedChildren f)) $ \expectedChildren ->
          plet (pfromData (pframe'childCount f)) $ \childCount ->
            plet (pfromData (pframe'foldCursor f)) $ \foldCursor ->
              plet
                ( pif
                    (kind #== pmapFrame)
                    (pquot # expectedChildren # 2)
                    expectedChildren
                )
                $ \maximumFoldCursor ->
                  pmatch (pfromData (pframe'sequence f)) $ \(PDataSequenceSummaryV1 root len payload memory) ->
                    pand'List
                      [ pconstrSmallFrame #<= kind
                      , kind #<= pmapFrame
                      , pconstructorIsWellFormed # frame
                      , poptionalHashIsWellFormed # pfromData (pframe'tail f)
                      , puint32IsWellFormed # expectedChildren
                      , pif
                          (kind #== pmapFrame)
                          (prem # expectedChildren # 2 #== 0)
                          (pconstant True)
                      , puint32IsWellFormed # childCount
                      , childCount #<= expectedChildren
                      , pfrontierIsWellFormed # childCount # pfromData (pframe'childPeaks f)
                      , puint32IsWellFormed # foldCursor
                      , foldCursor #<= maximumFoldCursor
                      , plengthBS # pfromData root #== 32
                      , puint32IsWellFormed # pfromData len
                      , pfromData len #== foldCursor
                      , puint64IsWellFormed # pfromData payload
                      , puint64IsWellFormed # pfromData memory
                      , pif
                          (foldCursor #== 0)
                          (pfromData (pframe'sequence f) #== pemptySequenceFor kind)
                          (childCount #== expectedChildren)
                      ]

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

-- | Aiken @encode_sequence@.
pencodeSequence :: forall (s :: S). Term s (PDataSequenceSummaryV1 :--> PByteString)
pencodeSequence = phoistAcyclic $
  plam $ \summary ->
    pmatch summary $ \(PDataSequenceSummaryV1 root len payload memory) ->
      (pencodeDefiniteArrayHeader # 4)
        <> (pencodeDefiniteBytes # pfromData root)
        <> pcborInt (pfromData len)
        <> pcborInt (pfromData payload)
        <> pcborInt (pfromData memory)

{- | Aiken @encode_frame_v1@.

Eleven items, and the frontier and the sequence each count as one — they are
nested arrays, not spliced in. Aborts on a malformed frame: a frame that cannot
exist has no encoding, and the encoding is what a proof commits to.
-}
pencodeFrameV1 :: forall (s :: S). Term s (PDataFrameV1 :--> PByteString)
pencodeFrameV1 = phoistAcyclic $
  plam $ \frame ->
    pif (pnot # (pframeIsWellFormedV1 # frame)) perror $
      pmatch frame $ \f ->
        (pencodeDefiniteArrayHeader # 11)
          <> pcborInt (pfromData (pframe'kind f))
          <> pcborInt (pfromData (pframe'constructor f))
          <> (pencodeDefiniteBytes # pfromData (pframe'constructorCborRoot f))
          <> pcborInt (pfromData (pframe'constructorCborLength f))
          <> pcborInt (pfromData (pframe'constructorMemory f))
          <> (pencodeDefiniteBytes # pfromData (pframe'tail f))
          <> pcborInt (pfromData (pframe'expectedChildren f))
          <> pcborInt (pfromData (pframe'childCount f))
          <> (pencodeFrontier # pfromData (pframe'childPeaks f))
          <> pcborInt (pfromData (pframe'foldCursor f))
          <> (pencodeSequence # pfromData (pframe'sequence f))

-- | Aiken @hash_frame_v1@.
phashFrameV1 :: forall (s :: S). Term s (PDataFrameV1 :--> PByteString)
phashFrameV1 = phoistAcyclic $
  plam $ \frame -> pblake2b_256 # (pframeDomain <> (pencodeFrameV1 # frame))

--------------------------------------------------------------------------------
-- Children
--------------------------------------------------------------------------------

{- | Aiken @summary_is_well_formed@.

@cbor_length > 0@ and @memory >= 4@ are the floors of any real @Data@ node:
nothing serialises to nothing, and every node costs four words before its leaf.
A child that claimed less would be a child the traversal never charged for.
-}
psummaryIsWellFormed :: forall (s :: S). Term s (PDataSummaryV1 :--> PBool)
psummaryIsWellFormed = phoistAcyclic $
  plam $ \summary ->
    pmatch summary $ \(PDataSummaryV1 root cborLength memory) ->
      pand'List
        [ plengthBS # pfromData root #== 32
        , 0 #< pfromData cborLength
        , puint64IsWellFormed # pfromData cborLength
        , 4 #<= pfromData memory
        , puint64IsWellFormed # pfromData memory
        ]

{- | Aiken @child_leaf_hash_v1@.

The leaf commits to the child's /index/ as well as its summary, so two children
with identical summaries at different positions are different leaves and cannot
be substituted for one another.
-}
pchildLeafHashV1 ::
  forall (s :: S). Term s (PInteger :--> PDataSummaryV1 :--> PByteString)
pchildLeafHashV1 = phoistAcyclic $
  plam $ \childIndex child ->
    pif (pnot # (puint32IsWellFormed # childIndex)) perror $
      pif (pnot # (psummaryIsWellFormed # child)) perror $
        pmatch child $ \(PDataSummaryV1 root cborLength memory) ->
          pblake2b_256
            #$ pchildDomain
            <> (pencodeDefiniteArrayHeader # 4)
            <> pcborInt childIndex
            <> (pencodeDefiniteBytes # pfromData root)
            <> pcborInt (pfromData cborLength)
            <> pcborInt (pfromData memory)

--------------------------------------------------------------------------------
-- Building
--------------------------------------------------------------------------------

-- | Aiken @initial_frame@ — aborting rather than declining, as the Aiken does.
pinitialFrame ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PDataSequenceSummaryV1 ->
  Term s PDataFrameV1
pinitialFrame kind constructor cborRoot cborLength memory tail expectedChildren sequence =
  plet
    ( pcon $
        PDataFrameV1
          { pframe'kind = pdata kind
          , pframe'constructor = pdata constructor
          , pframe'constructorCborRoot = pdata cborRoot
          , pframe'constructorCborLength = pdata cborLength
          , pframe'constructorMemory = pdata memory
          , pframe'tail = pdata tail
          , pframe'expectedChildren = pdata expectedChildren
          , pframe'childCount = pdata 0
          , pframe'childPeaks = pdata pemptyFrontier
          , pframe'foldCursor = pdata 0
          , pframe'sequence = pdata sequence
          }
    )
    $ \frame -> pif (pframeIsWellFormedV1 # frame) frame perror

-- | Aiken @initial_small_constr_frame_v1@.
pinitialSmallConstrFrameV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PInteger :--> PDataFrameV1)
pinitialSmallConstrFrameV1 = phoistAcyclic $
  plam $ \constructor tail expectedChildren ->
    pinitialFrame
      pconstrSmallFrame
      constructor
      (pconstant "")
      0
      0
      tail
      expectedChildren
      pemptyDataListSummaryV1

-- | Aiken @initial_large_constr_frame_v1@.
pinitialLargeConstrFrameV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PDataFrameV1
    )
pinitialLargeConstrFrameV1 = phoistAcyclic $
  plam $ \cborRoot cborLength memory tail expectedChildren ->
    pinitialFrame
      pconstrLargeFrame
      0
      cborRoot
      cborLength
      memory
      tail
      expectedChildren
      pemptyDataListSummaryV1

-- | Aiken @initial_list_frame_v1@.
pinitialListFrameV1 ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PDataFrameV1)
pinitialListFrameV1 = phoistAcyclic $
  plam $ \tail expectedChildren ->
    pinitialFrame
      plistFrame
      0
      (pconstant "")
      0
      0
      tail
      expectedChildren
      pemptyDataListSummaryV1

-- | Aiken @initial_map_frame_v1@ — the one kind whose empty sequence is a pair one.
pinitialMapFrameV1 ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PDataFrameV1)
pinitialMapFrameV1 = phoistAcyclic $
  plam $ \tail expectedChildren ->
    pinitialFrame
      pmapFrame
      0
      (pconstant "")
      0
      0
      tail
      expectedChildren
      pemptyDataPairSummaryV1

{- | Aiken @append_child_v1@ — the filling phase.

@fold_cursor == 0@ is the guard that closes this phase for good: once one child
has been folded, no more may be appended.
-}
pappendChildV1 ::
  forall (s :: S).
  Term s (PDataFrameV1 :--> PDataSummaryV1 :--> PMaybe PDataFrameV1)
pappendChildV1 = phoistAcyclic $
  plam $ \frame child ->
    pmatch frame $ \f ->
      plet (pfromData (pframe'childCount f)) $ \childCount ->
        pif
          ( pnot
              #$ pand'List
                [ pframeIsWellFormedV1 # frame
                , psummaryIsWellFormed # child
                , pfromData (pframe'foldCursor f) #== 0
                , childCount #< pfromData (pframe'expectedChildren f)
                ]
          )
          (pcon PNothing)
          $ pcon
          $ PJust
          $ pcon
            f
              { pframe'childCount = pdata (childCount + 1)
              , pframe'childPeaks =
                  pdata $
                    pappendLeaf
                      # childCount
                      # pfromData (pframe'childPeaks f)
                      # (pchildLeafHashV1 # childCount # child)
              }

--------------------------------------------------------------------------------
-- Folding
--------------------------------------------------------------------------------

{- | Aiken @fold_list_child_v1@ — the folding phase, right to left.

The expected index is computed, not supplied: @expected_children - fold_cursor -
1@. A caller offering children in any other order is refused, which is what
makes the resulting sequence root equal the one a whole-value commitment
produces — the summaries are built by /prepending/, so the last child must
arrive first.
-}
pfoldListChildV1 ::
  forall (s :: S).
  Term
    s
    ( PDataFrameV1
        :--> PInteger
        :--> PDataSummaryV1
        :--> PBuiltinList (PAsData PByteString)
        :--> PMaybe PDataFrameV1
    )
pfoldListChildV1 = phoistAcyclic $
  plam $ \frame childIndex child siblings ->
    pmatch frame $ \f ->
      plet (pfromData (pframe'expectedChildren f)) $ \expectedChildren ->
        plet (pfromData (pframe'foldCursor f)) $ \foldCursor ->
          plet (pfromData (pframe'childCount f)) $ \childCount ->
            pif
              ( pnot
                  #$ pand'List
                    [ pframeIsWellFormedV1 # frame
                    , pnot # (pfromData (pframe'kind f) #== pmapFrame)
                    , childCount #== expectedChildren
                    , foldCursor #< expectedChildren
                    , childIndex #== expectedChildren - foldCursor - 1
                    , pverifyMembership
                        # childCount
                        # pfromData (pframe'childPeaks f)
                        # childIndex
                        # (pchildLeafHashV1 # childIndex # child)
                        # siblings
                    ]
              )
              (pcon PNothing)
              $ pcon
              $ PJust
              $ pcon
                f
                  { pframe'foldCursor = pdata (foldCursor + 1)
                  , pframe'sequence =
                      pdata $
                        pprependDataListSummaryV1
                          # child
                          # pfromData (pframe'sequence f)
                  }

{- | Aiken @fold_map_pair_v1@ — two membership proofs per step.

The key sits at @2i@ and the value at @2i + 1@, and both are proved against the
same frontier, so a proof cannot pair a key with a value from another entry.
The cursor counts /pairs/, so the expected index is @expected_children / 2 -
fold_cursor - 1@.
-}
pfoldMapPairV1 ::
  forall (s :: S).
  Term
    s
    ( PDataFrameV1
        :--> PInteger
        :--> PDataSummaryV1
        :--> PDataSummaryV1
        :--> PBuiltinList (PAsData PByteString)
        :--> PBuiltinList (PAsData PByteString)
        :--> PMaybe PDataFrameV1
    )
pfoldMapPairV1 = phoistAcyclic $
  plam $ \frame pairIndex key value keySiblings valueSiblings ->
    pmatch frame $ \f ->
      plet (pfromData (pframe'expectedChildren f)) $ \expectedChildren ->
        plet (pquot # expectedChildren # 2) $ \pairCount ->
          plet (pfromData (pframe'foldCursor f)) $ \foldCursor ->
            plet (pfromData (pframe'childCount f)) $ \childCount ->
              plet (pairIndex * 2) $ \keyIndex ->
                pif
                  ( pnot
                      #$ pand'List
                        [ pframeIsWellFormedV1 # frame
                        , pfromData (pframe'kind f) #== pmapFrame
                        , childCount #== expectedChildren
                        , foldCursor #< pairCount
                        , pairIndex #== pairCount - foldCursor - 1
                        , pverifyMembership
                            # childCount
                            # pfromData (pframe'childPeaks f)
                            # keyIndex
                            # (pchildLeafHashV1 # keyIndex # key)
                            # keySiblings
                        , pverifyMembership
                            # childCount
                            # pfromData (pframe'childPeaks f)
                            # (keyIndex + 1)
                            # (pchildLeafHashV1 # (keyIndex + 1) # value)
                            # valueSiblings
                        ]
                  )
                  (pcon PNothing)
                  $ pcon
                  $ PJust
                  $ pcon
                    f
                      { pframe'foldCursor = pdata (foldCursor + 1)
                      , pframe'sequence =
                          pdata $
                            pprependDataPairSummaryV1
                              # key
                              # value
                              # pfromData (pframe'sequence f)
                      }

{- | Aiken @finalized_summary_v1@.

The frame is complete only when every child is in the frontier /and/ every one
has been folded. What comes out is the same summary "Midgard.CekData" would
have produced from the whole node — which is the point: a traversal proof and a
direct commitment name the same root.
-}
pfinalizedSummaryV1 ::
  forall (s :: S). Term s (PDataFrameV1 :--> PMaybe PDataSummaryV1)
pfinalizedSummaryV1 = phoistAcyclic $
  plam $ \frame ->
    pmatch frame $ \f ->
      plet (pfromData (pframe'kind f)) $ \kind ->
        plet (pfromData (pframe'expectedChildren f)) $ \expectedChildren ->
          plet
            ( pif
                (kind #== pmapFrame)
                (pquot # expectedChildren # 2)
                expectedChildren
            )
            $ \expectedFoldCursor ->
              plet (pfromData (pframe'sequence f)) $ \sequence ->
                pif
                  ( pnot
                      #$ pand'List
                        [ pframeIsWellFormedV1 # frame
                        , pfromData (pframe'childCount f) #== expectedChildren
                        , pfromData (pframe'foldCursor f) #== expectedFoldCursor
                        ]
                  )
                  (pcon PNothing)
                  $ pif
                    (kind #== pconstrSmallFrame)
                    ( pcon $
                        PJust $
                          psmallConstrDataSummaryV1
                            # pfromData (pframe'constructor f)
                            # sequence
                    )
                  $ pif
                    (kind #== pconstrLargeFrame)
                    ( pcon $
                        PJust $
                          plargeConstrDataSummaryFromCborV1
                            # pfromData (pframe'constructorCborRoot f)
                            # pfromData (pframe'constructorCborLength f)
                            # pfromData (pframe'constructorMemory f)
                            # sequence
                    )
                  $ pif
                    (kind #== plistFrame)
                    (pcon (PJust (plistDataSummaryV1 # sequence)))
                    (pcon (PJust (pmapDataSummaryV1 # sequence)))

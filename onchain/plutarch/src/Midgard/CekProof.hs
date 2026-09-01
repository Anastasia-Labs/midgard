{- |
Module      : Midgard.CekProof
Description : Plutarch port of @lib/midgard/cek-proof-v1.ak@.

The content-addressed program format the CEK fault proofs are stated over: every
term, value, sequence link, environment link, continuation frame and blob chunk
of a UPLC program is a 32-byte root, and a proof names roots rather than
carrying structures.

=== Five domains and one shape rule

Terms, values, sequences, environments and continuations hash under five
separate domain strings, and BLS expressions, blob chunks, blob branches,
machine states and program envelopes under four more. A preimage of one kind can
therefore never be read as another even where the CBOR shapes coincide — and
they do coincide: a sequence link and an environment link are byte-identical
but for the domain.

=== Why the sidecar is walked rather than trusted

'pverifyCompleteProgramMaterialV1' is handed a flat, sorted list of
(root, preimage) entries and an envelope naming a term root, a node count and a
total byte length. It walks the program from the term root, resolving each
child root against the list, and only accepts when the walk's own count and
byte total match the envelope /and/ the walk reached every entry. That is what
makes the sidecar complete rather than merely consistent: an entry the walk
never reaches fails the final length comparison, and a node the sidecar omits
fails the lookup.

=== Recursion is tied with 'pfix', never by name

A cycle among top-level Plutarch term definitions is an infinite value — it
type-checks, compiles, and exhausts memory when the term is built. The
traversal, the entry decoder, the sorted-roots check and the encoder over
entries are all self-recursive, and each ties its own knot.
-}
module Midgard.CekProof (
  -- * Limits
  pmaxBuiltinTag,
  pmaxBlobChunkBytesV1,
  pmaxBoundedBlobBytesV1,
  pprogramEnvelopeVersion,
  pmaxProgramEnvelopeCborBytes,
  pmaxProgramNodeCount,
  pmaxProgramMaterialByteLength,

  -- * The material types
  PProgramEnvelopeV1 (..),
  PProgramTermMaterialV1 (..),
  PProgramValueMaterialV1 (..),
  PProgramSequenceMaterialV1 (..),
  PProgramBlobMaterialV1 (..),

  -- * The empty roots
  pemptySequenceRootV1,
  pemptyEnvironmentRootV1,
  pemptyContinuationRootV1,

  -- * Term roots
  phashVariableTermV1,
  phashDelayTermV1,
  phashLambdaTermV1,
  phashApplicationTermV1,
  phashConstantTermV1,
  phashContextConstantTermV1,
  phashForceTermV1,
  phashErrorTermV1,
  phashBuiltinTermV1,
  phashConstrTermV1,
  phashCaseTermV1,

  -- * Value roots
  phashConstantValueV1,
  phashLambdaValueV1,
  phashDelayValueV1,
  phashConstrValueV1,
  phashBuiltinValueV1,
  phashBlsMillerLoopValueV1,
  phashBlsMillerLoopExpressionV1,
  phashBlsMultiplyExpressionV1,

  -- * Sequence and environment links
  phashSequenceNodeV1,
  phashEnvironmentNodeV1,

  -- * Continuation frames
  phashForceContinuationV1,
  phashApplyArgumentContinuationV1,
  phashApplyFunctionContinuationV1,
  phashConstrContinuationV1,
  phashCaseContinuationV1,
  phashApplyValueContinuationV1,
  phashCaseSelectContinuationV1,
  phashCaseApplyContinuationV1,

  -- * Blobs
  phashBlobChunkV1,
  phashBlobBranchV1,
  pboundedBlobRootV1,

  -- * Machine states and envelopes
  phashMachineStateV1,
  pencodeProgramEnvelopeV1,
  phashProgramEnvelopeV1,
  pinspectProgramEnvelopeV1,
  pdecodeProgramEnvelopeV1,

  -- * Material inspection
  pinspectProgramTermMaterialV1,
  pprogramTermChildRootsV1,
  pinspectProgramValueMaterialV1,
  pinspectProgramSequenceMaterialV1,
  pinspectProgramBlobMaterialV1,
  pinspectCompleteProgramMaterialSidecarV1,

  -- * Completeness
  pverifyCompleteProgramMaterialEntriesV1,
  pverifyCompleteProgramMaterialV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekData qualified as CekData
import Midgard.Common.Utils (pconstrOf)
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
  psliceLen,
 )
import Midgard.LedgerState (PCekProgramMaterialDatumV1 (..))

--------------------------------------------------------------------------------
-- Domains
--------------------------------------------------------------------------------

ptermNodeDomain, pvalueNodeDomain, psequenceNodeDomain :: forall (s :: S). Term s PByteString
ptermNodeDomain = pconstant "MidgardCekTermNodeV1"
pvalueNodeDomain = pconstant "MidgardCekValueNodeV1"
psequenceNodeDomain = pconstant "MidgardCekSequenceNodeV1"

penvironmentNodeDomain, pcontinuationNodeDomain :: forall (s :: S). Term s PByteString
penvironmentNodeDomain = pconstant "MidgardCekEnvironmentNodeV1"
pcontinuationNodeDomain = pconstant "MidgardCekContinuationNodeV1"

pblobChunkDomain, pblobBranchDomain :: forall (s :: S). Term s PByteString
pblobChunkDomain = pconstant "MidgardCekBlobChunkV1"
pblobBranchDomain = pconstant "MidgardCekBlobBranchV1"

pmachineStateDomain, pprogramEnvelopeDomain, pblsExpressionDomain ::
  forall (s :: S). Term s PByteString
pmachineStateDomain = pconstant "MidgardCekMachineStateV1"
pprogramEnvelopeDomain = pconstant "MidgardCekProgramEnvelopeV1"
pblsExpressionDomain = pconstant "MidgardCekBlsExpressionV1"

--------------------------------------------------------------------------------
-- Limits
--------------------------------------------------------------------------------

-- | The highest UPLC flat builtin tag, which is @ripemd_160@.
pmaxBuiltinTag :: forall (s :: S). Term s PInteger
pmaxBuiltinTag = 86

pmaxBlobChunkBytesV1, pmaxBoundedBlobBytesV1 :: forall (s :: S). Term s PInteger
pmaxBlobChunkBytesV1 = 4095
pmaxBoundedBlobBytesV1 = 9215

pprogramEnvelopeVersion :: forall (s :: S). Term s PInteger
pprogramEnvelopeVersion = 1

-- | The exact maximum of @[1, [1, 1, 0], h32, uint32, uint32]@.
pmaxProgramEnvelopeCborBytes :: forall (s :: S). Term s PInteger
pmaxProgramEnvelopeCborBytes = 50

-- | @floor((64 MiB − 446 fixed V1 DA bytes) / 42 minimum tuple bytes)@.
pmaxProgramNodeCount :: forall (s :: S). Term s PInteger
pmaxProgramNodeCount = 1597819

-- | The structural DA upper bound after the exact 446-byte fixed V1 framing.
pmaxProgramMaterialByteLength :: forall (s :: S). Term s PInteger
pmaxProgramMaterialByteLength = 67108418

puint32Max, puint64Max :: forall (s :: S). Term s PInteger
puint32Max = 4294967295
puint64Max = 18446744073709551615

--------------------------------------------------------------------------------
-- The material types
--------------------------------------------------------------------------------

-- | Aiken @ProgramEnvelopeV1@ — what a published script binds itself to.
data PProgramEnvelopeV1 (s :: S) = PProgramEnvelopeV1
  { penvelope'termRoot :: Term s (PAsData PByteString)
  , penvelope'nodeCount :: Term s (PAsData PInteger)
  , penvelope'materialByteLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgramEnvelopeV1)

{- | Aiken @ProgramTermMaterialV1@ — one UPLC term node as it travels.

@UnaryTerm@ folds delay, lambda and force into one shape carrying its own tag,
because the three encode identically but for that number. The runtime-only
context constant (tag 10) has no constructor here: it may appear in machine
evidence but never in a published program, and 'pinspectProgramTermMaterialV1'
is where that is enforced.
-}
data PProgramTermMaterialV1 (s :: S)
  = PVariableTerm {pterm'index :: Term s (PAsData PInteger)}
  | PUnaryTerm
      { pterm'tag :: Term s (PAsData PInteger)
      , pterm'child :: Term s (PAsData PByteString)
      }
  | PApplicationTerm
      { pterm'function :: Term s (PAsData PByteString)
      , pterm'argument :: Term s (PAsData PByteString)
      }
  | PConstantTerm {pterm'value :: Term s (PAsData PByteString)}
  | PErrorTerm
  | PBuiltinTerm {pterm'tag :: Term s (PAsData PInteger)}
  | PConstrTerm
      { pterm'tag :: Term s (PAsData PInteger)
      , pterm'count :: Term s (PAsData PInteger)
      , pterm'sequence :: Term s (PAsData PByteString)
      }
  | PCaseTerm
      { pterm'scrutinee :: Term s (PAsData PByteString)
      , pterm'count :: Term s (PAsData PInteger)
      , pterm'sequence :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgramTermMaterialV1)

{- | Aiken @ProgramValueMaterialV1@ — a constant in a published program.

Only constants can appear: a lambda or delay value closes over an environment,
and an environment is not part of a program's source text.
-}
data PProgramValueMaterialV1 (s :: S) = PConstantValue
  { pvalue'typeRoot :: Term s (PAsData PByteString)
  , pvalue'payloadRoot :: Term s (PAsData PByteString)
  , pvalue'payloadLength :: Term s (PAsData PInteger)
  , pvalue'semanticRoot :: Term s (PAsData PByteString)
  , pvalue'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgramValueMaterialV1)

-- | Aiken @ProgramSequenceMaterialV1@ — one link of a term sequence.
data PProgramSequenceMaterialV1 (s :: S) = PProgramSequenceMaterialV1
  { psequence'head :: Term s (PAsData PByteString)
  , psequence'tail :: Term s (PAsData PByteString)
  , psequence'length :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgramSequenceMaterialV1)

-- | Aiken @ProgramBlobMaterialV1@ — a leaf or a branch of a bounded blob.
data PProgramBlobMaterialV1 (s :: S)
  = PBlobChunk {pblob'bytes :: Term s (PAsData PByteString)}
  | PBlobBranch
      { pblob'left :: Term s (PAsData PByteString)
      , pblob'right :: Term s (PAsData PByteString)
      , pblob'byteLength :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgramBlobMaterialV1)

--------------------------------------------------------------------------------
-- Range assertions
--------------------------------------------------------------------------------

pexpectHash :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectHash = phoistAcyclic $
  plam $ \value -> pif (plengthBS # value #== 32) value perror

pexpectUint32 :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectUint32 = phoistAcyclic $
  plam $ \value -> pif (0 #<= value #&& value #<= puint32Max) value perror

pexpectUint64 :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectUint64 = phoistAcyclic $
  plam $ \value -> pif (0 #<= value #&& value #<= puint64Max) value perror

phashUnder ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PByteString
phashUnder domain preimage = pblake2b_256 # (domain <> preimage)

-- | 32 bytes read out of a @Data@ field, or nothing — Aiken @data_hash@.
pdataHash :: forall (s :: S). Term s (PData :--> PMaybe PByteString)
pdataHash = phoistAcyclic $
  plam $ \d ->
    pif (pnot # (pdataIsBytes # d)) (pcon PNothing) $
      plet (pasByteStr # d) $ \value ->
        pif (plengthBS # value #== 32) (pcon (PJust value)) (pcon PNothing)

pdataIsInteger, pdataIsBytes, pdataIsList :: forall (s :: S). Term s (PData :--> PBool)
pdataIsInteger = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse pfalse ptrue pfalse
pdataIsBytes = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse pfalse pfalse ptrue
pdataIsList = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse ptrue pfalse pfalse

pchoose5 ::
  forall (s :: S).
  Term s PData ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool
pchoose5 d a b c e f = pchooseData # d # a # b # c # e # f

ptrue, pfalse :: forall (s :: S). Term s PBool
ptrue = pconstant @PBool True
pfalse = pconstant @PBool False

--------------------------------------------------------------------------------
-- The empty roots
--------------------------------------------------------------------------------

{- | Aiken @empty_sequence_root_v1@ and its two siblings.

@0x8100@ is @[0]@ — a one-item array holding the tag zero — so the empty
sequence is a /node/ with a shape, not the hash of nothing. All three differ
only in their domain.
-}
pemptySequenceRootV1, pemptyEnvironmentRootV1, pemptyContinuationRootV1 ::
  forall (s :: S). Term s PByteString
pemptySequenceRootV1 = phashUnder psequenceNodeDomain (pconstant "\x81\x00")
pemptyEnvironmentRootV1 = phashUnder penvironmentNodeDomain (pconstant "\x81\x00")
pemptyContinuationRootV1 = phashUnder pcontinuationNodeDomain (pconstant "\x81\x00")

--------------------------------------------------------------------------------
-- Term roots
--------------------------------------------------------------------------------

phashTermPreimage :: forall (s :: S). Term s (PByteString :--> PByteString)
phashTermPreimage = phoistAcyclic $ plam $ phashUnder ptermNodeDomain

-- | Aiken @hash_variable_term_v1@ — a de Bruijn index.
phashVariableTermV1 :: forall (s :: S). Term s (PInteger :--> PByteString)
phashVariableTermV1 = phoistAcyclic $
  plam $ \index ->
    phashTermPreimage
      #$ pconstant "\x82"
      <> pcborInt 0
      <> pcborInt (pexpectUint32 # index)

{- | Aiken @hash_delay_term_v1@, @hash_lambda_term_v1@ and @hash_force_term_v1@.

The three are one shape with a different tag, which is why the material type
folds them into @UnaryTerm@.
-}
punaryTerm :: forall (s :: S). Term s PInteger -> Term s (PByteString :--> PByteString)
punaryTerm tag = plam $ \child ->
  phashTermPreimage
    #$ pconstant "\x82"
    <> pcborInt tag
    <> (pencodeDefiniteBytes #$ pexpectHash # child)

phashDelayTermV1, phashLambdaTermV1, phashForceTermV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString)
phashDelayTermV1 = phoistAcyclic $ punaryTerm 1
phashLambdaTermV1 = phoistAcyclic $ punaryTerm 2
phashForceTermV1 = phoistAcyclic $ punaryTerm 5

-- | Aiken @hash_application_term_v1@.
phashApplicationTermV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashApplicationTermV1 = phoistAcyclic $
  plam $ \function argument ->
    phashTermPreimage
      #$ pconstant "\x83"
      <> pcborInt 3
      <> (pencodeDefiniteBytes #$ pexpectHash # function)
      <> (pencodeDefiniteBytes #$ pexpectHash # argument)

-- | Aiken @hash_constant_term_v1@.
phashConstantTermV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
phashConstantTermV1 = phoistAcyclic $ punaryTerm 4

{- | Aiken @hash_context_constant_term_v1@.

Tag 10, the runtime-only constant holding the validation machine's
authenticated script context. A canonical source program never admits it, and
'pinspectProgramTermMaterialV1' has no branch that produces it.
-}
phashContextConstantTermV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
phashContextConstantTermV1 = phoistAcyclic $ punaryTerm 10

-- | Aiken @hash_error_term_v1@ — the only nullary term.
phashErrorTermV1 :: forall (s :: S). Term s PByteString
phashErrorTermV1 = phashTermPreimage # (pconstant "\x81" <> pcborInt 6)

-- | Aiken @hash_builtin_term_v1@.
phashBuiltinTermV1 :: forall (s :: S). Term s (PInteger :--> PByteString)
phashBuiltinTermV1 = phoistAcyclic $
  plam $ \tag ->
    pif (0 #<= tag #&& tag #<= pmaxBuiltinTag) `flip` perror $
      phashTermPreimage #$ pconstant "\x82" <> pcborInt 7 <> pcborInt tag

-- | Aiken @hash_constr_term_v1@.
phashConstrTermV1 ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
phashConstrTermV1 = phoistAcyclic $
  plam $ \tag termsCount termsRoot ->
    phashTermPreimage
      #$ pconstant "\x84"
      <> pcborInt 8
      <> pcborInt (pexpectUint64 # tag)
      <> pcborInt (pexpectUint32 # termsCount)
      <> (pencodeDefiniteBytes #$ pexpectHash # termsRoot)

-- | Aiken @hash_case_term_v1@.
phashCaseTermV1 ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PByteString :--> PByteString)
phashCaseTermV1 = phoistAcyclic $
  plam $ \scrutinee branchesCount branchesRoot ->
    phashTermPreimage
      #$ pconstant "\x84"
      <> pcborInt 9
      <> (pencodeDefiniteBytes #$ pexpectHash # scrutinee)
      <> pcborInt (pexpectUint32 # branchesCount)
      <> (pencodeDefiniteBytes #$ pexpectHash # branchesRoot)

--------------------------------------------------------------------------------
-- Value roots
--------------------------------------------------------------------------------

phashValuePreimage :: forall (s :: S). Term s (PByteString :--> PByteString)
phashValuePreimage = phoistAcyclic $ plam $ phashUnder pvalueNodeDomain

{- | Aiken @hash_constant_value_v1@.

Two roots for the same payload: @payload_root@ is the blob commitment the CEK
charges bytes against, @semantic_root@ the "Midgard.CekData" commitment a
@Data@ constant is compared under. They coincide exactly when the constant is a
@Data@ whose blob and semantic views agree, which is what
'psourceProgramMaterialChildrenV1' requires of a published program.
-}
phashConstantValueV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PByteString
    )
phashConstantValueV1 = phoistAcyclic $
  plam $ \typeRoot payloadRoot payloadLength semanticRoot memory ->
    phashValuePreimage
      #$ pconstant "\x86"
      <> pcborInt 0
      <> (pencodeDefiniteBytes #$ pexpectHash # typeRoot)
      <> (pencodeDefiniteBytes #$ pexpectHash # payloadRoot)
      <> pcborInt (pexpectUint64 # payloadLength)
      <> (pencodeDefiniteBytes #$ pexpectHash # semanticRoot)
      <> pcborInt (pexpectUint64 # memory)

pclosureValue ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PByteString :--> PByteString :--> PByteString)
pclosureValue tag = plam $ \body environment ->
  phashValuePreimage
    #$ pconstant "\x83"
    <> pcborInt tag
    <> (pencodeDefiniteBytes #$ pexpectHash # body)
    <> (pencodeDefiniteBytes #$ pexpectHash # environment)

-- | Aiken @hash_lambda_value_v1@ and @hash_delay_value_v1@ — the two closures.
phashLambdaValueV1, phashDelayValueV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashLambdaValueV1 = phoistAcyclic $ pclosureValue 1
phashDelayValueV1 = phoistAcyclic $ pclosureValue 2

-- | Aiken @hash_constr_value_v1@.
phashConstrValueV1 ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
phashConstrValueV1 = phoistAcyclic $
  plam $ \tag valuesCount valuesRoot ->
    phashValuePreimage
      #$ pconstant "\x84"
      <> pcborInt 3
      <> pcborInt (pexpectUint64 # tag)
      <> pcborInt (pexpectUint32 # valuesCount)
      <> (pencodeDefiniteBytes #$ pexpectHash # valuesRoot)

-- | Aiken @hash_builtin_value_v1@ — a builtin part-way through its saturation.
phashBuiltinValueV1 ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PByteString :--> PByteString)
phashBuiltinValueV1 = phoistAcyclic $
  plam $ \tag forcesRemaining argumentsCount argumentsRoot ->
    pif (0 #<= tag #&& tag #<= pmaxBuiltinTag) `flip` perror $
      phashValuePreimage
        #$ pconstant "\x85"
        <> pcborInt 4
        <> pcborInt tag
        <> pcborInt (pexpectUint32 # forcesRemaining)
        <> pcborInt (pexpectUint32 # argumentsCount)
        <> (pencodeDefiniteBytes #$ pexpectHash # argumentsRoot)

{- | Aiken @hash_bls_miller_loop_value_v1@.

A Miller-loop result is not a byte string a proof can carry, so it travels as
the root of the expression that produced it and is compared structurally.
-}
phashBlsMillerLoopValueV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
phashBlsMillerLoopValueV1 = phoistAcyclic $
  plam $ \expressionRoot ->
    phashValuePreimage
      #$ pconstant "\x82"
      <> pcborInt 5
      <> (pencodeDefiniteBytes #$ pexpectHash # expressionRoot)

pblsExpression ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PByteString :--> PByteString :--> PByteString)
pblsExpression tag = plam $ \left right ->
  phashUnder pblsExpressionDomain $
    pconstant "\x83"
      <> pcborInt tag
      <> (pencodeDefiniteBytes #$ pexpectHash # left)
      <> (pencodeDefiniteBytes #$ pexpectHash # right)

-- | Aiken @hash_bls_miller_loop_expression_v1@ and @hash_bls_multiply_expression_v1@.
phashBlsMillerLoopExpressionV1, phashBlsMultiplyExpressionV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashBlsMillerLoopExpressionV1 = phoistAcyclic $ pblsExpression 0
phashBlsMultiplyExpressionV1 = phoistAcyclic $ pblsExpression 1

--------------------------------------------------------------------------------
-- Sequence and environment links
--------------------------------------------------------------------------------

plinkNode ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
plinkNode domain = plam $ \item tail len ->
  pif (0 #< len) `flip` perror $
    phashUnder domain $
      pconstant "\x84"
        <> pcborInt 1
        <> (pencodeDefiniteBytes #$ pexpectHash # item)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)
        <> pcborInt (pexpectUint32 # len)

{- | Aiken @hash_sequence_node_v1@ and @hash_environment_node_v1@.

Byte-identical preimages under two different domains — the only thing keeping a
term sequence from being read as an environment.
-}
phashSequenceNodeV1, phashEnvironmentNodeV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
phashSequenceNodeV1 = phoistAcyclic $ plinkNode psequenceNodeDomain
phashEnvironmentNodeV1 = phoistAcyclic $ plinkNode penvironmentNodeDomain

--------------------------------------------------------------------------------
-- Continuation frames
--------------------------------------------------------------------------------

pcontinuation :: forall (s :: S). Term s PByteString -> Term s PByteString
pcontinuation = phashUnder pcontinuationNodeDomain

-- | Aiken @hash_force_continuation_v1@.
phashForceContinuationV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
phashForceContinuationV1 = phoistAcyclic $
  plam $ \tail ->
    pcontinuation $
      pconstant "\x83"
        <> pcborInt 1
        <> pcborInt 0
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)

-- | Aiken @hash_apply_argument_continuation_v1@.
phashApplyArgumentContinuationV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PByteString)
phashApplyArgumentContinuationV1 = phoistAcyclic $
  plam $ \argument environment tail ->
    pcontinuation $
      pconstant "\x85"
        <> pcborInt 1
        <> pcborInt 1
        <> (pencodeDefiniteBytes #$ pexpectHash # argument)
        <> (pencodeDefiniteBytes #$ pexpectHash # environment)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)

-- | Aiken @hash_apply_function_continuation_v1@.
phashApplyFunctionContinuationV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashApplyFunctionContinuationV1 = phoistAcyclic $
  plam $ \functionValue tail ->
    pcontinuation $
      pconstant "\x84"
        <> pcborInt 1
        <> pcborInt 2
        <> (pencodeDefiniteBytes #$ pexpectHash # functionValue)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)

-- | Aiken @hash_constr_continuation_v1@ — the widest frame, at nine items.
phashConstrContinuationV1 ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
    )
phashConstrContinuationV1 = phoistAcyclic $
  plam $ \tag remainingCount remainingRoot valuesCount valuesRoot environment tail ->
    pcontinuation $
      (pencodeDefiniteArrayHeader # 9)
        <> pcborInt 1
        <> pcborInt 3
        <> pcborInt (pexpectUint64 # tag)
        <> pcborInt (pexpectUint32 # remainingCount)
        <> (pencodeDefiniteBytes #$ pexpectHash # remainingRoot)
        <> pcborInt (pexpectUint32 # valuesCount)
        <> (pencodeDefiniteBytes #$ pexpectHash # valuesRoot)
        <> (pencodeDefiniteBytes #$ pexpectHash # environment)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)

-- | Aiken @hash_case_continuation_v1@.
phashCaseContinuationV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
phashCaseContinuationV1 = phoistAcyclic $
  plam $ \branchesCount branchesRoot environment tail ->
    pcontinuation $
      pconstant "\x86"
        <> pcborInt 1
        <> pcborInt 4
        <> pcborInt (pexpectUint32 # branchesCount)
        <> (pencodeDefiniteBytes #$ pexpectHash # branchesRoot)
        <> (pencodeDefiniteBytes #$ pexpectHash # environment)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)

-- | Aiken @hash_apply_value_continuation_v1@.
phashApplyValueContinuationV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashApplyValueContinuationV1 = phoistAcyclic $
  plam $ \value tail ->
    pcontinuation $
      pconstant "\x84"
        <> pcborInt 1
        <> pcborInt 5
        <> (pencodeDefiniteBytes #$ pexpectHash # value)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)

-- | Aiken @hash_case_select_continuation_v1@.
phashCaseSelectContinuationV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
phashCaseSelectContinuationV1 = phoistAcyclic $
  plam $ \environment tail valuesCount ->
    pcontinuation $
      pconstant "\x85"
        <> pcborInt 1
        <> pcborInt 6
        <> (pencodeDefiniteBytes #$ pexpectHash # environment)
        <> (pencodeDefiniteBytes #$ pexpectHash # tail)
        <> pcborInt (pexpectUint32 # valuesCount)

-- | Aiken @hash_case_apply_continuation_v1@.
phashCaseApplyContinuationV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashCaseApplyContinuationV1 = phoistAcyclic $
  plam $ \environment builtContinuation ->
    pcontinuation $
      pconstant "\x84"
        <> pcborInt 1
        <> pcborInt 7
        <> (pencodeDefiniteBytes #$ pexpectHash # environment)
        <> (pencodeDefiniteBytes #$ pexpectHash # builtContinuation)

--------------------------------------------------------------------------------
-- Blobs
--------------------------------------------------------------------------------

-- | Aiken @hash_blob_chunk_v1@.
phashBlobChunkV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
phashBlobChunkV1 = phoistAcyclic $
  plam $ \chunk ->
    pif (plengthBS # chunk #<= pmaxBlobChunkBytesV1) `flip` perror $
      phashUnder pblobChunkDomain (pencodeDefiniteBytes # chunk)

-- | Aiken @hash_blob_branch_v1@.
phashBlobBranchV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
phashBlobBranchV1 = phoistAcyclic $
  plam $ \left right byteLength ->
    phashUnder pblobBranchDomain $
      pconstant "\x83"
        <> (pencodeDefiniteBytes #$ pexpectHash # left)
        <> (pencodeDefiniteBytes #$ pexpectHash # right)
        <> pcborInt (pexpectUint64 # byteLength)

{- | Aiken @bounded_blob_root_v1@.

At most three chunks, so at most two branches: the bound is 9,215 bytes and a
chunk is 4,095. Written out rather than folded, exactly as the Aiken is, because
the shapes of the two- and three-chunk trees differ.
-}
pboundedBlobRootV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
pboundedBlobRootV1 = phoistAcyclic $
  plam $ \bytes ->
    plet (plengthBS # bytes) $ \len ->
      pif (len #<= pmaxBoundedBlobBytesV1) `flip` perror $
        pif (len #<= pmaxBlobChunkBytesV1) (phashBlobChunkV1 # bytes) $
          plet (len - pmaxBlobChunkBytesV1) $ \remaining ->
            plet
              (pif (remaining #<= pmaxBlobChunkBytesV1) remaining pmaxBlobChunkBytesV1)
              $ \secondLength ->
                plet
                  ( phashBlobBranchV1
                      # (phashBlobChunkV1 #$ psliceLen # bytes # 0 # pmaxBlobChunkBytesV1)
                      # ( phashBlobChunkV1
                            #$ psliceLen # bytes # pmaxBlobChunkBytesV1 # secondLength
                        )
                      # (pmaxBlobChunkBytesV1 + secondLength)
                  )
                  $ \left ->
                    pif (remaining #<= pmaxBlobChunkBytesV1) left $
                      phashBlobBranchV1
                        # left
                        # ( phashBlobChunkV1
                              #$ psliceLen
                                # bytes
                                # (pmaxBlobChunkBytesV1 + secondLength)
                                # (remaining - secondLength)
                          )
                        # len

--------------------------------------------------------------------------------
-- Machine states and envelopes
--------------------------------------------------------------------------------

{- | Aiken @hash_machine_state_v1@.

The whole of a CEK configuration in one hash: the mode, where in the trace it
sits, the three roots, one mode-specific auxiliary number and the two budgets.
Every fault proof over a step is a claim about two of these.
-}
phashMachineStateV1 ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
    )
phashMachineStateV1 = phoistAcyclic $
  plam $ \mode executionIndex focusRoot environmentRoot continuationRoot auxiliary cpu memory ->
    pif (0 #<= mode #&& mode #<= 8) `flip` perror $
      phashUnder pmachineStateDomain $
        (pencodeDefiniteArrayHeader # 9)
          <> pcborInt 1
          <> pcborInt mode
          <> pcborInt (pexpectUint32 # executionIndex)
          <> (pencodeDefiniteBytes #$ pexpectHash # focusRoot)
          <> (pencodeDefiniteBytes #$ pexpectHash # environmentRoot)
          <> (pencodeDefiniteBytes #$ pexpectHash # continuationRoot)
          <> pcborInt (pexpectUint64 # auxiliary)
          <> pcborInt (pexpectUint64 # cpu)
          <> pcborInt (pexpectUint64 # memory)

-- | Aiken @encode_program_envelope_v1@.
pencodeProgramEnvelopeV1 ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
    )
pencodeProgramEnvelopeV1 = phoistAcyclic $
  plam $ \major minor patch termRoot nodeCount materialByteLength ->
    pconstant "\x85"
      <> pcborInt 1
      <> ( pconstant "\x83"
             <> pcborInt (pexpectUint32 # major)
             <> pcborInt (pexpectUint32 # minor)
             <> pcborInt (pexpectUint32 # patch)
         )
      <> (pencodeDefiniteBytes #$ pexpectHash # termRoot)
      <> pcborInt (pexpectUint32 # nodeCount)
      <> pcborInt (pexpectUint64 # materialByteLength)

-- | Aiken @hash_program_envelope_v1@.
phashProgramEnvelopeV1 ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
    )
phashProgramEnvelopeV1 = phoistAcyclic $
  plam $ \major minor patch termRoot nodeCount materialByteLength ->
    phashUnder pprogramEnvelopeDomain $
      pencodeProgramEnvelopeV1
        # major
        # minor
        # patch
        # termRoot
        # nodeCount
        # materialByteLength

--------------------------------------------------------------------------------
-- Material encoding
--------------------------------------------------------------------------------

{- | Aiken @encode_program_term_material_v1@.

Note what is /not/ here: no @expect_hash@, no @expect_uint32@. The encoder is
only ever reached from an inspector that has already range-checked every field,
and from the re-encoding comparison at the end of that same inspector. Adding
the assertions would turn a decline into an abort.
-}
pencodeProgramTermMaterialV1 ::
  forall (s :: S). Term s (PProgramTermMaterialV1 :--> PByteString)
pencodeProgramTermMaterialV1 = phoistAcyclic $
  plam $ \term ->
    pmatch term $ \case
      PVariableTerm {pterm'index} ->
        pconstant "\x82" <> pcborInt 0 <> pcborInt (pfromData pterm'index)
      PUnaryTerm {pterm'tag, pterm'child} ->
        pconstant "\x82"
          <> pcborInt (pfromData pterm'tag)
          <> (pencodeDefiniteBytes # pfromData pterm'child)
      PApplicationTerm {pterm'function, pterm'argument} ->
        pconstant "\x83"
          <> pcborInt 3
          <> (pencodeDefiniteBytes # pfromData pterm'function)
          <> (pencodeDefiniteBytes # pfromData pterm'argument)
      PConstantTerm {pterm'value} ->
        pconstant "\x82" <> pcborInt 4 <> (pencodeDefiniteBytes # pfromData pterm'value)
      PErrorTerm -> pconstant "\x81" <> pcborInt 6
      PBuiltinTerm {pterm'tag} ->
        pconstant "\x82" <> pcborInt 7 <> pcborInt (pfromData pterm'tag)
      PConstrTerm {pterm'tag, pterm'count, pterm'sequence} ->
        pconstant "\x84"
          <> pcborInt 8
          <> pcborInt (pfromData pterm'tag)
          <> pcborInt (pfromData pterm'count)
          <> (pencodeDefiniteBytes # pfromData pterm'sequence)
      PCaseTerm {pterm'scrutinee, pterm'count, pterm'sequence} ->
        pconstant "\x84"
          <> pcborInt 9
          <> (pencodeDefiniteBytes # pfromData pterm'scrutinee)
          <> pcborInt (pfromData pterm'count)
          <> (pencodeDefiniteBytes # pfromData pterm'sequence)

-- | Aiken @encode_program_value_material_v1@.
pencodeProgramValueMaterialV1 ::
  forall (s :: S). Term s (PProgramValueMaterialV1 :--> PByteString)
pencodeProgramValueMaterialV1 = phoistAcyclic $
  plam $ \value ->
    pmatch value $ \(PConstantValue typeRoot payloadRoot payloadLength semanticRoot memory) ->
      pconstant "\x86"
        <> pcborInt 0
        <> (pencodeDefiniteBytes # pfromData typeRoot)
        <> (pencodeDefiniteBytes # pfromData payloadRoot)
        <> pcborInt (pfromData payloadLength)
        <> (pencodeDefiniteBytes # pfromData semanticRoot)
        <> pcborInt (pfromData memory)

-- | Aiken @encode_program_sequence_material_v1@.
pencodeProgramSequenceMaterialV1 ::
  forall (s :: S). Term s (PProgramSequenceMaterialV1 :--> PByteString)
pencodeProgramSequenceMaterialV1 = phoistAcyclic $
  plam $ \value ->
    pmatch value $ \(PProgramSequenceMaterialV1 head tail len) ->
      pconstant "\x84"
        <> pcborInt 1
        <> (pencodeDefiniteBytes # pfromData head)
        <> (pencodeDefiniteBytes # pfromData tail)
        <> pcborInt (pfromData len)

{- | Aiken @encode_program_blob_material_v1@.

A chunk's preimage is a bare CBOR byte string, not an array — which is why
'pinspectProgramBlobMaterialV1' takes the kind as an argument rather than
inferring it from the shape.
-}
pencodeProgramBlobMaterialV1 ::
  forall (s :: S). Term s (PProgramBlobMaterialV1 :--> PByteString)
pencodeProgramBlobMaterialV1 = phoistAcyclic $
  plam $ \value ->
    pmatch value $ \case
      PBlobChunk {pblob'bytes} -> pencodeDefiniteBytes # pfromData pblob'bytes
      PBlobBranch {pblob'left, pblob'right, pblob'byteLength} ->
        pconstant "\x83"
          <> (pencodeDefiniteBytes # pfromData pblob'left)
          <> (pencodeDefiniteBytes # pfromData pblob'right)
          <> pcborInt (pfromData pblob'byteLength)

--------------------------------------------------------------------------------
-- Material inspection
--------------------------------------------------------------------------------

-- | The decoded array of a preimage, if it is one.
pdecodedList ::
  forall (s :: S). Term s (PByteString :--> PMaybe (PBuiltinList PData))
pdecodedList = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdeserialise # preimage) $ \case
      PNothing -> pcon PNothing
      PJust decoded ->
        pif
          (pdataIsList # decoded)
          (pcon (PJust (pasList # decoded)))
          (pcon PNothing)

pitemAt :: forall (s :: S). Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pitemAt items index = pelemAt # index # items

{- | Aiken @inspect_program_term_material_v1@.

Dispatches on the decoded array's arity, then on the tag, and finally on whether
the second item is an integer or a 32-byte hash — which is what separates a
variable from a delay at the same arity. Every branch ends by re-encoding and
comparing, so nothing non-canonical survives.

Tag 10 has no branch. A context constant may appear in machine evidence but
never in a published program, and this is the only place that distinction is
made.
-}
pinspectProgramTermMaterialV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PProgramTermMaterialV1)
pinspectProgramTermMaterialV1 = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdecodedList # preimage) $ \case
      PNothing -> pcon PNothing
      PJust items ->
        plet (plength # items) $ \arity ->
          pif (arity #== 2) (pinspectTermArity2 preimage items) $
            pif (arity #== 1) (pinspectTermArity1 preimage items) $
              pif (arity #== 3) (pinspectTermArity3 preimage items) $
                pif (arity #== 4) (pinspectTermArity4 preimage items) $
                  pcon PNothing

pfinishTerm ::
  forall (s :: S).
  Term s PByteString ->
  Term s PProgramTermMaterialV1 ->
  Term s (PMaybe PProgramTermMaterialV1)
pfinishTerm preimage term =
  plet term $ \t ->
    pif
      (pencodeProgramTermMaterialV1 # t #== preimage)
      (pcon (PJust t))
      (pcon PNothing)

-- | Variable and builtin, or the three unary shapes and a constant.
pinspectTermArity2 ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PProgramTermMaterialV1)
pinspectTermArity2 preimage items =
  plet (pitemAt items 0) $ \tagData ->
    plet (pitemAt items 1) $ \secondData ->
      pif (pnot # (pdataIsInteger # tagData)) (pcon PNothing) $
        plet (pasInt # tagData) $ \tag ->
          pif (pdataIsInteger # secondData) `flip` (punaryOrConstant preimage tag secondData) $
            plet (pasInt # secondData) $ \index ->
              pif
                (tag #== 0 #&& 0 #<= index #&& index #<= puint32Max)
                (pfinishTerm preimage $ pcon $ PVariableTerm (pdata index))
                $ pif
                  (tag #== 7 #&& 0 #<= index #&& index #<= pmaxBuiltinTag)
                  (pfinishTerm preimage $ pcon $ PBuiltinTerm (pdata index))
                  (pcon PNothing)

punaryOrConstant ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PData ->
  Term s (PMaybe PProgramTermMaterialV1)
punaryOrConstant preimage tag secondData =
  pmatch (pdataHash # secondData) $ \case
    PNothing -> pcon PNothing
    PJust child ->
      pif
        (tag #== 1 #|| tag #== 2 #|| tag #== 5)
        (pfinishTerm preimage $ pcon $ PUnaryTerm (pdata tag) (pdata child))
        $ pif
          (tag #== 4)
          (pfinishTerm preimage $ pcon $ PConstantTerm (pdata child))
          (pcon PNothing)

-- | The error term, the only one-item shape.
pinspectTermArity1 ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PProgramTermMaterialV1)
pinspectTermArity1 preimage items =
  plet (pitemAt items 0) $ \tagData ->
    pif
      (pdataIsInteger # tagData #&& pasInt # tagData #== 6)
      (pfinishTerm preimage $ pcon PErrorTerm)
      (pcon PNothing)

-- | Application, the only three-item shape.
pinspectTermArity3 ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PProgramTermMaterialV1)
pinspectTermArity3 preimage items =
  plet (pitemAt items 0) $ \tagData ->
    pif (pnot # (pdataIsInteger # tagData)) (pcon PNothing) $
      pmatch (pdataHash # pitemAt items 1) $ \case
        PNothing -> pcon PNothing
        PJust function ->
          pmatch (pdataHash # pitemAt items 2) $ \case
            PNothing -> pcon PNothing
            PJust argument ->
              pif
                (pasInt # tagData #== 3)
                ( pfinishTerm preimage $
                    pcon $
                      PApplicationTerm (pdata function) (pdata argument)
                )
                (pcon PNothing)

-- | Constr and case, which share an arity and differ in their second item.
pinspectTermArity4 ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PProgramTermMaterialV1)
pinspectTermArity4 preimage items =
  plet (pitemAt items 0) $ \tagData ->
    plet (pitemAt items 1) $ \firstData ->
      plet (pitemAt items 2) $ \countData ->
        pif
          (pnot # (pdataIsInteger # tagData #&& pdataIsInteger # countData))
          (pcon PNothing)
          $ plet (pasInt # tagData) $ \tag ->
            plet (pasInt # countData) $ \count ->
              pmatch (pdataHash # pitemAt items 3) $ \case
                PNothing -> pcon PNothing
                PJust sequence ->
                  pif (pnot # (0 #<= count #&& count #<= puint32Max)) (pcon PNothing) $
                    pif (tag #== 8 #&& pdataIsInteger # firstData) `flip` (pinspectCaseTerm preimage tag firstData count sequence) $
                      plet (pasInt # firstData) $ \constrTag ->
                        pif
                          (0 #<= constrTag #&& constrTag #<= puint64Max)
                          ( pfinishTerm preimage $
                              pcon $
                                PConstrTerm (pdata constrTag) (pdata count) (pdata sequence)
                          )
                          (pcon PNothing)

pinspectCaseTerm ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PData ->
  Term s PInteger ->
  Term s PByteString ->
  Term s (PMaybe PProgramTermMaterialV1)
pinspectCaseTerm preimage tag firstData count sequence =
  pif (pnot # (tag #== 9)) (pcon PNothing) $
    pmatch (pdataHash # firstData) $ \case
      PNothing -> pcon PNothing
      PJust scrutinee ->
        pfinishTerm preimage $
          pcon $
            PCaseTerm (pdata scrutinee) (pdata count) (pdata sequence)

{- | Aiken @program_term_child_roots_v1@.

Read by tag rather than by @pmatch@: three of the eight arms are the empty list,
and a @pmatch@ whose arms have identical bodies mis-compiles in Plutarch — no arm
is selected and the wildcard is taken. The constructor order is variable, unary,
application, constant, error, builtin, constr, case, and the field positions are
written next to each read.
-}
pprogramTermChildRootsV1 ::
  forall (s :: S).
  Term s (PProgramTermMaterialV1 :--> PBuiltinList (PAsData PByteString))
pprogramTermChildRootsV1 = phoistAcyclic $
  plam $ \term ->
    let (tag, fields) = pconstrOf (pdata term)
     in plet tag $ \kind ->
          -- 0 variable, 4 error, 5 builtin: no children.
          pif (kind #== 0 #|| kind #== 4 #|| kind #== 5) (pcon PNil) $
            -- 1 unary: child at 1. 3 constant: value at 0. 6 constr: sequence at 2.
            pif (kind #== 1) (psingletonRoot fields 1) $
              pif (kind #== 3) (psingletonRoot fields 0) $
                pif (kind #== 6) (psingletonRoot fields 2) $
                  -- 2 application: function 0, argument 1.
                  pif
                    (kind #== 2)
                    (ppairRoots fields 0 1)
                    -- 7 case: scrutinee 0, sequence 2.
                    (ppairRoots fields 0 2)

psingletonRoot ::
  forall (s :: S).
  Term s (PBuiltinList PData) ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PByteString))
psingletonRoot fields index = pcons # pbytesAt fields index # pnil

ppairRoots ::
  forall (s :: S).
  Term s (PBuiltinList PData) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PByteString))
ppairRoots fields a b =
  pcons # pbytesAt fields a # (pcons # pbytesAt fields b # pnil)

pbytesAt ::
  forall (s :: S).
  Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PByteString)
pbytesAt fields index = pdata (pasByteStr # (pelemAt # index # fields))

-- | Aiken @inspect_program_value_material_v1@ — the one constant shape.
pinspectProgramValueMaterialV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PProgramValueMaterialV1)
pinspectProgramValueMaterialV1 = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdecodedList # preimage) $ \case
      PNothing -> pcon PNothing
      PJust items ->
        pif (pnot # (plength # items #== 6)) (pcon PNothing) $
          plet (pitemAt items 0) $ \tagData ->
            plet (pitemAt items 3) $ \payloadLengthData ->
              plet (pitemAt items 5) $ \memoryData ->
                pif
                  ( pnot
                      #$ pand'List
                        [ pdataIsInteger # tagData
                        , pdataIsInteger # payloadLengthData
                        , pdataIsInteger # memoryData
                        ]
                  )
                  (pcon PNothing)
                  $ pif (pnot # (pasInt # tagData #== 0)) (pcon PNothing)
                  $ pmatch (pdataHash # pitemAt items 1)
                  $ \case
                    PNothing -> pcon PNothing
                    PJust typeRoot ->
                      pmatch (pdataHash # pitemAt items 2) $ \case
                        PNothing -> pcon PNothing
                        PJust payloadRoot ->
                          pmatch (pdataHash # pitemAt items 4) $ \case
                            PNothing -> pcon PNothing
                            PJust semanticRoot ->
                              pcheckConstantValue
                                preimage
                                typeRoot
                                payloadRoot
                                (pasInt # payloadLengthData)
                                semanticRoot
                                (pasInt # memoryData)

pcheckConstantValue ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PMaybe PProgramValueMaterialV1)
pcheckConstantValue preimage typeRoot payloadRoot payloadLength semanticRoot memory =
  plet payloadLength $ \len ->
    plet memory $ \mem ->
      pif
        ( pnot
            #$ pand'List
              [0 #<= len, len #<= puint64Max, 0 #<= mem, mem #<= puint64Max]
        )
        (pcon PNothing)
        $ plet
          ( pcon $
              PConstantValue
                (pdata typeRoot)
                (pdata payloadRoot)
                (pdata len)
                (pdata semanticRoot)
                (pdata mem)
          )
        $ \value ->
          pif
            (pencodeProgramValueMaterialV1 # value #== preimage)
            (pcon (PJust value))
            (pcon PNothing)

-- | Aiken @inspect_program_sequence_material_v1@.
pinspectProgramSequenceMaterialV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PProgramSequenceMaterialV1)
pinspectProgramSequenceMaterialV1 = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdecodedList # preimage) $ \case
      PNothing -> pcon PNothing
      PJust items ->
        pif (pnot # (plength # items #== 4)) (pcon PNothing) $
          plet (pitemAt items 0) $ \tagData ->
            plet (pitemAt items 3) $ \lengthData ->
              pif
                ( pnot
                    #$ pand'List
                      [ pdataIsInteger # tagData
                      , pdataIsInteger # lengthData
                      , pdataIsInteger # tagData #&& pasInt # tagData #== 1
                      ]
                )
                (pcon PNothing)
                $ pmatch (pdataHash # pitemAt items 1)
                $ \case
                  PNothing -> pcon PNothing
                  PJust head ->
                    pmatch (pdataHash # pitemAt items 2) $ \case
                      PNothing -> pcon PNothing
                      PJust tail ->
                        plet (pasInt # lengthData) $ \len ->
                          pif (pnot # (0 #< len #&& len #<= puint32Max)) (pcon PNothing) $
                            plet
                              ( pcon $
                                  PProgramSequenceMaterialV1
                                    (pdata head)
                                    (pdata tail)
                                    (pdata len)
                              )
                              $ \node ->
                                pif
                                  (pencodeProgramSequenceMaterialV1 # node #== preimage)
                                  (pcon (PJust node))
                                  (pcon PNothing)

{- | Aiken @inspect_program_blob_material_v1@.

The kind is an argument, not a shape read: a chunk preimage is a bare CBOR byte
string and a branch preimage a three-item array, and reading the kind off the
shape would let a caller that asked for a chunk be handed a branch.
-}
pinspectProgramBlobMaterialV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PMaybe PProgramBlobMaterialV1)
pinspectProgramBlobMaterialV1 = phoistAcyclic $
  plam $ \kind preimage ->
    pif (kind #== 3) (pinspectBlobChunk preimage) $
      pif (kind #== 4) (pinspectBlobBranch preimage) (pcon PNothing)

pinspectBlobChunk ::
  forall (s :: S). Term s PByteString -> Term s (PMaybe PProgramBlobMaterialV1)
pinspectBlobChunk preimage =
  pmatch (pdeserialise # preimage) $ \case
    PNothing -> pcon PNothing
    PJust decoded ->
      pif (pnot # (pdataIsBytes # decoded)) (pcon PNothing) $
        plet (pasByteStr # decoded) $ \bytes ->
          pif (pnot # (plengthBS # bytes #<= pmaxBlobChunkBytesV1)) (pcon PNothing) $
            plet (pcon (PBlobChunk (pdata bytes))) $ \chunk ->
              pif
                (pencodeProgramBlobMaterialV1 # chunk #== preimage)
                (pcon (PJust chunk))
                (pcon PNothing)

pinspectBlobBranch ::
  forall (s :: S). Term s PByteString -> Term s (PMaybe PProgramBlobMaterialV1)
pinspectBlobBranch preimage =
  pmatch (pdecodedList # preimage) $ \case
    PNothing -> pcon PNothing
    PJust items ->
      pif (pnot # (plength # items #== 3)) (pcon PNothing) $
        plet (pitemAt items 2) $ \lengthData ->
          pif (pnot # (pdataIsInteger # lengthData)) (pcon PNothing) $
            pmatch (pdataHash # pitemAt items 0) $ \case
              PNothing -> pcon PNothing
              PJust left ->
                pmatch (pdataHash # pitemAt items 1) $ \case
                  PNothing -> pcon PNothing
                  PJust right ->
                    plet (pasInt # lengthData) $ \byteLength ->
                      pif
                        (pnot # (0 #< byteLength #&& byteLength #<= puint64Max))
                        (pcon PNothing)
                        $ plet
                          ( pcon $
                              PBlobBranch (pdata left) (pdata right) (pdata byteLength)
                          )
                        $ \branch ->
                          pif
                            (pencodeProgramBlobMaterialV1 # branch #== preimage)
                            (pcon (PJust branch))
                            (pcon PNothing)

--------------------------------------------------------------------------------
-- The sidecar
--------------------------------------------------------------------------------

type PEntryList = PBuiltinList (PAsData PCekProgramMaterialDatumV1)

-- | Aiken @encode_program_material_entry_v1@ — @[root, [1, kind, preimage]]@.
pencodeProgramMaterialEntryV1 ::
  forall (s :: S). Term s (PCekProgramMaterialDatumV1 :--> PByteString)
pencodeProgramMaterialEntryV1 = phoistAcyclic $
  plam $ \entry ->
    pmatch entry $ \(PCekProgramMaterialDatumV1 kind root preimage) ->
      pconstant "\x82"
        <> (pencodeDefiniteBytes #$ pexpectHash # pfromData root)
        <> ( pconstant "\x83"
               <> pcborInt 1
               <> pcborInt (pfromData kind)
               <> (pencodeDefiniteBytes # pfromData preimage)
           )

-- | Aiken @encode_program_material_entries_v1@ — the entries, back to back.
pencodeProgramMaterialEntriesV1 :: forall (s :: S). Term s (PEntryList :--> PByteString)
pencodeProgramMaterialEntriesV1 = phoistAcyclic $
  pfix $ \self -> plam $ \entries ->
    pelimList
      (\entry rest -> (pencodeProgramMaterialEntryV1 # pfromData entry) <> (self # rest))
      (pconstant "")
      entries

-- | Aiken @encode_complete_program_material_sidecar_v1@.
pencodeCompleteProgramMaterialSidecarV1 ::
  forall (s :: S). Term s (PEntryList :--> PByteString)
pencodeCompleteProgramMaterialSidecarV1 = phoistAcyclic $
  plam $ \entries ->
    pconstant "\x82"
      <> pcborInt 1
      <> (pencodeDefiniteArrayHeader #$ plength # entries)
      <> (pencodeProgramMaterialEntriesV1 # entries)

-- | Aiken @inspect_program_material_entry_data_v1@.
pinspectProgramMaterialEntryDataV1 ::
  forall (s :: S). Term s (PData :--> PMaybe PCekProgramMaterialDatumV1)
pinspectProgramMaterialEntryDataV1 = phoistAcyclic $
  plam $ \d ->
    pif (pnot # (pdataIsList # d)) (pcon PNothing) $
      plet (pasList # d) $ \items ->
        pif (pnot # (plength # items #== 2)) (pcon PNothing) $
          pmatch (pdataHash # pitemAt items 0) $ \case
            PNothing -> pcon PNothing
            PJust root ->
              plet (pitemAt items 1) $ \valueData ->
                pif (pnot # (pdataIsList # valueData)) (pcon PNothing) $
                  plet (pasList # valueData) $ \valueItems ->
                    pif (pnot # (plength # valueItems #== 3)) (pcon PNothing) $
                      pcheckEntryFields root valueItems

pcheckEntryFields ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PCekProgramMaterialDatumV1)
pcheckEntryFields root valueItems =
  plet (pitemAt valueItems 0) $ \versionData ->
    plet (pitemAt valueItems 1) $ \kindData ->
      plet (pitemAt valueItems 2) $ \preimageData ->
        pif
          ( pnot
              #$ pand'List
                [ pdataIsInteger # versionData
                , pdataIsInteger # kindData
                , pdataIsBytes # preimageData
                ]
          )
          (pcon PNothing)
          $ pif (pnot # (pasInt # versionData #== 1)) (pcon PNothing)
          $ plet (pasInt # kindData)
          $ \kind ->
            plet (pasByteStr # preimageData) $ \preimage ->
              pif
                (0 #<= kind #&& kind #<= 7 #&& 0 #< plengthBS # preimage)
                ( pcon $
                    PJust $
                      pcon $
                        PCekProgramMaterialDatumV1
                          (pdata kind)
                          (pdata root)
                          (pdata preimage)
                )
                (pcon PNothing)

-- | Aiken @inspect_program_material_entries_data_v1@.
pinspectProgramMaterialEntriesDataV1 ::
  forall (s :: S). Term s (PBuiltinList PData :--> PMaybe PEntryList)
pinspectProgramMaterialEntriesDataV1 = phoistAcyclic $
  pfix $ \self -> plam $ \entries ->
    pelimList
      ( \entryData rest ->
          pmatch (pinspectProgramMaterialEntryDataV1 # entryData) $ \case
            PNothing -> pcon PNothing
            PJust entry ->
              pmatch (self # rest) $ \case
                PNothing -> pcon PNothing
                PJust decoded -> pcon (PJust (pcons # pdata entry # decoded))
      )
      (pcon (PJust (pcon PNil)))
      entries

{- | Aiken @strictly_sorted_material_roots_v1@.

Strictly ascending, so duplicate roots are refused as well as reordered ones.
This is what makes the walk's final @length(seen) == length(entries)@ comparison
mean "the sidecar has no unreachable entries" rather than "the sidecar is at
least as big as the program".
-}
pstrictlySortedMaterialRootsV1 :: forall (s :: S). Term s (PEntryList :--> PBool)
pstrictlySortedMaterialRootsV1 = phoistAcyclic $
  plam $ \entries -> psortedFrom # entries # pcon PNothing

psortedFrom ::
  forall (s :: S). Term s (PEntryList :--> PMaybe PByteString :--> PBool)
psortedFrom = phoistAcyclic $
  pfix $ \self -> plam $ \entries previous ->
    pelimList
      ( \entry rest ->
          plet (pentryRoot # pfromData entry) $ \root ->
            pmatch previous $ \case
              PNothing -> self # rest # pcon (PJust root)
              PJust before -> before #< root #&& (self # rest # pcon (PJust root))
      )
      ptrue
      entries

pentryRoot :: forall (s :: S). Term s (PCekProgramMaterialDatumV1 :--> PByteString)
pentryRoot = phoistAcyclic $
  plam $ \entry ->
    pmatch entry $ \(PCekProgramMaterialDatumV1 _ root _) -> pfromData root

pentryKind :: forall (s :: S). Term s (PCekProgramMaterialDatumV1 :--> PInteger)
pentryKind = phoistAcyclic $
  plam $ \entry ->
    pmatch entry $ \(PCekProgramMaterialDatumV1 kind _ _) -> pfromData kind

pentryPreimage :: forall (s :: S). Term s (PCekProgramMaterialDatumV1 :--> PByteString)
pentryPreimage = phoistAcyclic $
  plam $ \entry ->
    pmatch entry $ \(PCekProgramMaterialDatumV1 _ _ preimage) -> pfromData preimage

-- | Aiken @inspect_complete_program_material_sidecar_v1@.
pinspectCompleteProgramMaterialSidecarV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PEntryList)
pinspectCompleteProgramMaterialSidecarV1 = phoistAcyclic $
  plam $ \sidecarCbor ->
    pmatch (pdecodedList # sidecarCbor) $ \case
      PNothing -> pcon PNothing
      PJust items ->
        pif (pnot # (plength # items #== 2)) (pcon PNothing) $
          plet (pitemAt items 0) $ \versionData ->
            pif (pnot # (pdataIsInteger # versionData)) (pcon PNothing) $
              pif (pnot # (pasInt # versionData #== 1)) (pcon PNothing) $
                plet (pitemAt items 1) $ \entriesData ->
                  pif (pnot # (pdataIsList # entriesData)) (pcon PNothing) $
                    pmatch (pinspectProgramMaterialEntriesDataV1 #$ pasList # entriesData) $ \case
                      PNothing -> pcon PNothing
                      PJust entries ->
                        pif
                          ( pstrictlySortedMaterialRootsV1
                              # entries
                              #&& ( pencodeCompleteProgramMaterialSidecarV1 # entries
                                      #== sidecarCbor
                                  )
                          )
                          (pcon (PJust entries))
                          (pcon PNothing)

{- | Aiken @program_material_root_matches_v1@.

Every entry must both parse under its declared kind and hash, under that kind's
domain, to the root it is filed at. Kinds 5, 6 and 7 hand off to
"Midgard.CekData"; the rest are this module's own.
-}
pprogramMaterialRootMatchesV1 ::
  forall (s :: S). Term s (PCekProgramMaterialDatumV1 :--> PBool)
pprogramMaterialRootMatchesV1 = phoistAcyclic $
  plam $ \entry ->
    plet (pentryRoot # entry) $ \root ->
      plet (pentryKind # entry) $ \kind ->
        plet (pentryPreimage # entry) $ \preimage ->
          pif (pnot # (plengthBS # root #== 32)) pfalse $
            pif (kind #== 0) (pmatchesTerm preimage root) $
              pif (kind #== 1) (pmatchesValue preimage root) $
                pif (kind #== 2) (pmatchesSequence preimage root) $
                  pif (kind #== 3 #|| kind #== 4) (pmatchesBlob kind preimage root) $
                    pif (kind #== 5) (pmatchesDataNode preimage root) $
                      pif (kind #== 6) (pmatchesDataListNode preimage root) $
                        pif (kind #== 7) (pmatchesDataPairNode preimage root) pfalse

pmatchesTerm ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesTerm preimage root =
  pmatch (pinspectProgramTermMaterialV1 # preimage) $ \case
    PNothing -> pfalse
    PJust _ -> phashTermPreimage # preimage #== root

pmatchesValue ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesValue preimage root =
  pmatch (pinspectProgramValueMaterialV1 # preimage) $ \case
    PNothing -> pfalse
    PJust _ -> phashValuePreimage # preimage #== root

pmatchesSequence ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesSequence preimage root =
  pmatch (pinspectProgramSequenceMaterialV1 # preimage) $ \case
    PNothing -> pfalse
    PJust _ -> phashUnder psequenceNodeDomain preimage #== root

pmatchesBlob ::
  forall (s :: S).
  Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesBlob kind preimage root =
  pmatch (pinspectProgramBlobMaterialV1 # kind # preimage) $ \case
    PNothing -> pfalse
    PJust _ ->
      phashUnder (pif (kind #== 3) pblobChunkDomain pblobBranchDomain) preimage #== root

pmatchesDataNode ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesDataNode preimage root =
  pmatch (CekData.pinspectDataNodePreimageV1 # preimage) $ \case
    PNothing -> pfalse
    PJust node -> CekData.phashDataNodeV1 # node #== root

pmatchesDataListNode ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesDataListNode preimage root =
  pmatch (CekData.pinspectDataListNodePreimageV1 # preimage) $ \case
    PNothing -> pfalse
    PJust node -> CekData.phashDataListNodeV1 # node #== root

pmatchesDataPairNode ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pmatchesDataPairNode preimage root =
  pmatch (CekData.pinspectDataPairNodePreimageV1 # preimage) $ \case
    PNothing -> pfalse
    PJust node -> CekData.phashDataPairNodeV1 # node #== root

--------------------------------------------------------------------------------
-- The walk
--------------------------------------------------------------------------------

{- | Aiken @ProgramMaterialTaskV1@ — one root still to be reached, and what to
read it as.

@expected_length@ is @-1@ where the parent imposes no length and a positive
count where it does, which is how a sequence's declared length is carried down
to the link that must have it. Kept out of @Data@ — it is scaffolding, never
serialised — so the list holding it is a Scott list rather than a builtin one.
-}
data PProgramMaterialTaskV1 (s :: S) = PProgramMaterialTaskV1
  { ptask'kind :: Term s PInteger
  , ptask'root :: Term s PByteString
  , ptask'expectedLength :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq)
  deriving (PlutusType) via (DeriveAsSOPStruct PProgramMaterialTaskV1)

-- | Aiken @ProgramMaterialTraversalV1@ — what the walk has accumulated.
data PProgramMaterialTraversalV1 (s :: S) = PProgramMaterialTraversalV1
  { ptraversal'seen :: Term s (PBuiltinList (PAsData PByteString))
  , ptraversal'nodeCount :: Term s PInteger
  , ptraversal'materialByteLength :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PProgramMaterialTraversalV1)

type PTaskList = PList PProgramMaterialTaskV1

-- | Aiken @material_task_v1@.
pmaterialTaskV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PProgramMaterialTaskV1
pmaterialTaskV1 kind root expectedLength =
  pcon (PProgramMaterialTaskV1 kind root expectedLength)

ptask :: forall (s :: S). Term s PInteger -> Term s PByteString -> Term s PProgramMaterialTaskV1
ptask kind root = pmaterialTaskV1 kind root (-1)

ptasks :: forall (s :: S). [Term s PProgramMaterialTaskV1] -> Term s PTaskList
ptasks = foldr (\t acc -> pcons # t # acc) pnil

-- | Aiken @find_program_material_entry_v1@ — the sidecar is a flat list.
pfindProgramMaterialEntryV1 ::
  forall (s :: S).
  Term s (PByteString :--> PEntryList :--> PMaybe PCekProgramMaterialDatumV1)
pfindProgramMaterialEntryV1 = phoistAcyclic $
  pfix $ \self -> plam $ \root entries ->
    pelimList
      ( \entry rest ->
          plet (pfromData entry) $ \e ->
            pif (pentryRoot # e #== root) (pcon (PJust e)) (self # root # rest)
      )
      (pcon PNothing)
      entries

{- | Aiken @source_program_material_children_v1@.

The task's kind and the entry's kind must agree — a term root resolved to a
value entry is a refusal, not a coercion — and a zero-count sequence must name
the canonical empty root rather than any root at all. That second rule is what
stops a program from hanging an arbitrary subtree off a count nobody reads.
-}
psourceProgramMaterialChildrenV1 ::
  forall (s :: S).
  Term
    s
    ( PProgramMaterialTaskV1
        :--> PCekProgramMaterialDatumV1
        :--> PMaybe PTaskList
    )
psourceProgramMaterialChildrenV1 = phoistAcyclic $
  plam $ \task entry ->
    pmatch task $ \(PProgramMaterialTaskV1 taskKind _ expectedLength) ->
      plet (pentryKind # entry) $ \entryKind ->
        plet (pentryPreimage # entry) $ \preimage ->
          pif (taskKind #== 0 #&& entryKind #== 0) (ptermChildren preimage) $
            pif (taskKind #== 1 #&& entryKind #== 1) (pvalueChildren preimage) $
              pif
                (taskKind #== 2 #&& entryKind #== 2)
                (psequenceChildren preimage expectedLength)
                $ pif (taskKind #== 5 #&& entryKind #== 5) (pdataNodeChildren preimage)
                $ pif
                  (taskKind #== 6 #&& entryKind #== 6)
                  (pdataListChildren preimage expectedLength)
                $ pif
                  (taskKind #== 7 #&& entryKind #== 7)
                  (pdataPairChildren preimage expectedLength)
                $ pif
                  (taskKind #== 3 #&& (entryKind #== 3 #|| entryKind #== 4))
                  (pblobChildren entryKind preimage)
                  (pcon PNothing)

{- | The children of a term.

Read by constructor tag rather than by @pmatch@: variable, error and builtin all
have no children, and three identical @pmatch@ arms are the shape Plutarch
mis-compiles. Constructor order is variable, unary, application, constant,
error, builtin, constr, case.
-}
ptermChildren :: forall (s :: S). Term s PByteString -> Term s (PMaybe PTaskList)
ptermChildren preimage =
  pmatch (pinspectProgramTermMaterialV1 # preimage) $ \case
    PNothing -> pcon PNothing
    PJust term ->
      let (tag, fields) = pconstrOf (pdata term)
       in plet tag $ \kind ->
            pif (kind #== 0 #|| kind #== 4 #|| kind #== 5) (pcon (PJust pnil)) $
              -- 1 unary: child at 1. 3 constant: a value root, not a term root.
              pif (kind #== 1) (pcon (PJust (ptasks [ptask 0 (pbytesOf fields 1)]))) $
                pif (kind #== 3) (pcon (PJust (ptasks [ptask 1 (pbytesOf fields 0)]))) $
                  -- 2 application: function 0, argument 1.
                  pif
                    (kind #== 2)
                    ( pcon $
                        PJust $
                          ptasks [ptask 0 (pbytesOf fields 0), ptask 0 (pbytesOf fields 1)]
                    )
                    $ pif
                      (kind #== 6)
                      -- 6 constr: tag 0, count 1, sequence 2.
                      (psequenceOrEmpty (pintOf fields 1) (pbytesOf fields 2) pnil)
                      -- 7 case: scrutinee 0, count 1, sequence 2.
                      $ psequenceOrEmpty
                        (pintOf fields 1)
                        (pbytesOf fields 2)
                        (ptasks [ptask 0 (pbytesOf fields 0)])

{- | A count of zero must name the canonical empty sequence root.

@extra@ is what the shape contributes regardless — a case term's scrutinee — and
is prepended to whichever branch is taken.
-}
psequenceOrEmpty ::
  forall (s :: S).
  Term s PInteger ->
  Term s PByteString ->
  Term s PTaskList ->
  Term s (PMaybe PTaskList)
psequenceOrEmpty count sequence extra =
  plet count $ \n ->
    plet sequence $ \root ->
      pif
        (n #== 0)
        ( pif
            (root #== pemptySequenceRootV1)
            (pcon (PJust extra))
            (pcon PNothing)
        )
        $ pcon
        $ PJust
        $ pconcat # extra # ptasks [pmaterialTaskV1 2 root n]

pbytesOf ::
  forall (s :: S). Term s (PBuiltinList PData) -> Term s PInteger -> Term s PByteString
pbytesOf fields index = pasByteStr # (pelemAt # index # fields)

pintOf ::
  forall (s :: S). Term s (PBuiltinList PData) -> Term s PInteger -> Term s PInteger
pintOf fields index = pasInt # (pelemAt # index # fields)

{- | The children of a constant value.

@payload_root == semantic_root@ is the source-program rule: a published constant
is a @Data@ whose blob view and semantic view are the same commitment. A value
whose two roots differ is a runtime construction, and no program may contain one.
-}
pvalueChildren :: forall (s :: S). Term s PByteString -> Term s (PMaybe PTaskList)
pvalueChildren preimage =
  pmatch (pinspectProgramValueMaterialV1 # preimage) $ \case
    PNothing -> pcon PNothing
    PJust value ->
      pmatch value $ \(PConstantValue typeRoot payloadRoot _ semanticRoot _) ->
        plet (pfromData semanticRoot) $ \semantic ->
          pif
            (pnot # (pfromData payloadRoot #== semantic))
            (pcon PNothing)
            ( pcon $
                PJust $
                  ptasks [ptask 3 (pfromData typeRoot), ptask 5 semantic]
            )

-- | The children of a sequence link, and the length it must have.
psequenceChildren ::
  forall (s :: S). Term s PByteString -> Term s PInteger -> Term s (PMaybe PTaskList)
psequenceChildren preimage expectedLength =
  pmatch (pinspectProgramSequenceMaterialV1 # preimage) $ \case
    PNothing -> pcon PNothing
    PJust node ->
      pmatch node $ \(PProgramSequenceMaterialV1 head tail len) ->
        plet (pfromData len) $ \n ->
          pif (pnot # (expectedLength #== -1 #|| n #== expectedLength)) (pcon PNothing) $
            plinkChildren
              0
              2
              (pfromData head)
              (pfromData tail)
              n
              pemptySequenceRootV1

{- | The children of any singly-linked list node.

The last link must point at the canonical empty root; every other link carries
its tail's length with it, so a walk cannot be redirected into a shorter or
longer list than the count promised.
-}
plinkChildren ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s (PMaybe PTaskList)
plinkChildren headKind tailKind head tail len emptyRoot =
  pif
    (len #== 1)
    ( pif
        (tail #== emptyRoot)
        (pcon (PJust (ptasks [ptask headKind head])))
        (pcon PNothing)
    )
    ( pcon $
        PJust $
          ptasks [ptask headKind head, pmaterialTaskV1 tailKind tail (len - 1)]
    )

{- | The children of an authenticated @Data@ node.

Read by constructor tag: the integer and byte-string arms are both
@[task(3, root)]@ over a field at the same position, which is exactly the
identical-body @pmatch@ Plutarch mis-compiles. Constructor order is
@ConstrSmall@, @ConstrLarge@, @Map@, @List@, @Integer@, @Bytes@; the field
positions are written next to each read.
-}
pdataNodeChildren :: forall (s :: S). Term s PByteString -> Term s (PMaybe PTaskList)
pdataNodeChildren preimage =
  pmatch (CekData.pinspectDataNodePreimageV1 # preimage) $ \case
    PNothing -> pcon PNothing
    PJust node ->
      let (tag, fields) = pconstrOf (pdata node)
       in plet tag $ \kind ->
            -- 0 ConstrSmall: fields_count 1, fields_root 2.
            pif (kind #== 0) (pdataSequenceOrEmpty 6 (pintOf fields 1) (pbytesOf fields 2) pnil) $
              -- 1 ConstrLarge: ctor root 0, fields_count 3, fields_root 4.
              pif
                (kind #== 1)
                ( pdataSequenceOrEmpty
                    6
                    (pintOf fields 3)
                    (pbytesOf fields 4)
                    (ptasks [ptask 3 (pbytesOf fields 0)])
                )
                -- 2 Map: entries_count 0, entries_root 1.
                $ pif
                  (kind #== 2)
                  (pdataSequenceOrEmpty 7 (pintOf fields 0) (pbytesOf fields 1) pnil)
                -- 3 List: items_count 0, items_root 1.
                $ pif
                  (kind #== 3)
                  (pdataSequenceOrEmpty 6 (pintOf fields 0) (pbytesOf fields 1) pnil)
                -- 4 Integer and 5 Bytes: a blob root at 0, and nothing else.
                $ pcon
                $ PJust
                $ ptasks [ptask 3 (pbytesOf fields 0)]

-- | The @Data@ analogue of 'psequenceOrEmpty', over the item or entry domain.
pdataSequenceOrEmpty ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PTaskList ->
  Term s (PMaybe PTaskList)
pdataSequenceOrEmpty kind count root extra =
  plet count $ \n ->
    plet root $ \r ->
      plet
        (pif (kind #== 6) CekData.pemptyDataListRootV1 CekData.pemptyDataPairRootV1)
        $ \emptyRoot ->
          pif
            (n #== 0)
            (pif (r #== emptyRoot) (pcon (PJust extra)) (pcon PNothing))
            $ pcon
            $ PJust
            $ pconcat # extra # ptasks [pmaterialTaskV1 kind r n]

-- | The children of a @Data@ item link.
pdataListChildren ::
  forall (s :: S). Term s PByteString -> Term s PInteger -> Term s (PMaybe PTaskList)
pdataListChildren preimage expectedLength =
  pmatch (CekData.pinspectDataListNodePreimageV1 # preimage) $ \case
    PNothing -> pcon PNothing
    PJust node ->
      pmatch node $ \(CekData.PDataListNodeV1 head _ _ tail len _ _) ->
        plet (pfromData len) $ \n ->
          pif (pnot # (expectedLength #== -1 #|| n #== expectedLength)) (pcon PNothing) $
            plinkChildren
              5
              6
              (pfromData head)
              (pfromData tail)
              n
              CekData.pemptyDataListRootV1

-- | The children of a @Data@ entry link, which opens a key and a value.
pdataPairChildren ::
  forall (s :: S). Term s PByteString -> Term s PInteger -> Term s (PMaybe PTaskList)
pdataPairChildren preimage expectedLength =
  pmatch (CekData.pinspectDataPairNodePreimageV1 # preimage) $ \case
    PNothing -> pcon PNothing
    PJust node ->
      pmatch node $ \(CekData.PDataPairNodeV1 key _ _ value _ _ tail len _ _) ->
        plet (pfromData len) $ \n ->
          plet (ptasks [ptask 5 (pfromData key), ptask 5 (pfromData value)]) $ \heads ->
            pif (pnot # (expectedLength #== -1 #|| n #== expectedLength)) (pcon PNothing) $
              pif
                (n #== 1)
                ( pif
                    (pfromData tail #== CekData.pemptyDataPairRootV1)
                    (pcon (PJust heads))
                    (pcon PNothing)
                )
                ( pcon $
                    PJust $
                      pconcat
                        # heads
                        # ptasks [pmaterialTaskV1 7 (pfromData tail) (n - 1)]
                )

-- | The children of a blob node: none for a chunk, two for a branch.
pblobChildren ::
  forall (s :: S). Term s PInteger -> Term s PByteString -> Term s (PMaybe PTaskList)
pblobChildren kind preimage =
  pmatch (pinspectProgramBlobMaterialV1 # kind # preimage) $ \case
    PNothing -> pcon PNothing
    PJust blob ->
      pmatch blob $ \case
        PBlobChunk _ -> pcon (PJust pnil)
        PBlobBranch {pblob'left, pblob'right} ->
          pcon $
            PJust $
              ptasks [ptask 3 (pfromData pblob'left), ptask 3 (pfromData pblob'right)]

{- | Aiken @walk_complete_program_material_v1@.

A worklist, not a recursion over structure: children are pushed in front of the
remaining tasks, and a root already seen is skipped without being counted twice.
Note the order — the entry is resolved and its children computed /before/ the
seen check, so a duplicate reference to a node that does not parse is still a
refusal.
-}
pwalkCompleteProgramMaterialV1 ::
  forall (s :: S).
  Term
    s
    ( PTaskList
        :--> PEntryList
        :--> PProgramMaterialTraversalV1
        :--> PMaybe PProgramMaterialTraversalV1
    )
pwalkCompleteProgramMaterialV1 = phoistAcyclic $
  pfix $ \self -> plam $ \tasks entries traversal ->
    pelimList
      ( \task rest ->
          plet (ptaskRoot task) $ \root ->
            pmatch (pfindProgramMaterialEntryV1 # root # entries) $ \case
              PNothing -> pcon PNothing
              PJust entry ->
                pmatch (psourceProgramMaterialChildrenV1 # task # entry) $ \case
                  PNothing -> pcon PNothing
                  PJust children ->
                    pmatch traversal $ \(PProgramMaterialTraversalV1 seen nodeCount byteLength) ->
                      pif
                        (pelem # pdata root # seen)
                        (self # rest # entries # traversal)
                        $ self
                          # (pconcat # children # rest)
                          # entries
                          # pcon
                            ( PProgramMaterialTraversalV1
                                (pcons # pdata root # seen)
                                (nodeCount + 1)
                                (byteLength + plengthBS # (pentryPreimage # entry))
                            )
      )
      (pcon (PJust traversal))
      tasks

ptaskRoot :: forall (s :: S). Term s PProgramMaterialTaskV1 -> Term s PByteString
ptaskRoot task = pmatch task $ \(PProgramMaterialTaskV1 _ root _) -> root

{- | Aiken @verify_complete_program_material_entries_v1@.

Three things at once: the entries are sorted and each hashes to its own root;
the walk from the envelope's term root succeeds; and the walk's own count and
byte total match what the envelope declared, with every entry reached. The last
clause is the completeness one — without it a sidecar could carry unreachable
material and still verify.
-}
pverifyCompleteProgramMaterialEntriesV1 ::
  forall (s :: S). Term s (PByteString :--> PEntryList :--> PBool)
pverifyCompleteProgramMaterialEntriesV1 = phoistAcyclic $
  plam $ \envelopeCbor entries ->
    pmatch (pinspectProgramEnvelopeV1 # envelopeCbor) $ \case
      PNothing -> pfalse
      PJust envelope ->
        pif
          ( pnot
              #$ pstrictlySortedMaterialRootsV1
                # entries
                #&& (pall # plam (\e -> pprogramMaterialRootMatchesV1 # pfromData e) # entries)
          )
          pfalse
          $ pmatch envelope
          $ \(PProgramEnvelopeV1 termRoot nodeCount materialByteLength) ->
            pmatch
              ( pwalkCompleteProgramMaterialV1
                  # ptasks [ptask 0 (pfromData termRoot)]
                  # entries
                  # pcon (PProgramMaterialTraversalV1 pnil 0 0)
              )
              $ \case
                PNothing -> pfalse
                PJust traversal ->
                  pmatch traversal $ \(PProgramMaterialTraversalV1 seen count byteLength) ->
                    pand'List
                      [ count #== pfromData nodeCount
                      , byteLength #== pfromData materialByteLength
                      , plength # seen #== plength # entries
                      ]

-- | Aiken @verify_complete_program_material_v1@.
pverifyCompleteProgramMaterialV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PBool)
pverifyCompleteProgramMaterialV1 = phoistAcyclic $
  plam $ \envelopeCbor sidecarCbor ->
    pmatch (pinspectCompleteProgramMaterialSidecarV1 # sidecarCbor) $ \case
      PNothing -> pfalse
      PJust entries -> pverifyCompleteProgramMaterialEntriesV1 # envelopeCbor # entries

--------------------------------------------------------------------------------
-- The envelope
--------------------------------------------------------------------------------

{- | Aiken @inspect_program_envelope_v1@.

The exact V1 script payload. The length cap comes first and is exact rather than
generous: a 50-byte ceiling on a shape whose maximum is 50 bytes means a
malformed envelope is rejected before anything is decoded.

The UPLC version is pinned at 1.1.0, not merely range-checked — a program
claiming another version is not a program this machine replays.
-}
pinspectProgramEnvelopeV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PProgramEnvelopeV1)
pinspectProgramEnvelopeV1 = phoistAcyclic $
  plam $ \envelopeCbor ->
    pif (pmaxProgramEnvelopeCborBytes #< plengthBS # envelopeCbor) (pcon PNothing) $
      pmatch (pdecodedList # envelopeCbor) $ \case
        PNothing -> pcon PNothing
        PJust items ->
          pif (pnot # (plength # items #== 5)) (pcon PNothing) $
            plet (pitemAt items 1) $ \versionData ->
              pif (pnot # (pdataIsList # versionData)) (pcon PNothing) $
                plet (pasList # versionData) $ \versionItems ->
                  pif (pnot # (plength # versionItems #== 3)) (pcon PNothing) $
                    penvelopeFields envelopeCbor items versionItems

penvelopeFields ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PProgramEnvelopeV1)
penvelopeFields envelopeCbor items versionItems =
  plet (pitemAt items 0) $ \envelopeVersionData ->
    plet (pitemAt items 2) $ \termRootData ->
      plet (pitemAt items 3) $ \nodeCountData ->
        plet (pitemAt items 4) $ \materialByteLengthData ->
          pif
            ( pnot
                #$ pand'List
                  [ pdataIsInteger # envelopeVersionData
                  , pdataIsInteger # pitemAt versionItems 0
                  , pdataIsInteger # pitemAt versionItems 1
                  , pdataIsInteger # pitemAt versionItems 2
                  , pdataIsBytes # termRootData
                  , pdataIsInteger # nodeCountData
                  , pdataIsInteger # materialByteLengthData
                  ]
            )
            (pcon PNothing)
            $ pcheckEnvelope
              envelopeCbor
              (pasInt # envelopeVersionData)
              (pasInt # pitemAt versionItems 0)
              (pasInt # pitemAt versionItems 1)
              (pasInt # pitemAt versionItems 2)
              (pasByteStr # termRootData)
              (pasInt # nodeCountData)
              (pasInt # materialByteLengthData)

pcheckEnvelope ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybe PProgramEnvelopeV1)
pcheckEnvelope envelopeCbor envelopeVersion major minor patch termRoot nodeCount materialByteLength =
  plet termRoot $ \root ->
    plet nodeCount $ \count ->
      plet materialByteLength $ \byteLength ->
        pif
          ( pnot
              #$ pand'List
                [ envelopeVersion #== pprogramEnvelopeVersion
                , major #== 1
                , minor #== 1
                , patch #== 0
                , plengthBS # root #== 32
                , 0 #< count
                , count #<= pmaxProgramNodeCount
                , 0 #< byteLength
                , byteLength #<= pmaxProgramMaterialByteLength
                ]
          )
          (pcon PNothing)
          $ pif
            ( pencodeProgramEnvelopeV1
                # major
                # minor
                # patch
                # root
                # count
                # byteLength
                #== envelopeCbor
            )
            ( pcon $
                PJust $
                  pcon $
                    PProgramEnvelopeV1 (pdata root) (pdata count) (pdata byteLength)
            )
            (pcon PNothing)

-- | Aiken @decode_program_envelope_v1@ — the same, aborting rather than declining.
pdecodeProgramEnvelopeV1 ::
  forall (s :: S). Term s (PByteString :--> PProgramEnvelopeV1)
pdecodeProgramEnvelopeV1 = phoistAcyclic $
  plam $ \envelopeCbor ->
    pmatch (pinspectProgramEnvelopeV1 # envelopeCbor) $ \case
      PNothing -> perror
      PJust envelope -> envelope

{- |
Module      : Midgard.CekBuiltin
Description : Plutarch port of @lib/midgard/cek-builtin-v1.ak@.

Authenticated CEK builtin witnesses, their cost sizes, and builtin semantics.
The implementation follows the Aiken tag dispatch exactly; the deliberately
different runtime witness type is retained so pre-budget type failures can
authenticate closures and partially applied builtins as well as constants.
-}
module Midgard.CekBuiltin (
  PValueWitnessV1 (..),
  PRuntimeValueWitnessV1 (..),
  PBlsExpressionWitnessV1 (..),
  PSemanticBuiltinWitnessV1 (..),
  pmaxDirectBlsMillerLoopLeaves,
  pargumentsRootV1,
  pruntimeArgumentsRootV1,
  pvalueMemorySizeV1,
  pbuiltinCostSizesV1,
  pdirectBuiltinBudgetV1,
  pverifyDirectBuiltinV1,
  pverifyDirectBlsFinalV1,
  pverifyDirectBlsFinalRootsV1,
  pverifyBuiltinTypeFailureV1,
  pisValidUtf8V1,
  pverifyDirectBuiltinFailureV1,
  pdirectBuiltinFailureBudgetV1,
  pverifySemanticBuiltinV1,
  pverifySemanticBuiltinFailureV1,
  psemanticConstantTypeV1,
  psemanticConstantPayloadV1,
  psemanticConstantMemoryV1,
  presultRootV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Data.Kind (Type)

import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_MlResult,
  pbls12_381_G1_add,
  pbls12_381_G1_compress,
  pbls12_381_G1_hashToGroup,
  pbls12_381_G1_neg,
  pbls12_381_G1_scalarMul,
  pbls12_381_G1_uncompress,
  pbls12_381_G2_add,
  pbls12_381_G2_compress,
  pbls12_381_G2_hashToGroup,
  pbls12_381_G2_neg,
  pbls12_381_G2_scalarMul,
  pbls12_381_G2_uncompress,
  pbls12_381_finalVerify,
  pbls12_381_millerLoop,
  pbls12_381_mulMlResult,
 )
import Plutarch.Builtin.Crypto (
  pblake2b_256,
  pblake2b_224,
  pkeccak_256,
  pripemd_160,
  pverifyEcdsaSecp256k1Signature,
  pverifyEd25519Signature,
  pverifySchnorrSecp256k1Signature,
 )
import Plutarch.Core.Internal.Builtins (pconsBS', pindexBS')
import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import PlutusCore qualified as PLC

import Aiken.Cbor (pdeserialise)
import Midgard.CekConstant (
  PConstantTypeV1 (..),
  PConstantWitnessV1 (..),
  pconstantMemorySizeV1,
  pconstantPayloadMemorySizeV1,
  pconstantPayloadV1,
  pconstantRootV1,
  pconstantTypeIsKnownV1,
  pconstantTypeV1,
  pdecodeConstantTypeV1,
  pbytearrayMemorySizeV1,
  pintegerMemorySizeV1,
  psemanticConstantRootV1,
 )
import Midgard.CekCost (PBuiltinBudgetV1 (..), pbuiltinArgumentCountV1, pbuiltinBudgetV1)
import Midgard.CekData (
  PDataListNodeV1 (..),
  PDataNodeV1 (..),
  PDataPairNodeV1 (..),
  PDataSequenceSummaryV1 (..),
  PDataSummaryV1 (..),
  pdataNodeCborLengthV1,
  pdataNodeMemoryV1,
  pemptyDataListSummaryV1,
  phashDataNodeV1,
  pintegerDataSummaryV1,
  plargeConstrDataSummaryV1,
  plistDataSummaryV1,
  pprependDataListSummaryV1,
  psmallConstrDataSummaryV1,
  psemanticDataSummaryV1,
  pverifyDataListLinkV1,
  pverifyDataNodeV1,
 )
import Midgard.CekDataScan (pboundedBlobRootV1)
import Midgard.CekProof (
  pemptySequenceRootV1,
  phashBlsMillerLoopValueV1,
  phashBlsMillerLoopExpressionV1,
  phashBlsMultiplyExpressionV1,
  phashBuiltinValueV1,
  phashConstrValueV1,
  phashDelayValueV1,
  phashLambdaValueV1,
  phashSequenceNodeV1,
 )

data PValueWitnessV1 (s :: S)
  = PConstantValue (Term s (PAsData PConstantWitnessV1))
  | PSemanticConstantValue
      (Term s (PAsData PByteString))
      (Term s (PAsData PDataSummaryV1))
      (Term s (PAsData PInteger))
  | POpaqueValue (Term s (PAsData PByteString))
  | PBlsMillerLoopValue (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueWitnessV1)

data PRuntimeValueWitnessV1 (s :: S)
  = PRuntimeConstantValue (Term s (PAsData PConstantWitnessV1))
  | PRuntimeSemanticConstantValue
      (Term s (PAsData PByteString))
      (Term s (PAsData PDataSummaryV1))
      (Term s (PAsData PInteger))
  | PRuntimeLambdaValue
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  | PRuntimeDelayValue
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  | PRuntimeConstrValue
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
  | PRuntimeBuiltinValue
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
  | PRuntimeBlsMillerLoopValue (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRuntimeValueWitnessV1)

data PBlsExpressionWitnessV1 (s :: S)
  = PBlsMillerLoopExpression
      (Term s (PAsData PConstantWitnessV1))
      (Term s (PAsData PConstantWitnessV1))
  | PBlsMultiplyExpression
      (Term s (PAsData PBlsExpressionWitnessV1))
      (Term s (PAsData PBlsExpressionWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBlsExpressionWitnessV1)

data PSemanticBuiltinWitnessV1 (s :: S) = PSemanticBuiltinWitnessV1
  { psemantic'dataNodes :: Term s (PAsData (PBuiltinList (PAsData PDataNodeV1)))
  , psemantic'listNodes :: Term s (PAsData (PBuiltinList (PAsData PDataListNodeV1)))
  , psemantic'pairNodes :: Term s (PAsData (PBuiltinList (PAsData PDataPairNodeV1)))
  , psemantic'scalarPreimages :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSemanticBuiltinWitnessV1)

data PBlsEvaluationV1 (s :: S) = PBlsEvaluationV1
  (Term s PByteString)
  (Term s PBuiltinBLS12_381_MlResult)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PBlsEvaluationV1)

pmaxDirectBlsMillerLoopLeaves :: forall (s :: S). Term s PInteger
pmaxDirectBlsMillerLoopLeaves = 10

pvalueRootV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PByteString)
pvalueRootV1 = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PConstantValue witness -> pconstantRootV1 # pfromData witness
  PSemanticConstantValue typeCbor payload memory ->
    pif (pconstantTypeIsKnownV1 # pfromData typeCbor)
      (psemanticConstantRootV1 # pfromData typeCbor # pfromData payload # pfromData memory)
      perror
  POpaqueValue root -> pif (plengthBS # pfromData root #== 32) (pfromData root) perror
  PBlsMillerLoopValue expressionRoot -> phashBlsMillerLoopValueV1 # pfromData expressionRoot

pruntimeValueRootV1 :: forall (s :: S). Term s (PRuntimeValueWitnessV1 :--> PByteString)
pruntimeValueRootV1 = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PRuntimeConstantValue witness -> pconstantRootV1 # pfromData witness
  PRuntimeSemanticConstantValue typeCbor payload memory ->
    pif (pconstantTypeIsKnownV1 # pfromData typeCbor)
      (psemanticConstantRootV1 # pfromData typeCbor # pfromData payload # pfromData memory)
      perror
  PRuntimeLambdaValue body environment ->
    phashLambdaValueV1 # pfromData body # pfromData environment
  PRuntimeDelayValue body environment ->
    phashDelayValueV1 # pfromData body # pfromData environment
  PRuntimeConstrValue tag count root ->
    phashConstrValueV1 # pfromData tag # pfromData count # pfromData root
  PRuntimeBuiltinValue tag forces count root ->
    phashBuiltinValueV1 # pfromData tag # pfromData forces # pfromData count # pfromData root
  PRuntimeBlsMillerLoopValue expressionRoot ->
    phashBlsMillerLoopValueV1 # pfromData expressionRoot

pargumentsRootV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PValueWitnessV1) :--> PPair PByteString PInteger)
pargumentsRootV1 = phoistAcyclic $
  plam $ \arguments ->
    (pfix $ \self ->
      plam
        ( \rest root count ->
            pelimList
              ( \argument tailArguments ->
                  self
                    # tailArguments
                    # (phashSequenceNodeV1 # (pvalueRootV1 # pfromData argument) # root # (count + 1))
                    # (count + 1)
              )
              (pcon (PPair root count))
              rest
        ))
      # arguments
      # pemptySequenceRootV1
      # 0

pruntimeArgumentsRootV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PRuntimeValueWitnessV1) :--> PPair PByteString PInteger)
pruntimeArgumentsRootV1 = phoistAcyclic $
  plam $ \arguments ->
    (pfix $ \self ->
      plam
        ( \rest root count ->
            pelimList
              ( \argument tailArguments ->
                  self
                    # tailArguments
                    # (phashSequenceNodeV1 # (pruntimeValueRootV1 # pfromData argument) # root # (count + 1))
                    # (count + 1)
              )
              (pcon (PPair root count))
              rest
        ))
      # arguments
      # pemptySequenceRootV1
      # 0

pvalueMemorySizeV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PInteger)
pvalueMemorySizeV1 = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PConstantValue witness -> pconstantMemorySizeV1 # pfromData witness
  PSemanticConstantValue typeCbor payload memory ->
    pmatch (pfromData payload) $ \summary ->
      pif
        ( pand'List
            [ pconstantTypeIsKnownV1 # pfromData typeCbor
            , plengthBS # pfromData (psummary'root summary) #== 32
            , 0 #<= pfromData (psummary'cborLength summary)
            , 0 #<= pfromData (psummary'memory summary)
            , 0 #<= pfromData memory
            ]
        )
        (pfromData memory)
        perror
  POpaqueValue root -> pif (plengthBS # pfromData root #== 32) 1 perror
  PBlsMillerLoopValue expressionRoot ->
    pif (plengthBS # pfromData expressionRoot #== 32) 192 perror

pconstantTypeOf :: forall (s :: S). Term s PValueWitnessV1 -> Term s PConstantTypeV1
pconstantTypeOf value = pmatch value $ \case
  PConstantValue witness -> pconstantTypeV1 # pfromData witness
  PSemanticConstantValue typeCbor _ _ -> pdecodeConstantTypeV1 # pfromData typeCbor
  POpaqueValue _ -> perror
  PBlsMillerLoopValue _ -> perror

pconstantPayloadOf :: forall (s :: S). Term s PValueWitnessV1 -> Term s PData
pconstantPayloadOf value = pmatch value $ \case
  PConstantValue witness -> pconstantPayloadV1 # pfromData witness
  _ -> perror

pbytesV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PByteString
pbytesV1 value =
  pif (pconstantTypeOf value #== pcon PByteStringConstant)
    (pasByteStr # pconstantPayloadOf value)
    perror

pg1BytesV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PByteString
pg1BytesV1 value =
  pif (pconstantTypeOf value #== pcon PBlsG1Constant)
    (plet (pasByteStr # pconstantPayloadOf value) $ \bytes ->
      pif (plengthBS # bytes #== 48) bytes perror)
    perror

pg2BytesV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PByteString
pg2BytesV1 value =
  pif (pconstantTypeOf value #== pcon PBlsG2Constant)
    (plet (pasByteStr # pconstantPayloadOf value) $ \bytes ->
      pif (plengthBS # bytes #== 96) bytes perror)
    perror

pbooleanV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PBool
pbooleanV1 value =
  pif (pconstantTypeOf value #== pcon PBooleanConstant) `flip` perror $
    pmatch (pasConstr # pconstantPayloadOf value) $ \(PBuiltinPair tag fields) ->
      pif (pnull # fields #&& (tag #== 0 #|| tag #== 1)) (tag #== 1) perror

pstringBytesV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PByteString
pstringBytesV1 value =
  pif (pconstantTypeOf value #== pcon PStringConstant)
    (pasByteStr # pconstantPayloadOf value)
    perror

punitV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PBool
punitV1 value =
  pif
    (pconstantTypeOf value #== pcon PUnitConstant #&& pconstantPayloadOf value #== pboolData (pconstant False))
    (pconstant True)
    perror

pstandardCostSizes ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s (PBuiltinList (PAsData PInteger))
pstandardCostSizes arguments =
  pmap # plam (\argument -> pdata (pvalueMemorySizeV1 # pfromData argument)) # arguments

pwithTwoValues ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  (Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s r) ->
  Term s r
pwithTwoValues values k =
  pelimList
    (\a rest -> pelimList (\b tailValues -> pif (pnull # tailValues) (k (pfromData a) (pfromData b)) perror) perror rest)
    perror
    values

pwithOneValue ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  (Term s PValueWitnessV1 -> Term s r) ->
  Term s r
pwithOneValue values k =
  pelimList
    (\value rest -> pif (pnull # rest) (k (pfromData value)) perror)
    perror
    values

pwithThreeValues ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  (Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s r) ->
  Term s r
pwithThreeValues values k =
  pelimList
    ( \a rest -> pelimList
        (\b rest' -> pelimList (\c tailValues -> pif (pnull # tailValues) (k (pfromData a) (pfromData b) (pfromData c)) perror) perror rest')
        perror
        rest
    )
    perror
    values

pbuiltinCostSizesV1 ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PValueWitnessV1) :--> PBuiltinList (PAsData PInteger))
pbuiltinCostSizesV1 = phoistAcyclic $ plam $ \tag arguments ->
  pif (tag #== 26)
    (pwithThreeValues arguments $ \condition _ _ ->
      plet (pbooleanV1 condition) $ \conditionValue ->
        pif (conditionValue #|| pnot # conditionValue)
          (pcons # pdata 1 #$ pcons # pdata 1 #$ pcons # pdata 1 # pnil)
          perror)
    $ pif (tag #== 27)
      (pwithTwoValues arguments $ \unit _ ->
        pif (pconstantTypeOf unit #== pcon PUnitConstant)
          (pcons # pdata 1 #$ pcons # pdata 1 # pnil)
          perror)
      $ pif (tag #== 28)
        (pwithTwoValues arguments $ \message _ ->
          pcons # pdata (plengthBS # pstringBytesV1 message) #$ pcons # pdata 1 # pnil)
      $ pif (tag #== 31)
        (pwithThreeValues arguments $ \source _ _ ->
          pcons # pdata (pvalueMemorySizeV1 # source) #$ pcons # pdata 1 #$ pcons # pdata 1 # pnil)
      $ pif (tag #== 36)
        (pelimList
          (\source rest -> pif (plength # rest #== 5)
            (pcons # pdata (pvalueMemorySizeV1 # pfromData source) #$ pcons # pdata 1 #$ pcons # pdata 1 #$ pcons # pdata 1 #$ pcons # pdata 1 #$ pcons # pdata 1 # pnil)
            perror)
          perror
          arguments)
      $ pif (75 #<= tag #&& tag #<= 77)
        (pwithThreeValues arguments $ \extend left right ->
          plet (pvalueMemorySizeV1 # left) $ \leftSize ->
          plet (pvalueMemorySizeV1 # right) $ \rightSize ->
          plet (pif (pbooleanV1 extend)
                  (pif (rightSize #< leftSize) leftSize rightSize)
                  (pif (leftSize #< rightSize) leftSize rightSize)) $ \normalized ->
            pcons # pdata 1 #$ pcons # pdata normalized #$ pcons # pdata normalized # pnil)
        (pstandardCostSizes arguments)

pdirectBuiltinBudgetV1 ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PValueWitnessV1) :--> PBuiltinBudgetV1)
pdirectBuiltinBudgetV1 = phoistAcyclic $ plam $ \tag arguments ->
  pbuiltinBudgetV1 # tag # (pbuiltinCostSizesV1 # tag # arguments)

presultIsConstantV1 ::
  forall (s :: S).
  Term s PValueWitnessV1 -> Term s PConstantTypeV1 -> Term s PData -> Term s PBool
presultIsConstantV1 result expectedType expectedPayload = pmatch result $ \case
  PConstantValue witness ->
    (pconstantTypeV1 # pfromData witness #== expectedType)
      #&& (pconstantPayloadV1 # pfromData witness #== expectedPayload)
  PSemanticConstantValue typeCbor payload memory ->
    (pdecodeConstantTypeV1 # pfromData typeCbor #== expectedType)
      #&& (pfromData payload #== psemanticDataSummaryV1 # expectedPayload)
      #&& (pfromData memory #== pconstantPayloadMemorySizeV1 # expectedType # expectedPayload)
  POpaqueValue _ -> pconstant False
  PBlsMillerLoopValue _ -> pconstant False

pintegerV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PInteger
pintegerV1 value =
  pif (pconstantTypeOf value #== pcon PIntegerConstant)
    (pasInt # pconstantPayloadOf value)
    perror

presultIsSelectedV1 ::
  forall (s :: S). Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PBool
presultIsSelectedV1 result selected = pvalueRootV1 # result #== pvalueRootV1 # selected

pboolData :: forall (s :: S). Term s PBool -> Term s PData
pboolData value = pforgetData $ pconstrBuiltin # pif value 1 0 # pnil

pverifyIntegerBinaryV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyIntegerBinaryV1 tag arguments result =
  pwithTwoValues arguments $ \leftValue rightValue ->
    plet (pintegerV1 leftValue) $ \left ->
    plet (pintegerV1 rightValue) $ \right ->
      pif (tag #== 0)
        (integerResult (left + right))
      $ pif (tag #== 1)
        (integerResult (left - right))
      $ pif (tag #== 2)
        (integerResult (left * right))
      $ pif (tag #== 3)
        (integerResult (pdiv # left # right))
      $ pif (tag #== 4)
        (integerResult (pquot # left # right))
      $ pif (tag #== 5)
        (integerResult (prem # left # right))
      $ pif (tag #== 6)
        (integerResult (pmod # left # right))
      $ pif (tag #== 7)
        (booleanResult (left #== right))
      $ pif (tag #== 8)
        (booleanResult (left #< right))
      $ pif (tag #== 9)
        (booleanResult (left #<= right))
        perror
  where
    integerResult value =
      presultIsConstantV1 result (pcon PIntegerConstant) (pforgetData $ pdata value)
    booleanResult value =
      presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pverifyBytesV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyBytesV1 tag arguments result =
  pif (tag #== 10)
    (pwithTwoValues arguments $ \left right ->
      presultIsConstantV1 result (pcon PByteStringConstant)
        (pforgetData $ pdata (pbytesV1 left <> pbytesV1 right)))
  $ pif (tag #== 11)
    (pwithTwoValues arguments $ \byte source ->
      plet (pintegerV1 byte) $ \byteValue ->
        pif (0 #<= byteValue #&& byteValue #<= 255)
          (bytesResult (pconsBS' # byteValue # pbytesV1 source))
          perror)
  $ pif (tag #== 12)
    (pwithThreeValues arguments $ \start len source ->
      bytesResult (psliceBS # pintegerV1 start # pintegerV1 len # pbytesV1 source))
  $ pif (tag #== 13)
    (pwithOneValue arguments $ \source -> integerResult (plengthBS # pbytesV1 source))
  $ pif (tag #== 14)
    (pwithTwoValues arguments $ \source index ->
      plet (pbytesV1 source) $ \sourceBytes ->
      plet (pintegerV1 index) $ \position ->
        pif (0 #<= position #&& position #< plengthBS # sourceBytes)
          (integerResult (pindexBS' # sourceBytes # position))
          perror)
  $ pif (tag #== 15)
    (pwithTwoValues arguments $ \left right -> booleanResult (pbytesV1 left #== pbytesV1 right))
  $ pif (tag #== 16)
    (pwithTwoValues arguments $ \left right -> booleanResult (pbytesV1 left #< pbytesV1 right))
  $ pif (tag #== 17)
    (pwithTwoValues arguments $ \left right -> booleanResult (pbytesV1 left #<= pbytesV1 right))
  $ pif (tag #== 18)
    (pwithOneValue arguments $ \source -> bytesResult (psha2_256 # pbytesV1 source))
  $ pif (tag #== 19)
    (pwithOneValue arguments $ \source -> bytesResult (psha3_256 # pbytesV1 source))
  $ pif (tag #== 20)
    (pwithOneValue arguments $ \source -> bytesResult (pblake2b_256 # pbytesV1 source))
    perror
  where
    bytesResult value =
      presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata value)
    integerResult value =
      presultIsConstantV1 result (pcon PIntegerConstant) (pforgetData $ pdata value)
    booleanResult value =
      presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pverifyControlV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyControlV1 tag arguments result =
  pif (tag #== 26)
    (pwithThreeValues arguments $ \condition whenTrue whenFalse ->
      presultIsSelectedV1 result (pif (pbooleanV1 condition) whenTrue whenFalse))
  $ pif (tag #== 27)
    (pwithTwoValues arguments $ \unit selected ->
      pif (punitV1 unit) (presultIsSelectedV1 result selected) perror)
  $ pif (tag #== 28)
    (pwithTwoValues arguments $ \message selected ->
      pif (0 #<= plengthBS # pstringBytesV1 message)
        (presultIsSelectedV1 result selected)
        perror)
    perror

pverifyStringsV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyStringsV1 tag arguments result =
  pif (tag #== 22)
    (pwithTwoValues arguments $ \left right ->
      stringResult (pencodeUtf8 # ((pdecodeUtf8 # pstringBytesV1 left) <> (pdecodeUtf8 # pstringBytesV1 right))))
  $ pif (tag #== 23)
    (pwithTwoValues arguments $ \left right ->
      booleanResult (pdecodeUtf8 # pstringBytesV1 left #== pdecodeUtf8 # pstringBytesV1 right))
  $ pif (tag #== 24)
    (pwithOneValue arguments $ \value -> bytesResult (pstringBytesV1 value))
  $ pif (tag #== 25)
    (pwithOneValue arguments $ \value -> stringResult (pbytesV1 value))
    perror
  where
    stringResult value =
      presultIsConstantV1 result (pcon PStringConstant) (pforgetData $ pdata value)
    bytesResult value =
      presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata value)
    booleanResult value =
      presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pverifySignatureV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifySignatureV1 tag arguments result =
  pwithThreeValues arguments $ \keyValue messageValue signatureValue ->
    plet (pbytesV1 keyValue) $ \key ->
    plet (pbytesV1 messageValue) $ \message ->
    plet (pbytesV1 signatureValue) $ \signature ->
      plet
        ( pif (tag #== 21)
            (pif (plengthBS # key #== 32 #&& plengthBS # signature #== 64)
              (pverifyEd25519Signature # key # message # signature) perror)
          $ pif (tag #== 52)
            (pif (plengthBS # key #== 33 #&& plengthBS # message #== 32 #&& plengthBS # signature #== 64)
              (pverifyEcdsaSecp256k1Signature # key # message # signature) perror)
          $ pif (tag #== 53)
            (pif (plengthBS # key #== 32 #&& plengthBS # message #== 32 #&& plengthBS # signature #== 64)
              (pverifySchnorrSecp256k1Signature # key # message # signature) perror)
            perror
        )
        $ \verified -> presultIsConstantV1 result (pcon PBooleanConstant) (pboolData verified)

pwithPairPayload ::
  forall (s :: S) (r :: S -> Type).
  Term s PValueWitnessV1 ->
  (Term s PConstantTypeV1 -> Term s PConstantTypeV1 -> Term s PData -> Term s PData -> Term s r) ->
  Term s r
pwithPairPayload value k = pmatch (pconstantTypeOf value) $ \case
  PPairConstant first second ->
    pmatch (pasConstr # pconstantPayloadOf value) $ \(PBuiltinPair tag fields) ->
      pif (tag #== 0)
        (pelimList
          (\firstPayload rest -> pelimList
            (\secondPayload tailFields -> pif (pnull # tailFields)
              (k (pfromData first) (pfromData second) firstPayload secondPayload) perror)
            perror rest)
          perror fields)
        perror
  _ -> perror

pwithListPayload ::
  forall (s :: S) (r :: S -> Type).
  Term s PValueWitnessV1 ->
  (Term s PConstantTypeV1 -> Term s (PBuiltinList PData) -> Term s r) ->
  Term s r
pwithListPayload value k = pmatch (pconstantTypeOf value) $ \case
  PListConstant element -> k (pfromData element) (pasList # pconstantPayloadOf value)
  _ -> perror

pdataPayloadV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PData
pdataPayloadV1 value =
  pif (pconstantTypeOf value #== pcon PDataConstant) (pconstantPayloadOf value) perror

pdataIsConstr, pdataIsMap, pdataIsList, pdataIsInteger, pdataIsBytes ::
  forall (s :: S). Term s PData -> Term s PBool
pdataIsConstr value = pchooseData # value # pconstant True # pconstant False # pconstant False # pconstant False # pconstant False
pdataIsMap value = pchooseData # value # pconstant False # pconstant True # pconstant False # pconstant False # pconstant False
pdataIsList value = pchooseData # value # pconstant False # pconstant False # pconstant True # pconstant False # pconstant False
pdataIsInteger value = pchooseData # value # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pdataIsBytes value = pchooseData # value # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True

ppairData :: forall (s :: S). Term s PData -> Term s PData -> Term s PData
ppairData first second =
  pforgetData $ pconstrBuiltin # 0 # (pcons # first #$ pcons # second # pnil)

pmapData :: forall (s :: S). Term s (PBuiltinList (PBuiltinPair PData PData) :--> PData)
pmapData = punsafeBuiltin PLC.MapData

psemanticPairsV1 ::
  forall (s :: S). Term s (PBuiltinList PData :--> PBuiltinList (PBuiltinPair PData PData))
psemanticPairsV1 = phoistAcyclic $ pfix $ \self -> plam $ \items ->
  pelimList
    ( \item rest -> pmatch (pasConstr # item) $ \(PBuiltinPair tag fields) ->
        pif (tag #== 0)
          (pelimList
            (\first tailFields -> pelimList
              (\second finalFields -> pif (pnull # finalFields)
                (pcons # pcon (PBuiltinPair first second) # (self # rest)) perror)
              perror tailFields)
            perror fields)
          perror
    )
    pnil
    items

pencodeSemanticPairsV1 ::
  forall (s :: S). Term s (PBuiltinList (PBuiltinPair PData PData) :--> PBuiltinList PData)
pencodeSemanticPairsV1 = phoistAcyclic $ pfix $ \self -> plam $ \items ->
  pelimList
    (\item rest -> pmatch item $ \(PBuiltinPair first second) ->
      pcons # ppairData first second # (self # rest))
    pnil
    items

pverifyChooseDataV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PBool
pverifyChooseDataV1 arguments result =
  pelimList (\source r1 ->
    pelimList (\whenConstr r2 ->
      pelimList (\whenMap r3 ->
        pelimList (\whenList r4 ->
          pelimList (\whenInteger r5 ->
            pelimList (\whenBytes end -> pif (pnull # end)
              ( presultIsSelectedV1 result
                  ( pchooseData
                      # pdataPayloadV1 (pfromData source)
                      # pfromData whenConstr
                      # pfromData whenMap
                      # pfromData whenList
                      # pfromData whenInteger
                      # pfromData whenBytes
                  )
              ) perror) perror r5) perror r4) perror r3) perror r2) perror r1) perror arguments

pverifyPairAndListV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyPairAndListV1 tag arguments result =
  pif (tag #== 29)
    (pwithOneValue arguments $ \pair ->
      pwithPairPayload pair $ \firstType _ firstPayload _ ->
        presultIsConstantV1 result firstType firstPayload)
  $ pif (tag #== 30)
    (pwithOneValue arguments $ \pair ->
      pwithPairPayload pair $ \_ secondType _ secondPayload ->
        presultIsConstantV1 result secondType secondPayload)
  $ pif (tag #== 31)
    (pwithThreeValues arguments $ \source whenEmpty whenNonempty ->
      pwithListPayload source $ \_ items ->
        presultIsSelectedV1 result (pif (pnull # items) whenEmpty whenNonempty))
  $ pif (tag #== 32)
    (pwithTwoValues arguments $ \item source ->
      pwithListPayload source $ \elementType items ->
        pif (pconstantTypeOf item #== elementType)
          (presultIsConstantV1 result
            (pcon $ PListConstant $ pdata elementType)
            (pforgetData $ pdata $ pcons # pconstantPayloadOf item # items))
          perror)
  $ pif (tag #== 33)
    (pelimList
      ( \source rest -> pif (pnull # rest)
          ( pmatch (pconstantTypeOf (pfromData source)) $ \case
              PListConstant element ->
                pelimList
                  (\headPayload _ -> presultIsConstantV1 result (pfromData element) headPayload)
                  perror
                  (pasList # pconstantPayloadOf (pfromData source))
              _ -> perror
          )
          perror
      )
      perror
      arguments)
  $ pif (tag #== 34)
    (pwithOneValue arguments $ \source ->
      pwithListPayload source $ \elementType items ->
        pelimList (\_ tailItems -> presultIsConstantV1 result
          (pcon $ PListConstant $ pdata elementType) (pforgetData $ pdata tailItems)) perror items)
  $ pif (tag #== 35)
    (pwithOneValue arguments $ \source ->
      pwithListPayload source $ \_ items ->
        presultIsConstantV1 result (pcon PBooleanConstant) (pboolData (pnull # items)))
    perror

pverifyDataV1 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PValueWitnessV1)) ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyDataV1 tag arguments result =
  pif (tag #== 37)
    (pwithTwoValues arguments $ \index fields ->
      pwithListPayload fields $ \elementType items ->
        pif (elementType #== pcon PDataConstant)
          (dataResult $ pforgetData $ pconstrBuiltin # pintegerV1 index # items)
          perror)
  $ pif (tag #== 38)
    (pwithOneValue arguments $ \itemsValue ->
      pwithListPayload itemsValue $ \elementType items ->
        pif (elementType #== dataPairType)
          (dataResult $ pmapData # (psemanticPairsV1 # items))
          perror)
  $ pif (tag #== 39)
    (pwithOneValue arguments $ \itemsValue ->
      pwithListPayload itemsValue $ \elementType items ->
        pif (elementType #== pcon PDataConstant) (dataResult $ plistData # items) perror)
  $ pif (tag #== 40)
    (pwithOneValue arguments $ \integer -> dataResult (pforgetData $ pdata $ pintegerV1 integer))
  $ pif (tag #== 41)
    (pwithOneValue arguments $ \bytes -> dataResult (pforgetData $ pdata $ pbytesV1 bytes))
  $ pif (tag #== 42)
    (pwithOneValue arguments $ \source ->
      plet (pdataPayloadV1 source) $ \value ->
        pif (pdataIsConstr value)
          (pmatch (pasConstr # value) $ \(PBuiltinPair index fields) ->
            presultIsConstantV1 result constrResultType
              (ppairData (pforgetData $ pdata index) (plistData # fields)))
          perror)
  $ pif (tag #== 43)
    (pwithOneValue arguments $ \source ->
      plet (pdataPayloadV1 source) $ \value ->
        pif (pdataIsMap value)
          (presultIsConstantV1 result (pcon $ PListConstant $ pdata dataPairType)
            (plistData #$ pencodeSemanticPairsV1 #$ pasMap # value))
          perror)
  $ pif (tag #== 44)
    (pwithOneValue arguments $ \source ->
      plet (pdataPayloadV1 source) $ \value ->
        pif (pdataIsList value)
          (presultIsConstantV1 result (pcon $ PListConstant $ pdata $ pcon PDataConstant)
            (plistData #$ pasList # value))
          perror)
  $ pif (tag #== 45)
    (pwithOneValue arguments $ \source ->
      plet (pdataPayloadV1 source) $ \value ->
        pif (pdataIsInteger value)
          (presultIsConstantV1 result (pcon PIntegerConstant) (pforgetData $ pdata $ pasInt # value))
          perror)
  $ pif (tag #== 46)
    (pwithOneValue arguments $ \source ->
      plet (pdataPayloadV1 source) $ \value ->
        pif (pdataIsBytes value)
          (presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata $ pasByteStr # value))
          perror)
  $ pif (tag #== 47)
    (pwithTwoValues arguments $ \left right ->
      booleanResult (pdataPayloadV1 left #== pdataPayloadV1 right))
  $ pif (tag #== 48)
    (pwithTwoValues arguments $ \first second ->
      presultIsConstantV1 result dataPairType (ppairData (pdataPayloadV1 first) (pdataPayloadV1 second)))
  $ pif (tag #== 49)
    (pwithOneValue arguments $ \unit ->
      pif (punitV1 unit)
        (presultIsConstantV1 result (pcon $ PListConstant $ pdata $ pcon PDataConstant) (plistData # pnil))
        perror)
  $ pif (tag #== 50)
    (pwithOneValue arguments $ \unit ->
      pif (punitV1 unit)
        (presultIsConstantV1 result (pcon $ PListConstant $ pdata dataPairType) (plistData # pnil))
        perror)
  $ pif (tag #== 51)
    (pwithOneValue arguments $ \source ->
      bytesResult (pserialiseData # pdataPayloadV1 source))
    perror
  where
    dataPairType = pcon $ PPairConstant (pdata $ pcon PDataConstant) (pdata $ pcon PDataConstant)
    constrResultType =
      pcon $ PPairConstant
        (pdata $ pcon PIntegerConstant)
        (pdata $ pcon $ PListConstant $ pdata $ pcon PDataConstant)
    dataResult value = presultIsConstantV1 result (pcon PDataConstant) value
    bytesResult value = presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata value)
    booleanResult value = presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pverifyBlsG1V1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PBool
pverifyBlsG1V1 tag arguments result =
  pif (tag #== 54)
    (pwithTwoValues arguments $ \left right ->
      g1Result $ pbls12_381_G1_compress
        # (pbls12_381_G1_add # (uncompress left) # (uncompress right)))
  $ pif (tag #== 55)
    (pwithOneValue arguments $ \source ->
      g1Result $ pbls12_381_G1_compress # (pbls12_381_G1_neg # uncompress source))
  $ pif (tag #== 56)
    (pwithTwoValues arguments $ \scalar source ->
      g1Result $ pbls12_381_G1_compress
        # (pbls12_381_G1_scalarMul # pintegerV1 scalar # uncompress source))
  $ pif (tag #== 57)
    (pwithTwoValues arguments $ \left right -> booleanResult (uncompress left #== uncompress right))
  $ pif (tag #== 58)
    (pwithTwoValues arguments $ \message domain ->
      g1Result $ pbls12_381_G1_compress # (pbls12_381_G1_hashToGroup # pbytesV1 message # pbytesV1 domain))
  $ pif (tag #== 59)
    (pwithOneValue arguments $ \source -> bytesResult $ pbls12_381_G1_compress # uncompress source)
  $ pif (tag #== 60)
    (pwithOneValue arguments $ \source ->
      g1Result $ pbls12_381_G1_compress # (pbls12_381_G1_uncompress # pbytesV1 source))
    perror
  where
    uncompress value = pbls12_381_G1_uncompress # pg1BytesV1 value
    g1Result bytes = presultIsConstantV1 result (pcon PBlsG1Constant) (pforgetData $ pdata bytes)
    bytesResult bytes = presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata bytes)
    booleanResult value = presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pverifyBlsG2V1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PBool
pverifyBlsG2V1 tag arguments result =
  pif (tag #== 61)
    (pwithTwoValues arguments $ \left right ->
      g2Result $ pbls12_381_G2_compress
        # (pbls12_381_G2_add # (uncompress left) # (uncompress right)))
  $ pif (tag #== 62)
    (pwithOneValue arguments $ \source ->
      g2Result $ pbls12_381_G2_compress # (pbls12_381_G2_neg # uncompress source))
  $ pif (tag #== 63)
    (pwithTwoValues arguments $ \scalar source ->
      g2Result $ pbls12_381_G2_compress
        # (pbls12_381_G2_scalarMul # pintegerV1 scalar # uncompress source))
  $ pif (tag #== 64)
    (pwithTwoValues arguments $ \left right -> booleanResult (uncompress left #== uncompress right))
  $ pif (tag #== 65)
    (pwithTwoValues arguments $ \message domain ->
      g2Result $ pbls12_381_G2_compress # (pbls12_381_G2_hashToGroup # pbytesV1 message # pbytesV1 domain))
  $ pif (tag #== 66)
    (pwithOneValue arguments $ \source -> bytesResult $ pbls12_381_G2_compress # uncompress source)
  $ pif (tag #== 67)
    (pwithOneValue arguments $ \source ->
      g2Result $ pbls12_381_G2_compress # (pbls12_381_G2_uncompress # pbytesV1 source))
    perror
  where
    uncompress value = pbls12_381_G2_uncompress # pg2BytesV1 value
    g2Result bytes = presultIsConstantV1 result (pcon PBlsG2Constant) (pforgetData $ pdata bytes)
    bytesResult bytes = presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata bytes)
    booleanResult value = presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pverifyBlsExpressionV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PBool
pverifyBlsExpressionV1 tag arguments result =
  pif (tag #== 68)
    (pwithTwoValues arguments $ \g1 g2 ->
      plet (pg1BytesV1 g1) $ \g1Compressed ->
      plet (pg2BytesV1 g2) $ \g2Compressed ->
        pif
          ( pbls12_381_G1_compress # (pbls12_381_G1_uncompress # g1Compressed) #== g1Compressed
              #&& pbls12_381_G2_compress # (pbls12_381_G2_uncompress # g2Compressed) #== g2Compressed
          )
          (pmatch result $ \case
            PBlsMillerLoopValue resultRoot ->
              pfromData resultRoot #== phashBlsMillerLoopExpressionV1 # (pvalueRootV1 # g1) # (pvalueRootV1 # g2)
            _ -> perror)
          perror)
  $ pif (tag #== 69)
    (pwithTwoValues arguments $ \left right ->
      pmatch left $ \case
        PBlsMillerLoopValue leftRoot -> pmatch right $ \case
          PBlsMillerLoopValue rightRoot -> pmatch result $ \case
            PBlsMillerLoopValue resultRoot ->
              pfromData resultRoot #== phashBlsMultiplyExpressionV1 # pfromData leftRoot # pfromData rightRoot
            _ -> perror
          _ -> perror
        _ -> perror)
    perror

pbyteStringToIntegerBuiltin ::
  forall (s :: S). Term s (PBool :--> PByteString :--> PInteger)
pbyteStringToIntegerBuiltin = punsafeBuiltin PLC.ByteStringToInteger

pintegerToByteStringBuiltin ::
  forall (s :: S). Term s (PBool :--> PInteger :--> PInteger :--> PByteString)
pintegerToByteStringBuiltin = punsafeBuiltin PLC.IntegerToByteString

pandByteStringBuiltin, porByteStringBuiltin, pxorByteStringBuiltin ::
  forall (s :: S). Term s (PBool :--> PByteString :--> PByteString :--> PByteString)
pandByteStringBuiltin = punsafeBuiltin PLC.AndByteString
porByteStringBuiltin = punsafeBuiltin PLC.OrByteString
pxorByteStringBuiltin = punsafeBuiltin PLC.XorByteString

pcomplementByteStringBuiltin :: forall (s :: S). Term s (PByteString :--> PByteString)
pcomplementByteStringBuiltin = punsafeBuiltin PLC.ComplementByteString

preadBitBuiltin :: forall (s :: S). Term s (PByteString :--> PInteger :--> PBool)
preadBitBuiltin = punsafeBuiltin PLC.ReadBit

pwriteBitsBuiltin ::
  forall (s :: S). Term s (PByteString :--> PBuiltinList PInteger :--> PBool :--> PByteString)
pwriteBitsBuiltin = punsafeBuiltin PLC.WriteBits

preplicateByteBuiltin :: forall (s :: S). Term s (PInteger :--> PInteger :--> PByteString)
preplicateByteBuiltin = punsafeBuiltin PLC.ReplicateByte

pshiftByteStringBuiltin, protateByteStringBuiltin ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PByteString)
pshiftByteStringBuiltin = punsafeBuiltin PLC.ShiftByteString
protateByteStringBuiltin = punsafeBuiltin PLC.RotateByteString

pcountSetBitsBuiltin, pfindFirstSetBitBuiltin ::
  forall (s :: S). Term s (PByteString :--> PInteger)
pcountSetBitsBuiltin = punsafeBuiltin PLC.CountSetBits
pfindFirstSetBitBuiltin = punsafeBuiltin PLC.FindFirstSetBit

psemanticIntegersV1 :: forall (s :: S). Term s (PBuiltinList PData :--> PBuiltinList PInteger)
psemanticIntegersV1 = phoistAcyclic $ pfix $ \self -> plam $ \items ->
  pelimList
    (\item rest -> pif (pdataIsInteger item)
      (pcons # (pasInt # item) # (self # rest)) perror)
    pnil
    items

pverifyV3BytesV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PBool
pverifyV3BytesV1 tag arguments result =
  pif (tag #== 71)
    (pwithOneValue arguments $ \source -> bytesResult (pkeccak_256 # pbytesV1 source))
  $ pif (tag #== 72)
    (pwithOneValue arguments $ \source -> bytesResult (pblake2b_224 # pbytesV1 source))
  $ pif (tag #== 73)
    (pwithThreeValues arguments $ \endianness size value ->
      plet (pintegerV1 size) $ \targetSize ->
      plet (pintegerV1 value) $ \integer ->
        pif (0 #<= targetSize #&& 0 #<= integer)
          (bytesResult $ pintegerToByteStringBuiltin # pbooleanV1 endianness # targetSize # integer)
          perror)
  $ pif (tag #== 74)
    (pwithTwoValues arguments $ \endianness source ->
      integerResult $ pbyteStringToIntegerBuiltin # pbooleanV1 endianness # pbytesV1 source)
  $ pif (75 #<= tag #&& tag #<= 77)
    (pwithThreeValues arguments $ \padding left right ->
      plet (pbooleanV1 padding) $ \pad ->
      plet (pbytesV1 left) $ \leftBytes ->
      plet (pbytesV1 right) $ \rightBytes ->
        bytesResult
          (pif (tag #== 75)
            (pandByteStringBuiltin # pad # leftBytes # rightBytes)
            (pif (tag #== 76)
              (porByteStringBuiltin # pad # leftBytes # rightBytes)
              (pxorByteStringBuiltin # pad # leftBytes # rightBytes))))
  $ pif (tag #== 78)
    (pwithOneValue arguments $ \source -> bytesResult (pcomplementByteStringBuiltin # pbytesV1 source))
  $ pif (tag #== 79)
    (pwithTwoValues arguments $ \source index ->
      plet (pbytesV1 source) $ \sourceBytes ->
      plet (pintegerV1 index) $ \bitIndex ->
        pif (0 #<= bitIndex #&& bitIndex #< plengthBS # sourceBytes * 8)
          (booleanResult $ preadBitBuiltin # sourceBytes # bitIndex)
          perror)
  $ pif (tag #== 80)
    (pwithThreeValues arguments $ \source indicesValue bitValue ->
      pwithListPayload indicesValue $ \elementType indexPayloads ->
        plet (pbytesV1 source) $ \sourceBytes ->
        plet (psemanticIntegersV1 # indexPayloads) $ \indices ->
          pif
            ( elementType #== pcon PIntegerConstant
                #&& pall # plam (\index -> 0 #<= index #&& index #< plengthBS # sourceBytes * 8) # indices
            )
            (bytesResult $ pwriteBitsBuiltin # sourceBytes # indices # pbooleanV1 bitValue)
            perror)
  $ pif (tag #== 81)
    (pwithTwoValues arguments $ \lengthValue byteValue ->
      plet (pintegerV1 lengthValue) $ \len ->
      plet (pintegerV1 byteValue) $ \byte ->
        pif (0 #<= len #&& 0 #<= byte #&& byte #<= 255)
          (bytesResult $ preplicateByteBuiltin # len # byte)
          perror)
  $ pif (tag #== 82)
    (pwithTwoValues arguments $ \source offset ->
      bytesResult $ pshiftByteStringBuiltin # pbytesV1 source # pintegerV1 offset)
  $ pif (tag #== 83)
    (pwithTwoValues arguments $ \source offset ->
      bytesResult $ protateByteStringBuiltin # pbytesV1 source # pintegerV1 offset)
  $ pif (tag #== 84)
    (pwithOneValue arguments $ \source -> integerResult $ pcountSetBitsBuiltin # pbytesV1 source)
  $ pif (tag #== 85)
    (pwithOneValue arguments $ \source -> integerResult $ pfindFirstSetBitBuiltin # pbytesV1 source)
  $ pif (tag #== 86)
    (pwithOneValue arguments $ \source -> bytesResult $ pripemd_160 # pbytesV1 source)
    perror
  where
    bytesResult bytes = presultIsConstantV1 result (pcon PByteStringConstant) (pforgetData $ pdata bytes)
    integerResult value = presultIsConstantV1 result (pcon PIntegerConstant) (pforgetData $ pdata value)
    booleanResult value = presultIsConstantV1 result (pcon PBooleanConstant) (pboolData value)

pevaluateBlsExpressionV1 ::
  forall (s :: S). Term s (PBlsExpressionWitnessV1 :--> PBlsEvaluationV1)
pevaluateBlsExpressionV1 = phoistAcyclic $ pfix $ \self -> plam $ \expression ->
  pmatch expression $ \case
    PBlsMillerLoopExpression g1Witness g2Witness ->
      plet (pcon $ PConstantValue g1Witness) $ \g1 ->
      plet (pcon $ PConstantValue g2Witness) $ \g2 ->
        pcon $ PBlsEvaluationV1
          (phashBlsMillerLoopExpressionV1 # (pvalueRootV1 # g1) # (pvalueRootV1 # g2))
          (pbls12_381_millerLoop
            # (pbls12_381_G1_uncompress # pg1BytesV1 g1)
            # (pbls12_381_G2_uncompress # pg2BytesV1 g2))
    PBlsMultiplyExpression left right ->
      pmatch (self # pfromData left) $ \(PBlsEvaluationV1 leftRoot leftResult) ->
      pmatch (self # pfromData right) $ \(PBlsEvaluationV1 rightRoot rightResult) ->
        pcon $ PBlsEvaluationV1
          (phashBlsMultiplyExpressionV1 # leftRoot # rightRoot)
          (pbls12_381_mulMlResult # leftResult # rightResult)

pblsExpressionMetricsV1 ::
  forall (s :: S). Term s (PBlsExpressionWitnessV1 :--> PPair PInteger PInteger)
pblsExpressionMetricsV1 = phoistAcyclic $ pfix $ \self -> plam $ \expression ->
  pmatch expression $ \case
    PBlsMillerLoopExpression _ _ -> pcon (PPair 1 1)
    PBlsMultiplyExpression left right ->
      pmatch (self # pfromData left) $ \(PPair leftLeaves leftDepth) ->
      pmatch (self # pfromData right) $ \(PPair rightLeaves rightDepth) ->
        pcon $ PPair (leftLeaves + rightLeaves) (pif (rightDepth #<= leftDepth) leftDepth rightDepth + 1)

pverifyDirectBlsFinalWithExpectedRootsV1 ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PBool ->
  Term s PBlsExpressionWitnessV1 ->
  Term s PBlsExpressionWitnessV1 ->
  Term s PValueWitnessV1 ->
  Term s PBool
pverifyDirectBlsFinalWithExpectedRootsV1 builtinRoot expectedLeft expectedRight enforce left right result =
  pmatch (pblsExpressionMetricsV1 # left) $ \(PPair leftLeaves leftDepth) ->
  pmatch (pblsExpressionMetricsV1 # right) $ \(PPair rightLeaves rightDepth) ->
    pif
      ( leftLeaves + rightLeaves #<= pmaxDirectBlsMillerLoopLeaves
          #&& leftDepth #<= pmaxDirectBlsMillerLoopLeaves
          #&& rightDepth #<= pmaxDirectBlsMillerLoopLeaves
      )
      (pmatch (pevaluateBlsExpressionV1 # left) $ \(PBlsEvaluationV1 leftRoot leftResult) ->
       pmatch (pevaluateBlsExpressionV1 # right) $ \(PBlsEvaluationV1 rightRoot rightResult) ->
       plet
        (pcons # pdata (pcon $ PBlsMillerLoopValue $ pdata leftRoot)
          #$ pcons # pdata (pcon $ PBlsMillerLoopValue $ pdata rightRoot) # pnil)
        $ \arguments ->
          pmatch (pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
            pand'List
              [ pif enforce (leftRoot #== expectedLeft #&& rightRoot #== expectedRight) (pconstant True)
              , builtinRoot #== phashBuiltinValueV1 # 70 # 0 # argumentsCount # argumentsRoot
              , presultIsConstantV1 result (pcon PBooleanConstant)
                  (pboolData $ pbls12_381_finalVerify # leftResult # rightResult)
              ])
      perror

pverifyDirectBlsFinalV1 ::
  forall (s :: S).
  Term s (PByteString :--> PBlsExpressionWitnessV1 :--> PBlsExpressionWitnessV1 :--> PValueWitnessV1 :--> PBool)
pverifyDirectBlsFinalV1 = phoistAcyclic $ plam $ \builtinRoot left right result ->
  pverifyDirectBlsFinalWithExpectedRootsV1 builtinRoot (pconstant "") (pconstant "") (pconstant False) left right result

pverifyDirectBlsFinalRootsV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PBlsExpressionWitnessV1 :--> PBlsExpressionWitnessV1 :--> PValueWitnessV1 :--> PBool)
pverifyDirectBlsFinalRootsV1 = phoistAcyclic $ plam $ \builtinRoot expectedLeft expectedRight left right result ->
  pif (plengthBS # expectedLeft #== 32 #&& plengthBS # expectedRight #== 32)
    (pverifyDirectBlsFinalWithExpectedRootsV1 builtinRoot expectedLeft expectedRight (pconstant True) left right result)
    perror

-- Runtime kind codes are internal and follow the Aiken declaration order.
pkAny, pkInteger, pkBytes, pkString, pkUnit, pkBoolean, pkList, pkPair, pkData, pkG1, pkG2, pkMiller, pkListData, pkListDataPair, pkListInteger ::
  forall (s :: S). Term s PInteger
pkAny = 0
pkInteger = 1
pkBytes = 2
pkString = 3
pkUnit = 4
pkBoolean = 5
pkList = 6
pkPair = 7
pkData = 8
pkG1 = 9
pkG2 = 10
pkMiller = 11
pkListData = 12
pkListDataPair = 13
pkListInteger = 14

pkindMatchesTypeV1 ::
  forall (s :: S). Term s PInteger -> Term s PConstantTypeV1 -> Term s PBool
pkindMatchesTypeV1 kind constantType =
  pif (kind #== pkInteger) (constantType #== pcon PIntegerConstant) $
  pif (kind #== pkBytes) (constantType #== pcon PByteStringConstant) $
  pif (kind #== pkString) (constantType #== pcon PStringConstant) $
  pif (kind #== pkUnit) (constantType #== pcon PUnitConstant) $
  pif (kind #== pkBoolean) (constantType #== pcon PBooleanConstant) $
  pif (kind #== pkList)
    (pmatch constantType $ \case PListConstant _ -> pconstant True; _ -> pconstant False) $
  pif (kind #== pkPair)
    (pmatch constantType $ \case PPairConstant _ _ -> pconstant True; _ -> pconstant False) $
  pif (kind #== pkData) (constantType #== pcon PDataConstant) $
  pif (kind #== pkG1) (constantType #== pcon PBlsG1Constant) $
  pif (kind #== pkG2) (constantType #== pcon PBlsG2Constant) $
  pif (kind #== pkListData)
    (constantType #== pcon (PListConstant (pdata $ pcon PDataConstant))) $
  pif (kind #== pkListDataPair)
    (constantType #== pcon (PListConstant (pdata $ pcon (PPairConstant (pdata $ pcon PDataConstant) (pdata $ pcon PDataConstant))))) $
  pif (kind #== pkListInteger)
    (constantType #== pcon (PListConstant (pdata $ pcon PIntegerConstant)))
    (pconstant False)

pruntimeValueMatchesKindV1 ::
  forall (s :: S). Term s PRuntimeValueWitnessV1 -> Term s PInteger -> Term s PBool
pruntimeValueMatchesKindV1 value kind =
  pif (kind #== pkAny) (pconstant True) $
  pmatch value $ \case
    PRuntimeConstantValue witness -> pkindMatchesTypeV1 kind (pconstantTypeV1 # pfromData witness)
    PRuntimeSemanticConstantValue typeCbor payload memory ->
      pmatch (pfromData payload) $ \summary ->
        pif
          ( plengthBS # pfromData (psummary'root summary) #== 32
              #&& 0 #<= pfromData (psummary'cborLength summary)
              #&& 0 #<= pfromData (psummary'memory summary)
              #&& 0 #<= pfromData memory
          )
          (pkindMatchesTypeV1 kind $ pdecodeConstantTypeV1 # pfromData typeCbor)
          perror
    PRuntimeBlsMillerLoopValue _ -> kind #== pkMiller
    _ -> pconstant False

pruntimeArgumentsMatchKindsV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PRuntimeValueWitnessV1) :--> PBuiltinList PInteger :--> PBool)
pruntimeArgumentsMatchKindsV1 = phoistAcyclic $ pfix $ \self -> plam $ \arguments kinds ->
  pelimList
    (\argument rest -> pelimList
      (\kind remaining -> pruntimeValueMatchesKindV1 (pfromData argument) kind #&& self # rest # remaining)
      (pconstant False)
      kinds)
    (pnull # kinds)
    arguments

pkinds :: forall (s :: S). [Integer] -> Term s (PBuiltinList PInteger)
pkinds = foldr (\kind rest -> pcons # pconstant kind # rest) pnil

pbuiltinArgumentKindsV1 :: forall (s :: S). Term s PInteger -> Term s (PBuiltinList PInteger)
pbuiltinArgumentKindsV1 tag =
  pif (0 #<= tag #&& tag #<= 9) (pkinds [1, 1]) $
  pif (tag #== 10) (pkinds [2, 2]) $
  pif (tag #== 11) (pkinds [1, 2]) $
  pif (tag #== 12) (pkinds [1, 1, 2]) $
  pif (tag #== 13) (pkinds [2]) $
  pif (tag #== 14) (pkinds [2, 1]) $
  pif (15 #<= tag #&& tag #<= 17) (pkinds [2, 2]) $
  pif (18 #<= tag #&& tag #<= 20) (pkinds [2]) $
  pif (tag #== 21) (pkinds [2, 2, 2]) $
  pif (tag #== 22 #|| tag #== 23) (pkinds [3, 3]) $
  pif (tag #== 24) (pkinds [3]) $
  pif (tag #== 25) (pkinds [2]) $
  pif (tag #== 26) (pkinds [5, 0, 0]) $
  pif (tag #== 27) (pkinds [4, 0]) $
  pif (tag #== 28) (pkinds [3, 0]) $
  pif (tag #== 29 #|| tag #== 30) (pkinds [7]) $
  pif (tag #== 31) (pkinds [6, 0, 0]) $
  pif (tag #== 32) (pkinds [0, 6]) $
  pif (33 #<= tag #&& tag #<= 35) (pkinds [6]) $
  pif (tag #== 36) (pkinds [8, 0, 0, 0, 0, 0]) $
  pif (tag #== 37) (pkinds [1, 12]) $
  pif (tag #== 38) (pkinds [13]) $
  pif (tag #== 39) (pkinds [12]) $
  pif (tag #== 40) (pkinds [1]) $
  pif (tag #== 41) (pkinds [2]) $
  pif (42 #<= tag #&& tag #<= 46) (pkinds [8]) $
  pif (tag #== 47 #|| tag #== 48) (pkinds [8, 8]) $
  pif (tag #== 49 #|| tag #== 50) (pkinds [4]) $
  pif (tag #== 51) (pkinds [8]) $
  pif (tag #== 52 #|| tag #== 53) (pkinds [2, 2, 2]) $
  pif (tag #== 54 #|| tag #== 57) (pkinds [9, 9]) $
  pif (tag #== 55 #|| tag #== 59) (pkinds [9]) $
  pif (tag #== 56) (pkinds [1, 9]) $
  pif (tag #== 58) (pkinds [2, 2]) $
  pif (tag #== 60) (pkinds [2]) $
  pif (tag #== 61 #|| tag #== 64) (pkinds [10, 10]) $
  pif (tag #== 62 #|| tag #== 66) (pkinds [10]) $
  pif (tag #== 63) (pkinds [1, 10]) $
  pif (tag #== 65) (pkinds [2, 2]) $
  pif (tag #== 67) (pkinds [2]) $
  pif (tag #== 68) (pkinds [9, 10]) $
  pif (tag #== 69 #|| tag #== 70) (pkinds [11, 11]) $
  pif (tag #== 71 #|| tag #== 72) (pkinds [2]) $
  pif (tag #== 73) (pkinds [5, 1, 1]) $
  pif (tag #== 74) (pkinds [5, 2]) $
  pif (75 #<= tag #&& tag #<= 77) (pkinds [5, 2, 2]) $
  pif (tag #== 78) (pkinds [2]) $
  pif (tag #== 79) (pkinds [2, 1]) $
  pif (tag #== 80) (pkinds [2, 14, 5]) $
  pif (tag #== 81) (pkinds [1, 1]) $
  pif (tag #== 82 #|| tag #== 83) (pkinds [2, 1]) $
  pif (84 #<= tag #&& tag #<= 86) (pkinds [2])
    perror

pmkConsArgumentsAreWellTypedV1 ::
  forall (s :: S). Term s (PBuiltinList (PAsData PRuntimeValueWitnessV1)) -> Term s PBool
pmkConsArgumentsAreWellTypedV1 arguments =
  pelimList
    (\element rest -> pelimList
      (\listValue tailArguments -> pif (pnull # tailArguments)
        (pmatch (pfromData element) $ \case
          PRuntimeConstantValue elementWitness -> pmatch (pfromData listValue) $ \case
            PRuntimeConstantValue listWitness -> pmatch (pconstantTypeV1 # pfromData listWitness) $ \case
              PListConstant elementType -> pconstantTypeV1 # pfromData elementWitness #== pfromData elementType
              _ -> pconstant False
            _ -> pconstant False
          _ -> pconstant False)
        (pconstant False))
      (pconstant False)
      rest)
    (pconstant False)
    arguments

prevealedRuntimePayloadBytesV1 ::
  forall (s :: S). Term s (PBuiltinList (PAsData PRuntimeValueWitnessV1) :--> PInteger)
prevealedRuntimePayloadBytesV1 = phoistAcyclic $ plam $ \arguments ->
  pfoldl
    # plam (\total argument -> total + pmatch (pfromData argument) (\case
        PRuntimeConstantValue witness -> pmatch (pfromData witness) $ \w -> plengthBS # pfromData (pwitness'payloadCbor w)
        _ -> 0))
    # 0
    # arguments

pverifyBuiltinTypeFailureV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PRuntimeValueWitnessV1) :--> PBool)
pverifyBuiltinTypeFailureV1 = phoistAcyclic $ plam $ \tag builtinRoot arguments ->
  pif (prevealedRuntimePayloadBytesV1 # arguments #<= pmaxDirectBuiltinRevealedPayloadBytes)
    (pmatch (pruntimeArgumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
      pand'List
        [ argumentsCount #== pbuiltinArgumentCountV1 # tag
        , builtinRoot #== phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot
        , pnot # pif (tag #== 32)
            (pmkConsArgumentsAreWellTypedV1 arguments)
            (pruntimeArgumentsMatchKindsV1 # arguments # pbuiltinArgumentKindsV1 tag)
        ])
    perror

pisValidUtf8V1 :: forall (s :: S). Term s (PByteString :--> PBool)
pisValidUtf8V1 = phoistAcyclic $
  plam $ \bytes ->
    pfix
      ( \go -> plam $ \index ->
          plet (plengthBS # bytes) $ \len ->
            pif (index #== len) (pconstant True) $
            pif (len #< index) (pconstant False) $
            plet (pindexBS' # bytes # index) $ \first ->
              pif (first #<= 0x7f) (go # (index + 1)) $
              pif (0xc2 #<= first #&& first #<= 0xdf) (tailBytes bytes go (index + 1) 1) $
              pif (index + 1 #>= len) (pconstant False) $
              plet (pindexBS' # bytes # (index + 1)) $ \second ->
                pif (first #== 0xe0)
                  (0xa0 #<= second #&& second #<= 0xbf #&& tailBytes bytes go (index + 2) 1) $
                pif (first #== 0xed)
                  (0x80 #<= second #&& second #<= 0x9f #&& tailBytes bytes go (index + 2) 1) $
                pif ((0xe1 #<= first #&& first #<= 0xec) #|| (0xee #<= first #&& first #<= 0xef))
                  (continuation second #&& tailBytes bytes go (index + 2) 1) $
                pif (first #== 0xf0)
                  (0x90 #<= second #&& second #<= 0xbf #&& tailBytes bytes go (index + 2) 2) $
                pif (0xf1 #<= first #&& first #<= 0xf3)
                  (continuation second #&& tailBytes bytes go (index + 2) 2) $
                pif (first #== 0xf4)
                  (0x80 #<= second #&& second #<= 0x8f #&& tailBytes bytes go (index + 2) 2)
                  (pconstant False)
      )
      # 0
  where
    continuation :: forall (s :: S). Term s PInteger -> Term s PBool
    continuation byte = 0x80 #<= byte #&& byte #<= 0xbf
    tailBytes ::
      forall (s :: S).
      Term s PByteString ->
      Term s (PInteger :--> PBool) ->
      Term s PInteger ->
      Term s PInteger ->
      Term s PBool
    tailBytes source go index required =
      pfix
        (\tailGo -> plam $ \cursor remaining ->
          pif (remaining #== 0) (go # cursor) $
          pif (cursor #< plengthBS # source)
            (continuation (pindexBS' # source # cursor) #&& tailGo # (cursor + 1) # (remaining - 1))
            (pconstant False))
        # index
        # required

punsignedIntegerByteSizeV1 :: forall (s :: S). Term s (PInteger :--> PInteger)
punsignedIntegerByteSizeV1 = phoistAcyclic $ pfix $ \self -> plam $ \value ->
  pif (value #== 0) 0 $ pif (value #< 256) 1 (1 + self # (pquot # value # 256))

pintegerToBytesFailsV1 ::
  forall (s :: S). Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PBool
pintegerToBytesFailsV1 arguments =
  pwithThreeValues arguments $ \endian sizeValue integerValue ->
    plet (pbooleanV1 endian) $ \endianValue ->
    pif (endianValue #|| pnot # endianValue)
      (plet (pintegerV1 sizeValue) $ \size ->
       plet (pintegerV1 integerValue) $ \integer ->
       plet (pif (integer #< 0) 0 (punsignedIntegerByteSizeV1 # integer)) $ \required ->
        size #< 0 #|| 8192 #< size #|| integer #< 0 #|| 8192 #< required
          #|| (0 #< size #&& size #< required))
      perror

pmodularPowerV1 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pmodularPowerV1 = phoistAcyclic $ plam $ \base exponent modulus ->
  pif (0 #<= exponent #&& 1 #< modulus)
    (pfix (\self -> plam $ \current remaining accumulator ->
      pif (remaining #== 0) accumulator $
        self # (pmod # (current * current) # modulus) # (pquot # remaining # 2)
          # pif (pmod # remaining # 2 #== 1) (pmod # (accumulator * current) # modulus) accumulator)
      # (pmod # base # modulus) # exponent # 1)
    perror

psecp256k1Prime :: forall (s :: S). Term s PInteger
psecp256k1Prime = 115792089237316195423570985008687907853269984665640564039457584007908834671663

psecp256k1XIsOnCurveV1 :: forall (s :: S). Term s PInteger -> Term s PBool
psecp256k1XIsOnCurveV1 x =
  pif (x #< 0 #|| psecp256k1Prime #<= x) (pconstant False) $
  plet (pmod # (x * x * x + 7) # psecp256k1Prime) $ \ySquared ->
    ySquared #== 0
      #|| pmodularPowerV1 # ySquared # (pquot # (psecp256k1Prime - 1) # 2) # psecp256k1Prime #== 1

psecp256k1PublicKeyIsValidV1 ::
  forall (s :: S). Term s PInteger -> Term s PByteString -> Term s PBool
psecp256k1PublicKeyIsValidV1 tag key =
  pif (tag #== 52)
    (plengthBS # key #== 33
      #&& (pindexBS' # key # 0 #== 2 #|| pindexBS' # key # 0 #== 3)
      #&& psecp256k1XIsOnCurveV1
        (pbyteStringToIntegerBuiltin # pconstant True # (psliceBS # 1 # 32 # key)))
    (pif (tag #== 53)
      (plengthBS # key #== 32 #&& psecp256k1XIsOnCurveV1 (pbyteStringToIntegerBuiltin # pconstant True # key))
      perror)

pblsPrime :: forall (s :: S). Term s PInteger
pblsPrime = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787

pblsCompressedCoordinateV1 ::
  forall (s :: S). Term s PByteString -> Term s PInteger -> Term s PInteger
pblsCompressedCoordinateV1 encoded offset =
  pbyteStringToIntegerBuiltin # pconstant True
    # (pconsBS' # (pmod # (pindexBS' # encoded # offset) # 32) # (psliceBS # (offset + 1) # 47 # encoded))

pblsUncompressHeaderIsInvalidV1 ::
  forall (s :: S). Term s PInteger -> Term s PByteString -> Term s PBool
pblsUncompressHeaderIsInvalidV1 tag encoded =
  plet (pif (tag #== 60) 48 (pif (tag #== 67) 96 perror)) $ \expectedLength ->
  pif (plengthBS # encoded #== expectedLength)
    (plet (pindexBS' # encoded # 0) $ \first ->
     plet (pmod # first # 128 #>= 64) $ \infinity ->
     plet (pblsCompressedCoordinateV1 encoded 0) $ \firstCoordinate ->
     plet (pif (tag #== 67)
            (pbyteStringToIntegerBuiltin # pconstant True # (psliceBS # 48 # 48 # encoded)) 0) $ \secondCoordinate ->
      first #< 128
        #|| pif infinity
          (pmod # first # 64 #>= 32 #|| firstCoordinate #/= 0 #|| secondCoordinate #/= 0)
          (pblsPrime #<= firstCoordinate #|| pblsPrime #<= secondCoordinate))
    perror

pblsG1CurveEncodingIsInvalidV1 :: forall (s :: S). Term s PByteString -> Term s PBool
pblsG1CurveEncodingIsInvalidV1 encoded =
  pif (plengthBS # encoded #== 48)
    (pif (pmod # (pindexBS' # encoded # 0) # 128 #>= 64) (pconstant False) $
      plet (pblsCompressedCoordinateV1 encoded 0) $ \x ->
      plet (pmod # (x * x * x + 4) # pblsPrime) $ \ySquared ->
        ySquared #/= 0
          #&& pmodularPowerV1 # ySquared # (pquot # (pblsPrime - 1) # 2) # pblsPrime #/= 1)
    perror

pmalformedSignatureV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PBool
pmalformedSignatureV1 tag arguments =
  pwithThreeValues arguments $ \keyValue messageValue signatureValue ->
    plet (pbytesV1 keyValue) $ \key ->
    plet (pbytesV1 messageValue) $ \message ->
    plet (pbytesV1 signatureValue) $ \signature ->
      pif (tag #== 21)
        (plengthBS # key #/= 32 #|| plengthBS # signature #/= 64)
      $ pif (tag #== 52)
        (pif (plengthBS # key #/= 33 #|| plengthBS # message #/= 32 #|| plengthBS # signature #/= 64)
          (pconstant True) (pnot # psecp256k1PublicKeyIsValidV1 tag key))
      $ pif (tag #== 53)
        (pif (plengthBS # key #/= 32 #|| plengthBS # message #/= 32 #|| plengthBS # signature #/= 64)
          (pconstant True) (pnot # psecp256k1PublicKeyIsValidV1 tag key))
        perror

pallBitIndicesValidV1 ::
  forall (s :: S). Term s (PBuiltinList PInteger :--> PInteger :--> PBool)
pallBitIndicesValidV1 = phoistAcyclic $ pfix $ \self -> plam $ \indices bitLength ->
  pelimList
    (\index rest -> 0 #<= index #&& index #< bitLength #&& self # rest # bitLength)
    (pconstant True)
    indices

pknownBuiltinFailureV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PBool
pknownBuiltinFailureV1 tag arguments =
  pif (3 #<= tag #&& tag #<= 6)
    (pwithTwoValues arguments $ \_ divisor -> pintegerV1 divisor #== 0) $
  pif (tag #== 11)
    (pwithTwoValues arguments $ \byte _ -> plet (pintegerV1 byte) $ \value -> value #< 0 #|| 255 #< value) $
  pif (tag #== 14)
    (pwithTwoValues arguments $ \source index -> plet (pintegerV1 index) $ \position ->
      position #< 0 #|| plengthBS # pbytesV1 source #<= position #|| 9223372036854775807 #< position) $
  pif (tag #== 21) (pmalformedSignatureV1 tag arguments) $
  pif (tag #== 25)
    (pwithOneValue arguments $ \source -> pnot #$ pisValidUtf8V1 # pbytesV1 source) $
  pif (tag #== 33 #|| tag #== 34)
    (pwithOneValue arguments $ \source -> pwithListPayload source $ \_ items -> pnull # items) $
  pif (42 #<= tag #&& tag #<= 46)
    (pwithOneValue arguments $ \source -> plet (pdataPayloadV1 source) $ \value ->
      pif (tag #== 42) (pnot # pdataIsConstr value) $
      pif (tag #== 43) (pnot # pdataIsMap value) $
      pif (tag #== 44) (pnot # pdataIsList value) $
      pif (tag #== 45) (pnot # pdataIsInteger value) (pnot # pdataIsBytes value)) $
  pif (tag #== 52 #|| tag #== 53) (pmalformedSignatureV1 tag arguments) $
  pif (tag #== 58 #|| tag #== 65)
    (pwithTwoValues arguments $ \_ domain -> plengthBS # pbytesV1 domain #> 255) $
  pif (tag #== 60)
    (pwithOneValue arguments $ \source -> plet (pbytesV1 source) $ \encoded ->
      plengthBS # encoded #/= 48
        #|| pif (plengthBS # encoded #== 48)
          (plet (pblsUncompressHeaderIsInvalidV1 tag encoded) $ \headerInvalid ->
            headerInvalid #|| pif (pnot # headerInvalid) (pblsG1CurveEncodingIsInvalidV1 encoded) (pconstant False))
          (pconstant False)) $
  pif (tag #== 67)
    (pwithOneValue arguments $ \source -> plet (pbytesV1 source) $ \encoded ->
      plengthBS # encoded #/= 96
        #|| pif (plengthBS # encoded #== 96) (pblsUncompressHeaderIsInvalidV1 tag encoded) (pconstant False)) $
  pif (tag #== 73) (pintegerToBytesFailsV1 arguments) $
  pif (tag #== 79)
    (pwithTwoValues arguments $ \source index -> plet (pintegerV1 index) $ \position ->
      position #< 0 #|| plengthBS # pbytesV1 source * 8 #<= position) $
  pif (tag #== 80)
    (pwithThreeValues arguments $ \source indices _ -> pwithListPayload indices $ \_ indexData ->
      pnot #$ pallBitIndicesValidV1 # (psemanticIntegersV1 # indexData) # (plengthBS # pbytesV1 source * 8)) $
  pif (tag #== 81)
    (pwithTwoValues arguments $ \lengthValue byteValue ->
      plet (pintegerV1 lengthValue) $ \len -> plet (pintegerV1 byteValue) $ \byte ->
        len #< 0 #|| 8192 #< len #|| byte #< 0 #|| 255 #< byte)
    (pconstant False)

pverifyDirectBuiltinFailureV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PValueWitnessV1) :--> PBool)
pverifyDirectBuiltinFailureV1 = phoistAcyclic $ plam $ \tag builtinRoot arguments ->
  pif (prevealedArgumentPayloadBytesV1 # arguments #<= pmaxDirectBuiltinRevealedPayloadBytes)
    (pmatch (pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
      pand'List
        [ argumentsCount #== pbuiltinArgumentCountV1 # tag
        , builtinRoot #== phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot
        , pknownBuiltinFailureV1 tag arguments
        ])
    perror

pdirectBuiltinFailureBudgetV1 ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PValueWitnessV1) :--> PBuiltinBudgetV1)
pdirectBuiltinFailureBudgetV1 = phoistAcyclic $ plam $ \tag arguments ->
  pif (pknownBuiltinFailureV1 tag arguments)
    (plet
      (pif (tag #== 60)
        (pwithOneValue arguments $ \source -> plengthBS # pbytesV1 source #== 48)
        (pif (tag #== 67)
          (pwithOneValue arguments $ \source -> plengthBS # pbytesV1 source #== 96)
          (pconstant False)))
      $ \blsPaid ->
        pif
          ( tag #== 4 #|| tag #== 5 #|| tag #== 6 #|| tag #== 52 #|| tag #== 53
              #|| tag #== 58 #|| tag #== 65 #|| tag #== 73 #|| blsPaid
          )
          (pdirectBuiltinBudgetV1 # tag # arguments)
          (pcon $ PBuiltinBudgetV1 (pdata 0) (pdata 0)))
    perror

pmaxDirectBuiltinRevealedPayloadBytes :: forall (s :: S). Term s PInteger
pmaxDirectBuiltinRevealedPayloadBytes = 9215

prevealedPayloadBytesV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PInteger)
prevealedPayloadBytesV1 = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PConstantValue witness -> pmatch (pfromData witness) $ \w ->
    plengthBS # pfromData (pwitness'payloadCbor w)
  PSemanticConstantValue _ _ _ -> 0
  POpaqueValue _ -> 0
  PBlsMillerLoopValue _ -> 0

prevealedArgumentPayloadBytesV1 ::
  forall (s :: S). Term s (PBuiltinList (PAsData PValueWitnessV1) :--> PInteger)
prevealedArgumentPayloadBytesV1 = phoistAcyclic $
  plam $ \arguments -> pfoldl # plam (\total value -> total + prevealedPayloadBytesV1 # pfromData value) # 0 # arguments

pverifyDirectBuiltinV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PValueWitnessV1) :--> PValueWitnessV1 :--> PBool)
pverifyDirectBuiltinV1 = phoistAcyclic $ plam $ \tag builtinRoot arguments result ->
  pmatch (pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
    pif
      ( pand'List
          [ builtinRoot #== phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot
          , prevealedArgumentPayloadBytesV1 # arguments + prevealedPayloadBytesV1 # result
              #<= pmaxDirectBuiltinRevealedPayloadBytes
          ]
      )
      ( pif (0 #<= tag #&& tag #<= 9)
          (pverifyIntegerBinaryV1 tag arguments result)
          (pif (10 #<= tag #&& tag #<= 20)
            (pverifyBytesV1 tag arguments result)
            (pif (tag #== 21 #|| tag #== 52 #|| tag #== 53)
              (pverifySignatureV1 tag arguments result)
            (pif (22 #<= tag #&& tag #<= 25)
              (pverifyStringsV1 tag arguments result)
            (pif (26 #<= tag #&& tag #<= 28)
              (pverifyControlV1 tag arguments result)
              (pif (29 #<= tag #&& tag #<= 35)
                (pverifyPairAndListV1 tag arguments result)
                (pif (tag #== 36)
                  (pverifyChooseDataV1 arguments result)
                (pif (37 #<= tag #&& tag #<= 51)
                  (pverifyDataV1 tag arguments result)
                (pif (54 #<= tag #&& tag #<= 60)
                  (pverifyBlsG1V1 tag arguments result)
                (pif (61 #<= tag #&& tag #<= 67)
                  (pverifyBlsG2V1 tag arguments result)
                (pif (tag #== 68 #|| tag #== 69)
                  (pverifyBlsExpressionV1 tag arguments result)
                (pif (71 #<= tag #&& tag #<= 86)
                  (pverifyV3BytesV1 tag arguments result)
                  (pconstant False))))))))))))
      )
      perror

data PSemanticPartsV1 (s :: S) = PSemanticPartsV1
  (Term s PConstantTypeV1)
  (Term s PDataSummaryV1)
  (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PSemanticPartsV1)

psemanticPartsV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PSemanticPartsV1
psemanticPartsV1 value = pmatch value $ \case
  PConstantValue witness ->
    plet (pconstantPayloadV1 # pfromData witness) $ \payload ->
      pcon $ PSemanticPartsV1
        (pconstantTypeV1 # pfromData witness)
        (psemanticDataSummaryV1 # payload)
        (pconstantMemorySizeV1 # pfromData witness)
  PSemanticConstantValue typeCbor payload memory ->
    pmatch (pfromData payload) $ \summary ->
      pif
        (plengthBS # pfromData (psummary'root summary) #== 32
          #&& 0 #<= pfromData (psummary'cborLength summary)
          #&& 0 #<= pfromData (psummary'memory summary)
          #&& 0 #<= pfromData memory)
        (pcon $ PSemanticPartsV1 (pdecodeConstantTypeV1 # pfromData typeCbor) (pfromData payload) (pfromData memory))
        perror
  _ -> perror

psemanticConstantTypeV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PConstantTypeV1)
psemanticConstantTypeV1 = phoistAcyclic $ plam $ \value ->
  pmatch (psemanticPartsV1 value) $ \(PSemanticPartsV1 constantType _ _) -> constantType

psemanticConstantPayloadV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PDataSummaryV1)
psemanticConstantPayloadV1 = phoistAcyclic $ plam $ \value ->
  pmatch (psemanticPartsV1 value) $ \(PSemanticPartsV1 _ payload _) -> payload

psemanticConstantMemoryV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PInteger)
psemanticConstantMemoryV1 = phoistAcyclic $ plam $ \value ->
  pmatch (psemanticPartsV1 value) $ \(PSemanticPartsV1 _ _ memory) -> memory

psummaryFromNodeV1 :: forall (s :: S). Term s PDataNodeV1 -> Term s PDataSummaryV1
psummaryFromNodeV1 node =
  pcon $ PDataSummaryV1
    (pdata $ phashDataNodeV1 # node)
    (pdata $ pdataNodeCborLengthV1 # node)
    (pdata $ pdataNodeMemoryV1 # node)

ptopNodeMatchesV1 ::
  forall (s :: S).
  Term s PDataSummaryV1 -> Term s PDataNodeV1 -> Term s (PMaybeData PDataListNodeV1) -> Term s (PMaybeData PDataPairNodeV1) -> Term s PBool
ptopNodeMatchesV1 summary node listSummary pairSummary =
  summary #== psummaryFromNodeV1 node
    #&& pverifyDataNodeV1 # node # listSummary # pairSummary

pdataConstantSummaryV1 :: forall (s :: S). Term s PValueWitnessV1 -> Term s PDataSummaryV1
pdataConstantSummaryV1 value = pmatch (psemanticPartsV1 value) $ \(PSemanticPartsV1 constantType payload memory) ->
  pif (constantType #== pcon PDataConstant #&& memory #== pfromDataSummaryMemory payload) payload perror
  where
    pfromDataSummaryMemory summary = pmatch summary $ \s -> pfromData (psummary'memory s)

presultIsSemanticConstantV1 ::
  forall (s :: S).
  Term s PValueWitnessV1 -> Term s PConstantTypeV1 -> Term s PDataSummaryV1 -> Term s PInteger -> Term s PBool
presultIsSemanticConstantV1 result expectedType expectedPayload expectedMemory =
  pmatch (psemanticPartsV1 result) $ \(PSemanticPartsV1 actualType actualPayload actualMemory) ->
    actualType #== expectedType #&& actualPayload #== expectedPayload #&& actualMemory #== expectedMemory

pexactOneData ::
  forall (s :: S) (a :: S -> Type) (r :: S -> Type). (PIsData a) =>
  Term s (PBuiltinList (PAsData a)) -> (Term s a -> Term s r) -> Term s r
pexactOneData values k = pelimList (\value rest -> pif (pnull # rest) (k $ pfromData value) perror) perror values

pexactTwoData ::
  forall (s :: S) (a :: S -> Type) (r :: S -> Type). (PIsData a) =>
  Term s (PBuiltinList (PAsData a)) -> (Term s a -> Term s a -> Term s r) -> Term s r
pexactTwoData values k = pelimList (\first rest -> pelimList
  (\second tailValues -> pif (pnull # tailValues) (k (pfromData first) (pfromData second)) perror)
  perror rest) perror values

pexactThreeData ::
  forall (s :: S) (a :: S -> Type) (r :: S -> Type). (PIsData a) =>
  Term s (PBuiltinList (PAsData a)) -> (Term s a -> Term s a -> Term s a -> Term s r) -> Term s r
pexactThreeData values k = pelimList (\first r1 -> pelimList (\second r2 -> pelimList
  (\third end -> pif (pnull # end) (k (pfromData first) (pfromData second) (pfromData third)) perror)
  perror r2) perror r1) perror values

plistCandidateForNodeV1 ::
  forall (s :: S). Term s PDataNodeV1 -> Term s (PBuiltinList (PAsData PDataListNodeV1)) -> Term s (PMaybeData PDataListNodeV1)
plistCandidateForNodeV1 node candidates =
  plet
    (pmatch node $ \case
      PConstrSmallData {pnode'fieldsCount} -> pfromData pnode'fieldsCount
      PConstrLargeData {pnode'fieldsCount} -> pfromData pnode'fieldsCount
      PListDataNode {pnode'itemsCount} -> pfromData pnode'itemsCount
      _ -> 0)
    $ \count -> pif (count #== 0)
      (pif (pnull # candidates) (pcon PDNothing) perror)
      (pexactOneData candidates $ \summary -> pcon $ PDJust $ pdata summary)

ppairCandidateForNodeV1 ::
  forall (s :: S). Term s PDataNodeV1 -> Term s (PBuiltinList (PAsData PDataPairNodeV1)) -> Term s (PMaybeData PDataPairNodeV1)
ppairCandidateForNodeV1 node candidates =
  plet (pmatch node $ \case PMapDataNode {pnode'entriesCount} -> pfromData pnode'entriesCount; _ -> 0) $ \count ->
    pif (count #== 0)
      (pif (pnull # candidates) (pcon PDNothing) perror)
      (pexactOneData candidates $ \summary -> pcon $ PDJust $ pdata summary)

psequenceFromListNodeV1 :: forall (s :: S). Term s PDataNodeV1 -> Term s PDataSequenceSummaryV1
psequenceFromListNodeV1 node = pmatch node $ \case
  PListDataNode {pnode'itemsCount, pnode'itemsRoot, pnode'memory} ->
    pcon $ PDataSequenceSummaryV1
      pnode'itemsRoot
      pnode'itemsCount
      (pdata $ pdataNodeCborLengthV1 # node - pif (pfromData pnode'itemsCount #== 0) 1 2)
      (pdata $ pfromData pnode'memory - 4)
  _ -> perror

pemptySemanticMaterialV1 :: forall (s :: S). PSemanticBuiltinWitnessV1 s -> Term s PBool
pemptySemanticMaterialV1 witness =
  pnull # pfromData (psemantic'dataNodes witness)
    #&& pnull # pfromData (psemantic'listNodes witness)
    #&& pnull # pfromData (psemantic'pairNodes witness)
    #&& pnull # pfromData (psemantic'scalarPreimages witness)

plistConstantSequenceV1 ::
  forall (s :: S).
  Term s PValueWitnessV1 -> Term s PConstantTypeV1 -> Term s PDataNodeV1 -> Term s (PMaybeData PDataListNodeV1) -> Term s PDataSequenceSummaryV1
plistConstantSequenceV1 value expectedElement node first =
  pmatch (psemanticPartsV1 value) $ \(PSemanticPartsV1 constantType payload memory) ->
    pif (constantType #== pcon (PListConstant $ pdata expectedElement)
      #&& ptopNodeMatchesV1 payload node first (pcon PDNothing))
      (plet (psequenceFromListNodeV1 node) $ \sequence ->
        pmatch sequence $ \s -> pif (memory #== pfromData (pseq'memory s)) sequence perror)
      perror

pcanonicalIntegerLeafV1 ::
  forall (s :: S). Term s PDataSummaryV1 -> Term s PDataNodeV1 -> Term s PByteString -> Term s PInteger
pcanonicalIntegerLeafV1 summary node rawCbor = pmatch node $ \case
  PIntegerDataNode {pnode'cborRoot, pnode'cborLength} ->
    pif (0 #< plengthBS # rawCbor
      #&& plengthBS # rawCbor #<= 4095
      #&& pfromData pnode'cborRoot #== pboundedBlobRootV1 # rawCbor
      #&& pfromData pnode'cborLength #== plengthBS # rawCbor
      #&& ptopNodeMatchesV1 summary node (pcon PDNothing) (pcon PDNothing))
      (pmatch (pdeserialise # rawCbor) $ \case
        PNothing -> perror
        PJust dataValue ->
          pif (pserialiseData # dataValue #== rawCbor #&& pdataIsInteger dataValue)
            (pasInt # dataValue) perror)
      perror
  _ -> perror

pcanonicalBytesLeafV1 ::
  forall (s :: S). Term s PDataSummaryV1 -> Term s PDataNodeV1 -> Term s PByteString -> Term s PByteString
pcanonicalBytesLeafV1 summary node bytesValue = pmatch node $ \case
  PBytesDataNode {pnode'bytesRoot, pnode'bytesLength} ->
    pif (plengthBS # bytesValue #<= 4095
      #&& pfromData pnode'bytesRoot #== pboundedBlobRootV1 # bytesValue
      #&& pfromData pnode'bytesLength #== plengthBS # bytesValue
      #&& ptopNodeMatchesV1 summary node (pcon PDNothing) (pcon PDNothing))
      bytesValue perror
  _ -> perror

pverifySemanticChooseDataV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticChooseDataV1 arguments result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pif (pnull # pfromData (psemantic'scalarPreimages w))
    (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \listSummary ->
     plet (ppairCandidateForNodeV1 node $ pfromData $ psemantic'pairNodes w) $ \pairSummary ->
     pif (ptopNodeMatchesV1 (sourceSummary arguments) node listSummary pairSummary)
       (select arguments node) perror)
    perror
  where
    sourceSummary args = pelimList (\source _ -> pdataConstantSummaryV1 $ pfromData source) perror args
    select args node = pelimList (\_ r1 -> pelimList (\whenConstr r2 -> pelimList (\whenMap r3 -> pelimList
      (\whenList r4 -> pelimList (\whenInteger r5 -> pelimList (\whenBytes end -> pif (pnull # end)
        (presultIsSelectedV1 result $ pmatch node $ \case
          PConstrSmallData {} -> pfromData whenConstr
          PConstrLargeData {} -> pfromData whenConstr
          PMapDataNode {} -> pfromData whenMap
          PListDataNode {} -> pfromData whenList
          PIntegerDataNode {} -> pfromData whenInteger
          PBytesDataNode {} -> pfromData whenBytes) perror) perror r5) perror r4) perror r3) perror r2) perror r1) perror args

pverifySemanticUnconstrV1 ::
  forall (s :: S). Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticUnconstrV1 source result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pif (pnull # pfromData (psemantic'pairNodes w))
    (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \listSummary ->
     pif (ptopNodeMatchesV1 (pdataConstantSummaryV1 source) node listSummary (pcon PDNothing))
       (pmatch node $ \case
          PConstrSmallData {pnode'constructor, pnode'fieldsCount, pnode'fieldsRoot} ->
            pif (pnull # pfromData (psemantic'scalarPreimages w))
              (finish (pfromData pnode'constructor) pnode'fieldsCount pnode'fieldsRoot listSummary) perror
          PConstrLargeData
            { pnode'constructorCborRoot
            , pnode'constructorCborLength
            , pnode'constructorMemory
            , pnode'fieldsCount
            , pnode'fieldsRoot
            } ->
              pexactOneData (pfromData $ psemantic'scalarPreimages w) $ \rawConstructor ->
                pif (0 #< plengthBS # rawConstructor
                  #&& plengthBS # rawConstructor #<= 4095
                  #&& pfromData pnode'constructorCborRoot #== pboundedBlobRootV1 # rawConstructor
                  #&& pfromData pnode'constructorCborLength #== plengthBS # rawConstructor)
                  (pmatch (pdeserialise # rawConstructor) $ \case
                    PNothing -> perror
                    PJust constructorData ->
                      pif (pserialiseData # constructorData #== rawConstructor #&& pdataIsInteger constructorData)
                        (plet (pasInt # constructorData) $ \constructor ->
                          pif (127 #< constructor
                            #&& pfromData pnode'constructorMemory #== 4 + pintegerMemorySizeV1 # constructor)
                            (finish constructor pnode'fieldsCount pnode'fieldsRoot listSummary) perror)
                        perror)
                  perror
          _ -> perror)
       perror)
    perror
  where
    finish constructor fieldsCount fieldsRoot listSummary =
      plet (pcon $ PDataSequenceSummaryV1 fieldsRoot fieldsCount
              (pdata $ pmatch listSummary $ \case PDNothing -> 0; PDJust s -> pmatch (pfromData s) $ \n -> pfromData (plistNode'payloadCborLength n))
              (pdata $ pmatch listSummary $ \case PDNothing -> 0; PDJust s -> pmatch (pfromData s) $ \n -> pfromData (plistNode'memory n))) $ \fields ->
      plet (pserialiseData # pforgetData (pdata constructor)) $ \constructorCbor ->
      plet (pintegerDataSummaryV1 # constructor # (pboundedBlobRootV1 # constructorCbor)) $ \constructorSummary ->
      plet (plistDataSummaryV1 # fields) $ \fieldsSummary ->
      plet (pprependDataListSummaryV1 # constructorSummary # (pprependDataListSummaryV1 # fieldsSummary # pemptyDataListSummaryV1)) $ \pairFields ->
        pmatch constructorSummary $ \constructorS -> pmatch fields $ \fieldsS ->
          presultIsSemanticConstantV1 result
            (pcon $ PPairConstant (pdata $ pcon PIntegerConstant) (pdata $ pcon $ PListConstant $ pdata $ pcon PDataConstant))
            (psmallConstrDataSummaryV1 # 0 # pairFields)
            (pfromData (psummary'memory constructorS) - 4 + pfromData (pseq'memory fieldsS))

pconstantMemoryFromNodeV1 :: forall (s :: S). Term s PConstantTypeV1 -> Term s PDataNodeV1 -> Term s PInteger
pconstantMemoryFromNodeV1 constantType node = pmatch constantType $ \case
  PIntegerConstant -> pdataNodeMemoryV1 # node - 4
  PByteStringConstant -> pdataNodeMemoryV1 # node - 4
  PStringConstant -> pdataNodeMemoryV1 # node - 4
  PUnitConstant -> 1
  PBooleanConstant -> 1
  PDataConstant -> pdataNodeMemoryV1 # node
  PListConstant element -> pmatch node $ \case
    PListDataNode {pnode'itemsCount, pnode'memory} ->
      pif (pfromData element #== pcon PDataConstant) (pfromData pnode'memory - 4) $
      pif (pfromData element #== pcon PIntegerConstant) (pfromData pnode'memory - 4 - pfromData pnode'itemsCount * 4) $
      pif (pfromData element #== pcon (PPairConstant (pdata $ pcon PDataConstant) (pdata $ pcon PDataConstant)))
        (pfromData pnode'memory - 4 - pfromData pnode'itemsCount * 4) perror
    _ -> perror
  PPairConstant first second ->
    pif (pfromData first #== pcon PDataConstant #&& pfromData second #== pcon PDataConstant)
      (pdataNodeMemoryV1 # node - 4) $
    pif (pfromData first #== pcon PIntegerConstant
          #&& pfromData second #== pcon (PListConstant $ pdata $ pcon PDataConstant))
      (pdataNodeMemoryV1 # node - 12) perror
  PBlsG1Constant -> 48
  PBlsG2Constant -> 96
  PBlsMillerLoopResultConstant -> 192

pverifySemanticPairV1 ::
  forall (s :: S).
  Term s PInteger -> Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticPairV1 tag source result witness = pmatch witness $ \w ->
  pexactThreeData (pfromData $ psemantic'dataNodes w) $ \payloadNode firstNode secondNode ->
  pexactTwoData (pfromData $ psemantic'listNodes w) $ \firstLink secondLink ->
  pif (pnull # pfromData (psemantic'pairNodes w) #&& pnull # pfromData (psemantic'scalarPreimages w))
    (pmatch (psemanticPartsV1 source) $ \(PSemanticPartsV1 constantType payload memory) ->
     pmatch constantType $ \case
      PPairConstant firstType secondType ->
        pif (ptopNodeMatchesV1 payload payloadNode (pcon $ PDJust $ pdata firstLink) (pcon PDNothing))
          (pmatch payloadNode $ \case
            PConstrSmallData {pnode'constructor, pnode'fieldsCount} ->
              pif (pfromData pnode'constructor #== 0 #&& pfromData pnode'fieldsCount #== 2
                #&& pverifyDataListLinkV1 # firstLink # firstNode # (pcon $ PDJust $ pdata secondLink)
                #&& pverifyDataListLinkV1 # secondLink # secondNode # (pcon PDNothing))
                (plet (psummaryFromNodeV1 firstNode) $ \firstSummary ->
                 plet (psummaryFromNodeV1 secondNode) $ \secondSummary ->
                 plet
                  (pmatch source $ \case
                    PConstantValue direct ->
                      pmatch (pasConstr # (pconstantPayloadV1 # pfromData direct)) $ \(PBuiltinPair pairTag fields) ->
                        pif (pairTag #== 0)
                          (pelimList (\firstPayload rest -> pelimList (\secondPayload end ->
                            pif (pnull # end
                              #&& psemanticDataSummaryV1 # firstPayload #== firstSummary
                              #&& psemanticDataSummaryV1 # secondPayload #== secondSummary)
                              (pcon $ PPair
                                (pconstantPayloadMemorySizeV1 # pfromData firstType # firstPayload)
                                (pconstantPayloadMemorySizeV1 # pfromData secondType # secondPayload))
                              perror) perror rest) perror fields)
                          perror
                    _ -> pcon $ PPair
                      (pconstantMemoryFromNodeV1 (pfromData firstType) firstNode)
                      (pconstantMemoryFromNodeV1 (pfromData secondType) secondNode))
                  $ \componentMemory -> pmatch componentMemory $ \(PPair firstMemory secondMemory) ->
                  pif (memory #== firstMemory + secondMemory)
                    (pif (tag #== 29)
                      (presultIsSemanticConstantV1 result (pfromData firstType) firstSummary firstMemory)
                      (pif (tag #== 30) (presultIsSemanticConstantV1 result (pfromData secondType) secondSummary secondMemory) perror))
                    perror)
                perror
            _ -> perror)
          perror
      _ -> perror)
    perror

psemanticListMemoryMatchesV1 ::
  forall (s :: S). Term s PValueWitnessV1 -> Term s PConstantTypeV1 -> Term s PDataNodeV1 -> Term s PInteger -> Term s PBool
psemanticListMemoryMatchesV1 source constantType node memory = pmatch source $ \case
  PConstantValue _ -> pconstant True
  _ -> memory #== pconstantMemoryFromNodeV1 constantType node

pverifySemanticListSimpleV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticListSimpleV1 tag arguments result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pif (pnull # pfromData (psemantic'pairNodes w) #&& pnull # pfromData (psemantic'scalarPreimages w))
    (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \first ->
      let finish source branches =
            pmatch (psemanticPartsV1 source) $ \(PSemanticPartsV1 constantType payload memory) ->
              pmatch constantType $ \case
                PListConstant element ->
                  pif (ptopNodeMatchesV1 payload node first (pcon PDNothing)
                    #&& psemanticListMemoryMatchesV1 source constantType node memory)
                    (plet (psequenceFromListNodeV1 node) $ \sequence -> pmatch sequence $ \seqParts ->
                      pif (tag #== 31)
                        (pexactTwoData branches $ \whenEmpty whenNonempty ->
                          presultIsSelectedV1 result $
                            pif (pfromData (pseq'length seqParts) #== 0) whenEmpty whenNonempty)
                        (pif (tag #== 35 #&& pnull # branches)
                          (presultIsSemanticConstantV1 result (pcon PBooleanConstant)
                            (psemanticDataSummaryV1 # pboolData (pfromData (pseq'length seqParts) #== 0)) 1)
                          perror))
                    perror
                _ -> perror
       in pelimList (\source branches -> finish (pfromData source) branches) perror arguments)
    perror

pverifySemanticListConsV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticListConsV1 arguments result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pif (pnull # pfromData (psemantic'pairNodes w) #&& pnull # pfromData (psemantic'scalarPreimages w))
    (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \first ->
      pwithTwoValues arguments $ \item source ->
        pmatch (psemanticPartsV1 source) $ \(PSemanticPartsV1 constantType payload memory) ->
        pmatch constantType $ \case
          PListConstant element ->
            pif (ptopNodeMatchesV1 payload node first (pcon PDNothing)
              #&& psemanticListMemoryMatchesV1 source constantType node memory)
              (pmatch (psemanticPartsV1 item) $ \(PSemanticPartsV1 itemType itemPayload itemMemory) ->
                pif (itemType #== pfromData element)
                  (plet (psequenceFromListNodeV1 node) $ \sequence ->
                    plet (pprependDataListSummaryV1 # itemPayload # sequence) $ \newSequence ->
                      presultIsSemanticConstantV1 result constantType (plistDataSummaryV1 # newSequence) (itemMemory + memory))
                  perror)
              perror
          _ -> perror)
    perror

pverifySemanticListHeadV1 ::
  forall (s :: S).
  Term s PInteger -> Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticListHeadV1 tag source result witness = pmatch witness $ \w ->
  pexactTwoData (pfromData $ psemantic'dataNodes w) $ \sourceNode headNode ->
  pelimList
    (\firstLink remainingLinks ->
      pif (pnull # pfromData (psemantic'pairNodes w) #&& pnull # pfromData (psemantic'scalarPreimages w))
        (pmatch (psemanticPartsV1 source) $ \(PSemanticPartsV1 constantType payload memory) ->
         pmatch constantType $ \case
          PListConstant element ->
            plet
              (pmatch (pfromData firstLink) $ \link -> pif (pfromData (plistNode'length link) #== 1)
                (pif (pnull # remainingLinks) (pcon PDNothing) perror)
                (pexactOneData remainingLinks $ \tailLink -> pcon $ PDJust $ pdata tailLink))
              $ \tailLink ->
              pif (ptopNodeMatchesV1 payload sourceNode (pcon $ PDJust firstLink) (pcon PDNothing))
                (pmatch sourceNode $ \case
                  PListDataNode {pnode'itemsCount, pnode'memory} ->
                    pmatch (pfromData firstLink) $ \link ->
                    pif (psemanticListMemoryMatchesV1 source constantType sourceNode memory
                      #&& pfromData (plistNode'length link) #== pfromData pnode'itemsCount
                      #&& pverifyDataListLinkV1 # pfromData firstLink # headNode # tailLink)
                      (plet (psummaryFromNodeV1 headNode) $ \headSummary ->
                       plet
                        (pmatch source $ \case
                          PConstantValue direct ->
                            pelimList
                              (\head _ -> pif (psemanticDataSummaryV1 # head #== headSummary)
                                (pconstantPayloadMemorySizeV1 # pfromData element # head) perror)
                              perror
                              (pasList # (pconstantPayloadV1 # pfromData direct))
                          _ -> pconstantMemoryFromNodeV1 (pfromData element) headNode)
                        $ \headMemory ->
                        pif (headMemory #<= memory)
                          (pif (tag #== 33)
                            (presultIsSemanticConstantV1 result (pfromData element) headSummary headMemory)
                            (pif (tag #== 34)
                              (plet
                                ( pcon $ PDataSequenceSummaryV1
                                    (plistNode'tail link)
                                    (pdata $ pfromData (plistNode'length link) - 1)
                                    (pdata $ pfromData (plistNode'payloadCborLength link) - pfromData (plistNode'headCborLength link))
                                    (pdata $ pfromData (plistNode'memory link) - pfromData (plistNode'headMemory link))
                                )
                                $ \tailSequence -> presultIsSemanticConstantV1 result constantType
                                  (plistDataSummaryV1 # tailSequence) (memory - headMemory))
                              perror))
                          perror)
                      perror
                  _ -> perror)
                perror
          _ -> perror)
        perror)
    perror
    (pfromData $ psemantic'listNodes w)

psummaryMemoryV1 :: forall (s :: S). Term s PDataSummaryV1 -> Term s PInteger
psummaryMemoryV1 summary = pmatch summary $ \s -> pfromData (psummary'memory s)

pverifySemanticUnlistV1 ::
  forall (s :: S). Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticUnlistV1 source result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pif (pnull # pfromData (psemantic'pairNodes w) #&& pnull # pfromData (psemantic'scalarPreimages w))
    (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \first ->
      plet (pdataConstantSummaryV1 source) $ \summary ->
      pif (ptopNodeMatchesV1 summary node first (pcon PDNothing))
        (plet (psequenceFromListNodeV1 node) $ \sequence -> pmatch sequence $ \s ->
          presultIsSemanticConstantV1 result
            (pcon $ PListConstant $ pdata $ pcon PDataConstant)
            summary
            (pfromData $ pseq'memory s))
        perror)
    perror

pverifySemanticScalarV1 ::
  forall (s :: S).
  Term s PInteger -> Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticScalarV1 tag source result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pexactOneData (pfromData $ psemantic'scalarPreimages w) $ \raw ->
  pif (pnull # pfromData (psemantic'listNodes w) #&& pnull # pfromData (psemantic'pairNodes w))
    (pmatch (psemanticPartsV1 source) $ \(PSemanticPartsV1 sourceType summary sourceMemory) ->
      pif (tag #== 40 #|| tag #== 45)
        (plet (pcanonicalIntegerLeafV1 summary node raw) $ \integer ->
          pif (plengthBS # (pserialiseData # pforgetData (pdata integer)) #== plengthBS # raw)
            (pif (tag #== 40)
              (pif (sourceType #== pcon PIntegerConstant #&& sourceMemory #== psummaryMemoryV1 summary - 4)
                (presultIsSemanticConstantV1 result (pcon PDataConstant) summary (psummaryMemoryV1 summary)) perror)
              (pif (sourceType #== pcon PDataConstant #&& sourceMemory #== psummaryMemoryV1 summary)
                (presultIsSemanticConstantV1 result (pcon PIntegerConstant) summary (psummaryMemoryV1 summary - 4)) perror))
            perror)
        (pif (tag #== 41 #|| tag #== 46)
          (plet (pcanonicalBytesLeafV1 summary node raw) $ \revealed ->
            pif (plengthBS # revealed #== plengthBS # raw)
              (pif (tag #== 41)
                (pif (sourceType #== pcon PByteStringConstant #&& sourceMemory #== psummaryMemoryV1 summary - 4)
                  (presultIsSemanticConstantV1 result (pcon PDataConstant) summary (psummaryMemoryV1 summary)) perror)
                (pif (sourceType #== pcon PDataConstant #&& sourceMemory #== psummaryMemoryV1 summary)
                  (presultIsSemanticConstantV1 result (pcon PByteStringConstant) summary (psummaryMemoryV1 summary - 4)) perror))
              perror)
          perror))
    perror

pverifySemanticMkListV1 ::
  forall (s :: S). Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticMkListV1 items result witness = pmatch witness $ \w ->
  pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
  pif (pnull # pfromData (psemantic'pairNodes w) #&& pnull # pfromData (psemantic'scalarPreimages w))
    (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \first ->
      plet (plistConstantSequenceV1 items (pcon PDataConstant) node first) $ \sequence ->
        pmatch sequence $ \s -> presultIsSemanticConstantV1 result (pcon PDataConstant)
          (plistDataSummaryV1 # sequence) (4 + pfromData (pseq'memory s)))
    perror

pverifySemanticMkConstrV1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticMkConstrV1 arguments result witness = pmatch witness $ \w ->
  pif (pnull # pfromData (psemantic'pairNodes w))
    (pwithTwoValues arguments $ \index fieldsValue ->
      let finish constructor fieldsNode =
            pif (0 #<= constructor)
              (plet (plistCandidateForNodeV1 fieldsNode $ pfromData $ psemantic'listNodes w) $ \first ->
                plet (plistConstantSequenceV1 fieldsValue (pcon PDataConstant) fieldsNode first) $ \fields ->
                plet
                  (pif (constructor #<= 127)
                    (psmallConstrDataSummaryV1 # constructor # fields)
                    (plet (pserialiseData # pforgetData (pdata constructor)) $ \rawConstructor ->
                      plargeConstrDataSummaryV1 # constructor # (pboundedBlobRootV1 # rawConstructor) # fields))
                  $ \summary -> presultIsSemanticConstantV1 result (pcon PDataConstant) summary (psummaryMemoryV1 summary))
              perror
       in pmatch index $ \case
            PConstantValue _ ->
              pexactOneData (pfromData $ psemantic'dataNodes w) $ \fieldsNode ->
                pif (pnull # pfromData (psemantic'scalarPreimages w))
                  (finish (pintegerV1 index) fieldsNode) perror
            PSemanticConstantValue {} ->
              pexactTwoData (pfromData $ psemantic'dataNodes w) $ \indexNode fieldsNode ->
              pexactOneData (pfromData $ psemantic'scalarPreimages w) $ \rawIndex ->
                pmatch (psemanticPartsV1 index) $ \(PSemanticPartsV1 indexType indexSummary indexMemory) ->
                  pif (indexType #== pcon PIntegerConstant #&& indexMemory #== psummaryMemoryV1 indexSummary - 4)
                    (finish (pcanonicalIntegerLeafV1 indexSummary indexNode rawIndex) fieldsNode) perror
            _ -> perror)
    perror

pverifySemanticSummaryOnlyDataV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticSummaryOnlyDataV1 tag arguments result witness = pmatch witness $ \w ->
  pif (pemptySemanticMaterialV1 w)
    (pif (tag #== 47)
      (pwithTwoValues arguments $ \left right ->
        presultIsSemanticConstantV1 result (pcon PBooleanConstant)
          (psemanticDataSummaryV1 # pboolData (pdataConstantSummaryV1 left #== pdataConstantSummaryV1 right)) 1)
    $ pif (tag #== 48)
      (pwithTwoValues arguments $ \first second ->
        plet (pdataConstantSummaryV1 first) $ \firstSummary ->
        plet (pdataConstantSummaryV1 second) $ \secondSummary ->
        plet (pprependDataListSummaryV1 # firstSummary
          # (pprependDataListSummaryV1 # secondSummary # pemptyDataListSummaryV1)) $ \fields ->
          presultIsSemanticConstantV1 result
            (pcon $ PPairConstant (pdata $ pcon PDataConstant) (pdata $ pcon PDataConstant))
            (psmallConstrDataSummaryV1 # 0 # fields)
            (psummaryMemoryV1 firstSummary + psummaryMemoryV1 secondSummary))
    $ pif (tag #== 49 #|| tag #== 50)
      (pwithOneValue arguments $ \unit ->
        pif (punitV1 unit)
          (presultIsSemanticConstantV1 result
            (pif (tag #== 49)
              (pcon $ PListConstant $ pdata $ pcon PDataConstant)
              (pcon $ PListConstant $ pdata $ pcon $ PPairConstant
                (pdata $ pcon PDataConstant) (pdata $ pcon PDataConstant)))
            (plistDataSummaryV1 # pemptyDataListSummaryV1) 0)
          perror)
      (pconstant False))
    perror

pverifySemanticSerialiseDataV1 ::
  forall (s :: S). Term s PValueWitnessV1 -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticSerialiseDataV1 source result witness = pmatch witness $ \w ->
  pif (pnull # pfromData (psemantic'dataNodes w)
    #&& pnull # pfromData (psemantic'listNodes w)
    #&& pnull # pfromData (psemantic'pairNodes w))
    (pexactOneData (pfromData $ psemantic'scalarPreimages w) $ \rawData ->
      pif (0 #< plengthBS # rawData #&& plengthBS # rawData #<= 4095)
        (pmatch (pdeserialise # rawData) $ \case
          PNothing -> perror
          PJust dataValue ->
            pif (pserialiseData # dataValue #== rawData
              #&& psemanticDataSummaryV1 # dataValue #== pdataConstantSummaryV1 source)
              (presultIsSemanticConstantV1 result (pcon PByteStringConstant)
                (psemanticDataSummaryV1 # pforgetData (pdata rawData))
                (pbytearrayMemorySizeV1 # rawData))
              perror)
        perror)
    perror

pverifySemanticDataV1 ::
  forall (s :: S).
  Term s PInteger -> Term s (PBuiltinList (PAsData PValueWitnessV1)) -> Term s PValueWitnessV1 -> Term s PSemanticBuiltinWitnessV1 -> Term s PBool
pverifySemanticDataV1 tag arguments result witness =
  pif (tag #== 37) (pverifySemanticMkConstrV1 arguments result witness) $
  pif (tag #== 39) (pwithOneValue arguments $ \items -> pverifySemanticMkListV1 items result witness) $
  pif (tag #== 40 #|| tag #== 41 #|| tag #== 45 #|| tag #== 46)
    (pwithOneValue arguments $ \source -> pverifySemanticScalarV1 tag source result witness) $
  pif (tag #== 42) (pwithOneValue arguments $ \source -> pverifySemanticUnconstrV1 source result witness) $
  pif (tag #== 44) (pwithOneValue arguments $ \source -> pverifySemanticUnlistV1 source result witness) $
  pif (47 #<= tag #&& tag #<= 50) (pverifySemanticSummaryOnlyDataV1 tag arguments result witness) $
  pif (tag #== 51) (pwithOneValue arguments $ \source -> pverifySemanticSerialiseDataV1 source result witness)
    (pconstant False)

pverifySemanticBuiltinV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PValueWitnessV1) :--> PValueWitnessV1 :--> PSemanticBuiltinWitnessV1 :--> PBool)
pverifySemanticBuiltinV1 = phoistAcyclic $ plam $ \tag builtinRoot arguments result witness ->
  pmatch (pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  pif (builtinRoot #== phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot)
    (pif (tag #== 29 #|| tag #== 30)
      (pwithOneValue arguments $ \source -> pverifySemanticPairV1 tag source result witness) $
     pif (tag #== 31 #|| tag #== 35) (pverifySemanticListSimpleV1 tag arguments result witness) $
     pif (tag #== 32) (pverifySemanticListConsV1 arguments result witness) $
     pif (tag #== 33 #|| tag #== 34)
      (pwithOneValue arguments $ \source -> pverifySemanticListHeadV1 tag source result witness) $
     pif (tag #== 36) (pverifySemanticChooseDataV1 arguments result witness) $
     pif (37 #<= tag #&& tag #<= 51) (pverifySemanticDataV1 tag arguments result witness)
       (pconstant False))
    perror

pverifySemanticBuiltinFailureV1 ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PValueWitnessV1) :--> PSemanticBuiltinWitnessV1 :--> PBool)
pverifySemanticBuiltinFailureV1 = phoistAcyclic $ plam $ \tag builtinRoot arguments witness ->
  pmatch (pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  pif (builtinRoot #== phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot)
    (pwithOneValue arguments $ \source -> pmatch witness $ \w ->
      pexactOneData (pfromData $ psemantic'dataNodes w) $ \node ->
      pif (pnull # pfromData (psemantic'scalarPreimages w))
        (plet (plistCandidateForNodeV1 node $ pfromData $ psemantic'listNodes w) $ \listSummary ->
         plet (ppairCandidateForNodeV1 node $ pfromData $ psemantic'pairNodes w) $ \pairSummary ->
         pif (tag #== 33 #|| tag #== 34)
           (pif (pnull # pfromData (psemantic'pairNodes w))
             (pmatch (psemanticPartsV1 source) $ \(PSemanticPartsV1 constantType payload memory) ->
               pmatch constantType $ \case
                 PListConstant _ ->
                   pif (ptopNodeMatchesV1 payload node listSummary (pcon PDNothing)
                     #&& psemanticListMemoryMatchesV1 source constantType node memory)
                     (pmatch (psequenceFromListNodeV1 node) $ \sequence -> pfromData (pseq'length sequence) #== 0)
                     perror
                 _ -> perror)
             perror)
           (pif (ptopNodeMatchesV1 (pdataConstantSummaryV1 source) node listSummary pairSummary)
             (pif (42 #<= tag #&& tag #<= 46)
               (pmatch node $ \case
                 PConstrSmallData {} -> tag #/= 42
                 PConstrLargeData {} -> tag #/= 42
                 PMapDataNode {} -> tag #/= 43
                 PListDataNode {} -> tag #/= 44
                 PIntegerDataNode {} -> tag #/= 45
                 PBytesDataNode {} -> tag #/= 46)
               (pconstant False))
             perror))
        perror)
    perror

presultRootV1 :: forall (s :: S). Term s (PValueWitnessV1 :--> PByteString)
presultRootV1 = pvalueRootV1

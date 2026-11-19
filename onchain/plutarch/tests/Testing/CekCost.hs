{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Testing.CekCost
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-cost-v1.ak@ — the pinned Plutus V3 builtin budget.

The reference here is not a reimplementation. It is __the Plutus cost model
itself__: @plutus-core@'s @cekCostModelForVariant DefaultFunSemanticsVariantC@,
evaluated through @plutus-core@'s own costing-function runners at the same
argument sizes the port is given. That is as independent as a reference gets —
the numbers, the model /shapes/, and the arithmetic all come from the library the
ledger runs, and none of them from the Aiken source or from the port.

Two pieces of glue make that possible and both are worth naming, because the
whole suite rests on them.

__The size wrapper.__ A costing function consumes @ExMemory@ measurements, not
values, so 'Sized' is an @ExMemoryUsage@ instance whose measurement /is/ the
number handed to it. That lets the reference be driven at exactly the sizes the
port is driven at, with no value of the right shape to construct and no rounding in
between.

__The tag table.__ Midgard's builtin tags are the UPLC /flat/ tags, which are not
@DefaultFun@'s declaration order: @serialiseData@ is 51 and the two secp256k1
verifications are 52 and 53, where the Haskell datatype puts them at 53, 22 and
23. The table below is written in flat order and is the one thing here that comes
from neither library — so it is checked twice over, by the arity it implies and
by the coefficients it selects.

=== One divergence, and it is not the port's

Every tag agrees with the Plutus model except the division family — tags 3 to 6,
@divideInteger@ and friends. Plutus's @quadratic_in_x_and_y@ reads
@c02@ as the coefficient of __y²__; the Aiken source multiplies it by __y__.
They agree for @y ∈ {0, 1}@, which is why the Aiken suite's own golden vectors
never caught it, and they diverge by @900·(y² − y)@ everywhere else above the
diagonal.

The port reproduces the Aiken source, because that is what a port is for, and the
tests below pin /both/ facts: the port matches the Aiken formula, and the Aiken
formula does not match Plutus. See @README.md@, "The pinned cost model".
-}
module Testing.CekCost (tests) where

import Data.Kind (Type)

import PlutusCore.Evaluation.Machine.BuiltinCostModel (
  BuiltinCostModel,
  ModelOneArgument,
  ModelSixArguments,
  ModelThreeArguments,
  ModelTwoArguments,
  paramAddInteger,
  paramAndByteString,
  paramAppendByteString,
  paramAppendString,
  paramBData,
  paramBlake2b_224,
  paramBlake2b_256,
  paramBls12_381_G1_add,
  paramBls12_381_G1_compress,
  paramBls12_381_G1_equal,
  paramBls12_381_G1_hashToGroup,
  paramBls12_381_G1_neg,
  paramBls12_381_G1_scalarMul,
  paramBls12_381_G1_uncompress,
  paramBls12_381_G2_add,
  paramBls12_381_G2_compress,
  paramBls12_381_G2_equal,
  paramBls12_381_G2_hashToGroup,
  paramBls12_381_G2_neg,
  paramBls12_381_G2_scalarMul,
  paramBls12_381_G2_uncompress,
  paramBls12_381_finalVerify,
  paramBls12_381_millerLoop,
  paramBls12_381_mulMlResult,
  paramByteStringToInteger,
  paramChooseData,
  paramChooseList,
  paramChooseUnit,
  paramComplementByteString,
  paramConsByteString,
  paramConstrData,
  paramCountSetBits,
  paramDecodeUtf8,
  paramDivideInteger,
  paramEncodeUtf8,
  paramEqualsByteString,
  paramEqualsData,
  paramEqualsInteger,
  paramEqualsString,
  paramFindFirstSetBit,
  paramFstPair,
  paramHeadList,
  paramIData,
  paramIfThenElse,
  paramIndexByteString,
  paramIntegerToByteString,
  paramKeccak_256,
  paramLengthOfByteString,
  paramLessThanByteString,
  paramLessThanEqualsByteString,
  paramLessThanEqualsInteger,
  paramLessThanInteger,
  paramListData,
  paramMapData,
  paramMkCons,
  paramMkNilData,
  paramMkNilPairData,
  paramMkPairData,
  paramModInteger,
  paramMultiplyInteger,
  paramNullList,
  paramOrByteString,
  paramQuotientInteger,
  paramReadBit,
  paramRemainderInteger,
  paramReplicateByte,
  paramRipemd_160,
  paramRotateByteString,
  paramSerialiseData,
  paramSha2_256,
  paramSha3_256,
  paramShiftByteString,
  paramSliceByteString,
  paramSndPair,
  paramSubtractInteger,
  paramTailList,
  paramTrace,
  paramUnBData,
  paramUnConstrData,
  paramUnIData,
  paramUnListData,
  paramUnMapData,
  paramVerifyEcdsaSecp256k1Signature,
  paramVerifyEd25519Signature,
  paramVerifySchnorrSecp256k1Signature,
  paramWriteBits,
  paramXorByteString,
  runCostingFunOneArgument,
  runCostingFunSixArguments,
  runCostingFunThreeArguments,
  runCostingFunTwoArguments,
 )
import PlutusCore.Default (BuiltinSemanticsVariant (..), DefaultFun)
import PlutusCore.Evaluation.Machine.CostingFun.Core (CostingFun)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExBudgetStream (sumExBudgetStream)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (cekCostModelForVariant)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusLedgerApi.Common (fromSatInt)
import PlutusCore.Evaluation.Machine.ExMemoryUsage (ExMemoryUsage (..), singletonRose)
import PlutusCore.Evaluation.Machine.MachineParameters (CostModel (..))

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CekCost (PBuiltinBudgetV1 (..), pbuiltinArgumentCountV1, pbuiltinBudgetV1)
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "CEK Cost Tests"
    [ testGroup "arity" arityTests
    , testGroup "against the Plutus cost model" modelTests
    , testGroup "where Aiken and Plutus disagree" divergenceTests
    , testGroup "failing closed" closedTests
    ]

--------------------------------------------------------------------------------
-- Arity
--------------------------------------------------------------------------------

{- | The arity table, checked against the Plutus builtins' own signatures.

The signatures are what the cost model is indexed by — a @ModelThreeArguments@
exists for a builtin precisely because that builtin takes three arguments — so
'referenceArity' below is read off the same table the budgets are, and a
disagreement here would mean the tag map is wrong rather than that a number is.
-}
arityTests :: [TestTree]
arityTests =
  [ testCase "every tag's arity is the builtin's own" $
      passertEval $
        pall'
          [ pbuiltinArgumentCountV1 # pconstant tag #== pconstant (referenceArity tag)
          | tag <- [0 .. 86]
          ]
  , testCase "a tag past the end of the table aborts" $
      pfails (pbuiltinArgumentCountV1 # pconstant @PInteger 87)
  , testCase "a negative tag aborts" $
      pfails (pbuiltinArgumentCountV1 # pconstant @PInteger (-1))
  ]

--------------------------------------------------------------------------------
-- The whole table against Plutus
--------------------------------------------------------------------------------

{- | One case per tag: the port's budget against the Plutus model's, at every
size vector the tag admits.

The division family is excluded and handled on its own below, because it is the
one place the two disagree.
-}
modelTests :: [TestTree]
modelTests =
  [ testCase ("tag " <> show tag <> " — " <> builtinName tag) $
      passertEval $
        pall'
          [ budgetIs tag sizes (referenceBudget tag sizes)
          | sizes <- sizeVectors (referenceArity tag)
          ]
  | tag <- [0 .. 86]
  , tag `notElem` divergentTags
  ]

{- | The three tags whose Aiken formula is not the Plutus model's.

Everything else — 84 of 87 — agrees with @plutus-core@ exactly, at every size
vector, in both dimensions.
-}
divergentTags :: [Integer]
divergentTags = [2, 3, 4, 5, 6, 21]

--------------------------------------------------------------------------------
-- Division
--------------------------------------------------------------------------------

{- | The three families where the Aiken source and the Plutus model part company.

Each is pinned twice: the port matches the Aiken formula (so the port is a
faithful port), and the Aiken formula differs from Plutus by a stated amount at
stated sizes (so the divergence is a recorded fact rather than a surprise).

None of these is a rounding difference. Each is a different function.
-}
divergenceTests :: [TestTree]
divergenceTests =
  [ testGroup "the division family — c02 multiplies y, not y²" divisionTests
  , testGroup "multiplyInteger — the sizes are added, not multiplied" multiplyTests
  , testGroup "verifyEd25519Signature — the signature is charged, not the message" ed25519Tests
  ]

{- | @divideInteger@, @quotientInteger@, @remainderInteger@ and @modInteger@.

Plutus's @quadratic_in_x_and_y@ reads @c02@ as the coefficient of __y²__; the
Aiken source multiplies it by __y__. They agree for @y ∈ {0, 1}@ — which is where
the Aiken suite's own golden vectors sit — and differ by @900·(y² − y)@ elsewhere
above the diagonal.
-}
divisionTests :: [TestTree]
divisionTests =
  [ testCase "the port reproduces the Aiken formula" $
      passertEval $
        pall'
          [ budgetIs tag sizes (aikenDivisionCpu sizes, referenceMemory tag sizes)
          | tag <- [3 .. 6]
          , sizes <- divisionVectors
          ]
  , testCase "…which agrees with Plutus wherever y ≤ 1" $
      passertEval $
        pall'
          [ budgetIs tag sizes (referenceBudget tag sizes)
          | tag <- [3 .. 6]
          , sizes@[_, y] <- divisionVectors
          , y <= 1
          ]
  , -- The measured divergence, as concrete pairs rather than as a formula, so a
    -- change to either side has to restate the numbers.
    testCase "…and over-charges by 900·(y² − y) above it" $ do
      assertEqual
        "the Plutus figures are the Plutus model's"
        [143168, 151382, 140069]
        [fst (referenceBudget 3 [3, 2]), fst (referenceBudget 3 [4, 3]), fst (referenceBudget 3 [2, 2])]
      passertEval $
        pall'
          [ budgetIs 3 [x, y] (portCpu, referenceMemory 3 [x, y])
          | ([x, y], plutusCpu, portCpu) <-
              [ ([3, 2], 143168, 144968)
              , ([4, 3], 151382, 156782)
              , ([2, 2], 140069, 141869)
              ]
          , portCpu - plutusCpu == 900 * (y * y - y)
          ]
  ]

divisionVectors :: [[Integer]]
divisionVectors = [[0, 0], [1, 1], [5, 1], [1, 5], [2, 2], [3, 2], [4, 3]]

{- | Aiken @division_cpu@, restated here from the Aiken source.

The deliberate second transcription: the port is checked against the formula as
written, so "the port matches Aiken" and "Aiken matches Plutus" stay separate
claims with separate verdicts.
-}
aikenDivisionCpu :: [Integer] -> Integer
aikenDivisionCpu [x, y]
  | x < y = 85848
  | otherwise =
      max 85848 (123203 + 1716 * x + 7305 * y + 57 * x * x + 549 * x * y - 900 * y)
aikenDivisionCpu _ = error "division takes two sizes"

{- | @multiplyInteger@.

Plutus charges @intercept + slope·(x·y)@ — the model is @multiplied_sizes@,
because multiplying an @x@-word integer by a @y@-word one is quadratic work. The
Aiken source uses its @linear2_add@ helper, which charges @slope·(x + y)@. Same
two coefficients, different function, and the gap grows with the operands: at
32×64 words Plutus charges 1,153,346 and the table charges 140,258.

They agree only where @x·y == x + y@, which over the sizes here means @(0,0)@ and
@(2,2)@.
-}
multiplyTests :: [TestTree]
multiplyTests =
  [ testCase "the port reproduces the Aiken formula" $
      passertEval $
        pall'
          [ budgetIs 2 [x, y] (90434 + 519 * (x + y), x + y)
          | [x, y] <- sizeVectors 2 <> [[2, 2]]
          ]
  , testCase "…which agrees with Plutus only where x·y = x + y" $
      passertEval $
        pall'
          [ budgetIs 2 sizes (referenceBudget 2 sizes)
          | sizes@[x, y] <- sizeVectors 2 <> [[2, 2]]
          , x * y == x + y
          ]
  , testCase "…and under-charges everywhere else" $ do
      assertEqual
        "the Plutus figures are the Plutus model's"
        [(90953, 2), (93029, 6), (1153346, 96)]
        [referenceBudget 2 [1, 1], referenceBudget 2 [5, 1], referenceBudget 2 [32, 64]]
      passertEval $
        pall'
          [ budgetIs 2 [x, y] (90434 + 519 * (x + y), x + y)
          | [x, y] <- [[1, 1], [5, 1], [32, 64]]
          , 519 * (x + y) < 519 * (x * y)
          ]
  ]

{- | @verifyEd25519Signature(pubKey, message, signature)@.

Plutus's model is @linear_in_y@: the cost tracks the __message__, which is the
only argument whose length varies. The Aiken source uses @linear3_z@, charging
the __signature__ — a fixed 64 bytes, so in practice a constant. A long message
is charged as though it were 64 bytes.

The two agree exactly when the second and third sizes are equal, which is why a
uniform fixture never separates them.
-}
ed25519Tests :: [TestTree]
ed25519Tests =
  [ testCase "the port reproduces the Aiken formula" $
      passertEval $
        pall'
          [ budgetIs 21 [x, y, z] (53384111 + 14333 * z, 10)
          | [x, y, z] <- sizeVectors 3
          ]
  , testCase "…which agrees with Plutus only where the message and signature are equal" $
      passertEval $
        pall'
          [ budgetIs 21 sizes (referenceBudget 21 sizes)
          | sizes@[_, y, z] <- sizeVectors 3
          , y == z
          ]
  , testCase "…and charges the wrong argument otherwise" $ do
      assertEqual
        "the Plutus figures are the Plutus model's"
        [(53412777, 10), (53412777, 10)]
        [referenceBudget 21 [1, 2, 3], referenceBudget 21 [3, 2, 1]]
      passertEval $
        pall'
          [ budgetIs 21 [x, y, z] (53384111 + 14333 * z, 10)
          | [x, y, z] <- [[1, 2, 3], [3, 2, 1]]
          ]
  ]

--------------------------------------------------------------------------------
-- Failing closed
--------------------------------------------------------------------------------

closedTests :: [TestTree]
closedTests =
  [ testCase "a tag past the end of the table aborts" $
      pfails (pbuiltinBudgetV1 # pconstant @PInteger 87 # sizesT [1])
  , testCase "a negative tag aborts" $
      pfails (pbuiltinBudgetV1 # pconstant @PInteger (-1) # sizesT [1])
  , testCase "too few sizes for the tag's arity aborts" $
      pfails (pbuiltinBudgetV1 # pconstant @PInteger 0 # sizesT [1])
  , testCase "too many sizes for the tag's arity aborts" $
      pfails (pbuiltinBudgetV1 # pconstant @PInteger 0 # sizesT [1, 2, 3])
  , testCase "a negative size aborts" $
      pfails (pbuiltinBudgetV1 # pconstant @PInteger 0 # sizesT [1, -1])
  , testCase "an empty size vector for a one-argument builtin aborts" $
      pfails (pbuiltinBudgetV1 # pconstant @PInteger 18 # sizesT [])
  ]

--------------------------------------------------------------------------------
-- Driving the port
--------------------------------------------------------------------------------

budgetIs :: forall (s :: S). Integer -> [Integer] -> (Integer, Integer) -> Term s PBool
budgetIs tag sizes (cpu, memory) =
  pmatch (pbuiltinBudgetV1 # pconstant tag # sizesT sizes) $
    \PBuiltinBudgetV1 {pbudget'cpu, pbudget'memory} ->
      pfromData pbudget'cpu
        #== pconstant cpu
        #&& (pfromData pbudget'memory #== pconstant memory)

sizesT :: forall (s :: S). [Integer] -> Term s (PBuiltinList (PAsData PInteger))
sizesT = foldr (\n acc -> pcons # pdata (pconstant n) # acc) pnil

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

--------------------------------------------------------------------------------
-- The reference: the Plutus cost model itself
--------------------------------------------------------------------------------

{- | A stand-in whose @ExMemory@ measurement is the number it carries.

Costing functions consume measurements rather than values, so this is what lets
the reference be driven at exactly the sizes under test.
-}
newtype Sized = Sized Integer

instance ExMemoryUsage Sized where
  memoryUsage (Sized n) = singletonRose (fromIntegral n)

-- | The pinned model. Variant C is the one whose coefficients Midgard transcribed.
plutusModel :: BuiltinCostModel
plutusModel = _builtinCostModel (cekCostModelForVariant variantC)
  where
    variantC :: BuiltinSemanticsVariant DefaultFun
    variantC = DefaultFunSemanticsVariantC

referenceBudget :: Integer -> [Integer] -> (Integer, Integer)
referenceBudget tag sizes = (cpu, memory)
  where
    ExBudget (ExCPU cpu') (ExMemory memory') = referenceExBudget tag sizes
    cpu = fromSatInt cpu'
    memory = fromSatInt memory'

referenceMemory :: Integer -> [Integer] -> Integer
referenceMemory tag sizes = snd (referenceBudget tag sizes)

referenceExBudget :: Integer -> [Integer] -> ExBudget
referenceExBudget tag sizes = case (referenceArity tag, sizes) of
  (1, [x]) -> sumExBudgetStream (runCostingFunOneArgument (oneArgument tag) (Sized x))
  (2, [x, y]) ->
    sumExBudgetStream (runCostingFunTwoArguments (twoArguments tag) (Sized x) (Sized y))
  (3, [x, y, z]) ->
    sumExBudgetStream
      (runCostingFunThreeArguments (threeArguments tag) (Sized x) (Sized y) (Sized z))
  (6, [a, b, c, d, e, f]) ->
    sumExBudgetStream
      ( runCostingFunSixArguments
          (sixArguments tag)
          (Sized a)
          (Sized b)
          (Sized c)
          (Sized d)
          (Sized e)
          (Sized f)
      )
  _ -> error ("no reference for tag " <> show tag <> " at " <> show sizes)

-- | Size vectors for an arity: the boundaries plus one asymmetric pair each way.
sizeVectors :: Integer -> [[Integer]]
sizeVectors 1 = [[0], [1], [32], [1000]]
sizeVectors 2 = [[0, 0], [1, 1], [5, 1], [1, 5], [32, 64]]
sizeVectors 3 = [[0, 0, 0], [1, 2, 3], [3, 2, 1], [8, 8, 8]]
sizeVectors 6 = [[1, 1, 1, 1, 1, 1], [2, 3, 4, 5, 6, 7]]
sizeVectors n = error ("no size vectors for arity " <> show n)

--------------------------------------------------------------------------------
-- The flat tag table
--------------------------------------------------------------------------------

{- | Midgard's builtin tag → the Plutus builtin's costing function.

Written in UPLC /flat/ tag order, which is what the machine encodes and what
Midgard's tables are keyed by. It differs from @DefaultFun@'s declaration order
in three places and all three are load-bearing: @serialiseData@ is 51, and the
two secp256k1 verifications are 52 and 53.
-}
oneArgument :: Integer -> CostingFun ModelOneArgument
oneArgument = \case
  13 -> paramLengthOfByteString plutusModel
  18 -> paramSha2_256 plutusModel
  19 -> paramSha3_256 plutusModel
  20 -> paramBlake2b_256 plutusModel
  24 -> paramEncodeUtf8 plutusModel
  25 -> paramDecodeUtf8 plutusModel
  29 -> paramFstPair plutusModel
  30 -> paramSndPair plutusModel
  33 -> paramHeadList plutusModel
  34 -> paramTailList plutusModel
  35 -> paramNullList plutusModel
  38 -> paramMapData plutusModel
  39 -> paramListData plutusModel
  40 -> paramIData plutusModel
  41 -> paramBData plutusModel
  42 -> paramUnConstrData plutusModel
  43 -> paramUnMapData plutusModel
  44 -> paramUnListData plutusModel
  45 -> paramUnIData plutusModel
  46 -> paramUnBData plutusModel
  49 -> paramMkNilData plutusModel
  50 -> paramMkNilPairData plutusModel
  51 -> paramSerialiseData plutusModel
  55 -> paramBls12_381_G1_neg plutusModel
  59 -> paramBls12_381_G1_compress plutusModel
  60 -> paramBls12_381_G1_uncompress plutusModel
  62 -> paramBls12_381_G2_neg plutusModel
  66 -> paramBls12_381_G2_compress plutusModel
  67 -> paramBls12_381_G2_uncompress plutusModel
  71 -> paramKeccak_256 plutusModel
  72 -> paramBlake2b_224 plutusModel
  78 -> paramComplementByteString plutusModel
  84 -> paramCountSetBits plutusModel
  85 -> paramFindFirstSetBit plutusModel
  86 -> paramRipemd_160 plutusModel
  tag -> error ("tag " <> show tag <> " is not a one-argument builtin")

twoArguments :: Integer -> CostingFun ModelTwoArguments
twoArguments = \case
  0 -> paramAddInteger plutusModel
  1 -> paramSubtractInteger plutusModel
  2 -> paramMultiplyInteger plutusModel
  3 -> paramDivideInteger plutusModel
  4 -> paramQuotientInteger plutusModel
  5 -> paramRemainderInteger plutusModel
  6 -> paramModInteger plutusModel
  7 -> paramEqualsInteger plutusModel
  8 -> paramLessThanInteger plutusModel
  9 -> paramLessThanEqualsInteger plutusModel
  10 -> paramAppendByteString plutusModel
  11 -> paramConsByteString plutusModel
  14 -> paramIndexByteString plutusModel
  15 -> paramEqualsByteString plutusModel
  16 -> paramLessThanByteString plutusModel
  17 -> paramLessThanEqualsByteString plutusModel
  22 -> paramAppendString plutusModel
  23 -> paramEqualsString plutusModel
  27 -> paramChooseUnit plutusModel
  28 -> paramTrace plutusModel
  32 -> paramMkCons plutusModel
  37 -> paramConstrData plutusModel
  47 -> paramEqualsData plutusModel
  48 -> paramMkPairData plutusModel
  54 -> paramBls12_381_G1_add plutusModel
  56 -> paramBls12_381_G1_scalarMul plutusModel
  57 -> paramBls12_381_G1_equal plutusModel
  58 -> paramBls12_381_G1_hashToGroup plutusModel
  61 -> paramBls12_381_G2_add plutusModel
  63 -> paramBls12_381_G2_scalarMul plutusModel
  64 -> paramBls12_381_G2_equal plutusModel
  65 -> paramBls12_381_G2_hashToGroup plutusModel
  68 -> paramBls12_381_millerLoop plutusModel
  69 -> paramBls12_381_mulMlResult plutusModel
  70 -> paramBls12_381_finalVerify plutusModel
  74 -> paramByteStringToInteger plutusModel
  79 -> paramReadBit plutusModel
  81 -> paramReplicateByte plutusModel
  82 -> paramShiftByteString plutusModel
  83 -> paramRotateByteString plutusModel
  tag -> error ("tag " <> show tag <> " is not a two-argument builtin")

threeArguments :: Integer -> CostingFun ModelThreeArguments
threeArguments = \case
  12 -> paramSliceByteString plutusModel
  21 -> paramVerifyEd25519Signature plutusModel
  26 -> paramIfThenElse plutusModel
  31 -> paramChooseList plutusModel
  52 -> paramVerifyEcdsaSecp256k1Signature plutusModel
  53 -> paramVerifySchnorrSecp256k1Signature plutusModel
  73 -> paramIntegerToByteString plutusModel
  75 -> paramAndByteString plutusModel
  76 -> paramOrByteString plutusModel
  77 -> paramXorByteString plutusModel
  80 -> paramWriteBits plutusModel
  tag -> error ("tag " <> show tag <> " is not a three-argument builtin")

sixArguments :: Integer -> CostingFun ModelSixArguments
sixArguments = \case
  36 -> paramChooseData plutusModel
  tag -> error ("tag " <> show tag <> " is not a six-argument builtin")

-- | The builtin's arity, read off which model the Plutus table holds for it.
referenceArity :: Integer -> Integer
referenceArity tag
  | tag == 36 = 6
  | tag `elem` [12, 21, 26, 31, 52, 53, 73, 75, 76, 77, 80] = 3
  | tag <= 11 || tag `elem` twoArgumentTags = 2
  | otherwise = 1

twoArgumentTags :: [Integer]
twoArgumentTags =
  [ 14
  , 15
  , 16
  , 17
  , 22
  , 23
  , 27
  , 28
  , 32
  , 37
  , 47
  , 48
  , 54
  , 56
  , 57
  , 58
  , 61
  , 63
  , 64
  , 65
  , 68
  , 69
  , 70
  , 74
  , 79
  , 81
  , 82
  , 83
  ]

-- | The builtin's name, for the test-case label only.
builtinName :: Integer -> String
builtinName tag = case referenceArity tag of
  1 -> nameOf (oneArgument tag)
  2 -> nameOf (twoArguments tag)
  3 -> nameOf (threeArguments tag)
  _ -> "chooseData"
  where
    nameOf :: forall (a :: Type). a -> String
    nameOf _ = flatNames !! fromIntegral tag

-- | The UPLC flat builtin tag table, in order.
flatNames :: [String]
flatNames =
  [ "addInteger"
  , "subtractInteger"
  , "multiplyInteger"
  , "divideInteger"
  , "quotientInteger"
  , "remainderInteger"
  , "modInteger"
  , "equalsInteger"
  , "lessThanInteger"
  , "lessThanEqualsInteger"
  , "appendByteString"
  , "consByteString"
  , "sliceByteString"
  , "lengthOfByteString"
  , "indexByteString"
  , "equalsByteString"
  , "lessThanByteString"
  , "lessThanEqualsByteString"
  , "sha2_256"
  , "sha3_256"
  , "blake2b_256"
  , "verifyEd25519Signature"
  , "appendString"
  , "equalsString"
  , "encodeUtf8"
  , "decodeUtf8"
  , "ifThenElse"
  , "chooseUnit"
  , "trace"
  , "fstPair"
  , "sndPair"
  , "chooseList"
  , "mkCons"
  , "headList"
  , "tailList"
  , "nullList"
  , "chooseData"
  , "constrData"
  , "mapData"
  , "listData"
  , "iData"
  , "bData"
  , "unConstrData"
  , "unMapData"
  , "unListData"
  , "unIData"
  , "unBData"
  , "equalsData"
  , "mkPairData"
  , "mkNilData"
  , "mkNilPairData"
  , "serialiseData"
  , "verifyEcdsaSecp256k1Signature"
  , "verifySchnorrSecp256k1Signature"
  , "bls12_381_G1_add"
  , "bls12_381_G1_neg"
  , "bls12_381_G1_scalarMul"
  , "bls12_381_G1_equal"
  , "bls12_381_G1_hashToGroup"
  , "bls12_381_G1_compress"
  , "bls12_381_G1_uncompress"
  , "bls12_381_G2_add"
  , "bls12_381_G2_neg"
  , "bls12_381_G2_scalarMul"
  , "bls12_381_G2_equal"
  , "bls12_381_G2_hashToGroup"
  , "bls12_381_G2_compress"
  , "bls12_381_G2_uncompress"
  , "bls12_381_millerLoop"
  , "bls12_381_mulMlResult"
  , "bls12_381_finalVerify"
  , "keccak_256"
  , "blake2b_224"
  , "integerToByteString"
  , "byteStringToInteger"
  , "andByteString"
  , "orByteString"
  , "xorByteString"
  , "complementByteString"
  , "readBit"
  , "writeBits"
  , "replicateByte"
  , "shiftByteString"
  , "rotateByteString"
  , "countSetBits"
  , "findFirstSetBit"
  , "ripemd_160"
  ]

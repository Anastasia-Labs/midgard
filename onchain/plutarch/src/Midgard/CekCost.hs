{- |
Module      : Midgard.CekCost
Description : Plutarch port of @lib/midgard/cek-cost-v1.ak@.

The exact Plutus V3 builtin budget under the canonical V1 pinned cost model.

A fault proof that replays a CEK step has to agree with the ledger about what
that step /cost/, to the unit, or the replay proves nothing: an operator who
could make a step look cheaper on L1 than it was on L2 could overrun the budget
off-chain and still pass the on-chain check. So this module is a transcription of
the protocol's pinned cost model — one @(cpu, memory)@ pair per builtin tag,
under the same size arguments the machine would have measured — and its only
claim is that it reproduces those numbers exactly.

=== Everything fails closed

There is no default branch that guesses. A tag outside @0..86@ aborts, a size
vector of the wrong length for its tag aborts, and a negative size aborts. That
matters more here than in most modules: a budget is consumed as a number, and a
wrong number is not obviously wrong to its caller, so the only safe behaviour for
an input the table does not cover is to have no behaviour at all.

=== The shapes, and why they are named

The Plutus cost model does not use one formula. It uses about a dozen — linear
in one argument, linear in the sum, linear in the larger, constant unless the
arguments are equal, quadratic — and which one a builtin uses is part of the
model rather than an implementation detail. They are named here exactly as the
Aiken source names them, because a mis-shaped formula that happened to agree on
the fixture's sizes is the failure this module is most exposed to, and a named
shape is checkable against the protocol parameters by eye.

=== Three places the pinned table is not the Plutus model

The transcription in @cek-cost-v1.ak@ was checked against @plutus-core@'s own
cost model — @DefaultFunSemanticsVariantC@, the variant whose coefficients it
carries — by evaluating both at the same sizes. Eighty-four of the eighty-seven
builtins agree exactly, in both dimensions, at every size. Three do not, and the
port reproduces all three /as written/, because a port that silently corrected
them would disagree with the tree it is a port of:

  * __the division family__ (tags 3–6). Plutus's @quadratic_in_x_and_y@ reads
    @c02@ as the coefficient of @y²@; the Aiken source multiplies it by @y@.
    They agree for @y ∈ {0, 1}@ and differ by @900·(y² − y)@ elsewhere above the
    diagonal.

  * __@multiplyInteger@__ (tag 2). Plutus charges @slope·(x·y)@ — multiplying an
    @x@-word integer by a @y@-word one is quadratic work. The table charges
    @slope·(x + y)@. At 32×64 words that is 140,258 against Plutus's 1,153,346.

  * __@verifyEd25519Signature@__ (tag 21). Plutus charges the __message__, the
    only argument whose length varies. The table charges the __signature__,
    which is always 64 bytes.

"Testing.CekCost" pins each one twice — that the port matches the Aiken formula,
and that the Aiken formula differs from Plutus by a stated amount at stated
sizes — so neither fact can drift silently. All three under- or over-charge a
replayed step relative to the machine the ledger runs, which is the direction
that matters for a proof that replays one.

=== The one-based reading of @sizes@

@cost_argument_sizes@ is the machine's @ExMemory@ measurement of each argument,
in argument order, and the arity is the builtin's own — not the number of
arguments a caller chose to supply. 'pbuiltinArgumentCountV1' is that arity
table, and it is public because the machine needs it before it has a budget: it
is what tells the walk how many operands to pop.
-}
module Midgard.CekCost (
  PBuiltinBudgetV1 (..),
  pbuiltinArgumentCountV1,
  pbuiltinBudgetV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

--------------------------------------------------------------------------------
-- The budget
--------------------------------------------------------------------------------

{- | Aiken @BuiltinBudgetV1@.

Exact Plutus V3 builtin budget under the canonical V1 pinned cost model.
-}
data PBuiltinBudgetV1 (s :: S) = PBuiltinBudgetV1
  { pbudget'cpu :: Term s (PAsData PInteger)
  , pbudget'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBuiltinBudgetV1)

{- | Aiken @builtin_budget_v1@.

The tag must be a real builtin, the size vector must be exactly as long as that
builtin's arity, and every size must be non-negative. All three are @expect@s:
there is no budget for an input the model does not describe.
-}
pbuiltinBudgetV1 ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PInteger) :--> PBuiltinBudgetV1)
pbuiltinBudgetV1 = phoistAcyclic $
  plam $ \tag sizes ->
    pif
      ( 0
          #<= tag
          #&& (tag #<= 86)
          #&& (plength # sizes #== pbuiltinArgumentCountV1 # tag)
          #&& (pall # plam (\size -> 0 #<= pfromData size) # sizes)
      )
      ( pcon $
          PBuiltinBudgetV1
            { pbudget'cpu = pdata (pcpuCostV1 # tag # sizes)
            , pbudget'memory = pdata (pmemoryCostV1 # tag # sizes)
            }
      )
      perror

--------------------------------------------------------------------------------
-- Arity
--------------------------------------------------------------------------------

{- | Aiken @builtin_argument_count_v1@.

How many operands the builtin takes — the machine's own arity, which is what the
walk pops and what the size vector must match. Tags outside @0..86@ abort rather
than answering a default, because a wrong arity is a wrong pop.
-}
pbuiltinArgumentCountV1 :: forall (s :: S). Term s (PInteger :--> PInteger)
pbuiltinArgumentCountV1 = phoistAcyclic $
  plam $ \tag ->
    pif (0 #<= tag #&& tag #<= 86) `flip` perror $
      pif (tag #== 36) 6 $
        pif
          ( pelem'
              tag
              [12, 21, 26, 31, 52, 53, 73, 75, 76, 77, 80]
          )
          3
          $ pif
            ( tag
                #<= 11
                #|| pelem'
                  tag
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
            )
            2
            1

--------------------------------------------------------------------------------
-- CPU
--------------------------------------------------------------------------------

-- | Aiken @cpu_cost_v1@ — the pinned CPU table, tag by tag and in its order.
pcpuCostV1 ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PInteger) :--> PInteger)
pcpuCostV1 = phoistAcyclic $
  plam $ \tag sizes ->
    pif (tag #== 0 #|| tag #== 1) (plinear2Max 100788 420 sizes) $
      pif (tag #== 2) (plinear2Add 90434 519 sizes) $
        pif (3 #<= tag #&& tag #<= 6) (pdivisionCpu sizes) $
          pif (tag #== 7) (plinear2Min 51775 558 sizes) $
            pif (tag #== 8) (plinear2Min 44749 541 sizes) $
              pif (tag #== 9) (plinear2Min 43285 552 sizes) $
                pif (tag #== 10) (plinear2Add 1000 173 sizes) $
                  pif (tag #== 11) (plinear2Y 72010 178 sizes) $
                    pif (tag #== 12) (plinear3Z 20467 1 sizes) $
                      pif (tag #== 13) (parity1 sizes 22100) $
                        pif (tag #== 14) (parity2 sizes 13169) $
                          pif (tag #== 15) (plinearOnEqual 29498 38 24548 sizes) $
                            pif (tag #== 16 #|| tag #== 17) (plinear2Min 28999 74 sizes) $
                              pif (tag #== 18) (plinear1 270652 22588 (ponlySize sizes)) $
                                pif (tag #== 19) (plinear1 1457325 64566 (ponlySize sizes)) $
                                  pif (tag #== 20) (plinear1 201305 8356 (ponlySize sizes)) $
                                    pif (tag #== 21) (plinear3Z 53384111 14333 sizes) $
                                      pif (tag #== 22) (plinear2Add 1000 59957 sizes) $
                                        pif (tag #== 23) (plinearOnEqual 1000 60594 39184 sizes) $
                                          pif (tag #== 24) (plinear1 1000 42921 (ponlySize sizes)) $
                                            pif (tag #== 25) (plinear1 91189 769 (ponlySize sizes)) $
                                              pcpuCostFrom26 tag sizes

-- | The tail of 'pcpuCostV1', split only to keep the nesting readable.
pcpuCostFrom26 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
pcpuCostFrom26 tag sizes =
  pif (tag #== 26) (parity3 sizes 76049) $
    pif (tag #== 27) (parity2 sizes 61462) $
      pif (tag #== 28) (parity2 sizes 59498) $
        pif (tag #== 29) (parity1 sizes 141895) $
          pif (tag #== 30) (parity1 sizes 141992) $
            pif (tag #== 31) (parity3 sizes 132994) $
              pif (tag #== 32) (parity2 sizes 72362) $
                pif (tag #== 33) (parity1 sizes 83150) $
                  pif (tag #== 34) (parity1 sizes 81663) $
                    pif (tag #== 35) (parity1 sizes 74433) $
                      pif (tag #== 36) (parity6 sizes 94375) $
                        pif (tag #== 37) (parity2 sizes 22151) $
                          pif (tag #== 38) (parity1 sizes 68246) $
                            pif (tag #== 39) (parity1 sizes 33852) $
                              pif (tag #== 40) (parity1 sizes 15299) $
                                pif (tag #== 41) (parity1 sizes 11183) $
                                  pif (tag #== 42) (parity1 sizes 24588) $
                                    pif (tag #== 43) (parity1 sizes 24623) $
                                      pif (tag #== 44) (parity1 sizes 25933) $
                                        pif (tag #== 45) (parity1 sizes 20744) $
                                          pif (tag #== 46) (parity1 sizes 20142) $
                                            pif (tag #== 47) (plinear2Min 898148 27279 sizes) $
                                              pif (tag #== 48) (parity2 sizes 11546) $
                                                pif (tag #== 49) (parity1 sizes 7243) $
                                                  pif (tag #== 50) (parity1 sizes 7391) $
                                                    pif (tag #== 51) (plinear1 955506 213312 (ponlySize sizes)) $
                                                      pcpuCostFrom52 tag sizes

-- | The tail of 'pcpuCostFrom26'.
pcpuCostFrom52 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
pcpuCostFrom52 tag sizes =
  pif (tag #== 52) (parity3 sizes 43053543) $
    pif (tag #== 53) (plinear3Y 43574283 26308 sizes) $
      pif (tag #== 54) (parity2 sizes 962335) $
        pif (tag #== 55) (parity1 sizes 267929) $
          pif (tag #== 56) (plinear2X 76433006 8868 sizes) $
            pif (tag #== 57) (parity2 sizes 442008) $
              pif (tag #== 58) (plinear2X 52538055 3756 sizes) $
                pif (tag #== 59) (parity1 sizes 2780678) $
                  pif (tag #== 60) (parity1 sizes 52948122) $
                    pif (tag #== 61) (parity2 sizes 1995836) $
                      pif (tag #== 62) (parity1 sizes 284546) $
                        pif (tag #== 63) (plinear2X 158221314 26549 sizes) $
                          pif (tag #== 64) (parity2 sizes 901022) $
                            pif (tag #== 65) (plinear2X 166917843 4307 sizes) $
                              pif (tag #== 66) (parity1 sizes 3227919) $
                                pif (tag #== 67) (parity1 sizes 74698472) $
                                  pif (tag #== 68) (parity2 sizes 254006273) $
                                    pif (tag #== 69) (parity2 sizes 2174038) $
                                      pif (tag #== 70) (parity2 sizes 333849714) $
                                        pif (tag #== 71) (plinear1 2261318 64571 (ponlySize sizes)) $
                                          pif (tag #== 72) (plinear1 207616 8310 (ponlySize sizes)) $
                                            pif (tag #== 73) (pquadratic3Z 1293828 28716 63 sizes) $
                                              pif (tag #== 74) (pquadratic2Y 1006041 43623 251 sizes) $
                                                pif (75 #<= tag #&& tag #<= 77) (plinear3YZ 100181 726 719 sizes) $
                                                  pif (tag #== 78) (plinear1 107878 680 (ponlySize sizes)) $
                                                    pif (tag #== 79) (parity2 sizes 95336) $
                                                      pif (tag #== 80) (plinear3Y 281145 18848 sizes) $
                                                        pif (tag #== 81) (plinear2X 180194 159 sizes) $
                                                          pif (tag #== 82) (plinear2X 158519 8942 sizes) $
                                                            pif (tag #== 83) (plinear2X 159378 8813 sizes) $
                                                              pif (tag #== 84) (plinear1 107490 3298 (ponlySize sizes)) $
                                                                pif (tag #== 85) (plinear1 106057 655 (ponlySize sizes)) $
                                                                  pif
                                                                    (tag #== 86)
                                                                    (plinear1 1964219 24520 (ponlySize sizes))
                                                                    perror

--------------------------------------------------------------------------------
-- Memory
--------------------------------------------------------------------------------

-- | Aiken @memory_cost_v1@ — the pinned memory table, tag by tag and in its order.
pmemoryCostV1 ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PInteger) :--> PInteger)
pmemoryCostV1 = phoistAcyclic $
  plam $ \tag sizes ->
    pif (tag #== 0 #|| tag #== 1) (plinear2Max 1 1 sizes) $
      pif (tag #== 2) (plinear2Add 0 1 sizes) $
        pif (tag #== 3 #|| tag #== 4) (plinear2Sub 0 1 1 sizes) $
          pif (tag #== 5 #|| tag #== 6) (plinear2Y 0 1 sizes) $
            pif (7 #<= tag #&& tag #<= 9) (parity2 sizes 1) $
              pif (tag #== 10 #|| tag #== 11) (plinear2Add 0 1 sizes) $
                pif (tag #== 12) (plinear3Z 4 0 sizes) $
                  pif (tag #== 13) (parity1 sizes 10) $
                    pif (tag #== 14) (parity2 sizes 4) $
                      pif (15 #<= tag #&& tag #<= 17) (parity2 sizes 1) $
                        pif (18 #<= tag #&& tag #<= 20) (parity1 sizes 4) $
                          pif (tag #== 21) (parity3 sizes 10) $
                            pif (tag #== 22) (plinear2Add 4 1 sizes) $
                              pif (tag #== 23) (parity2 sizes 1) $
                                pif (tag #== 24 #|| tag #== 25) (plinear1 4 2 (ponlySize sizes)) $
                                  pif (tag #== 26) (parity3 sizes 1) $
                                    pif (tag #== 27) (parity2 sizes 4) $
                                      pif (28 #<= tag #&& tag #<= 46) (pcheckedArity tag sizes 32) $
                                        pif (tag #== 47) (parity2 sizes 1) $
                                          pif (48 #<= tag #&& tag #<= 50) (pcheckedArity tag sizes 32) $
                                            pif (tag #== 51) (plinear1 0 2 (ponlySize sizes)) $
                                              pmemoryCostFrom52 tag sizes

-- | The tail of 'pmemoryCostV1'.
pmemoryCostFrom52 ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
pmemoryCostFrom52 tag sizes =
  pif (tag #== 52 #|| tag #== 53) (parity3 sizes 10) $
    pif (54 #<= tag #&& tag #<= 56) (pcheckedArity tag sizes 18) $
      pif (tag #== 57) (parity2 sizes 1) $
        pif (tag #== 58) (parity2 sizes 18) $
          pif (tag #== 59) (parity1 sizes 6) $
            pif (tag #== 60) (parity1 sizes 18) $
              pif (61 #<= tag #&& tag #<= 63) (pcheckedArity tag sizes 36) $
                pif (tag #== 64) (parity2 sizes 1) $
                  pif (tag #== 65) (parity2 sizes 36) $
                    pif (tag #== 66) (parity1 sizes 12) $
                      pif (tag #== 67) (parity1 sizes 36) $
                        pif (tag #== 68 #|| tag #== 69) (parity2 sizes 72) $
                          pif (tag #== 70) (parity2 sizes 1) $
                            pif (tag #== 71 #|| tag #== 72) (parity1 sizes 4) $
                              pif (tag #== 73) (pconstYOrLinearZ 0 1 sizes) $
                                pif (tag #== 74) (plinear2Y 0 1 sizes) $
                                  pif (75 #<= tag #&& tag #<= 77) (plinear3MaxYZ 0 1 sizes) $
                                    pif (tag #== 78) (plinear1 0 1 (ponlySize sizes)) $
                                      pif (tag #== 79) (parity2 sizes 1) $
                                        pif (tag #== 80) (plinear3X 0 1 sizes) $
                                          pif (tag #== 81) (plinear2X 1 1 sizes) $
                                            pif (tag #== 82 #|| tag #== 83) (plinear2X 0 1 sizes) $
                                              pif (tag #== 84 #|| tag #== 85) (parity1 sizes 1) $
                                                pif (tag #== 86) (parity1 sizes 3) perror

--------------------------------------------------------------------------------
-- The shapes
--------------------------------------------------------------------------------

-- Aiken's `max` and `min` are Plutarch's 'pmax' and 'pmin'; the originals are
-- two-line `if` expressions and agree with them on every input, including ties.

-- | Aiken @one@ — @expect [x] = sizes@.
ponlySize :: forall (s :: S). Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger
ponlySize sizes = pwithOne sizes id

-- | Aiken @linear1@; the argument is already extracted, as in the original.
plinear1 :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger
plinear1 quote slope x = quote + x * slope

plinear2X, plinear2Y, plinear2Add, plinear2Min, plinear2Max :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger
plinear2X quote slope sizes = pwithTwo sizes $ \x _ -> quote + x * slope
plinear2Y quote slope sizes = pwithTwo sizes $ \_ y -> quote + y * slope
plinear2Add quote slope sizes = pwithTwo sizes $ \x y -> quote + (x + y) * slope
plinear2Min quote slope sizes = pwithTwo sizes $ \x y -> quote + (pmin x y) * slope
plinear2Max quote slope sizes = pwithTwo sizes $ \x y -> quote + (pmax x y) * slope

plinear2Sub ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
plinear2Sub quote slope minimum sizes =
  pwithTwo sizes $ \x y -> quote + (pmax minimum (x - y)) * slope

plinearOnEqual ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
plinearOnEqual quote slope constant sizes =
  pwithTwo sizes $ \x y -> pif (x #== y) (quote + x * slope) constant

plinear3X, plinear3Y, plinear3Z, plinear3MaxYZ :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger
plinear3X quote slope sizes = pwithThree sizes $ \x _ _ -> quote + x * slope
plinear3Y quote slope sizes = pwithThree sizes $ \_ y _ -> quote + y * slope
plinear3Z quote slope sizes = pwithThree sizes $ \_ _ z -> quote + z * slope
plinear3MaxYZ quote slope sizes = pwithThree sizes $ \_ y z -> quote + (pmax y z) * slope

plinear3YZ ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
plinear3YZ quote ySlope zSlope sizes =
  pwithThree sizes $ \_ y z -> quote + y * ySlope + z * zSlope

pquadratic2Y, pquadratic3Z :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger
pquadratic2Y x0 x1 x2 sizes = pwithTwo sizes $ \_ y -> x0 + x1 * y + x2 * y * y
pquadratic3Z x0 x1 x2 sizes = pwithThree sizes $ \_ _ z -> x0 + x1 * z + x2 * z * z

pconstYOrLinearZ ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger
pconstYOrLinearZ quote slope sizes =
  pwithThree sizes $ \_ y z -> pif (y #== 0) (quote + slope * z) y

{- | Aiken @division_cpu@.

The one model that is not a linear shape: below the diagonal it is flat, and
above it the quoted polynomial, floored at the same flat value.
-}
pdivisionCpu :: forall (s :: S). Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger
pdivisionCpu sizes =
  pwithTwo sizes $ \x y ->
    pif (x #< y) 85848 $
      pmax 85848 (123203 + 1716 * x + 7305 * y + 57 * x * x + 549 * x * y - 900 * y)

--------------------------------------------------------------------------------
-- Arity guards
--------------------------------------------------------------------------------

-- | Aiken @expect [x] = sizes@.
pwithOne ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PInteger)) ->
  (Term s PInteger -> Term s r) ->
  Term s r
pwithOne sizes k =
  pelimList (\x rest -> pif (pnull # rest) (k (pfromData x)) perror) perror sizes

-- | Aiken @expect [x, y] = sizes@.
pwithTwo ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PInteger)) ->
  (Term s PInteger -> Term s PInteger -> Term s r) ->
  Term s r
pwithTwo sizes k =
  pelimList (\x rest -> pwithOne rest (k (pfromData x))) perror sizes

-- | Aiken @expect [x, y, z] = sizes@.
pwithThree ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PInteger)) ->
  (Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s r) ->
  Term s r
pwithThree sizes k =
  pelimList (\x rest -> pwithTwo rest (k (pfromData x))) perror sizes

-- | A constant cost behind @expect [_] = sizes@.
parity1 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger -> Term s PInteger
parity1 sizes cost = pwithOne sizes (\_ -> cost)

-- | A constant cost behind @expect [_, _] = sizes@.
parity2 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger -> Term s PInteger
parity2 sizes cost = pwithTwo sizes (\_ _ -> cost)

-- | A constant cost behind @expect [_, _, _] = sizes@.
parity3 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger -> Term s PInteger
parity3 sizes cost = pwithThree sizes (\_ _ _ -> cost)

-- | A constant cost behind @expect [_, _, _, _, _, _] = sizes@ — tag 36 alone.
parity6 ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PInteger)) -> Term s PInteger -> Term s PInteger
parity6 sizes cost = pif (plength # sizes #== 6) cost perror

{- | Aiken @expect list.length(sizes) == builtin_argument_count_v1(tag)@.

The whole-block arity guard the memory table uses where one cost covers a run of
tags of differing arity.
-}
pcheckedArity ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger ->
  Term s PInteger
pcheckedArity tag sizes cost =
  pif (plength # sizes #== pbuiltinArgumentCountV1 # tag) cost perror

-- | Aiken's @or { tag == a, tag == b, … }@ over a literal set.
pelem' :: forall (s :: S). Term s PInteger -> [Integer] -> Term s PBool
pelem' tag = foldr (\n acc -> tag #== pconstant n #|| acc) (pconstant False)

module Midgard.List (
  nTails,
  pdropFast,
  pisUniqueSet,
  pbuiltinListLengthFast,
  penforceNSpendRedeemers
) where 

import Plutarch
import Plutarch.Evaluate 
import Plutarch.Bitwise 
import Data.List (foldl')
import Plutarch.ByteString
import Plutarch.Num ((#*))
import Plutarch.Builtin
import Plutarch.Integer 
import Plutarch.List
import Plutarch.Bool
import Plutarch.Monadic qualified as P
import Midgard.Utils (pcond, (#>))
import Plutarch.Lift (pconstant)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (PRedeemer(..), PScriptPurpose(..))

pdropR :: forall (list :: PType -> PType) (a :: PType) (s :: S). 
          PIsListLike list a => 
          Term s (PInteger :--> (list a) :--> (list a))
pdropR = phoistAcyclic $
  let go :: Term _ (PInteger :--> (list a) :--> (list a))
      go = pfix #$ plam $ \self n ys -> 
            pif (n #== 0) ys (self # (n - 1) # (ptail # ys))
   in go 

nTails :: PIsListLike list a => Integer -> Term s (list a) -> Term s (list a)
nTails n xs = foldl' (\acc _ -> ptail # acc) xs (replicate (fromIntegral n) ())

-- nTails :: PIsListLike list a => Integer -> Term s (list a) -> Term s (list a)
-- nTails n xs = pdrop' n # xs
--   where
--     pdrop' :: PIsListLike list a => Integer -> ClosedTerm (list a :--> list a)
--     pdrop' 0 = plam id
--     pdrop' 1 = ptail
--     pdrop' n' = phoistAcyclic $ plam $ \x -> ptail #$ pdrop' (n' - 1) # x

pdropFast :: PIsListLike PBuiltinList a => Term s (PInteger :--> (PBuiltinList a) :--> (PBuiltinList a))
pdropFast = phoistAcyclic $
  let go = pfix #$ plam $ \self n ys -> 
            pcond 
              [ ((30 #<= n), (self # (n - 30) # (nTails 30 ys)))
              , ((25 #<= n), (self # (n - 25) # (nTails 25 ys)))
              , ((20 #<= n), (self # (n - 20) # (nTails 20 ys)))
              , ((15 #<= n), (self # (n - 15) # (nTails 15 ys)))
              , ((10 #<= n), (self # (n - 10) # (nTails 10 ys)))
              ]
              (pdropR # n # ys)
   in go 

byteBools :: ClosedTerm (PBuiltinList PBool)
byteBools = unsafeEvalTerm NoTracing $ foldr (\h t -> pcons # pconstant h # t) pnil (replicate 255 True)

emptyByteArray :: ClosedTerm PByteString 
emptyByteArray = phexByteStr "0000000000000000000000000000000000000000000000000000000000000000"

single_byte_powers :: ClosedTerm PByteString 
single_byte_powers = foldr (\x acc -> pconsBS # pconstant x # acc) mempty [1,2,4,8,16,32,64,128]
  --phexByteStr "0102040810204080"

pcheckIndex :: Term s (PInteger :--> PInteger :--> PInteger)
pcheckIndex = phoistAcyclic $ plam $ \tagBits index -> P.do
  bit <- plet $ pow2_trick # index
  shifted_bit <- plet $ 2 * bit
  set_bit <- plet $ tagBits + bit
  pif (pmod # set_bit # shifted_bit #> pmod # tagBits # shifted_bit)
    set_bit
    perror

pow2_trick :: Term s (PInteger :--> PInteger) 
pow2_trick = plam $ \exp -> 
  pcond
    [ ((exp #< 8), (pindexBS # single_byte_powers # exp))
    , ((exp #< 16), (256 #* pindexBS # single_byte_powers # (exp - 8)))
    , ((exp #< 24), (65536 #* pindexBS # single_byte_powers # (exp - 16)))
    , ((exp #< 32), (16777216 #* pindexBS # single_byte_powers # (exp - 24)))
    , ((exp #< 40), (4294967296 #* pindexBS # single_byte_powers # (exp - 32)))
    , ((exp #< 48), (1099511627776 #* pindexBS # single_byte_powers # (exp - 40)))
    , ((exp #< 56), (281474976710656 #* pindexBS # single_byte_powers # (exp - 48)))
    ]
    (281474976710656 #* ppow2 # (exp - 48))

ppow2 :: Term s (PInteger :--> PInteger)
ppow2 = phoistAcyclic $ pfix #$ plam $ \self e ->
  pif (e #< 8)
    (pif (e #< 0)
      0
      (pindexBS # single_byte_powers # e)
    )
    (pif (e #< 32)
      (256 #* self # (e - 8))
      (4294967296 #* self # (e - 32))
    )

-- let (_, result2, _) = fromRight (error "") (evalTerm NoTracing (pisUniqueSet # 10 # pconstant [0..9])
pisUniqueSet :: Term s (PInteger :--> PBuiltinList PInteger :--> PBool)
pisUniqueSet = phoistAcyclic $ plam $ \n xs ->
  let flagUniqueBits = pwriteBits # emptyByteArray # xs # byteBools
  in (pcountSetBits # flagUniqueBits #== (pbuiltinListLengthFast # n # xs))

-- exists to bench against pisUniqueSet
_pIsUnique :: Term s (PBuiltinList PInteger :--> PBool)
_pIsUnique = phoistAcyclic $ plam $ \list ->
  let go :: Term _ (PInteger :--> PBuiltinList PInteger :--> PBool)
      go = pfix #$ plam $ \self flag_bit xs ->
            pelimList 
              (\y ys -> 
                self # (pcheckIndex # flag_bit # y) # ys
              )
              (pconstant True)
              xs 
  in go # 0 # list

pbuiltinListLength :: forall s a. (PElemConstraint PBuiltinList a) => Term s PInteger -> Term s (PBuiltinList a :--> PInteger)
pbuiltinListLength acc =
  (pfix #$ plam $ \self acc l ->
    pelimList 
      (\_ ys -> self # (acc + 1) # ys)  -- cons case
      acc                               -- nil case
      l
  )
  # acc

tails10 :: PIsListLike list a => ClosedTerm (list a :--> list a)
tails10 = phoistAcyclic $ plam (nTails 10)
tails20 :: PIsListLike list a => ClosedTerm (list a :--> list a)
tails20 = phoistAcyclic $ plam (\xs -> tails10 # (tails10 # xs))
tails30 :: PIsListLike list a => ClosedTerm (list a :--> list a)
tails30 = phoistAcyclic $ plam (\xs -> tails20 # (tails10 # xs))

pbuiltinListLengthFast :: forall (a :: PType) (s :: S). (PElemConstraint PBuiltinList a) => Term s (PInteger :--> PBuiltinList a :--> PInteger)
pbuiltinListLengthFast = phoistAcyclic $ plam $ \n elems ->
  let go :: Term _ (PInteger :--> PInteger :--> PBuiltinList a :--> PInteger)
      go = pfix #$ plam $ \self remainingExpected currentCount xs ->
             pcond 
               [ ((30 #<= remainingExpected), (self # (remainingExpected - 30) # (currentCount + 30) # (tails30 # xs)))
               , ((20 #<= remainingExpected), (self # (remainingExpected - 20) # (currentCount + 20) # (tails20 # xs)))
               , ((10 #<= remainingExpected), (self # (remainingExpected - 10) # (currentCount + 10) # (tails10 # xs)))
               ]
               (pbuiltinListLength 0 # xs)
   in go # n # 0 # elems 

penforceNSpendRedeemers :: forall {s :: S}. Term s PInteger -> Term s (AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer) -> Term s PBool
penforceNSpendRedeemers n rdmrs =
    let isNonSpend :: Term _ (PAsData PScriptPurpose) -> Term _ PBool
        isNonSpend red = pnot # (pfstBuiltin # (pasConstr # (pforgetData red)) #== 1)
             
        isLastSpend :: Term _ (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PBool)
        isLastSpend = plam $ \redeemers -> 
          let constrPair :: Term s (PAsData PScriptPurpose)
              constrPair = pfstBuiltin # (phead # redeemers)
              constrIdx = pfstBuiltin # (pasConstr # (pforgetData constrPair))
           in pif 
                (constrIdx #== 1) 
                (pelimList (\x _ -> isNonSpend (pfstBuiltin # x)) (pconstant True) (ptail # redeemers))
                perror
     in isLastSpend # (pdropFast # (n - 1) # (pto rdmrs))
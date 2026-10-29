-- | The target semantic resolver's authenticated zero-withdrawal handshake.
module Midgard.ValidationSemanticYield (
    PDispatch (..),
    prequireSemanticYieldV1,
    prequireSemanticYieldsV1,
    puniqueSemanticDispatchV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.Common.Utils (pheadSingleton)
import Midgard.StateQueueYield qualified as Yield
import Midgard.ValidationMachine (PValidationOneStepWitnessV1)
import Midgard.ValidationResolution (PPreparedValidationResolutionStateV1)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

data PDispatch s = PDispatch
    { pdispatch'state :: Term s PPreparedValidationResolutionStateV1
    , pdispatch'transition :: Term s PValidationOneStepWitnessV1
    , pdispatch'extra :: Term s (PBuiltinList PData)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PDispatch)

prequireSemanticYieldV1 :: forall s. Term s (PTxInfo :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PScriptHash)
prequireSemanticYieldV1 = Yield.prequireAuthenticatedZeroYield

prequireSemanticYieldsV1 :: forall s. Term s (PTxInfo :--> PCurrencySymbol :--> PBuiltinList (PAsData PTokenName) :--> PBuiltinList (PAsData PInteger) :--> PBool)
prequireSemanticYieldsV1 = phoistAcyclic $ plam $ \tx policy ->
    ( pfix $ \self -> plam $ \roles indices ->
        pelimList
            ( \role rest ->
                pelimList
                    (\index remaining -> plengthBS # pto (prequireSemanticYieldV1 # tx # policy # pfromData role # pfromData index) #== 28 #&& self # rest # remaining)
                    perror
                    indices
            )
            (pnull # indices)
            roles
    )

pfields :: forall s. Term s PInteger -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pfields tag count dat = pmatch (pasConstr # dat) $ \(PBuiltinPair actual fields) ->
    pif (actual #== tag #&& plength # fields #== count) fields perror

pat :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pat fields index = pelemAt # index # fields

pisInteger, pisBytes :: forall s. Term s PData -> Term s PBool
pisInteger dat = pchooseData # dat # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pisBytes dat = pchooseData # dat # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True

penum :: forall s. Term s PInteger -> Term s PData -> Term s PBool
penum maximum dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) -> tag #>= 0 #&& tag #<= maximum #&& pnull # fields

-- Shape checks reproduce Aiken's checked Data upcasts, without imposing
-- machine-version, width or other semantic predicates at this dispatch layer.
pmachineShape :: forall s. Term s (PData :--> PBool)
pmachineShape = phoistAcyclic $ plam $ \dat -> plet (pfields 0 15 dat) $ \f ->
    (pall # plam (\index -> pisInteger $ pat f index) # pconstant @(PBuiltinList PInteger) [0, 8, 10, 11])
        #&& (pall # plam (\index -> pisBytes $ pat f index) # pconstant @(PBuiltinList PInteger) [1, 2, 3, 4, 6, 9, 13, 14])
        #&& penum 1 (pat f 5)
        #&& penum 14 (pat f 7)
        #&& penum 2 (pat f 12)

ppreparedShape :: forall s. Term s PData -> Term s PBool
ppreparedShape dat = plet (pfields 0 3 dat) $ \prepared -> plet (pfields 0 4 $ pat prepared 1) $ \resolution ->
    pisInteger (pat prepared 0)
        #&& pisBytes (pat prepared 2)
        #&& pisInteger (pat resolution 0)
        #&& pmachineShape
        # pat resolution 1
        #&& pisBytes (pat resolution 2)
        #&& pisBytes (pat resolution 3)

ptransitionShape :: forall s. Term s PData -> Term s PBool
ptransitionShape dat = plet (pfields 0 2 dat) $ \f -> pisBytes (pat f 0) #&& pmachineShape # pat f 1

puniqueSemanticDispatchV1 :: forall s. Term s (PBuiltinList (PAsData PScriptHash) :--> PTxInfo :--> PDispatch)
puniqueSemanticDispatchV1 = phoistAcyclic $ plam $ \dispatchers tx -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'redeemers} <- pmatch tx
    PTxInInfo{ptxInInfo'outRef, ptxInInfo'resolved} <-
        pmatch $
            pfromData $
                pheadSingleton
                    # ( pfilter
                            # plam
                                ( \input -> P.do
                                    PTxInInfo{ptxInInfo'resolved = resolved} <- pmatch $ pfromData input
                                    PTxOut{ptxOut'address} <- pmatch resolved
                                    PAddress credential _ <- pmatch ptxOut'address
                                    pmatch credential $ \case PScriptCredential hash -> pelem # hash # dispatchers; _ -> pconstant False
                                )
                            # pfromData ptxInfo'inputs
                      )
    PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
    datum <- plet $ pmatch ptxOut'datum $ \case POutputDatum dat -> pto dat; _ -> perror
    PBuiltinPair _ redeemer <-
        pmatch $
            pheadSingleton
                # (pfilter # plam (\pair -> pfromData (pfstBuiltin # pair) #== pcon (PSpending ptxInInfo'outRef)) # (pto $ pto $ pfromData ptxInfo'redeemers))
    datumFields <- plet $ pfields 0 2 datum
    state <- plet $ phead # pfields 0 1 (pat datumFields 1)
    action <- plet $ phead # pfields 1 1 (pto $ pfromData redeemer)
    PBuiltinPair tag fields <- pmatch $ pasConstr # action
    transition <- plet $ pat fields 2
    pif
        (tag #== 0 #&& plength # fields #>= 3 #&& pisBytes (pat datumFields 0) #&& ppreparedShape state #&& ptransitionShape transition)
        (pcon $ PDispatch (punsafeCoerce state) (punsafeCoerce transition) (pdrop 3 fields))
        perror

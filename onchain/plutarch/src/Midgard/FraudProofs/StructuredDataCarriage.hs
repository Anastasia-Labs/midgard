{- | Target Aiken structured evidence wire and reconstruction. Publications are
untrusted preimages; the consuming proof must authenticate the resolved Data.
-}
module Midgard.FraudProofs.StructuredDataCarriage (presolve) where

import Midgard.Common.Utils (pheadSingleton)
import Plutarch.LedgerApi.V3 (POutputDatum (..), PTxInInfo (..), PTxOut (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

{- | Evidence is InlineEvidence(Data) or StructuredEvidence(EvidenceTree).
Decode the wire directly: notably MapItems carries Map Data, not a list of
constructor-encoded pairs. This matches Aiken's List<Pair<_, _>> encoding.
-}
presolve ::
  forall s.
  Term s (PData :--> PBuiltinList (PAsData PTxInInfo) :--> PData)
presolve = phoistAcyclic $ plam $ \evidence references -> P.do
  PBuiltinPair tag fields <- pmatch $ pasConstr # evidence
  payload <- plet $ pheadSingleton # fields
  pif
    (tag #== 0)
    payload
    (pif (tag #== 1) (presolveTree # payload # references) perror)

presolveTree ::
  forall s.
  Term s (PData :--> PBuiltinList (PAsData PTxInInfo) :--> PData)
presolveTree = phoistAcyclic $ pfix $ \self -> plam $ \tree references -> P.do
  PBuiltinPair tag fields <- pmatch $ pasConstr # tree
  let resolve item = self # item # references
      one = pheadSingleton # fields
      parts = pasList # one
  pif
    (tag #== 0)
    ( plet (pasInt # one) $ \index ->
        pif
          (index #>= 0)
          ( pmatch (pfromData $ pelemAt # index # references) $ \PTxInInfo {ptxInInfo'resolved} ->
              pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'datum} ->
                pmatch ptxOut'datum $ \case
                  POutputDatum datum -> pto datum
                  _ -> perror
          )
          perror
    )
    $ pif
      (tag #== 1)
      ( pif
          (plength # fields #== 2)
          ( pforgetData $
              pconstrBuiltin
                # (pasInt #$ phead # fields)
                # (pasList # resolve (pheadSingleton #$ ptail # fields))
          )
          perror
      )
    $ pif
      (tag #== 2)
      (plistData #$ pmap # plam resolve # parts)
    $ pif
      (tag #== 3)
      ( pmapData
          #$ pmap
          # plam
            ( \pair ->
                pmkPairData # resolve (pfstBuiltin # pair) # resolve (psndBuiltin # pair)
            )
          # (pasMap # one)
      )
    $ pif
      (tag #== 4)
      ( pforgetData $
          pdata $
            pfoldl
              # plam (\acc part -> acc <> (pasByteStr # resolve part))
              # pconstant ""
              # parts
      )
    $ pif
      (tag #== 5)
      ( plistData
          #$ pfoldr
          # plam (\part acc -> pconcat # (pasList # resolve part) # acc)
          # pnil
          # parts
      )
    $ pif
      (tag #== 6)
      ( pmapData
          #$ pfoldr
          # plam (\part acc -> pconcat # (pasMap # resolve part) # acc)
          # pnil
          # parts
      )
      perror

pmapData :: forall s. Term s (PBuiltinList (PBuiltinPair PData PData) :--> PData)
pmapData = punsafeBuiltin PLC.MapData

pmkPairData :: forall s. Term s (PData :--> PData :--> PBuiltinPair PData PData)
pmkPairData = punsafeBuiltin PLC.MkPairData

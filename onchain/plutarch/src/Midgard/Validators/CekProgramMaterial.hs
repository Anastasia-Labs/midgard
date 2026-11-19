{- |
Module      : Midgard.Validators.CekProgramMaterial
Description : Plutarch port of
              @validators/user-events/cek-program-material-v1.ak@.

An address that accepts UTxOs and never gives them back.

Midgard V1 program material is append-only L1 data availability: each published
node is content-addressed, so it authenticates itself and publication needs no
permission. What it does need is permanence, and the way this validator supplies
it is to have no spend path at all — the @spend@ handler returns @False@ and the
catch-all fails. An operator therefore cannot erase a forced submitter's material
ahead of the classification or challenge that would use it.

The datum type is 'Midgard.LedgerState.PCekProgramMaterialDatumV1'; nothing here
reads it, because nothing here can succeed.
-}
module Midgard.Validators.CekProgramMaterial (cekProgramMaterialSpendValidator) where

import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude

{- | Aiken @validators/user-events/cek-program-material-v1.ak@ — @spend@ returning
@False@, and @else(_) { fail }@.

Both of the original's handlers reject, so the port collapses to 'perror'
without inspecting the context. The two differ in Aiken only in /how/ they
reject, and a validator's wrapper erases that difference.
-}
cekProgramMaterialSpendValidator ::
  forall (s :: S). Term s (PScriptContext :--> PUnit)
cekProgramMaterialSpendValidator = plam $ \_ctx -> perror

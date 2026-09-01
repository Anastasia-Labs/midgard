{- |
Module      : Midgard.Validators.Witness
Description : Plutarch port of @validators/user-events/witness.ak@.

The witness script is the staking credential paired with a single user event.
Its hash is derived from that event's nonce, so it exists once and only once,
and the ledger's own refusal to register an already-registered credential is
what stops the event being created twice.

"Midgard.UserEvents.Witness" is the /other/ side of this: the check an event
policy runs to confirm the pairing. This module is the witness script itself —
what it demands of a transaction that publishes its certificate.

Three redeemers, two purposes:

  * @MintOrBurn@ ties the certificate to the event's NFT. Registering the
    credential must accompany the mint; unregistering it must accompany the
    burn. Neither can happen alone, which is what makes the credential's
    lifetime and the event's the same lifetime.

  * The two @*ToProveNotRegistered@ redeemers serve the opposite need. A fraud
    proof sometimes has to show an event was /never/ created, and there is no
    ledger query for "this credential is unregistered". The trick is that
    registering a credential is only possible if it is not already registered,
    so a transaction that registers and immediately unregisters the credential
    is a proof of prior absence — one that leaves the ledger exactly as it found
    it. Both halves are witnessed by this script, and each half checks that the
    other is present.
-}
module Midgard.Validators.Witness (
  witnessPublishValidator,
  pdropList,
) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V3 (
  PCredential,
  PScriptContext (..),
  PScriptInfo (..),
  PTokenName,
  PTxCert (..),
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pquantityOfMint)
import Midgard.UserEvents.Witness (PPublishRedeemer (..))

{- | Aiken @validators/user-events/witness.ak@ — @publish@, and @else(_) { fail }@.

The Aiken validator parameter @nonce@ becomes the leading argument; apply it
with 'Plutarch.Evaluate.applyArguments' to obtain the deployable script. The
nonce doubles as the event NFT's asset name, which is why it is typed as a token
name here.

Note the two rejection modes, following the original exactly. @MintOrBurn@
returns @False@ for a certificate that is neither a registration nor an
unregistration; the other two branches are @expect@ chains, so they /error/.
-}
witnessPublishValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PTokenName
        :--> PScriptContext
        :--> PUnit
    )
witnessPublishValidator = plam $ \nonce ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  -- Aiken's `publish(redeemer, certificate, tx)` receives the certificate
  -- directly; Plutus V3 carries it in the script info, alongside an index the
  -- original never looks at.
  certificate <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PCertifyingScript _ cert -> cert
      _ -> perror
  PTxInfo {ptxInfo'mint, ptxInfo'txCerts} <- pmatch pscriptContext'txInfo
  certificates <- plet $ pfromData ptxInfo'txCerts

  pif
    ( pmatch
        (pfromData (punsafeCoerce @(PAsData PPublishRedeemer) (pto pscriptContext'redeemer)))
        $ \case
          PMintOrBurn targetPolicy ->
            plet (pquantityOfMint # pfromData ptxInfo'mint # targetPolicy # nonce) $ \mintQty ->
              pmatch certificate $ \case
                PTxCertRegStaking _ _ -> mintQty #== 1
                PTxCertUnRegStaking _ _ -> mintQty #== -1
                _ -> pconstant False
          PRegisterToProveNotRegistered index ->
            pproveNotRegisteredFromRegistration
              certificate
              certificates
              (pfromData index)
          PUnregisterToProveNotRegistered index ->
            pproveNotRegisteredFromUnregistration
              certificate
              certificates
              (pfromData index)
    )
    (pconstant ())
    perror

{- | Aiken @RegisterToProveNotRegistered@.

This script is witnessing the /registration/ half. The certificate at
@registration_certificate_index@ must be this very certificate, and the one
immediately after it must unregister the same credential.

"Immediately after" is the load-bearing part. Were an intervening certificate
allowed, the pair would no longer be a no-op: something could observe the
credential as registered in between, and the proof of prior absence would have
had a side effect. Requiring adjacency is what keeps the pair inert.
-}
pproveNotRegisteredFromRegistration ::
  forall (s :: S).
  Term s PTxCert ->
  Term s (PBuiltinList (PAsData PTxCert)) ->
  Term s PInteger ->
  Term s PBool
pproveNotRegisteredFromRegistration certificate certificates index = P.do
  -- expect [reg_cert, unreg_cert, ..] = tx.certificates |> list.drop(index)
  fromIndex <- plet $ pdropList # index # certificates
  regCert <- plet $ pheadOrError fromIndex
  unregCert <- plet $ pheadOrError (ptail # fromIndex)

  -- expect reg_cert == certificate
  ptrueOrError (regCert #== pdata certificate) $
    -- expect RegisterCredential { credential: reg_cred, .. } = reg_cert
    plet (pregisteredCredential (pfromData regCert)) $ \regCred ->
      -- expect UnregisterCredential { credential: unreg_cred, .. } = unreg_cert
      plet (punregisteredCredential (pfromData unregCert)) $ \unregCred ->
        ptrueOrError (regCred #== unregCred) (pconstant True)

{- | Aiken @UnregisterToProveNotRegistered@.

The mirror image: this script is witnessing the /unregistration/ half, and the
index points back at the registration it undoes. Only the credentials are
compared — the adjacency requirement lives in the branch above, and one of the
two suffices to pin the ordering of a pair that must both be witnessed anyway.
-}
pproveNotRegisteredFromUnregistration ::
  forall (s :: S).
  Term s PTxCert ->
  Term s (PBuiltinList (PAsData PTxCert)) ->
  Term s PInteger ->
  Term s PBool
pproveNotRegisteredFromUnregistration certificate certificates index = P.do
  -- expect UnregisterCredential { credential: unreg_cred, .. } = certificate
  unregCred <- plet $ punregisteredCredential certificate
  -- expect [reg_cert, ..] = tx.certificates |> list.drop(index)
  regCert <- plet $ pheadOrError (pdropList # index # certificates)
  regCred <- plet $ pregisteredCredential (pfromData regCert)
  ptrueOrError (regCred #== unregCred) (pconstant True)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{- | Aiken @list.drop@.

Faithful to the stdlib definition, including its two edge cases: a non-positive
count returns the list untouched, and running off the end returns the empty list
rather than failing. Both matter, because the caller's @expect@ on the result is
what rejects — a negative index is /not/ an error here, it simply leaves the
caller looking at the head of the list.
-}
pdropList ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PInteger :--> PBuiltinList a :--> PBuiltinList a)
pdropList = phoistAcyclic $
  pfix $ \self ->
    plam $ \n xs ->
      pif
        (n #<= 0)
        xs
        (pelimList (\_ rest -> self # (n - 1) # rest) xs xs)

-- | The head of a list, erroring when it is empty — Aiken's @expect [x, ..]@.
pheadOrError ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PBuiltinList a) ->
  Term s a
pheadOrError xs = pelimList (\x _ -> x) perror xs

-- | Continue when the condition holds, error otherwise — Aiken's @expect@.
ptrueOrError ::
  forall (s :: S) (a :: S -> Type).
  Term s PBool ->
  Term s a ->
  Term s a
ptrueOrError cond k = pif cond k perror

-- | The credential a registration certificate registers; anything else errors.
pregisteredCredential :: forall (s :: S). Term s PTxCert -> Term s PCredential
pregisteredCredential cert =
  pmatch cert $ \case
    PTxCertRegStaking credential _ -> credential
    _ -> perror

-- | The credential an unregistration certificate releases; anything else errors.
punregisteredCredential :: forall (s :: S). Term s PTxCert -> Term s PCredential
punregisteredCredential cert =
  pmatch cert $ \case
    PTxCertUnRegStaking credential _ -> credential
    _ -> perror

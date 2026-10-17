import {
  ledgerEd25519SignatureReady,
  ledgerSignatureVerifierUnavailable,
  type LedgerSignatureVerifierUnavailableError,
} from "@al-ft/midgard-core";
import { Effect } from "effect";

/**
 * Waits until the shared Ed25519 verifier can answer. The wait is
 * `ledgerEd25519SignatureReady`'s, bounded where it is defined
 * (`LEDGER_ED25519_READY_TIMEOUT_MS`). A verifier that cannot answer fails
 * with its typed local fault: never a defect, and never a verdict on a
 * signature.
 */
export const awaitLedgerEd25519SignatureReady: Effect.Effect<
  void,
  LedgerSignatureVerifierUnavailableError
> = Effect.tryPromise({
  try: () => ledgerEd25519SignatureReady,
  catch: ledgerSignatureVerifierUnavailable,
});

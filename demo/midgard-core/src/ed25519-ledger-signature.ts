// The ambient declaration has to travel with this file: every workspace
// package type-checks midgard-core from source, and only a reference pulls
// the declaration into their programs.
// eslint-disable-next-line @typescript-eslint/triple-slash-reference
/// <reference path="./codec/libsodium-wrappers-sumo.d.ts" />
import sodium from "libsodium-wrappers-sumo";

const ED25519_PUBLIC_KEY_BYTES = 32;
const ED25519_SIGNATURE_BYTES = 64;

let sodiumReady = false;

/**
 * Resolves once {@link verifyLedgerEd25519Signature} can answer, and rejects
 * if libsodium cannot initialise. Worker start-up and asynchronous entry
 * points await it before their first verification.
 */
export const ledgerEd25519SignatureReady: Promise<void> = sodium.ready.then(
  () => {
    sodiumReady = true;
  },
);
// A failed initialisation surfaces where the promise is awaited and at every
// verification; it must not also end the process as an unhandled rejection.
ledgerEd25519SignatureReady.catch(() => undefined);

/**
 * Verifies an Ed25519 signature exactly as the ledger does.
 *
 * Cardano's `verifyEd25519Signature` builtin, and the L1 ledger's witness
 * check, is libsodium's `crypto_sign_verify_detached`; this calls that same
 * function. Every Midgard verdict on an L2 signature goes through here, so
 * the node, the trace builder and the fault-proof tooling answer as
 * `verify_ed25519_signature` does on chain.
 *
 * A key that is not 32 bytes or a signature that is not 64 bytes does not
 * verify. Before libsodium has initialised there is no answer: the call
 * raises, a fault of this process and never a verdict on the signature.
 */
export const verifyLedgerEd25519Signature = (
  publicKey: Uint8Array,
  message: Uint8Array,
  signature: Uint8Array,
): boolean => {
  if (
    publicKey.length !== ED25519_PUBLIC_KEY_BYTES ||
    signature.length !== ED25519_SIGNATURE_BYTES
  ) {
    return false;
  }
  if (!sodiumReady) {
    throw new Error(
      "Ed25519 verification is unavailable: libsodium has not initialised",
    );
  }
  return sodium.crypto_sign_verify_detached(signature, message, publicKey);
};

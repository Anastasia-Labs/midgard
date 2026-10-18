// The ambient declaration has to travel with this file: every workspace
// package type-checks midgard-core from source, and only a reference pulls
// the declaration into their programs.
// eslint-disable-next-line @typescript-eslint/triple-slash-reference
/// <reference path="./codec/libsodium-wrappers-sumo.d.ts" />
import sodium from "libsodium-wrappers-sumo";

const ED25519_PUBLIC_KEY_BYTES = 32;
const ED25519_SIGNATURE_BYTES = 64;

/** Marks the error across module copies (an ESM and a CJS load of this file). */
const UNAVAILABLE = Symbol.for("midgard.ledgerSignatureVerifierUnavailable");

/**
 * The ledger's Ed25519 verifier could not answer in this process: libsodium
 * did not initialise, or it, or building the message it checks, raised. A
 * fault of this process, never a verdict on a signature: a caller retries or
 * stops, and never reads it as a signature that does not verify. The
 * original error is its `cause`, and its text is in the message.
 */
export class LedgerSignatureVerifierUnavailableError extends Error {
  constructor(message: string, options?: { readonly cause?: unknown }) {
    super(message);
    this.name = "LedgerSignatureVerifierUnavailableError";
    // Set as `Error`'s own `cause` option would, without needing the ES2022
    // library in every package that type-checks this file.
    if (options !== undefined && "cause" in options)
      Object.defineProperty(this, "cause", {
        value: options.cause,
        writable: true,
        configurable: true,
      });
    Object.defineProperty(this, UNAVAILABLE, { value: true });
  }
}

/** The `LedgerSignatureVerifierUnavailableError` in `error`'s cause chain. */
export const findLedgerSignatureVerifierUnavailable = (
  error: unknown,
): LedgerSignatureVerifierUnavailableError | undefined => {
  let current = error;
  for (let depth = 0; depth < 8; depth += 1) {
    if (typeof current !== "object" || current === null) return undefined;
    if (Reflect.get(current, UNAVAILABLE) === true)
      return current as LedgerSignatureVerifierUnavailableError;
    current = (current as { cause?: unknown }).cause;
  }
  return undefined;
};

/**
 * `cause` as the verifier's local fault: the typed error it already carries,
 * or a new one that keeps it as its cause and its text in the message.
 */
export const ledgerSignatureVerifierUnavailable = (
  cause: unknown,
): LedgerSignatureVerifierUnavailableError =>
  findLedgerSignatureVerifierUnavailable(cause) ??
  new LedgerSignatureVerifierUnavailableError(
    `Ed25519 verification is unavailable: ${
      cause instanceof Error ? cause.message : String(cause)
    }`,
    { cause },
  );

let sodiumReady = false;

/**
 * How long a wait for the verifier may take, measured from the first wait
 * (`ledgerEd25519SignatureReady()`), before the verifier is unavailable for
 * the life of the process. Initialisation is a local WebAssembly
 * instantiation with nothing external to wait on; the node's startup attempt
 * uses the same bound.
 */
export const LEDGER_ED25519_READY_TIMEOUT_MS = 10_000;

/** Set once the bound has run out: libsodium's later readiness no longer
 * makes the verifier answer. */
let readinessTimedOut = false;

/** libsodium's initialisation, started when this module loads. Its failure
 * is the verifier's local fault. */
const initialisation: Promise<void> = sodium.ready.then(
  () => {
    if (!readinessTimedOut) sodiumReady = true;
  },
  (cause: unknown) => {
    throw ledgerSignatureVerifierUnavailable(cause);
  },
);
// A failed initialisation surfaces at the wait and at every verification; it
// must not also end the process as an unhandled rejection.
initialisation.catch(() => undefined);

let readiness: Promise<void> | undefined;

/**
 * Resolves once {@link verifyLedgerEd25519Signature} can answer. Rejects with
 * a `LedgerSignatureVerifierUnavailableError` if libsodium cannot initialise,
 * at once, or has not within `LEDGER_ED25519_READY_TIMEOUT_MS` of the first
 * call, so no caller waits longer than that. The bound starts at the first
 * call, not when this module loads, so synchronous work between the two does
 * not count against it.
 *
 * Every call returns the same promise, so the outcome is settled once: every
 * later wait gets the same result at once, and after a timeout the verifier
 * stays unavailable even if libsodium initialises afterwards. Worker start-up
 * and asynchronous entry points wait on it before their first verification.
 */
export const ledgerEd25519SignatureReady = (): Promise<void> => {
  if (readiness !== undefined) return readiness;
  readiness = new Promise<void>((resolve, reject) => {
    let settled = false;
    // Not unref'd: a process with nothing else to do still waits out the
    // bound, and so reports the failure instead of ending with it unsettled.
    const timer = setTimeout(() => {
      if (settled) return;
      settled = true;
      readinessTimedOut = true;
      reject(
        new LedgerSignatureVerifierUnavailableError(
          `Ed25519 verification is unavailable: libsodium did not initialise within ${LEDGER_ED25519_READY_TIMEOUT_MS.toString()} ms`,
        ),
      );
    }, LEDGER_ED25519_READY_TIMEOUT_MS);
    initialisation.then(
      () => {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        resolve();
      },
      (fault: unknown) => {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        // Already the typed fault, which this returns as it is.
        reject(ledgerSignatureVerifierUnavailable(fault));
      },
    );
  });
  // As for the initialisation: a caller that drops the promise must not end
  // the process with an unhandled rejection.
  readiness.catch(() => undefined);
  return readiness;
};

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
 * verify. Before libsodium has initialised, or when the check itself raises,
 * there is no answer: the call raises `LedgerSignatureVerifierUnavailableError`,
 * a fault of this process and never a verdict on the signature.
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
    throw new LedgerSignatureVerifierUnavailableError(
      "Ed25519 verification is unavailable: libsodium has not initialised",
    );
  }
  try {
    return sodium.crypto_sign_verify_detached(signature, message, publicKey);
  } catch (cause) {
    throw ledgerSignatureVerifierUnavailable(cause);
  }
};

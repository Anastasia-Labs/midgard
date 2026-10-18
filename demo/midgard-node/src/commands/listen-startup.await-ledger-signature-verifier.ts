/**
 * Startup's wait for the ledger signature verifier
 * (`ledgerEd25519SignatureReady()`): libsodium answers every Ed25519 check
 * the node makes, and initialises asynchronously once per process.
 *
 * - It runs in `local_preflight`, before the instance lock, the database and
 *   the L1 follower, so the node follows, processes and holds no block until
 *   the verifier can answer.
 * - An attempt waits at most `attemptTimeout`. One that times out or rejects
 *   is waited out under `ledger_signature_verifier_unavailable`, `retryDelay`
 *   apart, for at most `maxAttempts` attempts in all.
 * - Past that the step fails exhausted (`StartupStepFailedError`, step
 *   `ledger_signature_verifier`), so the process exits non-zero and its
 *   supervisor restarts it (`withStartupHttpServer`).
 * - The verifier bounds its own initialisation
 *   (`LEDGER_ED25519_READY_TIMEOUT_MS`, which `attemptTimeout` matches),
 *   measured from the first wait in the process: this step's first attempt
 *   at the latest. One that fails or does not settle within it stays
 *   unavailable for the life of the process, so later attempts fail at once
 *   and the step is exhausted about `retryDelay` per attempt after the
 *   first. A restart is the retry that can clear it, and the node never runs
 *   without it.
 *
 * The bound is a constant, not configuration: initialisation is a local
 * WebAssembly instantiation with nothing external to wait on, so a short
 * bound (about half a minute at most) hands a failure to the supervisor
 * quickly.
 */
import {
  LEDGER_ED25519_READY_TIMEOUT_MS,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
import { Duration, Effect } from "effect";

import {
  retryStartupStep,
  type StartupStepFailedError,
} from "../services/startup-waiting.js";

/** The startup step the wait reports under. */
export const LEDGER_SIGNATURE_VERIFIER_STEP = "ledger_signature_verifier";

/** The ledger signature verifier has not initialised yet, or its
 * initialisation failed. */
export const LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE =
  "ledger_signature_verifier_unavailable";

export type LedgerSignatureVerifierStartupBounds = Readonly<{
  attemptTimeout: Duration.DurationInput;
  maxAttempts: number;
  retryDelay: Duration.DurationInput;
}>;

/** At most 3 attempts, 1 s apart, each as long as the verifier's own bound
 * (10 s). */
export const LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS: LedgerSignatureVerifierStartupBounds =
  {
    attemptTimeout: Duration.millis(LEDGER_ED25519_READY_TIMEOUT_MS),
    maxAttempts: 3,
    retryDelay: Duration.seconds(1),
  };

/** Waits until the ledger signature verifier can answer; see the module
 * documentation for its bound and its failure. */
export const awaitLedgerSignatureVerifierOnStartup = (
  bounds: LedgerSignatureVerifierStartupBounds = LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS,
): Effect.Effect<void, StartupStepFailedError> => {
  const attemptTimeout = Duration.decode(bounds.attemptTimeout);
  const retryDelayMs = Duration.toMillis(Duration.decode(bounds.retryDelay));
  return retryStartupStep(
    Effect.tryPromise({
      try: () => ledgerEd25519SignatureReady(),
      catch: (cause) =>
        new Error("The ledger signature verifier failed to initialise", {
          cause,
        }),
    }).pipe(
      Effect.timeoutFail({
        duration: attemptTimeout,
        onTimeout: () =>
          new Error(
            `The ledger signature verifier did not initialise within ${Duration.format(attemptTimeout)}`,
          ),
      }),
    ),
    {
      key: LEDGER_SIGNATURE_VERIFIER_STEP,
      reason: LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
      retryable: () => true,
      budget: { maxAttempts: bounds.maxAttempts },
      initialMs: retryDelayMs,
      maxMs: retryDelayMs,
    },
  );
};

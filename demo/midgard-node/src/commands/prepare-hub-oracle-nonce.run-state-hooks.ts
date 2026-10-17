import { Effect } from "effect";

import {
  type DeploymentRunCliOptions,
  recordHubOracleNonceSigned,
  recordHubOracleNonceSubmitted,
  recordHubOracleNonceTxHashConfirmed,
} from "./deployment-run-state.record-hub-oracle-nonce.js";
import type {
  PrepareHubOracleNonceOptions,
  SubmittedHubOracleNonceAttempt,
} from "./prepare-hub-oracle-nonce.js";

const recording = (what: string, write: () => Promise<unknown>) =>
  Effect.tryPromise({
    try: write,
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error(`Failed to record ${what} in run state: ${String(cause)}`),
  }).pipe(Effect.asVoid);

const nonceFields = (attempt: SubmittedHubOracleNonceAttempt) => ({
  txHash: attempt.txHash,
  address: attempt.address,
  lovelace: attempt.lovelace,
  inlineDatum: attempt.inlineDatum,
});

/**
 * The run-state writes a nonce transaction makes. `beforeSubmission` is the
 * write-ahead: the signed bytes are durable before any submission, so a crash
 * after submitting leaves run state that a rerun resumes instead of building a
 * second nonce.
 */
export const hubOracleNonceRunStateHooks = (
  options: DeploymentRunCliOptions,
  network: string,
) =>
  ({
    beforeSubmission: (attempt) =>
      recording("signed hub-oracle nonce", () =>
        recordHubOracleNonceSigned({
          options,
          network,
          ...nonceFields(attempt),
          signedTxCbor: attempt.signedTxCbor,
        }),
      ),
    onSubmitted: (attempt) =>
      recording("submitted hub-oracle nonce", () =>
        recordHubOracleNonceSubmitted({
          options,
          network,
          ...nonceFields(attempt),
        }),
      ),
    onTxHashConfirmed: (attempt, confirmationStatus) =>
      recording("confirmed hub-oracle nonce tx", () =>
        recordHubOracleNonceTxHashConfirmed({
          options,
          network,
          ...nonceFields(attempt),
          confirmationStatus,
        }),
      ),
  }) satisfies Required<
    Pick<
      PrepareHubOracleNonceOptions,
      "beforeSubmission" | "onSubmitted" | "onTxHashConfirmed"
    >
  >;

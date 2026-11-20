import type { TimeoutCorrectionJournal } from "@al-ft/midgard-fault-proofs/remove-unattested-block";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { Effect } from "effect";

export type AttestationTimeoutObservation =
  | { readonly status: "queue-empty" | "queue-attested" }
  | {
      readonly status: "waiting" | "near-timeout" | "timed-out";
      readonly headerHash: string;
      readonly deadlineMs: bigint;
      readonly remainingMs: bigint;
    };

/** Read-only pending-queue classification; the watcher never owns correction writes. */
export const observeAttestationTimeoutQueue = (
  queue: readonly SDKStateQueue.StateQueueUTxO[],
  nowMs: bigint,
  alertLeadMs: bigint,
): Effect.Effect<AttestationTimeoutObservation, SDK.DataCoercionError> =>
  Effect.gen(function* () {
    if (queue.length <= 1) return { status: "queue-empty" } as const;
    let pending:
      | { entry: SDKStateQueue.StateQueueUTxO; node: SDK.StateQueueNode }
      | undefined;
    for (const entry of queue.slice(1)) {
      const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(entry.datum);
      if (
        node.da_attestation === SDK.NO_DA_ATTESTATION &&
        (pending === undefined || node.block_time < pending.node.block_time)
      ) {
        pending = { entry, node };
      }
    }
    if (pending === undefined) return { status: "queue-attested" } as const;
    const { entry, node } = pending;
    const headerHash = yield* SDKStateQueue.headerHashFromStateQueueUTxO(entry);
    const deadlineMs =
      node.block_time + SDKStateQueue.DA_ATTESTATION_TIMEOUT_MS;
    const remainingMs = deadlineMs - nowMs;
    return {
      status:
        remainingMs <= 0n
          ? "timed-out"
          : remainingMs <= alertLeadMs
            ? "near-timeout"
            : "waiting",
      headerHash,
      deadlineMs,
      remainingMs,
    } as const;
  });

/** An absent target does not establish the outcome of retained signed bytes.
 * Completed work also reopens if its exact target returns after a rollback. */
export const timeoutCorrectionJournalNeedsRecovery = (
  journal:
    | Pick<TimeoutCorrectionJournal, "completed" | "targetHeaderHash">
    | undefined,
  queue: readonly SDKStateQueue.StateQueueUTxO[],
): boolean =>
  journal !== undefined &&
  (!journal.completed ||
    queue
      .slice(1)
      .some(
        ({ datum }) =>
          datum.key !== "Empty" &&
          datum.key.Key.key === journal.targetHeaderHash,
      ));

import type { TimeoutCorrectionJournal } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
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
  queue: readonly SDK.StateQueueUTxO[],
  nowMs: bigint,
  alertLeadMs: bigint,
): Effect.Effect<AttestationTimeoutObservation, SDK.DataCoercionError> =>
  Effect.gen(function* () {
    if (queue.length <= 1) return { status: "queue-empty" } as const;
    let pending:
      | { entry: SDK.StateQueueUTxO; node: SDK.StateQueueNode }
      | undefined;
    for (const entry of queue.slice(1)) {
      const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(entry.datum);
      if (
        node.da_attestation === SDK.NO_DA_ATTESTATION &&
        (pending === undefined ||
          node.header.endTime < pending.node.header.endTime)
      ) {
        pending = { entry, node };
      }
    }
    if (pending === undefined) return { status: "queue-attested" } as const;
    const { entry, node } = pending;
    const headerHash = yield* SDK.headerHashFromStateQueueUTxO(entry);
    const deadlineMs = node.header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
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
  queue: readonly SDK.StateQueueUTxO[],
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

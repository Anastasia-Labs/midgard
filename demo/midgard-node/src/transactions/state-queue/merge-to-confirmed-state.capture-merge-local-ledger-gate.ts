import {
  SUBMIT_SLOT_LENGTH_MS,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  registerSlotAwareDueWork,
  type SlotAwareDueWork,
} from "../../fibers/slot-aware-due-work.js";
import { submitSlotEvidence } from "../../l1-provider-view.js";
import { Database } from "../../services/index.js";
import { submitLowerBoundSlot } from "../submit-timing.js";
import { slotAwareDueWorkFromSubmitTiming } from "../submit-timing-due-work.js";
import {
  type NoInlineSubmitDefer,
  type NoInlineSubmitRecoveryOptions,
} from "../utils.js";
import { resolvePreSubmitSlotSnapshot } from "../utils.submit-recovery-options.js";
import {
  DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS,
  mergeSubmitValidityEvidence,
  planMergeLocalLedgerGate,
} from "./merge-readiness.js";
import {
  type CanonicalMergeCandidateReadiness,
  type MergeTxResult,
  slotFromUnixTime,
} from "./merge-to-confirmed-state.fetch-canonical-merge-candidate-readiness.js";
import { MERGE_CONFIRMATION_PROVIDER_RETRIES } from "./merge-to-confirmed-state.landed-unfinalized-merges.js";

/**
 * The merge's submit recovery. Without an explicit reader, the submit-slot
 * snapshot is the Lucid client's registered one (the local node's ledger tip).
 */
export const mergeSubmitRecoveryOptions = (
  deferEvidence: {
    readonly key: string;
    readonly dependencyKey: string;
    readonly invalidationKey: string;
  },
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
  confirmationDeadlineMs?: number,
): NoInlineSubmitRecoveryOptions => ({
  label: "merge",
  confirmationRetries: MERGE_CONFIRMATION_PROVIDER_RETRIES,
  ...(confirmationDeadlineMs === undefined ? {} : { confirmationDeadlineMs }),
  ...(submitSlotSnapshot === undefined
    ? {}
    : { slotSnapshot: submitSlotSnapshot }),
  requireSlotForBoundedTx: true,
  maxPreSubmitWaitMs: DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS,
  inlineWaitPolicy: "defer_positive_wait",
  noInlineSubmitDefer: deferEvidence,
});

export const mergeNoInlineSubmitDueWorkFromDefer = ({
  defer,
  localSubmitSlotSnapshot,
  nowMs,
}: {
  readonly defer: NoInlineSubmitDefer;
  readonly localSubmitSlotSnapshot?: {
    readonly currentSlot: number;
    readonly ledgerTipSlot?: number;
    readonly slotLengthMs: number;
    readonly source: string;
  };
  readonly nowMs?: number;
}): SlotAwareDueWork => {
  if (defer.kind !== "provider_slot_wait") {
    return {
      kind: "merge_submit_validity",
      key: defer.key,
      callerLabel: defer.callerLabel,
      reason: `merge_submit_${defer.kind}_not_reached`,
      observedSlot: defer.currentSlot,
      dueSlot: defer.dueSlot,
      dueAtMs: (nowMs ?? Date.now()) + defer.waitMs,
      waitMs: defer.waitMs,
      slotSource: defer.slotSource,
      dependencyKey: defer.dependencyKey,
      invalidationKey: defer.invalidationKey,
    };
  }
  if (localSubmitSlotSnapshot === undefined) {
    throw new Error(
      "local submit slot snapshot is required for merge provider-slot defer",
    );
  }
  const slotLengthMs = Math.max(
    1,
    Math.floor(localSubmitSlotSnapshot.slotLengthMs || SUBMIT_SLOT_LENGTH_MS),
  );
  const localLedgerSlot = submitLowerBoundSlot(localSubmitSlotSnapshot);
  return {
    kind: "merge_submit_validity",
    key: defer.key,
    callerLabel: defer.callerLabel,
    reason: `merge_submit_${defer.kind}_not_reached,provider_current_slot=${defer.currentSlot.toString()},provider_due_slot=${defer.dueSlot.toString()}`,
    observedSlot: localLedgerSlot,
    dueSlot:
      localLedgerSlot + Math.max(1, Math.ceil(defer.waitMs / slotLengthMs)),
    dueAtMs: (nowMs ?? Date.now()) + defer.waitMs,
    waitMs: defer.waitMs,
    slotSource: localSubmitSlotSnapshot.source,
    dependencyKey: defer.dependencyKey,
    invalidationKey: defer.invalidationKey,
  };
};

export const registerMergeNoInlineSubmitDueWork = (
  defer: NoInlineSubmitDefer,
  lucid: LucidEvolution,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
) =>
  Effect.gen(function* () {
    if (defer.kind !== "provider_slot_wait") {
      return registerSlotAwareDueWork(
        mergeNoInlineSubmitDueWorkFromDefer({ defer }),
      );
    }
    const localSnapshot = yield* resolvePreSubmitSlotSnapshot(
      lucid,
      submitSlotSnapshot,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to read the local ledger's submit slot before registering merge provider-slot defer",
            cause,
          }),
      ),
    );
    return registerSlotAwareDueWork(
      mergeNoInlineSubmitDueWorkFromDefer({
        defer,
        localSubmitSlotSnapshot: localSnapshot,
      }),
    );
  });

export const captureMergeLocalLedgerGate = ({
  lucid,
  validFromUnixTime,
  leaseToken,
  headerHash,
  candidateIdentity,
  submitSlotSnapshot,
}: {
  readonly lucid: LucidEvolution;
  readonly validFromUnixTime: number;
  readonly leaseToken?: string;
  readonly headerHash: string;
  readonly candidateIdentity?: string;
  readonly submitSlotSnapshot?: () => Effect.Effect<
    SubmitSlotSnapshot,
    unknown
  >;
}): Effect.Effect<
  | { readonly status: "ready" }
  | { readonly status: "retry_later"; readonly reason: string },
  SDK.StateQueueError,
  Database
> =>
  Effect.gen(function* () {
    const validFromSlot = yield* slotFromUnixTime(lucid, validFromUnixTime);
    const dueWorkEvidence = mergeSubmitValidityEvidence({
      headerHash,
      validFromSlot,
      ...(candidateIdentity === undefined ? {} : { candidateIdentity }),
    });
    const snapshot = yield* resolvePreSubmitSlotSnapshot(
      lucid,
      submitSlotSnapshot,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to read submit-ledger slot before merge build",
            cause,
          }),
      ),
    );
    // The mempool judges the lower bound at the ledger tip, which trails the
    // wall-clock slot between blocks.
    const localLedgerSlot = submitLowerBoundSlot(snapshot);
    const gate = planMergeLocalLedgerGate({
      validFromSlot,
      localLedgerSlot,
      slotLengthMs: snapshot.slotLengthMs,
      slotSource: snapshot.source,
      observedAtMs: snapshot.observedAtMs,
      maxWaitMs: DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS,
      inlineWaitPolicy: "defer_positive_wait",
      dependencyKey: dueWorkEvidence.dependencyKey,
      invalidationKey: dueWorkEvidence.invalidationKey,
    });
    const logFields = [
      `validFromUnixTime=${validFromUnixTime.toString()}`,
      `validFromSlot=${validFromSlot.toString()}`,
      `headerHash=${headerHash}`,
      `candidateIdentity=${candidateIdentity ?? "none"}`,
      `targetSlot=${gate.targetSlot.toString()}`,
      `localLedgerSlot=${localLedgerSlot.toString()}`,
      `deltaSlots=${gate.deltaSlots.toString()}`,
      `waitMs=${gate.waitMs.toString()}`,
      `slotSource=${snapshot.source}`,
      `leaseToken=${leaseToken ?? "none"}`,
      "callerLabel=merge",
      `dependencyKey=${dueWorkEvidence.dependencyKey}`,
      `invalidationKey=${dueWorkEvidence.invalidationKey}`,
      submitSlotEvidence(snapshot),
    ].join(",");
    if (gate.status === "ready") {
      yield* Effect.logInfo(`🔸 Merge local-ledger gate ready (${logFields}).`);
      return { status: "ready" } as const;
    }
    if (gate.status === "retry_later") {
      if (gate.submitTimingNotDuePlan === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Merge due-work planning lost submit-timing evidence",
            cause: gate.reason,
          }),
        );
      }
      const dueWork = slotAwareDueWorkFromSubmitTiming({
        kind: "merge_submit_validity",
        key: dueWorkEvidence.key,
        callerLabel: "merge",
        reason: "merge_submit_validity_not_reached",
        plan: gate.submitTimingNotDuePlan,
        nowMs: Date.now(),
      });
      registerSlotAwareDueWork(dueWork);
      yield* Effect.logInfo(
        `🔸 Skipping merge until local submit ledger reaches merge validity lower bound (kind=${dueWork.kind},key=${dueWork.key},current_slot=${dueWork.observedSlot.toString()},due_slot=${dueWork.dueSlot.toString()},wait_ms=${dueWork.waitMs.toString()},slot_source=${dueWork.slotSource},dependency_key=${dueWork.dependencyKey},${logFields}).`,
      );
      return {
        status: "retry_later",
        reason: `${gate.reason},valid_from_unix_time=${validFromUnixTime.toString()}`,
      } as const;
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Merge local-ledger planner unexpectedly allowed an inline wait in no-inline mode",
        cause: logFields,
      }),
    );
  });

/**
 * Skips a merge whose state-queue inputs a live own intent already spends
 * (the wallet view's `held`, `txHash#index`). That intent is the merge in
 * flight: the journal resends it once per tip until it lands, or abandons it
 * after bounded refusals, which releases its inputs. A rebuild would spend
 * the same inputs again and fund itself from the live intent's predicted
 * change, so it needs that intent to land and conflicts with it: the ledger
 * refuses it whatever happens. The skip builds nothing and sends nothing.
 */
export const ownMergeInFlightSkip = (
  held: ReadonlySet<string>,
  candidate: Pick<
    Extract<CanonicalMergeCandidateReadiness, { readonly status: "candidate" }>,
    "confirmedUTxO" | "firstBlockUTxO" | "readiness"
  >,
): Effect.Effect<MergeTxResult> | undefined => {
  const heldInputs = [candidate.confirmedUTxO, candidate.firstBlockUTxO]
    .map(({ utxo }) => `${utxo.txHash}#${utxo.outputIndex.toString()}`)
    .filter((outRef) => held.has(outRef));
  if (heldInputs.length === 0) return undefined;
  const { headerHash, readyAfterUnixTime, nowUnixTime } = candidate.readiness;
  const reason = `held_merge_inputs=${heldInputs.join("|")}`;
  return Effect.as(
    Effect.logInfo(
      `🔸 Skipping merge while an own merge intent spending its inputs is live (${reason},headerHash=${headerHash}).`,
    ),
    {
      status: "skipped_own_merge_in_flight",
      headerHash,
      reason,
      readyAfterUnixTime,
      nowUnixTime,
    } satisfies MergeTxResult,
  );
};

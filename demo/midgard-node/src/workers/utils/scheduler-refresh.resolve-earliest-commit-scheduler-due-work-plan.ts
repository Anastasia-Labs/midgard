import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SubmitSlotSnapshot } from "../../local-ledger-slot.js";
import { outRefLabel } from "../../tx-context.js";
import {
  type CommitSchedulerDiscoveryStage,
  type CommitSchedulerState,
  type CommitSchedulerStateQueueEvidence,
  type EarliestCommitSchedulerPlan,
  planEarliestCommitSchedulerDueWork,
} from "./commit-block-planner.js";
import {
  getOperatorKeyHash,
  getSchedulerDatumFromUTxO,
  requireExistingSchedulerWitnessUtxo,
} from "./scheduler-refresh.fetch-fresh-active-operator-input-for-commit.js";
import { resolveSchedulerRefreshValidityWindow } from "./scheduler-refresh.resolve-scheduler-refresh-witness-selection.js";
import {
  type ActiveSchedulerState,
  activeSchedulerState,
  SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
  schedulerSlotSnapshotFromSubmitSlot,
} from "./scheduler-refresh.scheduler-refresh-due-work-from-no-inline-submit-defer.js";

const activeSchedulerStateForEarliestCommitPlan = ({
  lucid,
  active,
  submitSlot,
}: {
  readonly lucid: LucidEvolution;
  readonly active: ActiveSchedulerState | undefined;
  readonly submitSlot?: SubmitSlotSnapshot;
}):
  | CommitSchedulerState
  | Extract<EarliestCommitSchedulerPlan, { status: "ambiguous" }> => {
  if (active === undefined) {
    return { status: "no_active_operators" };
  }
  const startTimeMs = Number(active.startTime);
  if (!Number.isSafeInteger(startTimeMs)) {
    return {
      status: "ambiguous",
      reason: "scheduler_active_start_time_unsafe",
    };
  }
  if (submitSlot === undefined) {
    return {
      status: "active",
      operatorKeyHash: active.operator,
      startTimeMs,
    };
  }
  const schedulerSlotSnapshot = schedulerSlotSnapshotFromSubmitSlot(
    lucid,
    submitSlot,
  );
  const { validFrom, validTo } = resolveSchedulerRefreshValidityWindow(
    lucid,
    active.startTime,
    schedulerSlotSnapshot,
  );
  const invalidBeforeSlot = Number(lucid.unixTimeToSlot(Number(validFrom)));
  const invalidHereafterSlot = Number(lucid.unixTimeToSlot(Number(validTo)));
  if (
    !Number.isSafeInteger(invalidBeforeSlot) ||
    !Number.isSafeInteger(invalidHereafterSlot)
  ) {
    return {
      status: "ambiguous",
      reason: "scheduler_transition_slot_unsafe",
    };
  }
  return {
    status: "active",
    operatorKeyHash: active.operator,
    startTimeMs,
    transitionInvalidBeforeSlot: invalidBeforeSlot,
    transitionInvalidHereafterSlot: invalidHereafterSlot,
  };
};

export const resolveEarliestCommitSchedulerDueWorkPlan = ({
  lucid,
  contracts,
  submitSlotSnapshot,
  stateQueueEvidence,
  localFinalizationPending,
  callerLabel,
  discoveryStage,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly submitSlotSnapshot: () => Effect.Effect<SubmitSlotSnapshot, unknown>;
  readonly stateQueueEvidence: CommitSchedulerStateQueueEvidence;
  readonly localFinalizationPending: boolean;
  readonly callerLabel: string;
  readonly discoveryStage: CommitSchedulerDiscoveryStage;
}): Effect.Effect<EarliestCommitSchedulerPlan, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const schedulerUtxos = (yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.scheduler.spendingScriptAddress,
      contracts.scheduler.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler UTxOs for earliest commit scheduler due-work planning",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);
    const schedulerRefInput = yield* requireExistingSchedulerWitnessUtxo(
      schedulerUtxos,
      schedulerWitnessUnit,
    );
    const schedulerDatum = yield* getSchedulerDatumFromUTxO(schedulerRefInput);
    const submitSlotEvidence = yield* Effect.either(submitSlotSnapshot());
    const schedulerState = activeSchedulerStateForEarliestCommitPlan({
      lucid,
      active: activeSchedulerState(schedulerDatum),
      submitSlot:
        submitSlotEvidence._tag === "Right"
          ? submitSlotEvidence.right
          : undefined,
    });
    if (schedulerState.status === "ambiguous") {
      return schedulerState;
    }
    return planEarliestCommitSchedulerDueWork({
      callerLabel,
      discoveryStage,
      schedulerOutRef: outRefLabel(schedulerRefInput),
      schedulerState,
      currentOperatorKeyHash: operatorKeyHash,
      submitSlotSnapshot:
        submitSlotEvidence._tag === "Right"
          ? submitSlotEvidence.right
          : undefined,
      submitSlotSnapshotError:
        submitSlotEvidence._tag === "Left"
          ? submitSlotEvidence.left
          : undefined,
      stateQueueEvidence,
      localFinalizationPending,
      maxInlineWaitMs: SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
    });
  });

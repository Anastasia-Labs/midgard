/**
 * Scheduler witness refresh and alignment helpers for block commitments.
 * The commit worker uses this module to read the real state_queue witness
 * context needed for scheduler-aligned, production-safe commit transactions.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data as LucidData,
  type LucidEvolution,
  paymentCredentialOf,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SlotAwareDueWork } from "../../fibers/slot-aware-due-work.js";
import type { SubmitSlotSnapshot } from "../../local-ledger-slot.js";
import { slotToUnixTimeForLucid } from "../../lucid-time.js";
import {
  applySubmittedTxToOperatorWalletView,
  availableOperatorWalletUtxos,
  fetchOperatorWalletView,
  type OperatorWalletView,
} from "../../operator-wallet-view.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../../transactions/reference-scripts.js";
import { planSubmitTiming } from "../../transactions/submit-timing.js";
import {
  slotAwareDueWorkFromSubmitTiming,
  type SubmitTimingNotDuePlanWithDueWorkEvidence,
} from "../../transactions/submit-timing-due-work.js";
import {
  awaitExactTransactionConfirmation,
  handleSignSubmitNoConfirmation,
  type NoInlineSubmitDefer,
  type NoInlineSubmitRecoveryOptions,
  type TxSignError,
  type TxSubmitError,
} from "../../transactions/utils.js";
import { compareOutRefs, outRefLabel } from "../../tx-context.js";
import {
  type CommitSchedulerDiscoveryStage,
  type CommitSchedulerState,
  type CommitSchedulerStateQueueEvidence,
  type CurrentOperatorSchedulerWindow,
  type EarliestCommitSchedulerPlan,
  planEarliestCommitSchedulerDueWork,
} from "./commit-block-planner.js";
import { alignUnixTimeToSlotBoundary } from "./commit-end-time.js";

export type NodeUtxoWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.LinkedListNodeView;
};

export type RealStateQueueWitnessContext = {
  readonly operatorKeyHash: string;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly correctionLockRefInput: SDK.CorrectionLockUTxO;
  readonly activeOperatorInput: UTxO & { datum: string };
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsSpendingScriptRef?: UTxO;
  readonly stateQueueSpendingScriptRef?: UTxO;
  readonly stateQueueMintingScriptRef?: UTxO;
  readonly stateQueueCommitYieldScriptRef: UTxO;
  readonly operatorWalletView: OperatorWalletView;
};

export type CommitTimingDueWork = {
  readonly type: "CommitTimingDueWork";
  readonly dueWork: SlotAwareDueWork;
};

export const schedulerRefreshDependencyKey = ({
  schedulerOutRef,
  currentOperator,
  currentStartTime,
  targetOperator,
  selectionKind,
}: {
  readonly schedulerOutRef: string;
  readonly currentOperator: string;
  readonly currentStartTime: bigint;
  readonly targetOperator: string;
  readonly selectionKind: SchedulerRefreshWitnessSelection["kind"];
}): string =>
  [
    `scheduler=${schedulerOutRef}`,
    `current_operator=${currentOperator}`,
    `current_start=${currentStartTime.toString()}`,
    `target_operator=${targetOperator}`,
    `selection=${selectionKind}`,
  ].join(",");

export const schedulerRefreshDueWorkFromSubmitTiming = (input: {
  readonly plan: SubmitTimingNotDuePlanWithDueWorkEvidence;
  readonly reason?: string;
}): CommitTimingDueWork => ({
  type: "CommitTimingDueWork",
  dueWork: slotAwareDueWorkFromSubmitTiming({
    kind: "commit_scheduler_refresh",
    key: "block_commitment",
    callerLabel: "scheduler-refresh",
    reason: input.reason ?? "scheduler_transition_not_reached",
    plan: input.plan,
  }),
});

export type SchedulerRefreshWitnessSelection =
  | {
      readonly kind: "Advance";
      readonly activeNode: NodeUtxoWithDatum;
    }
  | {
      readonly kind: "AppointFirst";
      readonly activeNode: NodeUtxoWithDatum;
      readonly registeredWitnessNode: NodeUtxoWithDatum;
    }
  | {
      readonly kind: "Rewind";
      readonly activeNode: NodeUtxoWithDatum;
      readonly activeRootNode: NodeUtxoWithDatum;
      readonly registeredWitnessNode: NodeUtxoWithDatum;
    };

type SchedulerAlignmentResult =
  | {
      readonly schedulerRefInput: UTxO;
      readonly operatorWalletView: OperatorWalletView;
    }
  | CommitTimingDueWork;

export type ActiveSchedulerState = {
  readonly operator: string;
  readonly startTime: bigint;
};

export type SchedulerRefreshStartTimeMode =
  | "validity-lower-bound"
  | "previous-shift-end";

const SCHEDULER_REFRESH_POLL_INTERVAL = "2 seconds";
const SCHEDULER_REFRESH_MAX_POLLS = 30;
const SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL = 96;
export const SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS = 5 * 60_000;
export const SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const SCHEDULER_SHIFT_DURATION_MS = SDK.SHIFT_DURATION_MS;
// Mirrors on-chain env.max_validity_range_length for scheduler spends.
const SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS = 8n * 60n * 1000n;
const SCHEDULER_REFRESH_VALID_FROM_BACKDATE_MS = 30_000;
const SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS = 30n * 1000n;
const SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS = 120_000;
const PREVIOUS_SHIFT_END_SCHEDULER_SPENDING_SCRIPT_HASHES = new Set([
  // Pre-2026-07-07 deployed preprod scheduler validators advance one shift at a
  // time and require output start_time == previous shift end.
  "adc0cb0642e888ec8f003ae71b5412bde1b31789c951597932f46712",
]);

export const schedulerRefreshStartTimeModeForSpendingScriptHash = (
  schedulerSpendingScriptHash: string,
): SchedulerRefreshStartTimeMode =>
  PREVIOUS_SHIFT_END_SCHEDULER_SPENDING_SCRIPT_HASHES.has(
    schedulerSpendingScriptHash.toLowerCase(),
  )
    ? "previous-shift-end"
    : "validity-lower-bound";

export const schedulerRefreshDueWorkFromNoInlineSubmitDefer = ({
  defer,
  localSubmitSlot,
  nowMs,
}: {
  readonly defer: NoInlineSubmitDefer;
  readonly localSubmitSlot?: SubmitSlotSnapshot;
  readonly nowMs: number;
}): CommitTimingDueWork => {
  const providerSlotWait = defer.kind === "provider_slot_wait";
  const observedSlot =
    providerSlotWait && localSubmitSlot !== undefined
      ? localSubmitSlot.currentSlot
      : defer.currentSlot;
  const slotLengthMs = Math.max(
    1,
    Math.floor(localSubmitSlot?.slotLengthMs ?? 1_000),
  );
  const dueSlot =
    providerSlotWait && localSubmitSlot !== undefined
      ? localSubmitSlot.currentSlot +
        Math.max(1, Math.ceil(defer.waitMs / slotLengthMs))
      : defer.dueSlot;
  return {
    type: "CommitTimingDueWork",
    dueWork: {
      kind: "commit_scheduler_refresh",
      key: "block_commitment",
      callerLabel: defer.callerLabel,
      reason: `scheduler_refresh_${defer.kind}_not_reached`,
      observedSlot,
      dueSlot,
      dueAtMs: nowMs + defer.waitMs,
      waitMs: defer.waitMs,
      slotSource:
        providerSlotWait && localSubmitSlot !== undefined
          ? localSubmitSlot.source
          : defer.slotSource,
      dependencyKey: defer.dependencyKey,
      invalidationKey: defer.invalidationKey,
    },
  };
};

export const schedulerRefreshRequiredOutsideMutationWorkerDueWork = ({
  submitTimingSnapshot,
  invalidBeforeSlot,
  invalidHereafterSlot,
  schedulerDependencyKey,
}: {
  readonly submitTimingSnapshot: SubmitSlotSnapshot;
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot: number;
  readonly schedulerDependencyKey: string;
}): CommitTimingDueWork =>
  schedulerRefreshDueWorkFromSubmitTiming({
    reason: "scheduler_refresh_required_outside_mutation_worker",
    plan: {
      status: "not_due",
      callerLabel: "scheduler-refresh",
      targetSlot: submitTimingSnapshot.currentSlot + 1,
      dueSlot: submitTimingSnapshot.currentSlot + 1,
      currentSlot: submitTimingSnapshot.currentSlot,
      observedSlot: submitTimingSnapshot.currentSlot,
      observedAtMs: submitTimingSnapshot.observedAtMs,
      deltaSlots: 1,
      waitMs: Math.max(1, submitTimingSnapshot.slotLengthMs),
      slotLengthMs: Math.max(1, submitTimingSnapshot.slotLengthMs),
      slotSource: submitTimingSnapshot.source,
      invalidBeforeSlot,
      ...(Number.isSafeInteger(invalidHereafterSlot)
        ? { invalidHereafterSlot }
        : {}),
      reason: "scheduler_refresh_required_outside_mutation_worker",
      dependencyKey: schedulerDependencyKey,
      invalidationKey: schedulerDependencyKey,
    },
  });

export type SchedulerSlotSnapshot = {
  readonly currentSlot: number;
  readonly currentSlotStartMs: number;
  readonly observedAtMs: number;
};

export const captureSchedulerSlotSnapshot = (
  lucid: LucidEvolution,
  observedAtMs: number = Date.now(),
  submitSlot?: SubmitSlotSnapshot,
): SchedulerSlotSnapshot => {
  if (submitSlot !== undefined) {
    return schedulerSlotSnapshotFromSubmitSlot(lucid, submitSlot);
  }
  const currentSlot = lucid.currentSlot();
  return {
    currentSlot,
    currentSlotStartMs:
      slotToUnixTimeForLucid(lucid, currentSlot) ?? observedAtMs,
    observedAtMs,
  };
};

export const schedulerSlotSnapshotFromSubmitSlot = (
  lucid: LucidEvolution,
  submitSlot: SubmitSlotSnapshot,
): SchedulerSlotSnapshot => ({
  currentSlot: submitSlot.currentSlot,
  currentSlotStartMs:
    slotToUnixTimeForLucid(lucid, submitSlot.currentSlot) ??
    submitSlot.observedAtMs,
  observedAtMs: submitSlot.observedAtMs,
});

const nodeKeyBytes = (key: SDK.NodeKey): string | undefined =>
  key === "Empty" ? undefined : key.Key.key;

const linkKeyBytes = (datum: SDK.LinkedListNodeView): string | undefined =>
  datum.next === "Empty" ? undefined : datum.next.Key.key;

const activeSchedulerState = (
  datum: SDK.SchedulerDatum,
): ActiveSchedulerState | undefined =>
  datum === "NoActiveOperators"
    ? undefined
    : {
        operator: datum.ActiveOperator.operator,
        startTime: BigInt(datum.ActiveOperator.start_time),
      };

const activeSchedulerDatum = (
  operator: string,
  startTime: bigint,
): SDK.SchedulerDatum => ({
  ActiveOperator: {
    operator,
    start_time: startTime,
  },
});

export const schedulerStateCoversCommitTarget = ({
  currentSchedulerState,
  operatorKeyHash,
  targetStartTime,
}: {
  readonly currentSchedulerState: ActiveSchedulerState | undefined;
  readonly operatorKeyHash: string;
  readonly targetStartTime: bigint;
}): boolean => {
  if (currentSchedulerState?.operator !== operatorKeyHash) {
    return false;
  }
  const currentShiftEndTime =
    currentSchedulerState.startTime + SCHEDULER_SHIFT_DURATION_MS;
  return (
    currentSchedulerState.startTime <= targetStartTime &&
    targetStartTime <= currentShiftEndTime
  );
};

const describeSchedulerDatum = (datum: SDK.SchedulerDatum): string => {
  const active = activeSchedulerState(datum);
  return active === undefined
    ? "NoActiveOperators"
    : `ActiveOperator(${active.operator},${active.startTime.toString()})`;
};

const findRootNode = (
  nodes: readonly NodeUtxoWithDatum[],
  label: string,
): NodeUtxoWithDatum => {
  const rootNode = nodes.find((node) => node.datum.key === "Empty");
  if (rootNode === undefined) {
    throw new Error(`${label} root node is missing`);
  }
  return rootNode;
};

const findMemberNode = (
  nodes: readonly NodeUtxoWithDatum[],
  key: string,
  label: string,
): NodeUtxoWithDatum => {
  const node = nodes.find(
    (candidate) => nodeKeyBytes(candidate.datum.key) === key,
  );
  if (node === undefined) {
    throw new Error(`${label} node for key ${key} was not found`);
  }
  return node;
};

const findLastMemberNode = (
  nodes: readonly NodeUtxoWithDatum[],
): NodeUtxoWithDatum | undefined =>
  nodes.find(
    (candidate) =>
      candidate.datum.key !== "Empty" && candidate.datum.next === "Empty",
  );

export const resolveSchedulerRefreshWitnessSelection = ({
  currentOperator,
  targetOperator,
  activeNodes,
  registeredNodes,
  allowGenesisRewind,
}: {
  readonly currentOperator: string;
  readonly targetOperator: string;
  readonly activeNodes: readonly NodeUtxoWithDatum[];
  readonly registeredNodes: readonly NodeUtxoWithDatum[];
  readonly allowGenesisRewind: boolean;
}): SchedulerRefreshWitnessSelection => {
  const targetNode = findMemberNode(
    activeNodes,
    targetOperator,
    "Active-operators",
  );
  const registeredWitnessNode =
    findLastMemberNode(registeredNodes) ??
    findRootNode(registeredNodes, "Registered-operators");

  if (allowGenesisRewind) {
    if (targetNode.datum.next !== "Empty") {
      throw new Error(
        `Operator ${targetOperator} cannot be appointed first because it is not the last active-operators node`,
      );
    }
    return {
      kind: "AppointFirst",
      activeNode: targetNode,
      registeredWitnessNode,
    };
  }

  if (linkKeyBytes(targetNode.datum) === currentOperator) {
    return {
      kind: "Advance",
      activeNode: targetNode,
    };
  }

  const activeRootNode = findRootNode(activeNodes, "Active-operators");
  const currentOperatorIsActiveHead =
    linkKeyBytes(activeRootNode.datum) === currentOperator;
  const targetNodeIsActiveTail = targetNode.datum.next === "Empty";
  if (!targetNodeIsActiveTail) {
    throw new Error(
      `Operator ${targetOperator} is not the next scheduled operator for current scheduler operator ${currentOperator}`,
    );
  }

  if (!currentOperatorIsActiveHead) {
    throw new Error(
      `Operator ${targetOperator} cannot rewind scheduler from current operator ${currentOperator}`,
    );
  }

  return {
    kind: "Rewind",
    activeNode: targetNode,
    activeRootNode,
    registeredWitnessNode,
  };
};

const toSdkSchedulerRefreshWitnessSelection = (
  selection: SchedulerRefreshWitnessSelection,
): SDK.SchedulerRefreshWitnessSelection => {
  switch (selection.kind) {
    case "Advance":
      return {
        kind: "Advance",
        activeNode: { utxo: selection.activeNode.utxo },
      };
    case "AppointFirst":
      return {
        kind: "AppointFirst",
        activeNode: { utxo: selection.activeNode.utxo },
        registeredWitnessNode: {
          utxo: selection.registeredWitnessNode.utxo,
        },
      };
    case "Rewind":
      return {
        kind: "Rewind",
        activeNode: { utxo: selection.activeNode.utxo },
        activeRootNode: { utxo: selection.activeRootNode.utxo },
        registeredWitnessNode: {
          utxo: selection.registeredWitnessNode.utxo,
        },
      };
  }
};

const parseNodeSetUtxos = (
  utxos: readonly UTxO[],
  label: string,
): Effect.Effect<readonly NodeUtxoWithDatum[], SDK.StateQueueError> =>
  Effect.forEach(utxos, (utxo) =>
    SDK.getLinkedListNodeViewFromUTxO(utxo).pipe(
      Effect.map((datum) => ({
        utxo,
        datum,
      })),
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: `Failed to decode ${label} node datum`,
            cause: `${outRefLabel(utxo)}: ${formatUnknownError(cause)}`,
          }),
      ),
    ),
  );

export const resolveSchedulerRefreshValidityWindow = (
  lucid: LucidEvolution,
  currentSchedulerStartTime: bigint,
  slotSnapshot: SchedulerSlotSnapshot = captureSchedulerSlotSnapshot(lucid),
): {
  readonly validFrom: bigint;
  readonly validTo: bigint;
} => {
  const minimumShiftStart = Number(
    currentSchedulerStartTime + SCHEDULER_SHIFT_DURATION_MS,
  );
  const submitLedgerBackdatedStart =
    slotSnapshot.currentSlotStartMs - SCHEDULER_REFRESH_VALID_FROM_BACKDATE_MS;
  let validFrom = alignUnixTimeToSlotBoundary(
    lucid,
    Math.max(submitLedgerBackdatedStart, minimumShiftStart),
  );
  if (validFrom < minimumShiftStart) {
    validFrom = alignUnixTimeToSlotBoundary(lucid, minimumShiftStart + 999);
  }
  return {
    validFrom: BigInt(validFrom),
    validTo: BigInt(validFrom) + SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS,
  };
};

export const resolveSchedulerFirstAppointmentValidityWindow = (
  lucid: LucidEvolution,
  targetCommitEndTime: bigint,
  slotSnapshot: SchedulerSlotSnapshot = captureSchedulerSlotSnapshot(lucid),
): {
  readonly validFrom: bigint;
  readonly validTo: bigint;
} => {
  const validFrom = BigInt(
    alignUnixTimeToSlotBoundary(
      lucid,
      Math.max(
        0,
        slotSnapshot.currentSlotStartMs -
          SCHEDULER_REFRESH_VALID_FROM_BACKDATE_MS,
      ),
    ),
  );
  if (validFrom >= targetCommitEndTime) {
    throw new Error(
      `Cannot appoint first scheduler operator because the target commit end-time is not in the future: valid_from=${validFrom.toString()},target_commit_end=${targetCommitEndTime.toString()}`,
    );
  }
  const maxRefreshValidTo = validFrom + SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS;
  const validTo =
    targetCommitEndTime < maxRefreshValidTo
      ? targetCommitEndTime
      : maxRefreshValidTo;
  if (
    targetCommitEndTime - validFrom <
      SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS ||
    validTo - validFrom < SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS
  ) {
    throw new Error(
      `Cannot appoint first scheduler operator because the target commit end-time leaves too little validity budget: valid_from=${validFrom.toString()},target_commit_end=${targetCommitEndTime.toString()},valid_to=${validTo.toString()},minimum_gap=${SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS.toString()}`,
    );
  }
  if (validTo - validFrom > SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS) {
    throw new Error(
      `Cannot appoint first scheduler operator with a validity range longer than the protocol maximum: valid_from=${validFrom.toString()},valid_to=${validTo.toString()}`,
    );
  }
  return { validFrom, validTo };
};

export const resolveRefreshedSchedulerStartTime = ({
  selection,
  currentSchedulerState,
  validFrom,
  validTo,
  startTimeMode = "validity-lower-bound",
}: {
  readonly selection: SchedulerRefreshWitnessSelection;
  readonly currentSchedulerState: ActiveSchedulerState | undefined;
  readonly validFrom: bigint;
  readonly validTo: bigint;
  readonly startTimeMode?: SchedulerRefreshStartTimeMode;
}): bigint => {
  if (selection.kind === "AppointFirst") {
    // Lucid's validTo is exclusive; Aiken sees the inclusive upper bound.
    return validTo - 1n;
  }
  if (currentSchedulerState === undefined) {
    throw new Error(
      "Cannot resolve end-of-shift scheduler start time without an active scheduler datum",
    );
  }
  const previousShiftEnd =
    currentSchedulerState.startTime + SCHEDULER_SHIFT_DURATION_MS;
  if (validFrom < previousShiftEnd) {
    throw new Error(
      `Cannot refresh scheduler before previous shift end: valid_from=${validFrom.toString()},previous_shift_end=${previousShiftEnd.toString()}`,
    );
  }
  if (startTimeMode === "previous-shift-end") {
    return previousShiftEnd;
  }
  return validFrom;
};

const awaitSubmittedSchedulerTx = (
  lucid: LucidEvolution,
  txHash: string,
  purpose: "refresh",
): Effect.Effect<void, SDK.StateQueueError> =>
  Effect.gen(function* () {
    yield* Effect.tryPromise({
      try: () =>
        awaitExactTransactionConfirmation(lucid, txHash, {
          timeout: SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS,
          checkInterval: SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS,
        }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed waiting for scheduler ${purpose} tx confirmation`,
          cause,
        }),
    });
  });

const getOperatorKeyHash = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const operatorAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const paymentCredential = paymentCredentialOf(operatorAddress);
    if (paymentCredential?.type !== "Key") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet does not have a key payment credential",
          cause: operatorAddress,
        }),
      );
    }
    return paymentCredential.hash;
  });

const selectActiveOperatorInput = (
  activeOperatorUtxos: readonly UTxO[],
  operatorKeyHash: string,
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    for (const utxo of activeOperatorUtxos) {
      const nodeDatumEither = yield* Effect.either(
        SDK.getLinkedListNodeViewFromUTxO(utxo),
      );
      if (nodeDatumEither._tag === "Left") {
        continue;
      }
      if (
        nodeDatumEither.right.key !== "Empty" &&
        nodeDatumEither.right.key.Key.key === operatorKeyHash
      ) {
        return utxo;
      }
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "No active-operators node for current operator key hash; cannot build real state_queue commit witness",
        cause: operatorKeyHash,
      }),
    );
  });

export const filterLocallyConsumedUtxos = (
  utxos: readonly UTxO[],
  consumedOutRefs: readonly string[],
): readonly UTxO[] => {
  const consumed = new Set(consumedOutRefs);
  return utxos.filter((utxo) => !consumed.has(outRefLabel(utxo)));
};

const fetchActiveOperatorUtxos = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  message: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  SDK.utxosAtByNFTPolicyId(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    contracts.activeOperators.policyId,
  ).pipe(
    Effect.map((beacons) => beacons.map((beacon) => beacon.utxo)),
    Effect.mapError(
      (cause) =>
        new SDK.StateQueueError({
          message,
          cause,
        }),
    ),
  );

const requireInlineActiveOperatorDatum = (
  activeOperatorInput: UTxO,
): Effect.Effect<UTxO & { datum: string }, SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (activeOperatorInput.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Active-operators UTxO must include inline datum for real state_queue commit",
          cause: `${activeOperatorInput.txHash}#${activeOperatorInput.outputIndex}`,
        }),
      );
    }
    return activeOperatorInput as UTxO & { datum: string };
  });

const fetchFreshActiveOperatorInputForCommit = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  operatorWalletView: OperatorWalletView,
): Effect.Effect<UTxO & { datum: string }, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const consumedOutRefs = new Set(operatorWalletView.consumedOutRefs);
    let lastCandidateLabels: readonly string[] = [];

    for (
      let pollCount = 0;
      pollCount < SCHEDULER_REFRESH_MAX_POLLS;
      pollCount += 1
    ) {
      const activeOperatorUtxos = yield* fetchActiveOperatorUtxos(
        lucid,
        contracts,
        "Failed to refresh active-operators UTxOs for state_queue commit",
      );
      lastCandidateLabels = activeOperatorUtxos.map(outRefLabel);
      const freshActiveOperatorUtxos = filterLocallyConsumedUtxos(
        activeOperatorUtxos,
        operatorWalletView.consumedOutRefs,
      );
      const activeOperatorInput = yield* Effect.either(
        selectActiveOperatorInput(freshActiveOperatorUtxos, operatorKeyHash),
      );
      if (activeOperatorInput._tag === "Right") {
        return yield* requireInlineActiveOperatorDatum(
          activeOperatorInput.right,
        );
      }

      const staleCandidateLabels = activeOperatorUtxos
        .filter((utxo) => consumedOutRefs.has(outRefLabel(utxo)))
        .map(outRefLabel);
      if (staleCandidateLabels.length === 0) {
        return yield* Effect.fail(activeOperatorInput.left);
      }
      if (pollCount === 0) {
        yield* Effect.logWarning(
          `Active-operators provider view still includes locally consumed outref(s) ${staleCandidateLabels.join(
            ",",
          )}; waiting for refreshed commit witness input.`,
        );
      }
      yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Timed out waiting for refreshed active-operators UTxO after scheduler refresh",
        cause: `operator=${operatorKeyHash},consumed_outrefs=${operatorWalletView.consumedOutRefs.join(
          ",",
        )},last_candidates=${lastCandidateLabels.join(",")}`,
      }),
    );
  });

const getSchedulerDatumFromUTxO = (
  schedulerUtxo: UTxO,
): Effect.Effect<SDK.SchedulerDatum, SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (schedulerUtxo.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Scheduler UTxO must include inline datum",
          cause: `${schedulerUtxo.txHash}#${schedulerUtxo.outputIndex}`,
        }),
      );
    }
    const schedulerDatum = schedulerUtxo.datum;
    return yield* Effect.try({
      try: () =>
        LucidData.from(
          schedulerDatum,
          SDK.SchedulerDatum as never,
        ) as SDK.SchedulerDatum,
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to decode scheduler datum",
          cause,
        }),
    });
  });

export const requireExistingSchedulerWitnessUtxo = (
  schedulerUtxos: readonly UTxO[],
  schedulerWitnessUnit: string,
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const existingWitness = [...schedulerUtxos]
      .filter((utxo) => (utxo.assets[schedulerWitnessUnit] ?? 0n) > 0n)
      .sort(compareOutRefs)[0];
    if (existingWitness !== undefined) {
      return existingWitness;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Incomplete protocol deployment: scheduler root UTxO is missing; refusing commit-time scheduler minting",
        cause: `unit=${schedulerWitnessUnit}`,
      }),
    );
  });

export const resolveCurrentOperatorSchedulerWindow = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  CurrentOperatorSchedulerWindow | undefined,
  SDK.StateQueueError
> =>
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
              "Failed to fetch scheduler UTxOs for scheduler-aware commit planning",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);
    const schedulerRefInput = yield* requireExistingSchedulerWitnessUtxo(
      schedulerUtxos,
      schedulerWitnessUnit,
    );
    const schedulerDatum = yield* getSchedulerDatumFromUTxO(schedulerRefInput);
    const active = activeSchedulerState(schedulerDatum);
    if (active?.operator !== operatorKeyHash) {
      return undefined;
    }
    const startTimeMs = Number(active.startTime);
    const endTimeMs = Number(active.startTime + SCHEDULER_SHIFT_DURATION_MS);
    if (
      !Number.isSafeInteger(startTimeMs) ||
      !Number.isSafeInteger(endTimeMs)
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Current scheduler window is outside the safe JavaScript time range",
          cause: `start=${active.startTime.toString()},duration=${SCHEDULER_SHIFT_DURATION_MS.toString()}`,
        }),
      );
    }
    return {
      schedulerOutRef: outRefLabel(schedulerRefInput),
      operatorKeyHash,
      startTimeMs,
      endTimeMs,
    } satisfies CurrentOperatorSchedulerWindow;
  });

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

const ensureSchedulerAlignedForCommit = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  schedulerRefInput: UTxO,
  activeOperatorUtxos: readonly UTxO[],
  registeredOperatorUtxos: readonly UTxO[],
  alignedEndTime: number,
  schedulerWitnessUnit: string,
  operatorWalletView?: OperatorWalletView,
  schedulerSpendingScriptRef?: UTxO,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
  allowSchedulerRefresh: boolean = true,
): Effect.Effect<
  SchedulerAlignmentResult,
  SDK.StateQueueError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const resolveOperatorWalletView = (
      walletView?: OperatorWalletView,
    ): Effect.Effect<OperatorWalletView, SDK.StateQueueError> =>
      walletView === undefined
        ? Effect.tryPromise({
            try: () => fetchOperatorWalletView(lucid),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to initialize operator wallet view for scheduler alignment",
                cause,
              }),
          })
        : Effect.succeed(walletView);
    const targetStartTime = BigInt(alignedEndTime);
    const activeNodes = yield* parseNodeSetUtxos(
      activeOperatorUtxos,
      "active-operators",
    );
    const registeredNodes = yield* parseNodeSetUtxos(
      registeredOperatorUtxos,
      "registered-operators",
    );
    let currentSchedulerRefInput = schedulerRefInput;
    let currentOperatorWalletView = operatorWalletView;
    const startTimeMode = schedulerRefreshStartTimeModeForSpendingScriptHash(
      contracts.scheduler.spendingScriptHash,
    );

    for (
      let refreshCount = 0;
      refreshCount <= SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL;
      refreshCount += 1
    ) {
      const schedulerDatum = yield* getSchedulerDatumFromUTxO(
        currentSchedulerRefInput,
      );
      const currentSchedulerState = activeSchedulerState(schedulerDatum);
      if (
        schedulerStateCoversCommitTarget({
          currentSchedulerState,
          operatorKeyHash,
          targetStartTime,
        })
      ) {
        return {
          schedulerRefInput: currentSchedulerRefInput,
          operatorWalletView: yield* resolveOperatorWalletView(
            currentOperatorWalletView,
          ),
        };
      }
      if (refreshCount === SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Scheduler alignment exceeded the maximum refresh count for one block commitment attempt",
            cause: `max_refreshes=${SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL.toString()},target_commit_end=${targetStartTime.toString()},current_scheduler=${describeSchedulerDatum(schedulerDatum)}`,
          }),
        );
      }

      const currentOperator = currentSchedulerState?.operator ?? "";
      const currentStartTime = currentSchedulerState?.startTime ?? 0n;
      const allowGenesisRewind = currentSchedulerState === undefined;
      const submitSlot =
        submitSlotSnapshot === undefined
          ? undefined
          : yield* submitSlotSnapshot().pipe(
              Effect.mapError(
                (cause) =>
                  new SDK.StateQueueError({
                    message:
                      "Failed to capture local Ogmios slot for scheduler refresh validity",
                    cause,
                  }),
              ),
            );
      const schedulerSlotSnapshot =
        submitSlot === undefined
          ? captureSchedulerSlotSnapshot(lucid)
          : schedulerSlotSnapshotFromSubmitSlot(lucid, submitSlot);
      const selection = yield* Effect.try({
        try: () =>
          resolveSchedulerRefreshWitnessSelection({
            currentOperator,
            targetOperator: operatorKeyHash,
            activeNodes,
            registeredNodes,
            allowGenesisRewind,
          }),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Current operator is not eligible to advance or rewind the scheduler for this commit window",
            cause,
          }),
      });
      const { validFrom, validTo } =
        selection.kind === "AppointFirst"
          ? yield* Effect.try({
              try: () =>
                resolveSchedulerFirstAppointmentValidityWindow(
                  lucid,
                  targetStartTime,
                  schedulerSlotSnapshot,
                ),
              catch: (cause) =>
                new SDK.StateQueueError({
                  message:
                    "Failed to resolve scheduler first-appointment validity window",
                  cause,
                }),
            })
          : resolveSchedulerRefreshValidityWindow(
              lucid,
              currentStartTime,
              schedulerSlotSnapshot,
            );
      const previousShiftCatchUp =
        startTimeMode === "previous-shift-end" &&
        currentStartTime <= targetStartTime;
      if (targetStartTime < validFrom && !previousShiftCatchUp) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Resolved commit end-time falls before the scheduler refresh window",
            cause: `commit_end=${targetStartTime.toString()},scheduler_valid_from=${validFrom.toString()}`,
          }),
        );
      }
      const submitTimingSnapshot: SubmitSlotSnapshot = submitSlot ?? {
        source: "test",
        currentSlot: schedulerSlotSnapshot.currentSlot,
        observedAtMs: schedulerSlotSnapshot.observedAtMs,
        slotLengthMs: 1_000,
      };
      const invalidBeforeSlot = Number(lucid.unixTimeToSlot(Number(validFrom)));
      const invalidHereafterSlot = Number(
        lucid.unixTimeToSlot(Number(validTo)),
      );
      const schedulerDependencyKey = schedulerRefreshDependencyKey({
        schedulerOutRef: outRefLabel(currentSchedulerRefInput),
        currentOperator,
        currentStartTime,
        targetOperator: operatorKeyHash,
        selectionKind: selection.kind,
      });
      const submitTiming = planSubmitTiming({
        callerLabel: "scheduler-refresh",
        invalidBeforeSlot,
        invalidHereafterSlot,
        slotSnapshot: submitTimingSnapshot,
        maxInlineWaitMs: SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
        inlineWaitPolicy: "defer_positive_wait",
        dependencyKey: schedulerDependencyKey,
        invalidationKey: schedulerDependencyKey,
      });
      if (submitTiming.status === "not_due") {
        if (
          submitTiming.dependencyKey === undefined ||
          submitTiming.invalidationKey === undefined
        ) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Scheduler refresh due-work planning lost dependency evidence",
              cause: `status=not_due,dependency_key=${submitTiming.dependencyKey ?? "missing"},invalidation_key=${submitTiming.invalidationKey ?? "missing"}`,
            }),
          );
        }
        const dueWorkOutput = schedulerRefreshDueWorkFromSubmitTiming({
          plan: {
            ...submitTiming,
            dependencyKey: submitTiming.dependencyKey,
            invalidationKey: submitTiming.invalidationKey,
          },
        });
        const entry = dueWorkOutput.dueWork;
        yield* Effect.logInfo(
          `🔹 Scheduler refresh is not due yet; registering slot-aware due work discovery_stage=scheduler_refresh_deep (kind=${entry.kind},key=${entry.key},current_slot=${entry.observedSlot.toString()},due_slot=${entry.dueSlot.toString()},wait_ms=${entry.waitMs.toString()},slot_source=${entry.slotSource},dependency_key=${entry.dependencyKey}).`,
        );
        return dueWorkOutput;
      }
      if (!allowSchedulerRefresh) {
        const dueWorkOutput =
          schedulerRefreshRequiredOutsideMutationWorkerDueWork({
            submitTimingSnapshot,
            invalidBeforeSlot,
            invalidHereafterSlot,
            schedulerDependencyKey,
          });
        yield* Effect.logInfo(
          `🔹 Scheduler refresh is required inside the commit mutation worker; deferring so refresh can run before COMMIT_WORKER_ACTIVE and block_commitment lease (kind=${dueWorkOutput.dueWork.kind},key=${dueWorkOutput.dueWork.key},current_slot=${dueWorkOutput.dueWork.observedSlot.toString()},due_slot=${dueWorkOutput.dueWork.dueSlot.toString()},wait_ms=${dueWorkOutput.dueWork.waitMs.toString()},dependency_key=${dueWorkOutput.dueWork.dependencyKey}).`,
        );
        return dueWorkOutput;
      }
      if (submitTiming.status !== "ready" && submitTiming.status !== "wait") {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Scheduler refresh validity planning failed before transaction build",
            cause: `status=${submitTiming.status},current_slot=${"currentSlot" in submitTiming ? submitTiming.currentSlot.toString() : "none"},invalid_before_slot=${invalidBeforeSlot.toString()},invalid_hereafter_slot=${invalidHereafterSlot.toString()}`,
          }),
        );
      }
      const refreshedSchedulerStartTime = yield* Effect.try({
        try: () =>
          resolveRefreshedSchedulerStartTime({
            selection,
            currentSchedulerState,
            validFrom,
            validTo,
            startTimeMode,
          }),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to resolve refreshed scheduler start time",
            cause,
          }),
      });
      const refreshedSchedulerDatum = activeSchedulerDatum(
        operatorKeyHash,
        refreshedSchedulerStartTime,
      );
      if (selection.kind !== "Advance") {
        const registeredWitness = selection.registeredWitnessNode;
        if (registeredWitness.datum.key !== "Empty") {
          const activationKey = registeredWitness.datum.key.Key.key;
          const activationTime = BigInt(
            `0x${activationKey === "" ? "0" : activationKey}`,
          );
          if (validTo >= activationTime) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  "Scheduler rewind window overlaps the next registered operator activation time",
                cause: `valid_to=${validTo.toString()},activation_time=${activationTime.toString()},registered_witness=${outRefLabel(registeredWitness.utxo)}`,
              }),
            );
          }
        }
      }
      const flowOperatorWalletView = yield* resolveOperatorWalletView(
        currentOperatorWalletView,
      );
      const presetWalletInputs = yield* SDK.requireOperatorWalletInputs(
        availableOperatorWalletUtxos(flowOperatorWalletView),
        "scheduler refresh tx",
      );
      yield* Effect.logInfo(
        `🔹 Refreshing scheduler witness datum for commit window via ${selection.kind} (attempt=${(
          refreshCount + 1
        ).toString()}/${SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL.toString()}, from=${describeSchedulerDatum(schedulerDatum)} to=${describeSchedulerDatum(refreshedSchedulerDatum)}, target=${targetStartTime.toString()}, validTo=${validTo.toString()}).`,
      );
      const refreshTxResult = yield* SDK.buildUnsignedSchedulerRefreshTxProgram(
        {
          lucid,
          scheduler: contracts.scheduler,
          operatorKeyHash,
          presetWalletInputs,
          schedulerInput: currentSchedulerRefInput,
          refreshedDatum: refreshedSchedulerDatum,
          validFrom,
          validTo,
          selection: toSdkSchedulerRefreshWitnessSelection(selection),
          schedulerSpendingScriptRef,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: `Failed to build scheduler refresh transaction through SDK: ${formatUnknownError(
                cause,
                { includeCause: true },
              )}`,
              cause,
            }),
        ),
      );
      const refreshTx = refreshTxResult.tx;

      const submitRecoveryOptions: NoInlineSubmitRecoveryOptions = {
        label: "scheduler-refresh",
        slotSnapshot: submitSlotSnapshot,
        requireSlotForBoundedTx: submitSlotSnapshot !== undefined,
        maxPreSubmitWaitMs: SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
        inlineWaitPolicy: "defer_positive_wait",
        noInlineSubmitDefer: {
          key: "block_commitment",
          dependencyKey: schedulerDependencyKey,
          invalidationKey: schedulerDependencyKey,
        },
      };
      const refreshSubmitResult = yield* handleSignSubmitNoConfirmation(
        lucid,
        refreshTx,
        submitRecoveryOptions,
      );
      if (refreshSubmitResult.status === "deferred") {
        return schedulerRefreshDueWorkFromNoInlineSubmitDefer({
          defer: refreshSubmitResult.defer,
          localSubmitSlot: submitSlot,
          nowMs: Date.now(),
        });
      }
      const refreshTxHash = refreshSubmitResult.txHash;
      const refreshedOperatorWalletView = applySubmittedTxToOperatorWalletView(
        flowOperatorWalletView,
        refreshTx.toTransaction(),
        refreshTxHash,
      );
      yield* Effect.logInfo(
        `🔹 Scheduler refresh transaction submitted: ${refreshTxHash}`,
      );
      yield* Effect.logInfo(
        `🔹 Scheduler refresh tx updated operator wallet view: available_utxos=${refreshedOperatorWalletView.knownUtxos.length.toString()},consumed_outrefs=${refreshedOperatorWalletView.consumedOutRefs.length.toString()}.`,
      );
      yield* awaitSubmittedSchedulerTx(lucid, refreshTxHash, "refresh");

      let refreshedSchedulerRefInput: UTxO | undefined;
      let pollCount = 0;
      while (pollCount < SCHEDULER_REFRESH_MAX_POLLS) {
        const schedulerWitnessUtxos = yield* Effect.tryPromise({
          try: () =>
            lucid.utxosAtWithUnit(
              contracts.scheduler.spendingScriptAddress,
              schedulerWitnessUnit,
            ),
          catch: (cause) =>
            new SDK.StateQueueError({
              message:
                "Failed to fetch scheduler witness UTxOs while waiting for scheduler refresh",
              cause,
            }),
        });
        for (const utxo of [...schedulerWitnessUtxos].sort(compareOutRefs)) {
          const utxoDatumEither = yield* Effect.either(
            getSchedulerDatumFromUTxO(utxo),
          );
          if (utxoDatumEither._tag === "Left") {
            continue;
          }
          const active = activeSchedulerState(utxoDatumEither.right);
          if (
            active?.operator === operatorKeyHash &&
            active.startTime === refreshedSchedulerStartTime
          ) {
            refreshedSchedulerRefInput = utxo;
            break;
          }
        }
        if (refreshedSchedulerRefInput !== undefined) {
          break;
        }

        pollCount += 1;
        yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
      }

      if (refreshedSchedulerRefInput === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Timed out waiting for refreshed scheduler UTxO to appear on-chain",
            cause: refreshTxHash,
          }),
        );
      }

      currentSchedulerRefInput = refreshedSchedulerRefInput;
      currentOperatorWalletView = refreshedOperatorWalletView;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "Scheduler alignment loop exited unexpectedly",
        cause: `target_commit_end=${targetStartTime.toString()}`,
      }),
    );
  });

export const fetchRealStateQueueWitnessContext = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  alignedEndTime: number,
  operatorWalletView?: OperatorWalletView,
  referenceScriptsAddress?: string,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
  allowSchedulerRefresh: boolean = true,
): Effect.Effect<
  RealStateQueueWitnessContext | CommitTimingDueWork,
  SDK.StateQueueError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const resolvedReferenceScripts =
      referenceScriptsAddress === undefined
        ? []
        : yield* fetchReferenceScriptUtxosProgram(
            lucid,
            referenceScriptsAddress,
            [
              {
                name: "scheduler spending",
                script: contracts.scheduler.spendingScript,
              },
              {
                name: "active-operators spending",
                script: contracts.activeOperators.spendingScript,
              },
              {
                name: "state-queue spending",
                script: contracts.stateQueue.spendingScript,
              },
              {
                name: "state-queue minting",
                script: contracts.stateQueue.mintingScript,
              },
              {
                name: "state-queue commit withdrawal",
                script: contracts.stateQueue.yields.commit.withdrawalScript,
              },
            ],
            contracts.referenceScriptAuth,
          );
    const optionalReferenceScript = (name: string): UTxO | undefined =>
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(resolvedReferenceScripts, name);
    const schedulerSpendingScriptRef =
      optionalReferenceScript("scheduler spending");
    const activeOperatorsSpendingScriptRef = optionalReferenceScript(
      "active-operators spending",
    );
    const stateQueueSpendingScriptRef = optionalReferenceScript(
      "state-queue spending",
    );
    const stateQueueMintingScriptRef = optionalReferenceScript(
      "state-queue minting",
    );
    const stateQueueCommitYieldScriptRef = referenceScriptByName(
      resolvedReferenceScripts,
      "state-queue commit withdrawal",
    );
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const activeOperatorUtxosForRefresh = yield* fetchActiveOperatorUtxos(
      lucid,
      contracts,
      "Failed to fetch active-operators UTxOs for state_queue commit",
    );
    const registeredOperatorUtxos = (yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      contracts.registeredOperators.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch registered-operators UTxOs for scheduler refresh",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);

    const schedulerUtxos = (yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.scheduler.spendingScriptAddress,
      contracts.scheduler.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to fetch scheduler UTxOs for state_queue commit",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);
    const initialSchedulerRefInput = yield* requireExistingSchedulerWitnessUtxo(
      schedulerUtxos,
      schedulerWitnessUnit,
    );
    const schedulerRefInput = yield* ensureSchedulerAlignedForCommit(
      lucid,
      contracts,
      operatorKeyHash,
      initialSchedulerRefInput,
      activeOperatorUtxosForRefresh,
      registeredOperatorUtxos,
      alignedEndTime,
      schedulerWitnessUnit,
      operatorWalletView,
      schedulerSpendingScriptRef,
      submitSlotSnapshot,
      allowSchedulerRefresh,
    );
    if ("dueWork" in schedulerRefInput) {
      return schedulerRefInput;
    }
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to resolve Cardano network for hub-oracle witness lookup",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    const hubOracleWitnessUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to fetch hub-oracle UTxOs for state_queue commit witness",
          cause,
        }),
    });
    if (hubOracleWitnessUtxos.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to resolve unique hub-oracle UTxO for state_queue commit witness",
          cause: `expected=1,found=${hubOracleWitnessUtxos.length},address=${hubOracleAddress},unit=${hubOracleUnit}`,
        }),
      );
    }
    const hubOracleRefInput = hubOracleWitnessUtxos[0];
    const correctionLockRefInput = yield* SDK.fetchCorrectionLockUTxOProgram(
      lucid,
      {
        correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
        hubOraclePolicyId: contracts.hubOracle.policyId,
      },
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch authenticated correction-lock witness for state_queue commit",
            cause,
          }),
      ),
    );

    const activeOperatorInput = yield* fetchFreshActiveOperatorInputForCommit(
      lucid,
      contracts,
      operatorKeyHash,
      schedulerRefInput.operatorWalletView,
    );

    return {
      operatorKeyHash,
      schedulerRefInput: schedulerRefInput.schedulerRefInput,
      hubOracleRefInput,
      correctionLockRefInput,
      activeOperatorInput,
      activeOperatorsSpendingScript: contracts.activeOperators.spendingScript,
      activeOperatorsSpendingScriptRef,
      stateQueueSpendingScriptRef,
      stateQueueMintingScriptRef,
      stateQueueCommitYieldScriptRef,
      operatorWalletView: schedulerRefInput.operatorWalletView,
    };
  });

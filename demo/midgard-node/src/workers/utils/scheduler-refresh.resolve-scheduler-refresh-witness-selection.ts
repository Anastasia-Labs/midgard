import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { awaitExactTransactionConfirmation } from "../../transactions/utils.js";
import { outRefLabel } from "../../tx-context.js";
import { alignUnixTimeToSlotBoundary } from "./commit-end-time.js";
import {
  type ActiveSchedulerState,
  activeSchedulerState,
  captureSchedulerSlotSnapshot,
  linkKeyBytes,
  nodeKeyBytes,
  type NodeUtxoWithDatum,
  SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS,
  SCHEDULER_REFRESH_VALID_FROM_BACKDATE_MS,
  SCHEDULER_SHIFT_DURATION_MS,
  SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS,
  SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS,
  SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS,
  type SchedulerRefreshStartTimeMode,
  type SchedulerRefreshWitnessSelection,
  type SchedulerSlotSnapshot,
} from "./scheduler-refresh.scheduler-refresh-due-work-from-no-inline-submit-defer.js";

export const describeSchedulerDatum = (datum: SDK.SchedulerDatum): string => {
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

export const toSdkSchedulerRefreshWitnessSelection = (
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

export const parseNodeSetUtxos = (
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
    // scheduler.ak requires the inclusive lower bound, which the ledger
    // presents as the start of its slot, to be at or after the shift end.
    validFrom = Number(
      SDK.slotAlignedLowerBoundAtOrAfter(lucid, BigInt(minimumShiftStart)),
    );
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
  // AppointFirstOperator binds the appointed start_time to the inclusive upper
  // bound the ledger presents, and the ledger carries that bound as a slot:
  // Lucid floors a millisecond validTo to its enclosing slot. The commit target
  // is wall-clock derived and generally falls mid-slot, so validTo must be the
  // slot boundary at or before it; otherwise `validTo - 1` names a time the
  // on-chain range never contains and the validator refuses the appointment.
  const validTo = BigInt(
    alignUnixTimeToSlotBoundary(
      lucid,
      Number(
        targetCommitEndTime < maxRefreshValidTo
          ? targetCommitEndTime
          : maxRefreshValidTo,
      ),
    ),
  );
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
    // Lucid's validTo is exclusive; Aiken sees the inclusive upper bound. This
    // holds only because resolveSchedulerFirstAppointmentValidityWindow puts
    // validTo on a slot boundary.
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

export const awaitSubmittedSchedulerTx = (
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

/**
 * Scheduler witness refresh and alignment helpers for block commitments.
 * The commit worker uses this module to read the real state_queue witness
 * context needed for scheduler-aligned, production-safe commit transactions.
 */
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

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { slotToUnixTimeForLucid } from "@/lucid-time.js";
import {
  applySubmittedTxToOperatorWalletView,
  availableOperatorWalletUtxos,
  fetchOperatorWalletView,
  type OperatorWalletView,
} from "@/operator-wallet-view.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";
import {
  handleSignSubmitNoConfirmation,
  type TxSignError,
  type TxSubmitError,
} from "@/transactions/utils.js";
import { compareOutRefs, outRefLabel } from "@/tx-context.js";
import { alignUnixTimeToSlotBoundary } from "@/workers/utils/commit-end-time.js";

export type NodeUtxoWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.LinkedListNodeView;
};

export type RealStateQueueWitnessContext = {
  readonly operatorKeyHash: string;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO & { datum: string };
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsSpendingScriptRef?: UTxO;
  readonly stateQueueSpendingScriptRef?: UTxO;
  readonly stateQueueMintingScriptRef?: UTxO;
  readonly operatorWalletView: OperatorWalletView;
};

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

type SchedulerAlignmentResult = {
  readonly schedulerRefInput: UTxO;
  readonly operatorWalletView: OperatorWalletView;
};

type ActiveSchedulerState = {
  readonly operator: string;
  readonly startTime: bigint;
};

const SCHEDULER_REFRESH_POLL_INTERVAL = "2 seconds";
const SCHEDULER_REFRESH_MAX_POLLS = 30;
const SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS = 90_000;
const SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const SCHEDULER_SHIFT_DURATION_MS = SDK.SHIFT_DURATION_MS;
// Mirrors on-chain env.max_validity_range_length for scheduler spends.
const SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS = 8n * 60n * 1000n;
const SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS = 30n * 1000n;

export type SchedulerSlotSnapshot = {
  readonly currentSlot: number;
  readonly currentSlotStartMs: number;
  readonly observedAtMs: number;
};

export const captureSchedulerSlotSnapshot = (
  lucid: LucidEvolution,
  observedAtMs: number = Date.now(),
): SchedulerSlotSnapshot => {
  const currentSlot = lucid.currentSlot();
  return {
    currentSlot,
    currentSlotStartMs:
      slotToUnixTimeForLucid(lucid, currentSlot) ?? observedAtMs,
    observedAtMs,
  };
};

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

export const selectFeeInput = (
  walletUtxos: readonly UTxO[],
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const sorted = [...walletUtxos].sort((a, b) => {
      const lovelaceA = a.assets.lovelace ?? 0n;
      const lovelaceB = b.assets.lovelace ?? 0n;
      if (lovelaceA === lovelaceB) {
        return compareOutRefs(a, b);
      }
      return lovelaceA > lovelaceB ? -1 : 1;
    });
    const feeInput = sorted[0];
    if (feeInput === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "No wallet UTxO available to fund real state_queue commit tx",
          cause: "empty wallet",
        }),
      );
    }
    return feeInput;
  });

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

const resolveSchedulerRefreshValidityWindow = (
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
  let validFrom = alignUnixTimeToSlotBoundary(
    lucid,
    Math.max(slotSnapshot.currentSlotStartMs, minimumShiftStart),
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
    alignUnixTimeToSlotBoundary(lucid, slotSnapshot.currentSlotStartMs),
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

const resolveRefreshedSchedulerStartTime = ({
  selection,
  currentSchedulerState,
  validTo,
}: {
  readonly selection: SchedulerRefreshWitnessSelection;
  readonly currentSchedulerState: ActiveSchedulerState | undefined;
  readonly validTo: bigint;
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
  return currentSchedulerState.startTime + SCHEDULER_SHIFT_DURATION_MS;
};

const awaitSubmittedSchedulerTx = (
  lucid: LucidEvolution,
  txHash: string,
  purpose: "refresh",
): Effect.Effect<void, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const confirmed = yield* Effect.tryPromise({
      try: () =>
        new Promise<boolean>((resolve, reject) => {
          const timeoutId = setTimeout(() => {
            reject(
              new Error(
                `scheduler ${purpose} confirmation timeout after ${SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS}ms`,
              ),
            );
          }, SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS);
          lucid
            .awaitTx(txHash, SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS)
            .then((result) => {
              clearTimeout(timeoutId);
              resolve(result);
            })
            .catch((error) => {
              clearTimeout(timeoutId);
              reject(error);
            });
        }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed waiting for scheduler ${purpose} tx confirmation`,
          cause,
        }),
    });
    if (!confirmed) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Scheduler ${purpose} tx did not confirm`,
          cause: txHash,
        }),
      );
    }
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
): Effect.Effect<
  SchedulerAlignmentResult,
  SDK.StateQueueError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const flowOperatorWalletView =
      operatorWalletView ??
      (yield* Effect.tryPromise({
        try: () => fetchOperatorWalletView(lucid),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to initialize operator wallet view for scheduler alignment",
            cause,
          }),
      }));
    const targetStartTime = BigInt(alignedEndTime);
    const schedulerDatum = yield* getSchedulerDatumFromUTxO(schedulerRefInput);
    const currentSchedulerState = activeSchedulerState(schedulerDatum);
    const currentShiftEndTime =
      currentSchedulerState === undefined
        ? 0n
        : currentSchedulerState.startTime + SCHEDULER_SHIFT_DURATION_MS;
    if (
      currentSchedulerState?.operator === operatorKeyHash &&
      currentSchedulerState.startTime <= targetStartTime &&
      targetStartTime <= currentShiftEndTime
    ) {
      return {
        schedulerRefInput,
        operatorWalletView: flowOperatorWalletView,
      };
    }
    const activeNodes = yield* parseNodeSetUtxos(
      activeOperatorUtxos,
      "active-operators",
    );
    const registeredNodes = yield* parseNodeSetUtxos(
      registeredOperatorUtxos,
      "registered-operators",
    );
    const currentOperator = currentSchedulerState?.operator ?? "";
    const currentStartTime = currentSchedulerState?.startTime ?? 0n;
    const allowGenesisRewind = currentSchedulerState === undefined;
    const schedulerSlotSnapshot = captureSchedulerSlotSnapshot(lucid);
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
    if (targetStartTime < validFrom) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Resolved commit end-time falls before the scheduler refresh window",
          cause: `commit_end=${targetStartTime.toString()},scheduler_valid_from=${validFrom.toString()}`,
        }),
      );
    }
    const refreshedSchedulerStartTime = yield* Effect.try({
      try: () =>
        resolveRefreshedSchedulerStartTime({
          selection,
          currentSchedulerState,
          validTo,
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
    const presetWalletInputs =
      availableOperatorWalletUtxos(flowOperatorWalletView);
    const feeInput = yield* selectFeeInput(presetWalletInputs);
    yield* Effect.logInfo(
      `🔹 Refreshing scheduler witness datum for commit window via ${selection.kind} (from=${describeSchedulerDatum(schedulerDatum)} to=${describeSchedulerDatum(refreshedSchedulerDatum)}, validTo=${validTo.toString()}).`,
    );
    const refreshTxResult = yield* SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid,
      scheduler: contracts.scheduler,
      operatorKeyHash,
      feeInput,
      presetWalletInputs,
      schedulerInput: schedulerRefInput,
      refreshedDatum: refreshedSchedulerDatum,
      validFrom,
      validTo,
      selection: toSdkSchedulerRefreshWitnessSelection(selection),
      schedulerSpendingScriptRef,
    }).pipe(
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

    const refreshTxHash = yield* handleSignSubmitNoConfirmation(
      lucid,
      refreshTx,
    );
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
          return {
            schedulerRefInput: utxo,
            operatorWalletView: refreshedOperatorWalletView,
          };
        }
      }

      pollCount += 1;
      yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Timed out waiting for refreshed scheduler UTxO to appear on-chain",
        cause: refreshTxHash,
      }),
    );
  });

export const fetchRealStateQueueWitnessContext = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  alignedEndTime: number,
  operatorWalletView?: OperatorWalletView,
  referenceScriptsAddress?: string,
): Effect.Effect<
  RealStateQueueWitnessContext,
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
    );
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
      activeOperatorInput,
      activeOperatorsSpendingScript: contracts.activeOperators.spendingScript,
      activeOperatorsSpendingScriptRef,
      stateQueueSpendingScriptRef,
      stateQueueMintingScriptRef,
      operatorWalletView: schedulerRefInput.operatorWalletView,
    };
  });

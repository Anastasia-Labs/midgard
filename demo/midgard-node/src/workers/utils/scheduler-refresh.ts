/**
 * Scheduler witness refresh and alignment helpers for block commitments.
 * The commit worker uses this module to read the real state_queue witness
 * context needed for scheduler-aligned, production-safe commit transactions.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  Data as LucidData,
  type LucidEvolution,
  type Script,
  type UTxO,
  credentialToAddress,
  paymentCredentialOf,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  handleSignSubmitNoConfirmation,
  type TxSignError,
  type TxSubmitError,
} from "@/transactions/utils.js";
import { formatUnknownError } from "@/error-format.js";
import { slotToUnixTimeForLucid } from "@/lucid-time.js";
import {
  availableOperatorWalletUtxos,
  applySubmittedTxToOperatorWalletView,
  fetchOperatorWalletView,
  type OperatorWalletView,
} from "@/operator-wallet-view.js";
import {
  compareOutRefs,
  outRefLabel,
} from "@/tx-context.js";
import { alignUnixTimeToSlotBoundary } from "@/workers/utils/commit-end-time.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";

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

type CanonicalSchedulerDatum =
  | "NoActiveOperators"
  | {
      readonly ActiveOperator: {
        readonly operator: string;
        readonly start_time: bigint;
      };
    };

type CanonicalSchedulerSpendRedeemer = {
  readonly scheduler_input_index: bigint;
  readonly scheduler_output_index: bigint;
  readonly advancing_approach:
    | {
        readonly GoToNextDueToEndOfShift: {
          readonly new_shifts_operator_node_ref_input_index: bigint;
        };
      }
    | {
        readonly RewindDueToEndOfShift: {
          readonly active_operators_root_ref_input_index: bigint;
          readonly active_operators_last_node_ref_input_index: bigint;
          readonly registered_element_ref_input_index: bigint;
        };
      }
    | {
        readonly AppointFirstOperator: {
          readonly new_shifts_operator_node_ref_input_index: bigint;
          readonly registered_element_ref_input_index: bigint;
        };
      };
};

type ActiveSchedulerState = {
  readonly operator: string;
  readonly startTime: bigint;
};

const SCHEDULER_REFRESH_POLL_INTERVAL = "2 seconds";
const SCHEDULER_REFRESH_MAX_POLLS = 30;
const MIN_SCHEDULER_WITNESS_LOVELACE = 5_000_000n;
const SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS = 90_000;
const SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const SCHEDULER_SHIFT_DURATION_MS = 30n;
const SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS = 5n * 60n * 1000n;
export const encodeSchedulerDatumForChain = (
  datum: CanonicalSchedulerDatum,
): string =>
  // Older deployed scheduler validators on preprod expect the root constructor
  // array in definite form. Lucid emits an indefinite root array here, so
  // normalize before publishing scheduler outputs on-chain.
  SDK.normalizeRootIndefiniteArrayEncoding(
    LucidData.to(datum as never, SDK.SchedulerDatum as never),
  );

const nodeKeyBytes = (key: SDK.NodeKey): string | undefined =>
  key === "Empty" ? undefined : key.Key.key;

const linkKeyBytes = (datum: SDK.LinkedListNodeView): string | undefined =>
  datum.next === "Empty" ? undefined : datum.next.Key.key;

const activeSchedulerState = (
  datum: CanonicalSchedulerDatum,
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
): CanonicalSchedulerDatum => ({
  ActiveOperator: {
    operator,
    start_time: startTime,
  },
});

const describeSchedulerDatum = (datum: CanonicalSchedulerDatum): string => {
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
  label: string,
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
    findLastMemberNode(registeredNodes, "Registered-operators") ??
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

  if (!allowGenesisRewind && !currentOperatorIsActiveHead) {
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
    SDK.getNodeDatumFromUTxO(utxo).pipe(
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

const decodeRegisteredOperatorActivationTime = (
  node: SDK.LinkedListNodeView,
): bigint | undefined => {
  const key = nodeKeyBytes(node.key);
  return key === undefined ? undefined : BigInt(`0x${key === "" ? "0" : key}`);
};

const resolveSchedulerRefreshValidityWindow = (
  lucid: LucidEvolution,
  currentSchedulerStartTime: bigint,
): {
  readonly validFrom: bigint;
  readonly validTo: bigint;
} => {
  const currentSlot = lucid.currentSlot();
  const currentSlotStart =
    slotToUnixTimeForLucid(lucid, currentSlot) ?? Date.now();
  const minimumShiftStart = Number(
    currentSchedulerStartTime + SCHEDULER_SHIFT_DURATION_MS,
  );
  let validFrom = alignUnixTimeToSlotBoundary(
    lucid,
    Math.max(currentSlotStart, minimumShiftStart),
  );
  if (validFrom < minimumShiftStart) {
    validFrom = alignUnixTimeToSlotBoundary(lucid, minimumShiftStart + 999);
  }
  return {
    validFrom: BigInt(validFrom),
    validTo: BigInt(validFrom) + SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS,
  };
};

const resolveSchedulerFirstAppointmentValidityWindow = (
  lucid: LucidEvolution,
  targetCommitEndTime: bigint,
): {
  readonly validFrom: bigint;
  readonly validTo: bigint;
} => {
  const currentSlot = lucid.currentSlot();
  const currentSlotStart =
    slotToUnixTimeForLucid(lucid, currentSlot) ?? Date.now();
  const validFrom = BigInt(alignUnixTimeToSlotBoundary(lucid, currentSlotStart));
  const validTo = targetCommitEndTime;
  if (validFrom >= validTo) {
    throw new Error(
      `Cannot appoint first scheduler operator because the target commit end-time is not in the future: valid_from=${validFrom.toString()},target_commit_end=${validTo.toString()}`,
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
  purpose: "bootstrap" | "refresh",
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

export const resolveReferenceInputIndexFromLedgerOrder = (
  target: UTxO,
  referenceInputs: readonly UTxO[],
): bigint => {
  const targetLabel = outRefLabel(target);
  const resolvedIndex = [...referenceInputs].sort(compareOutRefs).findIndex(
    (candidate) => outRefLabel(candidate) === targetLabel,
  );
  if (resolvedIndex < 0) {
    throw new Error(
      `Reference input ${targetLabel} missing from refresh witness set`,
    );
  }
  return BigInt(resolvedIndex);
};

const resolveInputIndexFromLedgerOrder = (
  target: UTxO,
  inputs: readonly UTxO[],
): bigint => {
  const targetLabel = outRefLabel(target);
  const resolvedIndex = [...inputs].sort(compareOutRefs).findIndex(
    (candidate) => outRefLabel(candidate) === targetLabel,
  );
  if (resolvedIndex < 0) {
    throw new Error(`Input ${targetLabel} missing from scheduler refresh input set`);
  }
  return BigInt(resolvedIndex);
};

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
        SDK.getNodeDatumFromUTxO(utxo),
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

const getSchedulerDatumFromUTxO = (
  schedulerUtxo: UTxO,
): Effect.Effect<CanonicalSchedulerDatum, SDK.StateQueueError> =>
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
        ) as CanonicalSchedulerDatum,
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to decode scheduler datum",
          cause,
        }),
    });
  });

const ensureRealSchedulerWitnessUtxo = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  schedulerUtxos: readonly UTxO[],
  schedulerMintingScriptRef?: UTxO,
): Effect.Effect<UTxO, SDK.StateQueueError | TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const existingWitness = [...schedulerUtxos]
      .filter((utxo) => (utxo.assets[schedulerWitnessUnit] ?? 0n) > 0n)
      .sort(compareOutRefs)[0];
    if (existingWitness !== undefined) {
      return existingWitness;
    }

    yield* Effect.logInfo(
      "🔹 Scheduler witness token with empty asset-name missing; creating one for real state_queue commits.",
    );
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to fetch wallet UTxOs for scheduler witness bootstrap",
          cause,
        }),
    });
    const feeInput = yield* selectFeeInput(walletUtxos);
    const bootstrapDatum: CanonicalSchedulerDatum = "NoActiveOperators";
    const bootstrapTx = yield* Effect.tryPromise({
      try: () => {
        const tx = lucid
          .newTx()
          .collectFrom([feeInput])
          .mintAssets({ [schedulerWitnessUnit]: 1n }, LucidData.void())
          .pay.ToContract(
            contracts.scheduler.spendingScriptAddress,
            {
              kind: "inline",
              value: encodeSchedulerDatumForChain(bootstrapDatum),
            },
            {
              lovelace: MIN_SCHEDULER_WITNESS_LOVELACE,
              [schedulerWitnessUnit]: 1n,
            },
          );
        return (
          schedulerMintingScriptRef === undefined
            ? tx.attach.Script(contracts.scheduler.mintingScript)
            : tx.readFrom([schedulerMintingScriptRef])
        ).complete({ localUPLCEval: true });
      },
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to build scheduler witness bootstrap tx: ${cause}`,
          cause,
        }),
    });

    const bootstrapTxHash = yield* handleSignSubmitNoConfirmation(
      lucid,
      bootstrapTx,
    );
    yield* Effect.logInfo(
      `🔹 Scheduler witness bootstrap submitted: ${bootstrapTxHash}`,
    );
    yield* awaitSubmittedSchedulerTx(lucid, bootstrapTxHash, "bootstrap");

    let pollCount = 0;
    while (pollCount < SCHEDULER_REFRESH_MAX_POLLS) {
      const witnessUtxos = yield* Effect.tryPromise({
        try: () =>
          lucid.utxosAtWithUnit(
            contracts.scheduler.spendingScriptAddress,
            schedulerWitnessUnit,
          ),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler witness UTxOs while waiting for bootstrap confirmation",
            cause,
          }),
      });
      const witness = [...witnessUtxos].sort(compareOutRefs)[0];
      if (witness !== undefined) {
        return witness;
      }
      pollCount += 1;
      yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Timed out waiting for scheduler witness UTxO bootstrap confirmation",
        cause: bootstrapTxHash,
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
              ),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to resolve scheduler first-appointment validity window",
                cause,
              }),
          })
        : resolveSchedulerRefreshValidityWindow(lucid, currentStartTime);
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
    const refreshedSchedulerDatumCbor =
      encodeSchedulerDatumForChain(refreshedSchedulerDatum);
    const witnessReferenceInputs =
      selection.kind === "Advance"
        ? [selection.activeNode.utxo]
        : selection.kind === "AppointFirst"
          ? [selection.activeNode.utxo, selection.registeredWitnessNode.utxo]
          : [
              selection.activeNode.utxo,
              selection.activeRootNode.utxo,
              selection.registeredWitnessNode.utxo,
            ];
    const referenceInputs =
      schedulerSpendingScriptRef === undefined
        ? witnessReferenceInputs
        : [...witnessReferenceInputs, schedulerSpendingScriptRef];
    if (selection.kind !== "Advance") {
      const registeredWitness = selection.registeredWitnessNode;
      if (registeredWitness.datum.key !== "Empty") {
        const activationTime = decodeRegisteredOperatorActivationTime(
          registeredWitness.datum,
        );
        if (activationTime === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Failed to decode registered-operators witness activation time for scheduler rewind",
              cause: outRefLabel(registeredWitness.utxo),
            }),
          );
        }
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
    const feeInput = yield* selectFeeInput(
      availableOperatorWalletUtxos(flowOperatorWalletView),
    );
    const schedulerInputIndex = resolveInputIndexFromLedgerOrder(
      schedulerRefInput,
      [feeInput, schedulerRefInput],
    );
    const schedulerSpendRedeemer: CanonicalSchedulerSpendRedeemer =
      selection.kind === "Advance"
        ? {
            scheduler_input_index: schedulerInputIndex,
            scheduler_output_index: 0n,
            advancing_approach: {
              GoToNextDueToEndOfShift: {
                new_shifts_operator_node_ref_input_index:
                  resolveReferenceInputIndexFromLedgerOrder(
                    selection.activeNode.utxo,
                    referenceInputs,
                ),
              },
            },
          }
        : selection.kind === "AppointFirst"
          ? {
              scheduler_input_index: schedulerInputIndex,
              scheduler_output_index: 0n,
              advancing_approach: {
                AppointFirstOperator: {
                  new_shifts_operator_node_ref_input_index:
                    resolveReferenceInputIndexFromLedgerOrder(
                      selection.activeNode.utxo,
                      referenceInputs,
                    ),
                  registered_element_ref_input_index:
                    resolveReferenceInputIndexFromLedgerOrder(
                      selection.registeredWitnessNode.utxo,
                      referenceInputs,
                    ),
                },
              },
            }
        : {
            scheduler_input_index: schedulerInputIndex,
            scheduler_output_index: 0n,
            advancing_approach: {
              RewindDueToEndOfShift: {
                active_operators_root_ref_input_index:
                  resolveReferenceInputIndexFromLedgerOrder(
                    selection.activeRootNode.utxo,
                    referenceInputs,
                  ),
                active_operators_last_node_ref_input_index:
                  resolveReferenceInputIndexFromLedgerOrder(
                    selection.activeNode.utxo,
                    referenceInputs,
                  ),
                registered_element_ref_input_index:
                  resolveReferenceInputIndexFromLedgerOrder(
                    selection.registeredWitnessNode.utxo,
                    referenceInputs,
                  ),
              },
            },
          };
    yield* Effect.logInfo(
      `🔹 Refreshing scheduler witness datum for commit window via ${selection.kind} (from=${describeSchedulerDatum(schedulerDatum)} to=${describeSchedulerDatum(refreshedSchedulerDatum)}, validTo=${validTo.toString()}).`,
    );

    /**
     * Builds the scheduler-refresh transaction.
     */
    const mkSchedulerRefreshTx = () =>
      lucid
        .newTx()
        .validFrom(Number(validFrom))
        .validTo(Number(validTo))
        .collectFrom([feeInput])
        .readFrom(referenceInputs)
        .collectFrom(
          [schedulerRefInput],
          LucidData.to(
            schedulerSpendRedeemer as never,
            SDK.SchedulerSpendRedeemer as never,
          ),
        )
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: refreshedSchedulerDatumCbor,
          },
          schedulerRefInput.assets,
        )
        .addSignerKey(operatorKeyHash);
    const mkSchedulerRefreshTxWithScript = () =>
      schedulerSpendingScriptRef === undefined
        ? mkSchedulerRefreshTx().attach.Script(contracts.scheduler.spendingScript)
        : mkSchedulerRefreshTx();

    const refreshTx = yield* Effect.tryPromise({
      try: () => mkSchedulerRefreshTxWithScript().complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to build scheduler refresh tx: ${cause}`,
          cause,
        }),
    });

    const refreshTxHash = yield* handleSignSubmitNoConfirmation(lucid, refreshTx);
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
        if (
          activeSchedulerState(utxoDatumEither.right)?.operator ===
            operatorKeyHash &&
          activeSchedulerState(utxoDatumEither.right)?.startTime ===
            refreshedSchedulerStartTime
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
        : yield* fetchReferenceScriptUtxosProgram(lucid, referenceScriptsAddress, [
            {
              name: "scheduler spending",
              script: contracts.scheduler.spendingScript,
            },
            {
              name: "scheduler minting",
              script: contracts.scheduler.mintingScript,
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
          ]);
    const schedulerSpendingScriptRef =
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(resolvedReferenceScripts, "scheduler spending");
    const schedulerMintingScriptRef =
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(resolvedReferenceScripts, "scheduler minting");
    const activeOperatorsSpendingScriptRef =
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(
            resolvedReferenceScripts,
            "active-operators spending",
          );
    const stateQueueSpendingScriptRef =
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(resolvedReferenceScripts, "state-queue spending");
    const stateQueueMintingScriptRef =
      referenceScriptsAddress === undefined
        ? undefined
        : referenceScriptByName(resolvedReferenceScripts, "state-queue minting");
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const activeOperatorUtxosForRefresh = (
      yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
    ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message:
                "Failed to fetch active-operators UTxOs for state_queue commit",
              cause,
            }),
        ),
      )
    ).map((beacon) => beacon.utxo);
    const registeredOperatorUtxos = (
      yield* SDK.utxosAtByNFTPolicyId(
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
      )
    ).map((beacon) => beacon.utxo);

    const schedulerUtxos = (
      yield* SDK.utxosAtByNFTPolicyId(
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
      )
    ).map((beacon) => beacon.utxo);
    const initialSchedulerRefInput = yield* ensureRealSchedulerWitnessUtxo(
      lucid,
      contracts,
      schedulerUtxos,
      schedulerMintingScriptRef,
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

    const activeOperatorUtxos = (
      yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
    ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message:
                "Failed to refresh active-operators UTxOs for state_queue commit",
              cause,
            }),
        ),
      )
    ).map((beacon) => beacon.utxo);
    const activeOperatorInput = yield* selectActiveOperatorInput(
      activeOperatorUtxos,
      operatorKeyHash,
    );

    if (activeOperatorInput.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Active-operators UTxO must include inline datum for real state_queue commit",
          cause: `${activeOperatorInput.txHash}#${activeOperatorInput.outputIndex}`,
        }),
      );
    }

    return {
      operatorKeyHash,
      schedulerRefInput: schedulerRefInput.schedulerRefInput,
      hubOracleRefInput,
      activeOperatorInput: activeOperatorInput as UTxO & { datum: string },
      activeOperatorsSpendingScript: contracts.activeOperators.spendingScript,
      activeOperatorsSpendingScriptRef,
      stateQueueSpendingScriptRef,
      stateQueueMintingScriptRef,
      operatorWalletView: schedulerRefInput.operatorWalletView,
    };
  });

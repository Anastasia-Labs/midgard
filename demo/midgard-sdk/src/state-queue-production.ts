import { assetsEqual } from "@al-ft/midgard-core/assets";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  compareOutRefs,
  findOutRefIndex,
  outRefLabel,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";
import {
  CML,
  Data,
  type Assets,
  type LucidEvolution,
  type RedeemerBuilder,
  type Script,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  castActiveOperatorDatumToData,
} from "@/active-operators.js";
import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
  resolveMintPolicyContextIndex,
  resolveRedeemerTxInfoIndex,
  withStubbedProviderEvaluation,
} from "@/cardano-redeemers.js";
import type {
  DataCoercionError,
  HashingError,
  MidgardValidators,
} from "@/common.js";
import {
  castConfirmedStateToData,
  castHeaderToData,
  type ConfirmedState,
  getHeaderFromStateQueueDatum,
  type Header,
  hashBlockHeader,
} from "@/ledger-state.js";
import {
  encodeLinkedListNodeView,
  linkedListDatumToNodeView,
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  LinkedListDatum,
  type LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@/linked-list.js";
import {
  SettlementDatum,
  SettlementMintRedeemer,
  type SettlementMintRedeemer as SettlementMintRedeemerType,
} from "@/settlement.js";
import {
  getConfirmedStateFromStateQueueDatum,
  StateQueueError,
  StateQueueRedeemer,
  type StateQueueFetchConfig,
  type StateQueueRedeemer as StateQueueRedeemerType,
  type StateQueueUTxO,
} from "@/state-queue.js";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  requireOutRefIndex,
  resolveOutRefIndexFromSet,
} from "@/tx-out-ref-order.js";

const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;
const ACTIVE_OPERATOR_MATURITY_DURATION_MS = 30n;
const MIN_SETTLEMENT_OUTPUT_LOVELACE = 5_000_000n;
const MERGE_SCRIPT_SPEND_REDEEMER_COUNT = 2;

export type OperatorWalletViewLike = {
  readonly knownUtxos: readonly UTxO[];
  readonly consumedOutRefs: readonly string[];
};

export type StateQueueCommitWitnessContext = {
  readonly operatorKeyHash: string;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO & { readonly datum: string };
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsSpendingScriptRef?: UTxO;
  readonly stateQueueSpendingScriptRef?: UTxO;
  readonly stateQueueMintingScriptRef?: UTxO;
  readonly operatorWalletView: OperatorWalletViewLike;
};

export type StateQueueCommitLayout = {
  readonly schedulerRefInputIndex: bigint;
  readonly latestBlockInputIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueSpendRedeemerIndex: bigint;
};

export const DEFAULT_STATE_QUEUE_COMMIT_LAYOUT: StateQueueCommitLayout = {
  schedulerRefInputIndex: 0n,
  latestBlockInputIndex: 0n,
  newBlockOutputIndex: 0n,
  continuedLatestBlockOutputIndex: 1n,
  activeOperatorsInputIndex: 1n,
  activeOperatorsRedeemerIndex: 1n,
  activeOperatorOutputIndex: 2n,
  hubOracleRefInputIndex: 0n,
  stateQueueSpendRedeemerIndex: 0n,
} as const;

type StateQueueCommitRedeemer = {
  readonly CommitBlockHeader: {
    readonly latest_block_input_index: bigint;
    readonly new_block_output_index: bigint;
    readonly continued_latest_block_output_index: bigint;
    readonly operator: string;
    readonly scheduler_ref_input_index: bigint;
    readonly active_operators_input_index: bigint;
    readonly active_operators_redeemer_index: bigint;
  };
};

type ActiveOperatorCommitRedeemer = {
  readonly UpdateBondHoldNewState: {
    readonly active_operator: string;
    readonly active_node_input_index: bigint;
    readonly active_node_output_index: bigint;
    readonly hub_oracle_ref_input_index: bigint;
    readonly state_queue_input_index: bigint;
    readonly state_queue_redeemer_index: bigint;
  };
};

const availableOperatorWalletUtxos = (
  view: OperatorWalletViewLike,
): readonly UTxO[] => {
  const consumedOutRefs = new Set(view.consumedOutRefs);
  return view.knownUtxos.filter(
    (utxo) => !consumedOutRefs.has(outRefLabel(utxo)),
  );
};

const selectCommitFeeInput = (
  walletUtxos: readonly UTxO[],
): Effect.Effect<UTxO, StateQueueError> =>
  Effect.gen(function* () {
    const feeInput = [...walletUtxos].sort((a, b) => {
      const lovelaceA = a.assets.lovelace ?? 0n;
      const lovelaceB = b.assets.lovelace ?? 0n;
      if (lovelaceA === lovelaceB) {
        return compareOutRefs(a, b);
      }
      return lovelaceA > lovelaceB ? -1 : 1;
    })[0];
    if (feeInput === undefined) {
      return yield* Effect.fail(
        new StateQueueError({
          message: "No wallet UTxO available to fund state_queue commit tx",
          cause: "empty wallet",
        }),
      );
    }
    return feeInput;
  });

export const selectPureAdaFeeInput = (
  walletUtxos: readonly UTxO[],
): Effect.Effect<UTxO, StateQueueError> =>
  Effect.gen(function* () {
    const pureAdaUtxos = walletUtxos.filter((utxo) =>
      Object.entries(utxo.assets).every(
        ([unit, amount]) => unit === "lovelace" || amount <= 0n,
      ),
    );
    if (pureAdaUtxos.length === 0) {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to select fee input for merge transaction",
          cause: "operator wallet has no pure-ADA UTxO",
        }),
      );
    }
    return yield* selectCommitFeeInput(pureAdaUtxos);
  });

const decodeActiveOperatorDatum = (data: unknown): ActiveOperatorDatum =>
  Data.castFrom(
    data as never,
    ActiveOperatorDatum as never,
  ) as ActiveOperatorDatum;

export const deriveStateQueueCommitLayout = ({
  latestBlockInput,
  activeOperatorInput,
  schedulerRefInput,
  hubOracleRefInput,
  txReferenceInputs,
  txInputs,
}: {
  readonly latestBlockInput: OutRefLike;
  readonly activeOperatorInput: OutRefLike;
  readonly schedulerRefInput?: OutRefLike;
  readonly hubOracleRefInput?: OutRefLike;
  readonly txReferenceInputs?: readonly OutRefLike[];
  readonly txInputs: readonly OutRefLike[];
}): StateQueueCommitLayout => {
  const sortedInputs = [...txInputs].sort(compareOutRefs);
  const latestBlockInputIndex = findOutRefIndex(sortedInputs, latestBlockInput);
  if (latestBlockInputIndex === undefined) {
    throw new Error(
      `Latest state-queue input ${outRefLabel(latestBlockInput)} missing from tx input set`,
    );
  }
  const activeOperatorsInputIndex = findOutRefIndex(
    sortedInputs,
    activeOperatorInput,
  );
  if (activeOperatorsInputIndex === undefined) {
    throw new Error(
      `Active operator input ${outRefLabel(activeOperatorInput)} missing from tx input set`,
    );
  }

  const stateQueueSpendRedeemerIndex =
    compareOutRefs(latestBlockInput, activeOperatorInput) < 0 ? 0n : 1n;
  const activeOperatorsRedeemerIndex =
    compareOutRefs(activeOperatorInput, latestBlockInput) < 0 ? 0n : 1n;

  if (schedulerRefInput === undefined && hubOracleRefInput === undefined) {
    return {
      ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
      latestBlockInputIndex: BigInt(latestBlockInputIndex),
      activeOperatorsInputIndex: BigInt(activeOperatorsInputIndex),
      stateQueueSpendRedeemerIndex,
      activeOperatorsRedeemerIndex,
    };
  }
  if (schedulerRefInput === undefined || hubOracleRefInput === undefined) {
    throw new Error(
      "State queue commit layout requires both scheduler and hub-oracle reference inputs when deriving non-default reference indices",
    );
  }

  const referenceInputs = txReferenceInputs ?? [
    schedulerRefInput,
    hubOracleRefInput,
  ];
  return {
    ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
    schedulerRefInputIndex: resolveOutRefIndexFromSet(
      schedulerRefInput,
      referenceInputs,
    ),
    latestBlockInputIndex: BigInt(latestBlockInputIndex),
    activeOperatorsInputIndex: BigInt(activeOperatorsInputIndex),
    hubOracleRefInputIndex: resolveOutRefIndexFromSet(
      hubOracleRefInput,
      referenceInputs,
    ),
    stateQueueSpendRedeemerIndex,
    activeOperatorsRedeemerIndex,
  };
};

export const makeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): StateQueueCommitRedeemer => ({
  CommitBlockHeader: {
    latest_block_input_index: layout.latestBlockInputIndex,
    new_block_output_index: layout.newBlockOutputIndex,
    continued_latest_block_output_index: layout.continuedLatestBlockOutputIndex,
    operator: operatorKeyHash,
    scheduler_ref_input_index: layout.schedulerRefInputIndex,
    active_operators_input_index: layout.activeOperatorsInputIndex,
    active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
  },
});

export const makeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): ActiveOperatorCommitRedeemer => ({
  UpdateBondHoldNewState: {
    active_operator: operatorKeyHash,
    active_node_input_index: layout.activeOperatorsInputIndex,
    active_node_output_index: layout.activeOperatorOutputIndex,
    hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    state_queue_input_index: layout.latestBlockInputIndex,
    state_queue_redeemer_index: layout.stateQueueSpendRedeemerIndex,
  },
});

export const encodeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): string =>
  Data.to(
    makeStateQueueCommitRedeemer(operatorKeyHash, layout) as never,
    StateQueueRedeemer as never,
  );

export const encodeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): string =>
  Data.to(
    makeActiveOperatorCommitRedeemer(operatorKeyHash, layout) as never,
    ActiveOperatorSpendRedeemer as never,
  );

type CommitLayoutLike = {
  readonly schedulerRefInputIndex: bigint;
  readonly latestBlockInputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly stateQueueSpendRedeemerIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
};

const COMMIT_LAYOUT_FIELDS = [
  { key: "schedulerRefInputIndex", label: "scheduler_ref_input_index" },
  { key: "latestBlockInputIndex", label: "latest_block_input_index" },
  { key: "activeOperatorsInputIndex", label: "active_operators_input_index" },
  {
    key: "activeOperatorsRedeemerIndex",
    label: "active_operators_redeemer_index",
  },
  {
    key: "stateQueueSpendRedeemerIndex",
    label: "state_queue_spend_redeemer_index",
  },
  { key: "newBlockOutputIndex", label: "new_block_output_index" },
  {
    key: "continuedLatestBlockOutputIndex",
    label: "continued_latest_block_output_index",
  },
  { key: "activeOperatorOutputIndex", label: "active_operator_output_index" },
  { key: "hubOracleRefInputIndex", label: "hub_oracle_ref_input_index" },
] as const satisfies readonly {
  readonly key: keyof CommitLayoutLike;
  readonly label: string;
}[];

const formatCommitLayout = (layout: CommitLayoutLike): string =>
  COMMIT_LAYOUT_FIELDS.map(
    ({ key, label }) => `${label}=${layout[key].toString()}`,
  ).join(",");

const commitLayoutsEqual = (
  left: StateQueueCommitLayout,
  right: StateQueueCommitLayout,
): boolean => COMMIT_LAYOUT_FIELDS.every(({ key }) => left[key] === right[key]);

export const deriveCommitLayoutFromDraftTx = ({
  tx,
  latestBlockInput,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  stateQueueAddress,
  headerNodeUnit,
  headerNodeDatum,
  previousHeaderNodeDatum,
}: {
  readonly tx: CML.Transaction;
  readonly latestBlockInput: UTxO;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly stateQueueAddress: string;
  readonly headerNodeUnit: string;
  readonly headerNodeDatum: string;
  readonly previousHeaderNodeDatum: string;
}): StateQueueCommitLayout => {
  const txBody = tx.body();
  const inputList = collectSortedInputOutRefs(txBody.inputs());
  const referenceInputListRaw = txBody.reference_inputs();
  const indexedOutputs = collectIndexedOutputs(txBody.outputs());

  const latestBlockInputIndex = requireOutRefIndex(inputList, latestBlockInput);
  const activeOperatorsInputIndex = requireOutRefIndex(
    inputList,
    activeOperatorInput,
  );
  if (referenceInputListRaw === undefined) {
    throw new Error(
      "Balanced draft tx body did not include reference inputs for scheduler witness",
    );
  }
  const referenceInputList = collectSortedInputOutRefs(referenceInputListRaw);
  const schedulerRefInputIndex = requireOutRefIndex(
    referenceInputList,
    schedulerRefInput,
  );
  const hubOracleRefInputIndex = requireOutRefIndex(
    referenceInputList,
    hubOracleRefInput,
  );

  const headerNodeOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === headerNodeDatum &&
      (output.assets[headerNodeUnit] ?? 0n) === 1n,
  );
  if (headerNodeOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one header-node output at ${stateQueueAddress} with datum ${headerNodeDatum.slice(0, 24)}..., found ${headerNodeOutputCandidates.length}`,
    );
  }

  const previousHeaderOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === previousHeaderNodeDatum,
  );
  if (previousHeaderOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one previous-header output at ${stateQueueAddress} with datum ${previousHeaderNodeDatum.slice(0, 24)}..., found ${previousHeaderOutputCandidates.length}`,
    );
  }

  const activeNodeOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === activeOperatorInput.address &&
      assetsEqual(output.assets, activeOperatorInput.assets),
  );
  if (activeNodeOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one active-operator output at ${activeOperatorInput.address} with unchanged assets, found ${activeNodeOutputCandidates.length}`,
    );
  }

  const redeemerPointers = getRedeemerPointersInContextOrder(tx);
  return {
    schedulerRefInputIndex,
    latestBlockInputIndex,
    activeOperatorsInputIndex,
    newBlockOutputIndex: BigInt(headerNodeOutputCandidates[0]!.index),
    continuedLatestBlockOutputIndex: BigInt(
      previousHeaderOutputCandidates[0]!.index,
    ),
    activeOperatorsRedeemerIndex: resolveRedeemerTxInfoIndex({
      pointers: redeemerPointers,
      target: { tag: CML.RedeemerTag.Spend, index: activeOperatorsInputIndex },
      label: `active-operator spend redeemer for input index ${activeOperatorsInputIndex.toString()}`,
    }),
    activeOperatorOutputIndex: BigInt(activeNodeOutputCandidates[0]!.index),
    hubOracleRefInputIndex,
    stateQueueSpendRedeemerIndex: resolveRedeemerTxInfoIndex({
      pointers: redeemerPointers,
      target: { tag: CML.RedeemerTag.Spend, index: latestBlockInputIndex },
      label: `state-queue spend redeemer for input index ${latestBlockInputIndex.toString()}`,
    }),
  };
};

const completeCommitTxForLayout = ({
  makeCommitTxForLayout,
  layout,
  label,
}: {
  readonly makeCommitTxForLayout: (
    commitLayout: StateQueueCommitLayout,
  ) => TxBuilder;
  readonly layout: StateQueueCommitLayout;
  readonly label: "derived" | "tx-derived";
}): Effect.Effect<TxSignBuilder, StateQueueError> => {
  const verb = label === "derived" ? "build" : "rebuild";
  const layoutLabel =
    label === "derived" ? "derived layout" : "tx-derived layout";
  return Effect.tryPromise({
    try: () => makeCommitTxForLayout(layout).complete({ localUPLCEval: true }),
    catch: (cause) =>
      new StateQueueError({
        message: `Failed to ${verb} block header commitment transaction with ${layoutLabel} (${formatCommitLayout(
          layout,
        )}): ${formatUnknownError(cause)}`,
        cause,
      }),
  });
};

export type DeterministicCommitTxBuilderInput = {
  readonly lucid: LucidEvolution;
  readonly contracts: MidgardValidators;
  readonly latestBlockInput: UTxO;
  readonly witness: StateQueueCommitWitnessContext;
  readonly headerNodeUnit: string;
  readonly appendedNodeDatumCbor: string;
  readonly previousHeaderNodeDatumCbor: string;
  readonly updatedActiveOperatorDatumCbor: string;
  readonly commitMintAssets: Readonly<Record<string, bigint>>;
  readonly makeBaseCommitTx: (stateQueueCommitRedeemer: string) => TxBuilder;
};

export const buildDeterministicCommitTxBuilder = ({
  lucid,
  contracts,
  latestBlockInput,
  witness,
  headerNodeUnit,
  appendedNodeDatumCbor,
  previousHeaderNodeDatumCbor,
  updatedActiveOperatorDatumCbor,
  commitMintAssets,
  makeBaseCommitTx,
}: DeterministicCommitTxBuilderInput): Effect.Effect<
  TxSignBuilder,
  StateQueueError
> =>
  Effect.gen(function* () {
    const feeInput = yield* selectCommitFeeInput(
      availableOperatorWalletUtxos(witness.operatorWalletView),
    );
    yield* Effect.logInfo(
      `🔹 Selected fee input ${outRefLabel(feeInput)} for state_queue commit tx.`,
    );

    const referenceInputs = [
      witness.schedulerRefInput,
      witness.hubOracleRefInput,
      ...(witness.activeOperatorsSpendingScriptRef === undefined
        ? []
        : [witness.activeOperatorsSpendingScriptRef]),
      ...(witness.stateQueueSpendingScriptRef === undefined
        ? []
        : [witness.stateQueueSpendingScriptRef]),
      ...(witness.stateQueueMintingScriptRef === undefined
        ? []
        : [witness.stateQueueMintingScriptRef]),
    ];

    const makeCommitTxForLayout = (commitLayout: StateQueueCommitLayout) => {
      const stateQueueCommitRedeemer = encodeStateQueueCommitRedeemer(
        witness.operatorKeyHash,
        commitLayout,
      );
      const tx = makeBaseCommitTx(stateQueueCommitRedeemer)
        .collectFrom([feeInput])
        .readFrom(referenceInputs)
        .collectFrom(
          [witness.activeOperatorInput],
          encodeActiveOperatorCommitRedeemer(
            witness.operatorKeyHash,
            commitLayout,
          ),
        )
        .pay.ToContract(
          witness.activeOperatorInput.address,
          {
            kind: "inline",
            value: updatedActiveOperatorDatumCbor,
          },
          witness.activeOperatorInput.assets,
        )
        .addSignerKey(witness.operatorKeyHash)
        .mintAssets(commitMintAssets, stateQueueCommitRedeemer);
      const withActiveOperatorsScript =
        witness.activeOperatorsSpendingScriptRef === undefined
          ? tx.attach.Script(witness.activeOperatorsSpendingScript)
          : tx;
      const withStateQueueSpendingScript =
        witness.stateQueueSpendingScriptRef === undefined
          ? withActiveOperatorsScript.attach.Script(
              contracts.stateQueue.spendingScript,
            )
          : withActiveOperatorsScript;
      return witness.stateQueueMintingScriptRef === undefined
        ? withStateQueueSpendingScript.attach.Script(
            contracts.stateQueue.mintingScript,
          )
        : withStateQueueSpendingScript;
    };

    const seedLayout = deriveStateQueueCommitLayout({
      latestBlockInput,
      activeOperatorInput: witness.activeOperatorInput,
      schedulerRefInput: witness.schedulerRefInput,
      hubOracleRefInput: witness.hubOracleRefInput,
      txReferenceInputs: referenceInputs,
      txInputs: [latestBlockInput, witness.activeOperatorInput, feeInput],
    });
    const commitLayout = yield* Effect.tryPromise({
      try: async () => {
        const [, , draftSignBuilder] = await withStubbedProviderEvaluation(
          lucid,
          () =>
            makeCommitTxForLayout(seedLayout).chain({
              localUPLCEval: true,
            }),
        );
        return deriveCommitLayoutFromDraftTx({
          tx: draftSignBuilder.toTransaction(),
          latestBlockInput,
          schedulerRefInput: witness.schedulerRefInput,
          hubOracleRefInput: witness.hubOracleRefInput,
          activeOperatorInput: witness.activeOperatorInput,
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          headerNodeUnit,
          headerNodeDatum: appendedNodeDatumCbor,
          previousHeaderNodeDatum: previousHeaderNodeDatumCbor,
        });
      },
      catch: (cause) =>
        new StateQueueError({
          message: `Failed to derive deterministic commit redeemer layout from balanced draft tx: ${formatUnknownError(
            cause,
          )}`,
          cause,
        }),
    });
    yield* Effect.logInfo(
      `🔹 Using commit redeemer layout: ${formatCommitLayout(commitLayout)}`,
    );

    let stableCommitLayout = commitLayout;
    let builtCommitTx = yield* completeCommitTxForLayout({
      makeCommitTxForLayout,
      layout: stableCommitLayout,
      label: "derived",
    });
    for (let iteration = 0; iteration < 2; iteration += 1) {
      const derivedSubmitLayout = deriveCommitLayoutFromDraftTx({
        tx: builtCommitTx.toTransaction(),
        latestBlockInput,
        schedulerRefInput: witness.schedulerRefInput,
        hubOracleRefInput: witness.hubOracleRefInput,
        activeOperatorInput: witness.activeOperatorInput,
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        headerNodeUnit,
        headerNodeDatum: appendedNodeDatumCbor,
        previousHeaderNodeDatum: previousHeaderNodeDatumCbor,
      });
      if (commitLayoutsEqual(stableCommitLayout, derivedSubmitLayout)) {
        return builtCommitTx;
      }
      if (iteration === 1) {
        return yield* Effect.fail(
          new StateQueueError({
            message:
              "Commit transaction layout did not converge after deterministic rebuild",
            cause: `authored=${formatCommitLayout(stableCommitLayout)}; derived=${formatCommitLayout(
              derivedSubmitLayout,
            )}`,
          }),
        );
      }
      yield* Effect.logWarning(
        `Commit layout drift detected after balancing; rebuilding with tx-derived indexes. authored=${formatCommitLayout(stableCommitLayout)} derived=${formatCommitLayout(
          derivedSubmitLayout,
        )}`,
      );
      stableCommitLayout = derivedSubmitLayout;
      builtCommitTx = yield* completeCommitTxForLayout({
        makeCommitTxForLayout,
        layout: stableCommitLayout,
        label: "tx-derived",
      });
    }

    return builtCommitTx;
  });

export type ProductionCommitBlockHeaderParams = {
  readonly lucid: LucidEvolution;
  readonly contracts: MidgardValidators;
  readonly latestBlock: StateQueueUTxO;
  readonly updatedNodeDatum: LinkedListNodeView;
  readonly newHeader: Header;
  readonly validTo: number;
  readonly witness: StateQueueCommitWitnessContext;
  readonly headerNodeLovelace?: bigint;
  readonly activeOperatorMaturityDurationMs?: bigint;
};

export type ProductionCommitBlockHeaderResult = {
  readonly tx: TxSignBuilder;
  readonly newHeaderHash: string;
};

export const buildProductionCommitBlockHeaderTxProgram = ({
  lucid,
  contracts,
  latestBlock,
  updatedNodeDatum,
  newHeader,
  validTo,
  witness,
  headerNodeLovelace = STATE_QUEUE_HEADER_NODE_LOVELACE,
  activeOperatorMaturityDurationMs = ACTIVE_OPERATOR_MATURITY_DURATION_MS,
}: ProductionCommitBlockHeaderParams): Effect.Effect<
  ProductionCommitBlockHeaderResult,
  StateQueueError | HashingError
> =>
  Effect.gen(function* () {
    const newHeaderHash = yield* hashBlockHeader(newHeader);
    const headerNodeUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + newHeaderHash,
    );
    const commitMintAssets = { [headerNodeUnit]: 1n };
    const headerNodeOutputAssets = {
      lovelace: headerNodeLovelace,
      ...commitMintAssets,
    };
    const appendedNodeDatum: LinkedListNodeView = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: castHeaderToData(newHeader) as LinkedListNodeView["data"],
    };
    const appendedNodeDatumCbor = encodeLinkedListNodeView(appendedNodeDatum);
    const updatedNodeDatumCbor = encodeLinkedListNodeView(updatedNodeDatum);
    const updatedActiveOperatorDatumCbor = yield* Effect.try({
      try: () => {
        const activeOperatorLinkedListDatum = Data.from(
          witness.activeOperatorInput.datum,
          LinkedListDatum,
        );
        const activeOperatorNodeView = linkedListDatumToNodeView(
          activeOperatorLinkedListDatum,
          ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + witness.operatorKeyHash,
        );
        const activeOperatorDatum = decodeActiveOperatorDatum(
          activeOperatorNodeView.data,
        );
        return encodeLinkedListNodeView({
          ...activeOperatorNodeView,
          data: castActiveOperatorDatumToData({
            ...activeOperatorDatum,
            bond_unlock_time:
              BigInt(validTo) - 1n + activeOperatorMaturityDurationMs,
          }) as LinkedListNodeView["data"],
        });
      },
      catch: (cause) =>
        new StateQueueError({
          message:
            "Failed to update active-operator bond-hold datum for commit tx",
          cause,
        }),
    });

    const makeBaseCommitTx = (stateQueueCommitRedeemer: string) =>
      lucid
        .newTx()
        .validTo(validTo)
        .collectFrom([latestBlock.utxo], stateQueueCommitRedeemer)
        .pay.ToContract(
          contracts.stateQueue.spendingScriptAddress,
          {
            kind: "inline",
            value: appendedNodeDatumCbor,
          },
          headerNodeOutputAssets,
        )
        .pay.ToContract(
          contracts.stateQueue.spendingScriptAddress,
          {
            kind: "inline",
            value: updatedNodeDatumCbor,
          },
          latestBlock.utxo.assets,
        );

    const tx = yield* buildDeterministicCommitTxBuilder({
      lucid,
      contracts,
      latestBlockInput: latestBlock.utxo,
      witness,
      headerNodeUnit,
      appendedNodeDatumCbor,
      previousHeaderNodeDatumCbor: updatedNodeDatumCbor,
      updatedActiveOperatorDatumCbor,
      commitMintAssets,
      makeBaseCommitTx,
    });

    return { tx, newHeaderHash };
  });

export type InitialMergeRedeemerSeedIndexes = {
  readonly stateQueueMintPointerIndex: number;
  readonly settlementMintPointerIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
};

export const deriveInitialMergeRedeemerSeedIndexes = ({
  stateQueuePolicyId,
  settlementPolicyId,
}: {
  readonly stateQueuePolicyId: string;
  readonly settlementPolicyId: string;
}): InitialMergeRedeemerSeedIndexes => {
  const policyIds = [stateQueuePolicyId, settlementPolicyId] as const;
  const stateQueueMintPointerIndex = Number(
    resolveMintPolicyContextIndex({
      policyIds,
      targetPolicyId: stateQueuePolicyId,
    }),
  );
  const settlementMintPointerIndex = Number(
    resolveMintPolicyContextIndex({
      policyIds,
      targetPolicyId: settlementPolicyId,
    }),
  );
  return {
    stateQueueMintPointerIndex,
    settlementMintPointerIndex,
    stateQueueRedeemerIndex:
      MERGE_SCRIPT_SPEND_REDEEMER_COUNT + stateQueueMintPointerIndex,
    settlementRedeemerIndex:
      MERGE_SCRIPT_SPEND_REDEEMER_COUNT + settlementMintPointerIndex,
  };
};

export type StateQueueMergeReferenceScripts = {
  readonly stateQueueSpending?: UTxO;
  readonly stateQueueMinting?: UTxO;
  readonly settlementMinting?: UTxO;
};

export type ProductionMergeToConfirmedStateParams = {
  readonly lucid: LucidEvolution;
  readonly fetchConfig: StateQueueFetchConfig;
  readonly contracts: MidgardValidators;
  readonly confirmedUTxO: StateQueueUTxO;
  readonly firstBlockUTxO: StateQueueUTxO;
  readonly validFrom: number;
  readonly feeInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly referenceScripts?: StateQueueMergeReferenceScripts;
  readonly settlementOutputLovelace?: bigint;
};

export type MergeRedeemerLayout = {
  readonly headerNodeInputIndex: number;
  readonly confirmedStateInputIndex: number;
  readonly confirmedStateOutputIndex: number;
  readonly settlementOutputIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
  readonly hubOracleRefInputIndex: number;
};

type MergeLayoutDerivation = {
  readonly layout: MergeRedeemerLayout;
  readonly diagnostics: unknown;
};

export type ProductionMergeToConfirmedStateResult = {
  readonly tx: TxSignBuilder;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly layout: MergeRedeemerLayout;
  readonly diagnostics: unknown;
};

const makeJsonSafe = (value: unknown): unknown => {
  try {
    return JSON.parse(
      JSON.stringify(value, (_key, nestedValue) =>
        typeof nestedValue === "bigint" ? nestedValue.toString() : nestedValue,
      ),
    ) as unknown;
  } catch {
    return formatUnknownError(value);
  }
};

const mergeStateQueueError = (
  errorCode: string,
  message: string,
  cause: unknown,
): StateQueueError =>
  new StateQueueError({
    message: `${errorCode}: ${message}`,
    cause: {
      error_code: errorCode,
      details: makeJsonSafe(cause),
    },
  });

const makeStateQueueMergeRedeemer = ({
  layout,
  headerNodeKey,
  blockHeader,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
}): StateQueueRedeemerType => ({
  MergeToConfirmedState: {
    header_node_key: headerNodeKey,
    header_node_input_index: BigInt(layout.headerNodeInputIndex),
    confirmed_state_input_index: BigInt(layout.confirmedStateInputIndex),
    confirmed_state_output_index: BigInt(layout.confirmedStateOutputIndex),
    m_settlement_redeemer_index: BigInt(layout.settlementRedeemerIndex),
    merged_block_transactions_root: blockHeader.transactionsRoot,
    merged_block_deposits_root: blockHeader.depositsRoot,
    merged_block_withdrawals_root: blockHeader.withdrawalsRoot,
  },
});

const makeSettlementSpawnRedeemer = ({
  layout,
  headerNodeKey,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
}): SettlementMintRedeemerType => ({
  Spawn: {
    settlement_id: headerNodeKey,
    output_index: BigInt(layout.settlementOutputIndex),
    state_queue_merge_redeemer_index: BigInt(layout.stateQueueRedeemerIndex),
    hub_ref_input_index: BigInt(layout.hubOracleRefInputIndex),
  },
});

const encodeMergeRedeemers = ({
  layout,
  headerNodeKey,
  blockHeader,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
}): {
  readonly stateQueue: string;
  readonly settlement: string;
} => ({
  stateQueue: Data.to(
    makeStateQueueMergeRedeemer({ layout, headerNodeKey, blockHeader }),
    StateQueueRedeemer,
  ),
  settlement: Data.to(
    makeSettlementSpawnRedeemer({ layout, headerNodeKey }),
    SettlementMintRedeemer,
  ),
});

const makeStateQueueMergeRedeemerBuilder = ({
  layout,
  confirmedUTxO,
  firstBlockUTxO,
  headerNodeKey,
  blockHeader,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly confirmedUTxO: StateQueueUTxO;
  readonly firstBlockUTxO: StateQueueUTxO;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
}): RedeemerBuilder => ({
  kind: "selected",
  inputs: [firstBlockUTxO.utxo, confirmedUTxO.utxo],
  makeRedeemer: (inputIndices) => {
    const headerNodeInputIndex = inputIndices[0];
    const confirmedStateInputIndex = inputIndices[1];
    if (
      headerNodeInputIndex === undefined ||
      confirmedStateInputIndex === undefined ||
      inputIndices.length !== 2
    ) {
      throw new Error(
        `Merge state_queue redeemer builder expected header and confirmed-state input indices, got ${inputIndices.length.toString()}`,
      );
    }
    return Data.to(
      makeStateQueueMergeRedeemer({
        layout: {
          ...layout,
          headerNodeInputIndex: Number(headerNodeInputIndex),
          confirmedStateInputIndex: Number(confirmedStateInputIndex),
        },
        headerNodeKey,
        blockHeader,
      }),
      StateQueueRedeemer,
    );
  },
});

const deriveMergeLayoutFromTx = ({
  tx,
  seedLayout,
  confirmedUTxO,
  firstBlockUTxO,
  hubOracleRefInput,
  stateQueueAddress,
  encodedConfirmedNodeDatum,
  settlementAddress,
  encodedSettlementDatum,
  settlementUnit,
  stateQueueMintPointerIndex,
  settlementMintPointerIndex,
}: {
  readonly tx: CML.Transaction;
  readonly seedLayout: MergeRedeemerLayout;
  readonly confirmedUTxO: StateQueueUTxO;
  readonly firstBlockUTxO: StateQueueUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly stateQueueAddress: string;
  readonly encodedConfirmedNodeDatum: string;
  readonly settlementAddress: string;
  readonly encodedSettlementDatum: string;
  readonly settlementUnit: string;
  readonly stateQueueMintPointerIndex: number;
  readonly settlementMintPointerIndex: number;
}): MergeLayoutDerivation => {
  const txBody = tx.body();
  const inputList = collectSortedInputOutRefs(txBody.inputs());
  const referenceInputList = txBody.reference_inputs();
  if (referenceInputList === undefined) {
    throw new Error("Merge tx did not include reference inputs");
  }
  const sortedReferenceInputList =
    collectSortedInputOutRefs(referenceInputList);
  const headerInput = findOutRefIndex(inputList, firstBlockUTxO.utxo);
  const confirmedInput = findOutRefIndex(inputList, confirmedUTxO.utxo);
  const hubOracleRefInputIndex = findOutRefIndex(
    sortedReferenceInputList,
    hubOracleRefInput,
  );
  if (
    headerInput === undefined ||
    confirmedInput === undefined ||
    hubOracleRefInputIndex === undefined
  ) {
    throw new Error(
      `Merge tx missing expected input index mapping (header=${headerInput},confirmed=${confirmedInput},hub_ref=${hubOracleRefInputIndex})`,
    );
  }

  const indexedOutputs = collectIndexedOutputs(txBody.outputs());
  const confirmedOutput = indexedOutputs.find(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === encodedConfirmedNodeDatum,
  );
  const settlementOutput = indexedOutputs.find(
    (output) =>
      output.address === settlementAddress &&
      output.datum === encodedSettlementDatum &&
      (output.assets[settlementUnit] ?? 0n) === 1n,
  );
  if (confirmedOutput === undefined || settlementOutput === undefined) {
    throw new Error(
      `Merge tx missing expected outputs (confirmed=${confirmedOutput?.index ?? "missing"},settlement=${settlementOutput?.index ?? "missing"})`,
    );
  }

  const redeemerPointers = getRedeemerPointersInContextOrder(tx);
  const txInfoRedeemerIndexes = getTxInfoRedeemerIndexes(redeemerPointers);
  const stateQueueRedeemerPointer = {
    tag: CML.RedeemerTag.Mint,
    index: BigInt(stateQueueMintPointerIndex),
  };
  const settlementRedeemerPointer = {
    tag: CML.RedeemerTag.Mint,
    index: BigInt(settlementMintPointerIndex),
  };
  const stateQueueRedeemerIndex = Number(
    resolveRedeemerTxInfoIndex({
      pointers: redeemerPointers,
      target: stateQueueRedeemerPointer,
      label: "state_queue merge mint redeemer",
    }),
  );
  const settlementRedeemerIndex = Number(
    resolveRedeemerTxInfoIndex({
      pointers: redeemerPointers,
      target: settlementRedeemerPointer,
      label: "settlement spawn mint redeemer",
    }),
  );

  return {
    layout: {
      headerNodeInputIndex: headerInput,
      confirmedStateInputIndex: confirmedInput,
      confirmedStateOutputIndex: confirmedOutput.index,
      settlementOutputIndex: settlementOutput.index,
      stateQueueRedeemerIndex,
      settlementRedeemerIndex,
      hubOracleRefInputIndex,
    },
    diagnostics: {
      initialLayout: seedLayout,
      redeemerPointersContextOrder: redeemerPointers.map(
        (pointer, index) =>
          `${index}:${pointer.tag.toString()}:${pointer.index.toString()}`,
      ),
      redeemerPointersTxInfoOrder: redeemerPointers
        .map((pointer, contextIndex) => ({
          pointer,
          contextIndex,
          txInfoIndex: txInfoRedeemerIndexes[contextIndex]!,
        }))
        .sort((a, b) => a.txInfoIndex - b.txInfoIndex)
        .map(
          ({ pointer, contextIndex, txInfoIndex }) =>
            `${txInfoIndex}:${pointer.tag.toString()}:${pointer.index.toString()}(context=${contextIndex})`,
        ),
      stateQueueRedeemerTxInfoIndex: stateQueueRedeemerIndex,
      settlementRedeemerTxInfoIndex: settlementRedeemerIndex,
      stateQueueRedeemerCbor:
        findRedeemerDataCbor(tx, stateQueueRedeemerPointer) ?? "missing",
      settlementRedeemerCbor:
        findRedeemerDataCbor(tx, settlementRedeemerPointer) ?? "missing",
    },
  };
};

const assertMergeRedeemerInvariants = ({
  layout,
  headerNodeKey,
  blockHeader,
  encodedStateQueueMergeRedeemer,
  encodedSettlementSpawnRedeemer,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly encodedStateQueueMergeRedeemer: string;
  readonly encodedSettlementSpawnRedeemer: string;
}): void => {
  const decodedStateQueue = Data.from(
    encodedStateQueueMergeRedeemer,
    StateQueueRedeemer,
  ) as StateQueueRedeemerType;
  const decodedSettlement = Data.from(
    encodedSettlementSpawnRedeemer,
    SettlementMintRedeemer,
  ) as SettlementMintRedeemerType;
  const mismatches: string[] = [];

  if (!("MergeToConfirmedState" in decodedStateQueue)) {
    mismatches.push("state_queue variant mismatch");
  } else {
    const stateQueueMerge = decodedStateQueue.MergeToConfirmedState;
    if (stateQueueMerge.header_node_key !== headerNodeKey) {
      mismatches.push("state_queue.header_node_key mismatch");
    }
    if (
      stateQueueMerge.header_node_input_index !==
      BigInt(layout.headerNodeInputIndex)
    ) {
      mismatches.push("state_queue.header_node_input_index mismatch");
    }
    if (
      stateQueueMerge.confirmed_state_input_index !==
      BigInt(layout.confirmedStateInputIndex)
    ) {
      mismatches.push("state_queue.confirmed_state_input_index mismatch");
    }
    if (
      stateQueueMerge.confirmed_state_output_index !==
      BigInt(layout.confirmedStateOutputIndex)
    ) {
      mismatches.push("state_queue.confirmed_state_output_index mismatch");
    }
    if (
      stateQueueMerge.m_settlement_redeemer_index !==
      BigInt(layout.settlementRedeemerIndex)
    ) {
      mismatches.push("state_queue.m_settlement_redeemer_index mismatch");
    }
    if (
      stateQueueMerge.merged_block_transactions_root !==
      blockHeader.transactionsRoot
    ) {
      mismatches.push("state_queue.transactions_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_deposits_root !== blockHeader.depositsRoot
    ) {
      mismatches.push("state_queue.deposits_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_withdrawals_root !==
      blockHeader.withdrawalsRoot
    ) {
      mismatches.push("state_queue.withdrawals_root mismatch");
    }
  }

  if (!("Spawn" in decodedSettlement)) {
    mismatches.push("settlement variant mismatch");
  } else {
    const settlementSpawn = decodedSettlement.Spawn;
    if (settlementSpawn.settlement_id !== headerNodeKey) {
      mismatches.push("settlement.settlement_id mismatch");
    }
    if (settlementSpawn.output_index !== BigInt(layout.settlementOutputIndex)) {
      mismatches.push("settlement.output_index mismatch");
    }
    if (
      settlementSpawn.state_queue_merge_redeemer_index !==
      BigInt(layout.stateQueueRedeemerIndex)
    ) {
      mismatches.push("settlement.state_queue_merge_redeemer_index mismatch");
    }
    if (
      settlementSpawn.hub_ref_input_index !==
      BigInt(layout.hubOracleRefInputIndex)
    ) {
      mismatches.push("settlement.hub_ref_input_index mismatch");
    }
  }

  if (mismatches.length > 0) {
    throw new Error(JSON.stringify({ mismatches, layout }));
  }
};

export const buildProductionMergeToConfirmedStateTxProgram = ({
  lucid,
  fetchConfig,
  contracts,
  confirmedUTxO,
  firstBlockUTxO,
  validFrom,
  feeInput,
  hubOracleRefInput,
  referenceScripts,
  settlementOutputLovelace = MIN_SETTLEMENT_OUTPUT_LOVELACE,
}: ProductionMergeToConfirmedStateParams): Effect.Effect<
  ProductionMergeToConfirmedStateResult,
  StateQueueError | DataCoercionError | HashingError
> =>
  Effect.gen(function* () {
    const blockHeader = yield* getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    if (firstBlockUTxO.datum.key === "Empty") {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to build merge transaction",
          cause: "first queued block cannot be a root node",
        }),
      );
    }
    const headerNodeKey = firstBlockUTxO.datum.key.Key.key;
    const recomputedHeaderHash = yield* hashBlockHeader(blockHeader);
    if (recomputedHeaderHash !== headerNodeKey) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Failed to build merge transaction: queued block key/hash mismatch",
          cause: `datumKey=${headerNodeKey},computed=${recomputedHeaderHash}`,
        }),
      );
    }

    const { data: confirmedStateData } =
      yield* getConfirmedStateFromStateQueueDatum(confirmedUTxO.datum);
    const updatedConfirmedState: ConfirmedState = {
      headerHash: headerNodeKey,
      prevHeaderHash: confirmedStateData.headerHash,
      utxoRoot: blockHeader.utxosRoot,
      startTime: confirmedStateData.startTime,
      endTime: blockHeader.endTime,
      protocolVersion: blockHeader.protocolVersion,
    };
    const updatedConfirmedNodeDatum: LinkedListNodeView = {
      ...confirmedUTxO.datum,
      data: castConfirmedStateToData(
        updatedConfirmedState,
      ) as LinkedListNodeView["data"],
      next: firstBlockUTxO.datum.next,
    };

    const stateQueueAssetsToBurn: Assets = {
      [toUnit(fetchConfig.stateQueuePolicyId, firstBlockUTxO.assetName)]: -1n,
    };
    const settlementUnit = toUnit(contracts.settlement.policyId, headerNodeKey);
    const settlementAssetsToMint: Assets = { [settlementUnit]: 1n };
    const settlementOutputAssets: Assets = {
      lovelace: settlementOutputLovelace,
      ...settlementAssetsToMint,
    };
    const settlementDatum = {
      deposits_root: blockHeader.depositsRoot,
      withdrawals_root: blockHeader.withdrawalsRoot,
      transactions_root: blockHeader.transactionsRoot,
      resolution_claim: null,
    };
    const encodedConfirmedNodeDatum = encodeLinkedListNodeView(
      updatedConfirmedNodeDatum,
    );
    const encodedSettlementDatum = Data.to(settlementDatum, SettlementDatum);
    const mergeReferenceInputs = [
      hubOracleRefInput,
      ...(referenceScripts?.stateQueueSpending === undefined
        ? []
        : [referenceScripts.stateQueueSpending]),
      ...(referenceScripts?.stateQueueMinting === undefined
        ? []
        : [referenceScripts.stateQueueMinting]),
      ...(referenceScripts?.settlementMinting === undefined
        ? []
        : [referenceScripts.settlementMinting]),
    ];

    const makeMergeTx = (
      encodedStateQueueMergeRedeemer: string | RedeemerBuilder,
      encodedSettlementSpawnRedeemer: string | RedeemerBuilder,
    ) =>
      lucid
        .newTx()
        .validFrom(validFrom)
        .collectFrom([confirmedUTxO.utxo, firstBlockUTxO.utxo], Data.void())
        .collectFrom([feeInput])
        .readFrom(mergeReferenceInputs)
        .pay.ToContract(
          fetchConfig.stateQueueAddress,
          { kind: "inline", value: encodedConfirmedNodeDatum },
          confirmedUTxO.utxo.assets,
        )
        .pay.ToContract(
          contracts.settlement.spendingScriptAddress,
          { kind: "inline", value: encodedSettlementDatum },
          settlementOutputAssets,
        )
        .mintAssets(stateQueueAssetsToBurn, encodedStateQueueMergeRedeemer)
        .mintAssets(settlementAssetsToMint, encodedSettlementSpawnRedeemer);

    const makeMergeTxWithScripts = (
      encodedStateQueueMergeRedeemer: string | RedeemerBuilder,
      encodedSettlementSpawnRedeemer: string | RedeemerBuilder,
    ) => {
      const tx = makeMergeTx(
        encodedStateQueueMergeRedeemer,
        encodedSettlementSpawnRedeemer,
      );
      const withStateQueueSpendingScript =
        referenceScripts?.stateQueueSpending === undefined
          ? tx.attach.Script(contracts.stateQueue.spendingScript)
          : tx;
      const withStateQueueMintingScript =
        referenceScripts?.stateQueueMinting === undefined
          ? withStateQueueSpendingScript.attach.Script(
              contracts.stateQueue.mintingScript,
            )
          : withStateQueueSpendingScript;
      return referenceScripts?.settlementMinting === undefined
        ? withStateQueueMintingScript.attach.Script(
            contracts.settlement.mintingScript,
          )
        : withStateQueueMintingScript;
    };

    type UnevaluatedDraftBuilder = ReturnType<typeof makeMergeTxWithScripts> & {
      readonly config: () => Promise<unknown>;
      readonly rawConfig: () => {
        readonly txBuilder: {
          readonly build_for_evaluation: (
            fee: number,
            changeAddress: ReturnType<typeof CML.Address.from_bech32>,
          ) => {
            readonly draft_tx: () => CML.Transaction;
          };
        };
      };
    };
    const buildUnevaluatedDraftTx = async (
      encodedStateQueueMergeRedeemer: string,
      encodedSettlementSpawnRedeemer: string,
    ): Promise<CML.Transaction> => {
      const tx = makeMergeTxWithScripts(
        encodedStateQueueMergeRedeemer,
        encodedSettlementSpawnRedeemer,
      ) as UnevaluatedDraftBuilder;
      await tx.config();
      const walletAddress = await lucid.wallet().address();
      return tx
        .rawConfig()
        .txBuilder.build_for_evaluation(
          0,
          CML.Address.from_bech32(walletAddress),
        )
        .draft_tx();
    };

    const seedIndexes = deriveInitialMergeRedeemerSeedIndexes({
      stateQueuePolicyId: fetchConfig.stateQueuePolicyId,
      settlementPolicyId: contracts.settlement.policyId,
    });
    const initialHubOracleRefInputIndex = findOutRefIndex(
      [...mergeReferenceInputs].sort(compareOutRefs),
      hubOracleRefInput,
    );
    if (initialHubOracleRefInputIndex === undefined) {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to derive initial merge hub-oracle reference index",
          cause: "hub-oracle reference input missing from merge reference set",
        }),
      );
    }
    const initialLayout: MergeRedeemerLayout = {
      headerNodeInputIndex: 0,
      confirmedStateInputIndex: 1,
      confirmedStateOutputIndex: 0,
      settlementOutputIndex: 1,
      stateQueueRedeemerIndex: seedIndexes.stateQueueRedeemerIndex,
      settlementRedeemerIndex: seedIndexes.settlementRedeemerIndex,
      hubOracleRefInputIndex: initialHubOracleRefInputIndex,
    };
    const derivedLayout = yield* Effect.tryPromise({
      try: async () => {
        const encoded = encodeMergeRedeemers({
          layout: initialLayout,
          headerNodeKey,
          blockHeader,
        });
        const draftTx = await buildUnevaluatedDraftTx(
          encoded.stateQueue,
          encoded.settlement,
        );
        return deriveMergeLayoutFromTx({
          tx: draftTx,
          seedLayout: initialLayout,
          confirmedUTxO,
          firstBlockUTxO,
          hubOracleRefInput,
          stateQueueAddress: fetchConfig.stateQueueAddress,
          encodedConfirmedNodeDatum,
          settlementAddress: contracts.settlement.spendingScriptAddress,
          encodedSettlementDatum,
          settlementUnit,
          stateQueueMintPointerIndex: seedIndexes.stateQueueMintPointerIndex,
          settlementMintPointerIndex: seedIndexes.settlementMintPointerIndex,
        });
      },
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_LAYOUT_DERIVATION_FAILED",
          `Failed to derive merge redeemer layout from balanced draft tx: ${formatUnknownError(cause)}`,
          { cause: formatUnknownError(cause), initialLayout },
        ),
    });
    const finalLayout = derivedLayout.layout;
    yield* Effect.logInfo(
      `🔸 Merge redeemer layout: header_input=${finalLayout.headerNodeInputIndex},confirmed_input=${finalLayout.confirmedStateInputIndex},confirmed_output=${finalLayout.confirmedStateOutputIndex},settlement_output=${finalLayout.settlementOutputIndex},hub_ref_input=${finalLayout.hubOracleRefInputIndex},state_queue_redeemer_index=${finalLayout.stateQueueRedeemerIndex},settlement_redeemer_index=${finalLayout.settlementRedeemerIndex}`,
    );

    const finalEncodedRedeemers = encodeMergeRedeemers({
      layout: finalLayout,
      headerNodeKey,
      blockHeader,
    });
    yield* Effect.try({
      try: () =>
        assertMergeRedeemerInvariants({
          layout: finalLayout,
          headerNodeKey,
          blockHeader,
          encodedStateQueueMergeRedeemer: finalEncodedRedeemers.stateQueue,
          encodedSettlementSpawnRedeemer: finalEncodedRedeemers.settlement,
        }),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Failed merge redeemer invariant checks",
          { cause: formatUnknownError(cause), layout: finalLayout },
        ),
    });

    const stateQueueRedeemerBuilder = makeStateQueueMergeRedeemerBuilder({
      layout: finalLayout,
      confirmedUTxO,
      firstBlockUTxO,
      headerNodeKey,
      blockHeader,
    });
    const txBuilder = yield* Effect.tryPromise({
      try: () =>
        makeMergeTxWithScripts(
          stateQueueRedeemerBuilder,
          finalEncodedRedeemers.settlement,
        ).complete({ localUPLCEval: true }),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_UPLC_EVAL_FAILED",
          "Failed to finalize the transaction for merging oldest block into confirmed state",
          {
            remote: formatUnknownError(cause),
            layout: finalLayout,
            draftDiagnostics: derivedLayout.diagnostics,
          },
        ),
    });

    const finalLayoutCheck = yield* Effect.try({
      try: () =>
        deriveMergeLayoutFromTx({
          tx: txBuilder.toTransaction(),
          seedLayout: finalLayout,
          confirmedUTxO,
          firstBlockUTxO,
          hubOracleRefInput,
          stateQueueAddress: fetchConfig.stateQueueAddress,
          encodedConfirmedNodeDatum,
          settlementAddress: contracts.settlement.spendingScriptAddress,
          encodedSettlementDatum,
          settlementUnit,
          stateQueueMintPointerIndex: seedIndexes.stateQueueMintPointerIndex,
          settlementMintPointerIndex: seedIndexes.settlementMintPointerIndex,
        }),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Failed final merge transaction redeemer-layout verification",
          { cause: formatUnknownError(cause), draftLayout: finalLayout },
        ),
    });
    const actualLayout = finalLayoutCheck.layout;
    const mismatches = [
      actualLayout.headerNodeInputIndex === finalLayout.headerNodeInputIndex
        ? undefined
        : "headerNodeInputIndex",
      actualLayout.confirmedStateInputIndex ===
      finalLayout.confirmedStateInputIndex
        ? undefined
        : "confirmedStateInputIndex",
      actualLayout.confirmedStateOutputIndex ===
      finalLayout.confirmedStateOutputIndex
        ? undefined
        : "confirmedStateOutputIndex",
      actualLayout.settlementOutputIndex === finalLayout.settlementOutputIndex
        ? undefined
        : "settlementOutputIndex",
      actualLayout.stateQueueRedeemerIndex ===
      finalLayout.stateQueueRedeemerIndex
        ? undefined
        : "stateQueueRedeemerIndex",
      actualLayout.settlementRedeemerIndex ===
      finalLayout.settlementRedeemerIndex
        ? undefined
        : "settlementRedeemerIndex",
      actualLayout.hubOracleRefInputIndex === finalLayout.hubOracleRefInputIndex
        ? undefined
        : "hubOracleRefInputIndex",
    ].filter((field): field is string => field !== undefined);
    if (mismatches.length > 0) {
      return yield* Effect.fail(
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Final merge transaction layout drifted after balancing",
          {
            mismatches,
            expected: finalLayout,
            actual: actualLayout,
            diagnostics: finalLayoutCheck.diagnostics,
          },
        ),
      );
    }

    return {
      tx: txBuilder,
      headerNodeKey,
      blockHeader,
      layout: finalLayout,
      diagnostics: derivedLayout.diagnostics,
    };
  });

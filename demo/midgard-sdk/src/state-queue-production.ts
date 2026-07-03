import { assetsEqual } from "@al-ft/midgard-core/assets";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type Assets,
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Script,
  toUnit,
  type TxBuilder,
  type TxOutput,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  castActiveOperatorDatumToData,
} from "@/active-operators.js";
import type {
  DataCoercionError,
  HashingError,
  MidgardValidators,
  OutputReference,
} from "@/common.js";
import { outputReferenceFromUTxO } from "@/common.js";
import {
  castConfirmedStateToData,
  castStateQueueNodeToData,
  type ConfirmedState,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  type Header,
  NO_DA_ATTESTATION,
} from "@/ledger-state.js";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  encodeLinkedListNodeView,
  LinkedListDatum,
  linkedListDatumToNodeView,
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
  type StateQueueFetchConfig,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
  StateQueueSpendRedeemer,
  type StateQueueUTxO,
} from "@/state-queue.js";
import { completeOptionsWithLocalEval } from "@/tx-completion.js";
import {
  requireInputIndex as requireContextInputIndex,
  requireMintRedeemerIndex as requireContextMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex as requireContextReferenceInputIndex,
  requireSpendRedeemerIndex as requireContextSpendRedeemerIndex,
} from "@/tx-context-redeemer.js";
import { outputDatumCborMatches } from "@/tx-output-utils.js";

const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;
const ACTIVE_OPERATOR_MATURITY_DURATION_MS = 30n;
const MIN_SETTLEMENT_OUTPUT_LOVELACE = 5_000_000n;

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

type StateQueueCommitLayout = {
  readonly schedulerRefInputIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueMintRedeemerIndex: bigint;
};

type StateQueueCommitRedeemer = {
  readonly CommitBlockHeader: {
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
    readonly state_queue_redeemer_index: bigint;
  };
};

export const requireOperatorWalletInputs = (
  walletUtxos: readonly UTxO[],
  transactionLabel: string,
): Effect.Effect<readonly UTxO[], StateQueueError> =>
  Effect.gen(function* () {
    if (walletUtxos.length === 0) {
      return yield* Effect.fail(
        new StateQueueError({
          message: `No operator wallet inputs available to fund ${transactionLabel}`,
          cause: "operator wallet has no available UTxO",
        }),
      );
    }
    return walletUtxos;
  });

const availableOperatorWalletUtxos = (
  view: OperatorWalletViewLike,
): readonly UTxO[] => {
  const consumedOutRefs = new Set(view.consumedOutRefs);
  return view.knownUtxos.filter(
    (utxo) => !consumedOutRefs.has(outRefLabel(utxo)),
  );
};

const decodeActiveOperatorDatum = (data: unknown): ActiveOperatorDatum =>
  Data.castFrom(
    data as never,
    ActiveOperatorDatum as never,
  ) as ActiveOperatorDatum;

const makeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): StateQueueCommitRedeemer => ({
  CommitBlockHeader: {
    new_block_output_index: layout.newBlockOutputIndex,
    continued_latest_block_output_index: layout.continuedLatestBlockOutputIndex,
    operator: operatorKeyHash,
    scheduler_ref_input_index: layout.schedulerRefInputIndex,
    active_operators_input_index: layout.activeOperatorsInputIndex,
    active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
  },
});

const makeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): ActiveOperatorCommitRedeemer => ({
  UpdateBondHoldNewState: {
    active_operator: operatorKeyHash,
    active_node_input_index: layout.activeOperatorsInputIndex,
    active_node_output_index: layout.activeOperatorOutputIndex,
    hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    state_queue_redeemer_index: layout.stateQueueMintRedeemerIndex,
  },
});

const encodeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): string =>
  Data.to(
    makeStateQueueCommitRedeemer(operatorKeyHash, layout) as never,
    StateQueueRedeemer as never,
  );

const encodeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): string =>
  Data.to(
    makeActiveOperatorCommitRedeemer(operatorKeyHash, layout) as never,
    ActiveOperatorSpendRedeemer as never,
  );

const encodeStateQueueLinkedListMutationSpendRedeemer = (): string =>
  Data.to("LinkedListMutation" as never, StateQueueSpendRedeemer as never);

type CommitLayoutLike = {
  readonly schedulerRefInputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly stateQueueMintRedeemerIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
};

const COMMIT_LAYOUT_FIELDS = [
  { key: "schedulerRefInputIndex", label: "scheduler_ref_input_index" },
  { key: "activeOperatorsInputIndex", label: "active_operators_input_index" },
  {
    key: "activeOperatorsRedeemerIndex",
    label: "active_operators_redeemer_index",
  },
  {
    key: "stateQueueMintRedeemerIndex",
    label: "state_queue_mint_redeemer_index",
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

const requireUniqueContextOutputIndex = (
  outputs: readonly TxOutput[],
  predicate: (output: TxOutput) => boolean,
  label: string,
): bigint => {
  let foundIndex: bigint | undefined;
  for (let index = 0; index < outputs.length; index += 1) {
    if (!predicate(outputs[index]!)) {
      continue;
    }
    if (foundIndex !== undefined) {
      throw new Error(`${label} output selector matched multiple outputs`);
    }
    foundIndex = BigInt(index);
  }
  if (foundIndex === undefined) {
    throw new Error(`${label} output is missing from final tx outputs`);
  }
  return foundIndex;
};

const deriveCommitLayoutFromRedeemerContext = ({
  ctx,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  stateQueuePolicyId,
  stateQueueAddress,
  headerNodeUnit,
  headerNodeDatum,
  previousHeaderNodeDatum,
}: {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly headerNodeUnit: string;
  readonly headerNodeDatum: string;
  readonly previousHeaderNodeDatum: string;
}): StateQueueCommitLayout => {
  const activeOperatorsInputIndex = requireContextInputIndex(
    ctx,
    activeOperatorInput,
    "state-queue commit active operator",
  );
  return {
    schedulerRefInputIndex: requireContextReferenceInputIndex(
      ctx,
      schedulerRefInput,
      "state-queue commit scheduler",
    ),
    newBlockOutputIndex: requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === stateQueueAddress &&
        outputDatumCborMatches(output, headerNodeDatum) &&
        (output.assets[headerNodeUnit] ?? 0n) === 1n,
      "state-queue commit new header",
    ),
    continuedLatestBlockOutputIndex: requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === stateQueueAddress &&
        outputDatumCborMatches(output, previousHeaderNodeDatum),
      "state-queue commit continued latest header",
    ),
    activeOperatorsInputIndex,
    activeOperatorsRedeemerIndex: requireContextSpendRedeemerIndex(
      ctx,
      activeOperatorInput,
      "state-queue commit active operator",
    ),
    activeOperatorOutputIndex: requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === activeOperatorInput.address &&
        assetsEqual(output.assets, activeOperatorInput.assets),
      "state-queue commit active operator",
    ),
    hubOracleRefInputIndex: requireContextReferenceInputIndex(
      ctx,
      hubOracleRefInput,
      "state-queue commit hub oracle",
    ),
    stateQueueMintRedeemerIndex: requireContextMintRedeemerIndex(
      ctx,
      stateQueuePolicyId,
      "state-queue commit mint",
    ),
  };
};

export type DeterministicCommitTxBuilderInput = {
  readonly contracts: MidgardValidators;
  readonly witness: StateQueueCommitWitnessContext;
  readonly headerNodeUnit: string;
  readonly appendedNodeDatumCbor: string;
  readonly previousHeaderNodeDatumCbor: string;
  readonly updatedActiveOperatorDatumCbor: string;
  readonly commitMintAssets: Readonly<Record<string, bigint>>;
  readonly makeBaseCommitTx: (
    stateQueueCommitSpendRedeemer: BuildTxWithRedeemer | string,
  ) => TxBuilder;
};

export const buildDeterministicCommitTxBuilder = ({
  contracts,
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
    const presetWalletInputs = yield* requireOperatorWalletInputs(
      availableOperatorWalletUtxos(witness.operatorWalletView),
      "state_queue commit tx",
    );
    yield* Effect.logInfo(
      `🔹 Using ${presetWalletInputs.length.toString()} preset operator wallet input(s) for state_queue commit tx.`,
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

    let commitLayout: StateQueueCommitLayout | undefined;
    const layoutFromContext = (
      ctx: Parameters<BuildTxWithRedeemer>[0],
    ): StateQueueCommitLayout => {
      const layout = deriveCommitLayoutFromRedeemerContext({
        ctx,
        schedulerRefInput: witness.schedulerRefInput,
        hubOracleRefInput: witness.hubOracleRefInput,
        activeOperatorInput: witness.activeOperatorInput,
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        headerNodeUnit,
        headerNodeDatum: appendedNodeDatumCbor,
        previousHeaderNodeDatum: previousHeaderNodeDatumCbor,
      });
      commitLayout = layout;
      return layout;
    };
    const stateQueueCommitSpendRedeemer = (() =>
      encodeStateQueueLinkedListMutationSpendRedeemer()) satisfies BuildTxWithRedeemer;
    const stateQueueCommitMintRedeemer = ((ctx) =>
      encodeStateQueueCommitRedeemer(
        witness.operatorKeyHash,
        layoutFromContext(ctx),
      )) satisfies BuildTxWithRedeemer;
    const activeOperatorCommitRedeemer = ((ctx) =>
      encodeActiveOperatorCommitRedeemer(
        witness.operatorKeyHash,
        layoutFromContext(ctx),
      )) satisfies BuildTxWithRedeemer;

    const makeCommitTx = () => {
      const tx = makeBaseCommitTx(stateQueueCommitSpendRedeemer)
        .readFrom(referenceInputs)
        .collectFrom(
          [witness.activeOperatorInput],
          activeOperatorCommitRedeemer,
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
        .mintAssets(commitMintAssets, stateQueueCommitMintRedeemer);
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

    const builtCommitTx = yield* Effect.tryPromise({
      try: () =>
        makeCommitTx().complete(
          completeOptionsWithLocalEval({ presetWalletInputs }),
        ),
      catch: (cause) =>
        new StateQueueError({
          message: `Failed to build block header commitment transaction with final redeemer context: ${formatUnknownError(
            cause,
          )}`,
          cause,
        }),
    });
    if (commitLayout === undefined) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "BuildTxWithRedeemer did not resolve state-queue commit layout",
          cause: "missing BuildTxWithRedeemer commit layout callback",
        }),
      );
    }
    yield* Effect.logInfo(
      `🔹 Using commit redeemer layout: ${formatCommitLayout(commitLayout)}`,
    );

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
      data: castStateQueueNodeToData({
        header: newHeader,
        da_attestation: NO_DA_ATTESTATION,
      }) as LinkedListNodeView["data"],
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

    const makeBaseCommitTx = (
      stateQueueCommitRedeemer: BuildTxWithRedeemer | string,
    ) =>
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
      contracts,
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
  readonly presetWalletInputs?: readonly UTxO[];
  readonly hubOracleRefInput: UTxO;
  readonly referenceScripts?: StateQueueMergeReferenceScripts;
  readonly settlementOutputLovelace?: bigint;
};

export type MergeRedeemerLayout = {
  readonly confirmedStateOutputIndex: number;
  readonly settlementOutputIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
  readonly hubOracleRefInputIndex: number;
};

export type MergeLayoutDiagnostics = {
  readonly stateQueueRedeemerTxInfoIndex: number;
  readonly settlementRedeemerTxInfoIndex: number;
  readonly stateQueueRedeemerCbor: string;
  readonly settlementRedeemerCbor: string;
};

export type ProductionMergeToConfirmedStateResult = {
  readonly tx: TxSignBuilder;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly layout: MergeRedeemerLayout;
  readonly diagnostics: MergeLayoutDiagnostics;
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
  confirmedStateInputOutRef,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly confirmedStateInputOutRef: OutputReference;
}): StateQueueRedeemerType => ({
  MergeToConfirmedState: {
    header_node_key: headerNodeKey,
    confirmed_state_input_outref: confirmedStateInputOutRef,
    confirmed_state_output_index: BigInt(layout.confirmedStateOutputIndex),
    m_settlement_redeemer_index: BigInt(layout.settlementRedeemerIndex),
    merged_block_withdrawals_root: blockHeader.withdrawalsRoot,
    merged_block_forced_transactions_root: blockHeader.forcedTransactionsRoot,
    merged_block_transactions_root: blockHeader.transactionsRoot,
    merged_block_deposits_root: blockHeader.depositsRoot,
    merged_block_transition_trace_root: blockHeader.transitionTraceRoot,
    merged_block_event_to_step_root: blockHeader.eventToStepRoot,
    merged_block_withdrawal_count: blockHeader.withdrawalCount,
    merged_block_forced_transaction_count: blockHeader.forcedTransactionCount,
    merged_block_l2_transaction_count: blockHeader.l2TransactionCount,
    merged_block_deposit_count: blockHeader.depositCount,
    merged_block_total_event_count: blockHeader.totalEventCount,
    merged_block_transition_step_count: blockHeader.transitionStepCount,
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

const deriveMergeLayoutFromRedeemerContext = ({
  ctx,
  confirmedUTxO,
  hubOracleRefInput,
  stateQueuePolicyId,
  stateQueueAddress,
  encodedConfirmedNodeDatum,
  settlementPolicyId,
  settlementAddress,
  encodedSettlementDatum,
  settlementOutputAssets,
}: {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly confirmedUTxO: StateQueueUTxO;
  readonly hubOracleRefInput: UTxO;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly encodedConfirmedNodeDatum: string;
  readonly settlementPolicyId: string;
  readonly settlementAddress: string;
  readonly encodedSettlementDatum: string;
  readonly settlementOutputAssets: Assets;
}): MergeRedeemerLayout => ({
  confirmedStateOutputIndex: Number(
    requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === stateQueueAddress &&
        outputDatumCborMatches(output, encodedConfirmedNodeDatum) &&
        assetsEqual(output.assets, confirmedUTxO.utxo.assets),
      "state-queue merge confirmed state",
    ),
  ),
  settlementOutputIndex: Number(
    requireUniqueContextOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === settlementAddress &&
        outputDatumCborMatches(output, encodedSettlementDatum) &&
        assetsEqual(output.assets, settlementOutputAssets),
      "state-queue merge settlement",
    ),
  ),
  stateQueueRedeemerIndex: Number(
    requireContextMintRedeemerIndex(
      ctx,
      stateQueuePolicyId,
      "state-queue merge state_queue mint",
    ),
  ),
  settlementRedeemerIndex: Number(
    requireContextMintRedeemerIndex(
      ctx,
      settlementPolicyId,
      "state-queue merge settlement mint",
    ),
  ),
  hubOracleRefInputIndex: Number(
    requireContextReferenceInputIndex(
      ctx,
      hubOracleRefInput,
      "state-queue merge hub oracle",
    ),
  ),
});

const assertMergeRedeemerInvariants = ({
  layout,
  headerNodeKey,
  blockHeader,
  confirmedStateInputOutRef,
  encodedStateQueueMergeRedeemer,
  encodedSettlementSpawnRedeemer,
}: {
  readonly layout: MergeRedeemerLayout;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly confirmedStateInputOutRef: OutputReference;
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

  if (
    typeof decodedStateQueue !== "object" ||
    decodedStateQueue === null ||
    !("MergeToConfirmedState" in decodedStateQueue)
  ) {
    mismatches.push("state_queue variant mismatch");
  } else {
    const stateQueueMerge = decodedStateQueue.MergeToConfirmedState;
    if (stateQueueMerge.header_node_key !== headerNodeKey) {
      mismatches.push("state_queue.header_node_key mismatch");
    }
    if (
      stateQueueMerge.confirmed_state_input_outref.transactionId !==
      confirmedStateInputOutRef.transactionId
    ) {
      mismatches.push("state_queue.confirmed_state_input_outref tx mismatch");
    }
    if (
      stateQueueMerge.confirmed_state_input_outref.outputIndex !==
      confirmedStateInputOutRef.outputIndex
    ) {
      mismatches.push(
        "state_queue.confirmed_state_input_outref index mismatch",
      );
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
      stateQueueMerge.merged_block_withdrawals_root !==
      blockHeader.withdrawalsRoot
    ) {
      mismatches.push("state_queue.withdrawals_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_forced_transactions_root !==
      blockHeader.forcedTransactionsRoot
    ) {
      mismatches.push("state_queue.forced_transactions_root mismatch");
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
      stateQueueMerge.merged_block_transition_trace_root !==
      blockHeader.transitionTraceRoot
    ) {
      mismatches.push("state_queue.transition_trace_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_event_to_step_root !==
      blockHeader.eventToStepRoot
    ) {
      mismatches.push("state_queue.event_to_step_root mismatch");
    }
    if (
      stateQueueMerge.merged_block_withdrawal_count !==
      blockHeader.withdrawalCount
    ) {
      mismatches.push("state_queue.withdrawal_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_forced_transaction_count !==
      blockHeader.forcedTransactionCount
    ) {
      mismatches.push("state_queue.forced_transaction_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_l2_transaction_count !==
      blockHeader.l2TransactionCount
    ) {
      mismatches.push("state_queue.l2_transaction_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_deposit_count !== blockHeader.depositCount
    ) {
      mismatches.push("state_queue.deposit_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_total_event_count !==
      blockHeader.totalEventCount
    ) {
      mismatches.push("state_queue.total_event_count mismatch");
    }
    if (
      stateQueueMerge.merged_block_transition_step_count !==
      blockHeader.transitionStepCount
    ) {
      mismatches.push("state_queue.transition_step_count mismatch");
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
  presetWalletInputs,
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
      forced_transactions_root: blockHeader.forcedTransactionsRoot,
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
      encodedStateQueueMergeRedeemer: BuildTxWithRedeemer | string,
      encodedSettlementSpawnRedeemer: BuildTxWithRedeemer | string,
    ) =>
      lucid
        .newTx()
        .validFrom(validFrom)
        .collectFrom(
          [confirmedUTxO.utxo, firstBlockUTxO.utxo],
          encodeStateQueueLinkedListMutationSpendRedeemer(),
        )
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
      encodedStateQueueMergeRedeemer: BuildTxWithRedeemer | string,
      encodedSettlementSpawnRedeemer: BuildTxWithRedeemer | string,
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

    let mergeLayout: MergeRedeemerLayout | undefined;
    let stateQueueRedeemerCbor: string | undefined;
    let settlementRedeemerCbor: string | undefined;
    const layoutFromContext = (
      ctx: Parameters<BuildTxWithRedeemer>[0],
    ): MergeRedeemerLayout => {
      const layout = deriveMergeLayoutFromRedeemerContext({
        ctx,
        confirmedUTxO,
        hubOracleRefInput,
        stateQueuePolicyId: fetchConfig.stateQueuePolicyId,
        stateQueueAddress: fetchConfig.stateQueueAddress,
        encodedConfirmedNodeDatum,
        settlementPolicyId: contracts.settlement.policyId,
        settlementAddress: contracts.settlement.spendingScriptAddress,
        encodedSettlementDatum,
        settlementOutputAssets,
      });
      mergeLayout = layout;
      return layout;
    };
    const stateQueueMergeRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        fetchConfig.stateQueuePolicyId,
        "state-queue merge state_queue mint",
      );
      const redeemer = Data.to(
        makeStateQueueMergeRedeemer({
          layout: layoutFromContext(ctx),
          headerNodeKey,
          blockHeader,
          confirmedStateInputOutRef: outputReferenceFromUTxO(
            confirmedUTxO.utxo,
          ),
        }),
        StateQueueRedeemer,
      );
      stateQueueRedeemerCbor = redeemer;
      return redeemer;
    }) satisfies BuildTxWithRedeemer;
    const settlementSpawnRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.settlement.policyId,
        "state-queue merge settlement mint",
      );
      const redeemer = Data.to(
        makeSettlementSpawnRedeemer({
          layout: layoutFromContext(ctx),
          headerNodeKey,
        }),
        SettlementMintRedeemer,
      );
      settlementRedeemerCbor = redeemer;
      return redeemer;
    }) satisfies BuildTxWithRedeemer;

    const txBuilder = yield* Effect.tryPromise({
      try: () =>
        makeMergeTxWithScripts(
          stateQueueMergeRedeemer,
          settlementSpawnRedeemer,
        ).complete(completeOptionsWithLocalEval({ presetWalletInputs })),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_UPLC_EVAL_FAILED",
          "Failed to finalize the transaction for merging oldest block into confirmed state",
          { remote: formatUnknownError(cause) },
        ),
    });
    if (
      mergeLayout === undefined ||
      stateQueueRedeemerCbor === undefined ||
      settlementRedeemerCbor === undefined
    ) {
      return yield* Effect.fail(
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "BuildTxWithRedeemer did not resolve final merge redeemer layout",
          {
            mergeLayoutResolved: mergeLayout !== undefined,
            stateQueueRedeemerResolved: stateQueueRedeemerCbor !== undefined,
            settlementRedeemerResolved: settlementRedeemerCbor !== undefined,
          },
        ),
      );
    }
    const resolvedMergeLayout = mergeLayout;
    const resolvedStateQueueRedeemerCbor = stateQueueRedeemerCbor;
    const resolvedSettlementRedeemerCbor = settlementRedeemerCbor;
    const diagnostics: MergeLayoutDiagnostics = {
      stateQueueRedeemerTxInfoIndex:
        resolvedMergeLayout.stateQueueRedeemerIndex,
      settlementRedeemerTxInfoIndex:
        resolvedMergeLayout.settlementRedeemerIndex,
      stateQueueRedeemerCbor: resolvedStateQueueRedeemerCbor,
      settlementRedeemerCbor: resolvedSettlementRedeemerCbor,
    };
    yield* Effect.logInfo(
      `🔸 Merge redeemer layout: confirmed_output=${resolvedMergeLayout.confirmedStateOutputIndex},settlement_output=${resolvedMergeLayout.settlementOutputIndex},hub_ref_input=${resolvedMergeLayout.hubOracleRefInputIndex},state_queue_redeemer_index=${resolvedMergeLayout.stateQueueRedeemerIndex},settlement_redeemer_index=${resolvedMergeLayout.settlementRedeemerIndex}`,
    );
    yield* Effect.try({
      try: () =>
        assertMergeRedeemerInvariants({
          layout: resolvedMergeLayout,
          headerNodeKey,
          blockHeader,
          confirmedStateInputOutRef: outputReferenceFromUTxO(
            confirmedUTxO.utxo,
          ),
          encodedStateQueueMergeRedeemer: resolvedStateQueueRedeemerCbor,
          encodedSettlementSpawnRedeemer: resolvedSettlementRedeemerCbor,
        }),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Failed final merge redeemer invariant checks",
          { cause: formatUnknownError(cause), layout: resolvedMergeLayout },
        ),
    });

    return {
      tx: txBuilder,
      headerNodeKey,
      blockHeader,
      layout: resolvedMergeLayout,
      diagnostics,
    };
  });

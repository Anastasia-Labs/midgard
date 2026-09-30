import { assetsEqual } from "@al-ft/midgard-core/assets";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  type BuildTxWithRedeemer,
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { MidgardValidators } from "./common.js";
import { type Header } from "./ledger-state.js";
import { type LinkedListNodeView } from "./linked-list.js";
import {
  encodeStateQueueYieldRedeemer,
  StateQueueError,
  type StateQueueUTxO,
} from "./state-queue.js";
import {
  availableOperatorWalletUtxos,
  encodeActiveOperatorCommitRedeemer,
  encodeStateQueueCommitRedeemer,
  encodeStateQueueLinkedListMutationSpendRedeemer,
  formatCommitLayout,
  requireOperatorWalletInputs,
  requireUniqueContextOutputIndex,
  type StateQueueCommitLayout,
  type StateQueueCommitWitnessContext,
} from "./state-queue-transactions.commit-layout-fields.js";
import { completeOptionsWithLocalEval } from "./tx-completion.js";
import {
  requireInputIndex as requireContextInputIndex,
  requireMintRedeemerIndex as requireContextMintRedeemerIndex,
  requireReferenceInputIndex as requireContextReferenceInputIndex,
  requireSpendRedeemerIndex as requireContextSpendRedeemerIndex,
} from "./tx-context-redeemer.js";
import { dedupeAndSortUtxos } from "./tx-out-ref-order.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

const deriveCommitLayoutFromRedeemerContext = ({
  ctx,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  confirmedStateRefInput,
  headStateQueueNodeRefInput,
  stateQueueCommitYieldScriptRef,
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
  readonly confirmedStateRefInput?: UTxO;
  readonly headStateQueueNodeRefInput?: UTxO;
  readonly stateQueueCommitYieldScriptRef: UTxO;
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
    yieldToRefInputIndex: requireContextReferenceInputIndex(
      ctx,
      stateQueueCommitYieldScriptRef,
      "state-queue commit yield target",
    ),
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
    confirmedStateRefInputIndex:
      confirmedStateRefInput === undefined
        ? null
        : requireContextReferenceInputIndex(
            ctx,
            confirmedStateRefInput,
            "state-queue commit confirmed-state root",
          ),
    headStateQueueNodeRefInputIndex:
      headStateQueueNodeRefInput === undefined
        ? null
        : requireContextReferenceInputIndex(
            ctx,
            headStateQueueNodeRefInput,
            "state-queue commit current head",
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
  readonly yieldRewardAddress: string;
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
  yieldRewardAddress,
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

    const referenceInputs = dedupeAndSortUtxos([
      witness.schedulerRefInput,
      witness.hubOracleRefInput,
      witness.correctionLockRefInput.utxo,
      witness.stateQueueCommitYieldScriptRef,
      ...(witness.activeOperatorsSpendingScriptRef === undefined
        ? []
        : [witness.activeOperatorsSpendingScriptRef]),
      ...(witness.stateQueueSpendingScriptRef === undefined
        ? []
        : [witness.stateQueueSpendingScriptRef]),
      ...(witness.stateQueueMintingScriptRef === undefined
        ? []
        : [witness.stateQueueMintingScriptRef]),
      ...(witness.confirmedStateRefInput === undefined
        ? []
        : [witness.confirmedStateRefInput]),
      ...(witness.headStateQueueNodeRefInput === undefined
        ? []
        : [witness.headStateQueueNodeRefInput]),
    ]);

    let commitLayout: StateQueueCommitLayout | undefined;
    const layoutFromContext = (
      ctx: Parameters<BuildTxWithRedeemer>[0],
    ): StateQueueCommitLayout => {
      const layout = deriveCommitLayoutFromRedeemerContext({
        ctx,
        schedulerRefInput: witness.schedulerRefInput,
        hubOracleRefInput: witness.hubOracleRefInput,
        activeOperatorInput: witness.activeOperatorInput,
        confirmedStateRefInput: witness.confirmedStateRefInput,
        headStateQueueNodeRefInput: witness.headStateQueueNodeRefInput,
        stateQueueCommitYieldScriptRef: witness.stateQueueCommitYieldScriptRef,
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
        .mintAssets(commitMintAssets, stateQueueCommitMintRedeemer)
        .withdraw(yieldRewardAddress, 0n, (() =>
          encodeStateQueueYieldRedeemer()) satisfies BuildTxWithRedeemer);
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

export type CommitBlockHeaderParams = {
  readonly lucid: LucidEvolution;
  readonly contracts: MidgardValidators;
  readonly latestBlock: StateQueueUTxO;
  readonly updatedNodeDatum: LinkedListNodeView;
  readonly newHeader: Header;
  readonly validFrom: number;
  readonly validTo: number;
  readonly witness: StateQueueCommitWitnessContext;
  readonly headerNodeLovelace?: bigint;
  readonly activeOperatorMaturityDurationMs?: bigint;
};

export type CommitBlockHeaderResult = {
  readonly tx: TxSignBuilder;
  readonly newHeaderHash: string;
};

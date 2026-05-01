/**
 * Deterministic commit-transaction assembly for the block worker.
 * This module owns fee-input selection, draft probing, redeemer-layout
 * convergence, and strict local UPLC-evaluated rebuilds for block commits.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { withStubbedProviderEvaluation } from "@/cml-redeemers.js";
import { formatUnknownError } from "@/error-format.js";
import { slotToUnixTimeForLucid } from "@/lucid-time.js";
import { availableOperatorWalletUtxos } from "@/operator-wallet-view.js";
import { outRefLabel } from "@/tx-context.js";
import {
  buildRealCommitDraftDiagnostics,
  deriveCommitLayoutFromDraftTx,
} from "@/workers/utils/commit-layout-diagnostics.js";
import {
  type StateQueueCommitLayout,
  deriveStateQueueCommitLayout,
  encodeActiveOperatorCommitRedeemer,
  encodeStateQueueCommitRedeemer,
} from "@/workers/utils/commit-redeemers.js";
import {
  type RealStateQueueWitnessContext,
  selectFeeInput,
} from "@/workers/utils/scheduler-refresh.js";

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

type CommitLayoutBuildLabel = "derived" | "tx-derived";

export type DeterministicCommitTxBuilderInput = {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly latestBlockInput: UTxO;
  readonly witness: RealStateQueueWitnessContext;
  readonly headerNodeUnit: string;
  readonly appendedNodeDatumCbor: string;
  readonly previousHeaderNodeDatumCbor: string;
  readonly updatedActiveOperatorDatumCbor: string;
  readonly commitMintAssets: Readonly<Record<string, bigint>>;
  readonly makeBaseCommitTx: (stateQueueCommitRedeemer: string) => TxBuilder;
};

const formatLayout = (layout: CommitLayoutLike): string =>
  `scheduler_ref_input_index=${layout.schedulerRefInputIndex.toString()},latest_block_input_index=${layout.latestBlockInputIndex.toString()},active_operators_input_index=${layout.activeOperatorsInputIndex.toString()},active_operators_redeemer_index=${layout.activeOperatorsRedeemerIndex.toString()},state_queue_spend_redeemer_index=${layout.stateQueueSpendRedeemerIndex.toString()},new_block_output_index=${layout.newBlockOutputIndex.toString()},continued_latest_block_output_index=${layout.continuedLatestBlockOutputIndex.toString()},active_operator_output_index=${layout.activeOperatorOutputIndex.toString()},hub_oracle_ref_input_index=${layout.hubOracleRefInputIndex.toString()}`;

const commitLayoutsEqual = (
  left: StateQueueCommitLayout,
  right: StateQueueCommitLayout,
): boolean =>
  left.schedulerRefInputIndex === right.schedulerRefInputIndex &&
  left.latestBlockInputIndex === right.latestBlockInputIndex &&
  left.activeOperatorsInputIndex === right.activeOperatorsInputIndex &&
  left.newBlockOutputIndex === right.newBlockOutputIndex &&
  left.continuedLatestBlockOutputIndex ===
    right.continuedLatestBlockOutputIndex &&
  left.activeOperatorsRedeemerIndex === right.activeOperatorsRedeemerIndex &&
  left.activeOperatorOutputIndex === right.activeOperatorOutputIndex &&
  left.hubOracleRefInputIndex === right.hubOracleRefInputIndex &&
  left.stateQueueSpendRedeemerIndex === right.stateQueueSpendRedeemerIndex;

const completeCommitTxForLayout = ({
  makeCommitTxForLayout,
  layout,
  label,
}: {
  readonly makeCommitTxForLayout: (
    commitLayout: StateQueueCommitLayout,
  ) => TxBuilder;
  readonly layout: StateQueueCommitLayout;
  readonly label: CommitLayoutBuildLabel;
}): Effect.Effect<TxSignBuilder, SDK.StateQueueError> => {
  const verb = label === "derived" ? "build" : "rebuild";
  const layoutLabel = label === "derived" ? "derived layout" : "tx-derived layout";
  return Effect.tryPromise({
    try: () =>
      makeCommitTxForLayout(layout).complete({
        localUPLCEval: true,
      }),
    catch: (cause) =>
      new SDK.StateQueueError({
        message: `Failed to ${verb} block header commitment transaction with ${layoutLabel} (${formatLayout(
          layout,
        )}): ${cause}`,
        cause,
      }),
  }).pipe(
    Effect.catchAll((remoteError) =>
      Effect.gen(function* () {
        const localEvalDiagnostic = yield* Effect.either(
          Effect.tryPromise({
            try: () =>
              makeCommitTxForLayout(layout).complete({
                localUPLCEval: true,
              }),
            catch: (cause) =>
              new SDK.StateQueueError({
                message: `Local UPLC eval diagnostic failed for ${layoutLabel} (${formatLayout(
                  layout,
                )}): ${cause}`,
                cause,
              }),
          }),
        );
        const localDiagnosticMessage =
          localEvalDiagnostic._tag === "Left"
            ? formatUnknownError(localEvalDiagnostic.left)
            : "local UPLC eval unexpectedly succeeded";
        yield* Effect.logError(
          `Commit ${verb} failed for ${layoutLabel} ${formatLayout(layout)}; remote=${formatUnknownError(
            remoteError,
          )}; local_diagnostic=${localDiagnosticMessage}`,
        );
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              label === "derived"
                ? "Failed to build block header commitment transaction with deterministic layout"
                : "Failed to rebuild block header commitment transaction with deterministic layout",
            cause: `layout=${formatLayout(layout)}; remote=${formatUnknownError(
              remoteError,
            )}; local_diagnostic=${localDiagnosticMessage}`,
          }),
        );
      }),
    ),
  );
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
  SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const feeInputCandidates = availableOperatorWalletUtxos(
      witness.operatorWalletView,
    );
    const feeInput = yield* selectFeeInput(feeInputCandidates);
    const feeInputNonAdaUnits = Object.entries(feeInput.assets)
      .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
      .map(([unit, amount]) => `${unit}:${amount.toString()}`);
    yield* Effect.logInfo(
      `🔹 Selected fee input ${outRefLabel(feeInput)} from ${feeInputCandidates.length} operator-wallet candidate(s) (known=${witness.operatorWalletView.knownUtxos.length}, consumed_excluded=${witness.operatorWalletView.consumedOutRefs.length}) (non-ADA units: ${
        feeInputNonAdaUnits.length > 0 ? feeInputNonAdaUnits.join(",") : "none"
      }).`,
    );

    /**
     * Builds a commit transaction for a specific layout plan.
     */
    const makeCommitTxForLayout = (commitLayout: StateQueueCommitLayout) => {
      const stateQueueCommitRedeemer = encodeStateQueueCommitRedeemer(
        witness.operatorKeyHash,
        commitLayout,
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
      txReferenceInputs: [
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
      ],
      txInputs: [latestBlockInput, witness.activeOperatorInput, feeInput],
    });
    const { commitLayout, draftDiagnostics } = yield* Effect.tryPromise({
      try: async () => {
        const [, , draftSignBuilder] = await withStubbedProviderEvaluation(
          lucid,
          () =>
            makeCommitTxForLayout(seedLayout).chain({
              localUPLCEval: true,
            }),
        );
        const draftTx = draftSignBuilder.toTransaction();
        const draftTtl = draftTx.body().ttl();
        const txValidityUpperBoundSlot =
          draftTtl === undefined ? undefined : Number(draftTtl);
        const txValidityUpperBoundUnixTime =
          txValidityUpperBoundSlot === undefined
            ? undefined
            : slotToUnixTimeForLucid(lucid, txValidityUpperBoundSlot);
        const commitLayout = deriveCommitLayoutFromDraftTx({
          tx: draftTx,
          latestBlockInput,
          schedulerRefInput: witness.schedulerRefInput,
          hubOracleRefInput: witness.hubOracleRefInput,
          activeOperatorInput: witness.activeOperatorInput,
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          headerNodeUnit,
          headerNodeDatum: appendedNodeDatumCbor,
          previousHeaderNodeDatum: previousHeaderNodeDatumCbor,
        });
        const draftDiagnostics = buildRealCommitDraftDiagnostics({
          tx: draftTx,
          layout: commitLayout,
          operatorKeyHash: witness.operatorKeyHash,
          latestBlockInput,
          schedulerRefInput: witness.schedulerRefInput,
          hubOracleRefInput: witness.hubOracleRefInput,
          activeOperatorInput: witness.activeOperatorInput,
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          headerNodeUnit,
          appendedNodeDatumCbor,
          previousHeaderNodeDatumCbor,
          schedulerPolicyId: contracts.scheduler.policyId,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          activeOperatorsPolicyId: contracts.activeOperators.policyId,
          txValidityUpperBoundSlot: txValidityUpperBoundSlot?.toString(),
          txValidityUpperBoundUnixTime:
            txValidityUpperBoundUnixTime?.toString(),
        });
        return {
          commitLayout,
          draftDiagnostics,
        };
      },
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to derive deterministic commit redeemer layout from balanced draft tx: ${formatUnknownError(
            cause,
          )}`,
          cause,
        }),
    });
    yield* Effect.logInfo(
      `🔹 Using commit redeemer layout: ${formatLayout(commitLayout)}`,
    );
    yield* Effect.logInfo(
      `🔹 Real commit draft diagnostics: ${JSON.stringify(
        draftDiagnostics,
        (_key, value) => (typeof value === "bigint" ? value.toString() : value),
      )}`,
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
          new SDK.StateQueueError({
            message:
              "Commit transaction layout did not converge after deterministic rebuild",
            cause: `authored=${formatLayout(stableCommitLayout)}; derived=${formatLayout(
              derivedSubmitLayout,
            )}`,
          }),
        );
      }
      yield* Effect.logWarning(
        `Commit layout drift detected after balancing; rebuilding with tx-derived indexes. authored=${formatLayout(stableCommitLayout)} derived=${formatLayout(
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

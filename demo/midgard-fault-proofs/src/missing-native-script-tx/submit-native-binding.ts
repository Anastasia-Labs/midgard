import {
  HUB_ORACLE_ASSET_NAME,
  type NativeTxInclusionCarriage,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { prepareNativeTxInclusionCarriage } from "../native-inclusion-carriage.js";
import {
  type PublishedProofChunk,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { type FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  type MissingNativeScriptTxStepIndex,
  missingNativeScriptTxStepLabel,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScript,
} from "./submit-common.js";

export type MissingNativeScriptTxBindingResult = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
};

/** Shared body of the two bare-`NativeTxInclusionArgs` binding steps. */
export const submitMissingNativeScriptTxBinding = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadToken,
  stateQueueBlockOutRef,
  txInclusion,
  nextDatum,
  spendRedeemerSchema,
  wrapInclusionArgs,
  wrapInclusionCarriage,
  publishedProofChunks = [],
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: {
    readonly steps: readonly {
      readonly spendingScript: Script;
      readonly spendingScriptHash: string;
      readonly spendingScriptAddress: string;
    }[];
    readonly hubOraclePolicyId: string;
    readonly stateQueuePolicyId: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 2;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly nextDatum: string;
  readonly spendRedeemerSchema: Parameters<typeof Data.to>[1];
  /** Family-specific wrapper around the authenticated inclusion carriage. */
  readonly wrapInclusionArgs?: (
    args: Readonly<Record<string, unknown>>,
  ) => unknown;
  readonly wrapInclusionCarriage?: (
    carriage: NativeTxInclusionCarriage,
  ) => unknown;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  /** Runs after local evaluation/signing and before provider submission. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation: boolean;
}): Promise<MissingNativeScriptTxBindingResult> => {
  const label = missingNativeScriptTxStepLabel(
    stepIndex as MissingNativeScriptTxStepIndex,
  );
  const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${label} hub oracle`,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${label} state-queue block`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw missingNativeScriptTxSubmitError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);

  signer.selectWallet(lucid);
  if (publishedProofChunks.length > 0 && wrapInclusionCarriage === undefined)
    throw missingNativeScriptTxSubmitError(
      "published proof chunks require a carriage-aware family",
    );
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks: publishedProofChunks,
    }),
  );
  const stepReference = requireMissingNativeScriptTxReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const carriage = prepareNativeTxInclusionCarriage({
    blueprint,
    network,
    txInclusion,
    publishedProofChunks,
    witnessReferenceScripts,
    label,
    baseReferenceInputs: [hubOracleUtxo, stateQueueBlockUtxo, stepReference],
  });
  const nextStep = contracts.steps[stepIndex + 1];
  if (nextStep === undefined) {
    throw missingNativeScriptTxSubmitError(`${label} has no successor step.`);
  }
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        hubOracleRefInputIndex: bigint;
        stateQueueNodeRefInputIndex: bigint;
      }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${label} output`,
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        `${label} hub oracle`,
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        `${label} state-queue node`,
      ),
    };
    layout = resolved;
    const inclusion = carriage.redeemer(ctx, {
      input_index: resolved.inputIndex,
      output_index: resolved.outputIndex,
      hub_ref_input_index: resolved.hubOracleRefInputIndex,
      state_queue_node_ref_input_index: resolved.stateQueueNodeRefInputIndex,
    });
    const args =
      wrapInclusionCarriage !== undefined
        ? wrapInclusionCarriage(inclusion)
        : (() => {
            if (!("RedeemerCarriedInclusion" in inclusion))
              throw missingNativeScriptTxSubmitError(
                "bare inclusion cannot carry chunks",
              );
            const bare = inclusion.RedeemerCarriedInclusion[0];
            return wrapInclusionArgs === undefined
              ? bare
              : wrapInclusionArgs(bare);
          })();
    return Data.to(
      {
        Continue: [args],
      },
      spendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(carriage.referenceInputs)
    .pay.ToContract(
      nextStep.spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = carriage.attachWithdrawal(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw missingNativeScriptTxSubmitError(
      `BuildTxWithRedeemer did not resolve ${label} layout.`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: `${label}-spend`,
          utxo: stepReference,
          expectedScript: contracts.steps[stepIndex].spendingScript,
        },
        ...carriage.referenceScriptCandidates,
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `provider returned transaction hash ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
  };
};

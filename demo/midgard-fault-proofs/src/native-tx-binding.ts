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
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { prepareNativeTxInclusionCarriage } from "./native-inclusion-carriage.js";
import { type PublishedProofChunk } from "./proof-chunk-carriage.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "./runtime.js";
import { completeStep, fundStep, submitStep } from "./step-completion.js";
import {
  requireNativeTxMatchesCompactCbor,
  type SubmitStep01TxInclusion,
} from "./step-support.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { type FaultProofWitnessReferenceScripts } from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

/** Structural contract view the shared binding step reads. */
export type NativeTxBindingContracts = {
  readonly steps: readonly {
    readonly spendingScript: Script;
    readonly spendingScriptHash: string;
    readonly spendingScriptAddress: string;
  }[];
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
};

const nativeTxBindingError = (message: string): Error =>
  new Error(`native-tx binding: ${message}`);

const nativeTxBindingStepLabel = (stepIndex: number): string =>
  `native-tx binding step 0${(stepIndex + 1).toString()}`;

const requireStepReferenceScript = ({
  utxo,
  expectedScriptHash,
  label,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly label: string;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw nativeTxBindingError(
      `reference UTxO ${outRefLabel(utxo)} for ${label} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw nativeTxBindingError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export type NativeTxBindingResult = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
};

/** Shared body of the two bare-`NativeTxInclusionArgs` binding steps. */
export const submitNativeTxBinding = async ({
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
  readonly contracts: NativeTxBindingContracts;
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
}): Promise<NativeTxBindingResult> => {
  const label = nativeTxBindingStepLabel(stepIndex);
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
    throw nativeTxBindingError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);

  if (publishedProofChunks.length > 0 && wrapInclusionCarriage === undefined)
    throw nativeTxBindingError(
      "published proof chunks require a carriage-aware family",
    );
  const funding = await fundStep(lucid, {
    signer,
    excluding: publishedProofChunks.map(({ utxo }) => utxo),
  });
  const stepReference = requireStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    label,
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
    throw nativeTxBindingError(`${label} has no successor step.`);
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
              throw nativeTxBindingError("bare inclusion cannot carry chunks");
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
  const base = funding.tx
    .collectFrom([threadUtxo], redeemer)
    .readFrom(carriage.referenceInputs)
    .pay.ToContract(
      nextStep.spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    );
  const tx = carriage.attachWithdrawal(base);
  const signed = await completeStep(tx, funding);
  if (layout === undefined) {
    throw nativeTxBindingError(
      `BuildTxWithRedeemer did not resolve ${label} layout.`,
    );
  }
  const txHash = await submitStep(lucid, signed, {
    label: label,
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
    awaitConfirmation,
  });
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
  };
};

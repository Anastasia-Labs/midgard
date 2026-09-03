import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { DistinctAssetAccumulationContracts } from "./contracts-v1.js";
import {
  classifyDistinctAssetAccumulationFinding,
  type DistinctAssetAccumulationFinding,
} from "./family-v1.js";
import {
  DistinctAssetStep01RedeemerSchema,
  DistinctAssetStep02DatumSchema,
} from "./schemas-v1.js";

const coordinateData = (
  finding: DistinctAssetAccumulationFinding,
): Readonly<{ fold: bigint; primary_index: bigint; asset_index: bigint }> => {
  const coordinate = finding.coordinate;
  return coordinate.kind === "input"
    ? {
        fold: 0n,
        primary_index: BigInt(coordinate.inputIndex),
        asset_index: BigInt(coordinate.assetIndex),
      }
    : coordinate.kind === "output"
      ? {
          fold: 1n,
          primary_index: BigInt(coordinate.outputIndex),
          asset_index: BigInt(coordinate.assetIndex),
        }
      : {
          fold: 2n,
          primary_index: BigInt(coordinate.mintIndex),
          asset_index: 0n,
        };
};

export const submitDistinctAssetAccumulationStep01Accepted = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  finding,
  threadUtxo,
  threadToken,
  stateQueueBlockOutRef,
  txInclusion,
  validationTracesRoot,
  validationTraceCount,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: DistinctAssetAccumulationContracts;
  readonly signer: ResolvedProverSigner;
  readonly finding: DistinctAssetAccumulationFinding;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly validationTracesRoot: string;
  readonly validationTraceCount: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  classifyDistinctAssetAccumulationFinding(finding);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: finding.subject,
        validation_traces_root: validationTracesRoot,
        validation_trace_count: validationTraceCount,
        coordinate: coordinateData(finding),
      },
    } as never,
    DistinctAssetStep02DatumSchema as never,
  );
  return await submitMissingNativeScriptTxBinding({
    lucid,
    blueprint,
    network,
    contracts,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: DistinctAssetStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      coordinate: coordinateData(finding),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const submitDistinctAssetAccumulationStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding,
  forcedSource,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DistinctAssetAccumulationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: DistinctAssetAccumulationFinding;
  readonly forcedSource: Readonly<{
    header: Readonly<Record<string, unknown>>;
    [field: string]: unknown;
  }>;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  classifyDistinctAssetAccumulationFinding(finding);
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const header = forcedSource.header as {
    validation_traces_root: string;
    validation_trace_count: bigint;
  };
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: finding.subject,
        validation_traces_root: header.validation_traces_root,
        validation_trace_count: header.validation_trace_count,
        coordinate: coordinateData(finding),
      },
    } as never,
    DistinctAssetStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "distinctAssetAccumulationLimit forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "distinctAssetAccumulationLimit",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "distinctAssetAccumulationLimit forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: inputIndex,
                output_index: outputIndex,
              },
            },
            coordinate: coordinateData(finding),
          },
        ],
      } as never,
      DistinctAssetStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "distinctAssetAccumulationLimit step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("distinctAssetAccumulationLimit: forced layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

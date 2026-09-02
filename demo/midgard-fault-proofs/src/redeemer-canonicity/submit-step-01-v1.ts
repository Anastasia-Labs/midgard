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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import { submitMissingNativeScriptTxBindingV1 } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { RedeemerCanonicityContractsV1 } from "./contracts-v1.js";
import {
  classifyRedeemerCanonicityFindingV1,
  type RedeemerCanonicityFindingV1,
} from "./family-v1.js";
import {
  RedeemerCanonicityStep01RedeemerV1Schema,
  RedeemerCanonicityStep02DatumV1Schema,
} from "./schemas-v1.js";

const nextDatum = (
  finding: RedeemerCanonicityFindingV1,
  signer: ResolvedProverSigner,
  witnessSetHash: string,
) => {
  try {
    return Data.to(
      {
        fraud_prover: signer.paymentKeyHash,
        data: {
          subject: finding.subject,
          witness_set_hash: witnessSetHash,
          redeemer_index: BigInt(finding.redeemerIndex),
        },
      } as never,
      RedeemerCanonicityStep02DatumV1Schema as never,
    );
  } catch {
    throw new Error("redeemer-canonicity: failed to encode step-02 datum");
  }
};

export const submitRedeemerCanonicityStep01AcceptedV1 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  finding: rawFinding,
  threadUtxo,
  threadToken,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: RedeemerCanonicityContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly finding: RedeemerCanonicityFindingV1;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const finding = classifyRedeemerCanonicityFindingV1(rawFinding);
  return await submitMissingNativeScriptTxBindingV1({
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
    nextDatum: nextDatum(
      finding,
      signer,
      txInclusion.nativeTx.witness_set_hash,
    ),
    spendRedeemerSchema: RedeemerCanonicityStep01RedeemerV1Schema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      redeemer_index: BigInt(finding.redeemerIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const submitRedeemerCanonicityStep01ForcedV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding: rawFinding,
  forcedSource,
  witnessSetHash,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: RedeemerCanonicityFindingV1;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly witnessSetHash: string;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const finding = classifyRedeemerCanonicityFindingV1(rawFinding);
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "redeemer-canonicity",
    stepIndex,
  });
  const datum = nextDatum(finding, signer, witnessSetHash);
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "redeemer-canonicity forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "redeemer-canonicity",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "redeemer-canonicity step-01 output",
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
            redeemer_index: BigInt(finding.redeemerIndex),
          },
        ],
      } as never,
      RedeemerCanonicityStep01RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "redeemer-canonicity step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("redeemer-canonicity: forced layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

import {
  forcedLeafMembershipV1,
  type ForcedTransactionSourceMembershipProof,
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
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import { buildNativeTxBinding } from "../native-tx-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { RedeemerCanonicityContracts } from "./contracts.js";
import {
  classifyRedeemerCanonicityFinding,
  type RedeemerCanonicityFinding,
} from "./family.js";
import {
  RedeemerCanonicityStep01RedeemerSchema,
  RedeemerCanonicityStep02DatumSchema,
} from "./schemas.js";

const nextDatum = (
  finding: RedeemerCanonicityFinding,
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
          redeemer_index: finding.redeemerIndex,
        },
      } as never,
      RedeemerCanonicityStep02DatumSchema as never,
    );
  } catch {
    throw new Error("redeemer-canonicity: failed to encode step-02 datum");
  }
};

export const buildRedeemerCanonicityStep01Accepted = async ({
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
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: RedeemerCanonicityContracts;
  readonly signer: ResolvedProverSigner;
  readonly finding: RedeemerCanonicityFinding;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const finding = classifyRedeemerCanonicityFinding(rawFinding);
  return await buildNativeTxBinding({
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
    spendRedeemerSchema: RedeemerCanonicityStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      redeemer_index: finding.redeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};

export const buildRedeemerCanonicityStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding: rawFinding,
  forcedSource,
  witnessSetHash,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: RedeemerCanonicityFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly witnessSetHash: string;
  readonly referenceScriptUtxo: UTxO;
}) => {
  const finding = classifyRedeemerCanonicityFinding(rawFinding);
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
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
                membership: forcedLeafMembershipV1(
                  forcedSource.membership as ForcedTransactionSourceMembershipProof,
                ),
                input_index: inputIndex,
                output_index: outputIndex,
              },
            },
            redeemer_index: finding.redeemerIndex,
          },
        ],
      } as never,
      RedeemerCanonicityStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "redeemer-canonicity step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
  });
  if (outputIndex === undefined)
    throw new Error("redeemer-canonicity: forced layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};

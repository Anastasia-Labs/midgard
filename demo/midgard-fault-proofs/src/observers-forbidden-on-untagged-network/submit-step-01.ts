import {
  forcedLeafMembershipV1,
  type ForcedTransactionSourceMembershipProof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
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
import type { SubmitStep01TxInclusion } from "../step-support.js";
import { requireInitialStepDatum } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ObserversForbiddenContracts } from "./contracts.js";
import {
  classifyObserversForbiddenFinding,
  type ObserversForbiddenFinding,
} from "./family.js";
import {
  ObserversForbiddenStep01RedeemerSchema,
  ObserversForbiddenStep02DatumSchema,
} from "./schemas.js";

const nextDatum = (
  finding: ObserversForbiddenFinding,
  signer: ResolvedProverSigner,
): string => {
  const exact = classifyObserversForbiddenFinding(finding);
  return Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: { subject: exact.subject, network_id: BigInt(exact.networkId) },
    } as never,
    ObserversForbiddenStep02DatumSchema as never,
  );
};

export const buildObserversForbiddenStep01Accepted = async ({
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
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: ObserversForbiddenContracts;
  readonly signer: ResolvedProverSigner;
  readonly finding: ObserversForbiddenFinding;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) =>
  await buildNativeTxBinding({
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
    nextDatum: nextDatum(finding, signer),
    spendRedeemerSchema: ObserversForbiddenStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
  });

export const buildObserversForbiddenStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding,
  forcedSource,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: ObserversForbiddenFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
}) => {
  const exact = classifyObserversForbiddenFinding(finding);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "observers-forbidden-on-untagged-network",
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "observers-forbidden-on-untagged-network",
    stepIndex: 0,
  });
  const datum = nextDatum(exact, signer);
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
      "observers-forbidden forced step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "observers-forbidden forced output",
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
                input_index: requireInputIndex(
                  ctx,
                  threadUtxo,
                  "observers-forbidden",
                ),
                output_index: outputIndex,
              },
            },
          },
        ],
      } as never,
      ObserversForbiddenStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "observers-forbidden step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
  });
  if (outputIndex === undefined)
    throw new Error("observersForbidden: layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};

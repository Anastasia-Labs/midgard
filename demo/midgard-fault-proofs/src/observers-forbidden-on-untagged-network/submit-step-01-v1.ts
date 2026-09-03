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
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ObserversForbiddenContracts } from "./contracts-v1.js";
import {
  classifyObserversForbiddenFinding,
  type ObserversForbiddenFinding,
} from "./family-v1.js";
import {
  ObserversForbiddenStep01RedeemerSchema,
  ObserversForbiddenStep02DatumSchema,
} from "./schemas-v1.js";

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

export const submitObserversForbiddenStep01Accepted = async ({
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
  preSubmitBoundary,
  awaitConfirmation = true,
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
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitMissingNativeScriptTxBinding({
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
    preSubmitBoundary,
    awaitConfirmation,
  });

export const submitObserversForbiddenStep01Forced = async ({
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
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: ObserversForbiddenFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
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
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "observers-forbidden step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("observersForbidden: layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

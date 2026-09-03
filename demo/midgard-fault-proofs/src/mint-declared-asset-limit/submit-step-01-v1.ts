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
import type { MintDeclaredAssetLimitContracts } from "./contracts-v1.js";
import {
  classifyMintDeclaredAssetLimitFinding,
  type MintDeclaredAssetLimitFinding,
} from "./family-v1.js";
import {
  MintDeclaredAssetLimitStep01RedeemerSchema,
  MintDeclaredAssetLimitStep02DatumSchema,
} from "./schemas-v1.js";

const nextDatum = (
  finding: MintDeclaredAssetLimitFinding,
  signer: ResolvedProverSigner,
): string => {
  const exact = classifyMintDeclaredAssetLimitFinding(finding);
  return Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        Bound: {
          bound: {
            subject: exact.subject,
            policy_index: BigInt(exact.policyIndex),
          },
        },
      },
    } as never,
    MintDeclaredAssetLimitStep02DatumSchema as never,
  );
};

export const submitMintDeclaredAssetLimitStep01Accepted = async ({
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
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly signer: ResolvedProverSigner;
  readonly finding: MintDeclaredAssetLimitFinding;
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
    spendRedeemerSchema: MintDeclaredAssetLimitStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      policy_index: BigInt(finding.policyIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });

export const submitMintDeclaredAssetLimitStep01Forced = async ({
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
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: MintDeclaredAssetLimitFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const exact = classifyMintDeclaredAssetLimitFinding(finding);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex: 0,
  });
  const datum = nextDatum(exact, signer);
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let resolvedOutputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "mint-declared-asset-limit forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "mint-declared-asset-limit",
    );
    resolvedOutputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "mint-declared-asset-limit forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: inputIndex,
                output_index: resolvedOutputIndex,
              },
            },
            policy_index: BigInt(exact.policyIndex),
          },
        ],
      } as never,
      MintDeclaredAssetLimitStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "mint-declared-asset-limit step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (resolvedOutputIndex === undefined)
    throw new Error("mintDeclaredAssetLimit: forced layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolvedOutputIndex.toString()}`,
  };
};

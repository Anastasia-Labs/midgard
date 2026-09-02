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
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MintDeclaredAssetLimitContractsV1 } from "./contracts-v1.js";
import {
  classifyMintDeclaredAssetLimitFindingV1,
  type MintDeclaredAssetLimitFindingV1,
} from "./family-v1.js";
import {
  MintDeclaredAssetLimitStep01RedeemerV1Schema,
  MintDeclaredAssetLimitStep02DatumV1Schema,
} from "./schemas-v1.js";

const nextDatum = (
  finding: MintDeclaredAssetLimitFindingV1,
  signer: ResolvedProverSigner,
): string => {
  const exact = classifyMintDeclaredAssetLimitFindingV1(finding);
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
    MintDeclaredAssetLimitStep02DatumV1Schema as never,
  );
};

export const submitMintDeclaredAssetLimitStep01AcceptedV1 = async ({
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
  readonly contracts: MintDeclaredAssetLimitContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly finding: MintDeclaredAssetLimitFindingV1;
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
}) =>
  await submitMissingNativeScriptTxBindingV1({
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
    spendRedeemerSchema: MintDeclaredAssetLimitStep01RedeemerV1Schema,
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

export const submitMintDeclaredAssetLimitStep01ForcedV1 = async ({
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
  readonly contracts: MintDeclaredAssetLimitContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: MintDeclaredAssetLimitFindingV1;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const exact = classifyMintDeclaredAssetLimitFindingV1(finding);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
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
      MintDeclaredAssetLimitStep01RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
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

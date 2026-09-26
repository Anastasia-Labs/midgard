import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type VerdictSubject,
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
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import { requireInitialStepDatum } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import {
  classifyMintDeclaredAssetLimitFinding,
  type MintDeclaredAssetLimitFinding,
} from "./family.js";
import {
  MintDeclaredAssetLimitStep01RedeemerSchema,
  MintDeclaredAssetLimitStep02DatumSchema,
} from "./schemas.js";

const boundDatum = (
  subject: VerdictSubject,
  policyIndex: number,
  signer: ResolvedProverSigner,
): string =>
  Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        Bound: {
          bound: { subject, policy_index: BigInt(policyIndex) },
        },
      },
    } as never,
    MintDeclaredAssetLimitStep02DatumSchema as never,
  );

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
}) => {
  const exact = classifyMintDeclaredAssetLimitFinding(finding);
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
    nextDatum: boundDatum(exact.subject, exact.policyIndex, signer),
    spendRedeemerSchema: MintDeclaredAssetLimitStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      policy_index: BigInt(exact.policyIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

/**
 * Binds a forced leaf from explicit wire inputs: the subject the datum will
 * carry, the coordinate the datum claims and the coordinate the redeemer
 * asserts. The classified builder below keeps them consistent; this form
 * lets a lifecycle present a mutated coordinate, subject or leaf on chain.
 */
export const submitMintDeclaredAssetLimitStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  subject,
  datumPolicyIndex,
  redeemerPolicyIndex,
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
  readonly subject: VerdictSubject;
  readonly datumPolicyIndex: number;
  readonly redeemerPolicyIndex: number;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
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
  const datum = boundDatum(subject, datumPolicyIndex, signer);
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
            policy_index: BigInt(redeemerPolicyIndex),
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

export const submitMintDeclaredAssetLimitStep01Forced = async ({
  finding,
  ...rest
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
  return await submitMintDeclaredAssetLimitStep01ForcedRaw({
    ...rest,
    subject: exact.subject,
    datumPolicyIndex: exact.policyIndex,
    redeemerPolicyIndex: exact.policyIndex,
  });
};

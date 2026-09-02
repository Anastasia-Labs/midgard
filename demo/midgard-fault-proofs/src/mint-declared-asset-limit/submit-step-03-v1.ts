import { decodeMidgardFieldPreimageV1 } from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MintDeclaredAssetLimitContractsV1 } from "./contracts-v1.js";
import type { MintDeclaredAssetLimitEvidenceV1 } from "./family-v1.js";
import {
  MintDeclaredAssetLimitStep03DatumV1Schema,
  MintDeclaredAssetLimitStep03RedeemerV1Schema,
  MintDeclaredAssetLimitStep04DatumV1Schema,
} from "./schemas-v1.js";
import {
  encodeMintDeclaredWalkCheckpointV1,
  hashMintDeclaredWalkCheckpointV1,
  type MintDeclaredAssetLimitStagedPlanV1,
  mintDeclaredFoldPrefixV1,
} from "./staged-plan-v1.js";

export const submitMintDeclaredAssetLimitStep03V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  walkOrdinal,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly staged: MintDeclaredAssetLimitStagedPlanV1;
  readonly walkOrdinal: number;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const nextCheckpoint = staged.walk[walkOrdinal];
  if (nextCheckpoint === undefined)
    throw new Error("mintDeclaredAssetLimit: walk ordinal is outside plan");
  const priorCheckpoint =
    walkOrdinal === 0 ? staged.initialWalk : staged.walk[walkOrdinal - 1]!;
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    policy_index: bigint;
    target_policy_id: string;
    target_declared_count: bigint;
    checkpoint_hash: string;
    accumulated_count: bigint;
    previous_policy: string;
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: MintDeclaredAssetLimitStep03DatumV1Schema as never,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  if (
    state.policy_index !== BigInt(evidence.policyIndex) ||
    state.target_policy_id !== evidence.targetPolicyId ||
    state.target_declared_count !== BigInt(evidence.targetDeclaredCount) ||
    state.checkpoint_hash !==
      hashMintDeclaredWalkCheckpointV1(priorCheckpoint) ||
    state.outcome !== 0n
  )
    throw new Error("mintDeclaredAssetLimit: fold datum/checkpoint changed");
  const prefix = mintDeclaredFoldPrefixV1({
    items: staged.items,
    nextItemIndex: priorCheckpoint.nextItemIndex,
    policyIndex: evidence.policyIndex,
  });
  if (
    state.accumulated_count !== BigInt(prefix.accumulatedCount) ||
    state.previous_policy !== prefix.previousPolicy
  )
    throw new Error("mintDeclaredAssetLimit: fold accumulator changed");
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: 5,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: true,
    label: "mintDeclaredAssetLimit fold field 5",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("mintDeclaredAssetLimit: field carriage disappeared");
  const certificateUtxo = await resolveFaultProofFieldPreimageCertificateV1({
    lucid,
    network: lucid.config().network!,
    planned,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("mintDeclaredAssetLimit: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "mintDeclaredAssetLimit fold field 5",
  });
  const terminal = walkOrdinal === staged.walk.length - 1;
  const nextPrefix = terminal
    ? null
    : mintDeclaredFoldPrefixV1({
        items: staged.items,
        nextItemIndex: nextCheckpoint.nextItemIndex,
        policyIndex: evidence.policyIndex,
      });
  const nextData = terminal
    ? {
        subject: evidence.subject,
        policy_index: BigInt(evidence.policyIndex),
        crossing: evidence.crossing,
      }
    : {
        subject: evidence.subject,
        policy_index: BigInt(evidence.policyIndex),
        target_policy_id: evidence.targetPolicyId,
        target_declared_count: BigInt(evidence.targetDeclaredCount),
        checkpoint_hash: hashMintDeclaredWalkCheckpointV1(nextCheckpoint),
        accumulated_count: BigInt(nextPrefix!.accumulatedCount),
        previous_policy: nextPrefix!.previousPolicy,
        outcome: 0n,
      };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (terminal
      ? MintDeclaredAssetLimitStep04DatumV1Schema
      : MintDeclaredAssetLimitStep03DatumV1Schema) as never,
  );
  const nextStep = terminal ? contracts.steps[3] : contracts.steps[2];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "mintDeclaredAssetLimit step-03");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "mintDeclaredAssetLimit",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "mintDeclaredAssetLimit step-03 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening,
            checkpoint_bytes:
              encodeMintDeclaredWalkCheckpointV1(priorCheckpoint).toString(
                "hex",
              ),
            item_budget: BigInt(
              nextCheckpoint.nextItemIndex - priorCheckpoint.nextItemIndex,
            ),
          },
        ],
      } as never,
      MintDeclaredAssetLimitStep03RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: `mintDeclaredAssetLimit step-03 walk ${walkOrdinal.toString()}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("mintDeclaredAssetLimit: step-03 layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

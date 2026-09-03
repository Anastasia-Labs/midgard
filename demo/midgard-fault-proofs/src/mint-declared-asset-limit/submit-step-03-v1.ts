import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  type FieldOpening,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts-v1.js";
import type { MintDeclaredAssetLimitEvidence } from "./family-v1.js";
import {
  MintDeclaredAssetLimitStep03DatumSchema,
  MintDeclaredAssetLimitStep03RedeemerSchema,
  MintDeclaredAssetLimitStep04DatumSchema,
} from "./schemas-v1.js";
import {
  encodeMintDeclaredWalkCheckpoint,
  hashMintDeclaredWalkCheckpoint,
  type MintDeclaredAssetLimitStagedPlan,
  mintDeclaredFoldPrefix,
} from "./staged-plan-v1.js";

export const submitMintDeclaredAssetLimitStep03 = async ({
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
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: MintDeclaredAssetLimitStagedPlan;
  readonly walkOrdinal: number;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const nextCheckpoint = staged.walk[walkOrdinal];
  if (nextCheckpoint === undefined)
    throw new Error("mintDeclaredAssetLimit: walk ordinal is outside plan");
  const priorCheckpoint =
    walkOrdinal === 0 ? staged.initialWalk : staged.walk[walkOrdinal - 1]!;
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
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
    schema: MintDeclaredAssetLimitStep03DatumSchema as never,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  if (
    state.policy_index !== BigInt(evidence.policyIndex) ||
    state.target_policy_id !== evidence.targetPolicyId ||
    state.target_declared_count !== BigInt(evidence.targetDeclaredCount) ||
    state.checkpoint_hash !== hashMintDeclaredWalkCheckpoint(priorCheckpoint) ||
    state.outcome !== 0n
  )
    throw new Error("mintDeclaredAssetLimit: fold datum/checkpoint changed");
  const prefix = mintDeclaredFoldPrefix({
    items: staged.items,
    nextItemIndex: priorCheckpoint.nextItemIndex,
    policyIndex: evidence.policyIndex,
  });
  if (
    state.accumulated_count !== BigInt(prefix.accumulatedCount) ||
    state.previous_policy !== prefix.previousPolicy
  )
    throw new Error("mintDeclaredAssetLimit: fold accumulator changed");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: 5,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: true,
    label: "mintDeclaredAssetLimit fold field 5",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("mintDeclaredAssetLimit: field carriage disappeared");
  const certificateUtxo = await resolveFaultProofFieldPreimageCertificate({
    lucid,
    network: lucid.config().network!,
    planned,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("mintDeclaredAssetLimit: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
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
    : mintDeclaredFoldPrefix({
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
        checkpoint_hash: hashMintDeclaredWalkCheckpoint(nextCheckpoint),
        accumulated_count: BigInt(nextPrefix!.accumulatedCount),
        previous_policy: nextPrefix!.previousPolicy,
        outcome: 0n,
      };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (terminal
      ? MintDeclaredAssetLimitStep04DatumSchema
      : MintDeclaredAssetLimitStep03DatumSchema) as never,
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
              encodeMintDeclaredWalkCheckpoint(priorCheckpoint).toString("hex"),
            item_budget: BigInt(
              nextCheckpoint.nextItemIndex - priorCheckpoint.nextItemIndex,
            ),
          },
        ],
      } as never,
      MintDeclaredAssetLimitStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
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

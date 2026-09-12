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
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import {
  MINT_DECLARED_OUTCOME_CROSSING,
  MINT_DECLARED_OUTCOME_SCANNING,
  type MintDeclaredAssetLimitEvidence,
  type MintDeclaredAssetLimitFoldStateData,
  mintDeclaredFoldDataMatches,
  mintDeclaredFoldStateData,
} from "./family.js";
import {
  MintDeclaredAssetLimitStep03DatumSchema,
  MintDeclaredAssetLimitStep03RedeemerSchema,
  MintDeclaredAssetLimitStep04DatumSchema,
} from "./schemas.js";
import {
  encodeMintDeclaredWalkCheckpoint,
  hashMintDeclaredWalkCheckpoint,
  initialMintDeclaredFoldSnapshot,
  type MintDeclaredAssetLimitStagedPlan,
  type MintDeclaredFoldSnapshot,
} from "./staged-plan.js";

/**
 * Builds one step-03 transaction from explicit wire inputs. The plan-driven
 * builder below derives every input from the staged plan; the explicit form
 * exists so a lifecycle can present the validator with a substituted
 * checkpoint, budget, successor or claimed state and observe its refusal.
 */
export const submitMintDeclaredAssetLimitStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  priorData,
  checkpointBytesHex,
  budget,
  next,
  nextStepIndex = next.kind === "decision" ? 3 : 2,
  referenceScriptUtxo,
  stepRole,
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
  /** The fold datum the thread is expected to carry now. */
  readonly priorData: MintDeclaredAssetLimitFoldStateData;
  readonly checkpointBytesHex: string;
  readonly budget: bigint;
  readonly next:
    | Readonly<{ kind: "fold"; data: MintDeclaredAssetLimitFoldStateData }>
    | Readonly<{ kind: "decision"; crossing: boolean }>;
  /** Successor step index; the honest value is 2 (fold) or 3 (decision). */
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
  readonly referenceScriptUtxo: UTxO;
  readonly stepRole: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex,
    threadOutRef,
  });
  const state =
    requireLinearFaultStepState<MintDeclaredAssetLimitFoldStateData>({
      threadUtxo,
      signer,
      schema: MintDeclaredAssetLimitStep03DatumSchema as never,
      family: "mint-declared-asset-limit",
      stepIndex,
    });
  if (!mintDeclaredFoldDataMatches(state, priorData))
    throw new Error("mintDeclaredAssetLimit: fold datum/checkpoint changed");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
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
  const nextData =
    next.kind === "decision"
      ? {
          subject: evidence.subject,
          policy_index: BigInt(evidence.policyIndex),
          crossing: next.crossing,
        }
      : next.data;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (next.kind === "decision"
      ? MintDeclaredAssetLimitStep04DatumSchema
      : MintDeclaredAssetLimitStep03DatumSchema) as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
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
            checkpoint_bytes: checkpointBytesHex,
            budget,
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
    stepRole,
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

/** The fold datum committed by snapshot `ordinal` of the plan (-1: initial). */
export const mintDeclaredFoldSnapshotData = ({
  evidence,
  staged,
  snapshot,
}: {
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly staged: MintDeclaredAssetLimitStagedPlan;
  readonly snapshot: MintDeclaredFoldSnapshot;
}): MintDeclaredAssetLimitFoldStateData =>
  mintDeclaredFoldStateData({
    subject: evidence.subject,
    target: staged.target,
    cursor: snapshot.cursor,
    checkpointHash: hashMintDeclaredWalkCheckpoint(snapshot.checkpoint),
  });

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
  const nextSnapshot = staged.walk[walkOrdinal];
  if (nextSnapshot === undefined)
    throw new Error("mintDeclaredAssetLimit: walk ordinal is outside plan");
  const priorSnapshot =
    walkOrdinal === 0
      ? initialMintDeclaredFoldSnapshot(staged)
      : staged.walk[walkOrdinal - 1]!;
  if (
    staged.target.policyIndex !== evidence.policyIndex ||
    staged.target.targetPolicyId !== evidence.targetPolicyId ||
    staged.target.targetDeclaredCount !== evidence.targetDeclaredCount
  )
    throw new Error("mintDeclaredAssetLimit: staged plan target changed");
  const terminal =
    nextSnapshot.cursor.outcome !== MINT_DECLARED_OUTCOME_SCANNING;
  return await submitMintDeclaredAssetLimitStep03Raw({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    evidence,
    nativeTxCompactCbor,
    priorData: mintDeclaredFoldSnapshotData({
      evidence,
      staged,
      snapshot: priorSnapshot,
    }),
    checkpointBytesHex: encodeMintDeclaredWalkCheckpoint(
      priorSnapshot.checkpoint,
    ).toString("hex"),
    budget: BigInt(staged.foldBudget),
    next: terminal
      ? {
          kind: "decision",
          crossing:
            nextSnapshot.cursor.outcome === MINT_DECLARED_OUTCOME_CROSSING,
        }
      : {
          kind: "fold",
          data: mintDeclaredFoldSnapshotData({
            evidence,
            staged,
            snapshot: nextSnapshot,
          }),
        },
    referenceScriptUtxo,
    stepRole: `mintDeclaredAssetLimit step-03 walk ${walkOrdinal.toString()}`,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

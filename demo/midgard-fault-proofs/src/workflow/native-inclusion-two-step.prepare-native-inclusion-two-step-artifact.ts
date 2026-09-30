import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import {
  prepareInvalidRangeFromCanonicalEvidence,
  prepareZeroInputFromCanonicalEvidence,
} from "../evidence/prepare-from-evidence.js";
import type { InvalidRangeContracts } from "../invalid-range/contracts.js";
import { prepareInvalidRangeForcedPlan } from "../invalid-range/v1.js";
import type { PreparedTxInclusionJson } from "../prepare-double-spend.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { ZeroInputContracts } from "../zero-input/contracts.js";
import {
  prepareZeroInputEvidence,
  ZeroInputVerdictSubjectSchema,
} from "../zero-input/family.js";
import { ZeroInputForcedSourcePayloadSchema } from "../zero-input/schemas.js";
import { prepareZeroInputForcedPlan } from "../zero-input/v1.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.js";
import { normalizeJournalJson } from "./journal.js";
import { admitNativeInclusionTwoStepArtifact } from "./native-inclusion-two-step.admit-native-inclusion-two-step-artifact.js";
import {
  HEX_32,
  NATIVE_INCLUSION_TWO_STEP_ARTIFACT,
  type NativeInclusionTwoStepArtifact,
  type NativeInclusionTwoStepCategory,
  NATURAL,
} from "./native-inclusion-two-step.parse-artifact.js";

const selectedTxId = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
): string => {
  if (
    classification.category === "invalidRange" &&
    classification.selected.detectionId.startsWith("invalid-range:forced:")
  ) {
    const fields = classification.selected.detectionId.split(":");
    if (
      fields.length !== 5 ||
      !NATURAL.test(fields[2] ?? "") ||
      !HEX_32.test(fields[3] ?? "") ||
      classification.selected.position !== BigInt(fields[2]!)
    )
      throw new Error("invalidRange forced classification is malformed");
    return fields[3]!;
  }
  if (
    classification.category === "zeroInput" &&
    classification.selected.detectionId.startsWith("zero-input:forced:")
  ) {
    const fields = classification.selected.detectionId.split(":");
    if (
      fields.length !== 4 ||
      !NATURAL.test(fields[2] ?? "") ||
      !HEX_32.test(fields[3] ?? "") ||
      classification.selected.position !== BigInt(fields[2]!)
    )
      throw new Error("zeroInput forced classification is malformed");
    return fields[3]!;
  }
  const prefix =
    classification.category === "invalidRange" ? "invalid-range" : "zero-input";
  const fields = classification.selected.detectionId.split(":");
  const expectedLength = classification.category === "invalidRange" ? 4 : 3;
  if (
    fields.length !== expectedLength ||
    fields[0] !== prefix ||
    !NATURAL.test(fields[1] ?? "") ||
    !HEX_32.test(fields[2] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error(`${classification.category} classification is malformed`);
  }
  return fields[2]!;
};

export const prepareNativeInclusionTwoStepArtifact = async <
  Category extends NativeInclusionTwoStepCategory,
>({
  category,
  evidence,
  classification,
}: {
  readonly category: Category;
  readonly evidence: Parameters<
    typeof prepareInvalidRangeFromCanonicalEvidence
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<NativeInclusionTwoStepArtifact> => {
  if (
    classification.category !== category ||
    classification.headerHash !== evidence.headerHash
  ) {
    throw new Error(
      `${category} classification differs from canonical evidence`,
    );
  }
  const txId = selectedTxId(classification);
  let preparedHeaderHash: string;
  let preparedNodeTxId: string;
  let preparedInclusion: Omit<PreparedTxInclusionJson, "nativeTx">;
  let violationReason: string | null;
  let blockSlot: string | null;
  let sourceKind: "accepted" | "forced" = "accepted";
  let subjectCbor = "";
  let inputFieldPreimageCbor = "";
  let inputFieldCommitment = "00".repeat(32);
  let forcedSourceCbor = "";
  if (
    category === "invalidRange" &&
    !classification.selected.detectionId.startsWith("invalid-range:forced:")
  ) {
    const prepared = await prepareInvalidRangeFromCanonicalEvidence({
      evidence,
      txId,
    });
    preparedHeaderHash = prepared.headerHash;
    preparedNodeTxId = prepared.tx.nodeTxId;
    preparedInclusion = prepared.tx.txInclusion;
    violationReason = prepared.tx.violationReason;
    blockSlot = prepared.blockSlot.toString();
    subjectCbor = Data.to(
      SDK.acceptedVerdictSubject(preparedNodeTxId) as never,
      SDK.InvalidRangeVerdictSubjectSchema as never,
    );
  } else if (category === "invalidRange") {
    const forced = await prepareInvalidRangeForcedPlan({
      block: evidence,
    });
    if (
      forced.detectionId !== classification.selected.detectionId ||
      forced.evidence.subject.transaction_id !== txId
    )
      throw new Error("invalidRange forced plan changed classification");
    const transaction =
      evidence.reconstruction.forcedTransactions[
        Number(classification.selected.position)
      ];
    if (transaction === undefined)
      throw new Error(
        "invalidRange forced transaction disappeared from retained DA",
      );
    sourceKind = "forced";
    preparedHeaderHash = forced.headerHash;
    preparedNodeTxId = txId;
    preparedInclusion = {
      nativeTxId: txId,
      nativeTxCompactCbor: forced.nativeTxCompactCbor,
      l2TransactionSourceCbor: Data.to(
        {
          tx_id: transaction.value.tx_id,
          source: transaction.value.submitted_source,
        } as never,
        SDK.L2TransactionSource as never,
      ),
      transactionsPhasRoot: "00".repeat(32),
      txMembershipProofCbor: "",
    };
    violationReason = forced.evidence.subject.rejection_reason as string;
    blockSlot = forced.evidence.blockSlot.toString();
    subjectCbor = Data.to(
      forced.evidence.subject as never,
      SDK.InvalidRangeVerdictSubjectSchema as never,
    );
    forcedSourceCbor = Data.to(
      forced.forcedSource as never,
      SDK.InvalidRangeForcedSourcePayloadSchema as never,
    );
  } else if (
    !classification.selected.detectionId.startsWith("zero-input:forced:")
  ) {
    const prepared = await prepareZeroInputFromCanonicalEvidence({
      evidence,
      txId,
    });
    preparedHeaderHash = prepared.headerHash;
    preparedNodeTxId = prepared.tx.nodeTxId;
    preparedInclusion = prepared.tx.txInclusion;
    violationReason = null;
    blockSlot = null;
    const retained = evidence.transactions.find(
      (transaction) => transaction.nodeTxId === preparedNodeTxId,
    );
    if (retained === undefined)
      throw new Error(
        "zeroInput accepted transaction disappeared from retained DA",
      );
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(retained.txCbor, "hex"),
    );
    const field = material.fieldPreimages[0];
    if (field === undefined)
      throw new Error("zeroInput accepted field 0 disappeared");
    const acceptedEvidence = prepareZeroInputEvidence({
      finding: { subject: SDK.acceptedVerdictSubject(preparedNodeTxId) },
      inputFieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    subjectCbor = Data.to(
      acceptedEvidence.subject as never,
      ZeroInputVerdictSubjectSchema as never,
    );
    inputFieldPreimageCbor = acceptedEvidence.inputFieldPreimageCbor;
    inputFieldCommitment = acceptedEvidence.inputFieldCommitment;
  } else {
    const forced = await prepareZeroInputForcedPlan({
      block: evidence,
    });
    if (
      forced.detectionId !== classification.selected.detectionId ||
      forced.evidence.subject.transaction_id !== txId
    )
      throw new Error("zeroInput forced plan changed classification");
    const transaction =
      evidence.reconstruction.forcedTransactions[
        Number(classification.selected.position)
      ];
    if (transaction === undefined)
      throw new Error(
        "zeroInput forced transaction disappeared from retained DA",
      );
    sourceKind = "forced";
    preparedHeaderHash = forced.headerHash;
    preparedNodeTxId = forced.evidence.subject.transaction_id;
    preparedInclusion = {
      nativeTxId: preparedNodeTxId,
      nativeTxCompactCbor: forced.nativeTxCompactCbor,
      l2TransactionSourceCbor: Data.to(
        {
          tx_id: transaction.value.tx_id,
          source: transaction.value.submitted_source,
        } as never,
        SDK.L2TransactionSource as never,
      ),
      transactionsPhasRoot: "00".repeat(32),
      txMembershipProofCbor: "",
    };
    violationReason = null;
    blockSlot = null;
    subjectCbor = Data.to(
      forced.evidence.subject as never,
      ZeroInputVerdictSubjectSchema as never,
    );
    inputFieldPreimageCbor = forced.evidence.inputFieldPreimageCbor;
    inputFieldCommitment = forced.evidence.inputFieldCommitment;
    forcedSourceCbor = Data.to(
      forced.forcedSource as never,
      ZeroInputForcedSourcePayloadSchema as never,
    );
  }
  if (
    classification.selected.detectionId !==
    (category === "invalidRange"
      ? sourceKind === "forced"
        ? `invalid-range:forced:${classification.selected.position.toString()}:${preparedNodeTxId}:${violationReason}`
        : `invalid-range:${classification.selected.position.toString()}:${preparedNodeTxId}:${violationReason}`
      : sourceKind === "forced"
        ? `zero-input:forced:${classification.selected.position.toString()}:${preparedNodeTxId}`
        : `zero-input:${classification.selected.position.toString()}:${preparedNodeTxId}`)
  ) {
    throw new Error(`${category} prepared transaction changed classification`);
  }
  if (classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${category} detection position exceeds journal encoding`);
  }
  const artifact = normalizeJournalJson({
    schemaVersion: NATIVE_INCLUSION_TWO_STEP_ARTIFACT,
    category,
    headerHash: preparedHeaderHash,
    detectionId: classification.selected.detectionId,
    position: Number(classification.selected.position),
    blockSlot,
    violationReason,
    nativeTxId: preparedInclusion.nativeTxId,
    nativeTxCompactCbor: preparedInclusion.nativeTxCompactCbor,
    l2TransactionSourceCbor: preparedInclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: preparedInclusion.transactionsPhasRoot,
    txMembershipProofCbor: preparedInclusion.txMembershipProofCbor,
    sourceKind,
    subjectCbor,
    inputFieldPreimageCbor,
    inputFieldCommitment,
    forcedSourceCbor,
  }) as NativeInclusionTwoStepArtifact;
  admitNativeInclusionTwoStepArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type WitnessRole = (typeof WITNESS_ROLES)[number];

/** Both categories bind the same roles; only the two step scripts differ. */
export type NativeInclusionTwoStepWorkflowReferenceScripts<
  Category extends
    NativeInclusionTwoStepCategory = NativeInclusionTwoStepCategory,
> = LinearFamilyReferenceScripts<Category, WitnessRole, false>;

export type AssemblyContext<Category extends NativeInclusionTwoStepCategory> =
  LinearFamilyAssemblyContext<Category, WitnessRole, false>;

export type BoundConfig<Category extends NativeInclusionTwoStepCategory> =
  Readonly<{
    category: Category;
    lucid: LucidEvolution;
    blueprint: unknown;
    deploymentInfo: unknown;
    network: FraudProofWorkflowDeploymentBinding<Category>["network"];
    signer: ResolvedProverSigner;
    headerHash: string;
    referenceScripts: NativeInclusionTwoStepWorkflowReferenceScripts<Category>;
    stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    fraudProverRewardLovelace: bigint;
    zeroInputContracts: ZeroInputContracts | null;
    invalidRangeContracts: InvalidRangeContracts | null;
    categoryId: string;
  }>;

import {
  decodeMidgardNativeByteListPreimage,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubjectV1,
  FraudProofComputationThreadStepDatum,
  HeaderV1Schema,
  InputSetUniquenessStep02Datum,
  InputSetUniquenessStep03DatumSchema,
  InputSetUniquenessStep04DatumSchema,
  InputSetUniquenessVerdictSubjectSchema,
  MIDGARD_FIELD_INDEX_V1,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import type { InputSetUniquenessContractsV1 } from "../input-set-uniqueness/contracts-v1.js";
import {
  detectInputSetUniquenessForcedReplayV1,
  INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1,
} from "../input-set-uniqueness/replay-v1.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID_V1,
  type InputSetUniquenessClaimV1,
  scanInputSetUniquenessV1,
} from "../input-set-uniqueness/scan-v1.js";
import { submitInputSetUniquenessForcedStep01V1 } from "../input-set-uniqueness/submit-input-set-uniqueness-forced-step-01.js";
import { submitInputSetUniquenessInit } from "../input-set-uniqueness/submit-input-set-uniqueness-init.js";
import { submitInputSetUniquenessStep01 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-01.js";
import { submitInputSetUniquenessStep02 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-02.js";
import { submitInputSetUniquenessStep03V1 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-03.js";
import {
  submitInputSetUniquenessStep04AdvanceV1,
  submitInputSetUniquenessStep04FinalizeV1,
} from "../input-set-uniqueness/submit-input-set-uniqueness-step-04.js";
import {
  bindForcedDuplicateInputV1,
  inputSetUnionIsStrictlyIncreasingV1,
} from "../input-set-uniqueness/wrongful-rejection-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStoreV1,
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import {
  admitProductionNativeInclusionArtifactV1,
  canonicalHexV1,
  EVEN_HEX_V1,
  exactJournalRecordV1,
  HEX_28_V1,
  HEX_32_V1,
  NATURAL_DECIMAL_V1,
  type ProductionNativeInclusionArtifactV1,
  safeNaturalNumberV1,
} from "./production-native-index-artifact-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  resolveDirectFirstProofChunksV1,
  withProductionProofChunkPrerequisiteV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1 =
  "midgard-production-input-set-uniqueness-artifact-v1" as const;
export const PRODUCTION_INPUT_SET_UNIQUENESS_FORCED_ARTIFACT_V1 =
  "midgard-production-input-set-uniqueness-forced-artifact-v1" as const;

export const InputSetUniquenessForcedSourceV1Schema = Data.Object({
  header: HeaderV1Schema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
});

type ClaimJsonV1 =
  | Readonly<{
      kind: "duplicateSpendInputs" | "duplicateReferenceInputs";
      firstIndex: string;
      secondIndex: string;
    }>
  | Readonly<{
      kind: "spendReferenceOverlap";
      spendIndex: string;
      referenceIndex: string;
    }>;

export type ProductionInputSetUniquenessArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: ProductionNativeInclusionArtifactV1;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
    claim: ClaimJsonV1;
  }>;

export type ProductionInputSetUniquenessForcedArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_INPUT_SET_UNIQUENESS_FORCED_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    forcedIndex: number;
    transactionId: string;
    subjectCbor: string;
    nativeTxCompactCbor: string;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
    forcedSourceCbor: string;
  }>;

type AdmittedAcceptedArtifactV1 = Readonly<{
  sourceKind: "accepted";
  artifact: ProductionInputSetUniquenessArtifactV1;
  inclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  claim: InputSetUniquenessClaimV1;
  spendPlan: FaultProofFieldOpeningPlanV1 | null;
  referencePlan: FaultProofFieldOpeningPlanV1 | null;
}>;

type AdmittedForcedArtifactV1 = Readonly<{
  sourceKind: "forced";
  artifact: ProductionInputSetUniquenessForcedArtifactV1;
  forcedSource: Data.Static<typeof InputSetUniquenessForcedSourceV1Schema>;
  spendPlan: FaultProofFieldOpeningPlanV1;
  referencePlan: FaultProofFieldOpeningPlanV1;
}>;

type AdmittedArtifactV1 = AdmittedAcceptedArtifactV1 | AdmittedForcedArtifactV1;

const inputItems = (
  tx: MidgardNativeTxFullV1,
  field: "spend" | "reference",
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    field === "spend"
      ? tx.body.spendInputsPreimageCbor
      : tx.body.referenceInputsPreimageCbor,
    `${field} inputs`,
  ).map((item) => Buffer.from(item).toString("hex"));

const claimJson = (claim: InputSetUniquenessClaimV1): ClaimJsonV1 =>
  claim.kind === "spendReferenceOverlap"
    ? Object.freeze({
        kind: claim.kind,
        spendIndex: claim.spendIndex.toString(),
        referenceIndex: claim.referenceIndex.toString(),
      })
    : Object.freeze({
        kind: claim.kind,
        firstIndex: claim.firstIndex.toString(),
        secondIndex: claim.secondIndex.toString(),
      });

const claimIdentity = (claim: InputSetUniquenessClaimV1): string =>
  claim.kind === "spendReferenceOverlap"
    ? `${claim.kind}:${claim.spendIndex.toString()}:${claim.referenceIndex.toString()}`
    : `${claim.kind}:${claim.firstIndex.toString()}:${claim.secondIndex.toString()}`;

const parseClaim = (value: unknown): InputSetUniquenessClaimV1 => {
  const record = exactJournalRecordV1(
    value,
    typeof value === "object" && value !== null && "kind" in value
      ? (value as { readonly kind?: unknown }).kind === "spendReferenceOverlap"
        ? ["kind", "spendIndex", "referenceIndex"]
        : ["kind", "firstIndex", "secondIndex"]
      : [],
    "input-set-uniqueness claim",
  );
  const natural = (field: string): bigint => {
    const item = record[field];
    if (typeof item !== "string" || !NATURAL_DECIMAL_V1.test(item)) {
      throw new Error(`input-set-uniqueness claim ${field} is malformed`);
    }
    return BigInt(item);
  };
  if (record.kind === "duplicateSpendInputs") {
    return {
      kind: "duplicateSpendInputs",
      firstIndex: natural("firstIndex"),
      secondIndex: natural("secondIndex"),
    };
  }
  if (record.kind === "duplicateReferenceInputs") {
    return {
      kind: "duplicateReferenceInputs",
      firstIndex: natural("firstIndex"),
      secondIndex: natural("secondIndex"),
    };
  }
  if (record.kind === "spendReferenceOverlap") {
    return {
      kind: "spendReferenceOverlap",
      spendIndex: natural("spendIndex"),
      referenceIndex: natural("referenceIndex"),
    };
  }
  throw new Error("input-set-uniqueness claim kind is unsupported");
};

const parseItemList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) => {
      const parsed = canonicalHexV1(
        item,
        EVEN_HEX_V1,
        `${label}[${index.toString()}]`,
      );
      if (!/^825820[0-9a-f]{64}19[0-9a-f]{4}$/u.test(parsed)) {
        throw new Error(`${label}[${index.toString()}] is not an out-ref item`);
      }
      return parsed;
    }),
  );
};

const admitAcceptedProductionInputSetUniquenessArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedAcceptedArtifactV1 => {
  if (!HEX_28_V1.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "spendInputItemCbors",
      "referenceInputItemCbors",
      "claim",
    ],
    "input-set-uniqueness artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness artifact identity changed");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "input-set-uniqueness header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "input-set-uniqueness position",
  );
  const tx = admitProductionNativeInclusionArtifactV1(
    parsed.tx,
    "input-set-uniqueness transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("input-set-uniqueness transaction is not accepted");
  }
  const spends = parseItemList(
    parsed.spendInputItemCbors,
    "input-set-uniqueness spend inputs",
  );
  const references = parseItemList(
    parsed.referenceInputItemCbors,
    "input-set-uniqueness reference inputs",
  );
  const claim = parseClaim(parsed.claim);
  const rederived = scanInputSetUniquenessV1({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  });
  if (
    !rederived.some(
      (candidate) => claimIdentity(candidate) === claimIdentity(claim),
    )
  ) {
    throw new Error(
      "input-set-uniqueness artifact does not re-derive its claim",
    );
  }
  const expectedDetection = `${INPUT_SET_UNIQUENESS_VIOLATION_ID_V1}:${position.toString()}:${tx.artifact.nativeTxId}:${claimIdentity(claim)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness detection identity changed");
  }
  const plan = (
    fieldIndex: number,
    items: readonly string[],
  ): FaultProofFieldOpeningPlanV1 =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: tx.artifact.nativeTxId,
      nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: carriageOwner,
      label: "input-set-uniqueness artifact",
    });
  const spendPlan =
    claim.kind === "duplicateReferenceInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX_V1.spendInputs, spends);
  const referencePlan =
    claim.kind === "duplicateSpendInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX_V1.referenceInputs, references);
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) satisfies ProductionInputSetUniquenessArtifactV1;
  return Object.freeze({
    sourceKind: "accepted" as const,
    artifact,
    inclusion: tx.inclusion,
    claim,
    spendPlan,
    referencePlan,
  });
};

export const admitProductionInputSetUniquenessForcedArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedForcedArtifactV1 => {
  if (!HEX_28_V1.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "forcedIndex",
      "transactionId",
      "subjectCbor",
      "nativeTxCompactCbor",
      "spendInputItemCbors",
      "referenceInputItemCbors",
      "forcedSourceCbor",
    ],
    "input-set-uniqueness forced artifact",
  );
  if (
    parsed.schemaVersion !==
      PRODUCTION_INPUT_SET_UNIQUENESS_FORCED_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness forced artifact identity changed");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "input-set-uniqueness forced header hash",
  );
  const transactionId = canonicalHexV1(
    parsed.transactionId,
    HEX_32_V1,
    "input-set-uniqueness forced transaction id",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "input-set-uniqueness forced position",
  );
  const forcedIndex = safeNaturalNumberV1(
    parsed.forcedIndex,
    "input-set-uniqueness forced index",
  );
  if (position !== forcedIndex) {
    throw new Error("input-set-uniqueness forced position changed");
  }
  const subjectCbor = canonicalHexV1(
    parsed.subjectCbor,
    EVEN_HEX_V1,
    "input-set-uniqueness forced subject",
  );
  const nativeTxCompactCbor = canonicalHexV1(
    parsed.nativeTxCompactCbor,
    EVEN_HEX_V1,
    "input-set-uniqueness forced compact transaction",
  );
  const forcedSourceCbor = canonicalHexV1(
    parsed.forcedSourceCbor,
    EVEN_HEX_V1,
    "input-set-uniqueness forced source",
  );
  const subject = Data.from(
    subjectCbor,
    InputSetUniquenessVerdictSubjectSchema as never,
  );
  const bound = bindForcedDuplicateInputV1(subject as never);
  const forcedSource = Data.from(
    forcedSourceCbor,
    InputSetUniquenessForcedSourceV1Schema as never,
  ) as Data.Static<typeof InputSetUniquenessForcedSourceV1Schema>;
  const leaf = forcedSource.membership.value;
  if (
    leaf.tx_id !== transactionId ||
    leaf.source.compact_cbor !== nativeTxCompactCbor ||
    leaf.verdict === "ForcedTxValid" ||
    Data.to(
      forcedVerdictSubjectV1({
        transactionId: leaf.tx_id,
        sourceKey: forcedSource.membership.key,
        rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
      }) as never,
      InputSetUniquenessVerdictSubjectSchema as never,
    ) !== subjectCbor
  ) {
    throw new Error("input-set-uniqueness forced subject/source changed");
  }
  const spends = parseItemList(
    parsed.spendInputItemCbors,
    "input-set-uniqueness forced spend inputs",
  );
  const references = parseItemList(
    parsed.referenceInputItemCbors,
    "input-set-uniqueness forced reference inputs",
  );
  if (
    !inputSetUnionIsStrictlyIncreasingV1({
      spendInputItemCbors: spends,
      referenceInputItemCbors: references,
    })
  ) {
    throw new Error("input-set-uniqueness forced union is not unique");
  }
  const count = (field: bigint) =>
    field === 0n
      ? BigInt(spends.length)
      : field === 1n
        ? BigInt(references.length)
        : -1n;
  if (
    bound.first_item_index < 0n ||
    bound.second_item_index < 0n ||
    bound.first_item_index >= count(bound.first_field_index) ||
    bound.second_item_index >= count(bound.second_field_index) ||
    bound.first_field_index > bound.second_field_index ||
    (bound.first_field_index === bound.second_field_index &&
      bound.first_item_index >= bound.second_item_index)
  ) {
    throw new Error("input-set-uniqueness forced coordinates changed");
  }
  const expectedDetection = `${INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transactionId}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness forced detection identity changed");
  }
  const makePlan = (fieldIndex: number, items: readonly string[]) =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: transactionId,
      nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: carriageOwner,
      label: "input-set-uniqueness forced artifact",
    });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_INPUT_SET_UNIQUENESS_FORCED_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    forcedIndex,
    transactionId,
    subjectCbor,
    nativeTxCompactCbor,
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    forcedSourceCbor,
  }) satisfies ProductionInputSetUniquenessForcedArtifactV1;
  return Object.freeze({
    sourceKind: "forced" as const,
    artifact,
    forcedSource,
    spendPlan: makePlan(MIDGARD_FIELD_INDEX_V1.spendInputs, spends),
    referencePlan: makePlan(MIDGARD_FIELD_INDEX_V1.referenceInputs, references),
  });
};

const admitAnyProductionInputSetUniquenessArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifactV1 => {
  const schemaVersion =
    typeof value === "object" && value !== null && "schemaVersion" in value
      ? (value as { readonly schemaVersion?: unknown }).schemaVersion
      : undefined;
  return schemaVersion === PRODUCTION_INPUT_SET_UNIQUENESS_FORCED_ARTIFACT_V1
    ? admitProductionInputSetUniquenessForcedArtifactV1(value, carriageOwner)
    : admitAcceptedProductionInputSetUniquenessArtifactV1(value, carriageOwner);
};

/** Backwards-compatible accepted-invalid artifact admission. */
export const admitProductionInputSetUniquenessArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedAcceptedArtifactV1 =>
  admitAcceptedProductionInputSetUniquenessArtifactV1(value, carriageOwner);

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  if (
    classification.category !== "inputSetUniqueness" ||
    classification.selected.violationId !==
      INPUT_SET_UNIQUENESS_VIOLATION_ID_V1 ||
    fields.length !== 6 ||
    fields[0] !== INPUT_SET_UNIQUENESS_VIOLATION_ID_V1 ||
    !NATURAL_DECIMAL_V1.test(fields[1] ?? "") ||
    !HEX_32_V1.test(fields[2] ?? "") ||
    ![
      "duplicateSpendInputs",
      "duplicateReferenceInputs",
      "spendReferenceOverlap",
    ].includes(fields[3] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[4] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[5] ?? "") ||
    !Number.isSafeInteger(position) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("input-set-uniqueness classification is malformed");
  }
  return Object.freeze({ position, txId: fields[2]! });
};

export const prepareProductionInputSetUniquenessArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<
  | ProductionInputSetUniquenessArtifactV1
  | ProductionInputSetUniquenessForcedArtifactV1
> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("input-set-uniqueness classification changed header");
  }
  if (
    classification.category === "inputSetUniqueness" &&
    classification.selected.violationId ===
      INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID_V1
  ) {
    const detection = detectInputSetUniquenessForcedReplayV1(evidence).find(
      (candidate) =>
        candidate.detectionId === classification.selected.detectionId &&
        candidate.position === classification.selected.position,
    );
    if (detection === undefined) {
      throw new Error(
        "input-set-uniqueness forced classification disappeared on replay",
      );
    }
    const transaction =
      evidence.reconstruction.forcedTransactions[detection.forcedIndex];
    if (transaction === undefined) {
      throw new Error("input-set-uniqueness forced leaf disappeared");
    }
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: evidence.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: { tx_order_id: transaction.key },
      },
    });
    const artifact = normalizeJournalJsonV1({
      schemaVersion: PRODUCTION_INPUT_SET_UNIQUENESS_FORCED_ARTIFACT_V1,
      headerHash: evidence.headerHash,
      detectionId: detection.detectionId,
      position: detection.forcedIndex,
      forcedIndex: detection.forcedIndex,
      transactionId: detection.transactionId,
      subjectCbor: Data.to(
        detection.bound.subject as never,
        InputSetUniquenessVerdictSubjectSchema as never,
      ),
      nativeTxCompactCbor: transaction.value.source.compact_cbor,
      spendInputItemCbors: detection.spendInputItemCbors,
      referenceInputItemCbors: detection.referenceInputItemCbors,
      forcedSourceCbor: Data.to(
        { header: evidence.header, membership } as never,
        InputSetUniquenessForcedSourceV1Schema as never,
      ),
    }) as ProductionInputSetUniquenessForcedArtifactV1;
    admitProductionInputSetUniquenessForcedArtifactV1(artifact);
    return Object.freeze(artifact);
  }
  const selected = selectedIdentity(classification);
  const decoded = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const tx = decoded.find((candidate) => candidate.nodeTxId === selected.txId);
  if (tx === undefined) {
    throw new Error("input-set-uniqueness selected transaction disappeared");
  }
  const spends = inputItems(tx.nativeTx, "spend");
  const references = inputItems(tx.nativeTx, "reference");
  const claim = scanInputSetUniquenessV1({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  }).find((candidate) =>
    classification.selected.detectionId.endsWith(claimIdentity(candidate)),
  );
  if (claim === undefined) {
    throw new Error("input-set-uniqueness selected claim disappeared");
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_INPUT_SET_UNIQUENESS_ARTIFACT_V1,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: tx.nodeTxId,
      nativeTxCompactCbor: tx.nativeCompactCbor,
      l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: requireProof(
        trie,
        transactionSourceTrieItemV1(tx).key,
        "input-set-uniqueness transaction",
      ),
    },
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) as ProductionInputSetUniquenessArtifactV1;
  admitProductionInputSetUniquenessArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type InputSetUniquenessWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: InputSetUniquenessContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["catalogue"];
  referenceScripts: InputSetUniquenessWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const record = (value: unknown, label: string) =>
  exactJournalRecordV1(
    value,
    typeof value === "object" && value !== null ? Object.keys(value) : [],
    label,
  );

const actionInput = (action: FraudProofWorkflowActionV1) => {
  const input = record(action.input, "input-set-uniqueness action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "inputSetUniqueness" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("input-set-uniqueness action identity changed");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`input-set-uniqueness action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfigV1,
  plan: FaultProofFieldOpeningPlanV1 | null,
) => {
  if (plan === null)
    return Object.freeze({ publications: [], certificate: undefined });
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("input-set-uniqueness field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("input-set-uniqueness field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const captureRemoval = async (
  config: BoundConfigV1,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransactionV1(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "inputSetUniqueness",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "input-set-uniqueness removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease ? { mutationLease } : {}),
  });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"inputSetUniqueness"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "inputSetUniqueness",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionInputSetUniquenessArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitAnyProductionInputSetUniquenessArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-set-uniqueness artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      if (admitted.sourceKind === "forced") {
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransactionV1(
            async (preSubmitBoundary) => {
              await submitInputSetUniquenessForcedStep01V1({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                header: admitted.forcedSource.header,
                membership: admitted.forcedSource.membership,
                referenceScriptUtxo: config.referenceScripts.steps[0],
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const chunks = await resolveDirectFirstProofChunksV1({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.tx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.inclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      if (admitted.sourceKind !== "accepted") {
        throw new Error(
          "input-set-uniqueness forced artifact cannot enter accepted step-02",
        );
      }
      const spend = await resolveField(config, admitted.spendPlan);
      const reference = await resolveField(config, admitted.referencePlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              claim: admitted.claim,
              nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedSpendCarriageUtxos: spend.publications,
              ...(spend.certificate === undefined
                ? {}
                : { spendCertificateUtxo: spend.certificate }),
              publishedReferenceCarriageUtxos: reference.publications,
              ...(reference.certificate === undefined
                ? {}
                : { referenceCertificateUtxo: reference.certificate }),
              publishMissingCarriage: false,
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      if (admitted.sourceKind !== "forced") {
        throw new Error(
          "input-set-uniqueness accepted artifact cannot enter forced step-03",
        );
      }
      const spend = await resolveField(config, admitted.spendPlan);
      const reference = await resolveField(config, admitted.referencePlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep03V1({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedSpendCarriageUtxos: spend.publications,
              publishedReferenceCarriageUtxos: reference.publications,
              ...(spend.certificate === undefined
                ? {}
                : { spendCertificateUtxo: spend.certificate }),
              ...(reference.certificate === undefined
                ? {}
                : { referenceCertificateUtxo: reference.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[2],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      if (admitted.sourceKind !== "forced") {
        throw new Error(
          "input-set-uniqueness accepted artifact cannot enter forced step-04",
        );
      }
      const threadOutRef = stringField(input, "threadOutRef");
      const thread = await fetchUtxoByOutRef({
        lucid: config.lucid,
        outRef: parseOutRef(threadOutRef, "input-set-uniqueness thread"),
        label: "input-set-uniqueness forced step-04 thread",
      });
      if (thread.datum == null) {
        throw new Error("input-set-uniqueness forced thread omitted datum");
      }
      const state = Data.from(
        thread.datum,
        InputSetUniquenessStep04DatumSchema as never,
      ) as {
        data: { cursor: bigint; spend_count: bigint; reference_count: bigint };
      };
      const total = state.data.spend_count + state.data.reference_count;
      if (state.data.cursor === total) {
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransactionV1(
            async (preSubmitBoundary) => {
              await submitInputSetUniquenessStep04FinalizeV1({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef,
                spendInputItemCbors: admitted.artifact.spendInputItemCbors,
                referenceInputItemCbors:
                  admitted.artifact.referenceInputItemCbors,
                referenceScriptUtxo: config.referenceScripts.steps[3],
                witnessReferenceScripts: config.referenceScripts.witnesses,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const readingSpend = state.data.cursor < state.data.spend_count;
      const opening = await resolveField(
        config,
        readingSpend ? admitted.spendPlan : admitted.referencePlan,
      );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep04AdvanceV1({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef,
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedCarriageUtxos: opening.publications,
              ...(opening.certificate === undefined
                ? {}
                : { certificateUtxo: opening.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[3],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") return await captureRemoval(config, input);
    throw new Error(
      `unsupported input-set-uniqueness stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInputSetUniquenessWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InputSetUniquenessWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInputSetUniquenessWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"inputSetUniqueness">;
  l1: FraudProofFamilyL1ObservationPortV1<"inputSetUniqueness">;
  transactions: ProductionLinearFamilyTransactionPortV1<"inputSetUniqueness">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundInputSetUniquenessWorkflowV1 = async (
  config: ManifestBoundInputSetUniquenessWorkflowConfigV1,
): Promise<ManifestBoundInputSetUniquenessWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "inputSetUniqueness",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      InputSetUniquenessStep02Datum,
      InputSetUniquenessStep03DatumSchema,
      InputSetUniquenessStep04DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error("input-set-uniqueness manifest omitted certificate policy");
  }
  const certificate = binding.fieldPreimageCertificate;
  const chain = binding.resolvedContracts.contracts.inputSetUniqueness;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    stateQueuePolicyId === undefined ||
    chain === undefined ||
    chain.steps.length !== 4
  ) {
    throw new Error("input-set-uniqueness deployment chain is incomplete");
  }
  const contracts: InputSetUniquenessContractsV1 = {
    steps: [chain.steps[0]!, chain.steps[1]!, chain.steps[2]!, chain.steps[3]!],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  };
  const ref = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo,
    });
  const references: InputSetUniquenessWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze([
        ref("fraudProofInputSetUniqueness", config.referenceScripts.steps[0]),
        ref(
          "fraudProofInputSetUniquenessStep02",
          config.referenceScripts.steps[1],
        ),
        ref(
          "fraudProofInputSetUniquenessStep03",
          config.referenceScripts.steps[2],
        ),
        ref(
          "fraudProofInputSetUniquenessStep04",
          config.referenceScripts.steps[3],
        ),
      ] as const),
      witnesses: Object.freeze({
        computationThreadMint: ref(
          "computationThreadMint",
          config.referenceScripts.witnesses.computationThreadMint,
        ),
        fraudProofMint: ref(
          "fraudProofMint",
          config.referenceScripts.witnesses.fraudProofMint,
        ),
        phasMembershipWithdraw: ref(
          "phasMembershipWithdraw",
          config.referenceScripts.witnesses.phasMembershipWithdraw,
        ),
        chunkedVerifyWithdraw: ref(
          "chunkedVerifyWithdraw",
          config.referenceScripts.witnesses.chunkedVerifyWithdraw,
        ),
      }),
      fieldPreimageCertificateMint: ref(
        "fieldPreimageCertificateMint",
        config.referenceScripts.fieldPreimageCertificateMint,
      ),
    });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined) {
    throw new Error("input-set-uniqueness raw-L1 omitted publications");
  }
  const publications = l1.publications;
  const transactions = createTransactionPort({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    contracts,
    category: binding.resolvedContracts.category,
    catalogue: binding.catalogue,
    referenceScripts: references,
    certificate,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "inputSetUniqueness",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = (
    selector: (
      artifact: AdmittedArtifactV1,
    ) => FaultProofFieldOpeningPlanV1 | null,
  ) =>
    createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "inputSetUniqueness",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications,
      requirementForAction: ({ action, artifact }) => {
        const input = actionInput(action);
        const admitted = admitAnyProductionInputSetUniquenessArtifactV1(
          artifact,
          config.signer.paymentKeyHash,
        );
        if (
          (admitted.sourceKind === "accepted" && input.stage !== "step_02") ||
          (admitted.sourceKind === "forced" &&
            input.stage !== "step_03" &&
            input.stage !== "step_04")
        ) {
          return null;
        }
        const plan = selector(admitted);
        return plan === null
          ? null
          : ({
              planned: plan,
              compactCbor: plan.nativeTxCompactCbor,
              certificate: {
                policyId: certificate.policyId,
                mintingScript: certificate.mintingScript,
                referenceScriptUtxo: references.fieldPreimageCertificateMint,
              },
            } satisfies ProductionFieldCarriageRequirementV1);
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: fieldPrerequisite((artifact) => artifact.spendPlan),
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: fieldPrerequisite((artifact) => artifact.referencePlan),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: createAuthenticatedProofChunkPrerequisitePortV1({
      category: "inputSetUniqueness",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications,
      proofCborForAction: ({ action, artifact }) =>
        (() => {
          if (actionInput(action).stage !== "step_01") return null;
          const admitted = admitAnyProductionInputSetUniquenessArtifactV1(
            artifact,
            config.signer.paymentKeyHash,
          );
          return admitted.sourceKind === "accepted"
            ? admitted.artifact.tx.txMembershipProofCbor
            : null;
        })(),
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const runOrResumeManifestBoundInputSetUniquenessWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInputSetUniquenessWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["inputSetUniqueness"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

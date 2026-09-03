import {
  decodeMidgardNativeByteListPreimage,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  ForcedInclusionTxSchema,
  forcedVerdictSubject,
  FraudProofComputationThreadStepDatum,
  HeaderSchema,
  InputSetUniquenessStep02Datum,
  InputSetUniquenessStep03DatumSchema,
  InputSetUniquenessStep04DatumSchema,
  InputSetUniquenessVerdictSubjectSchema,
  MIDGARD_FIELD_INDEX,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import type { InputSetUniquenessContracts } from "../input-set-uniqueness/contracts-v1.js";
import {
  detectInputSetUniquenessForcedReplay,
  INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../input-set-uniqueness/replay-v1.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID,
  type InputSetUniquenessClaim,
  scanInputSetUniqueness,
} from "../input-set-uniqueness/scan-v1.js";
import { submitInputSetUniquenessForcedStep01 } from "../input-set-uniqueness/submit-input-set-uniqueness-forced-step-01.js";
import { submitInputSetUniquenessInit } from "../input-set-uniqueness/submit-input-set-uniqueness-init.js";
import { submitInputSetUniquenessStep01 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-01.js";
import { submitInputSetUniquenessStep02 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-02.js";
import { submitInputSetUniquenessStep03 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-03.js";
import {
  submitInputSetUniquenessStep04Advance,
  submitInputSetUniquenessStep04Finalize,
} from "../input-set-uniqueness/submit-input-set-uniqueness-step-04.js";
import {
  bindForcedDuplicateInput,
  inputSetUnionIsStrictlyIncreasing,
} from "../input-set-uniqueness/wrongful-rejection-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassification } from "./classification-v1.js";
import { INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./production-linear-family-adapter-v1.js";
import {
  admitNativeInclusionArtifact,
  canonicalHex,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  type NativeInclusionArtifact,
  NATURAL_DECIMAL,
  safeNaturalNumber,
} from "./production-native-index-artifact-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

export const INPUT_SET_UNIQUENESS_ARTIFACT =
  "midgard-production-input-set-uniqueness-artifact-v1" as const;
export const INPUT_SET_UNIQUENESS_FORCED_ARTIFACT =
  "midgard-production-input-set-uniqueness-forced-artifact-v1" as const;

export const InputSetUniquenessForcedSourceSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxSchema,
  ),
});

type ClaimJson =
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

export type InputSetUniquenessArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INPUT_SET_UNIQUENESS_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
    claim: ClaimJson;
  }>;

export type InputSetUniquenessForcedArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INPUT_SET_UNIQUENESS_FORCED_ARTIFACT;
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

type AdmittedAcceptedArtifact = Readonly<{
  sourceKind: "accepted";
  artifact: InputSetUniquenessArtifact;
  inclusion: ReturnType<typeof admitNativeInclusionArtifact>["inclusion"];
  claim: InputSetUniquenessClaim;
  spendPlan: FaultProofFieldOpeningPlan | null;
  referencePlan: FaultProofFieldOpeningPlan | null;
}>;

type AdmittedForcedArtifact = Readonly<{
  sourceKind: "forced";
  artifact: InputSetUniquenessForcedArtifact;
  forcedSource: Data.Static<typeof InputSetUniquenessForcedSourceSchema>;
  spendPlan: FaultProofFieldOpeningPlan;
  referencePlan: FaultProofFieldOpeningPlan;
}>;

type AdmittedArtifact = AdmittedAcceptedArtifact | AdmittedForcedArtifact;

const inputItems = (
  tx: MidgardNativeTxFull,
  field: "spend" | "reference",
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    field === "spend"
      ? tx.body.spendInputsPreimageCbor
      : tx.body.referenceInputsPreimageCbor,
    `${field} inputs`,
  ).map((item) => Buffer.from(item).toString("hex"));

const claimJson = (claim: InputSetUniquenessClaim): ClaimJson =>
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

const claimIdentity = (claim: InputSetUniquenessClaim): string =>
  claim.kind === "spendReferenceOverlap"
    ? `${claim.kind}:${claim.spendIndex.toString()}:${claim.referenceIndex.toString()}`
    : `${claim.kind}:${claim.firstIndex.toString()}:${claim.secondIndex.toString()}`;

const parseClaim = (value: unknown): InputSetUniquenessClaim => {
  const record = exactJournalRecord(
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
    if (typeof item !== "string" || !NATURAL_DECIMAL.test(item)) {
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
      const parsed = canonicalHex(
        item,
        EVEN_HEX,
        `${label}[${index.toString()}]`,
      );
      if (!/^825820[0-9a-f]{64}19[0-9a-f]{4}$/u.test(parsed)) {
        throw new Error(`${label}[${index.toString()}] is not an out-ref item`);
      }
      return parsed;
    }),
  );
};

const admitAcceptedInputSetUniquenessArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedAcceptedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
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
    parsed.schemaVersion !== INPUT_SET_UNIQUENESS_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "input-set-uniqueness header hash",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "input-set-uniqueness position",
  );
  const tx = admitNativeInclusionArtifact(
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
  const rederived = scanInputSetUniqueness({
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
  const expectedDetection = `${INPUT_SET_UNIQUENESS_VIOLATION_ID}:${position.toString()}:${tx.artifact.nativeTxId}:${claimIdentity(claim)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness detection identity changed");
  }
  const plan = (
    fieldIndex: number,
    items: readonly string[],
  ): FaultProofFieldOpeningPlan =>
    planFaultProofFieldOpening({
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
      : plan(MIDGARD_FIELD_INDEX.spendInputs, spends);
  const referencePlan =
    claim.kind === "duplicateSpendInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX.referenceInputs, references);
  const artifact = Object.freeze({
    schemaVersion: INPUT_SET_UNIQUENESS_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) satisfies InputSetUniquenessArtifact;
  return Object.freeze({
    sourceKind: "accepted" as const,
    artifact,
    inclusion: tx.inclusion,
    claim,
    spendPlan,
    referencePlan,
  });
};

export const admitInputSetUniquenessForcedArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedForcedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
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
    parsed.schemaVersion !== INPUT_SET_UNIQUENESS_FORCED_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness forced artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "input-set-uniqueness forced header hash",
  );
  const transactionId = canonicalHex(
    parsed.transactionId,
    HEX_32,
    "input-set-uniqueness forced transaction id",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "input-set-uniqueness forced position",
  );
  const forcedIndex = safeNaturalNumber(
    parsed.forcedIndex,
    "input-set-uniqueness forced index",
  );
  if (position !== forcedIndex) {
    throw new Error("input-set-uniqueness forced position changed");
  }
  const subjectCbor = canonicalHex(
    parsed.subjectCbor,
    EVEN_HEX,
    "input-set-uniqueness forced subject",
  );
  const nativeTxCompactCbor = canonicalHex(
    parsed.nativeTxCompactCbor,
    EVEN_HEX,
    "input-set-uniqueness forced compact transaction",
  );
  const forcedSourceCbor = canonicalHex(
    parsed.forcedSourceCbor,
    EVEN_HEX,
    "input-set-uniqueness forced source",
  );
  const subject = Data.from(
    subjectCbor,
    InputSetUniquenessVerdictSubjectSchema as never,
  );
  const bound = bindForcedDuplicateInput(subject as never);
  const forcedSource = Data.from(
    forcedSourceCbor,
    InputSetUniquenessForcedSourceSchema as never,
  ) as Data.Static<typeof InputSetUniquenessForcedSourceSchema>;
  const leaf = forcedSource.membership.value;
  if (
    leaf.tx_id !== transactionId ||
    leaf.source.compact_cbor !== nativeTxCompactCbor ||
    leaf.verdict === "ForcedTxValid" ||
    Data.to(
      forcedVerdictSubject({
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
    !inputSetUnionIsStrictlyIncreasing({
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
  const expectedDetection = `${INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transactionId}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness forced detection identity changed");
  }
  const makePlan = (fieldIndex: number, items: readonly string[]) =>
    planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: transactionId,
      nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: carriageOwner,
      label: "input-set-uniqueness forced artifact",
    });
  const artifact = Object.freeze({
    schemaVersion: INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
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
  }) satisfies InputSetUniquenessForcedArtifact;
  return Object.freeze({
    sourceKind: "forced" as const,
    artifact,
    forcedSource,
    spendPlan: makePlan(MIDGARD_FIELD_INDEX.spendInputs, spends),
    referencePlan: makePlan(MIDGARD_FIELD_INDEX.referenceInputs, references),
  });
};

const admitAnyInputSetUniquenessArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
  const schemaVersion =
    typeof value === "object" && value !== null && "schemaVersion" in value
      ? (value as { readonly schemaVersion?: unknown }).schemaVersion
      : undefined;
  return schemaVersion === INPUT_SET_UNIQUENESS_FORCED_ARTIFACT
    ? admitInputSetUniquenessForcedArtifact(value, carriageOwner)
    : admitAcceptedInputSetUniquenessArtifact(value, carriageOwner);
};

/** Backwards-compatible accepted-invalid artifact admission. */
export const admitInputSetUniquenessArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedAcceptedArtifact =>
  admitAcceptedInputSetUniquenessArtifact(value, carriageOwner);

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  if (
    classification.category !== "inputSetUniqueness" ||
    classification.selected.violationId !== INPUT_SET_UNIQUENESS_VIOLATION_ID ||
    fields.length !== 6 ||
    fields[0] !== INPUT_SET_UNIQUENESS_VIOLATION_ID ||
    !NATURAL_DECIMAL.test(fields[1] ?? "") ||
    !HEX_32.test(fields[2] ?? "") ||
    ![
      "duplicateSpendInputs",
      "duplicateReferenceInputs",
      "spendReferenceOverlap",
    ].includes(fields[3] ?? "") ||
    !NATURAL_DECIMAL.test(fields[4] ?? "") ||
    !NATURAL_DECIMAL.test(fields[5] ?? "") ||
    !Number.isSafeInteger(position) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("input-set-uniqueness classification is malformed");
  }
  return Object.freeze({ position, txId: fields[2]! });
};

export const prepareInputSetUniquenessArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<InputSetUniquenessArtifact | InputSetUniquenessForcedArtifact> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("input-set-uniqueness classification changed header");
  }
  if (
    classification.category === "inputSetUniqueness" &&
    classification.selected.violationId ===
      INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID
  ) {
    const detection = detectInputSetUniquenessForcedReplay(evidence).find(
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
    const artifact = normalizeJournalJson({
      schemaVersion: INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
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
        InputSetUniquenessForcedSourceSchema as never,
      ),
    }) as InputSetUniquenessForcedArtifact;
    admitInputSetUniquenessForcedArtifact(artifact);
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
  const claim = scanInputSetUniqueness({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  }).find((candidate) =>
    classification.selected.detectionId.endsWith(claimIdentity(candidate)),
  );
  if (claim === undefined) {
    throw new Error("input-set-uniqueness selected claim disappeared");
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  const artifact = normalizeJournalJson({
    schemaVersion: INPUT_SET_UNIQUENESS_ARTIFACT,
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
        transactionSourceTrieItem(tx).key,
        "input-set-uniqueness transaction",
      ),
    },
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) as InputSetUniquenessArtifact;
  admitInputSetUniquenessArtifact(artifact);
  return Object.freeze(artifact);
};

export type InputSetUniquenessWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: InputSetUniquenessContracts;
  category: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["catalogue"];
  referenceScripts: InputSetUniquenessWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const record = (value: unknown, label: string) =>
  exactJournalRecord(
    value,
    typeof value === "object" && value !== null ? Object.keys(value) : [],
    label,
  );

const actionInput = (action: FraudProofWorkflowAction) => {
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
  config: BoundConfig,
  plan: FaultProofFieldOpeningPlan | null,
) => {
  if (plan === null)
    return Object.freeze({ publications: [], certificate: undefined });
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("input-set-uniqueness field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
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
  const transaction = await captureLocallyEvaluatedTransaction(
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
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
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
  config: BoundConfig,
): LinearFamilyTransactionPort<"inputSetUniqueness"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "inputSetUniqueness",
  prepare: async ({ evidence, classification }) =>
    await prepareInputSetUniquenessArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitAnyInputSetUniquenessArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-set-uniqueness artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitInputSetUniquenessForcedStep01({
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
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.tx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep03({
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
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitInputSetUniquenessStep04Finalize({
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
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep04Advance({
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

export type ManifestBoundInputSetUniquenessWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InputSetUniquenessWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInputSetUniquenessWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">;
  l1: FraudProofFamilyL1ObservationPort<"inputSetUniqueness">;
  transactions: LinearFamilyTransactionPort<"inputSetUniqueness">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundInputSetUniquenessWorkflow = async (
  config: ManifestBoundInputSetUniquenessWorkflowConfig,
): Promise<ManifestBoundInputSetUniquenessWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
  const contracts: InputSetUniquenessContracts = {
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
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo,
    });
  const references: InputSetUniquenessWorkflowReferenceScripts = Object.freeze({
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
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
  let adapter = createLinearFamilyWorkflowAdapter({
    category: "inputSetUniqueness",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = (
    selector: (artifact: AdmittedArtifact) => FaultProofFieldOpeningPlan | null,
  ) =>
    createAuthenticatedFieldCarriagePrerequisitePort({
      category: "inputSetUniqueness",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications,
      requirementForAction: ({ action, artifact }) => {
        const input = actionInput(action);
        const admitted = admitAnyInputSetUniquenessArtifact(
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
            } satisfies FieldCarriageRequirement);
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    });
  adapter = withFieldCarriagePrerequisite({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: fieldPrerequisite((artifact) => artifact.spendPlan),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: fieldPrerequisite((artifact) => artifact.referencePlan),
  });
  adapter = withProofChunkPrerequisite({
    category: "inputSetUniqueness",
    base: adapter,
    prerequisite: createAuthenticatedProofChunkPrerequisitePort({
      category: "inputSetUniqueness",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications,
      proofCborForAction: ({ action, artifact }) =>
        (() => {
          if (actionInput(action).stage !== "step_01") return null;
          const admitted = admitAnyInputSetUniquenessArtifact(
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
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundInputSetUniquenessWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInputSetUniquenessWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["inputSetUniqueness"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

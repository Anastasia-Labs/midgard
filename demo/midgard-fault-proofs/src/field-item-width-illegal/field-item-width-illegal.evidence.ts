import {
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  locateMidgardFieldItemFault,
  midgardFieldCommitment,
  midgardFieldItemWidthIsLegal,
  midgardFieldStride,
  midgardFixedFieldItemIsLegal,
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  leafAdmitsFault,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { verdictDetectionSubject } from "../workflow/detection-subject.js";
import type { ReplayPrerequisiteFailure } from "../workflow/replay-prerequisite.js";

export const FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY =
  "fieldItemWidthIllegal" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_PROPOSED_ID = "00000021" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_OUTPUTS_FIELD = 2;
export const FIELD_ITEM_WIDTH_ILLEGAL_MINT_FIELD = 5;
export const FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES = 16_384;

const fail = (message: string): never => {
  throw new Error(`${FIELD_ITEM_WIDTH_ILLEGAL_CATEGORY}: ${message}`);
};

export const fieldItemWidthCoordinateIsSupported = (
  fieldIndex: bigint,
  itemIndex: bigint,
): boolean =>
  itemIndex >= 0n &&
  ((fieldIndex >= 0n && fieldIndex <= 5n) || fieldIndex === 7n);

export const fieldItemWidthIsIllegal = (
  fieldIndex: number,
  itemWidth: number,
): boolean => {
  if (!Number.isSafeInteger(itemWidth) || itemWidth < 0)
    return fail("item width must be a non-negative safe integer");
  if (
    !Number.isSafeInteger(fieldIndex) ||
    !fieldItemWidthCoordinateIsSupported(BigInt(fieldIndex), 0n)
  )
    return fail(`field ${fieldIndex.toString()} is outside the width family`);
  return !midgardFieldItemWidthIsLegal(fieldIndex, itemWidth);
};

export type FieldItemWidthFinding = Readonly<{
  subject: VerdictSubject;
  fieldIndex: bigint;
  itemIndex: bigint;
}>;

const namesObservedItem = ({
  subject,
  fieldIndex,
  itemIndex,
}: FieldItemWidthFinding): boolean => {
  const reason = subject.rejection_reason;
  return (
    subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION &&
    reason !== null &&
    typeof reason !== "string" &&
    "FieldItemWidthIllegal" in reason &&
    reason.FieldItemWidthIllegal.field_index === fieldIndex &&
    reason.FieldItemWidthIllegal.item_index === itemIndex
  );
};

/** Mirrors bind_coordinate_v1, including exact signed static-domain claims. */
export const classifyFieldItemWidthFinding = (
  finding: FieldItemWidthFinding,
): FieldItemWidthFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  if (
    typeof finding.fieldIndex !== "bigint" ||
    typeof finding.itemIndex !== "bigint"
  )
    return fail("field/item coordinates must be exact signed integers");
  if (
    !fieldItemWidthCoordinateIsSupported(
      finding.fieldIndex,
      finding.itemIndex,
    ) &&
    !namesObservedItem(finding)
  )
    return fail(
      "unsupported coordinate is not the exact typed rejection reason",
    );
  return Object.freeze({ ...finding });
};

type ItemObservation =
  | "ItemOutOfDomain"
  | Readonly<{
      ItemLegality: Readonly<{ item_is_legal: boolean }>;
    }>;

type EvidenceObservation = FieldItemWidthFinding &
  Readonly<{
    observation: ItemObservation;
    decisiveFaultHolds: boolean;
  }>;

export type FieldItemWidthStaticDomainEvidence = EvidenceObservation &
  Readonly<{
    kind: "StaticDomain";
    fieldPreimageHex: null;
    fieldCommitmentHex: null;
    itemHex: null;
    itemWidth: null;
    itemCount: null;
    carriage: null;
  }>;

export type FieldItemWidthFieldEvidence = EvidenceObservation &
  Readonly<{
    kind: "Field";
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    itemHex: string | null;
    itemWidth: number | null;
    itemCount: number;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export type FieldItemWidthEvidence =
  | FieldItemWidthStaticDomainEvidence
  | FieldItemWidthFieldEvidence;

type FieldItemWidthObservation =
  | FieldItemWidthStaticDomainEvidence
  | Omit<FieldItemWidthFieldEvidence, "carriage">;

type FieldEvidenceInput = Readonly<{
  finding: FieldItemWidthFinding;
  fieldPreimage: Uint8Array;
  committedFieldHashHex: string;
}>;
type StaticEvidenceInput = Readonly<{
  finding: FieldItemWidthFinding;
  fieldPreimage?: undefined;
  committedFieldHashHex?: undefined;
}>;

export function prepareFieldItemWidthEvidence(
  input: FieldEvidenceInput,
): FieldItemWidthFieldEvidence;
export function prepareFieldItemWidthEvidence(
  input: StaticEvidenceInput,
): FieldItemWidthStaticDomainEvidence;
export function prepareFieldItemWidthEvidence(
  input: FieldEvidenceInput | StaticEvidenceInput,
): FieldItemWidthEvidence;
export function prepareFieldItemWidthEvidence(
  input: FieldEvidenceInput | StaticEvidenceInput,
): FieldItemWidthEvidence {
  const observed = observeFieldItemWidthEvidence(input);
  if (observed.kind === "StaticDomain") return observed;
  return Object.freeze({
    ...observed,
    carriage: selectMidgardFieldCarriageTier(
      observed.fieldPreimageHex.length / 2,
    ),
  });
}

/** Preserve committed bytes; observation remains possible beyond the carriage ceiling. */
function observeFieldItemWidthEvidence({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: FieldEvidenceInput | StaticEvidenceInput): FieldItemWidthObservation {
  const finding = classifyFieldItemWidthFinding(rawFinding);
  if (
    !fieldItemWidthCoordinateIsSupported(finding.fieldIndex, finding.itemIndex)
  ) {
    if (fieldPreimage !== undefined || committedFieldHashHex !== undefined)
      return fail("static domain evidence does not consume field bytes");
    return Object.freeze({
      ...finding,
      kind: "StaticDomain",
      observation: "ItemOutOfDomain",
      decisiveFaultHolds: false,
      fieldPreimageHex: null,
      fieldCommitmentHex: null,
      itemHex: null,
      itemWidth: null,
      itemCount: null,
      carriage: null,
    });
  }
  if (fieldPreimage === undefined || committedFieldHashHex === undefined)
    return fail("supported coordinate requires its authenticated whole field");
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex))
    return fail("committed field hash must be 32 bytes of lowercase hex");
  const field = Buffer.from(fieldPreimage);
  const actualCommitment = midgardFieldCommitment(field).toString("hex");
  if (actualCommitment !== committedFieldHashHex)
    return fail("retained field preimage does not match committed field hash");
  const fieldIndex = Number(finding.fieldIndex); // Supported fields are exactly 0..5 and 7.
  const stride = midgardFieldStride(fieldIndex);
  let count: number;
  let item: Buffer | null = null;
  let itemWidth: number | null = null;
  let observation: ItemObservation = "ItemOutOfDomain";
  if (stride !== 0) {
    const { count: itemCount, nextOffset } =
      decodeMidgardFieldArrayHeader(field);
    count = itemCount;
    if (field.length !== nextOffset + count * stride)
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        "field count and fixed stride do not match its length",
      );
    if (finding.itemIndex < BigInt(count)) {
      const itemIndex = Number(finding.itemIndex); // Bounded by the authenticated count.
      const offset = nextOffset + itemIndex * stride;
      item = field.subarray(offset, offset + stride);
      const previous =
        itemIndex === 0 ? null : field.subarray(offset - stride, offset);
      itemWidth = stride - 2;
      observation = {
        ItemLegality: {
          item_is_legal: midgardFixedFieldItemIsLegal(
            fieldIndex,
            previous,
            item,
          ),
        },
      };
    }
  } else {
    const items = decodeMidgardFieldPreimage(field);
    count = items.length;
    if (finding.itemIndex < BigInt(count)) {
      item = items[Number(finding.itemIndex)]!;
      itemWidth = item.length;
      observation = {
        ItemLegality: {
          item_is_legal: midgardFieldItemWidthIsLegal(fieldIndex, itemWidth),
        },
      };
    }
  }
  return Object.freeze({
    ...finding,
    kind: "Field",
    fieldPreimageHex: field.toString("hex"),
    fieldCommitmentHex: actualCommitment,
    itemHex: item?.toString("hex") ?? null,
    itemWidth,
    itemCount: count,
    observation,
    decisiveFaultHolds:
      observation !== "ItemOutOfDomain" &&
      !observation.ItemLegality.item_is_legal,
  });
}

/** Twin of terminal_contradiction_v1; a later claim does not hide an earlier fault. */
export const fieldItemWidthEvidenceCloses = (
  evidence: EvidenceObservation,
): boolean => {
  classifyFieldItemWidthFinding(evidence);
  if (
    evidence.observation === "ItemOutOfDomain" ||
    evidence.observation.ItemLegality.item_is_legal
  )
    return namesObservedItem(evidence);
  return !leafAdmitsFault(evidence.subject, {
    phase: "canonical",
    position: {
      kind: "item",
      fieldIndex: evidence.fieldIndex,
      itemIndex: evidence.itemIndex,
    },
  });
};

const isFieldGrammarError = (error: unknown): error is MidgardTxCodecError =>
  error instanceof MidgardTxCodecError &&
  error.code === MidgardTxCodecErrorCodes.CborDecode;

type FaultMaterial =
  | ReturnType<typeof deriveMidgardNativeTxFaultEvidenceMaterial>
  | ReturnType<typeof deriveMidgardForcedTxFaultEvidenceMaterial>;

const evidenceAt = (
  material: FaultMaterial,
  finding: FieldItemWidthFinding,
): FieldItemWidthObservation => {
  if (
    !fieldItemWidthCoordinateIsSupported(finding.fieldIndex, finding.itemIndex)
  )
    return observeFieldItemWidthEvidence({ finding });
  const fieldPreimage = material.fieldPreimages[Number(finding.fieldIndex)]!;
  return observeFieldItemWidthEvidence({
    finding,
    fieldPreimage,
    committedFieldHashHex:
      midgardFieldCommitment(fieldPreimage).toString("hex"),
  });
};

const transactionFinding = (
  material: FaultMaterial,
  subject: VerdictSubject,
): FieldItemWidthObservation | null => {
  const reason = subject.rejection_reason;
  let grammarError: MidgardTxCodecError | undefined;
  if (
    reason !== null &&
    typeof reason !== "string" &&
    "FieldItemWidthIllegal" in reason
  ) {
    const coordinate = reason.FieldItemWidthIllegal;
    try {
      const named = evidenceAt(material, {
        subject,
        fieldIndex: coordinate.field_index,
        itemIndex: coordinate.item_index,
      });
      if (fieldItemWidthEvidenceCloses(named)) return named;
    } catch (error) {
      if (!isFieldGrammarError(error)) throw error;
      grammarError = error;
    }
  }
  // Each authenticated field can independently contradict the committed verdict.
  // A different field's grammar must not prevent opening that candidate.
  for (const [fieldIndex, field] of material.fieldPreimages.entries()) {
    try {
      const fault = locateMidgardFieldItemFault(fieldIndex, field);
      if (fault !== null) {
        const observed = evidenceAt(material, {
          subject,
          fieldIndex: BigInt(fault.fieldIndex),
          itemIndex: BigInt(fault.itemIndex),
        });
        if (fieldItemWidthEvidenceCloses(observed)) return observed;
        // A resolved prefix ends at its first fault admitted by the verdict.
        if (grammarError === undefined) return null;
      }
    } catch (error) {
      if (!isFieldGrammarError(error)) throw error;
      grammarError ??= error;
    }
  }
  if (grammarError !== undefined) throw grammarError;
  return null;
};

const transactionDetectionIdPrefix = (
  subject: VerdictSubject,
  position: bigint,
  violationId: string,
): string =>
  `${violationId}:${subject.source_kind === 1n ? "forced:" : ""}${position.toString()}:${subject.transaction_id}:`;

export const fieldItemWidthDetectionId = (
  {
    evidence,
    position,
  }: Readonly<{ evidence: FieldItemWidthObservation; position: bigint }>,
  violationId: string,
): string =>
  `${transactionDetectionIdPrefix(evidence.subject, position, violationId)}${evidence.fieldIndex.toString()}:${evidence.itemIndex.toString()}:${evidence.itemWidth?.toString() ?? "domain"}`;

export const fieldItemWidthFindingsFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
  selection?: Readonly<{ detectionId: string; violationId: string }>,
): Readonly<{
  findings: readonly Readonly<{
    evidence: FieldItemWidthObservation;
    position: bigint;
  }>[];
  failures: readonly ReplayPrerequisiteFailure[];
}> => {
  const findings: { evidence: FieldItemWidthObservation; position: bigint }[] =
    [];
  const failures: ReplayPrerequisiteFailure[] = [];
  const observe = (
    material: FaultMaterial,
    subject: VerdictSubject,
    position: bigint,
  ) => {
    // Recovery selects after source authentication; other subjects' inner
    // grammar cannot prevent opening this exact independently provable item.
    if (
      selection !== undefined &&
      !selection.detectionId.startsWith(
        transactionDetectionIdPrefix(subject, position, selection.violationId),
      )
    )
      return;
    const eventKeyCbor =
      verdictDetectionSubject(subject).subjectEventKeyCbors[0]!;
    try {
      const evidence = transactionFinding(material, subject);
      if (evidence !== null) findings.push({ evidence, position });
    } catch (error) {
      // Only whole-field grammar refusal is outside this family's domain.
      // Source decoding and identity checks stay outside this boundary.
      if (!isFieldGrammarError(error)) throw error;
      failures.push({
        headerHash: block.headerHash,
        eventKeyCbor,
        prerequisite: "representable_field_shape",
      });
    }
  };
  for (const [index, transaction] of block.transactions.entries()) {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    const transactionId = material.transactionId.toString("hex");
    if (transaction.nodeTxId !== transactionId)
      return fail("retained-DA transaction identity changed");
    observe(material, acceptedVerdictSubject(transactionId), BigInt(index));
  }
  for (const [
    index,
    transaction,
  ] of block.reconstruction.forcedTransactions.entries()) {
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      transaction.fullTransactionCbor,
    );
    if (
      material.transactionId.toString("hex") !== transaction.value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        transaction.value.submitted_source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        transaction.value.submitted_source.witness_set_compact_cbor ||
      material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
        transaction.value.submitted_source.field_preimage_lengths_cbor
    )
      return fail("forced transaction differs from its authenticated leaf");
    const subject = forcedVerdictSubject({
      transactionId: transaction.value.tx_id,
      sourceKey: transaction.key,
      rejectionReason:
        transaction.value.verdict === "ForcedTxValid"
          ? null
          : transaction.value.verdict.ForcedTxInvalid.reason,
    });
    observe(material, subject, BigInt(index));
  }
  return { findings, failures };
};

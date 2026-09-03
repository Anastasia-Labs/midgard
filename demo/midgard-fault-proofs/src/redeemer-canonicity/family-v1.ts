import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormed,
  buildMidgardRedeemerItemProofTrace,
  decodeMidgardFieldPreimage,
  decodeMidgardRedeemerWitnessItem,
  midgardFieldCommitment,
  MidgardRedeemerItemProofModes,
  type MidgardRedeemerItemProofTrace,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const REDEEMER_CANONICITY_CATEGORY = "redeemerCanonicity" as const;
export const REDEEMER_CANONICITY_CATEGORY_ID = "00000028" as const;
export const REDEEMER_CANONICITY_FIELD_INDEX = 8 as const;
export const REDEEMER_CANONICITY_VIOLATION_ID = "redeemer-malformed" as const;

const fail = (message: string): never => {
  throw new Error(`${REDEEMER_CANONICITY_CATEGORY}: ${message}`);
};

const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

export type RedeemerCanonicityFinding = Readonly<{
  subject: VerdictSubject;
  redeemerIndex: number;
}>;

export const classifyRedeemerCanonicityFinding = (
  finding: RedeemerCanonicityFinding,
): RedeemerCanonicityFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  exactIndex(finding.redeemerIndex, "redeemer index");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    const reason = finding.subject.rejection_reason;
    if (
      typeof reason === "string" ||
      reason === null ||
      !("RedeemerMalformed" in reason) ||
      Number(reason.RedeemerMalformed.redeemer_index) !== finding.redeemerIndex
    )
      return fail("typed reason/redeemer coordinate changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity changed");
  }
  return Object.freeze({ ...finding });
};

const canonicalData = (bytes: Uint8Array): boolean => {
  try {
    assertMidgardPlutusDataWellFormed(bytes);
    return Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        Buffer.from(bytes).toString("hex"),
      ),
      "hex",
    ).equals(Buffer.from(bytes));
  } catch {
    return false;
  }
};

export type RedeemerCanonicityEvidence = RedeemerCanonicityFinding &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    itemHex: string;
    itemCount: number;
    canonical: boolean;
    trace: MidgardRedeemerItemProofTrace | null;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

/** Evidence is derived from the exact retained field-8 preimage and commitment. */
export const prepareRedeemerCanonicityEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: RedeemerCanonicityFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): RedeemerCanonicityEvidence => {
  const finding = classifyRedeemerCanonicityFinding(rawFinding);
  const actual = midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field 8 changed commitment");
  const items = decodeMidgardFieldPreimage(fieldPreimage);
  const item = items[finding.redeemerIndex];
  if (item === undefined) return fail("redeemer coordinate is outside field 8");
  let canonical = false;
  try {
    const decoded = decodeMidgardRedeemerWitnessItem(item);
    canonical =
      ["Spend", "Mint", "Reward", "Receive"].includes(decoded.purpose) &&
      canonicalData(decoded.redeemerCbor);
  } catch {
    canonical = false;
  }
  let trace: MidgardRedeemerItemProofTrace | null = null;
  if (canonical) {
    trace = buildMidgardRedeemerItemProofTrace({
      itemIndex: finding.redeemerIndex,
      itemCount: items.length,
      itemBytes: item,
      mode: MidgardRedeemerItemProofModes.Data,
    });
  }
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    itemHex: item.toString("hex"),
    itemCount: items.length,
    canonical,
    trace,
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
  });
};

export const redeemerCanonicityEvidenceCloses = (
  evidence: RedeemerCanonicityEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, !evidence.canonical);

export const selectCanonicalRedeemerCanonicityEvidence = (
  values: readonly RedeemerCanonicityEvidence[],
): RedeemerCanonicityEvidence => {
  if (values.length === 0) return fail("no authenticated detection");
  return [...values].sort(
    (left, right) =>
      left.redeemerIndex - right.redeemerIndex ||
      left.subject.transaction_id.localeCompare(right.subject.transaction_id),
  )[0]!;
};

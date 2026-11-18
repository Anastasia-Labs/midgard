import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

import { claimedItem } from "../evidence/claimed-coordinate.js";
import { planRedeemerData, type RedeemerDataPlan } from "./data-plan.js";

export const REDEEMER_CANONICITY_CATEGORY = "redeemerCanonicity" as const;
export const REDEEMER_CANONICITY_FIELD_INDEX = 8 as const;

const fail = (message: string): never => {
  throw new Error(`${REDEEMER_CANONICITY_CATEGORY}: ${message}`);
};

const exactIndex = (value: bigint, label: string): bigint => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER))
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

export type RedeemerCanonicityFinding = Readonly<{
  subject: VerdictSubject;
  /** Any integer the rejection codec admits; a rejection may name one field 8 lacks. */
  redeemerIndex: bigint;
}>;

/**
 * The coordinate a `PlutusExecutionFailed` rejection binds
 * (`rule.execution_failed_redeemer_index`): it names no redeemer, so only a
 * transaction whose field 8 has no items answers it.
 */
export const REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX = -1n;

/** Whether `subject` rejects the transaction for a Plutus execution failure. */
export const claimsPlutusExecutionFailure = (subject: VerdictSubject) => {
  const reason = subject.rejection_reason;
  return (
    subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION &&
    typeof reason !== "string" &&
    reason !== null &&
    "PlutusExecutionFailed" in reason
  );
};

export const classifyRedeemerCanonicityFinding = (
  finding: RedeemerCanonicityFinding,
): RedeemerCanonicityFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  if (typeof finding.redeemerIndex !== "bigint")
    return fail("redeemer index must be an integer");
  if (claimsPlutusExecutionFailure(finding.subject)) {
    if (finding.redeemerIndex !== REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX)
      return fail("a Plutus execution failure binds no redeemer");
    return Object.freeze({ ...finding });
  }
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    // A rejection may name any redeemer coordinate, including one field 8
    // lacks; an acceptance names a redeemer the field holds.
    const reason = finding.subject.rejection_reason;
    if (
      typeof reason === "string" ||
      reason === null ||
      !("RedeemerMalformed" in reason) ||
      reason.RedeemerMalformed.redeemer_index !== finding.redeemerIndex
    )
      return fail("typed reason/redeemer coordinate changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity changed");
  } else exactIndex(finding.redeemerIndex, "redeemer index");
  return Object.freeze({ ...finding });
};

export type RedeemerCanonicityEvidence = RedeemerCanonicityFinding &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    itemHex: string;
    itemCount: number;
    canonical: boolean;
    /** The data steps' read; `null` when step 02 decides the item alone. */
    dataPlan: RedeemerDataPlan | null;
    carriage: "Inline" | "RawUtxo" | "Certified";
    /**
     * The rejection names a redeemer field 8 does not have (a negative
     * coordinate, or one past the field's last item), or a Plutus execution
     * failure of a transaction with no redeemers. Step 02 then reads the
     * field's count instead of an item and writes the canonical verdict
     * straight to the terminal.
     */
    absent: boolean;
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
  const executionFailure = claimsPlutusExecutionFailure(finding.subject);
  const item = executionFailure
    ? undefined
    : claimedItem(items, finding.redeemerIndex);
  if (item === undefined) {
    // Canonical validation reads no redeemer field 8 lacks, and a
    // transaction with no redeemers runs no Plutus script: only a rejection
    // citing either is wrong.
    if (finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION)
      return fail("redeemer coordinate is outside field 8");
    if (executionFailure && items.length > 0)
      return fail("field 8 carries redeemers a Plutus script runs with");
    return Object.freeze({
      ...finding,
      fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
      fieldCommitmentHex: actual,
      itemHex: "",
      itemCount: items.length,
      canonical: true,
      dataPlan: null,
      carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
      absent: true,
    });
  }
  const dataPlan = planRedeemerData(item);
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    itemHex: item.toString("hex"),
    itemCount: items.length,
    canonical: dataPlan?.canonical ?? false,
    dataPlan,
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
    absent: false,
  });
};

export const redeemerCanonicityEvidenceCloses = (
  evidence: RedeemerCanonicityEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, !evidence.canonical);

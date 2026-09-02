import { createHash } from "node:crypto";

import { computeScriptIntegrityHashForLanguages } from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_V1 =
  "scriptIntegrityHashMismatch" as const;
export const SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID_V1 =
  "00000033" as const;
export const SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID_V1 =
  "script-integrity-hash-mismatch" as const;

const fail = (message: string): never => {
  throw new Error(`scriptIntegrityHashMismatch: ${message}`);
};
const h32 = (value: string, label: string): string =>
  /^[0-9a-f]{64}$/u.test(value)
    ? value
    : fail(`${label} is not canonical hash32`);

export type ScriptIntegrityHashMismatchFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
}>;
export type ScriptIntegrityHashMismatchEvidenceV1 = Readonly<{
  finding: ScriptIntegrityHashMismatchFindingV1;
  scriptIntegrityHash: string;
  redeemerWitnessHash: string;
  selectedLanguageBitmap: 0 | 1 | 2 | 3;
  executionCount: bigint;
  expectedHash: string;
}>;

export const languagesForIntegrityBitmapV1 = (
  bitmap: 0 | 1 | 2 | 3,
): readonly ("PlutusV3" | "MidgardV1")[] =>
  Object.freeze([
    ...(bitmap % 2 === 1 ? (["PlutusV3"] as const) : []),
    ...(bitmap >= 2 ? (["MidgardV1"] as const) : []),
  ]);

export const prepareScriptIntegrityHashMismatchEvidenceV1 = ({
  finding,
  scriptIntegrityHash,
  redeemerWitnessHash,
  selectedLanguageBitmap,
  executionCount,
}: Omit<
  ScriptIntegrityHashMismatchEvidenceV1,
  "expectedHash"
>): ScriptIntegrityHashMismatchEvidenceV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    fail("verdict subject is not canonical");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "object" ||
      reason !== "ScriptIntegrityHashMismatch"
    )
      fail("typed rejection reason changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  if (![0, 1, 2, 3].includes(selectedLanguageBitmap))
    fail("language bitmap is outside V1 domain");
  if (executionCount < 0n) fail("execution count is negative");
  const expectedHash = computeScriptIntegrityHashForLanguages(
    Buffer.from(h32(redeemerWitnessHash, "redeemer witness hash"), "hex"),
    languagesForIntegrityBitmapV1(selectedLanguageBitmap),
  ).toString("hex");
  return Object.freeze({
    finding: Object.freeze(finding),
    scriptIntegrityHash: h32(scriptIntegrityHash, "script integrity hash"),
    redeemerWitnessHash,
    selectedLanguageBitmap,
    executionCount,
    expectedHash,
  });
};

export const scriptIntegrityHashMismatchFaultHoldsV1 = (
  evidence: ScriptIntegrityHashMismatchEvidenceV1,
): boolean => evidence.scriptIntegrityHash !== evidence.expectedHash;
export const scriptIntegrityHashMismatchEvidenceClosesV1 = (
  evidence: ScriptIntegrityHashMismatchEvidenceV1,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? scriptIntegrityHashMismatchFaultHoldsV1(evidence)
    : !scriptIntegrityHashMismatchFaultHoldsV1(evidence);
export const scriptIntegrityHashMismatchEvidenceIdentityV1 = (
  evidence: ScriptIntegrityHashMismatchEvidenceV1,
): string =>
  createHash("sha256")
    .update(SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID_V1)
    .update(evidence.finding.subject.transaction_id)
    .update(evidence.scriptIntegrityHash)
    .update(evidence.redeemerWitnessHash)
    .update(evidence.selectedLanguageBitmap.toString())
    .digest("hex");

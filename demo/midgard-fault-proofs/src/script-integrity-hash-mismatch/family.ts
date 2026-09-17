import { createHash } from "node:crypto";

import { computeScriptIntegrityHashForLanguages } from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY =
  "scriptIntegrityHashMismatch" as const;
export const SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID = "00000033" as const;
export const SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID =
  "script-integrity-hash-mismatch" as const;

const fail = (message: string): never => {
  throw new Error(`scriptIntegrityHashMismatch: ${message}`);
};
const h32 = (value: string, label: string): string =>
  /^[0-9a-f]{64}$/u.test(value)
    ? value
    : fail(`${label} is not canonical hash32`);

export type ScriptIntegrityHashMismatchFinding = Readonly<{
  subject: VerdictSubject;
}>;
export type ScriptIntegrityHashMismatchEvidence = Readonly<{
  finding: ScriptIntegrityHashMismatchFinding;
  scriptIntegrityHash: string;
  redeemerWitnessHash: string;
  selectedLanguageBitmap: 0 | 1 | 2 | 3;
  executionCount: bigint;
  expectedHash: string;
}>;

export const languagesForIntegrityBitmap = (
  bitmap: 0 | 1 | 2 | 3,
): readonly ("PlutusV3" | "MidgardV1")[] =>
  Object.freeze([
    ...(bitmap % 2 === 1 ? (["PlutusV3"] as const) : []),
    ...(bitmap >= 2 ? (["MidgardV1"] as const) : []),
  ]);

export const prepareScriptIntegrityHashMismatchEvidence = ({
  finding,
  scriptIntegrityHash,
  redeemerWitnessHash,
  selectedLanguageBitmap,
  executionCount,
}: Omit<
  ScriptIntegrityHashMismatchEvidence,
  "expectedHash"
>): ScriptIntegrityHashMismatchEvidence => {
  if (!verdictSubjectIsCanonical(finding.subject))
    fail("verdict subject is not canonical");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "object" ||
      reason !== "ScriptIntegrityHashMismatch"
    )
      fail("typed rejection reason changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  if (![0, 1, 2, 3].includes(selectedLanguageBitmap))
    fail("language bitmap is outside V1 domain");
  if (executionCount < 0n) fail("execution count is negative");
  const expectedHash = computeScriptIntegrityHashForLanguages(
    Buffer.from(h32(redeemerWitnessHash, "redeemer witness hash"), "hex"),
    languagesForIntegrityBitmap(selectedLanguageBitmap),
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

export const scriptIntegrityHashMismatchFaultHolds = (
  evidence: ScriptIntegrityHashMismatchEvidence,
): boolean => evidence.scriptIntegrityHash !== evidence.expectedHash;
export const scriptIntegrityHashMismatchEvidenceCloses = (
  evidence: ScriptIntegrityHashMismatchEvidence,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? scriptIntegrityHashMismatchFaultHolds(evidence)
    : !scriptIntegrityHashMismatchFaultHolds(evidence);
export const scriptIntegrityHashMismatchEvidenceIdentity = (
  evidence: ScriptIntegrityHashMismatchEvidence,
): string =>
  createHash("sha256")
    .update(SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID)
    .update(evidence.finding.subject.transaction_id)
    .update(evidence.scriptIntegrityHash)
    .update(evidence.redeemerWitnessHash)
    .update(evidence.selectedLanguageBitmap.toString())
    .digest("hex");

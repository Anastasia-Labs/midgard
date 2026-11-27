import { EMPTY_NULL_ROOT } from "@al-ft/midgard-core";
import { type RejectionReason } from "@al-ft/midgard-sdk/core";
import {
  bindExactVerdictSubjectReason,
  encodeVerdictSubject,
  terminalVerdictContradiction,
  type VerdictSubject,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import { cborDefiniteBytes } from "../cbor-serialise.js";
import {
  type ForcedLeafEvidence,
  forcedLeafVerdictSubject,
  requireForcedLeafAccepted,
  requireForcedLeafRejectedFor,
} from "../evidence/forced-leaf-evidence.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY =
  "scriptIntegrityHashMissing" as const;
export const SCRIPT_INTEGRITY_HASH_MISSING_REASON =
  "ScriptIntegrityHashMissing" as const;
export const SCRIPT_INTEGRITY_HASH_MISSING_ABSENT_HASH =
  EMPTY_NULL_ROOT.toString("hex");

const exactReason = (reason: RejectionReason): boolean =>
  reason === SCRIPT_INTEGRITY_HASH_MISSING_REASON;

const canonicalHex = (value: string, bytes: number, name: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)) {
    throw new Error(`${name} must be canonical ${bytes.toString()}-byte hex`);
  }
  return value;
};

export type ScriptIntegrityHashMissingFinding = {
  readonly category: typeof SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY;
  readonly headerHash: string;
  readonly transactionId: string;
  readonly direction: "wrongfulAcceptance" | "wrongfulRejection";
  readonly source: "accepted" | "forced";
  readonly rejectionReason: RejectionReason | null;
};

export const assertScriptIntegrityHashMissingFinding = (
  finding: ScriptIntegrityHashMissingFinding,
): void => {
  canonicalHex(finding.headerHash, 28, "headerHash");
  canonicalHex(finding.transactionId, 32, "transactionId");
  if (finding.category !== SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY) {
    throw new Error("finding belongs to another fault-proof category");
  }
  if (finding.direction === "wrongfulRejection") {
    if (finding.source !== "forced" || finding.rejectionReason === null) {
      throw new Error(
        "wrongful rejection requires an authenticated forced reason",
      );
    }
    if (!exactReason(finding.rejectionReason)) {
      throw new Error("forced leaf reason is not ScriptIntegrityHashMissing");
    }
  } else if (finding.rejectionReason !== null) {
    throw new Error("wrongful acceptance cannot carry a rejection reason");
  }
};

export type ScriptIntegrityHashMissingEvidence = {
  readonly finding: ScriptIntegrityHashMissingFinding;
  readonly subject: VerdictSubject;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly fieldPreimageLengthsCbor: string;
  readonly scriptWitnessesPreimageCbor: string;
  readonly redeemersPreimageCbor: string;
  readonly scriptIntegrityHash: string;
  readonly scriptLanguages: readonly (0 | 3 | 128)[];
  readonly redeemerCount: number;
  readonly forcedLeaf?: ForcedLeafEvidence;
};

export const scriptIntegrityHashMissingFaultHolds = ({
  scriptIntegrityHash,
  scriptLanguages,
  redeemerCount,
}: Pick<
  ScriptIntegrityHashMissingEvidence,
  "scriptIntegrityHash" | "scriptLanguages" | "redeemerCount"
>): boolean => {
  canonicalHex(scriptIntegrityHash, 32, "scriptIntegrityHash");
  if (!Number.isSafeInteger(redeemerCount) || redeemerCount < 0) {
    throw new Error("redeemerCount must be a non-negative safe integer");
  }
  const containsNonNative = scriptLanguages.some((language) => language !== 0);
  const requiresPlutus =
    containsNonNative ||
    redeemerCount > 0 ||
    scriptIntegrityHash !== SCRIPT_INTEGRITY_HASH_MISSING_ABSENT_HASH;
  return (
    requiresPlutus &&
    scriptIntegrityHash === SCRIPT_INTEGRITY_HASH_MISSING_ABSENT_HASH
  );
};

/** Retained payloads remain evidence; this preparation never trusts a verdict. */
export const prepareScriptIntegrityHashMissingEvidence = (
  evidence: ScriptIntegrityHashMissingEvidence,
): ScriptIntegrityHashMissingEvidence => {
  assertScriptIntegrityHashMissingFinding(evidence.finding);
  if (evidence.subject.transaction_id !== evidence.finding.transactionId) {
    throw new Error("authenticated subject transaction differs from finding");
  }
  const expectedSourceKind = evidence.finding.source === "forced" ? 1n : 0n;
  const expectedDirection =
    evidence.finding.direction === "wrongfulRejection" ? 1n : 0n;
  if (
    evidence.subject.source_kind !== expectedSourceKind ||
    evidence.subject.direction !== expectedDirection
  ) {
    throw new Error(
      "authenticated subject source/direction differs from finding",
    );
  }
  for (const [name, value] of Object.entries({
    nativeTxCompactCbor: evidence.nativeTxCompactCbor,
    witnessSetCompactCbor: evidence.witnessSetCompactCbor,
    fieldPreimageLengthsCbor: evidence.fieldPreimageLengthsCbor,
    scriptWitnessesPreimageCbor: evidence.scriptWitnessesPreimageCbor,
    redeemersPreimageCbor: evidence.redeemersPreimageCbor,
  })) {
    if (!/^(?:[0-9a-f]{2})+$/u.test(value)) {
      throw new Error(`${name} must be non-empty canonical CBOR hex`);
    }
  }
  canonicalHex(evidence.scriptIntegrityHash, 32, "scriptIntegrityHash");
  if (evidence.finding.source === "forced") {
    if (evidence.forcedLeaf === undefined) {
      throw new Error("forced source requires retained forced-leaf evidence");
    }
    if (evidence.finding.direction === "wrongfulRejection") {
      requireForcedLeafRejectedFor(
        evidence.forcedLeaf,
        SCRIPT_INTEGRITY_HASH_MISSING_REASON,
      );
      bindExactVerdictSubjectReason(
        evidence.subject,
        SCRIPT_INTEGRITY_HASH_MISSING_REASON,
      );
    } else {
      requireForcedLeafAccepted(evidence.forcedLeaf);
    }
    const bound = forcedLeafVerdictSubject(evidence.forcedLeaf);
    if (
      bound.transaction_id !== evidence.subject.transaction_id ||
      bound.source_key !== evidence.subject.source_key ||
      bound.direction !== evidence.subject.direction ||
      bound.source_kind !== evidence.subject.source_kind
    ) {
      throw new Error("authenticated forced leaf differs from verdict subject");
    }
  }
  if (
    !terminalVerdictContradiction(
      evidence.subject,
      scriptIntegrityHashMissingFaultHolds(evidence),
    )
  ) {
    throw new Error(
      "authenticated semantics do not contradict the operator verdict",
    );
  }
  return Object.freeze({ ...evidence });
};

/** Exact twin of Aiken `encode_decision_state_v1`. */
export const encodeScriptIntegrityHashMissingDecisionState = (
  evidence: ScriptIntegrityHashMissingEvidence,
): Buffer => {
  const subject = encodeVerdictSubject(evidence.subject);
  const boolSchema = Data.Boolean();
  return Buffer.concat([
    Buffer.from([0x84]),
    cborDefiniteBytes(subject),
    cborDefiniteBytes(
      Buffer.from(
        canonicalHex(evidence.scriptIntegrityHash, 32, "scriptIntegrityHash"),
        "hex",
      ),
    ),
    Buffer.from(
      Data.to(
        evidence.scriptLanguages.some((language) => language !== 0) as never,
        boolSchema as never,
      ),
      "hex",
    ),
    Buffer.from(
      Data.to((evidence.redeemerCount > 0) as never, boolSchema as never),
      "hex",
    ),
  ]);
};

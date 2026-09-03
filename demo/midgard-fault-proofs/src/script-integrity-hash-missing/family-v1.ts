import { createHash } from "node:crypto";

import {
  bindExactVerdictSubjectReason,
  encodeVerdictSubject,
  type RejectionReason,
  terminalVerdictContradiction,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type ForcedLeafEvidence,
  forcedLeafVerdictSubject,
  requireForcedLeafAccepted,
  requireForcedLeafRejectedFor,
} from "../evidence/forced-leaf-evidence-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY =
  "scriptIntegrityHashMissing" as const;
export const SCRIPT_INTEGRITY_HASH_MISSING_PROPOSED_ID = "00000023";
export const SCRIPT_INTEGRITY_HASH_MISSING_REASON =
  "ScriptIntegrityHashMissing" as const;
export const SCRIPT_INTEGRITY_HASH_MISSING_ZERO_HASH = "00".repeat(32);

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

export const classifyScriptIntegrityHashMissingFinding = (
  finding: ScriptIntegrityHashMissingFinding,
): typeof SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY => {
  assertScriptIntegrityHashMissingFinding(finding);
  return SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY;
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
    scriptIntegrityHash !== SCRIPT_INTEGRITY_HASH_MISSING_ZERO_HASH;
  return (
    requiresPlutus &&
    scriptIntegrityHash === SCRIPT_INTEGRITY_HASH_MISSING_ZERO_HASH
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

const cborBytes = (bytes: Buffer): Buffer => {
  if (bytes.length < 24)
    return Buffer.concat([Buffer.from([0x40 + bytes.length]), bytes]);
  if (bytes.length <= 0xff)
    return Buffer.concat([Buffer.from([0x58, bytes.length]), bytes]);
  const header = Buffer.alloc(3);
  header[0] = 0x59;
  header.writeUInt16BE(bytes.length, 1);
  return Buffer.concat([header, bytes]);
};

/** Exact twin of Aiken `encode_decision_state_v1`. */
export const encodeScriptIntegrityHashMissingDecisionState = (
  evidence: ScriptIntegrityHashMissingEvidence,
): Buffer => {
  const subject = encodeVerdictSubject(evidence.subject);
  const boolSchema = Data.Boolean();
  return Buffer.concat([
    Buffer.from([0x84]),
    cborBytes(subject),
    cborBytes(
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

/** Durable identity of every authenticated byte and dispatch coordinate. */
export const scriptIntegrityHashMissingEvidenceDigest = (
  evidence: ScriptIntegrityHashMissingEvidence,
): string => {
  const prepared = prepareScriptIntegrityHashMissingEvidence(evidence);
  return createHash("sha256")
    .update(
      JSON.stringify({
        domain: "MidgardScriptIntegrityHashMissingEvidenceV1",
        finding: prepared.finding,
        subjectCbor: encodeVerdictSubject(prepared.subject).toString("hex"),
        nativeTxCompactCbor: prepared.nativeTxCompactCbor,
        witnessSetCompactCbor: prepared.witnessSetCompactCbor,
        fieldPreimageLengthsCbor: prepared.fieldPreimageLengthsCbor,
        scriptWitnessesPreimageCbor: prepared.scriptWitnessesPreimageCbor,
        redeemersPreimageCbor: prepared.redeemersPreimageCbor,
        scriptIntegrityHash: prepared.scriptIntegrityHash,
        scriptLanguages: prepared.scriptLanguages,
        redeemerCount: prepared.redeemerCount,
        forcedLeafFingerprint: prepared.forcedLeaf?.eventKeyFingerprint ?? null,
      }),
    )
    .digest("hex");
};

export type ScriptIntegrityHashMissingCarriage =
  | { readonly kind: "direct" }
  | { readonly kind: "published"; readonly chunkOutRefs: readonly string[] }
  | { readonly kind: "rawFields"; readonly fieldOutRefs: readonly string[] }
  | {
      readonly kind: "certifiedFields";
      readonly certificateOutRefs: readonly string[];
      readonly chunkOutRefs: readonly string[];
    };

export const SCRIPT_INTEGRITY_HASH_MISSING_CERTIFIED_CHUNK_BYTES = 15_148;

export const selectScriptIntegrityHashMissingCarriage = ({
  membershipBytes,
  fieldBytes,
  directBudget = 8_192,
}: {
  readonly membershipBytes: number;
  readonly fieldBytes: number;
  readonly directBudget?: number;
}): "direct" | "published" | "rawFields" | "certifiedFields" => {
  if (membershipBytes < 0 || fieldBytes < 0)
    throw new Error("evidence sizes cannot be negative");
  if (membershipBytes + fieldBytes <= directBudget) return "direct";
  if (fieldBytes > SCRIPT_INTEGRITY_HASH_MISSING_CERTIFIED_CHUNK_BYTES)
    return "certifiedFields";
  return membershipBytes > fieldBytes ? "published" : "rawFields";
};

export type ScriptIntegrityHashMissingWorkflowStage =
  | "absent"
  | "init"
  | "step01"
  | "step02"
  | "step03"
  | "scriptGrammar"
  | "scriptScan"
  | "redeemerGrammar"
  | "step04"
  | "remove"
  | "complete"
  | "cancelled";

export type ScriptIntegrityHashMissingJournal = {
  readonly workflowId: string;
  readonly evidenceDigest: string;
  readonly stage: ScriptIntegrityHashMissingWorkflowStage;
  readonly submittedTxHash?: string;
};

export type ScriptIntegrityHashMissingWorkflowDeps = {
  readonly loadJournal: (
    workflowId: string,
  ) => Promise<ScriptIntegrityHashMissingJournal | null>;
  readonly appendJournal: (
    entry: ScriptIntegrityHashMissingJournal,
  ) => Promise<void>;
  readonly observeStage: (
    workflowId: string,
  ) => Promise<ScriptIntegrityHashMissingWorkflowStage>;
  readonly submit: (
    stage: Exclude<
      ScriptIntegrityHashMissingWorkflowStage,
      "absent" | "complete" | "cancelled"
    >,
    evidence: ScriptIntegrityHashMissingEvidence,
  ) => Promise<{ readonly txHash: string }>;
};

const nextStage = (
  stage: ScriptIntegrityHashMissingWorkflowStage,
):
  | "init"
  | "step01"
  | "step02"
  | "step03"
  | "scriptGrammar"
  | "scriptScan"
  | "redeemerGrammar"
  | "step04"
  | "remove"
  | null =>
  stage === "absent"
    ? "init"
    : stage === "init"
      ? "step01"
      : stage === "step01"
        ? "step02"
        : stage === "step02"
          ? "step03"
          : stage === "step03"
            ? "scriptGrammar"
            : stage === "scriptGrammar"
              ? "scriptScan"
              : stage === "scriptScan"
                ? "redeemerGrammar"
                : stage === "redeemerGrammar"
                  ? "step04"
                  : stage === "step04"
                    ? "remove"
                    : stage === "remove"
                      ? null
                      : null;

export const runScriptIntegrityHashMissingWorkflow = async ({
  workflowId,
  evidence: rawEvidence,
  deps,
}: {
  readonly workflowId: string;
  readonly evidence: ScriptIntegrityHashMissingEvidence;
  readonly deps: ScriptIntegrityHashMissingWorkflowDeps;
}): Promise<ScriptIntegrityHashMissingJournal> => {
  const evidence = prepareScriptIntegrityHashMissingEvidence(rawEvidence);
  const evidenceDigest = scriptIntegrityHashMissingEvidenceDigest(evidence);
  const prior = await deps.loadJournal(workflowId);
  if (prior !== null && prior.evidenceDigest !== evidenceDigest) {
    throw new Error("durable workflow evidence identity changed on restart");
  }
  let observed = await deps.observeStage(workflowId);
  while (observed !== "complete" && observed !== "cancelled") {
    const action = nextStage(observed);
    if (action === null) {
      observed = "complete";
      break;
    }
    const { txHash } = await deps.submit(action, evidence);
    if (!/^[0-9a-f]{64}$/u.test(txHash))
      throw new Error("submission returned a non-canonical tx hash");
    const entry = Object.freeze({
      workflowId,
      evidenceDigest,
      stage: action,
      submittedTxHash: txHash,
    });
    await deps.appendJournal(entry);
    const reconciled = await deps.observeStage(workflowId);
    if (reconciled === observed)
      throw new Error(
        `transaction ${txHash} did not advance authenticated chain state`,
      );
    observed = reconciled;
  }
  const terminal = Object.freeze({
    workflowId,
    evidenceDigest,
    stage: observed,
  });
  await deps.appendJournal(terminal);
  return terminal;
};

export const SCRIPT_INTEGRITY_HASH_MISSING_WIRING = Object.freeze({
  category: SCRIPT_INTEGRITY_HASH_MISSING_CATEGORY,
  proposedId: SCRIPT_INTEGRITY_HASH_MISSING_PROPOSED_ID,
  firstStepHashRole: "scriptIntegrityHashMissingStep01",
  orderedPhysicalScripts: [
    "fraud_proofs/script_integrity_hash_missing/step_01.main.spend",
    "fraud_proofs/script_integrity_hash_missing/step_02.main.spend",
    "fraud_proofs/script_integrity_hash_missing/step_03.main.spend",
    "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
    "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
    "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
    "fraud_proofs/script_integrity_hash_missing/step_04.main.spend",
  ],
  parameterOrder: {
    step01: [
      "step02ScriptHash",
      "computationThreadPolicyId",
      "hubOracleScriptHash",
    ],
    step02: ["step03ScriptHash", "computationThreadPolicyId"],
    step03: [
      "step04ScriptHash",
      "scriptGrammarScriptHash",
      "computationThreadPolicyId",
      "fieldPreimageCertificatePolicyId",
    ],
    scriptGrammar: [
      "scriptScanScriptHash",
      "computationThreadPolicyId",
      "fieldPreimageCertificatePolicyId",
    ],
    scriptScan: [
      "redeemerGrammarScriptHash",
      "computationThreadPolicyId",
      "fieldPreimageCertificatePolicyId",
    ],
    redeemerGrammar: [
      "step04ScriptHash",
      "computationThreadPolicyId",
      "fieldPreimageCertificatePolicyId",
    ],
    step04: [
      "fraudProofPolicyId",
      "fraudProofTokenAddress",
      "computationThreadPolicyId",
    ],
  },
  runnerFactory: "runScriptIntegrityHashMissingWorkflowV1",
  watcherClassifier: "classifyScriptIntegrityHashMissingFindingV1",
});

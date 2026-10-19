/** One durable per-event input artifact for both transaction source roots and
 * both verdict polarities. Block-initial and creator-transaction proofs are retired. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  admitInputExistenceArtifact,
  inputExistenceArtifact,
} from "../input-existence/artifact.js";
import {
  type PreparedInputExistence,
  prepareInputExistence,
} from "../input-existence/prepare.js";
import { ledgerKeyBytesHex } from "../step-support.js";
import type { CanonicalBlockClassification } from "./classification.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorLedger,
} from "./complete-replay.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import {
  exactJournalRecord,
  safeNaturalNumber,
} from "./native-index-artifact.js";

export const LEDGER_ABSENCE_ARTIFACT =
  "midgard-production-input-existence-artifact-v1";
export type LedgerAbsenceCategory = "nonExistentInput" | "noReferenceInput";
export type LedgerAbsenceArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof LEDGER_ABSENCE_ARTIFACT;
    category: LedgerAbsenceCategory;
    detectionId: string;
    position: number;
    evidence: JournalJsonObject;
  }>;

const identity = (
  category: LedgerAbsenceCategory,
  position: number,
  prepared: PreparedInputExistence,
): string => {
  const violation =
    category === "nonExistentInput"
      ? "non-existent-input"
      : "no-reference-input";
  if (
    prepared.source.subject.direction ===
    SDK.PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
  ) {
    return `${violation}-wrongful-rejection:${position}`;
  }
  if (prepared.selectedInput === null)
    throw new Error("input-existence acceptance selected no input");
  return `${violation}:${position}:${prepared.inputIndex}:${prepared.source.subject.transaction_id}:${ledgerKeyBytesHex(prepared.selectedInput)}`;
};

export const admitLedgerAbsenceArtifact = (value: unknown, owner: string) => {
  const raw = exactJournalRecord(
    value,
    ["schemaVersion", "category", "detectionId", "position", "evidence"],
    "production input-existence artifact",
  );
  if (
    raw.schemaVersion !== LEDGER_ABSENCE_ARTIFACT ||
    (raw.category !== "nonExistentInput" && raw.category !== "noReferenceInput")
  ) {
    throw new Error("input-existence artifact workflow identity changed");
  }
  const position = safeNaturalNumber(raw.position, "input-existence position");
  const prepared = admitInputExistenceArtifact(raw.evidence, owner);
  if (
    prepared.field !== (raw.category === "nonExistentInput" ? 0 : 1) ||
    raw.detectionId !== identity(raw.category, position, prepared)
  ) {
    throw new Error("input-existence artifact detection identity changed");
  }
  const artifact = normalizeJournalJson({
    schemaVersion: LEDGER_ABSENCE_ARTIFACT,
    category: raw.category,
    detectionId: raw.detectionId,
    position,
    evidence: inputExistenceArtifact(prepared),
  }) as LedgerAbsenceArtifact;
  return { artifact, prepared };
};

export const prepareLedgerAbsenceArtifact = async ({
  category,
  evidence,
  replayContext,
  classification,
  owner,
}: {
  readonly category: LedgerAbsenceCategory;
  readonly evidence: CanonicalBlockEvidence;
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
  readonly owner: string;
}): Promise<LedgerAbsenceArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.category !== category ||
    classification.selected.subjectEventKeyCbors.length !== 1
  ) {
    throw new Error("input-existence classification/evidence changed");
  }
  const selected = classification.selected;
  if (
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "input-existence classification position exceeds safe range",
    );
  }
  const field = category === "nonExistentInput" ? 0 : 1;
  const eventKey = Data.from(selected.subjectEventKeyCbors[0]!, SDK.EventKey);
  const fields = selected.detectionId.split(":");
  const violation = field === 0 ? "non-existent-input" : "no-reference-input";
  let index: bigint | undefined;
  if (
    selected.violationId === violation &&
    fields.length === 5 &&
    /^(?:0|[1-9][0-9]*)$/u.test(fields[2]!)
  ) {
    index = BigInt(fields[2]!);
  } else if (selected.violationId !== `${violation}-wrongful-rejection`) {
    throw new Error("input-existence classification names another rule");
  }
  const prepared = await prepareInputExistence({
    block: evidence,
    eventKey,
    field,
    inputIndex: index,
    predecessor: completeCanonicalReplayPredecessorLedger({
      evidence,
      context: replayContext,
    }),
    owner,
  });
  const wrongfulAcceptance =
    prepared.source.subject.direction ===
    SDK.PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE;
  if (
    wrongfulAcceptance
      ? !(
          typeof prepared.ledgerEvidence === "object" &&
          "Absent" in prepared.ledgerEvidence
        )
      : typeof prepared.ledgerEvidence === "object" &&
        "Absent" in prepared.ledgerEvidence
  ) {
    throw new Error(
      "input-existence classification is not a fault at the event pre-state",
    );
  }
  return admitLedgerAbsenceArtifact(
    {
      schemaVersion: LEDGER_ABSENCE_ARTIFACT,
      category,
      detectionId: selected.detectionId,
      position: Number(selected.position),
      evidence: inputExistenceArtifact(prepared),
    },
    owner,
  ).artifact;
};

/** Exactly the physical step's one proof; no creator/root compatibility path. */
export const inputExistenceProofForStage = (
  prepared: PreparedInputExistence,
  stage: string,
): string | null => {
  if (stage === "step_01")
    return prepared.source.kind === "accepted"
      ? prepared.source.inclusion.txMembershipProofCbor
      : null;
  if (stage === "step_02")
    return Data.to(prepared.eventMembership.proof, SDK.Proof);
  if (stage === "step_03")
    return Data.to(prepared.transitionMembership.proof, SDK.Proof);
  if (
    stage === "step_04" &&
    typeof prepared.ledgerEvidence === "object" &&
    "Absent" in prepared.ledgerEvidence
  ) {
    return Data.to(prepared.ledgerEvidence.Absent.proof, SDK.Proof);
  }
  return null;
};

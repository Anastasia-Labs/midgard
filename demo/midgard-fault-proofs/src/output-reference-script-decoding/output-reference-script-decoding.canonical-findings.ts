/**
 * The central replay route: every outputReferenceScriptDecoding complete-replay
 * finding, with one exact stable violation id per rejection arm.
 */
import {
  encodeProofThreadForcedSourceKey,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { verdictDetectionSubject } from "../workflow/detection-subject.js";
import {
  detectOutputReferenceScriptDecodingCompleteReplay,
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY,
  type OutputReferenceScriptDecodingEvidence,
  outputReferenceScriptDecodingViolationId,
} from "./output-reference-script-decoding.js";

const fail = (message: string): never => {
  throw new Error(`${OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY}: ${message}`);
};

export type OutputReferenceScriptDecodingReplayFinding =
  CanonicalViolationDetection &
    Readonly<{ evidence: OutputReferenceScriptDecodingEvidence }>;

/** Every complete-replay finding, with one exact stable violation id per rejection arm. */
export const outputReferenceScriptDecodingCanonicalFindings = (
  block: CanonicalBlockEvidence,
): readonly OutputReferenceScriptDecodingReplayFinding[] =>
  detectOutputReferenceScriptDecodingCompleteReplay(block).map((evidence) => {
    const forced =
      evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION;
    const violationId = outputReferenceScriptDecodingViolationId(
      forced ? evidence.accusedClass : evidence.resultClass,
    );
    const position = forced
      ? block.reconstruction.forcedTransactions.findIndex(
          ({ key, value }) =>
            value.tx_id === evidence.subject.transaction_id &&
            encodeProofThreadForcedSourceKey(key).toString("hex") ===
              evidence.subject.source_key,
        )
      : block.transactions.findIndex(
          ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
        );
    if (position < 0)
      return fail("detected subject is absent from authenticated block");
    return Object.freeze({
      ...verdictDetectionSubject(evidence.subject),
      detectionId: `${violationId}:${forced ? "forced" : "accepted"}:${position.toString()}:${evidence.subject.transaction_id}:${evidence.outputIndex.toString()}:${evidence.resultClass.toString()}`,
      headerHash: block.headerHash,
      violationId,
      position: BigInt(position),
      diagnostic: forced
        ? `forced transaction ${evidence.subject.transaction_id} was rejected for a decodable output reference script at ${evidence.outputIndex.toString()}`
        : `accepted transaction ${evidence.subject.transaction_id} has an undecodable output reference script at ${evidence.outputIndex.toString()}`,
      evidence,
    });
  });

/** Central replay route with one exact stable violation id per rejection arm. */
export const detectOutputReferenceScriptDecodingCanonicalViolations = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  outputReferenceScriptDecodingCanonicalFindings(block).map(
    ({ evidence: _evidence, ...detection }) => Object.freeze(detection),
  );

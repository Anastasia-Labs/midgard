/** The claim material for the event a validation-trace detection names, from
 * the same DA replay the detector ran: the detection's single subject event,
 * its pre-state ledger over the admitted predecessor (the one the detector
 * walked to, when it kept it), and its honest trace. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerBeforeTransactionEvent } from "../input-existence/event-ledger.js";
import type {
  CanonicalBlockClassification,
  CanonicalViolationDetection,
} from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  VALIDATION_TRACE_VIOLATION_ID,
} from "../workflow/complete-replay.js";
import { requireReplayPredecessorLedger } from "../workflow/complete-replay.replay-context-identity.js";
import { sameJson } from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  encodeValidationTraceDisputeArtifact,
  parseValidationTraceDisputeArtifact,
} from "./artifact.js";
import {
  buildValidationTraceClaimMaterial,
  type ValidationTraceClaimMaterial,
} from "./claim-material.js";
import { detectedLedger } from "./detected-ledgers.js";
import { type CommittedL2Event, eventRunsPlutus } from "./replay.js";

export type DetectedValidationTraceClaim = {
  readonly event: CommittedL2Event;
  readonly material: ValidationTraceClaimMaterial;
};

export const detectedValidationTraceClaim = async ({
  evidence,
  context,
  detection,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
  readonly detection: CanonicalViolationDetection;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<DetectedValidationTraceClaim> => {
  if (
    detection.violationId !== VALIDATION_TRACE_VIOLATION_ID ||
    detection.headerHash !== evidence.headerHash ||
    detection.frontier !== "accepted" ||
    detection.subjectEventKeyCbors.length !== 1
  )
    throw new Error(
      "validation claim: the detection is not one validation-trace event of this block",
    );
  const [subject] = detection.subjectEventKeyCbors;
  const event = evidence.reconstruction.sourceEvents.find(
    (candidate) => candidate.fingerprint === subject,
  );
  if (event === undefined || !eventRunsPlutus(event))
    throw new Error(
      "validation claim: the detected event is not an accepted normal transaction that runs Plutus",
    );
  const opening = evidence.reconstruction.eventToStepByFingerprint.get(
    event.fingerprint,
  );
  if (opening === undefined || opening.value.phase !== event.phase)
    throw new Error(
      "validation claim: the detected event has no committed event-to-step opening",
    );
  const predecessor = requireReplayPredecessorLedger({ evidence, context });
  const ledger =
    detectedLedger({
      evidence,
      predecessor,
      fingerprint: event.fingerprint,
    }) ??
    (await ledgerBeforeTransactionEvent({
      block: evidence,
      eventKey: event.eventKey,
      stepIndex: opening.value.step_index,
      ...(predecessor === undefined ? {} : { predecessor }),
    }));
  return {
    event,
    material: await buildValidationTraceClaimMaterial({
      evidence,
      event,
      ledger,
      l1SlotConfig,
    }),
  };
};

type FaultClassification = Extract<
  CanonicalBlockClassification,
  { readonly decision: "fault_detected" }
>;

/** The workflow artifact for the classification's selected detection. */
export const prepareValidationTraceArtifact = async ({
  evidence,
  context,
  classification,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
  readonly classification: FaultClassification;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<JournalJsonObject> => {
  if (classification.category !== "validationTraceDispute")
    throw new Error("validation-trace adapter received another family");
  const { material } = await detectedValidationTraceClaim({
    evidence,
    context,
    detection: classification.selected,
    l1SlotConfig,
  });
  return encodeValidationTraceDisputeArtifact({
    headerHash: evidence.headerHash,
    material,
  });
};

/**
 * Checks a durable artifact against this replay: it must be exactly the
 * artifact of one validation-trace detection of the block. Not necessarily
 * the selected one: an earlier event held when the workflow started may
 * since have been proven to fail too, and the started claim stays valid.
 */
export const validateValidationTraceArtifact = async ({
  evidence,
  context,
  classification,
  artifact,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
  readonly classification: FaultClassification;
  readonly artifact: JournalJsonObject;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<void> => {
  const { eventKeyCbor } = parseValidationTraceDisputeArtifact(artifact);
  const named = classification.detections.find(
    ({ violationId, subjectEventKeyCbors: [subject] }) =>
      violationId === VALIDATION_TRACE_VIOLATION_ID &&
      evidence.reconstruction.sourceEvents.some(
        (event) =>
          event.fingerprint === subject &&
          Data.to(event.eventKey, SDK.EventKey) === eventKeyCbor,
      ),
  );
  const rebuilt =
    named === undefined
      ? undefined
      : await prepareValidationTraceArtifact({
          evidence,
          context,
          classification: { ...classification, selected: named },
          l1SlotConfig,
        });
  if (rebuilt === undefined || !sameJson(rebuilt, artifact))
    throw new Error(
      "validation-trace artifact differs from this evidence's replay",
    );
};

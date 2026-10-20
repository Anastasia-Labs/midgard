/** The claim material for the event a validation-trace detection names, from
 * the same DA replay the detector ran: the detection's single subject event,
 * its pre-state ledger over the admitted predecessor, and its honest trace. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerBeforeTransactionEvent } from "../input-existence/event-ledger.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  VALIDATION_TRACE_VIOLATION_ID,
} from "../workflow/complete-replay.js";
import { requireReplayPredecessorLedger } from "../workflow/complete-replay.replay-context-identity.js";
import {
  buildValidationTraceClaimMaterial,
  type ValidationTraceClaimMaterial,
} from "./claim-material.js";
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
  const ledger = await ledgerBeforeTransactionEvent({
    block: evidence,
    eventKey: event.eventKey,
    stepIndex: opening.value.step_index,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
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

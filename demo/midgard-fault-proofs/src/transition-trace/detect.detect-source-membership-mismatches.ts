import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";

import {
  detection,
  maybeUnsupported,
  orderedTrace,
  sourceForStep,
  type TransitionTraceDetection,
} from "./detect.detect-count-faults.js";
import { transitionTraceError } from "./errors.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  buildDuplicateTraceEventFault,
  buildEventToStepMismatchFault,
  buildInvalidForcedTransactionNoOpWitness,
  buildInvalidWithdrawalNoOpWitness,
  buildMappedEventMissingFromSourceFault,
  buildSourceEventMissingTraceFault,
  buildSourcePhaseMismatchFault,
  buildTraceLinkFault,
} from "./witnesses.js";

export const detectTraceLinkFaults = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  const steps = orderedTrace(reconstruction);
  for (let index = 0; index < steps.length - 1; index += 1) {
    const lower = steps[index]!;
    const upper = steps[index + 1]!;
    if (lower.post_utxos_root !== upper.pre_utxos_root) {
      detections.push(
        detection({
          reconstruction,
          kind: "traceLink",
          invariant: "adjacent_trace_roots",
          diagnostic: `Trace step ${lower.step_index.toString()} post_utxos_root ${lower.post_utxos_root} does not equal step ${upper.step_index.toString()} pre_utxos_root ${upper.pre_utxos_root}.`,
          fault: await buildTraceLinkFault({
            reconstruction,
            lowerStepIndex: lower.step_index,
          }),
        }),
      );
    }
  }
  return detections;
};

export const detectDuplicateTraceEvents = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const seen = new Map<string, SDK.TransitionStep>();
  const detections: TransitionTraceDetection[] = [];
  for (const step of orderedTrace(reconstruction)) {
    const fingerprint = eventKeyFingerprint(step.event_key);
    const prior = seen.get(fingerprint);
    if (prior !== undefined && prior.step_index !== step.step_index) {
      detections.push(
        detection({
          reconstruction,
          kind: "duplicateTraceEvent",
          invariant: "trace_event_key_unique",
          diagnostic: `Trace steps ${prior.step_index.toString()} and ${step.step_index.toString()} both commit event key ${fingerprint}.`,
          fault: await buildDuplicateTraceEventFault({
            reconstruction,
            leftStepIndex: prior.step_index,
            rightStepIndex: step.step_index,
          }),
        }),
      );
    } else {
      seen.set(fingerprint, step);
    }
  }
  return detections;
};

export const detectEventToStepMismatches = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const step of orderedTrace(reconstruction)) {
    const mapped = reconstruction.eventToStepByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (
      mapped === undefined ||
      mapped.value.step_index !== step.step_index ||
      mapped.value.phase !== step.phase
    ) {
      const mappedText =
        mapped === undefined
          ? "absent"
          : `step_index=${mapped.value.step_index.toString()},phase=${mapped.value.phase}`;
      detections.push(
        detection({
          reconstruction,
          kind: "eventToStepMismatch",
          invariant: "event_to_step_matches_trace",
          diagnostic: `Trace step ${step.step_index.toString()} maps event key ${eventKeyFingerprint(
            step.event_key,
          )}, but event_to_step is ${mappedText}.`,
          fault: await buildEventToStepMismatchFault({
            reconstruction,
            stepIndex: step.step_index,
          }),
        }),
      );
    }
  }
  return detections;
};

export const detectSourceMembershipMismatches = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const mapped of reconstruction.eventToStep) {
    const fingerprint = eventKeyFingerprint(mapped.key);
    const source = reconstruction.sourceEventsByFingerprint.get(fingerprint);
    const trace = reconstruction.traceByStepIndex.get(mapped.value.step_index);
    if (source === undefined && trace !== undefined) {
      detections.push(
        await maybeUnsupported(
          async () =>
            detection({
              reconstruction,
              kind: "sourceMembershipMismatch",
              invariant: "mapped_event_has_source_member",
              diagnostic: `event_to_step maps event key ${fingerprint}, but the source root for phase ${mapped.value.phase} has no matching member.`,
              fault: await buildMappedEventMissingFromSourceFault({
                reconstruction,
                stepIndex: mapped.value.step_index,
                eventKey: mapped.key,
              }),
            }),
          {
            kind: "sourceMembershipMismatch",
            invariant: "mapped_event_has_source_member",
            diagnostic: `event_to_step maps event key ${fingerprint}, but the source root for phase ${mapped.value.phase} has no matching member.`,
            reason: "",
          },
        ),
      );
    }
  }
  for (const source of reconstruction.sourceEvents) {
    if (!reconstruction.eventToStepByFingerprint.has(source.fingerprint)) {
      detections.push(
        await maybeUnsupported(
          async () =>
            detection({
              reconstruction,
              kind: "sourceMembershipMismatch",
              invariant: "source_event_has_event_to_step_member",
              diagnostic: `Source event ${source.fingerprint} is committed in ${source.phase}, but event_to_step has no matching member.`,
              fault: await buildSourceEventMissingTraceFault({
                reconstruction,
                eventKey: source.eventKey,
              }),
            }),
          {
            kind: "sourceMembershipMismatch",
            invariant: "source_event_has_event_to_step_member",
            diagnostic: `Source event ${source.fingerprint} is committed in ${source.phase}, but event_to_step has no matching member.`,
            reason: "",
          },
        ),
      );
    }
  }
  for (const step of orderedTrace(reconstruction)) {
    const source = sourceForStep(reconstruction, step);
    if (source !== undefined && source.phase !== step.phase) {
      detections.push(
        await maybeUnsupported(
          async () =>
            detection({
              reconstruction,
              kind: "sourceMembershipMismatch",
              invariant: "source_phase_matches_trace_phase",
              diagnostic: `Trace step ${step.step_index.toString()} phase ${step.phase} does not match source phase ${source.phase}.`,
              fault: await buildSourcePhaseMismatchFault({
                reconstruction,
                stepIndex: step.step_index,
              }),
            }),
          {
            kind: "sourceMembershipMismatch",
            invariant: "source_phase_matches_trace_phase",
            diagnostic: `Trace step ${step.step_index.toString()} phase ${step.phase} does not match source phase ${source.phase}.`,
            reason: "",
          },
        ),
      );
    }
  }
  return detections;
};

export const detectInvalidNoOpTransitions = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const step of orderedTrace(reconstruction)) {
    const source = sourceForStep(reconstruction, step);
    if (source === undefined) {
      continue;
    }
    if (
      source.phase === "Withdrawal" &&
      source.entry.value.validity !== "WithdrawalIsValid" &&
      step.pre_utxos_root !== step.post_utxos_root
    ) {
      const witness = await buildInvalidWithdrawalNoOpWitness({
        reconstruction,
        stepIndex: step.step_index,
      });
      detections.push(
        detection({
          reconstruction,
          kind: "invalidOneStepTransition",
          invariant: "invalid_withdrawal_is_no_op",
          diagnostic: `Invalid withdrawal trace step ${step.step_index.toString()} changes UTxO root from ${step.pre_utxos_root} to ${step.post_utxos_root}.`,
          fault: SDK.invalidOneStepTransitionFault(witness),
        }),
      );
    }
    if (
      source.phase === "ForcedTransaction" &&
      source.entry.value.verdict !== "ForcedTxValid" &&
      step.pre_utxos_root !== step.post_utxos_root
    ) {
      const witness = await buildInvalidForcedTransactionNoOpWitness({
        reconstruction,
        stepIndex: step.step_index,
      });
      detections.push(
        detection({
          reconstruction,
          kind: "invalidOneStepTransition",
          invariant: "invalid_forced_transaction_is_no_op",
          diagnostic: `Invalid forced transaction trace step ${step.step_index.toString()} changes UTxO root from ${step.pre_utxos_root} to ${step.post_utxos_root}.`,
          fault: SDK.invalidOneStepTransitionFault(witness),
        }),
      );
    }
  }
  return detections;
};

export const acceptedTerminalPostRoot = (
  terminalAcceptanceWitnessCbor: string,
): string => {
  const bytes = Buffer.from(terminalAcceptanceWitnessCbor, "hex");
  const array = readCborArrayHeader(bytes, 0, "terminal acceptance witness");
  if (array.length !== 4) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness must contain exactly four fields.",
    );
  }
  const version = readCborInteger(bytes, array.nextOffset, "terminal version");
  if (version.value !== 1n) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness has an unsupported version.",
    );
  }
  const tag = readCborBytes(bytes, version.nextOffset, "terminal tag");
  if (tag.value.length !== 0) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness tag must be empty.",
    );
  }
  const root = readCborBytes(bytes, tag.nextOffset, "terminal ledger root");
  if (root.value.length !== 32) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness ledger root must contain 32 bytes.",
    );
  }
  const frontier = readCborBytes(
    bytes,
    root.nextOffset,
    "terminal delta frontier",
  );
  if (frontier.nextOffset !== bytes.length) {
    throw transitionTraceError(
      "malformedPayload",
      "Terminal acceptance witness contains trailing bytes.",
    );
  }
  return root.value.toString("hex");
};

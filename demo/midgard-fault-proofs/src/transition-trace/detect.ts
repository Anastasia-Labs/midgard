import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";

import {
  TransitionTraceChallengerError,
  transitionTraceError,
} from "./errors.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  type AcceptedTransactionTransitionMismatchEvidence,
  buildAcceptedTransactionTransitionMismatchFault,
  buildCountFault,
  buildDuplicateTraceEventFault,
  buildEventToStepMismatchFault,
  buildInvalidForcedTransactionNoOpWitness,
  buildInvalidWithdrawalNoOpWitness,
  buildMappedEventMissingFromSourceFault,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildSourceEventMissingTraceFault,
  buildSourcePhaseMismatchFault,
  buildTraceBoundaryFault,
  buildTraceLinkFault,
  buildTransitionFaultProof,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
  rootCountProof,
} from "./witnesses.js";

export type TransitionTraceFaultKind =
  | "traceBoundary"
  | "traceLink"
  | "eventToStepMismatch"
  | "sourceMembershipMismatch"
  | "invalidOneStepTransition"
  | "omittedDueL1Event"
  | "duplicateTraceEvent"
  | "outOfWindowSourceEvent"
  | "countFault"
  | "acceptedTransactionTransitionMismatch";

export type TransitionTraceDetection =
  | {
      readonly buildable: true;
      readonly kind: TransitionTraceFaultKind;
      readonly invariant: string;
      readonly diagnostic: string;
      readonly fault: SDK.TransitionFault;
      readonly proof: SDK.TransitionFaultProof;
    }
  | {
      readonly buildable: false;
      readonly kind: TransitionTraceFaultKind;
      readonly invariant: string;
      readonly diagnostic: string;
      readonly reason: string;
    };

export type TransitionTraceDetectionEvidence = {
  readonly omittedDueL1Events?: readonly OmittedDueL1EventEvidence[];
  readonly outOfWindowSourceEvents?: readonly OutOfWindowSourceEventEvidence[];
  readonly acceptedTransactionTransitionMismatches?: readonly AcceptedTransactionTransitionMismatchEvidence[];
};

const detection = ({
  reconstruction,
  kind,
  invariant,
  diagnostic,
  fault,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly kind: TransitionTraceFaultKind;
  readonly invariant: string;
  readonly diagnostic: string;
  readonly fault: SDK.TransitionFault;
}): TransitionTraceDetection => ({
  buildable: true,
  kind,
  invariant,
  diagnostic,
  fault,
  proof: buildTransitionFaultProof({ reconstruction, fault }),
});

const unsupportedDetection = ({
  kind,
  invariant,
  diagnostic,
  reason,
}: {
  readonly kind: TransitionTraceFaultKind;
  readonly invariant: string;
  readonly diagnostic: string;
  readonly reason: string;
}): TransitionTraceDetection => ({
  buildable: false,
  kind,
  invariant,
  diagnostic,
  reason,
});

const maybeUnsupported = async (
  build: () => Promise<TransitionTraceDetection>,
  fallback: Omit<
    Extract<TransitionTraceDetection, { buildable: false }>,
    "buildable"
  >,
): Promise<TransitionTraceDetection> => {
  try {
    return await build();
  } catch (error) {
    if (
      error instanceof TransitionTraceChallengerError &&
      error.code === "unsupportedWitness"
    ) {
      return unsupportedDetection({
        ...fallback,
        reason: error.message,
      });
    }
    throw error;
  }
};

const orderedTrace = (
  reconstruction: TransitionTraceReconstruction,
): readonly SDK.TransitionStep[] =>
  [...reconstruction.transitionTrace]
    .sort((left, right) =>
      left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
    )
    .map((entry) => entry.value);

const sourceForStep = (
  reconstruction: TransitionTraceReconstruction,
  step: SDK.TransitionStep,
): SourceEventRecord | undefined =>
  reconstruction.sourceEventsByFingerprint.get(
    eventKeyFingerprint(step.event_key),
  );

export const detectCountFaults = (
  reconstruction: TransitionTraceReconstruction,
): readonly TransitionTraceDetection[] => {
  const header = reconstruction.header;
  const detections: TransitionTraceDetection[] = [];
  const expectedTotal =
    header.withdrawalCount +
    header.forcedTransactionCount +
    header.l2TransactionCount +
    header.depositCount;
  if (header.totalEventCount !== expectedTotal) {
    const fault = buildCountFault("HeaderTotalCountMismatch");
    detections.push(
      detection({
        reconstruction,
        kind: "countFault",
        invariant: "header_total_event_count",
        diagnostic: `HeaderV1 total_event_count ${header.totalEventCount.toString()} does not equal source count sum ${expectedTotal.toString()}.`,
        fault,
      }),
    );
  }
  if (header.transitionStepCount !== header.totalEventCount) {
    const fault = buildCountFault("HeaderTransitionStepCountMismatch");
    detections.push(
      detection({
        reconstruction,
        kind: "countFault",
        invariant: "header_transition_step_count",
        diagnostic: `HeaderV1 transition_step_count ${header.transitionStepCount.toString()} does not equal total_event_count ${header.totalEventCount.toString()}.`,
        fault,
      }),
    );
  }
  const rootCountChecks = [
    {
      invariant: "withdrawals_root_count",
      root: reconstruction.rootData.withdrawals,
      expected: header.withdrawalCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.withdrawals),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "forced_transactions_root_count",
      root: reconstruction.rootData.forcedTransactions,
      expected: header.forcedTransactionCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.forcedTransactions),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "transactions_root_count",
      root: reconstruction.rootData.transactions,
      expected: header.l2TransactionCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.transactions),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "deposits_root_count",
      root: reconstruction.rootData.deposits,
      expected: header.depositCount,
      witness: {
        SourceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.deposits),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "event_to_step_root_count",
      root: reconstruction.rootData.eventToStep,
      expected: header.totalEventCount,
      witness: {
        EventToStepRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.eventToStep),
        },
      } satisfies SDK.CountFaultWitness,
    },
    {
      invariant: "transition_trace_root_count",
      root: reconstruction.rootData.transitionTrace,
      expected: header.transitionStepCount,
      witness: {
        TransitionTraceRootCountMismatch: {
          proof: rootCountProof(reconstruction.rootData.transitionTrace),
        },
      } satisfies SDK.CountFaultWitness,
    },
  ] as const;
  for (const check of rootCountChecks) {
    if (check.root.count !== check.expected) {
      detections.push(
        detection({
          reconstruction,
          kind: "countFault",
          invariant: check.invariant,
          diagnostic: `${check.invariant} committed count ${check.root.count.toString()} does not match header count ${check.expected.toString()}.`,
          fault: buildCountFault(check.witness),
        }),
      );
    }
  }
  return detections;
};

const detectTraceBoundaryFaults = async (
  reconstruction: TransitionTraceReconstruction,
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  const first = reconstruction.traceByStepIndex.get(0n);
  if (
    first !== undefined &&
    first.value.pre_utxos_root !== reconstruction.header.prevUtxosRoot
  ) {
    detections.push(
      detection({
        reconstruction,
        kind: "traceBoundary",
        invariant: "trace_start_prev_utxos_root",
        diagnostic: `Trace step 0 pre_utxos_root ${first.value.pre_utxos_root} does not equal header.prev_utxos_root ${reconstruction.header.prevUtxosRoot}.`,
        fault: await buildTraceBoundaryFault({
          reconstruction,
          side: "TraceStart",
          stepIndex: 0n,
        }),
      }),
    );
  }
  const lastIndex = reconstruction.header.transitionStepCount - 1n;
  const last =
    lastIndex >= 0n
      ? reconstruction.traceByStepIndex.get(lastIndex)
      : undefined;
  if (
    last !== undefined &&
    last.value.post_utxos_root !== reconstruction.header.utxosRoot
  ) {
    detections.push(
      detection({
        reconstruction,
        kind: "traceBoundary",
        invariant: "trace_end_utxos_root",
        diagnostic: `Last trace step post_utxos_root ${last.value.post_utxos_root} does not equal header.utxos_root ${reconstruction.header.utxosRoot}.`,
        fault: await buildTraceBoundaryFault({
          reconstruction,
          side: "TraceEnd",
          stepIndex: lastIndex,
        }),
      }),
    );
  }
  return detections;
};

const detectTraceLinkFaults = async (
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

const detectDuplicateTraceEvents = async (
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

const detectEventToStepMismatches = async (
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

const detectSourceMembershipMismatches = async (
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

const detectInvalidNoOpTransitions = async (
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
      source.entry.value.operator_validity !== "TxIsValid" &&
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

const acceptedTerminalPostRoot = (
  terminalAcceptanceWitnessCbor: string,
): string => {
  const bytes = Buffer.from(terminalAcceptanceWitnessCbor, "hex");
  const array = readCborArrayHeader(
    bytes,
    0,
    "terminal acceptance witness",
  );
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

const detectAcceptedTransactionTransitionMismatches = (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly AcceptedTransactionTransitionMismatchEvidence[],
): readonly TransitionTraceDetection[] => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    if (item.claim.descriptor_membership.value.verdict !== "Accepted") {
      throw transitionTraceError(
        "missingWitnessData",
        "Accepted transition mismatch evidence must reference an accepted descriptor.",
      );
    }
    const committedPostRoot =
      item.claim.transition_step_membership.value.post_utxos_root;
    const validatedPostRoot = acceptedTerminalPostRoot(
      item.terminalAcceptanceWitnessCbor,
    );
    if (committedPostRoot !== validatedPostRoot) {
      detections.push(
        detection({
          reconstruction,
          kind: "acceptedTransactionTransitionMismatch",
          invariant: "accepted_transaction_uses_validated_ledger_root",
          diagnostic: `Accepted transaction transition commits ${committedPostRoot}, but its authenticated terminal validation witness commits ${validatedPostRoot}.`,
          fault: buildAcceptedTransactionTransitionMismatchFault(item),
        }),
      );
    }
  }
  return detections;
};

const detectOmittedDueL1Events = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly OmittedDueL1EventEvidence[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const eventKey =
      item.kind === "deposit"
        ? ({ DepositEventKey: { deposit_id: item.depositId } } as SDK.EventKey)
        : item.kind === "withdrawal"
          ? ({
              WithdrawalEventKey: { withdrawal_id: item.withdrawalId },
            } as SDK.EventKey)
          : ({
              ForcedTransactionEventKey: { tx_order_id: item.txOrderId },
            } as SDK.EventKey);
    const fingerprint = eventKeyFingerprint(eventKey);
    if (!reconstruction.sourceEventsByFingerprint.has(fingerprint)) {
      detections.push(
        detection({
          reconstruction,
          kind: "omittedDueL1Event",
          invariant: "due_l1_event_is_in_source_root",
          diagnostic: `Due ${item.kind} L1 event ${fingerprint} is absent from the committed ${eventKeyPhase(
            eventKey,
          )} source root.`,
          fault: await buildOmittedDueL1EventFault({
            reconstruction,
            evidence: item,
          }),
        }),
      );
    }
  }
  return detections;
};

const detectOutOfWindowSourceEvents = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly OutOfWindowSourceEventEvidence[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const eventKey =
      item.kind === "deposit"
        ? ({ DepositEventKey: { deposit_id: item.depositId } } as SDK.EventKey)
        : item.kind === "withdrawal"
          ? ({
              WithdrawalEventKey: { withdrawal_id: item.withdrawalId },
            } as SDK.EventKey)
          : ({
              ForcedTransactionEventKey: { tx_order_id: item.txOrderId },
            } as SDK.EventKey);
    const fingerprint = eventKeyFingerprint(eventKey);
    if (reconstruction.sourceEventsByFingerprint.has(fingerprint)) {
      detections.push(
        detection({
          reconstruction,
          kind: "outOfWindowSourceEvent",
          invariant: "source_event_is_within_block_window",
          diagnostic: `Out-of-window ${item.kind} L1 event ${fingerprint} is present in the committed source root.`,
          fault: await buildOutOfWindowSourceEventFault({
            reconstruction,
            evidence: item,
          }),
        }),
      );
    }
  }
  return detections;
};

export const detectTransitionTraceFaults = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence = {},
): Promise<readonly TransitionTraceDetection[]> => [
  ...detectCountFaults(reconstruction),
  ...(await detectTraceBoundaryFaults(reconstruction)),
  ...(await detectTraceLinkFaults(reconstruction)),
  ...(await detectDuplicateTraceEvents(reconstruction)),
  ...(await detectEventToStepMismatches(reconstruction)),
  ...(await detectSourceMembershipMismatches(reconstruction)),
  ...(await detectInvalidNoOpTransitions(reconstruction)),
  ...detectAcceptedTransactionTransitionMismatches(
    reconstruction,
    evidence.acceptedTransactionTransitionMismatches ?? [],
  ),
  ...(await detectOmittedDueL1Events(
    reconstruction,
    evidence.omittedDueL1Events ?? [],
  )),
  ...(await detectOutOfWindowSourceEvents(
    reconstruction,
    evidence.outOfWindowSourceEvents ?? [],
  )),
];

export const detectFirstTransitionTraceFault = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence = {},
): Promise<TransitionTraceDetection | undefined> =>
  (await detectTransitionTraceFaults(reconstruction, evidence))[0];

import * as SDK from "@al-ft/midgard-sdk";

import { TransitionTraceChallengerError } from "./errors.js";
import {
  eventKeyFingerprint,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  type AcceptedTransactionTransitionMismatchEvidence,
  buildCountFault,
  buildTraceBoundaryFault,
  buildTransitionFaultProof,
  type L2TransactionTransitionEvidence,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
  rootCountProof,
  type ValidDepositTransitionEvidence,
  type ValidWithdrawalTransitionEvidence,
} from "./witnesses.js";

/**
 * The complete set of fault kinds `detectTransitionTraceFaults` can report,
 * as a runtime tuple rather than a bare union — so closure-contract tests can
 * enumerate it directly instead of hand-copying the kind list. Add new kinds
 * here (the type below is derived from this array, not the reverse).
 */
export const TRANSITION_TRACE_FAULT_KINDS = [
  "traceBoundary",
  "traceLink",
  "eventToStepMismatch",
  "sourceMembershipMismatch",
  "invalidOneStepTransition",
  "omittedDueL1Event",
  "duplicateTraceEvent",
  "outOfWindowSourceEvent",
  "countFault",
  "acceptedTransactionTransitionMismatch",
] as const;

export type TransitionTraceFaultKind =
  (typeof TRANSITION_TRACE_FAULT_KINDS)[number];

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
  readonly depositTransitions?: readonly (ValidDepositTransitionEvidence & {
    readonly stepIndex: bigint;
  })[];
  readonly withdrawalTransitions?: readonly (ValidWithdrawalTransitionEvidence & {
    readonly stepIndex: bigint;
  })[];
  readonly omittedDueL1Events?: readonly OmittedDueL1EventEvidence[];
  readonly outOfWindowSourceEvents?: readonly OutOfWindowSourceEventEvidence[];
  readonly acceptedTransactionTransitionMismatches?: readonly AcceptedTransactionTransitionMismatchEvidence[];
  /**
   * Authenticated ledger mutation witnesses for L2 steps whose committed
   * post-root is disputed. Retained DA authenticates the transaction and its
   * field preimages, but does not contain the predecessor ledger tree needed
   * to derive these proofs automatically.
   */
  readonly l2TransactionTransitions?: readonly (L2TransactionTransitionEvidence & {
    readonly stepIndex: bigint;
  })[];
};

export const detection = ({
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

export const maybeUnsupported = async (
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

export const orderedTrace = (
  reconstruction: TransitionTraceReconstruction,
): readonly SDK.TransitionStep[] =>
  [...reconstruction.transitionTrace]
    .sort((left, right) =>
      left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
    )
    .map((entry) => entry.value);

export const sourceForStep = (
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

export const detectTraceBoundaryFaults = async (
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

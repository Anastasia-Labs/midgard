export type SpeculativeInvalidationReason =
  | "T1"
  | "T2"
  | "T3"
  | "T4"
  | "T5"
  | "T6"
  | "T7";

export type UserEventBarrierWatermarks = {
  readonly depositMs: number;
  readonly withdrawalMs: number;
  readonly txOrderMs: number;
  readonly refreshedAtMs: number;
};

export type SpeculativeCandidateSummary = {
  readonly candidateId: string;
  readonly baseHeaderHash: string;
  readonly endTimeMs: number;
  readonly builtAtMs: number;
  readonly buildDurationMs: number;
  readonly invalidationKey: string;
  readonly watermarks: UserEventBarrierWatermarks;
  readonly expectedUserEventCounts: {
    readonly deposits: number;
    readonly forcedTransactions: number;
    readonly withdrawals: number;
  };
  readonly expectedL2TransactionCount: number;
  readonly roots: {
    readonly utxos: string;
    /** Raw transaction MPF root retained by the local finalization path. */
    readonly rawTransactions: string;
    readonly transactions: string;
    readonly deposits: string;
    readonly forcedTransactions: string;
    readonly withdrawals: string;
    readonly transitionTrace: string;
    readonly eventToStep: string;
  };
};

type ActiveBase = {
  readonly baseHeaderHash: string;
  readonly rebuildAttempts: number;
};

export type SpeculativeCommitState =
  | { readonly _tag: "Idle" }
  | (ActiveBase & {
      readonly _tag: "Building";
      readonly startedAtMs: number;
    })
  | (ActiveBase & {
      readonly _tag: "ReadyToSubmit";
      readonly candidate: SpeculativeCandidateSummary;
    })
  | (ActiveBase & {
      readonly _tag: "Submitting";
      readonly candidate: SpeculativeCandidateSummary;
      readonly confirmationObservedAtMs: number;
    })
  | (ActiveBase & {
      readonly _tag: "Invalidated";
      readonly reason: SpeculativeInvalidationReason;
      readonly invalidatedAtMs: number;
    })
  | (ActiveBase & {
      readonly _tag: "Degraded";
      readonly reason: SpeculativeInvalidationReason;
      readonly degradedAtMs: number;
    });

export type SpeculativeCommitEvent =
  | {
      readonly _tag: "SubmittedBase";
      readonly baseHeaderHash: string;
      readonly atMs: number;
    }
  | {
      readonly _tag: "CandidateReady";
      readonly candidate: SpeculativeCandidateSummary;
    }
  | {
      readonly _tag: "ConfirmationObserved";
      readonly confirmedHeaderHash: string;
      readonly atMs: number;
    }
  | {
      readonly _tag: "Invalidate";
      readonly reason: SpeculativeInvalidationReason;
      readonly atMs: number;
    }
  | { readonly _tag: "RebuildStarted"; readonly atMs: number }
  | {
      readonly _tag: "SubmitSucceeded";
      readonly submittedHeaderHash: string;
      readonly atMs: number;
    }
  | { readonly _tag: "SubmissionDeferred" }
  | { readonly _tag: "Clear" };

export const idleSpeculativeCommitState = (): SpeculativeCommitState => ({
  _tag: "Idle",
});

const invalidate = (
  state: Exclude<SpeculativeCommitState, { readonly _tag: "Idle" }>,
  reason: SpeculativeInvalidationReason,
  atMs: number,
  maxRebuildAttempts: number,
): SpeculativeCommitState => {
  if (state._tag === "Invalidated" || state._tag === "Degraded") return state;
  const rebuildAttempts = state.rebuildAttempts + 1;
  return rebuildAttempts > maxRebuildAttempts
    ? {
        _tag: "Degraded",
        baseHeaderHash: state.baseHeaderHash,
        rebuildAttempts,
        reason,
        degradedAtMs: atMs,
      }
    : {
        _tag: "Invalidated",
        baseHeaderHash: state.baseHeaderHash,
        rebuildAttempts,
        reason,
        invalidatedAtMs: atMs,
      };
};

export const reduceSpeculativeCommitState = (
  state: SpeculativeCommitState,
  event: SpeculativeCommitEvent,
  maxRebuildAttempts: number,
): SpeculativeCommitState => {
  if (!Number.isSafeInteger(maxRebuildAttempts) || maxRebuildAttempts <= 0) {
    throw new RangeError("maxRebuildAttempts must be a positive safe integer");
  }
  if (event._tag === "Clear") return idleSpeculativeCommitState();
  if (event._tag === "SubmittedBase") {
    return {
      _tag: "Building",
      baseHeaderHash: event.baseHeaderHash,
      rebuildAttempts: 0,
      startedAtMs: event.atMs,
    };
  }
  if (event._tag === "Invalidate") {
    return state._tag === "Idle"
      ? state
      : invalidate(state, event.reason, event.atMs, maxRebuildAttempts);
  }
  switch (state._tag) {
    case "Idle":
      return state;
    case "Building":
      if (event._tag !== "CandidateReady") return state;
      return event.candidate.baseHeaderHash === state.baseHeaderHash
        ? {
            _tag: "ReadyToSubmit",
            baseHeaderHash: state.baseHeaderHash,
            rebuildAttempts: state.rebuildAttempts,
            candidate: event.candidate,
          }
        : invalidate(
            state,
            "T2",
            event.candidate.builtAtMs,
            maxRebuildAttempts,
          );
    case "ReadyToSubmit":
      if (event._tag !== "ConfirmationObserved") return state;
      return event.confirmedHeaderHash === state.baseHeaderHash
        ? {
            _tag: "Submitting",
            baseHeaderHash: state.baseHeaderHash,
            rebuildAttempts: state.rebuildAttempts,
            candidate: state.candidate,
            confirmationObservedAtMs: event.atMs,
          }
        : invalidate(state, "T2", event.atMs, maxRebuildAttempts);
    case "Submitting":
      if (event._tag === "SubmissionDeferred") {
        return {
          _tag: "ReadyToSubmit",
          baseHeaderHash: state.baseHeaderHash,
          rebuildAttempts: state.rebuildAttempts,
          candidate: state.candidate,
        };
      }
      return event._tag === "SubmitSucceeded"
        ? {
            _tag: "Building",
            baseHeaderHash: event.submittedHeaderHash,
            rebuildAttempts: 0,
            startedAtMs: event.atMs,
          }
        : state;
    case "Invalidated":
      return event._tag === "RebuildStarted"
        ? {
            _tag: "Building",
            baseHeaderHash: state.baseHeaderHash,
            rebuildAttempts: state.rebuildAttempts,
            startedAtMs: event.atMs,
          }
        : state;
    case "Degraded":
      return state;
  }
};

export type SpeculativeInvalidationEvidence = {
  readonly pendingBaseAbandoned?: boolean;
  readonly confirmedHeaderHash?: string;
  readonly candidateBaseHeaderHash?: string;
  readonly userEventCountsMatch?: boolean;
  readonly schedulerWindowFits?: boolean;
  readonly resetInProgress?: boolean;
  readonly confirmationExpired?: boolean;
  readonly processRestarted?: boolean;
};

export const decideSpeculativeInvalidation = (
  evidence: SpeculativeInvalidationEvidence,
): SpeculativeInvalidationReason | undefined => {
  if (evidence.processRestarted === true) return "T7";
  if (evidence.resetInProgress === true) return "T5";
  if (evidence.pendingBaseAbandoned === true) return "T1";
  if (evidence.confirmationExpired === true) return "T6";
  if (
    evidence.confirmedHeaderHash !== undefined &&
    evidence.candidateBaseHeaderHash !== undefined &&
    evidence.confirmedHeaderHash !== evidence.candidateBaseHeaderHash
  ) {
    return "T2";
  }
  if (evidence.userEventCountsMatch === false) return "T3";
  if (evidence.schedulerWindowFits === false) return "T4";
  return undefined;
};

export const minimumBarrierWatermarkMs = (
  watermarks: UserEventBarrierWatermarks,
): number =>
  Math.min(watermarks.depositMs, watermarks.withdrawalMs, watermarks.txOrderMs);

export const barrierWatermarksAreFresh = ({
  watermarks,
  nowMs,
  maxStalenessMs,
}: {
  readonly watermarks: UserEventBarrierWatermarks;
  readonly nowMs: number;
  readonly maxStalenessMs: number;
}): boolean =>
  minimumBarrierWatermarkMs(watermarks) > 0 &&
  nowMs - minimumBarrierWatermarkMs(watermarks) <= maxStalenessMs;

export const sameSpeculativeSourceIdSet = (
  actualIds: readonly string[],
  expectedIds: readonly string[],
): boolean => {
  if (actualIds.length !== expectedIds.length) return false;
  const expected = new Set(expectedIds);
  return (
    expected.size === expectedIds.length &&
    actualIds.every((id) => expected.has(id))
  );
};

export const shouldRetrySpeculativeConfirmationWake = ({
  state,
  confirmedHeaderHash,
}: {
  readonly state: SpeculativeCommitState;
  readonly confirmedHeaderHash: string;
}): boolean =>
  (state._tag === "Building" || state._tag === "Invalidated") &&
  state.baseHeaderHash === confirmedHeaderHash;

export const speculationOverlapEfficiency = ({
  buildDurationMs,
  confirmationWaitMs,
}: {
  readonly buildDurationMs: number;
  readonly confirmationWaitMs: number;
}): number => {
  const build = Math.max(0, buildDurationMs);
  const confirmation = Math.max(0, confirmationWaitMs);
  return build === 0 ? 1 : Math.min(build, confirmation) / build;
};

import { Effect, Ref } from "effect";

import type { GateState, GateView } from "../services/follower-write-gate.js";
import type { Globals } from "../services/globals.js";
import type { CommitEventCounts } from "./utils/commit-block-planner.event-window.js";

export type CommitPlanningSelection = Readonly<{
  mode: "normal" | "catch_up";
  observedAtMs: number;
  authority: Readonly<{
    view: GateView;
    /** Canonical nonnegative PostgreSQL bigint counter (migration 0021),
     * emitted by the follower driver as epoch::text; never a lease UUID. */
    epoch: string;
    baseHeaderHash: string;
    baseUtxosRoot: string;
    deploymentFingerprint: string | null;
  }>;
  coverageStartTimeMs: number;
  selectedEndTimeMs: number;
  blockTimeMs: number;
  canonicalHorizonMs: number;
  eligibleCutoffMs: number;
  /** SQL caps bound the number of returned instants. These are lower bounds,
   * not a claim that every eligible pending row was enumerated. */
  eligibleEventCountLowerBounds: CommitEventCounts;
  selectedEventCounts: CommitEventCounts;
  includedInstantCount: number;
  selectedOrdinaryTxCount: number;
  rescue: "whole_instant" | "ordinary" | null;
}>;
export type CommitPlanningNotice = Readonly<{ type: "CommitPlanningNotice" }> &
  (
    | Readonly<{ status: "selected"; selection: CommitPlanningSelection }>
    | Readonly<{
        status: "unavailable";
        reason:
          | "planning_started"
          | "missing_follower_permit"
          | "missing_admitted_parent"
          | "deployment_identity_unavailable";
      }>
  );
export const COMMIT_PLANNING_STARTED_NOTICE: CommitPlanningNotice = {
  type: "CommitPlanningNotice",
  status: "unavailable",
  reason: "planning_started",
};
export type CommitPlanningSnapshot = Readonly<{
  selection: CommitPlanningSelection;
  receivedAtMs: number;
}>;

const natural = (v: unknown): v is number =>
  typeof v === "number" && Number.isSafeInteger(v) && v >= 0;
const record = (v: unknown): v is Record<string, unknown> =>
  typeof v === "object" && v !== null && !Array.isArray(v);
const hex = (v: unknown, length: number): v is string =>
  typeof v === "string" && v.length === length && /^[a-f0-9]+$/u.test(v);
/** Migration 0021 starts at 0 and the sole driver increments. Refuse negative,
 * noncanonical or overflowing counters; bound the input before BigInt parse. */
const historyEpoch = (v: unknown): v is string =>
  typeof v === "string" &&
  /^(?:0|[1-9][0-9]{0,18})$/u.test(v) &&
  BigInt(v) <= 9223372036854775807n;
const counts = (v: unknown): v is CommitEventCounts =>
  record(v) &&
  natural(v.deposits) &&
  natural(v.withdrawals) &&
  natural(v.forcedTransactions);
const view = (v: unknown): v is GateView =>
  record(v) &&
  natural(v.generation) &&
  natural(v.slot) &&
  natural(v.height) &&
  hex(v.hash, 64);

/** The shared worker boundary refuses malformed timing/count/identity evidence.
 * Acceptance of a notice is only observation, never authorization to submit. */
export const parseCommitPlanningNotice = (
  input: unknown,
): CommitPlanningNotice | undefined => {
  if (!record(input) || input.type !== "CommitPlanningNotice") return undefined;
  if (input.status === "unavailable")
    return [
      "planning_started",
      "missing_follower_permit",
      "missing_admitted_parent",
      "deployment_identity_unavailable",
    ].includes(String(input.reason))
      ? (input as CommitPlanningNotice)
      : undefined;
  if (input.status !== "selected" || !record(input.selection)) return undefined;
  const s = input.selection;
  if (
    !["normal", "catch_up"].includes(String(s.mode)) ||
    !record(s.authority) ||
    !view(s.authority.view) ||
    !historyEpoch(s.authority.epoch) ||
    !hex(s.authority.baseHeaderHash, 56) ||
    !hex(s.authority.baseUtxosRoot, 64) ||
    !(
      s.authority.deploymentFingerprint === null ||
      hex(s.authority.deploymentFingerprint, 64)
    )
  )
    return undefined;
  if (
    ![
      s.observedAtMs,
      s.coverageStartTimeMs,
      s.selectedEndTimeMs,
      s.blockTimeMs,
      s.canonicalHorizonMs,
      s.eligibleCutoffMs,
      s.includedInstantCount,
      s.selectedOrdinaryTxCount,
    ].every(natural) ||
    !counts(s.eligibleEventCountLowerBounds) ||
    !counts(s.selectedEventCounts)
  )
    return undefined;
  const selected = s as unknown as CommitPlanningSelection;
  if (
    selected.selectedEndTimeMs <= selected.coverageStartTimeMs ||
    selected.selectedEndTimeMs > selected.blockTimeMs ||
    selected.eligibleCutoffMs >
      Math.min(selected.canonicalHorizonMs, selected.blockTimeMs) ||
    selected.selectedEndTimeMs > selected.eligibleCutoffMs ||
    (selected.mode === "normal" &&
      selected.selectedEndTimeMs !== selected.blockTimeMs) ||
    (selected.mode === "catch_up" && selected.selectedOrdinaryTxCount !== 0) ||
    ![null, "whole_instant", "ordinary"].includes(selected.rescue)
  )
    return undefined;
  if (
    Object.entries(selected.selectedEventCounts).some(
      ([kind, n]) =>
        n >
        selected.eligibleEventCountLowerBounds[kind as keyof CommitEventCounts],
    )
  )
    return undefined;
  return input as CommitPlanningNotice;
};

export const makeCommitPlanningNotice = (
  selection: CommitPlanningSelection,
): CommitPlanningNotice => {
  const notice = {
    type: "CommitPlanningNotice",
    status: "selected",
    selection,
  } as const;
  if (parseCommitPlanningNotice(notice) === undefined)
    throw new Error("invalid source-owned commit planning evidence");
  return notice;
};

/** Parent receipt time is independent of the worker's clock; a new attempt
 * explicitly clears its prior plan. Malformed notices also clear to unknown. */
export const applyCommitPlanningNotice = (
  globals: Pick<Globals, "COMMIT_PLANNING">,
  message: unknown,
) =>
  Effect.gen(function* () {
    const notice = parseCommitPlanningNotice(message);
    yield* Ref.set(
      globals.COMMIT_PLANNING,
      notice?.status === "selected"
        ? { selection: notice.selection, receivedAtMs: Date.now() }
        : null,
    );
  });

/** Exact current view/epoch matching intentionally invalidates a prior plan
 * after forward advancement as well as rollback/recompute. A stale plan is
 * retained as historical diagnostic evidence with no active mode assertion. */
export const commitPlanningReadiness = (input: {
  snapshot: CommitPlanningSnapshot | null;
  gate: GateState | undefined;
  localEpoch: string | undefined;
  recomputing: boolean;
  deploymentFingerprint: string | undefined;
  nowMs: number;
  maxAgeMs: number;
}) => {
  const snapshot = input.snapshot;
  if (snapshot === null)
    return {
      status: "unavailable" as const,
      reason: "no_selected_plan" as const,
      planned: null,
    };
  const s = snapshot.selection;
  const gate = input.gate;
  const ageMs = Math.max(
    0,
    input.nowMs - Math.min(s.observedAtMs, snapshot.receivedAtMs),
  );
  const current = gate?.applied;
  const sameView =
    current !== undefined &&
    current.generation === s.authority.view.generation &&
    current.slot === s.authority.view.slot &&
    current.hash === s.authority.view.hash &&
    current.height === s.authority.view.height;
  const staleReason =
    input.recomputing || gate?.pending !== undefined
      ? "follower_recomputing"
      : !sameView ||
          gate?.epoch !== s.authority.epoch ||
          input.localEpoch !== s.authority.epoch
        ? "follower_authority_changed"
        : input.deploymentFingerprint !== s.authority.deploymentFingerprint
          ? "deployment_authority_changed"
          : !natural(input.nowMs) ||
              !natural(input.maxAgeMs) ||
              input.nowMs < s.observedAtMs ||
              input.nowMs < snapshot.receivedAtMs
            ? "clock_invalid"
            : ageMs > input.maxAgeMs
              ? "plan_expired"
              : undefined;
  return {
    status:
      staleReason === undefined ? ("current" as const) : ("stale" as const),
    reason: staleReason ?? null,
    ageMs,
    receivedAtMs: snapshot.receivedAtMs,
    planned: s,
  };
};

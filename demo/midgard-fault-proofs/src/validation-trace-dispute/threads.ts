/** The threads defenders opened on a case of ours, and what the challenger
 * does next on each.
 *
 * A thread carries one token of the case policy, which mints it only in a
 * Defend on that case, named hash(case id ‖ defender key hash). The threads
 * are found along our case output's spend chain (case-history.ts): the case
 * token's complete L1 history names every thread a Defend left and every one
 * a close burned, and each thread it holds open is re-read by its token at
 * the game, timeout and boundary validators and, in its one-step resolution,
 * at the holders it visits on its way to the award (thread-observation.ts).
 * The threads found are checked against the case's own count of open
 * threads; a shortfall holds and reports, never decides.
 *
 * Each thread is played on its own, the most pressing first: on our turn we
 * reveal our midpoint, or on ReadyForOneStep enter the one-step resolution,
 * the earliest of our deadlines first; a defender whose clock ran out is
 * timed out; a thread in its resolution moves on hop by hop; a thread won
 * either way is closed into the case. A one step the runner does not drive
 * stops at a resumable "one-step required" state. A thread on our turn is
 * never timed out. With every thread closed the case settles. A defender's
 * win on one thread decides only the case: every other thread is still
 * played to its own end (its resolution, timeout and close), and the run
 * stops under its own state once no thread of ours has a move left. */
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import type { ValidationTraceOneStepHolder } from "./one-step-holders.js";
import type { ValidationTraceOneStepPlanning } from "./one-step-plan.js";

export { validationTraceThreadTokenName } from "./case-history.js";

/** A defended case whose threads are all closed with no valid claim. */
export const VALIDATION_TRACE_CASE_CONTESTED =
  "validation-trace-case-contested";

/** A defender won a thread of our case, which can no longer convict. */
export const VALIDATION_TRACE_DEFENDER_WON = "validation-trace-defender-won";

/** A thread of our case waits on its defender's move until a deadline. */
export const VALIDATION_TRACE_THREAD_AWAITING =
  "validation-trace-thread-awaiting-defender";

/** A thread reached ReadyForOneStep: the one-step resolution must run.
 * Only our move leaves ReadyForOneStep, so our clock runs on a thread
 * waiting here; our entry must land by the earlier of that clock's end and
 * the case's expiry, the deadline the alarm names. */
export const VALIDATION_TRACE_ONE_STEP_REQUIRED =
  "validation-trace-one-step-required";

/** A thread went the defender's way; the challenger has no move on it. */
export const VALIDATION_TRACE_THREAD_LOST = "validation-trace-thread-lost";

/** The threads found differ from the case's count of open threads. */
export const VALIDATION_TRACE_THREAD_COUNT_HOLD =
  "validation-trace-thread-count-hold";

/** Our turn on a thread, but no trace of ours matches its roots. */
export const VALIDATION_TRACE_MATERIAL_HOLD = "validation-trace-material-hold";

/** A thread's history does not decide the pending item step its one-step
 * resolution verifies (one-step-context-item-hops.ts). */
export const VALIDATION_TRACE_ITEM_PENDING_HOLD =
  "validation-trace-item-pending-hold";

/** How the threads were found: along our case output's spend chain. */
export const VALIDATION_TRACE_THREAD_DISCOVERY = "spend-chain" as const;

/** The validators that hold a thread with its case thread datum. */
export type ValidationTraceThreadHolder = "game" | "timeout" | "boundary";

/** A thread of our case, as found and checked. */
export type ValidationTraceThread = Readonly<{
  utxo: UTxO;
  outRef: string;
  unit: string;
  at: ValidationTraceThreadHolder;
  thread: SDK.CaseThread;
}>;

/**
 * A thread of ours in its one-step resolution past the boundary, as found
 * and checked: its token, its defender (from the case's history), and the
 * holder it is at, whose datum names us its prover.
 */
export type ValidationTraceResolvingThread = Readonly<{
  utxo: UTxO;
  outRef: string;
  unit: string;
  defenderVkh: string;
  holder: Exclude<ValidationTraceOneStepHolder, "boundary">;
}>;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * `utxo` as a thread of case `caseId` for `defenderVkh` under `unit`, or
 * undefined when it is not one.
 */
export const readValidationTraceThread = ({
  utxo,
  unit,
  caseId,
  defenderVkh,
  prover,
  at,
}: {
  readonly utxo: UTxO;
  readonly unit: string;
  readonly caseId: string;
  readonly defenderVkh: string;
  readonly prover: string;
  readonly at: ValidationTraceThreadHolder;
}): ValidationTraceThread | undefined => {
  if (utxo.assets[unit] !== 1n || typeof utxo.datum !== "string") return;
  let datum: SDK.CaseThreadDatum;
  try {
    datum = Data.from(utxo.datum, SDK.CaseThreadDatum);
  } catch {
    return;
  }
  const thread = datum.data;
  if (
    thread === null ||
    datum.fraud_prover !== prover ||
    thread.case_id !== caseId ||
    thread.header.defender_vkh !== defenderVkh
  )
    return;
  return Object.freeze({ utxo, outRef: outRefOf(utxo), unit, at, thread });
};

export type ValidationTraceThreadMove =
  | "reveal"
  | "timeout"
  | "close_thread"
  | "enter_resolution"
  | "resolve"
  | "close_awarded";

export type ValidationTraceThreadDecision =
  | Readonly<{
      kind: "move";
      stage: ValidationTraceThreadMove;
      thread: ValidationTraceThread | ValidationTraceResolvingThread;
    }>
  | Readonly<{ kind: "pending"; reason: string }>
  /** Every thread is closed and none went the defender's way. */
  | Readonly<{ kind: "settle" }>;

/** The end of a defender's turn: a timeout lands with its lower bound past it. */
export const validationTraceDefenderDeadlineMs = (
  thread: SDK.CaseThread,
): number => Number(thread.turn_started_at + thread.defender_clock_ms);

/** The case's expiry: `started_at + D_case`, the validators' own bound. */
export const validationTraceCaseExpiryMs = (caseDatum: SDK.Case): number =>
  Number(caseDatum.started_at) + MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;

/** The end of our turn: our move's lower bound may not pass it. */
export const validationTraceChallengerDeadlineMs = (
  thread: SDK.CaseThread,
): number => Number(thread.turn_started_at + thread.challenger_clock_ms);

/** Whether `material` is the trace the thread's challenger root commits. */
export const isThreadMaterial = (
  thread: SDK.CaseThread,
  material: ValidationTraceClaimMaterial | undefined,
): material is ValidationTraceClaimMaterial =>
  material !== undefined &&
  material.challengerRoot === thread.challenger_root &&
  material.stepCount === thread.step_count;

/**
 * The next action over the threads of our case at `caseOutRef`: `threads`
 * at the game, timeout and boundary, and `resolving` past the boundary.
 * Our reveals and entries into the one-step resolution come first, the
 * earliest deadline first, then timeouts, earliest first, then resolution
 * hops, then closes; otherwise the most
 * pressing wait. A thread ReadyForOneStep enters its resolution only on the
 * step `plannings` (by the thread's out-ref) plans for it; with none, the
 * resolution is not installed. With every thread closed the case settles;
 * on a case a defender won, the run stops once no thread has a move left or
 * awaits its defender.
 */
export const decideValidationTraceThreads = ({
  threads,
  resolving = [],
  plannings = new Map(),
  caseUnit,
  caseOutRef,
  caseDatum,
  now,
  material,
}: {
  readonly threads: readonly ValidationTraceThread[];
  readonly resolving?: readonly ValidationTraceResolvingThread[];
  readonly plannings?: ReadonlyMap<string, ValidationTraceOneStepPlanning>;
  readonly caseUnit: string;
  readonly caseOutRef: string;
  readonly caseDatum: SDK.Case;
  readonly now: number;
  readonly material: ValidationTraceClaimMaterial | undefined;
}): ValidationTraceThreadDecision => {
  const pending = (reason: string) => ({ kind: "pending" as const, reason });
  type Found = ValidationTraceThread | ValidationTraceResolvingThread;
  const moves: Record<ValidationTraceThreadMove, Found[]> = {
    reveal: [],
    enter_resolution: [],
    timeout: [],
    resolve: [],
    close_thread: [],
    close_awarded: [],
  };
  const oneStep: string[] = [];
  const lost: string[] = [];
  const held: string[] = [];
  const awaiting: { reason: string; deadline: number }[] = [];
  for (const found of threads) {
    const { thread, unit, outRef } = found;
    const turn = thread.turn;
    const label = `thread ${unit} at ${outRef}`;
    if (found.at === "timeout") {
      if (typeof turn !== "string" && "AwaitingOperator" in turn)
        moves.close_thread.push(found);
      else lost.push(`${label} timed out on the challenger's side`);
    } else if (found.at === "boundary") {
      if (isThreadMaterial(thread, material)) moves.resolve.push(found);
      else held.push(`${label} commits a trace this workflow does not hold`);
    } else if (turn === "ReadyForOneStep" || "AwaitingChallenger" in turn) {
      const planning = plannings.get(outRef);
      if (now > validationTraceChallengerDeadlineMs(thread))
        lost.push(`${label} ran out of the challenger's clock`);
      else if (!isThreadMaterial(thread, material))
        held.push(`${label} commits a trace this workflow does not hold`);
      else if (turn !== "ReadyForOneStep") moves.reveal.push(found);
      else if (planning?.kind === "plan") moves.enter_resolution.push(found);
      else
        oneStep.push(
          `${label} is ReadyForOneStep between states ${thread.low_index.toString()} and ${thread.high_index.toString()}, and must enter its resolution by ${Math.min(validationTraceCaseExpiryMs(caseDatum), validationTraceChallengerDeadlineMs(thread)).toString()}; ${planning?.reason ?? "the one-step resolution is not installed"}`,
        );
    } else if (now > validationTraceDefenderDeadlineMs(thread))
      moves.timeout.push(found);
    else {
      const deadline = validationTraceDefenderDeadlineMs(thread);
      awaiting.push({
        deadline,
        reason: `${VALIDATION_TRACE_THREAD_AWAITING}: ${label} awaits its defender until ${deadline.toString()}`,
      });
    }
  }
  for (const found of resolving)
    if (found.holder === "award") moves.close_awarded.push(found);
    else if (material === undefined)
      held.push(
        `thread ${found.unit} at ${found.outRef} is in its one-step resolution without our trace`,
      );
    else moves.resolve.push(found);
  const by =
    (deadline: (thread: SDK.CaseThread) => number) => (found: Found) =>
      "thread" in found ? deadline(found.thread) : Number.POSITIVE_INFINITY;
  const ours = by(validationTraceChallengerDeadlineMs);
  const theirs = by(validationTraceDefenderDeadlineMs);
  // The most pressing first: our move whose clock ends soonest, then the
  // timeout whose defender's clock ended first.
  const [first] = [
    ...moves.reveal.map((thread) => ["reveal", thread] as const),
    ...moves.enter_resolution.map(
      (thread) => ["enter_resolution", thread] as const,
    ),
  ].sort(([, a], [, b]) => ours(a) - ours(b));
  if (first !== undefined)
    return { kind: "move", stage: first[0], thread: first[1] };
  const [timeout] = [...moves.timeout].sort((a, b) => theirs(a) - theirs(b));
  if (timeout !== undefined)
    return { kind: "move", stage: "timeout", thread: timeout };
  for (const stage of ["resolve", "close_thread", "close_awarded"] as const) {
    const [next] = moves[stage];
    if (next !== undefined) return { kind: "move", stage, thread: next };
  }
  const [soonest] = awaiting.sort((a, b) => a.deadline - b.deadline);
  // A defender's win decides the case, not our other threads: each is played
  // to its own end, and the run stops only once none has a move left.
  if (caseDatum.defender_won)
    return pending(
      soonest?.reason ??
        `${VALIDATION_TRACE_DEFENDER_WON}: a defender won a thread of case ${caseUnit} at ${caseOutRef}, which cannot convict, and no thread of ours has a move left, so the run stops`,
    );
  if (oneStep.length > 0)
    return pending(
      `${VALIDATION_TRACE_ONE_STEP_REQUIRED}: case ${caseUnit} at ${caseOutRef}: ${oneStep.join("; ")}, so it waits`,
    );
  if (lost.length > 0)
    return pending(`${VALIDATION_TRACE_THREAD_LOST}: ${lost.join("; ")}`);
  if (held.length > 0)
    return pending(`${VALIDATION_TRACE_MATERIAL_HOLD}: ${held.join("; ")}`);
  const found = threads.length + resolving.length;
  if (BigInt(found) !== caseDatum.open_threads)
    return pending(
      `${VALIDATION_TRACE_THREAD_COUNT_HOLD}: case ${caseUnit} at ${caseOutRef} counts ${caseDatum.open_threads.toString()} open threads; ${VALIDATION_TRACE_THREAD_DISCOVERY} discovery found ${found.toString()}, so it holds until they agree`,
    );
  if (soonest !== undefined) return pending(soonest.reason);
  if (caseDatum.neither_claim_valid)
    return pending(
      `${VALIDATION_TRACE_CASE_CONTESTED}: case ${caseUnit} at ${caseOutRef} has no open thread and no valid claim; its close is not installed, so it waits`,
    );
  return { kind: "settle" };
};

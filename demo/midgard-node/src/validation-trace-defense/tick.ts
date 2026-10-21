/**
 * One tick of the defending operator, and the step the fiber repeats.
 *
 * - With no L1 follower view (no follower in this process, or none yet)
 *   the tick does nothing and names nothing: every read and every journaled
 *   intent rests on the follower.
 * - Otherwise it opens its plan (before its first L1 read), surveys, decides
 *   at L1 time now, and records the named states and the Defends it can
 *   build in the node's health before playing anything: an item it cannot
 *   fund names `validation-trace-defense-underfunded` with what it lacks, a
 *   Defend with no active node of ours names
 *   `validation-trace-defense-not-active`, and neither is played.
 * - An item whose workflow key has a live journaled attempt needs no
 *   funding (the attempt spends it already, so the wallet views no longer
 *   hold it): it is played, and comes to `pending`; S6 alone resends it.
 * - It plays the items earliest deadline first, each on its own: one that
 *   fails does not stop the rest, and the tick then fails with them all.
 * - The step never fails: a failed tick is recorded in the health and
 *   retried at the next one; nothing exits.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Cause, Effect, Either, Ref } from "effect";

import { retrieveUnreconciledSignedSubmission } from "../database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { Globals } from "../services/index.js";
import { IntentJournal } from "../services/intent-journal.js";
import {
  decideValidationTraceDefense,
  type ValidationTraceDefenseState,
  type ValidationTraceDefenseWork,
} from "./decide.js";
import {
  readDefenseAnchors,
  type ValidationTraceDefensePort,
} from "./deployment.js";
import { validationTraceDefenseChain } from "./discovery.js";
import { planDefendFunding, planMoveFunding } from "./funding.js";
import type {
  ValidationTraceDefenseDue,
  ValidationTraceDefenseHealth,
} from "./health.js";
import {
  liveUnder,
  playDefend,
  playExpiry,
  type PlayOutcome,
  playReveal,
  workKey,
} from "./play.js";
import { surveyValidationTraceDefense } from "./survey.js";

/** What the tick did with one item. */
export type TickPlay = Readonly<{
  kind: ValidationTraceDefenseWork["kind"];
  subject: string;
  outcome: PlayOutcome | "failed";
}>;

const subjectOf = (item: ValidationTraceDefenseWork): string =>
  item.kind === "defend" ? item.defenseCase.caseUnit : item.thread.unit;

const underfunded = (
  item: ValidationTraceDefenseWork,
  shortfall: bigint,
): ValidationTraceDefenseState => ({
  state: "validation-trace-defense-underfunded",
  subject: subjectOf(item),
  ...(item.kind === "expiry" ? {} : { deadline: item.deadline }),
  detail: `the operator wallet and the defense pocket lack ${shortfall.toString()} lovelace for the ${item.kind}`,
  failsReadiness: true,
  shortfallLovelace: shortfall,
});

/** The wallet views of the operator wallet and the pocket, now. */
const readViews = (port: ValidationTraceDefensePort) =>
  Effect.gen(function* () {
    const journal = yield* IntentJournal;
    const base = yield* journal.walletView(port.lucid, port.baseAddress);
    const pocket = yield* journal.walletView(port.lucid, port.pocketAddress);
    return { base: base.utxos, pocket: pocket.utxos };
  });

/** One tick; the items it played. */
export const validationTraceDefenseTick = (port: ValidationTraceDefensePort) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const journal = yield* IntentJournal;
    const health = globals.VALIDATION_TRACE_DEFENSE_HEALTH;
    const plan = yield* journal.openPlan;
    if (plan.kind === "none") {
      yield* Ref.update(
        health,
        (last): ValidationTraceDefenseHealth => ({
          ...last,
          following: false,
          states: [],
          dueDefends: [],
          defendDueSinceMs: null,
          decidedAtMs: Date.now(),
        }),
      );
      return [] as TickPlay[];
    }
    const wallNow = Date.now();
    const l1Now = port.lucid.slotToUnixTime(port.lucid.currentSlot());
    const survey = yield* surveyValidationTraceDefense({
      chain: validationTraceDefenseChain(port.dispute),
      ourVkh: port.ourVkh,
      l1SlotConfig: port.l1SlotConfig,
      cache: port.cache,
    });
    const casePolicyId = port.dispute.case.policyId;
    const decision = decideValidationTraceDefense(survey, l1Now, (unit) => {
      const orphan = survey.orphanThreads.find(
        (thread) => thread.unit === unit,
      );
      return orphan === undefined
        ? undefined
        : port.cache.get(casePolicyId + orphan.caseId);
    });
    const kept = new Set([
      ...survey.cases.map(({ caseUnit }) => caseUnit),
      ...survey.orphanThreads.map(({ caseId }) => casePolicyId + caseId),
    ]);
    for (const caseUnit of [...port.cache.keys()])
      if (!kept.has(caseUnit)) port.cache.delete(caseUnit);

    const anchors = yield* readDefenseAnchors(port);
    const views = yield* readViews(port);
    const states: ValidationTraceDefenseState[] = [...decision.states];
    const due: ValidationTraceDefenseDue[] = [];
    const playable: ValidationTraceDefenseWork[] = [];
    for (const item of decision.work) {
      const live = yield* liveUnder(workKey(item));
      if (item.kind === "defend") {
        if (anchors.activeNode === undefined) {
          states.push({
            state: "validation-trace-defense-not-active",
            subject: subjectOf(item),
            deadline: item.deadline,
            detail: "a case on our block, and no active-operator node of ours",
            failsReadiness: true,
          });
          continue;
        }
        const funding = planDefendFunding(views.base, views.pocket);
        if (!live && funding.kind === "short") {
          states.push(underfunded(item, funding.shortfall));
          continue;
        }
        if (l1Now <= item.deadline)
          due.push({ subject: subjectOf(item), deadline: item.deadline });
      } else {
        const funding = planMoveFunding(views.pocket);
        if (!live && funding.kind === "short") {
          states.push(underfunded(item, funding.shortfall));
          continue;
        }
      }
      playable.push(item);
    }
    const commitInFlight =
      (yield* retrieveUnreconciledSignedSubmission) !== undefined;
    yield* Ref.update(
      health,
      (last): ValidationTraceDefenseHealth => ({
        ...last,
        following: true,
        states,
        dueDefends: due,
        defendDueSinceMs:
          due.length === 0 || commitInFlight
            ? null
            : (last.defendDueSinceMs ?? wallNow),
        decidedAtMs: wallNow,
        l1OffsetMs: l1Now - wallNow,
      }),
    );

    const played: TickPlay[] = [];
    const failures: string[] = [];
    for (const item of playable) {
      const result = yield* Effect.either(
        Effect.gen(function* () {
          if (yield* liveUnder(workKey(item))) return "pending" as const;
          // Each item reads the views it spends from now: an earlier
          // item's predicted change is in them.
          const now = yield* readViews(port);
          switch (item.kind) {
            case "defend": {
              const funding = planDefendFunding(now.base, now.pocket);
              return funding.kind === "short"
                ? ("moved" as const)
                : yield* playDefend(port, item, funding, plan);
            }
            case "reveal": {
              const funding = planMoveFunding(now.pocket);
              return funding.kind === "short"
                ? ("moved" as const)
                : yield* playReveal(port, item, funding.inputs, plan);
            }
            case "expiry": {
              const funding = planMoveFunding(now.pocket);
              return funding.kind === "short"
                ? ("moved" as const)
                : yield* playExpiry(port, item, funding.inputs, plan);
            }
          }
        }),
      );
      const subject = subjectOf(item);
      if (Either.isLeft(result)) {
        failures.push(
          `${item.kind} ${subject}: ${formatUnknownError(result.left)}`,
        );
        played.push({ kind: item.kind, subject, outcome: "failed" });
      } else played.push({ kind: item.kind, subject, outcome: result.right });
    }
    if (failures.length > 0)
      return yield* Effect.fail(
        new Error(
          `validation-trace defense: ${failures.length.toString()} item(s) failed: ${failures.join("; ")}`,
        ),
      );
    return played;
  });

/**
 * The fiber's step: `tick`, with its outcome recorded in `health`. Never
 * fails.
 */
export const validationTraceDefenseStep = <E, R>(
  tick: Effect.Effect<unknown, E, R>,
  health: Ref.Ref<ValidationTraceDefenseHealth>,
): Effect.Effect<void, never, R> =>
  tick.pipe(
    Effect.zipRight(
      Ref.update(health, (last) => ({
        ...last,
        lastProgressAtMs: Date.now(),
        lastError: null,
        consecutiveFailures: 0,
      })),
    ),
    Effect.catchAllCause((cause) => {
      const error = formatUnknownError(Cause.squash(cause));
      return Ref.update(health, (last) => ({
        ...last,
        lastFailureAtMs: Date.now(),
        lastError: error,
        consecutiveFailures: last.consecutiveFailures + 1,
      })).pipe(
        Effect.zipRight(
          Effect.logWarning(`validation-trace defense tick failed: ${error}`),
        ),
      );
    }),
  );

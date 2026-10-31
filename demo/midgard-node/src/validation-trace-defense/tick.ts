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
 * - An item the wallet views cannot fund only because another item's live
 *   attempt holds what would fund it (the wallets as they are once that
 *   attempt dies would) names `validation-trace-defense-funding-pending`,
 *   a detail, and is not played; one the wallets cannot fund even then is
 *   underfunded.
 * - The published references are read once a tick, before any item takes
 *   the L1 control plane; while they cannot be read, the items that would
 *   be built name `validation-trace-defense-references-unresolved`, with
 *   what is missing, and are not played.
 * - It plays the items earliest deadline first, each on its own: one that
 *   fails does not stop the rest, and the tick then fails with them all.
 * - The step never fails: a failed tick is recorded in the health and
 *   retried at the next one; nothing exits.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { UTxO } from "@lucid-evolution/lucid";
import { Cause, Effect, Either, Option, Ref } from "effect";

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
  readWalletFacts,
  type ValidationTraceDefensePort,
} from "./deployment.js";
import { validationTraceDefenseChain } from "./discovery.js";
import { planDefendFunding, planMoveFunding } from "./funding.js";
import type {
  ValidationTraceDefenseDue,
  ValidationTraceDefenseFirstDue,
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

const fundingPending = (
  item: ValidationTraceDefenseWork,
): ValidationTraceDefenseState => ({
  state: "validation-trace-defense-funding-pending",
  subject: subjectOf(item),
  ...(item.kind === "expiry" ? {} : { deadline: item.deadline }),
  detail: `another live defense transaction of ours holds what funds the ${item.kind}, until it lands or dies`,
  failsReadiness: false,
});

const referencesUnresolved = (
  item: ValidationTraceDefenseWork,
  detail: string,
): ValidationTraceDefenseState => ({
  state: "validation-trace-defense-references-unresolved",
  subject: subjectOf(item),
  ...(item.kind === "expiry" ? {} : { deadline: item.deadline }),
  detail: `the ${item.kind} cannot be built: ${detail}`,
  failsReadiness: false,
});

/** What the wallets `views` lack for `item`, or undefined when they fund it. */
const shortfallOf = (
  item: ValidationTraceDefenseWork,
  views: Readonly<{ base: readonly UTxO[]; pocket: readonly UTxO[] }>,
): bigint | undefined => {
  const funding =
    item.kind === "defend"
      ? planDefendFunding(views.base, views.pocket)
      : planMoveFunding(views.pocket);
  return funding.kind === "short" ? funding.shortfall : undefined;
};

/**
 * `last` with the Defends `due` at wall-clock `wallNow` recorded: one due
 * for the first time is recorded unless a signed commit is unresolved (it
 * cannot be played yet); one recorded keeps its first time whatever
 * commits do; one whose deadline has passed at L1 time `l1Now` is
 * forgotten.
 */
export const recordDefendFirstDue = (
  last: ValidationTraceDefenseHealth["defendFirstDue"],
  due: readonly ValidationTraceDefenseDue[],
  commitInFlight: boolean,
  wallNow: number,
  l1Now: number,
): Map<string, ValidationTraceDefenseFirstDue> => {
  const next = new Map(
    [...last].filter(([, first]) => l1Now <= first.deadline),
  );
  for (const { subject, deadline } of due) {
    const first = next.get(subject);
    if (first !== undefined)
      next.set(subject, {
        firstDueMs: first.firstDueMs,
        deadline: Math.max(first.deadline, deadline),
      });
    else if (!commitInFlight)
      next.set(subject, { firstDueMs: wallNow, deadline });
  }
  return next;
};

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
      materialWorker: port.materialWorker,
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
    const live = new Map<ValidationTraceDefenseWork, boolean>();
    for (const item of decision.work)
      live.set(item, yield* liveUnder(workKey(item)));
    let facts:
      | Effect.Effect.Success<ReturnType<typeof readWalletFacts>>
      | undefined;
    const states: ValidationTraceDefenseState[] = [...decision.states];
    const candidates: ValidationTraceDefenseWork[] = [];
    for (const item of decision.work) {
      if (item.kind === "defend" && anchors.activeNode === undefined) {
        states.push({
          state: "validation-trace-defense-not-active",
          subject: subjectOf(item),
          deadline: item.deadline,
          detail: "a case on our block, and no active-operator node of ours",
          failsReadiness: true,
        });
        continue;
      }
      const shortfall = live.get(item) ? undefined : shortfallOf(item, views);
      if (shortfall !== undefined) {
        const othersLive = [...live].some(
          ([other, isLive]) => isLive && other !== item,
        );
        // The wallets as they are once the other attempts die.
        if (othersLive) facts ??= yield* readWalletFacts(port);
        states.push(
          othersLive &&
            facts !== undefined &&
            shortfallOf(item, facts) === undefined
            ? fundingPending(item)
            : underfunded(item, shortfall),
        );
        continue;
      }
      candidates.push(item);
    }
    // Read once, before any item takes the control plane; every play of
    // this tick reads the same result.
    const references = yield* Effect.cached(port.references);
    const unresolved = candidates.some((item) => !live.get(item))
      ? Either.getLeft(yield* Effect.either(references))
      : Option.none();
    const playable = Option.isNone(unresolved)
      ? candidates
      : candidates.filter((item) => {
          if (live.get(item)) return true;
          states.push(
            referencesUnresolved(
              item,
              formatUnknownError(unresolved.value, { includeCause: true }),
            ),
          );
          return false;
        });
    const due: ValidationTraceDefenseDue[] = playable.flatMap((item) =>
      item.kind === "defend" && l1Now <= item.deadline
        ? [{ subject: subjectOf(item), deadline: item.deadline }]
        : [],
    );
    const commitInFlight =
      (yield* retrieveUnreconciledSignedSubmission) !== undefined;
    yield* Ref.update(
      health,
      (last): ValidationTraceDefenseHealth => ({
        ...last,
        following: true,
        states,
        dueDefends: due,
        defendFirstDue: recordDefendFirstDue(
          last.defendFirstDue,
          due,
          commitInFlight,
          wallNow,
          l1Now,
        ),
        decidedAtMs: wallNow,
        l1OffsetMs: l1Now - wallNow,
      }),
    );
    const playPort: ValidationTraceDefensePort = { ...port, references };

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
                : yield* playDefend(playPort, item, funding, plan);
            }
            case "reveal": {
              const funding = planMoveFunding(now.pocket);
              return funding.kind === "short"
                ? ("moved" as const)
                : yield* playReveal(playPort, item, funding.inputs, plan);
            }
            case "expiry": {
              const funding = planMoveFunding(now.pocket);
              return funding.kind === "short"
                ? ("moved" as const)
                : yield* playExpiry(playPort, item, funding.inputs, plan);
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

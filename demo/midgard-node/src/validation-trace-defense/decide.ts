/**
 * What a defending operator does next, and the named states it reports,
 * from one survey at `now` (pure):
 *
 * - the work list: a Defend of each case on our targets that we have not
 *   defended and our material defends, due by `started_at + C` (its lower
 *   bound may not pass it); a RevealOperator on each live thread of ours
 *   awaiting our midpoint, due by our clock's end; and an expiry of each
 *   live thread of ours past its case's maximum duration. Defends and
 *   reveals run earliest deadline first; expiries, which have no deadline,
 *   come last;
 * - the named states, each with its deadline and whether it fails
 *   `/readyz` (liveness first: an intervention-required stop is named and
 *   fails readiness; nothing exits).
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import {
  classifyValidationTraceThread,
  type ValidationTraceThread,
  validationTraceThreadExpiresAfterMs,
} from "@al-ft/midgard-fault-proofs";

import type { OurThreadFacts } from "./discovery.js";
import { VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS } from "./health.js";
import type { ValidationTraceDefenseOutcome } from "./material.js";
import type { DefenseCase, DefenseSurvey } from "./survey.js";

export const VALIDATION_TRACE_DEFENSE_STATES = [
  "validation-trace-defense-awaiting",
  "validation-trace-defense-one-step",
  "validation-trace-defense-won-pending-expiry",
  "validation-trace-defense-thread-lost",
  "validation-trace-defense-operator-fault",
  "validation-trace-defense-trace-disagrees",
  "validation-trace-defense-material-unavailable",
  "validation-trace-defense-underfunded",
  "validation-trace-defense-not-active",
  "validation-trace-defense-late",
] as const;

export type ValidationTraceDefenseStateName =
  (typeof VALIDATION_TRACE_DEFENSE_STATES)[number];

export type ValidationTraceDefenseState = Readonly<{
  state: ValidationTraceDefenseStateName;
  /** The case or thread unit it is about. */
  subject: string;
  /** When it stops being repairable, in ms, when it has a deadline. */
  deadline?: number;
  detail: string;
  failsReadiness: boolean;
  /** What the node lacks to fund the move, when underfunded. */
  shortfallLovelace?: bigint;
}>;

export { VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS };

/** The last instant a Defend's lower bound may be: `started_at + C`. */
export const validationTraceDefendByMs = (caseDatum: {
  readonly started_at: bigint;
}): number =>
  Number(caseDatum.started_at) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;

type Defend = Extract<ValidationTraceDefenseOutcome, { kind: "defend" }>;

export type ValidationTraceDefenseWork =
  | Readonly<{
      kind: "defend";
      deadline: number;
      defenseCase: DefenseCase;
      material: Defend["material"];
      priorDefenders: readonly string[];
    }>
  | Readonly<{
      kind: "reveal";
      deadline: number;
      thread: ValidationTraceThread;
      material: Defend["material"];
    }>
  | Readonly<{
      kind: "expiry";
      /** Expiries have no deadline: they come after every other item. */
      deadline: number;
      thread: ValidationTraceThread;
    }>;

export type ValidationTraceDefenseDecision = Readonly<{
  work: readonly ValidationTraceDefenseWork[];
  states: readonly ValidationTraceDefenseState[];
}>;

const nearDeadline = (now: number, deadline: number) =>
  deadline - now < VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS;

/** The state of an outcome that does not defend, if any. */
const materialState = (
  subject: string,
  outcome: ValidationTraceDefenseOutcome | undefined,
  deadline: number,
  now: number,
): ValidationTraceDefenseState | undefined => {
  switch (outcome?.kind) {
    case "defend":
      return;
    case "operator_fault":
      // A named detail: the protocol itself stops the operator.
      return {
        state: "validation-trace-defense-operator-fault",
        subject,
        deadline,
        detail: outcome.detail,
        failsReadiness: false,
      };
    case "trace_disagrees":
      return {
        state: "validation-trace-defense-trace-disagrees",
        subject,
        deadline,
        detail: outcome.detail,
        failsReadiness: nearDeadline(now, deadline),
      };
    case "unavailable":
    case undefined:
      return {
        state: "validation-trace-defense-material-unavailable",
        subject,
        deadline,
        detail: outcome?.detail ?? "no defense material yet",
        failsReadiness: nearDeadline(now, deadline),
      };
  }
};

/** Our live thread's work and state at `now`. */
const threadDecision = (
  thread: ValidationTraceThread,
  material: ValidationTraceDefenseOutcome | undefined,
  now: number,
): {
  work: ValidationTraceDefenseWork[];
  states: ValidationTraceDefenseState[];
} => {
  const subject = thread.unit;
  if (now > validationTraceThreadExpiresAfterMs(thread.thread.header))
    return {
      work: [
        {
          kind: "expiry",
          deadline: Number.MAX_SAFE_INTEGER,
          thread,
        },
      ],
      states: [],
    };
  const standing = classifyValidationTraceThread({
    thread: thread.thread,
    at: thread.at,
    side: "operator",
    now,
  });
  const state = (
    name: ValidationTraceDefenseStateName,
    detail: string,
    deadline?: number,
  ): ValidationTraceDefenseState => ({
    state: name,
    subject,
    ...(deadline === undefined ? {} : { deadline }),
    detail,
    failsReadiness: false,
  });
  switch (standing.kind) {
    case "our_move": {
      if (material?.kind === "defend")
        return {
          work: [
            {
              kind: "reveal",
              deadline: standing.deadline,
              thread,
              material: material.material,
            },
          ],
          states: [],
        };
      const lacking = materialState(subject, material, standing.deadline, now);
      return { work: [], states: lacking === undefined ? [] : [lacking] };
    }
    case "their_move":
      return {
        work: [],
        states: [
          state(
            "validation-trace-defense-awaiting",
            "the challenger's move",
            standing.deadline,
          ),
        ],
      };
    case "one_step":
      return {
        work: [],
        states: [
          state("validation-trace-defense-one-step", "the one-step resolution"),
        ],
      };
    case "their_clock_out":
    case "won":
      return {
        work: [],
        states: [
          state(
            "validation-trace-defense-won-pending-expiry",
            "the challenger ran out of clock",
            validationTraceThreadExpiresAfterMs(thread.thread.header),
          ),
        ],
      };
    case "lost":
      return {
        work: [],
        states: [
          state("validation-trace-defense-thread-lost", "our clock ran out"),
        ],
      };
  }
};

/** Our thread's work and state, wherever it stands. */
const standingDecision = (
  subject: string,
  facts: OurThreadFacts,
  material: ValidationTraceDefenseOutcome | undefined,
  now: number,
) => {
  const lost = (detail: string) => ({
    work: [],
    states: [
      {
        state: "validation-trace-defense-thread-lost" as const,
        subject,
        detail,
        failsReadiness: false,
      },
    ],
  });
  switch (facts.kind) {
    case "live":
      return threadDecision(facts.thread, material, now);
    case "awarded":
      return lost("the thread is at the award");
    case "closed":
      return facts.lost
        ? lost("the thread closed with a lost-defense receipt")
        : { work: [], states: [] };
    case "resolving":
      return {
        work: [],
        states: [
          {
            state: "validation-trace-defense-one-step" as const,
            subject,
            detail: "the one-step resolution",
            failsReadiness: false,
          },
        ],
      };
    case "none":
      return { work: [], states: [] };
  }
};

/** The case's work and states at `now`. */
const caseDecision = (defenseCase: DefenseCase, now: number) => {
  const subject = defenseCase.caseUnit;
  if (defenseCase.thread.kind !== "none")
    return standingDecision(
      subject,
      defenseCase.thread,
      defenseCase.outcome,
      now,
    );
  if (defenseCase.defended === true) return { work: [], states: [] };
  const deadline = validationTraceDefendByMs(defenseCase.datum);
  if (defenseCase.priorDefenders === undefined)
    return {
      work: [],
      states: [
        {
          state: "validation-trace-defense-material-unavailable" as const,
          subject,
          deadline,
          detail:
            "the case's history is no longer complete in the follower's retention",
          failsReadiness: nearDeadline(now, deadline),
        },
      ],
    };
  const outcome = defenseCase.outcome;
  if (outcome?.kind !== "defend") {
    const lacking = materialState(subject, outcome, deadline, now);
    return { work: [], states: lacking === undefined ? [] : [lacking] };
  }
  return {
    work: [
      {
        kind: "defend" as const,
        deadline,
        defenseCase,
        material: outcome.material,
        priorDefenders: defenseCase.priorDefenders,
      },
    ],
    states: nearDeadline(now, deadline)
      ? [
          {
            state: "validation-trace-defense-late" as const,
            subject,
            deadline,
            detail: "the Defend has not landed",
            failsReadiness: true,
          },
        ]
      : [],
  };
};

/** The work list, earliest deadline first, and the named states. */
export const decideValidationTraceDefense = (
  survey: DefenseSurvey,
  now: number,
  /** Material of threads whose case is gone, by thread unit. */
  orphanMaterial: (
    threadUnit: string,
  ) => ValidationTraceDefenseOutcome | undefined = () => undefined,
): ValidationTraceDefenseDecision => {
  const work: ValidationTraceDefenseWork[] = [];
  const states: ValidationTraceDefenseState[] = [];
  for (const decided of [
    ...survey.cases.map((defenseCase) => caseDecision(defenseCase, now)),
    ...survey.orphanThreads.map((thread) =>
      standingDecision(
        thread.unit,
        thread.facts,
        orphanMaterial(thread.unit),
        now,
      ),
    ),
  ]) {
    work.push(...decided.work);
    states.push(...decided.states);
  }
  work.sort((a, b) => a.deadline - b.deadline);
  return { work, states };
};

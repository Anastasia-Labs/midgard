/**
 * What `/readyz` makes of the validation-trace defender's health:
 *
 * - an underfunded move and a case with no active node of ours fail it,
 *   with what is lacking; our own fault and the waiting states are details;
 * - missing material fails it only once its deadline is near, and a due
 *   Defend near its deadline fails it as late;
 * - a failing tick is recorded, never raised, and a fiber failing past the
 *   stall bound fails it until a tick succeeds;
 * - with no follower the defender names nothing;
 * - every reason it names stays readable on a public node;
 * - the node's `/readyz` answers unready on an underfunded move or a case
 *   with no active node of ours, and stays ready on our own fault.
 */
import "./utils.js";

import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { publicReadinessPresentation } from "../src/commands/listen-readiness-public-presentation.js";
import {
  VALIDATION_TRACE_DEFENSE_STATES,
  type ValidationTraceDefenseState,
} from "../src/validation-trace-defense/decide.js";
import {
  initialValidationTraceDefenseHealth,
  VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS,
  VALIDATION_TRACE_DEFENSE_STALL_MS,
  type ValidationTraceDefenseHealth,
  validationTraceDefenseReadiness,
} from "../src/validation-trace-defense/health.js";
import { validationTraceDefenseStep } from "../src/validation-trace-defense/tick.js";
import { readyz } from "./readiness-honest-degradation-route.readyz-fixture.js";

const NOW = 1_800_000_000_000;
const subject = "ab".repeat(30);
const far = NOW + 4 * VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS;
const near = NOW + VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS - 1;

const state = (
  name: ValidationTraceDefenseState["state"],
  failsReadiness: boolean,
  extra: Partial<ValidationTraceDefenseState> = {},
): ValidationTraceDefenseState => ({
  state: name,
  subject,
  detail: "free-form detail never published",
  failsReadiness,
  ...extra,
});

const following = (
  overrides: Partial<ValidationTraceDefenseHealth>,
): ValidationTraceDefenseHealth => ({
  ...initialValidationTraceDefenseHealth(NOW),
  following: true,
  ...overrides,
});

const readiness = (health: ValidationTraceDefenseHealth) =>
  validationTraceDefenseReadiness(health, NOW);

describe("the validation-trace defender's readiness", () => {
  it("fails on an underfunded move and on no active node, and keeps the rest as details", () => {
    const { reasons, details } = readiness(
      following({
        states: [
          state("validation-trace-defense-underfunded", true, {
            deadline: far,
            shortfallLovelace: 12_345n,
          }),
          state("validation-trace-defense-not-active", true, { deadline: far }),
          state("validation-trace-defense-operator-fault", false),
          state("validation-trace-defense-awaiting", false, { deadline: far }),
        ],
      }),
    );
    expect(reasons).toStrictEqual([
      `validation-trace-defense-underfunded:${subject}:${far.toString()}:12345`,
      `validation-trace-defense-not-active:${subject}:${far.toString()}`,
    ]);
    expect(details).toStrictEqual([
      `validation-trace-defense-operator-fault:${subject}:none`,
      `validation-trace-defense-awaiting:${subject}:${far.toString()}`,
    ]);
  });

  it("fails on missing material only near its deadline", () => {
    for (const [deadline, fails] of [
      [far, false],
      [near, true],
    ] as const) {
      const { reasons, details } = readiness(
        following({
          states: [
            state("validation-trace-defense-material-unavailable", false, {
              deadline,
            }),
          ],
        }),
      );
      expect([reasons.length, details.length]).toStrictEqual(
        fails ? [1, 0] : [0, 1],
      );
    }
  });

  it("fails on a due Defend near its deadline as late, and not before", () => {
    expect(
      readiness(following({ dueDefends: [{ subject, deadline: far }] }))
        .reasons,
    ).toStrictEqual([]);
    expect(
      readiness(following({ dueDefends: [{ subject, deadline: near }] }))
        .reasons,
    ).toStrictEqual([
      `validation-trace-defense-late:${subject}:${near.toString()}`,
    ]);
  });

  it("fails once the fiber has failed past the stall bound", () => {
    const failing = (sinceProgress: number) =>
      readiness(
        following({
          consecutiveFailures: 3,
          lastProgressAtMs: NOW - sinceProgress,
        }),
      ).reasons;
    expect(failing(VALIDATION_TRACE_DEFENSE_STALL_MS)).toStrictEqual([]);
    expect(failing(VALIDATION_TRACE_DEFENSE_STALL_MS + 1)).toStrictEqual([
      "validation-trace-defense-stalled:3",
    ]);
  });

  it("names nothing with no follower", () => {
    expect(
      readiness({
        ...following({
          states: [state("validation-trace-defense-not-active", true)],
          dueDefends: [{ subject, deadline: near }],
        }),
        following: false,
      }),
    ).toStrictEqual({ reasons: [], details: [] });
  });

  it("keeps every reason it names readable on a public node", () => {
    const { reasons, details } = readiness(
      following({
        states: [
          state("validation-trace-defense-underfunded", true, {
            deadline: far,
            shortfallLovelace: 7n,
          }),
          state("validation-trace-defense-not-active", true),
          state("validation-trace-defense-operator-fault", false),
        ],
        dueDefends: [{ subject, deadline: near }],
        consecutiveFailures: 1,
        lastProgressAtMs: NOW - VALIDATION_TRACE_DEFENSE_STALL_MS - 1,
      }),
    );
    const body = { ready: false, reasons, details };
    expect(publicReadinessPresentation(body, "preprod-public")).toStrictEqual(
      body,
    );
    // Every state the defender can name is published as it is.
    for (const name of VALIDATION_TRACE_DEFENSE_STATES) {
      const line = `${name}:${subject}:${far.toString()}`;
      expect(
        publicReadinessPresentation({ details: [line] }, "preprod-public"),
      ).toStrictEqual({ details: [line] });
    }
    // A free-form line in the same place is not published.
    expect(
      publicReadinessPresentation(
        { reasons: ["validation-trace-defense-not-active:not a unit:none"] },
        "preprod-public",
      ),
    ).toStrictEqual({ reasons: ["E_READINESS_DIAGNOSTIC_REDACTED"] });
  });

  it("makes /readyz unready on an underfunded move and on no active node, and keeps our own fault ready", async () => {
    const withStates = (states: readonly ValidationTraceDefenseState[]) =>
      readyz({
        globalsSetup: (globals) =>
          Ref.set(globals.VALIDATION_TRACE_DEFENSE_HEALTH, {
            ...initialValidationTraceDefenseHealth(Date.now()),
            following: true,
            states,
          }),
      });
    const deadline = Date.now() + 4 * VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS;
    const underfunded = state("validation-trace-defense-underfunded", true, {
      deadline,
      shortfallLovelace: 1n,
    });
    const notActive = state("validation-trace-defense-not-active", true, {
      deadline,
    });
    const fault = state("validation-trace-defense-operator-fault", false);
    for (const failing of [underfunded, notActive]) {
      const response = await withStates([failing, fault]);
      expect(response).toMatchObject({ status: 503, ready: false });
      expect(response.reasons).toStrictEqual([
        `${failing.state}:${subject}:${deadline.toString()}${failing.shortfallLovelace === undefined ? "" : ":1"}`,
      ]);
      expect(response.details).toContain(
        `validation-trace-defense-operator-fault:${subject}:none`,
      );
    }
    const ready = await withStates([fault]);
    expect(ready).toMatchObject({ status: 200, ready: true, reasons: [] });
    expect(ready.details).toContain(
      `validation-trace-defense-operator-fault:${subject}:none`,
    );
  });

  it("records a failing tick without failing, and names the fiber stalled once it keeps failing past the bound", async () => {
    const health = Effect.runSync(
      Ref.make(following({ lastProgressAtMs: Date.now() })),
    );
    const step = (tick: Effect.Effect<unknown, Error>) =>
      Effect.runPromise(validationTraceDefenseStep(tick, health));
    for (const attempt of [1, 2]) {
      await step(
        Effect.fail(new Error(`no references, attempt ${attempt.toString()}`)),
      );
      expect(Effect.runSync(Ref.get(health))).toMatchObject({
        consecutiveFailures: attempt,
        lastError: `Error: no references, attempt ${attempt.toString()}`,
      });
    }
    const failing = Effect.runSync(Ref.get(health));
    expect(
      validationTraceDefenseReadiness(
        failing,
        failing.lastProgressAtMs + VALIDATION_TRACE_DEFENSE_STALL_MS + 1,
      ).reasons,
    ).toStrictEqual(["validation-trace-defense-stalled:2"]);
    // A tick that succeeds clears it.
    await step(Effect.void);
    const recovered = Effect.runSync(Ref.get(health));
    expect(recovered).toMatchObject({
      consecutiveFailures: 0,
      lastError: null,
    });
    expect(
      validationTraceDefenseReadiness(
        recovered,
        recovered.lastProgressAtMs + VALIDATION_TRACE_DEFENSE_STALL_MS + 1,
      ).reasons,
    ).toStrictEqual([]);
  });
});

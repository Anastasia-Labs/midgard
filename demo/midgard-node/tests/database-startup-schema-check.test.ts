/**
 * The node's startup schema step (`ensureSchemaWithStartupRetry`): a dropped
 * connection is waited out within the database budget, its reason reported
 * to the startup (`reportStartupWaiting`) until the step runs; one that
 * outlives the budget fails under its reason; a schema refusal fails at
 * once under `database_schema_incompatible`.
 */
import { Cause, Duration, Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ensureSchemaWithStartupRetry } from "../src/database/init.js";
import { NodeSchemaError } from "../src/database/schema.js";
import {
  DATABASE_SCHEMA_INCOMPATIBLE,
  DATABASE_UNREACHABLE,
  findStartupStepFailure,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";

const FAST = {
  baseDelay: Duration.millis(5),
  maxDelay: Duration.millis(20),
  budget: Duration.minutes(15),
} as const;

/** Runs `effect` for up to `ms` with its startup reports recorded;
 * `result` is `None` when it was still waiting. */
const captureFor = async <A, E>(effect: Effect.Effect<A, E>, ms: number) => {
  const reported: [string, readonly string[]][] = [];
  const result = await Effect.runPromise(
    Effect.either(
      effect.pipe(
        Effect.timeoutOption(Duration.millis(ms)),
        Effect.locally(StartupWaitingReporter, (key, reasons) =>
          Effect.sync(() => {
            reported.push([key, reasons]);
          }),
        ),
      ),
    ),
  );
  return { result, reported };
};

describe("the startup schema step", () => {
  const scripted = (failures: readonly unknown[]) => {
    let calls = 0;
    const effect = Effect.suspend(() => {
      const failure = failures[calls];
      calls += 1;
      return failure === undefined ? Effect.void : Effect.fail(failure);
    });
    return { effect, calls: () => calls };
  };
  const refused = () =>
    new Error("Failed to reserve a connection", {
      cause: Object.assign(new Error("connect ECONNREFUSED"), {
        code: "ECONNREFUSED",
      }),
    });

  it("waits out a dropped connection, reporting its reason, then passes once", async () => {
    const check = scripted([refused(), refused()]);
    const { result, reported } = await captureFor(
      ensureSchemaWithStartupRetry(check.effect, FAST),
      10_000,
    );
    expect(result).toMatchObject({ _tag: "Right", right: { _tag: "Some" } });
    expect(check.calls()).toBe(3);
    expect(reported).toEqual([
      ["database_schema_check", [DATABASE_UNREACHABLE]],
      ["database_schema_check", [DATABASE_UNREACHABLE]],
      ["database_schema_check", []],
    ]);
  });

  it("fails under database_unreachable once the wait outlives the budget", async () => {
    const failure = refused();
    const check = scripted(Array.from({ length: 10_000 }, () => failure));
    const { result, reported } = await captureFor(
      ensureSchemaWithStartupRetry(check.effect, {
        ...FAST,
        budget: Duration.millis(200),
      }),
      10_000,
    );
    expect(result._tag).toBe("Left");
    const error = result._tag === "Left" ? result.left : undefined;
    expect(
      error === undefined
        ? undefined
        : findStartupStepFailure(Cause.fail(error)),
    ).toMatchObject({
      step: "database_schema_check",
      reason: DATABASE_UNREACHABLE,
      exhausted: true,
    });
    expect(check.calls()).toBeGreaterThan(3);
    expect(check.calls()).toBeLessThan(1_000);
    expect(reported.at(-1)).toEqual(["database_schema_check", []]);
  });

  it("fails a schema refusal at once", async () => {
    const check = scripted([
      new NodeSchemaError({ message: "created by a different node schema" }),
    ]);
    const { result } = await captureFor(
      ensureSchemaWithStartupRetry(check.effect, FAST),
      10_000,
    );
    expect(result._tag).toBe("Left");
    const error = result._tag === "Left" ? result.left : undefined;
    expect(
      error === undefined
        ? undefined
        : findStartupStepFailure(Cause.fail(error)),
    ).toMatchObject({
      step: "database_schema_check",
      reason: DATABASE_SCHEMA_INCOMPATIBLE,
      exhausted: false,
    });
    expect(check.calls()).toBe(1);
  });
});

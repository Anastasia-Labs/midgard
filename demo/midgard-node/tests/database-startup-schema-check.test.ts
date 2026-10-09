/**
 * The node's startup schema compatibility check
 * (`assertCompatibleWithStartupRetry`): a dropped connection or a running
 * migration is waited out with no deadline, its reason reported to the
 * startup (`reportStartupWaiting`) until the check runs; a schema verdict
 * fails at once.
 */
import { Duration, Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import { assertCompatibleWithStartupRetry } from "../src/database/init.js";
import { MigrationError } from "../src/database/migrations/runner.js";
import {
  DATABASE_UNREACHABLE,
  SCHEMA_MIGRATION_IN_PROGRESS,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";

const FAST = {
  baseDelay: Duration.millis(5),
  maxDelay: Duration.millis(20),
} as const;

/** Runs `effect` for up to `ms` with its logs and startup reports recorded;
 * `result` is `None` when it was still waiting. */
const captureFor = async <A, E>(effect: Effect.Effect<A, E>, ms: number) => {
  const logs: string[] = [];
  const reported: [string, readonly string[]][] = [];
  const logger = Logger.make(({ message }) => {
    logs.push(Array.isArray(message) ? message.join(" ") : String(message));
  });
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
    ).pipe(Effect.provide(Logger.replace(Logger.defaultLogger, logger))),
  );
  return { result, logs, reported };
};

describe("the startup schema compatibility check", () => {
  const scripted = (failures: readonly MigrationError[]) => {
    let calls = 0;
    const effect = Effect.suspend(() => {
      const failure = failures[calls];
      calls += 1;
      return failure === undefined ? Effect.void : Effect.fail(failure);
    });
    return { effect, calls: () => calls };
  };
  const refused = (code: string) =>
    new MigrationError({
      code,
      message: "Failed to reserve schema migration connection",
      cause: Object.assign(new Error("connect ECONNREFUSED"), {
        code: "ECONNREFUSED",
      }),
    });

  it("waits out a dropped connection and a running migration, then passes once", async () => {
    const check = scripted([
      refused("schema_lock_failed"),
      new MigrationError({
        code: "schema_migration_in_progress",
        message: "Could not acquire Midgard schema migration advisory lock",
      }),
    ]);
    const { result, logs } = await captureFor(
      assertCompatibleWithStartupRetry(check.effect, FAST),
      10_000,
    );
    expect(result._tag).toBe("Right");
    expect(check.calls()).toBe(3);
    expect(
      logs.filter((line) => line.includes("Database unready")),
    ).toHaveLength(2);
  });

  it("reports each wait's reason to the startup, and none once it passes", async () => {
    const check = scripted([
      refused("schema_lock_failed"),
      new MigrationError({
        code: SCHEMA_MIGRATION_IN_PROGRESS,
        message: "Could not acquire Midgard schema migration advisory lock",
      }),
    ]);
    const { result, reported } = await captureFor(
      assertCompatibleWithStartupRetry(check.effect, FAST),
      10_000,
    );
    expect(result).toMatchObject({ _tag: "Right", right: { _tag: "Some" } });
    expect(reported).toEqual([
      ["database_schema_check", [DATABASE_UNREACHABLE]],
      ["database_schema_check", [SCHEMA_MIGRATION_IN_PROGRESS]],
      ["database_schema_check", []],
    ]);
  });

  it("keeps waiting while a migration holds the schema lock, with no deadline", async () => {
    const running = new MigrationError({
      code: SCHEMA_MIGRATION_IN_PROGRESS,
      message: "Could not acquire Midgard schema migration advisory lock",
    });
    const check = scripted(Array.from({ length: 10_000 }, () => running));
    const { result } = await captureFor(
      assertCompatibleWithStartupRetry(check.effect, FAST),
      1_000,
    );
    expect(result).toMatchObject({ _tag: "Right", right: { _tag: "None" } });
    expect(check.calls()).toBeGreaterThan(10);
  });

  it.each([
    "schema_not_migrated",
    "schema_unversioned_database",
    "schema_checksum_mismatch",
  ])("fails a %s verdict at once", async (code) => {
    const check = scripted([
      new MigrationError({ code, message: "incompatible" }),
    ]);
    const { result } = await captureFor(
      assertCompatibleWithStartupRetry(check.effect, FAST),
      10_000,
    );
    expect(result._tag).toBe("Left");
    expect(check.calls()).toBe(1);
  });
});

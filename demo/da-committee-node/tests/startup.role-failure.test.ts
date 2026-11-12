/**
 * The committee's startup adapter over the shared failure classification
 * (`classifyRoleFailure`), against its conformance rows: a transient or
 * contended dependency is retried for at most the startup budget and then
 * exits non-zero; a failure a restart may clear (a port in use) exits at
 * once; a deterministic or unknown one holds the process up, unready.
 */
import type { RoleFailureClass } from "@al-ft/midgard-l1-follower";
import { ROLE_FAILURE_ROWS } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import {
  classifyStartupFailure,
  retryStartup,
  type StartupFailureClass,
  startupFailureOutcome,
} from "../src/startup.js";

const COMMITTEE: Readonly<
  Record<
    RoleFailureClass,
    Readonly<{ startup: StartupFailureClass; outcome: "exit" | "hold" }>
  >
> = {
  transient: { startup: "transient", outcome: "exit" },
  contention: { startup: "transient", outcome: "exit" },
  restart: { startup: "restart", outcome: "exit" },
  deterministic: { startup: "fatal", outcome: "hold" },
  unknown: { startup: "fatal", outcome: "hold" },
};

describe("the committee's startup adapter", () => {
  it.each(ROLE_FAILURE_ROWS)("classifies $name", (row) => {
    expect(classifyStartupFailure(row.error())).toBe(
      COMMITTEE[row.expected].startup,
    );
  });

  it.each(ROLE_FAILURE_ROWS)(
    "exits or holds on $name once its budget is spent",
    async (row) => {
      const failure = await retryStartup({
        attempt: () => Promise.reject(row.error()),
        onFailure: () => undefined,
        write: () => undefined,
        budgetMs: 0,
        sleep: () => Promise.resolve(),
      }).then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(startupFailureOutcome(failure)).toBe(
        COMMITTEE[row.expected].outcome,
      );
    },
  );
});

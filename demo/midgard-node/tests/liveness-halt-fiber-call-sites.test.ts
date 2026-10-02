/**
 * Each held fiber in the node's roster is held under its own name's entry of
 * `FIBER_HALT_SOURCES`. The table itself is pinned in `liveness-halt.test.ts`;
 * this pins the call sites: a roster entry held under another fiber's name
 * (the commit fiber under "merge", say) would stop for the wrong halts.
 */
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { nodeFibers } from "../src/commands/listen.node-fibers.js";
import { Globals } from "../src/services/globals.js";
import type { NodeConfigDep } from "../src/services/index.js";
import {
  FIBER_HALT_SOURCES,
  type HeldFiber,
} from "../src/services/liveness-halt.js";

// The sources each held roster entry, once run, asked to be held under.
const held = vi.hoisted(() => ({ sources: [] as (readonly string[])[] }));

// Every name gets a source list of its own, so a call site that names another
// fiber shows up even where two fibers share their real sources.
vi.mock("../src/services/liveness-halt.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/services/liveness-halt.js")>();
  const { Effect: E } = await import("effect");
  return {
    ...actual,
    FIBER_HALT_SOURCES: Object.fromEntries(
      Object.keys(actual.FIBER_HALT_SOURCES).map((name) => [
        name,
        [`held_as:${name}`],
      ]),
    ),
    pausedWhileHalted: (
      schedule: unknown,
      _globals: unknown,
      sources: readonly string[],
    ) => {
      held.sources.push(sources);
      return schedule;
    },
    restartedAcrossHalts: (
      _globals: unknown,
      sources: readonly string[],
      _fiber: unknown,
    ) => {
      held.sources.push(sources);
      return E.void;
    },
  };
});

// The scheduled held fibers run their schedule-built effect; stand them in.
vi.mock("../src/fibers/index.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await importOriginal<typeof import("../src/fibers/index.js")>()),
    blockCommitmentFiber: () => E.void,
    mergeFiber: () => E.void,
  };
});

const nodeConfig = {
  ADMISSION_BACKLOG_REFRESH_MS: 1_000,
  MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS: 1_000,
  WAIT_BETWEEN_BLOCK_COMMITMENT: 1_000,
  WAIT_BETWEEN_BLOCK_CONFIRMATION: 1_000,
  SPECULATIVE_COMMIT_BUILD: true,
  USER_EVENT_BARRIER_REFRESH_MS: 1_000,
  WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: 1_000,
  WAIT_BETWEEN_RETENTION_SWEEPS: 1_000,
  WAIT_BETWEEN_MERGE_TXS: 1_000,
  TX_QUEUE_POLL_INTERVAL_MS: 1_000,
} as unknown as NodeConfigDep;

describe("held fiber call sites", () => {
  it("holds every held roster fiber under its own name", async () => {
    const roster = nodeFibers({ nodeConfig, withMonitoring: false });
    const heldAs: Record<string, (readonly string[])[]> = {};
    for (const name of Object.keys(FIBER_HALT_SOURCES) as HeldFiber[]) {
      held.sources.length = 0;
      // A held entry returns at once here; an entry no longer held runs its
      // real fiber, which the timeout stops, and records nothing.
      await Effect.runPromise(
        (
          roster[name].pipe(
            Effect.provide(Globals.Default),
          ) as unknown as Effect.Effect<unknown, unknown, never>
        ).pipe(Effect.timeout("2 seconds"), Effect.exit),
      );
      heldAs[name] = [...held.sources];
    }
    expect(heldAs).toEqual(
      Object.fromEntries(
        Object.keys(FIBER_HALT_SOURCES).map((name) => [
          name,
          [[`held_as:${name}`]],
        ]),
      ),
    );
  });
});

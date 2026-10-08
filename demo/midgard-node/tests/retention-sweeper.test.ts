import "./utils.js";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Exit, Ref, Schedule } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { RetentionL1View } from "../src/database/daPayloads.js";
import {
  fetchRetentionL1View,
  RETENTION_L1_VIEW_STALE,
  retentionL1ViewTimeoutMs,
  retentionSweeperFiber,
} from "../src/fibers/retention-sweeper.js";
import { L1SlotUnknownError } from "../src/l1-heads.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { makeRetentionL1Queue } from "./helpers/retention-l1-view.js";
import { provideDatabaseLayers } from "./utils.js";

const SWEEP_MS = 1_000;
const FATAL_MS = 60_000;
const START = 1_000_000;

const VIEW: RetentionL1View = {
  confirmedHeadHash: Buffer.alloc(28, 1),
  liveQueueHeaderHashes: [],
};

/**
 * Runs the sweeper on fresh node globals. `read` yields the L1 view source's
 * outcome per read and may inspect the liveness reasons raised so far. The
 * SQL client counts the statements a sweep issues (each sweep issues one
 * before its stub fails, which the sweep swallows), so `sweeps` is the number
 * of sweeps that ran. `nowMs` walks `clock`, holding its last value.
 */
const runSweeper = (options: {
  readonly clock: readonly number[];
  readonly read: (
    index: number,
    reasons: () => ReadonlyMap<string, string>,
  ) => Effect.Effect<RetentionL1View, unknown, never>;
  readonly sweeps: number;
  readonly sweepMs?: number;
  readonly fatalMs?: number;
  /** Holds the L1 control-plane permit for the whole run. */
  readonly holdControlPlane?: boolean;
  /** The L1 now each sweep dates itself with; START by default. */
  readonly l1NowMs?: Effect.Effect<number, L1SlotUnknownError>;
}) => {
  let reads = 0;
  let tick = 0;
  let statements = 0;
  const nowMs = () =>
    options.clock[Math.min(tick++, options.clock.length - 1)]!;
  const sql = (first: unknown) => {
    if (Array.isArray(first) && "raw" in first) {
      statements += 1;
      throw new Error("no database in this test");
    }
    return first;
  };
  return Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const reasons = () => Effect.runSync(Ref.get(globals.LIVENESS_REASONS));
      if (options.holdControlPlane === true)
        yield* globals.L1_CONTROL_PLANE.take(1);
      const exit = yield* Effect.exit(
        retentionSweeperFiber(Schedule.recurs(options.sweeps - 1), {
          fetchL1View: Effect.suspend(() => options.read(reads++, reasons)),
          nowMs,
          l1NowMs: options.l1NowMs ?? Effect.succeed(START),
        }).pipe(
          Effect.timeoutFail({
            duration: "2 seconds",
            onTimeout: () => new Error("sweeper blocked"),
          }),
        ),
      );
      return { exit, reasons: reasons() };
    }).pipe(
      Effect.provideService(NodeConfig, {
        RETENTION_DAYS: 0,
        WAIT_BETWEEN_RETENTION_SWEEPS: options.sweepMs ?? SWEEP_MS,
        L1_VIEW_FATAL_MS: options.fatalMs ?? FATAL_MS,
      } as never),
      Effect.provideService(SqlClient.SqlClient, sql as never),
      Effect.provideService(ContractDeploymentIdentity, {} as never),
      Effect.provideService(Lucid, {} as never),
      Effect.provideService(MidgardContracts, {} as never),
      Effect.provide(Globals.Default),
    ),
  ).then((result) => ({
    ...result,
    reads: () => reads,
    sweeps: () => statements,
  }));
};

const unreadable = () => Effect.fail(new Error("ogmios connection refused"));

const staleReason = (reasons: ReadonlyMap<string, string>) =>
  [...reasons.values()].filter((reason) => reason === RETENTION_L1_VIEW_STALE);

describe("retention sweeper L1-view deadline", () => {
  it("reports unavailable DA recovery proof and clears it after the next successful proof read", async () => {
    const seen: string[][] = [];
    const result = await runSweeper({
      clock: [START],
      sweeps: 2,
      read: (index, reasons) => {
        seen.push([...reasons().values()]);
        return Effect.succeed({
          ...VIEW,
          retirementProofUnavailable: index === 0,
        });
      },
    });
    expect(seen).toEqual([[], ["retention_da_recovery_proof_unavailable"]]);
    expect([...result.reasons.values()]).not.toContain(
      "retention_da_recovery_proof_unavailable",
    );
    expect(result.reads()).toBe(2);
  });

  it("past L1_VIEW_FATAL_MS raises retention_l1_view_stale, sweeps nothing and keeps reading", async () => {
    // Ref init; sweep 1 reads at the deadline (a sweep without DA pruning);
    // sweeps 2-5 read 1 ms past it.
    const { exit, reads, sweeps, reasons } = await runSweeper({
      clock: [START, START + FATAL_MS, START + FATAL_MS, START + FATAL_MS + 1],
      read: unreadable,
      sweeps: 5,
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(staleReason(reasons)).toEqual([RETENTION_L1_VIEW_STALE]);
    expect(reads()).toBe(5);
    expect(sweeps()).toBe(1);
  });

  it("clears the reason on the first good view after the deadline and sweeps once per good view", async () => {
    // Sweep 1 reads past the deadline and raises; sweep 2's read returns.
    const raisedAtRead: string[][] = [];
    const { exit, reads, sweeps, reasons } = await runSweeper({
      clock: [
        START,
        START + FATAL_MS + 1,
        START + FATAL_MS + 1,
        START + FATAL_MS + 2,
      ],
      read: (index, current) => {
        raisedAtRead.push(staleReason(current()));
        return index === 0 ? unreadable() : Effect.succeed(VIEW);
      },
      sweeps: 2,
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(raisedAtRead).toEqual([[], [RETENTION_L1_VIEW_STALE]]);
    expect(staleReason(reasons)).toEqual([]);
    expect(reads()).toBe(2);
    expect(sweeps()).toBe(1);
  });

  it("measures the deadline from the last successful L1 view, so a healthy node raises nothing", async () => {
    // A good view at START + FATAL_MS, then two failed reads 10 ms and one
    // full deadline after it: neither is past the deadline.
    const { exit, reads, sweeps, reasons } = await runSweeper({
      clock: [
        START,
        START + FATAL_MS,
        START + FATAL_MS + 5,
        START + FATAL_MS + 10,
        START + 2 * FATAL_MS,
      ],
      read: (index) => (index === 0 ? Effect.succeed(VIEW) : unreadable()),
      sweeps: 3,
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(staleReason(reasons)).toEqual([]);
    expect(reads()).toBe(3);
    expect(sweeps()).toBe(3);
  });

  it.each([
    ["an interruptible", Effect.async<RetentionL1View>(() => {})],
    [
      "an uninterruptible",
      Effect.uninterruptible(Effect.async<RetentionL1View>(() => {})),
    ],
  ] as const)(
    "abandons %s hung L1 read, raises the reason at the deadline and keeps reading",
    async (_label, hung) => {
      const { exit, reads, sweeps, reasons } = await runSweeper({
        clock: [START, START + 1, START + 60 + 1],
        read: () => hung,
        sweeps: 5,
        sweepMs: 20,
        fatalMs: 60,
      });
      expect(Exit.isSuccess(exit)).toBe(true);
      expect(staleReason(reasons)).toEqual([RETENTION_L1_VIEW_STALE]);
      expect(reads()).toBe(5);
      expect(sweeps()).toBe(0);
    },
    5_000,
  );
});

describe("retention sweeper L1 now", () => {
  it("prunes nothing while the L1 slot is unknown, and keeps reading", async () => {
    const { exit, reads, sweeps, reasons } = await runSweeper({
      clock: [START],
      read: () => Effect.succeed(VIEW),
      sweeps: 3,
      l1NowMs: Effect.fail(
        new L1SlotUnknownError({ message: "L1 slot unknown: test" }),
      ),
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(reads()).toBe(3);
    expect(sweeps()).toBe(0);
    expect(staleReason(reasons)).toEqual([]);
  });
});

describe("retention sweeper tx-order watermark", () => {
  it("never takes the L1 control plane, so a held permit does not delay a sweep", async () => {
    const { exit, reads, sweeps } = await runSweeper({
      clock: [START],
      read: () => Effect.succeed(VIEW),
      sweeps: 2,
      holdControlPlane: true,
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(reads()).toBe(2);
    expect(sweeps()).toBe(2);
  });
});

describe("retention sweeper L1 read timeout", () => {
  it("reads a queue whose walk outgrew the sweep interval instead of abandoning every read", async () => {
    // Each walk takes 70 ms against a 25 ms interval: the reads at 25 ms and
    // 50 ms are abandoned, the doubled 100 ms read returns, and later reads
    // get four times the last walk.
    let completed = 0;
    const { exit, reads, reasons } = await runSweeper({
      clock: [START],
      read: () =>
        Effect.sleep("70 millis").pipe(
          Effect.tap(() => {
            completed += 1;
          }),
          Effect.as(VIEW),
        ),
      sweeps: 5,
      sweepMs: 25,
      fatalMs: 10_000,
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(staleReason(reasons)).toEqual([]);
    expect(reads()).toBe(5);
    expect(completed).toBeGreaterThanOrEqual(2);
  }, 10_000);

  it("scales with the last walk and consecutive timeouts, and never exceeds the deadline", () => {
    const at = (lastWalkMs: number | undefined, consecutiveTimeouts: number) =>
      retentionL1ViewTimeoutMs({
        sweepMs: 1_000,
        fatalMs: 60_000,
        lastWalkMs,
        consecutiveTimeouts,
      });
    expect(at(undefined, 0)).toBe(1_000);
    expect(at(100, 0)).toBe(1_000);
    expect(at(5_000, 0)).toBe(20_000);
    expect(at(undefined, 1)).toBe(2_000);
    expect(at(undefined, 3)).toBe(8_000);
    expect(at(5_000, 1)).toBe(40_000);
    expect(at(5_000, 2)).toBe(60_000);
    expect(at(undefined, 1_000)).toBe(60_000);
  });
});

describe("retention L1 view exemption sets", () => {
  it("takes the confirmed head from the root datum and every header node as live", async () => {
    const queue = await makeRetentionL1Queue({
      confirmedHeadHash: "ab".repeat(28),
      liveUtxosRoots: ["71".repeat(32), "72".repeat(32)],
    });
    const view = await Effect.runPromise(
      provideDatabaseLayers(queue.provide(fetchRetentionL1View)),
    );
    expect(view.confirmedHeadHash.toString("hex")).toBe("ab".repeat(28));
    expect(
      view.liveQueueHeaderHashes.map((hash) => hash.toString("hex")),
    ).toEqual(queue.liveHeaderHashes);
    expect(new Set(queue.liveHeaderHashes).size).toBe(2);
  });
});

describe("L1_VIEW_FATAL_MS configuration", () => {
  const loadNodeConfig = () =>
    Effect.runPromise(
      Effect.gen(function* () {
        return yield* NodeConfig;
      }).pipe(Effect.provide(NodeConfig.layer)),
    );

  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("defaults to the DA attestation timeout, valid at the default sweep interval", async () => {
    vi.stubEnv("L1_VIEW_FATAL_MS", undefined);
    vi.stubEnv("WAIT_BETWEEN_RETENTION_SWEEPS", undefined);
    const timeoutMs =
      SELECTED_DEPLOYMENT_PROFILE.timing.da_attestation_timeout_ms;
    const sweepMs = Math.min(900_000, Math.floor(timeoutMs / 4));
    expect(Number(SDK.DA_ATTESTATION_TIMEOUT_MS)).toBe(timeoutMs);
    await expect(loadNodeConfig()).resolves.toMatchObject({
      WAIT_BETWEEN_RETENTION_SWEEPS: sweepMs,
      L1_VIEW_FATAL_MS: timeoutMs,
    });
  });

  it("accepts exactly three sweep intervals and rejects one millisecond less", async () => {
    vi.stubEnv("WAIT_BETWEEN_RETENTION_SWEEPS", SWEEP_MS.toString());
    vi.stubEnv("L1_VIEW_FATAL_MS", (3 * SWEEP_MS).toString());
    await expect(loadNodeConfig()).resolves.toMatchObject({
      L1_VIEW_FATAL_MS: 3 * SWEEP_MS,
    });
    vi.stubEnv("L1_VIEW_FATAL_MS", (3 * SWEEP_MS - 1).toString());
    await expect(loadNodeConfig()).rejects.toThrow(/three poll intervals/u);
  });

  it("rejects a deadline above the deployed retention margin", async () => {
    vi.stubEnv(
      "L1_VIEW_FATAL_MS",
      (MIDGARD_RETENTION_WINDOW.marginMs + 1).toString(),
    );
    await expect(loadNodeConfig()).rejects.toThrow(/retention margin/u);
  });
});

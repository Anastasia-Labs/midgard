import "./utils.js";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option, Schedule } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { RetentionL1View } from "../src/database/daPayloads.js";
import {
  fetchRetentionL1View,
  RetentionL1ViewUnavailableError,
  retentionSweeperFiber,
} from "../src/fibers/retention-sweeper.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { makeRetentionL1Queue } from "./helpers/retention-l1-view.js";

const SWEEP_MS = 1_000;
const FATAL_MS = 60_000;
const START = 1_000_000;

const VIEW: RetentionL1View = {
  confirmedHeadHash: Buffer.alloc(28, 1),
  liveQueueHeaderHashes: [],
};

/**
 * Runs the sweeper next to a never-ending sibling, as `listen` composes its
 * fibers. `reads` yields the L1 view source's outcome per read. The services a
 * sweep would touch are inert stubs: a sweep swallows their failures, so only
 * the fatal error can end the group. `nowMs` walks `clock`, holding its last
 * value.
 */
const runSweeper = (options: {
  readonly clock: readonly number[];
  readonly read: (
    index: number,
  ) => Effect.Effect<RetentionL1View, unknown, never>;
  readonly sweeps: number;
  readonly sweepMs?: number;
  readonly fatalMs?: number;
  readonly withSibling?: boolean;
}) => {
  let reads = 0;
  let tick = 0;
  const nowMs = () =>
    options.clock[Math.min(tick++, options.clock.length - 1)]!;
  const fiber = retentionSweeperFiber(Schedule.recurs(options.sweeps - 1), {
    fetchL1View: Effect.suspend(() => options.read(reads++)),
    nowMs,
  });
  return Effect.runPromise(
    Effect.exit(
      options.withSibling === false
        ? fiber
        : Effect.all([fiber, Effect.never], { concurrency: "unbounded" }),
    ).pipe(
      Effect.provideService(NodeConfig, {
        RETENTION_DAYS: 0,
        WAIT_BETWEEN_RETENTION_SWEEPS: options.sweepMs ?? SWEEP_MS,
        L1_VIEW_FATAL_MS: options.fatalMs ?? FATAL_MS,
      } as never),
      Effect.provideService(SqlClient.SqlClient, {} as never),
      Effect.provideService(ContractDeploymentIdentity, {} as never),
      Effect.provideService(Lucid, {} as never),
      Effect.provideService(MidgardContracts, {} as never),
    ),
  ).then((exit) => ({ exit, reads: () => reads }));
};

const unreadable = () => Effect.fail(new Error("ogmios connection refused"));

const failureOf = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit)
    ? Option.getOrUndefined(Cause.failureOption(exit.cause))
    : undefined;

describe("retention sweeper L1-view exit rule", () => {
  it("fails the fiber group once the last L1 view is older than L1_VIEW_FATAL_MS", async () => {
    // Ref init; sweep 1 reads at the deadline (skip only); sweep 2 reads 1 ms
    // past it.
    const { exit, reads } = await runSweeper({
      clock: [START, START + FATAL_MS, START + FATAL_MS, START + FATAL_MS + 1],
      read: unreadable,
      sweeps: 5,
    });
    const failure = failureOf(exit);
    expect(failure).toBeInstanceOf(RetentionL1ViewUnavailableError);
    expect(failure).toMatchObject({
      l1ViewAgeMs: FATAL_MS + 1,
      l1ViewFatalMs: FATAL_MS,
    });
    expect(reads()).toBe(2);
  });

  it("measures the deadline from the last successful L1 view, so a healthy node never exits", async () => {
    // A good view at START + FATAL_MS, then two failed reads 10 ms and one
    // full deadline after it: neither is past the deadline.
    const { exit, reads } = await runSweeper({
      clock: [
        START,
        START + FATAL_MS,
        START + FATAL_MS + 5,
        START + FATAL_MS + 10,
        START + 2 * FATAL_MS,
      ],
      read: (index) => (index === 0 ? Effect.succeed(VIEW) : unreadable()),
      sweeps: 3,
      withSibling: false,
    });
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(reads()).toBe(3);
  });

  it.each([
    ["an interruptible", Effect.async<RetentionL1View>(() => {})],
    [
      "an uninterruptible",
      Effect.uninterruptible(Effect.async<RetentionL1View>(() => {})),
    ],
  ] as const)(
    "abandons %s hung L1 read after one sweep interval and still fires the deadline",
    async (_label, hung) => {
      const { exit, reads } = await runSweeper({
        clock: [START, START + 1, START + 60 + 1],
        read: () => hung,
        sweeps: 5,
        sweepMs: 20,
        fatalMs: 60,
      });
      expect(failureOf(exit)).toMatchObject({
        _tag: "RetentionL1ViewUnavailableError",
        l1ViewAgeMs: 61,
        l1ViewFatalMs: 60,
      });
      expect(reads()).toBe(1);
    },
    5_000,
  );
});

describe("retention L1 view exemption sets", () => {
  it("takes the confirmed head from the root datum and every header node as live", async () => {
    const queue = await makeRetentionL1Queue({
      confirmedHeadHash: "ab".repeat(28),
      liveUtxosRoots: ["71".repeat(32), "72".repeat(32)],
    });
    const view = await Effect.runPromise(queue.provide(fetchRetentionL1View));
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
    expect(Number(SDK.DA_ATTESTATION_TIMEOUT_MS)).toBe(3_600_000);
    await expect(loadNodeConfig()).resolves.toMatchObject({
      WAIT_BETWEEN_RETENTION_SWEEPS: 900_000,
      L1_VIEW_FATAL_MS: 3_600_000,
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

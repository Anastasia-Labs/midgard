/**
 * `midgard_local_ledger_divergence_total` as the Prometheus exporter of a
 * monitored node serves it (`MidgardLocalLedgerDivergence` alerts on its
 * increase). Every check's series is exported at 0 from startup, so the
 * first divergence shows as an increase. A commit worker's refusal on a
 * diverged base is counted once, when its output reaches the main thread,
 * whose metrics the exporter reads: a worker thread's metrics stay in that
 * thread, so the check the worker runs counts nothing itself. Each
 * divergence the main thread's verification of the landed ledger records
 * is counted under its check, `confirmed_ledger` at the frontier or
 * `confirmed_ledger_tail` at the tail, the first named on `/readyz`, and
 * clears the verified frontier commits build from; the next pass records it
 * again and clears the reason. A divergence from a run below the recorded
 * generation is counted too, but raises nothing and leaves the record.
 */
import { Effect, Either, Ref } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  CONFIRMED_LEDGER_VERIFICATION_SOURCE,
  recordConfirmedLedgerVerification,
} from "../src/mpf/confirmed-ledger-verification.js";
import {
  checkLedgerRoot,
  exportLedgerDivergenceSeries,
  LEDGER_DIVERGENCE_CHECKS,
  type LedgerDivergenceCheck,
} from "../src/mpf/local-ledger-divergence.js";
import { Globals } from "../src/services/globals.globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { takeCommitWorkerOutput } from "../src/workers/commit-worker-message.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import {
  sampleValue,
  startPrometheusExporter,
} from "./prometheus-scrape.fixture.js";

const COUNTER = "midgard_local_ledger_divergence_total";
const BASE = "dd".repeat(28);

let exporter: Awaited<ReturnType<typeof startPrometheusExporter>>;
beforeAll(async () => {
  exporter = await startPrometheusExporter();
});
afterAll(async () => {
  await exporter.stop();
});

/** Each check's exported count, as one scrape reads them. */
const exported = async () => {
  const text = await exporter.scrape();
  return Object.fromEntries(
    LEDGER_DIVERGENCE_CHECKS.map((check) => [
      check,
      sampleValue(text, COUNTER, { check }),
    ]),
  ) as Record<LedgerDivergenceCheck, number | undefined>;
};

const counts = (
  values: Partial<Record<LedgerDivergenceCheck, number>> = {},
): Record<LedgerDivergenceCheck, number> => ({
  landed_parent: 0,
  own_parent: 0,
  commit_base_landed: 0,
  commit_base_confirmed_ledger: 0,
  confirmed_ledger: 0,
  confirmed_ledger_tail: 0,
  ...values,
});

const globals = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));

describe("the local ledger divergence counter", { concurrent: false }, () => {
  it("exports every check's series at 0 from startup", async () => {
    await Effect.runPromise(exportLedgerDivergenceSeries);
    expect(await exported()).toEqual(counts());
  });

  it("counts a commit worker's refusal on a diverged base once, in the main thread", async () => {
    const node = await globals();
    const refused: WorkerOutput = {
      type: "AwaitingCommitBaseOutput",
      baseHeaderHash: BASE,
      detail: `local ledger divergence (commit_base_confirmed_ledger): the ledger at ${BASE} diverges`,
      divergence: "commit_base_confirmed_ledger",
    };
    expect(takeCommitWorkerOutput(node, refused, 16)).toEqual(refused);
    expect(await exported()).toEqual(
      counts({ commit_base_confirmed_ledger: 1 }),
    );

    // A wait on the base is no divergence.
    takeCommitWorkerOutput(
      node,
      {
        type: "AwaitingCommitBaseOutput",
        baseHeaderHash: BASE,
        detail: "the commit base is not processed yet",
      },
      16,
    );
    expect(await exported()).toEqual(
      counts({ commit_base_confirmed_ledger: 1 }),
    );
  });

  it("counts nothing for the check the worker runs, so an in-process build is counted once too", async () => {
    const before = await exported();
    const result = await Effect.runPromise(
      Effect.either(
        checkLedgerRoot({
          check: "commit_base_landed",
          headerHash: BASE,
          root: "ab".repeat(32),
          derivedRoot: "cd".repeat(32),
        }),
      ),
    );
    expect(Either.isLeft(result)).toBe(true);
    expect(await exported()).toEqual(before);
  });

  it("counts each divergence the confirmed_ledger verification records once, names the frontier's and clears the verified frontier until a pass", async () => {
    const node = await globals();
    const frontier = { headerHash: BASE, utxosRoot: "ab".repeat(32) };
    const tail = { headerHash: "c1".repeat(28), utxosRoot: "ac".repeat(32) };
    const reasons = () =>
      Effect.runPromise(activeLivenessReasons(node)).then((active) =>
        active.map(({ source, reason }) => ({ source, reason })),
      );
    await Effect.runPromise(
      recordConfirmedLedgerVerification(node, {
        kind: "verified",
        frontier,
        generation: "1",
        tail: frontier,
        tailDigests: new Map(),
        entries: 1,
        durationMs: 1,
      }),
    );
    expect(
      await Effect.runPromise(Ref.get(node.VERIFIED_CONFIRMED_LEDGER)),
    ).toEqual({ ...frontier, generation: "1", tailDigests: new Map() });
    const before = await exported();
    await Effect.runPromise(
      recordConfirmedLedgerVerification(node, {
        kind: "diverged",
        frontier,
        generation: "1",
        tail,
        entries: 1,
        durationMs: 1,
        divergences: [
          {
            check: "confirmed_ledger",
            headerHash: BASE,
            root: frontier.utxosRoot,
            derivedRoot: "cd".repeat(32),
          },
          {
            check: "confirmed_ledger_tail",
            headerHash: tail.headerHash,
            root: tail.utxosRoot,
            derivedRoot: "ce".repeat(32),
          },
        ],
      }),
    );
    expect(await exported()).toEqual({
      ...before,
      confirmed_ledger: (before.confirmed_ledger ?? 0) + 1,
      confirmed_ledger_tail: (before.confirmed_ledger_tail ?? 0) + 1,
    });
    expect(await reasons()).toEqual([
      {
        source: CONFIRMED_LEDGER_VERIFICATION_SOURCE,
        reason: "commit_base_ledger_divergence:confirmed_ledger",
      },
    ]);
    expect(
      await Effect.runPromise(Ref.get(node.VERIFIED_CONFIRMED_LEDGER)),
    ).toBeUndefined();

    await Effect.runPromise(
      recordConfirmedLedgerVerification(node, {
        kind: "verified",
        frontier,
        generation: "1",
        tail: frontier,
        tailDigests: new Map(),
        entries: 1,
        durationMs: 1,
      }),
    );
    expect(await reasons()).toEqual([]);
    expect(
      await Effect.runPromise(Ref.get(node.VERIFIED_CONFIRMED_LEDGER)),
    ).toEqual({ ...frontier, generation: "1", tailDigests: new Map() });
  });

  it("counts a divergence from a run below the recorded generation, raising nothing and leaving the record", async () => {
    const node = await globals();
    const frontier = { headerHash: BASE, utxosRoot: "ab".repeat(32) };
    const tail = { headerHash: "c1".repeat(28), utxosRoot: "ac".repeat(32) };
    const verified = {
      kind: "verified",
      frontier,
      generation: "12",
      tail: frontier,
      tailDigests: new Map(),
      entries: 1,
      durationMs: 1,
    } as const;
    await Effect.runPromise(recordConfirmedLedgerVerification(node, verified));
    const before = await exported();
    // A run that read generation 9 (before a fold or another write moved
    // it to 12) still found a ledger that does not derive its root.
    await Effect.runPromise(
      recordConfirmedLedgerVerification(node, {
        kind: "diverged",
        frontier,
        generation: "9",
        tail,
        entries: 1,
        durationMs: 1,
        divergences: [
          {
            check: "confirmed_ledger",
            headerHash: BASE,
            root: frontier.utxosRoot,
            derivedRoot: "cd".repeat(32),
          },
          {
            check: "confirmed_ledger_tail",
            headerHash: tail.headerHash,
            root: tail.utxosRoot,
            derivedRoot: "ce".repeat(32),
          },
        ],
      }),
    );
    expect(await exported()).toEqual({
      ...before,
      confirmed_ledger: (before.confirmed_ledger ?? 0) + 1,
      confirmed_ledger_tail: (before.confirmed_ledger_tail ?? 0) + 1,
    });
    expect(
      (await Effect.runPromise(activeLivenessReasons(node))).filter(
        ({ source }) => source === CONFIRMED_LEDGER_VERIFICATION_SOURCE,
      ),
    ).toEqual([]);
    expect(
      await Effect.runPromise(Ref.get(node.VERIFIED_CONFIRMED_LEDGER)),
    ).toEqual({ ...frontier, generation: "12", tailDigests: new Map() });
  });
});

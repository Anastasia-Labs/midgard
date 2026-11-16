/**
 * The main thread's verification of `confirmed_ledger` on the node
 * database, as the node runs it (`confirmedLedgerVerificationFiber`, its
 * roots derived in its worker thread): a pass records the frontier commits
 * on a landed tail build from; a `confirmed_ledger` row changed outside a
 * fold is caught by the periodic re-check, which counts it under
 * `confirmed_ledger`, names it on `/readyz` and clears the recorded
 * frontier; once the row is restored, the next re-check records the
 * frontier again and clears the reason. The run covers the processed landed
 * deltas to the queue tail too: a produced output changed in a landed row
 * is caught the same way under `confirmed_ledger_tail`, named at the tail,
 * and a changed `confirmed_ledger` row the tail row spends is still caught
 * at the frontier. A run that fails before it
 * passes or diverges names `confirmed_ledger_verification_failed` and the
 * next run that completes clears it.
 */
import { SqlClient } from "@effect/sql";
import { Duration, Effect, Fiber, Metric, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import {
  COMMIT_BASE_LEDGER_DIVERGENCE,
  CONFIRMED_LEDGER_VERIFICATION_FAILED,
} from "../src/fibers/block-commitment.worker-readiness.js";
import { deriveConfirmedLedgerRoots } from "../src/mpf/confirmed-ledger-roots.js";
import {
  CONFIRMED_LEDGER_VERIFICATION_SOURCE,
  confirmedLedgerVerificationFiber,
} from "../src/mpf/confirmed-ledger-verification.js";
import { localLedgerDivergenceCounter } from "../src/mpf/local-ledger-divergence.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import {
  E0,
  E1,
  fixture,
  FRONTIER_HASH,
  rootOf,
  run,
  seed,
} from "./commit-base-resolver.fixture.js";
import { simOutput } from "./helpers/landed-blocks-sim.universe.js";

const REASON = `${COMMIT_BASE_LEDGER_DIVERGENCE}:confirmed_ledger`;
const TAIL_REASON = `${COMMIT_BASE_LEDGER_DIVERGENCE}:confirmed_ledger_tail`;

const confirmedLedgerCount = Metric.value(
  Metric.tagged(localLedgerDivergenceCounter, "check", "confirmed_ledger"),
).pipe(Effect.map((state) => state.count));

/** `confirmed_ledger`'s one row now holds `output`, outside any fold. */
const overwriteRow = (output: Buffer) =>
  withFollowerWrite(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE confirmed_ledger SET output = ${output}
        WHERE outref = ${E0.outref}`;
    }),
  );

/** The landed row `headerHash` now produces `output` in place of `E1`,
 * outside any processing. */
const overwriteProduced = (headerHash: string, output: Buffer) =>
  withFollowerWrite(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE node_landed_blocks
        SET produced_outputs = ARRAY[${output}]::bytea[]
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`;
    }),
  );

/** The verification's raised reasons and the frontier it recorded, read
 * from `Globals`. */
const observe = Effect.gen(function* () {
  const globals = yield* Globals;
  return {
    verified: Ref.get(globals.VERIFIED_CONFIRMED_LEDGER),
    reasons: activeLivenessReasons(globals).pipe(
      Effect.map((active) =>
        active.filter(
          (each) => each.source === CONFIRMED_LEDGER_VERIFICATION_SOURCE,
        ),
      ),
    ),
  };
});

/** Polls `probe` until it holds, failing the test after `withinMs`. */
const eventually = <R>(
  probe: Effect.Effect<boolean, never, R>,
  withinMs = 60_000,
) =>
  probe.pipe(
    Effect.repeat({
      until: (held) => held,
      schedule: Schedule.spaced(Duration.millis(50)),
    }),
    Effect.timeoutFail({
      duration: Duration.millis(withinMs),
      onTimeout: () => new Error("the condition never held"),
    }),
  );

describe("confirmed_ledger verification", { concurrent: false }, () => {
  it("records the verified frontier, catches a row changed outside a fold on the re-check, and clears once it is restored", async () => {
    const r0 = await rootOf([E0]);
    await seed([E0], { headerHash: FRONTIER_HASH, utxosRoot: r0 });
    await run(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const verified = Ref.get(globals.VERIFIED_CONFIRMED_LEDGER);
        const reasons = activeLivenessReasons(globals).pipe(
          Effect.map((active) =>
            active.filter(
              (each) => each.source === CONFIRMED_LEDGER_VERIFICATION_SOURCE,
            ),
          ),
        );
        const before = yield* confirmedLedgerCount;
        const fiber = yield* Effect.fork(
          confirmedLedgerVerificationFiber({
            watchIntervalMs: 50,
            recheckIntervalMs: 200,
          }),
        );

        // The startup run records the frontier.
        yield* eventually(
          Effect.map(
            verified,
            (pair) =>
              pair?.headerHash === FRONTIER_HASH && pair.utxosRoot === r0,
          ),
        );
        expect(yield* reasons).toEqual([]);

        // A row changed outside a fold: the frontier does not move, the
        // re-check catches it.
        yield* overwriteRow(simOutput(9_000_000n));
        yield* eventually(
          Effect.map(reasons, (active) =>
            active.some((each) => each.reason === REASON),
          ),
        );
        expect(yield* verified).toBeUndefined();
        expect((yield* confirmedLedgerCount) - before).toBeGreaterThanOrEqual(
          1,
        );
        const [raised] = yield* reasons;
        expect(raised?.detail).toContain(FRONTIER_HASH);

        // Restored, the next re-check records the frontier again.
        yield* overwriteRow(E0.output);
        yield* eventually(
          Effect.map(
            verified,
            (pair) =>
              pair?.headerHash === FRONTIER_HASH && pair.utxosRoot === r0,
          ),
        );
        expect(yield* reasons).toEqual([]);
        yield* Fiber.interrupt(fiber);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);
  it("catches a produced output changed in a landed row on the re-check, naming the tail, and clears once it is restored", async () => {
    const { headerHash, r0 } = await fixture({ frontier: true, row: {} });
    await run(
      Effect.gen(function* () {
        const { verified, reasons } = yield* observe;
        const atFrontier = Effect.map(
          verified,
          (pair) => pair?.headerHash === FRONTIER_HASH && pair.utxosRoot === r0,
        );
        const fiber = yield* Effect.fork(
          confirmedLedgerVerificationFiber({
            watchIntervalMs: 50,
            recheckIntervalMs: 200,
          }),
        );

        yield* eventually(atFrontier);
        expect(yield* reasons).toEqual([]);

        // `confirmed_ledger` and the frontier are untouched: only the
        // tail's stored delta changed.
        yield* overwriteProduced(headerHash, simOutput(9_000_000n));
        yield* eventually(
          Effect.map(reasons, (active) =>
            active.some((each) => each.reason === TAIL_REASON),
          ),
        );
        expect(yield* verified).toBeUndefined();
        const [raised] = yield* reasons;
        expect(raised?.detail).toContain(headerHash);

        yield* overwriteProduced(headerHash, E1.output);
        yield* eventually(atFrontier);
        expect(yield* reasons).toEqual([]);
        yield* Fiber.interrupt(fiber);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);

  it("catches a confirmed_ledger entry changed outside a fold that the tail row spends, naming the frontier, and clears once it is restored", async () => {
    const { r0 } = await fixture({ frontier: true, row: {} });
    await run(
      Effect.gen(function* () {
        const { verified, reasons } = yield* observe;
        const atFrontier = Effect.map(
          verified,
          (pair) => pair?.headerHash === FRONTIER_HASH && pair.utxosRoot === r0,
        );
        const before = yield* confirmedLedgerCount;
        const fiber = yield* Effect.fork(
          confirmedLedgerVerificationFiber({
            watchIntervalMs: 50,
            recheckIntervalMs: 200,
          }),
        );

        yield* eventually(atFrontier);
        expect(yield* reasons).toEqual([]);

        // The tail row spends `E0`, so the tail's ledger no longer holds
        // it: only the frontier's root covers the changed bytes.
        yield* overwriteRow(simOutput(9_000_000n));
        yield* eventually(
          Effect.map(reasons, (active) =>
            active.some((each) => each.reason === REASON),
          ),
        );
        expect(yield* verified).toBeUndefined();
        expect((yield* confirmedLedgerCount) - before).toBeGreaterThanOrEqual(
          1,
        );
        const [raised] = yield* reasons;
        expect(raised?.detail).toContain(FRONTIER_HASH);

        yield* overwriteRow(E0.output);
        yield* eventually(atFrontier);
        expect(yield* reasons).toEqual([]);
        yield* Fiber.interrupt(fiber);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);

  it("names a run that fails before it passes, and clears it on the next run that completes", async () => {
    const r0 = await rootOf([E0]);
    await seed([E0], { headerHash: FRONTIER_HASH, utxosRoot: r0 });
    let failing = true;
    await run(
      Effect.gen(function* () {
        const { verified, reasons } = yield* observe;
        const fiber = yield* Effect.fork(
          confirmedLedgerVerificationFiber({
            watchIntervalMs: 50,
            recheckIntervalMs: 200,
            deriveRoots: (input) =>
              failing
                ? Effect.fail(new Error("the root worker ran out of heap"))
                : deriveConfirmedLedgerRoots(input),
          }),
        );

        yield* eventually(
          Effect.map(reasons, (active) =>
            active.some(
              (each) => each.reason === CONFIRMED_LEDGER_VERIFICATION_FAILED,
            ),
          ),
        );
        expect(yield* verified).toBeUndefined();
        const [raised] = yield* reasons;
        expect(raised?.detail).toContain("the root worker ran out of heap");

        failing = false;
        yield* eventually(
          Effect.map(
            verified,
            (pair) =>
              pair?.headerHash === FRONTIER_HASH && pair.utxosRoot === r0,
          ),
        );
        expect(yield* reasons).toEqual([]);
        yield* Fiber.interrupt(fiber);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);
});

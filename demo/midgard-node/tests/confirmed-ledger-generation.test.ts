/**
 * The confirmed-ledger generation on the node database (migration 0038): a
 * sequence owned by no table, drawn by every frontier write and by every
 * statement that writes `confirmed_ledger`, whatever writes it, so a
 * verification names exactly the ledger it read. A commit built from the
 * last verification waits once `confirmed_ledger` is written outside a
 * fold, even at the same frontier and root; a pass from a run during which
 * the frontier was unfolded and folded again never verifies the refolded
 * ledger; a clean fold landed processing commits advances the
 * verification, so the commit on it builds with no wait; and a fold
 * committed while a run reads is replayed onto the run's pass when it is
 * recorded, so the commit on it builds with no wait too.
 */
import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Either, Fiber, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { landedLedger, onOneSnapshot } from "../src/landed-blocks/ledger.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import {
  Frontier,
  markApplied,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { advanceVerifiedConfirmedLedger } from "../src/mpf/confirmed-ledger-advance.js";
import {
  type DeriveConfirmedLedgerRoots,
  deriveConfirmedLedgerRoots,
} from "../src/mpf/confirmed-ledger-roots.js";
import { verifyConfirmedLedger } from "../src/mpf/confirmed-ledger-run.js";
import { recordConfirmedLedgerVerification } from "../src/mpf/confirmed-ledger-verification.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.globals.js";
import {
  CommitBaseUnverifiedError,
  verifiedLandedLedgerAt,
} from "../src/workers/commit-block-header.landed-commit-base.js";
import {
  parseVerifiedFrontier,
  verifiedFrontierWorkerInput,
} from "../src/workers/utils/commit-block-header.js";
import {
  E0,
  expectUnverified,
  fixture,
  FRONTIER_HASH,
  resolve,
  run,
  verifiedNow,
} from "./commit-base-resolver.fixture.js";
import {
  confirmedAt,
  fixture as ownFixture,
  harness,
  inNode,
  ports,
  queueOf,
} from "./landed-blocks-own.fixture.js";
import { resetApplicationTables } from "./utils.js";

const generation = Frontier.retrieveWithGeneration.pipe(
  Effect.map((frontier) => BigInt(frontier!.generation)),
);

/** The sequence's last drawn value. */
const drawn = Effect.flatMap(SqlClient.SqlClient, (sql) =>
  sql<{ last_value: string }>`
    SELECT last_value FROM node_confirmed_ledger_generation`.pipe(
    Effect.map(([row]) => BigInt(row!.last_value)),
  ),
);

/** Runs `statement` on `confirmed_ledger` outside any fold. */
const direct = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => withFollowerWrite(Effect.flatMap(SqlClient.SqlClient, statement));

/** What a commit worker resolves at `headerHash` from the record in
 * `globals`, on the landed ledger as it reads it. */
const commitAt = (
  globals: Pick<Globals, "VERIFIED_CONFIRMED_LEDGER">,
  headerHash: string,
) =>
  Effect.gen(function* () {
    const posted = verifiedFrontierWorkerInput(
      yield* Ref.get(globals.VERIFIED_CONFIRMED_LEDGER),
    );
    const verified = yield* parseVerifiedFrontier(posted.verifiedFrontier);
    const landed = yield* onOneSnapshot(
      Effect.flatMap(retrieveRows, landedLedger),
    );
    return yield* Effect.either(
      verifiedLandedLedgerAt(landed!, headerHash, verified),
    );
  });

describe("the confirmed-ledger generation", { concurrent: false }, () => {
  it("draws a new generation on every direct SQL write to confirmed_ledger, and the commit built from the last verification waits", async () => {
    const { tail, headerHash, r1 } = await fixture({ frontier: true, row: {} });
    const passed = await verifiedNow();
    expect(passed).toBeDefined();
    const seen = [await run(generation)];
    for (const statement of [
      // Each leaves `confirmed_ledger` as it was: the generation names the
      // writes, not the content.
      (sql: SqlClient.SqlClient) =>
        sql`UPDATE confirmed_ledger SET output = output`,
      (sql: SqlClient.SqlClient) =>
        sql`DELETE FROM confirmed_ledger WHERE false`,
      (sql: SqlClient.SqlClient) =>
        sql`INSERT INTO confirmed_ledger (tx_id, outref, output, address)
          SELECT tx_id, outref, output, address FROM confirmed_ledger
          ON CONFLICT (outref) DO NOTHING`,
      (sql: SqlClient.SqlClient) =>
        Effect.zipRight(
          sql`CREATE TEMP TABLE kept ON COMMIT DROP AS SELECT * FROM confirmed_ledger`,
          Effect.zipRight(
            sql`TRUNCATE confirmed_ledger`,
            sql`INSERT INTO confirmed_ledger SELECT * FROM kept`,
          ),
        ),
    ]) {
      await run(direct(statement));
      seen.push(await run(generation));
      expect(seen.at(-1)! > seen.at(-2)!).toBe(true);
      // The frontier and root are the ones verified; the generation is not.
      expectUnverified(await resolve(tail, r1, undefined, passed!), headerHash);
    }
    // A run now verifies the ledger at its new generation.
    const outcome = await resolve(tail, r1);
    expect(Either.getOrThrow(outcome).source).toBe(`landed:${headerHash}`);
  }, 120_000);

  it("keeps drawing upward across a reset of the application tables", async () => {
    await fixture({ frontier: true });
    const before = await run(generation);
    await run(resetApplicationTables);
    await fixture({ frontier: true });
    expect((await run(generation)) > before).toBe(true);
  }, 60_000);

  it("writes no frontier on a write to a ledger with none, and the frontier written next draws above every earlier generation", async () => {
    const { r0 } = await fixture({ frontier: false });
    const before = await run(drawn);
    await run(
      direct(
        (sql) => sql`DELETE FROM confirmed_ledger WHERE outref = ${E0.outref}`,
      ),
    );
    expect(await run(Frontier.retrieveWithGeneration)).toBeUndefined();
    await run(
      withFollowerWrite(
        Frontier.upsert({ headerHash: FRONTIER_HASH, utxosRoot: r0 }),
      ),
    );
    expect((await run(generation)) > before).toBe(true);
  }, 60_000);

  it("records no stale pass from a run during which the frontier was unfolded and folded again", async () => {
    const { a, genesisState, journalA } = await ownFixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const unmerged = queueOf(genesisState, [a]);
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const advancing: LandedBlockPorts<never> = {
          ...ports(state),
          folded: (fold) => advanceVerifiedConfirmedLedger(globals, fold),
        };
        yield* processLandedQueue(advancing, unmerged);
        yield* markApplied([a.hash]);
        yield* processLandedQueue(advancing, merged);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        const read = yield* generation;

        // A run takes its snapshot at `a`, then waits as its roots session
        // starts, before it reads `confirmed_ledger` on that snapshot.
        const started = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const gated: DeriveConfirmedLedgerRoots = (delta) =>
          Effect.zipRight(
            Effect.zipRight(
              Deferred.succeed(started, undefined),
              Deferred.await(release),
            ),
            deriveConfirmedLedgerRoots(delta),
          );
        const inFlight = yield* Effect.fork(verifyConfirmedLedger(gated));
        yield* Deferred.await(started);

        // The merge rolls back and lands again: `a` is unfolded and folded
        // again, the same header and root.
        yield* processLandedQueue(advancing, unmerged);
        expect((yield* Frontier.retrieve)?.headerHash).not.toBe(a.hash);
        yield* processLandedQueue(advancing, merged);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        const refolded = yield* generation;
        expect(refolded > read).toBe(true);

        yield* Deferred.succeed(release, undefined);
        const stale = yield* Fiber.join(inFlight);
        expect(stale).toMatchObject({
          kind: "verified",
          frontier: { headerHash: a.hash },
          generation: read.toString(),
        });
        yield* recordConfirmedLedgerVerification(globals, stale);
        const waited = yield* commitAt(globals, a.hash);
        expect(Either.isLeft(waited)).toBe(true);
        expect((waited as Either.Left<unknown, unknown>).left).toBeInstanceOf(
          CommitBaseUnverifiedError,
        );

        // A run on the refolded ledger verifies it, and the stale pass
        // recorded after it is ignored.
        const fresh = yield* verifyConfirmedLedger(deriveConfirmedLedgerRoots);
        yield* recordConfirmedLedgerVerification(globals, fresh);
        yield* recordConfirmedLedgerVerification(globals, stale);
        expect(yield* Ref.get(globals.VERIFIED_CONFIRMED_LEDGER)).toMatchObject(
          { headerHash: a.hash, generation: refolded.toString() },
        );
        expect(Either.isRight(yield* commitAt(globals, a.hash))).toBe(true);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);

  it("advances the verification on a clean fold landed processing commits, so the commit on it builds with no wait", async () => {
    const { a, genesisState, journalA } = await ownFixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const advancing: LandedBlockPorts<never> = {
          ...ports(state),
          folded: (fold) => advanceVerifiedConfirmedLedger(globals, fold),
        };
        yield* processLandedQueue(advancing, queueOf(genesisState, [a]));
        yield* markApplied([a.hash]);
        // A full run at the genesis frontier checks `a`'s delta to the tail.
        const pass = yield* verifyConfirmedLedger(deriveConfirmedLedgerRoots);
        expect(pass.kind).toBe("verified");
        yield* recordConfirmedLedgerVerification(globals, pass);

        // The merge lands: the fold of `a` advances the verification.
        yield* processLandedQueue(advancing, queueOf(confirmedAt(a, ""), []));
        const frontier = yield* Frontier.retrieveWithGeneration;
        expect(frontier?.headerHash).toBe(a.hash);
        expect(yield* Ref.get(globals.VERIFIED_CONFIRMED_LEDGER)).toMatchObject(
          {
            headerHash: a.hash,
            utxosRoot: a.header.utxosRoot,
            generation: frontier!.generation,
          },
        );
        const built = yield* commitAt(globals, a.hash);
        expect(Either.getOrThrow(built)?.root).toBe(a.header.utxosRoot);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);

  it("replays a fold committed while a run reads onto the run's pass, so the commit on it builds with no wait", async () => {
    const { a, genesisState, journalA } = await ownFixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const advancing: LandedBlockPorts<never> = {
          ...ports(state),
          folded: (fold) => advanceVerifiedConfirmedLedger(globals, fold),
        };
        yield* processLandedQueue(advancing, queueOf(genesisState, [a]));
        yield* markApplied([a.hash]);

        // A run takes its snapshot at the genesis frontier, then waits as
        // its roots session starts.
        const started = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const gated: DeriveConfirmedLedgerRoots = (delta) =>
          Effect.zipRight(
            Effect.zipRight(
              Deferred.succeed(started, undefined),
              Deferred.await(release),
            ),
            deriveConfirmedLedgerRoots(delta),
          );
        const inFlight = yield* Effect.fork(verifyConfirmedLedger(gated));
        yield* Deferred.await(started);

        // The merge lands while it reads: the fold of `a` has no record to
        // advance, and is logged.
        yield* processLandedQueue(advancing, queueOf(confirmedAt(a, ""), []));
        expect(
          yield* Ref.get(globals.VERIFIED_CONFIRMED_LEDGER),
        ).toBeUndefined();
        const frontier = yield* Frontier.retrieveWithGeneration;
        expect(frontier?.headerHash).toBe(a.hash);

        yield* Deferred.succeed(release, undefined);
        const pass = yield* Fiber.join(inFlight);
        expect(pass).toMatchObject({
          kind: "verified",
          tail: { headerHash: a.hash },
        });
        expect(
          pass.kind === "verified" && BigInt(pass.generation),
        ).toBeLessThan(BigInt(frontier!.generation));

        // Recorded, the pass is replayed over the fold to `a`.
        yield* recordConfirmedLedgerVerification(globals, pass);
        expect(yield* Ref.get(globals.VERIFIED_CONFIRMED_LEDGER)).toMatchObject(
          {
            headerHash: a.hash,
            utxosRoot: a.header.utxosRoot,
            generation: frontier!.generation,
          },
        );
        const built = yield* commitAt(globals, a.hash);
        expect(Either.getOrThrow(built)?.root).toBe(a.header.utxosRoot);
      }).pipe(Effect.provide(Globals.Default)),
    );
  }, 120_000);
});

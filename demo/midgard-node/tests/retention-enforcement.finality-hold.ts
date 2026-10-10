import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { DaPayloadsDB } from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { retentionSweepAction } from "../src/fibers/retention-sweeper.js";
import { NodeConfig } from "../src/services/index.js";
import {
  dbEnabled,
  NOW,
  seedPayload,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import {
  FINAL_THROUGH,
  NOT_FINAL,
  prune,
  remainingHashes,
  seedPublished,
  seedQueueTerminal,
  withSweepServices,
} from "./retention-enforcement.terminal-merge.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const OLD = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);

/** The sweeper's deadline gauge, read back by its metric key. */
const deadlineGauge = Metric.gauge(
  "da_payload_retention_deadline_remaining_ms",
  {
    description:
      "Milliseconds remaining before the oldest still-challengeable retained DA payload reaches its retention deadline",
  },
);

const hex = (bytes: Buffer): string => bytes.toString("hex");

/**
 * Final merge F, then a successor S whose merge landed but is not final. At
 * its tip L1 lists S as the confirmed head and neither header as queued,
 * while a reader at finality still sees F as the confirmed head and S queued
 * on it.
 */
const seedSupersededHead = Effect.gen(function* () {
  const final = yield* seedPublished(OLD, 1);
  const successor = yield* seedPayload("successor", OLD, OLD);
  yield* seedQueueTerminal(successor, "merged", 2, NOT_FINAL);
  return { final: final.headerHash, successor };
});

/** The successor's merge reached finality: its row is at a final height. */
const finalizeSuccessor = (successor: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE node_l1_queue_terminals SET height = 2
      WHERE header_hash = ${successor}`;
  });

/** A tip view in which L1 lists `confirmed` as its head and nothing queued,
 * final through `finalThroughHeight` (null: no final height). */
const tipView = (
  confirmed: Buffer,
  finalThroughHeight: number | null = FINAL_THROUGH,
): DaPayloadsDB.RetentionL1View => ({
  confirmedHeadHash: confirmed,
  liveQueueHeaderHashes: [],
  ...(finalThroughHeight === null ? {} : { finalThroughHeight }),
});

describe.skipIf(!dbEnabled)("DA payload retention held to L1 finality", () => {
  beforeAll(async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        MigrationRunner.migrate({
          appVersion: "test",
          actor: "retention-enforcement-finality-hold.test",
        }).pipe(Effect.asVoid),
      ) as Effect.Effect<void, never, never>,
    );
  }, 120_000);

  const clearAll = resetApplicationTables;

  const run = <A>(
    effect: Effect.Effect<A, unknown, SqlClient.SqlClient | NodeConfig>,
  ) =>
    Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* clearAll;
          return yield* effect.pipe(Effect.ensuring(Effect.orDie(clearAll)));
        }),
      ) as Effect.Effect<A, never, never>,
    );

  it("holds the final head past the horizon while its successor's merge is not final", async () => {
    const outcome = await run(
      Effect.gen(function* () {
        const seeded = yield* seedSupersededHead;
        const deleted = yield* prune({ view: tipView(seeded.successor) });
        return { seeded, deleted, remaining: yield* remainingHashes };
      }),
    );
    expect(outcome.deleted).toBe(0);
    expect(outcome.remaining).toEqual(
      [hex(outcome.seeded.final), hex(outcome.seeded.successor)].sort(),
    );
  });

  it("prunes the superseded head once its successor's merge is final", async () => {
    const outcome = await run(
      Effect.gen(function* () {
        const seeded = yield* seedSupersededHead;
        yield* finalizeSuccessor(seeded.successor);
        const deleted = yield* prune({ view: tipView(seeded.successor) });
        return { seeded, deleted, remaining: yield* remainingHashes };
      }),
    );
    expect(outcome.deleted).toBe(1);
    expect(outcome.remaining).toEqual([hex(outcome.seeded.successor)]);
  });

  it("holds a header a not-yet-final transition took out of the queue until a later merge is final", async () => {
    const outcome = await run(
      Effect.gen(function* () {
        const final = yield* seedPublished(OLD, 1);
        const taken = yield* seedPayload("taken", OLD, OLD);
        // The tip view lists neither header: only the hold keeps them.
        const view = tipView(deterministicFixtureBytes("tip-head", 28));
        yield* seedQueueTerminal(taken, "merged", 2, NOT_FINAL);
        const held = yield* prune({ view });
        const heldRemaining = yield* remainingHashes;
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE node_l1_queue_terminals SET height = 2
          WHERE header_hash = ${taken}`;
        yield* seedQueueTerminal(
          deterministicFixtureBytes("later", 28),
          "merged",
          3,
        );
        return {
          expected: [hex(final.headerHash), hex(taken)].sort(),
          held,
          heldRemaining,
          released: yield* prune({ view }),
          remaining: yield* remainingHashes,
        };
      }),
    );
    expect(outcome.held).toBe(0);
    expect(outcome.heldRemaining).toEqual(outcome.expected);
    expect(outcome.released).toBe(2);
    expect(outcome.remaining).toEqual([]);
  });

  it("holds every terminal header while the view carries no final height", async () => {
    const outcome = await run(
      Effect.gen(function* () {
        const seeded = yield* seedSupersededHead;
        yield* finalizeSuccessor(seeded.successor);
        const deleted = yield* prune({
          view: tipView(deterministicFixtureBytes("other-head", 28), null),
        });
        return { deleted, remaining: yield* remainingHashes, seeded };
      }),
    );
    expect(outcome.deleted).toBe(0);
    expect(outcome.remaining).toEqual(
      [hex(outcome.seeded.final), hex(outcome.seeded.successor)].sort(),
    );
  });

  it("holds nothing without a verified deployment identity", async () => {
    const deleted = await run(
      Effect.gen(function* () {
        yield* seedSupersededHead;
        return yield* prune({
          view: tipView(deterministicFixtureBytes("other-head", 28)),
          digest: undefined,
        });
      }),
    );
    expect(deleted).toBe(2);
  });

  it("leaves held payloads out of the retention deadline", async () => {
    const freshEnd = new Date(
      NOW.getTime() - MIDGARD_RETENTION_WINDOW.requiredRetentionMs / 2,
    );
    const remainingMs = await run(
      Effect.gen(function* () {
        const seeded = yield* seedSupersededHead;
        yield* seedPayload("fresh", freshEnd, freshEnd);
        yield* withSweepServices(
          retentionSweepAction(tipView(seeded.successor), NOW),
          0,
        );
        return (yield* Metric.value(deadlineGauge)).value;
      }),
    );
    expect(remainingMs).toBe(
      freshEnd.getTime() +
        MIDGARD_RETENTION_WINDOW.requiredRetentionMs -
        NOW.getTime(),
    );
  });
});

import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { retentionSweepAction } from "../src/fibers/retention-sweeper.js";
import { NodeConfig } from "../src/services/index.js";
import { createDatabaseStateQueueCorrectionObserverStore } from "../src/services/state-queue-correction-observer.js";
import { makeState } from "../src/services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import {
  dbEnabled,
  deploymentManifest,
  NOW,
  seedPayload,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import {
  prune,
  remainingHashes,
  seedPublished,
  terminalMerge,
  withSweepServices,
} from "./retention-enforcement.terminal-merge.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

const OLD = new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);

/** One block short of the manifest's L1 finality depth. */
const notFinal = (): bigint =>
  BigInt(deploymentManifest.l1Finality.confirmationDepth) - 1n;

/** The sweeper's deadline gauge, read back by its metric key. */
const deadlineGauge = Metric.gauge(
  "da_payload_retention_deadline_remaining_ms",
  {
    description:
      "Milliseconds remaining before the oldest still-challengeable retained DA payload reaches its retention deadline",
  },
);

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** Persists the correction observer state through the production store. */
const saveObserver = (
  admitted: readonly SDK.StateQueueAuthenticatedTransition[],
  pending: readonly SDK.StateQueueAuthenticatedTransition[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const store = createDatabaseStateQueueCorrectionObserverStore({
      sql,
      deploymentManifest,
    });
    const cursor = (admitted.at(-1) ?? pending.at(-1))!.nextQueue;
    yield* Effect.promise(() =>
      store.save(
        makeState({
          schemaVersion: "midgard-node-state-queue-correction-observer-v1",
          deploymentIdentityDigest: deploymentManifest.manifestId,
          stateQueuePolicyId:
            deploymentManifest.contracts.stateQueueMint.scriptHash,
          cursorQueue: cursor,
          pending,
          admitted,
          retractedTransactionHashes: [],
          postFinalityRollbackIncidents: [],
        }),
      ),
    );
  });

/**
 * Final merge F, then a successor S whose merge L1 already shows but which is
 * not yet final. At its tip L1 lists S as the confirmed head and neither
 * header as queued, while a reader at release finality still sees F as the
 * confirmed head and S queued on it.
 */
const seedSupersededHead = Effect.gen(function* () {
  const final = yield* seedPublished(OLD, 1);
  const successor = yield* seedPayload("successor", OLD, OLD);
  const successorMerge = terminalMerge(successor, 2, notFinal());
  yield* saveObserver([final.transition], [successorMerge]);
  return { final: final.headerHash, successor };
});

/** The successor's merge reached the finality depth. */
const finalizeSuccessor = (successor: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ readonly transition_record: unknown }>`
      SELECT transition_record FROM da_payload_terminal_outcomes`;
    const record = row!.transition_record;
    const final = (
      typeof record === "string" ? JSON.parse(record) : record
    ) as SDK.StateQueueAuthenticatedTransition;
    yield* saveObserver([final, terminalMerge(successor, 2)], []);
  });

/** A tip view in which L1 lists `confirmed` as its head and nothing queued. */
const tipView = (confirmed: Buffer): DaPayloadsDB.RetentionL1View => ({
  confirmedHeadHash: confirmed,
  liveQueueHeaderHashes: [],
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

  const clearAll = Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM state_queue_terminal_observer_states`;
    yield* DaPayloadTerminalOutcomesDB.clear;
    yield* DaPayloadsDB.clear;
  });

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
        yield* saveObserver(
          [final.transition],
          [terminalMerge(taken, 2, notFinal())],
        );
        const held = yield* prune({ view });
        const heldRemaining = yield* remainingHashes;
        yield* saveObserver(
          [
            final.transition,
            terminalMerge(taken, 2),
            terminalMerge(deterministicFixtureBytes("later", 28), 3),
          ],
          [],
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

  it("still prunes when a pending transition carries a malformed removed header", async () => {
    const outcome = await run(
      Effect.gen(function* () {
        const seeded = yield* seedSupersededHead;
        yield* finalizeSuccessor(seeded.successor);
        const sql = yield* SqlClient.SqlClient;
        yield* sql`
          UPDATE state_queue_terminal_observer_states SET state_record = jsonb_set(
            CASE jsonb_typeof(state_record)
              WHEN 'string' THEN (state_record #>> '{}')::jsonb
              ELSE state_record
            END,
            '{pending}',
            '[{"removedHeaderHashes": ["not-a-header-hash"]}]'::jsonb)`;
        const deleted = yield* prune({ view: tipView(seeded.successor) });
        return { seeded, deleted, remaining: yield* remainingHashes };
      }),
    );
    expect(outcome.deleted).toBe(1);
    expect(outcome.remaining).toEqual([hex(outcome.seeded.successor)]);
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

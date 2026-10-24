import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { DaPayloadsDB } from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import {
  deploymentManifest,
  NOW,
  seedPayload,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import {
  remainingHashes,
  seedPublished,
  seedQueueTerminal,
} from "./retention-enforcement.terminal-merge.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const clear = resetApplicationTables;

const run = <A>(work: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      clear.pipe(Effect.zipRight(work), Effect.ensuring(Effect.orDie(clear))),
    ) as Effect.Effect<A, never>,
  );

const identity = () => Buffer.from(deploymentManifest.manifestId, "hex");
const old = () => new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY);

/** Prunes at `NOW` under a view final through `finalThroughHeight`. */
const pruneAt = (finalThroughHeight: number) =>
  DaPayloadsDB.pruneBeyondRetention({
    challengeableCutoff: computeChallengeableCutoff(NOW),
    view: {
      confirmedHeadHash: deterministicFixtureBytes("terminal-finality", 28),
      liveQueueHeaderHashes: [],
      finalThroughHeight,
    },
    deploymentIdentityDigest: identity(),
  });

describe.skipIf(process.env.MIDGARD_SKIP_DB_TESTS === "1")(
  "DA retirement waits for the terminal tx to be final",
  () => {
    beforeAll(async () => {
      await Effect.runPromise(
        provideDatabaseLayers(
          MigrationRunner.migrate({
            appVersion: "test",
            actor: "da-terminal-finality",
          }),
        ) as Effect.Effect<unknown, never>,
      );
    }, 120_000);

    it("keeps a superseded merge's bytes until the later merge is final, inclusive", async () => {
      const result = await run(
        Effect.gen(function* () {
          yield* seedPublished(old(), 1, 10);
          const successor = yield* seedPublished(old(), 2, 20);
          // At 19 the successor's merge can still roll back: the first header
          // is the boundary a reader at finality sees.
          const held = yield* pruneAt(19);
          const released = yield* pruneAt(20);
          return {
            held,
            released,
            successor,
            remaining: yield* remainingHashes,
          };
        }),
      );
      expect({ held: result.held, released: result.released }).toEqual({
        held: 0,
        released: 1,
      });
      expect(result.remaining).toEqual([
        result.successor.headerHash.toString("hex"),
      ]);
    });

    it("retains a removal inside the time horizon until its tx is final, inclusive", async () => {
      const result = await run(
        Effect.gen(function* () {
          const hash = yield* seedPayload("removed-finality", NOW, NOW);
          yield* seedQueueTerminal(hash, "removed", 1, 30);
          const deleted: number[] = [];
          for (const through of [0, 29, 30])
            deleted.push(yield* pruneAt(through));
          return deleted;
        }),
      );
      expect(result).toEqual([0, 0, 1]);
    });

    it("re-reads the facts: a rollback that deletes the removal row keeps the bytes", async () => {
      const result = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const hash = yield* seedPayload("removal-rolled-back", NOW, NOW);
          yield* seedQueueTerminal(hash, "removed", 1, 30);
          // The follower's rewind deletes the row of a rolled-back tx.
          yield* sql`DELETE FROM node_l1_queue_terminals WHERE header_hash = ${hash}`;
          return {
            deleted: yield* pruneAt(30),
            remaining: yield* remainingHashes,
            hash: hash.toString("hex"),
          };
        }),
      );
      expect(result.deleted).toBe(0);
      expect(result.remaining).toEqual([result.hash]);
    });
  },
);

/**
 * The commit anchor (plan §8.1) on Postgres: the anchor block a view reads,
 * the journal that stores it, the signing step that keeps the commit only
 * while the anchor is on the follower's chain, and the anchor rule across
 * the follower's prune boundary.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type CommitAnchor,
  commitAnchorCanonical,
  readCommitAnchorBlock,
} from "../src/database/commit-anchor.js";
import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { COMMIT_ANCHOR_NOT_CANONICAL_MESSAGE } from "../src/database/pendingBlockFinalizations.assert-canonical-event-members.js";
import {
  FollowerWrite,
  type FollowerWritePermit,
} from "../src/services/follower-write-gate.js";
import { assertCommitUserEventSourceCompleteness } from "../src/workers/commit-block-header/submission.commit-event-sources.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import {
  FOLLOWER_GENERATION,
  followerBlockHash,
  modelSlotTime,
  writeFollowerTip,
} from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { journalFixture } from "./local-mutation-job-abandonment.journal-fixture.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

const D = 2;
const FORK = FOLLOWER_GENERATION + 1;

/** Model chain block `height` at slot 10 * height, of `generation`. */
const block = (height: number, generation: number = FOLLOWER_GENERATION) => ({
  slot: 10 * height + (generation === FOLLOWER_GENERATION ? 0 : 5),
  height,
  generation,
});
const viewOf = (b: ReturnType<typeof block>): View => ({
  generation: b.generation,
  point: { slot: b.slot, hash: followerBlockHash(b.slot, b.generation) },
  height: b.height,
});
const anchorOf = (b: ReturnType<typeof block>): CommitAnchor => ({
  hash: followerBlockHash(b.slot, b.generation),
  height: b.height,
  slot: b.slot,
});

/** The follower followed heights `from`..`to` of `generation`. */
const follow = (from: number, to: number, generation = FOLLOWER_GENERATION) =>
  Effect.gen(function* () {
    for (let height = from; height <= to; height += 1) {
      const b = block(height, generation);
      yield* writeFollowerTip(b.slot, generation, height);
    }
  });

/** A follower rewind to `height`, then a fork of `FORK` blocks up to `to`. */
const rewindAndFork = (height: number, to: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM l1_blocks WHERE height > ${height}`;
    yield* follow(height + 1, to, FORK);
  });

/** One signed transaction (its key is random per call): its bytes and body hash. */
const signedTx = () => {
  const cbor = Buffer.from(makeCardanoSignedMapOutputTxBytes());
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  const body = tx.body();
  const hash = CML.hash_transaction(body);
  const txHash = Buffer.from(hash.to_hex(), "hex");
  hash.free();
  body.free();
  tx.free();
  return { cbor, txHash };
};

/** A commit journaled under `permit` with the anchor d below its view. */
const journalUnder = (
  permit: FollowerWritePermit,
  headerHash: Buffer,
  txHash: Buffer,
) =>
  Effect.gen(function* () {
    const anchor = anchorOf(block(permit.view.height - D));
    const prepared =
      yield* PendingBlockFinalizationsDB.preparePendingSubmission(
        { ...journalFixture(headerHash), preparedTxHash: txHash },
        {
          beforeJournalInsert: assertCommitUserEventSourceCompleteness({
            blockEndTimeMs:
              modelSlotTime(anchor.slot) + EVENT_WAIT_DURATION_MS - 1,
            commitAnchor: anchor,
            depth: D,
            slotToUnixTime: modelSlotTime,
            includedDepositEntries: [],
            includedForcedTransactionEntries: [],
            includedWithdrawalEntries: [],
          }),
        },
      ).pipe(Effect.provideService(FollowerWrite, permit));
    return { prepared, anchor };
  });

const sign = (
  permit: FollowerWritePermit,
  headerHash: Buffer,
  { cbor, txHash }: ReturnType<typeof signedTx>,
) =>
  run(
    Effect.either(
      PendingBlockFinalizationsDB.recordSignedIntent(
        headerHash,
        txHash,
        cbor,
      ).pipe(Effect.provideService(FollowerWrite, permit)),
    ),
  );

const header = (label: string) =>
  deterministicFixtureBytes(`commit-anchor:${label}`, 28);

describe("commit anchor block", () => {
  it("is the block d below the view, unavailable below the origin or once the view left the chain", async () => {
    const read = (view: View, depth: number) =>
      run(readCommitAnchorBlock({ view, depth }));
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* follow(50, 60);
      }),
    );
    const view = viewOf(block(60));
    expect(await read(view, D)).toEqual({
      kind: "anchored",
      anchor: anchorOf(block(58)),
    });
    expect(await read(view, 0)).toEqual({
      kind: "anchored",
      anchor: anchorOf(block(60)),
    });
    expect((await read(view, 11)).kind).toBe("unavailable");
    expect(await read(view, 11)).toMatchObject({ reason: "outside_history" });
    await run(rewindAndFork(55, 61));
    expect(await read(view, D)).toMatchObject({ reason: "view_gone" });
    expect(await read(viewOf(block(61, FORK)), D)).toEqual({
      kind: "anchored",
      anchor: anchorOf(block(59, FORK)),
    });
  });
});

describe("commit anchor at journaling and signing", () => {
  it("stores the anchor with the journal", async () => {
    const permit = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* follow(50, 60);
        return yield* openFollowerWriteGateAt(viewOf(block(60)));
      }),
    );
    const headerHash = header("stored");
    const { anchor } = await run(
      journalUnder(permit, headerHash, signedTx().txHash),
    );
    const rows = await run(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql<{
          commit_anchor_hash: Buffer;
          commit_anchor_height: string;
          commit_anchor_slot: string;
        }>`SELECT commit_anchor_hash, commit_anchor_height::text AS commit_anchor_height,
            commit_anchor_slot::text AS commit_anchor_slot
          FROM pending_block_finalizations WHERE header_hash = ${headerHash}`,
      ),
    );
    expect(rows).toHaveLength(1);
    expect(Buffer.from(rows[0]!.commit_anchor_hash).equals(anchor.hash)).toBe(
      true,
    );
    expect(Number(rows[0]!.commit_anchor_height)).toBe(anchor.height);
    expect(Number(rows[0]!.commit_anchor_slot)).toBe(anchor.slot);
  });

  it("refuses to journal a commit without an anchor under a permit", async () => {
    const permit = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* follow(50, 60);
        return yield* openFollowerWriteGateAt(viewOf(block(60)));
      }),
    );
    const refused = await run(
      Effect.either(
        PendingBlockFinalizationsDB.preparePendingSubmission({
          ...journalFixture(header("anchorless")),
          preparedTxHash: signedTx().txHash,
        }).pipe(Effect.provideService(FollowerWrite, permit)),
      ),
    );
    expect(Either.isLeft(refused)).toBe(true);
    expect(String(Either.isLeft(refused) ? refused.left.message : "")).toMatch(
      /requires a commit anchor/,
    );
  });

  it.each([
    ["below the anchor", 57, false],
    ["at the anchor", 58, true],
    ["above the anchor", 59, true],
  ] as const)(
    "signs after a rewind %s only while the anchor stays on the chain",
    async (_, rewindTo, signs) => {
      const permit = await run(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          yield* follow(50, 60);
          return yield* openFollowerWriteGateAt(viewOf(block(60)));
        }),
      );
      const headerHash = header(`sign-${rewindTo.toString()}`);
      const tx = signedTx();
      await run(journalUnder(permit, headerHash, tx.txHash));
      // The driver applies the forked chain's view; a new permit there.
      const after = await run(
        Effect.gen(function* () {
          yield* rewindAndFork(rewindTo, 62);
          return yield* openFollowerWriteGateAt(viewOf(block(62, FORK)));
        }),
      );
      const signed = await sign(after, headerHash, tx);
      if (signs) expect(signed).toEqual(Either.right(undefined));
      else {
        expect(Either.isLeft(signed)).toBe(true);
        expect(
          Either.isLeft(signed) ? (signed.left as Error).message : "",
        ).toContain(COMMIT_ANCHOR_NOT_CANONICAL_MESSAGE);
      }
    },
  );
});

describe("commit anchor rule across the prune boundary", () => {
  const canonical = (anchor: CommitAnchor) =>
    run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows = yield* sql<{ canonical: boolean }>`
          SELECT ${commitAnchorCanonical(sql, "a")} AS canonical
          FROM (SELECT ${anchor.hash}::bytea AS commit_anchor_hash,
              ${anchor.height}::bigint AS commit_anchor_height,
              ${anchor.slot}::bigint AS commit_anchor_slot) a`;
        return rows[0]?.canonical;
      }),
    );

  it("holds a pruned anchor canonical only while no other block stands at its height", async () => {
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* follow(50, 60);
      }),
    );
    const kept = anchorOf(block(53));
    expect(await canonical(kept)).toBe(true);
    expect(await canonical(anchorOf(block(53, FORK)))).toBe(false);
    // The follower prunes through block 54: block 53 is gone.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM l1_blocks WHERE height < 55`;
        yield* sql`UPDATE l1_follower_cursor SET pruned_through_slot = ${block(54).slot}`;
      }),
    );
    expect(await canonical(kept)).toBe(true);
    // A pruned height says nothing about which block stood there; a stored
    // block at the height does.
    await run(
      Effect.flatMap(
        SqlClient.SqlClient,
        (
          sql,
        ) => sql`INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count)
          VALUES (${block(53, FORK).slot}, ${followerBlockHash(block(53, FORK).slot, FORK)}, 53, NULL, 0)`,
      ),
    );
    expect(await canonical(kept)).toBe(false);
    // Above the prune boundary only the stored block counts.
    expect(await canonical(anchorOf(block(56, FORK)))).toBe(false);
  });
});

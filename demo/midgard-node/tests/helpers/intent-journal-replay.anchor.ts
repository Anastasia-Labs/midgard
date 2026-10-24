/**
 * A replayed commit's anchor on the replay's chain (`replayJournaledOnFollower`).
 * The flow journaled its commit anchor as a block of the chain its follower
 * followed (`follower-emulator.chain.ts`). The replay applies its own blocks,
 * one per emulator height, so the same block has another hash and slot
 * there. The anchor is rebound to the replay's block at the emulator height
 * of the flow's anchor, so the commit predicate judges it on the replay's
 * chain as the flow's follower did on its own.
 */
import type { SqlClient } from "@effect/sql";
import type { ManagedRuntime } from "effect";

/** The emulator height of a flow block, by hash (hex); `undefined` when the flow's chain has none. */
export type AnchorEmulatorHeight = (anchorHash: string) => number | undefined;

/**
 * Rebinds the anchor of the pending block `header` to the replay's block at
 * the flow anchor's emulator height, which the replay has applied: the
 * anchor lies below the view the commit was planned at, and the replay
 * records it at the block before its landing.
 */
export const rebindCommitAnchor = async ({
  runtime,
  sql,
  header,
  emulatorHeightOf,
}: {
  readonly runtime: ManagedRuntime.ManagedRuntime<SqlClient.SqlClient, unknown>;
  readonly sql: SqlClient.SqlClient;
  readonly header: Buffer;
  readonly emulatorHeightOf: AnchorEmulatorHeight;
}): Promise<void> => {
  const [row] = await runtime.runPromise(
    sql<{ readonly hash: Buffer | null }>`SELECT commit_anchor_hash AS hash
      FROM pending_block_finalizations WHERE header_hash = ${header}`,
  );
  if (row?.hash == null) return;
  const anchor = row.hash.toString("hex");
  const height = emulatorHeightOf(anchor);
  if (height === undefined)
    throw new Error(
      `commit ${header.toString("hex")}: its anchor ${anchor} is not a block of the flow's followed chain`,
    );
  const [block] = await runtime.runPromise(
    sql<{ readonly hash: Buffer; readonly slot: string }>`SELECT hash,
        slot::text AS slot
      FROM l1_blocks WHERE height = ${height}`,
  );
  if (block === undefined)
    throw new Error(
      `commit ${header.toString("hex")}: the replay has no block at its anchor's emulator height ${height.toString()}`,
    );
  await runtime.runPromise(
    sql`UPDATE pending_block_finalizations
      SET commit_anchor_hash = ${block.hash}, commit_anchor_height = ${height},
        commit_anchor_slot = ${Number(block.slot)}
      WHERE header_hash = ${header}`,
  );
};

/**
 * The emulator heights of a followed chain's blocks (`FollowedChain`), its
 * origin one below the first block's.
 */
export const followedEmulatorHeights = (chain: {
  readonly origin: Readonly<{ hash: string }>;
  readonly blocks: ReadonlyArray<
    Readonly<{ hash: string; emulatorHeight: number }>
  >;
}): AnchorEmulatorHeight => {
  const heights = new Map(
    chain.blocks.map(({ hash, emulatorHeight }) => [hash, emulatorHeight]),
  );
  const first = chain.blocks[0];
  if (first !== undefined)
    heights.set(chain.origin.hash, first.emulatorHeight - 1);
  return (anchorHash) => heights.get(anchorHash);
};

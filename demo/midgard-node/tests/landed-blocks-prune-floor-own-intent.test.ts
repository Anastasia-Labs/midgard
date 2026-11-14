/**
 * The landed frontier's prune floor (`prune-floor.ts`) keeps the commit
 * intent of an own block that landed but is not processed yet: the
 * follower prunes an intent once it was terminal (here, landed) for k
 * blocks, and while the node lags, the floor holds the prune boundary back
 * to where the frontier's header entered the queue, below the own block's
 * landing. So the node never reads that block's journal dead because its
 * intent row is gone (`deadJournals`). Without the floor, the same prune
 * deletes the intent.
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms.
 */
import type {
  FactStore,
  FollowerProjection,
  OutRef,
} from "@al-ft/midgard-l1-follower";
import { intentJournalProjection } from "@al-ft/midgard-l1-follower";
import type { SimOutput } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { stateQueueProjection } from "../src/l1-state-queue/index.js";
import { landedFrontierPruneFloor } from "../src/landed-blocks/index.js";
import { db } from "./helpers/forced-orders-node-store.js";
import { insertCommitIntent } from "./helpers/own-block-facts.js";
import {
  NODE_PREFIX,
  nodeDatum,
  queueOutput,
  ROOT_ASSET,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";
import { followChain, K } from "./landed-blocks-replay-foreign.fixture.js";
import {
  benchSlotOf,
  originSlot,
  startFor,
} from "./landed-blocks-replay-foreign.window.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];

beforeEach(async () => {
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
  await db(resetApplicationTables);
});

/** The header the node's confirmed ledger holds (the queue root's). */
const FRONTIER = "fb".repeat(28);

/**
 * The queue root at the frontier, then this node's own block appended
 * after it and left unprocessed, its commit journaled as an intent; then
 * `K + 3` more blocks and one prune step, with the landed frontier's floor
 * on the prune or not. Returns whether the intent row is left.
 */
const pruneOverUnprocessedOwnBlock = async (floorInForce: boolean) => {
  const floor = landedFrontierPruneFloor({
    config: SIM_QUEUE_CONFIG,
    needs: () => Promise.resolve({ frontier: FRONTIER, processed: new Set() }),
    slotOf: benchSlotOf,
  });
  const projection: FollowerProjection = {
    ...stateQueueProjection(SIM_QUEUE_CONFIG),
    pruneFloor: {
      name: floor.name,
      floor: (context) =>
        floorInForce ? floor.floor(context) : Promise.resolve(null),
    },
  };
  const { store, chain } = await followChain({}, [
    projection,
    intentJournalProjection,
  ]);
  opened.push(store);
  const send = async (inputs: OutRef[], outputs: SimOutput[]) => {
    const [txHash] = await chain.forward([
      { inputs, outputs, nonce: chain.chain.nonce() },
    ]);
    return txHash!;
  };
  const advance = async (blocks: number) => {
    for (let block = 0; block < blocks; block++) await chain.forward([]);
  };
  await advance(1);
  const origin = await originSlot(store);
  await advance(4);
  const root = (link: string | null) =>
    queueOutput(ROOT_ASSET, rootDatum(FRONTIER, link, startFor(origin + 2)));
  const rootTx = await send([chain.chain.outsideInput()], [root(null)]);
  const header: SDK.Header = {
    ...simHeader(1, FRONTIER),
    startTime: startFor(origin + 2),
    endTime: startFor(origin + 4),
  };
  const headerHash = SDK.stateQueueHeaderHash(header);
  const commit = await send(
    [{ txHash: rootTx, index: 0 }],
    [
      root(headerHash),
      queueOutput(
        NODE_PREFIX + headerHash,
        nodeDatum(header, "Unattested", null),
      ),
    ],
  );
  const landedAt = chain.tip.point.slot;
  await db(
    insertCommitIntent(
      Buffer.from(headerHash, "hex"),
      Buffer.from(commit),
      Buffer.from("own-commit-cbor"),
    ),
  );
  await advance(K + 3);
  const pruned = await store.prune();
  if (!("prunedThroughSlot" in pruned))
    throw new Error(`prune: ${JSON.stringify(pruned)}`);
  const intents = await db(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ n: number }>`SELECT COUNT(*)::int AS n FROM l1_intents
        WHERE tx_hash = ${Buffer.from(commit)}`,
    ),
  );
  return {
    landedAt,
    prunedThroughSlot: pruned.prunedThroughSlot,
    intentKept: intents[0]?.n === 1,
  };
};

describe("an own block that landed and is not processed yet", () => {
  it("keeps its commit intent through a prune while the landed frontier's floor holds", async () => {
    const result = await pruneOverUnprocessedOwnBlock(true);
    expect(result.prunedThroughSlot).toBeLessThan(result.landedAt);
    expect(result.intentKept).toBe(true);
  });

  it("loses its commit intent to the same prune without the floor", async () => {
    const result = await pruneOverUnprocessedOwnBlock(false);
    expect(result.prunedThroughSlot).toBeGreaterThanOrEqual(result.landedAt);
    expect(result.intentKept).toBe(false);
  });
});

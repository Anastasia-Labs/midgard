/**
 * The production foreign replayer judges a block's transactions at its block
 * time (its state-queue node's `block_time`), whose slot the header carries,
 * not at its end: a block committed after its end imports at its block time,
 * and at its end its header's slot disagrees.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { afterEach, beforeEach, expect, it } from "vitest";

import { rebind } from "./foreign-block-import.fixture.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  clock,
  emptyBlock,
  followChain,
  inNode,
  inputFor,
  replayerFor,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

it("judges a block committed after its end at its block time's slot", async () => {
  const { store } = await followChain();
  opened.push(store);
  const empty = await emptyBlock();
  // A catch-up block: it covers (1, 2] and was committed at 5,999 ms, whose
  // slot (5) its header carries.
  const block = await rebind({
    ...empty,
    block_body: {
      ...empty.block_body,
      header: { ...empty.block_body.header, blockSlot: 5n },
    },
  });
  await retain(await retainedRow(block));
  const replayAt = async (blockTime: bigint) =>
    inNode(
      replayerFor(store)({ ...(await inputFor(store, block)), blockTime }),
    );
  expect(await replayAt(5_999n)).toMatchObject({ kind: "replayed" });
  // Judged at its end's slot (0) instead, the header's slot disagrees.
  expect(await replayAt(block.block_body.header.endTime)).toMatchObject({
    kind: "invalid",
    detail: expect.stringContaining("validation context differs"),
  });
});

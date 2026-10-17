/**
 * A foreign block's fetched DA payload is retained under its body's
 * validation-trace pair: the header pins the pair to the empty tree, and the
 * DA table requires the row's count to equal its forced and L2 counts.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";
import { afterEach, beforeEach, expect, it, vi } from "vitest";

import { foreignDaFetchMemo } from "../src/da/foreign-retained-da.js";
import { DaPayloadsDB } from "../src/database/index.js";
import { payloadOf } from "../src/landed-blocks/replay-foreign.payload.js";
import { computeDaPayloadRoots } from "../src/workers/commit-block-header/da-payload.compute-da-payload-roots.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  forcedAndL2Block,
  inNode,
  retain,
  retained,
} from "./landed-blocks-replay-foreign.fixture.js";
import { serveDa } from "./landed-blocks-replay-foreign.transport.js";
import { resetApplicationTables } from "./utils.js";

beforeEach(async () => {
  await db(resetApplicationTables);
});
afterEach(() => {
  vi.restoreAllMocks();
});

it("retains a fetched block with forced and L2 events under its body's validation-trace pair", async () => {
  const block = await forcedAndL2Block();
  const { header, header_hash: headerHash } = block.block_body;
  expect(header.forcedTransactionCount).toBe(1n);
  expect(header.l2TransactionCount).toBe(1n);
  expect(header.validationTracesRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  expect(header.validationTraceCount).toBe(0n);
  serveDa(() => block);
  const { acquired } = await inNode(
    payloadOf(
      foreignDaFetchMemo(),
      new Map(),
      headerHash,
      header,
      header.endTime,
    ),
  );
  if (acquired === undefined) throw new Error("the payload was not fetched");
  await retain(acquired);
  const kept = await retained(block);
  if (Option.isNone(kept)) throw new Error("the payload was not retained");
  const { validationTracesRoot } = await Effect.runPromise(
    computeDaPayloadRoots(block),
  );
  expect(validationTracesRoot).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  expect(kept.value[DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]).toBe(
    validationTracesRoot,
  );
  expect(kept.value[DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]).toBe(2n);
});

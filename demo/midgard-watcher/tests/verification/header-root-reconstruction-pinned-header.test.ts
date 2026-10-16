import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  buildFixture,
  commitMutatedHeader,
  evaluateFixture,
} from "./header-root-reconstruction.build-fixture.js";
import {
  bufferEntries,
  corpusTransaction,
  type Fixture,
} from "./header-root-reconstruction.watcher-header-record.js";

/** A block with two L2 transactions, so its body carries two traces. */
const builtFixture = async (): Promise<Fixture> =>
  await buildFixture({
    transactions: [corpusTransaction(0), corpusTransaction(1)],
  });

/**
 * Commits the header the state-queue validator requires: the validation-trace
 * pair is the empty root and a zero count, while the payload body keeps its
 * traces and declares their number.
 */
const commitPair = async (
  fixture: Fixture,
  pair: Pick<SDK.Header, "validationTracesRoot" | "validationTraceCount">,
  fields: Partial<SDK.Header> = {},
): Promise<Fixture> =>
  await commitMutatedHeader(fixture, (header) => ({
    ...header,
    ...pair,
    ...fields,
  }));

/** The root of the traces the payload body carries. */
const builtRoot = async (fixture: Fixture): Promise<string> =>
  (
    await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      bufferEntries(fixture.payload.block_body.validation_traces),
    )
  ).root;

const PINNED = {
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTraceCount: 0n,
} as const;

describe("W22 reconstruction of a header with a pinned validation-trace pair", () => {
  it("accepts an honest pinned header of a block with L2 transactions", async () => {
    const built = await builtFixture();
    const fixture = await commitPair(built, PINNED);
    expect(
      fixture.header.forcedTransactionCount + fixture.header.l2TransactionCount,
    ).toBeGreaterThan(0n);
    expect(fixture.payload.block_body.counts.validationTraceCount).toBe(2n);
    expect(fixture.payload.block_body.validation_traces).toHaveLength(2);
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "accept",
      reasonCodes: [],
      rootMismatches: [],
      countMismatches: [],
    });
    expect(result.reconstructedRoots).toMatchObject({
      validation_traces_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    expect(result.reconstructedCounts).toMatchObject({
      l2_transaction_count: "2",
      validation_trace_count: "0",
    });
  });

  it("rejects a header that commits the empty root with the built count", async () => {
    const built = await builtFixture();
    const fixture = await commitPair(built, {
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTraceCount: 2n,
    });
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["count_mismatch"],
      rootMismatches: [],
      countMismatches: ["validation_trace_count"],
    });
  });

  it.each([
    [
      "the built root and count",
      (root: string) => ({
        validationTracesRoot: root,
        validationTraceCount: 2n,
      }),
    ],
    [
      "a built root with a zero count",
      (root: string) => ({
        validationTracesRoot: root,
        validationTraceCount: 0n,
      }),
    ],
  ])("rejects a header that commits %s", async (_label, pair) => {
    const built = await builtFixture();
    const root = await builtRoot(built);
    expect(root).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    const result = await evaluateFixture(await commitPair(built, pair(root)));
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["root_mismatch"],
      rootMismatches: ["validation_traces_root"],
      countMismatches: [],
    });
  });

  it("still rejects a pinned header whose transition_step_count differs from the payload", async () => {
    const fixture = await commitPair(await builtFixture(), PINNED, {
      transitionStepCount: 1n,
    });
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["count_mismatch"],
      rootMismatches: [],
      countMismatches: ["transition_step_count"],
    });
  });

  it("still rejects a pinned header whose deposits_root differs from the payload", async () => {
    const fixture = await commitPair(await builtFixture(), PINNED, {
      depositsRoot: h32(0x5a),
    });
    const result = await evaluateFixture(fixture);
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["root_mismatch"],
      rootMismatches: ["deposits_root"],
      countMismatches: [],
    });
  });
});

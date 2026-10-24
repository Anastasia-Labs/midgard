import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import {
  buildCountedRoot,
  detectTransitionTraceFaults,
  reconstructDaPayload,
} from "../src/transition-trace/index.js";
import { deriveTransitionTraceReplayEvidence } from "../src/transition-trace/replay.js";
import { corpus } from "./cardano-capability-retained-da.reconstruct-authenticated-canonical-transaction-from-field-chunks.js";
import { buildStrictRetainedDaPairFixture } from "./helpers/cardano-capability-retained-da.js";
import { buildL2ReplayBlock } from "./transition-trace-challenger.build-l2-replay-fixture.js";

type Block = {
  readonly payload: SDK.DaPayload;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.Header;
  readonly headerHash: string;
};

type ValidationTracePair = Pick<
  SDK.Header,
  "validationTracesRoot" | "validationTraceCount"
>;

const pinnedPair: ValidationTracePair = {
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTraceCount: 0n,
};

const recommit = async (
  block: Block,
  {
    fields,
    body = {},
  }: {
    readonly fields?: Partial<SDK.Header>;
    readonly body?: Partial<SDK.DaPayload["block_body"]>;
  },
): Promise<Block> => {
  const header: SDK.Header = { ...block.header, ...fields };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...block.payload,
    block_body: {
      ...block.payload.block_body,
      ...body,
      header_hash: headerHash,
      header,
    },
  };
  return {
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    header,
    headerHash,
  };
};

const reconstruct = (block: Block) =>
  reconstructDaPayload({
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    expectedHeaderHash: block.headerHash,
    committedHeader: block.header,
  });

const retainedBlock = async (): Promise<Block> => {
  const boundary = corpus.entries.find(
    ({ label }) => label === "maximum-list-redeemer-breadth",
  )!;
  return await buildStrictRetainedDaPairFixture({
    canonicalTransactionCbor: Buffer.from(boundary.canonicalCborHex, "hex"),
  });
};

const builtPair = async (block: Block): Promise<ValidationTracePair> => {
  const members = block.payload.block_body.validation_traces;
  const built = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    members.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  return {
    validationTracesRoot: built.root,
    validationTraceCount: built.count,
  };
};

describe("validation-trace pair pinned to the empty tree", () => {
  it("accepts an honest pinned block and keeps its members as body data", async () => {
    const block = await retainedBlock();
    expect(block.header.validationTracesRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(block.header.validationTraceCount).toBe(0n);
    expect(
      block.header.forcedTransactionCount + block.header.l2TransactionCount,
    ).toBeGreaterThan(0n);
    const built = await builtPair(block);
    expect(built.validationTraceCount).toBe(2n);

    const reconstruction = await reconstruct(block);
    expect(reconstruction.roots.validationTracesRoot).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    expect(reconstruction.counts.validationTraceCount).toBe(0n);
    expect(reconstruction.rootData.validationTraces.root).toBe(
      built.validationTracesRoot,
    );
    expect(reconstruction.rootData.validationTraces.count).toBe(
      block.payload.block_body.counts.validationTraceCount,
    );
    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
  });

  it.each([
    ["the built root and count", "built", "built", "rootMismatch"],
    ["the built root with a zero count", "built", "zero", "rootMismatch"],
    ["the empty root with the built count", "empty", "built", "countMismatch"],
  ] as const)(
    "refuses a header committing %s",
    async (_label, root, count, code) => {
      const honest = await retainedBlock();
      const built = await builtPair(honest);
      const block = await recommit(honest, {
        fields: {
          validationTracesRoot:
            root === "built"
              ? built.validationTracesRoot
              : SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount:
            count === "built" ? built.validationTraceCount : 0n,
        },
      });
      await expect(reconstruct(block)).rejects.toMatchObject({ code });
    },
  );

  it("still reports a pinned header whose l2_transaction_count differs from its source root", async () => {
    const honest = await retainedBlock();
    const block = await recommit(honest, {
      fields: { l2TransactionCount: honest.header.l2TransactionCount + 1n },
    });
    const detections = await detectTransitionTraceFaults(
      await reconstruct(block),
    );
    expect(detections.map(({ kind }) => kind)).toContain("countFault");
  });

  it.each([
    ["declares one member more than it carries", "overCount"],
    ["carries one member fewer than it declares", "dropMember"],
  ] as const)("refuses a body that %s", async (_label, tamper) => {
    const honest = await retainedBlock();
    const body = honest.payload.block_body;
    const block = await recommit(honest, {
      body:
        tamper === "overCount"
          ? {
              counts: {
                ...body.counts,
                validationTraceCount: body.counts.validationTraceCount + 1n,
              },
            }
          : { validation_traces: body.validation_traces.slice(1) },
    });
    await expect(reconstruct(block)).rejects.toMatchObject({
      code: "countMismatch",
    });
  });

  // Reconstruction checks the declared count against the traces the body
  // carries, and nothing more: here it declares one, for its L2 transaction,
  // and carries two. That the count covers forced plus L2 transactions is
  // the watcher's shape check, pinned by "rejects a forced-and-L2 body that
  // declares and carries a trace for its L2 transaction only" in
  // midgard-watcher/tests/verification/header-root-reconstruction-pinned-header.test.ts.
  it("refuses a body that declares fewer traces than it carries", async () => {
    const honest = await retainedBlock();
    const body = honest.payload.block_body;
    expect(body.counts.forcedTransactionCount).toBe(1n);
    expect(body.counts.l2TransactionCount).toBe(1n);
    expect(body.validation_traces).toHaveLength(2);
    const block = await recommit(honest, {
      body: {
        counts: {
          ...body.counts,
          validationTraceCount: body.counts.l2TransactionCount,
        },
      },
    });
    await expect(reconstruct(block)).rejects.toMatchObject({
      code: "countMismatch",
    });
  });

  it("replays an honest pinned block with a transaction to no fault", async () => {
    const { block, predecessorEntries } = await buildL2ReplayBlock({
      matchingCommittedRoot: true,
    });
    const pinned = await recommit(block, { fields: pinnedPair });
    expect(pinned.payload.block_body.validation_traces).toHaveLength(1);
    expect(pinned.payload.block_body.transactions).toHaveLength(1);

    const current = await reconstruct(pinned);
    const predecessor = await checkLedgerAgainstRoot({
      entries: predecessorEntries,
      utxosRoot: current.header.prevUtxosRoot,
      label: "predecessor",
    });
    const evidence = await deriveTransitionTraceReplayEvidence({
      current,
      predecessor,
      deposits: [],
      network: "Custom",
      depositPolicyId: "34".repeat(28),
    });
    expect(evidence.l2TransactionTransitions).toHaveLength(1);
    expect(await detectTransitionTraceFaults(current, evidence)).toEqual([]);

    const unpinned = await recommit(block, { fields: await builtPair(block) });
    await expect(reconstruct(unpinned)).rejects.toMatchObject({
      code: "rootMismatch",
    });
  });
});

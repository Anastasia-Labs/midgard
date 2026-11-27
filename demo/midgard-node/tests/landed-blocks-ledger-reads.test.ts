/**
 * The trie nodes the node's ledger store reads to take in one landed block
 * (open its parent's ledger, apply the block, check the root, keep the
 * ledger after it), for an own block checked against its journal and for a
 * foreign block replayed by the production replayer, on parent ledgers of
 * 1,000 and of 50,000 UTxOs. A block touches a few paths of the trie, so
 * the count grows with the trie's depth, never with the parent ledger.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "@al-ft/midgard-fault-proofs/block-replay";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type LedgerStorage,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import { replayValidationVerdictEvent } from "@al-ft/midgard-validation/validation-machine";
import { Effect, Runtime } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import type * as Ledger from "../src/database/utils/ledger.js";
import { LANDED_BLOCK_REBASE_PENDING } from "../src/landed-blocks/holds.js";
import { readLedgerOutputs } from "../src/landed-blocks/ledger-outputs.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import type { Database } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";
import {
  confirmedState,
  foreignPorts,
  landedQueue,
  seed,
  spendingBlock,
} from "./landed-blocks-foreign-ledger.fixture.js";
import {
  A1,
  block,
  confirmedAt,
  entry,
  G0,
  harness,
  inNode as inOwnNode,
  ports,
  queueOf,
} from "./landed-blocks-own.fixture.js";
import { attested } from "./landed-blocks-replay-foreign.availability.js";
import {
  followChain,
  inNode,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { resetApplicationTables } from "./utils.js";

const SIZES = [1_000, 50_000] as const;

/** Fifty times the UTxOs adds about one level to a hex trie (16^1.4): the
 * nodes read stay under twice those of the smaller ledger, where a walk of
 * the parent ledger reads at least one node per UTxO. */
const expectDepthBound = (reads: readonly number[]) => {
  expect(reads[0]).toBeGreaterThan(0);
  expect(reads[1]).toBeLessThan(2 * reads[0]!);
};

/** `count` canonical UTxOs no block here touches. */
const filler = (count: number) =>
  Array.from({ length: count }, (_, index) =>
    entry(`reads-filler-${index.toString()}`, 1_000_000n + BigInt(index)),
  );

/** A memory trie storage whose node reads are counted, and a ledger store
 * over it reading outputs with `readOutputs` when given. */
const countedLedgers = (
  readOutputs?: LedgerStorage["readOutputs"],
): { store: LedgerStore; nodesRead: () => number } => {
  const storage = createMemoryLedgerStorage();
  let read = 0;
  const store = createLedgerStore({
    ...storage,
    readNodes: (hashes) => {
      read += hashes.length;
      return storage.readNodes(hashes);
    },
    ...(readOutputs === undefined ? {} : { readOutputs }),
  });
  return { store, nodesRead: () => read };
};

/** Has `store` keep the ledger of `entries`; that ledger. */
const keep = async (
  store: LedgerStore,
  entries: readonly Ledger.MinimalEntry[],
) => {
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(
    entries.map(({ outref, output }) => ({
      type: "insert" as const,
      outRef: outref,
      output,
    })),
  );
  await store.persist(ledger);
  return ledger;
};

/** The root of `ledger` with `spent` removed and `produced` added. */
const rootAfter = async (
  store: LedgerStore,
  root: string,
  spent: readonly Buffer[],
  produced: readonly Ledger.MinimalEntry[],
) =>
  (
    await (
      await store.at(root)
    ).apply([
      ...spent.map((outRef) => ({ type: "delete" as const, outRef })),
      ...produced.map(({ outref, output }) => ({
        type: "insert" as const,
        outRef: outref,
        output,
      })),
    ])
  ).root;

/** Nodes read to process one own block on a parent ledger of `size` UTxOs. */
const ownBlockReads = async (size: number) => {
  const { store, nodesRead } = countedLedgers();
  const parent = await keep(store, [G0, ...filler(size - 1)]);
  const after = await rootAfter(store, parent.root, [G0.outref], [A1]);
  const own = block(
    1,
    { hash: SDK.GENESIS_HEADER_HASH, root: parent.root, endTime: 0n },
    after,
  );
  const state = harness();
  state.ledgers = Promise.resolve(store);
  state.journals.set(own.hash, {
    baseTailHeaderHash: SDK.GENESIS_HEADER_HASH,
    baseUtxosRoot: parent.root,
    expectedUtxosRoot: after,
    spent: [G0.outref],
    produced: [A1],
    depositIds: [],
    withdrawals: [],
    forcedIds: [],
    txIds: [],
  });
  return inOwnNode(
    Effect.gen(function* () {
      yield* withFollowerWrite(
        Frontier.upsert({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: parent.root,
        }),
      );
      const before = nodesRead();
      const held = yield* processLandedQueue(
        ports(state),
        queueOf(confirmedAt("genesis", parent.root), [own]),
      );
      const read = nodesRead() - before;
      expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
      expect((yield* retrieveRows).map((row) => row.state)).toEqual([
        "processed",
      ]);
      return read;
    }),
  );
};

/** What the normal fixture's transfer `small` produces. */
const producedOf = async (
  small: Awaited<ReturnType<typeof spendingBlock>>,
): Promise<Ledger.MinimalEntry[]> => {
  const replay = await Effect.runPromise(
    replayValidationVerdictEvent({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      canonicalTransactionCbor: Buffer.from(
        small.payload.block_body.transaction_preimages[0]![1],
        "hex",
      ),
      ledgerWitnessEntries: [
        { outRef: small.spent.outref, output: small.spent.output },
      ],
      blockTimeMs: 2,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      sourceKind: "normal",
    }),
  );
  return replay.statePatch.upsertedOutRefs.map(([outRef, output]) => ({
    outref: Buffer.from(outRef, "hex"),
    output: Buffer.from(output),
  }));
};

/** The normal fixture's transfer on the parent `parentHash` whose ledger
 * root is `prior`, ending at `post`. */
const transferOn = async (parentHash: string, prior: string, post: string) => {
  const { payload } = await normalFixture();
  const body = payload.block_body;
  // The transfer's one transition step, between the roots of this parent.
  const step = encodeTransitionStepCbor({
    schema_version: 1n,
    step_index: 0n,
    event_key: { L2TransactionEventKey: { tx_id: body.transactions[0]![0] } },
    phase: "L2Transaction",
    pre_utxos_root: prior,
    post_utxos_root: post,
  }).toString("hex");
  const bound: SDKDa.DaPayload = await rebind({
    ...payload,
    block_body: {
      ...body,
      header: {
        ...body.header,
        prevHeaderHash: parentHash,
        prevUtxosRoot: prior,
        utxosRoot: post,
      },
      transition_trace: [
        [encodeTransitionIntegerCbor(0n).toString("hex"), step],
      ],
    },
  });
  return {
    payload: bound,
    hash: bound.block_body.header_hash,
    header: bound.block_body.header,
  };
};

describe("ledger store reads per landed block", () => {
  const opened: FactStore[] = [];
  afterEach(async () => {
    await Promise.all(opened.splice(0).map((store) => store.close()));
  });

  /** Nodes read to process one foreign block on a parent ledger of `size`
   * UTxOs, the block's spent output in `confirmed_ledger`. */
  const foreignBlockReads = async (size: number) => {
    await inNode(resetApplicationTables);
    const { store: follower } = await followChain();
    opened.push(follower);
    const rootHash = "33".repeat(28);
    const small = await spendingBlock(rootHash);
    const measured = inNode(
      Effect.gen(function* () {
        const runtime = yield* Effect.runtime<Database>();
        const { store, nodesRead } = countedLedgers((outRefs) =>
          Runtime.runPromise(runtime)(readLedgerOutputs(outRefs)),
        );
        const parent = yield* Effect.promise(() =>
          keep(store, [small.spent, ...filler(size - 1)]),
        );
        const post = yield* Effect.promise(async () =>
          rootAfter(
            store,
            parent.root,
            [small.spent.outref],
            await producedOf(small),
          ),
        );
        const transfer = yield* Effect.promise(() =>
          transferOn(rootHash, parent.root, post),
        );
        yield* Effect.promise(async () =>
          retain(await retainedRow(transfer.payload)),
        );
        const queue = yield* Effect.promise(async () =>
          landedQueue(
            follower,
            confirmedState(rootHash, parent.root, transfer.header.startTime),
            [{ ...transfer, status: await attested(transfer.payload) }],
          ),
        );
        yield* seed([small.spent], {
          headerHash: rootHash,
          utxosRoot: parent.root,
        });
        const before = nodesRead();
        const held = yield* processLandedQueue(
          foreignPorts(follower, store),
          queue,
        );
        const read = nodesRead() - before;
        expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect((yield* retrieveRows).map((row) => row.state)).toEqual([
          "processed",
        ]);
        return read;
      }),
    );
    try {
      return await measured;
    } finally {
      // The next size follows the chain again under the store's lease.
      opened.splice(opened.indexOf(follower), 1);
      await follower.close();
    }
  };

  it("reads trie nodes in proportion to the trie's depth for an own block", async () => {
    const reads: number[] = [];
    for (const size of SIZES) reads.push(await ownBlockReads(size));
    expectDepthBound(reads);
  }, 600_000);

  it("reads trie nodes in proportion to the trie's depth for a foreign block", async () => {
    const reads: number[] = [];
    for (const size of SIZES) reads.push(await foreignBlockReads(size));
    expectDepthBound(reads);
  }, 600_000);
});

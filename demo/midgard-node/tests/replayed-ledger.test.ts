import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  createReplayedLedger,
  fileLedgerCheckpointStore,
  ledgerAfter,
  ledgerEntriesRoot,
  LedgerRootMismatchError,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerSource,
  singleLedgerAt,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { rebind } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

// The funded ledger the native normal fixture spends from (its parent block's
// post-state); the fixture's header names "11".repeat(28) as that parent.
const parentEntries = (lovelace = 3_000_000n) => {
  const signer = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x17));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(signer.to_public().hash()),
  ).to_address();
  return [
    {
      outRef: makeOutRefCbor(0x11),
      output: Buffer.from(
        makeMidgardTxOutput(
          address,
          CML.Value.from_coin(lovelace),
        ).to_cbor_bytes(),
      ),
    },
  ];
};

const source = (
  payloads: readonly SDK.DaPayload[],
  asked: string[] = [],
): ReplayedLedgerSource => ({
  payloadAt: async (headerHash) => {
    asked.push(headerHash);
    return payloads.find(
      (payload) => payload.block_body.header_hash === headerHash,
    );
  },
  // The fixture blocks carry only an L2 transaction.
  materialFor: async (payload) => ({
    blockTimeMs: Number(payload.block_body.header.endTime),
    replayUserEvent: () => Effect.die("fixture has no user event"),
    verifyForcedSource: () => Effect.die("fixture has no forced order"),
  }),
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
});

const rehashed = async (payload: SDK.DaPayload, header: SDK.Header) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    ...payload,
    block_body: { ...payload.block_body, header, header_hash: headerHash },
  };
};

describe("the watcher's replayed ledger", { timeout: 60_000 }, () => {
  let directory: string;
  let path: string;
  beforeEach(async () => {
    directory = await mkdtemp(join(tmpdir(), "replayed-ledger-"));
    path = join(directory, "checkpoint.json");
  });
  afterEach(async () => {
    await rm(directory, { recursive: true, force: true });
  });

  const fixtureChain = async () => {
    const block = (await normalFixture()).payload;
    const header = block.block_body.header;
    const confirmed = {
      headerHash: header.prevHeaderHash,
      utxosRoot: header.prevUtxosRoot,
    };
    await fileLedgerCheckpointStore(path).write({
      headerHash: confirmed.headerHash,
      entries: parentEntries(),
    });
    return { block, confirmed };
  };

  it("gives the node's root for the block the node imports", async () => {
    const f = await normalFixture();
    const node = await f.imported();
    if (node._tag !== "Right") throw new Error("the node refused the fixture");
    const { confirmed } = await fixtureChain();
    const ledger = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
    });
    expect(await ledger.restore(confirmed)).toBe("restored");
    const entries = await ledger.ledgerAt(source([f.payload]))(
      f.payload.block_body.header_hash,
    );
    expect(entries).toBeDefined();
    const root = await ledgerEntriesRoot(entries!);
    expect(root).toBe(node.right.root);
    expect(root).toBe(f.payload.block_body.header.utxosRoot);
    const pairs = (rows: readonly (readonly [Buffer, Buffer])[]) =>
      rows
        .map(
          ([outRef, output]) =>
            `${outRef.toString("hex")}:${output.toString("hex")}`,
        )
        .sort();
    expect(
      pairs(entries!.map(({ outRef, output }) => [outRef, output])),
    ).toEqual(pairs(node.right.entries.map((row) => [row.outref, row.output])));
  });

  it("resumes from its checkpoint after a restart", async () => {
    const { block, confirmed } = await fixtureChain();
    const headerHash = block.block_body.header_hash;
    const first = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
    });
    expect(await first.restore(confirmed)).toBe("restored");
    // Only the live block is retained: the confirmed one comes from the file.
    const asked: string[] = [];
    const live = source([block], asked);
    const before = await first.ledgerAt(live)(headerHash);
    expect(asked).toEqual([headerHash]);

    // Restart: a new ledger over the same file replays the live queue.
    const second = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
    });
    expect(await second.restore(confirmed)).toBe("restored");
    expect(await second.ledgerAt(live)(headerHash)).toEqual(before);

    // The block merges: the checkpoint moves to it, and a restart then needs
    // no retained payload at all.
    const merged = { headerHash, utxosRoot: block.block_body.header.utxosRoot };
    expect(await second.advance(merged, live)).toBe(true);
    const third = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
    });
    expect(await third.restore(merged)).toBe("restored");
    const none: string[] = [];
    expect(await third.ledgerAt(source([], none))(headerHash)).toEqual(before);
    expect(none).toEqual([]);

    // A checkpoint for an older head is a candidate: admitted against the
    // root its first child binds when the walk reaches it.
    await fileLedgerCheckpointStore(path).write({
      headerHash: confirmed.headerHash,
      entries: parentEntries(),
    });
    const fourth = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
    });
    expect(await fourth.restore(merged)).toBe("candidate");
    expect(await fourth.ledgerAt(live)(headerHash)).toEqual(before);
  });

  it("needs no repair when L1 rolls back an unmerged block", async () => {
    const { block, confirmed } = await fixtureChain();
    const ledger = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
    });
    expect(await ledger.restore(confirmed)).toBe("restored");
    const replaced = await rebind({
      ...block,
      block_body: {
        ...block.block_body,
        header: { ...block.block_body.header, operatorVkey: "33".repeat(28) },
      },
    });
    const rolledBack = block.block_body.header_hash;
    const replacement = replaced.block_body.header_hash;
    expect(replacement).not.toBe(rolledBack);
    const checkpoint = await readFile(path, "utf8");
    await ledger.ledgerAt(source([block]))(rolledBack);
    // After the rollback only the replacement is retained and asked for; it
    // replays from the untouched checkpoint.
    const entries = await ledger.ledgerAt(source([replaced]))(replacement);
    expect(await ledgerEntriesRoot(entries!)).toBe(
      replaced.block_body.header.utxosRoot,
    );
    expect(await readFile(path, "utf8")).toBe(checkpoint);
  });

  it("reports a block whose replay does not open its utxos_root and derives nothing above it", async () => {
    const { block, confirmed } = await fixtureChain();
    const refusals: ReplayedLedgerRefusal[] = [];
    const ledger = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
      onRefusal: (refusal) => refusals.push(refusal),
    });
    expect(await ledger.restore(confirmed)).toBe("restored");
    const wrong = await rehashed(block, {
      ...block.block_body.header,
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    const child = await rehashed(block, {
      ...block.block_body.header,
      prevHeaderHash: wrong.block_body.header_hash,
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    const blocks = source([wrong, child]);
    // The mismatching block's own replayed ledger is served unchecked, so the
    // classifier records the post-ledger mismatch; it is never admitted.
    const served = await ledger.ledgerAt(blocks)(wrong.block_body.header_hash);
    expect(await ledgerEntriesRoot(served!)).toBe(
      block.block_body.header.utxosRoot,
    );
    await expect(
      ledgerAfter({
        ledgerAt: ledger.ledgerAt(blocks),
        headerHash: wrong.block_body.header_hash,
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        label: "post ledger",
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
    expect(
      await ledger.ledgerAt(blocks)(child.block_body.header_hash),
    ).toBeUndefined();
    expect(refusals.map(({ headerHash, kind }) => [headerHash, kind])).toEqual([
      [wrong.block_body.header_hash, "root-mismatch"],
      [wrong.block_body.header_hash, "root-mismatch"],
      [wrong.block_body.header_hash, "root-mismatch"],
    ]);
  });

  it("refuses a predecessor ledger with one wrong entry at the root check", async () => {
    const { block, confirmed } = await fixtureChain();
    // The funded output with one lovelace more: a well-formed ledger that is
    // not the one ConfirmedState commits to.
    const tampered = parentEntries(3_000_001n);
    await expect(
      ledgerAfter({
        ledgerAt: singleLedgerAt(confirmed.headerHash, tampered),
        headerHash: confirmed.headerHash,
        utxosRoot: confirmed.utxosRoot,
        label: "predecessor ledger",
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
    // The same ledger as a persisted checkpoint is refused on load and never
    // used: the block above it then has no ledger (its parent is not retained).
    await fileLedgerCheckpointStore(path).write({
      headerHash: confirmed.headerHash,
      entries: tampered,
    });
    const refusals: ReplayedLedgerRefusal[] = [];
    const ledger = createReplayedLedger({
      checkpoints: fileLedgerCheckpointStore(path),
      onRefusal: (refusal) => refusals.push(refusal),
    });
    expect(await ledger.restore(confirmed)).toBe("refused");
    expect(refusals).toMatchObject([
      { headerHash: confirmed.headerHash, kind: "root-mismatch" },
    ]);
    expect(
      await ledger.ledgerAt(source([block]))(block.block_body.header_hash),
    ).toBeUndefined();
    // So is a corrupt file.
    await writeFile(path, "{");
    expect(await ledger.restore(confirmed)).toBe("refused");
  });
});

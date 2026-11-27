import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  ledgerAfter,
  LedgerRootMismatchError,
} from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  createReplayedLedger,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerSource,
} from "@al-ft/midgard-fault-proofs/replayed-ledger";
import {
  entriesOf,
  entriesRoot,
} from "@al-ft/midgard-fault-proofs/test-support/fixture-ledger";
import { oracleLedgerRoot } from "@al-ft/midgard-fault-proofs/test-support/ledger-root-oracle";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

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
  payloads: readonly SDKDa.DaPayload[],
  asked: string[] = [],
): ReplayedLedgerSource => ({
  headerAt: async (headerHash) =>
    payloads.find((payload) => payload.block_body.header_hash === headerHash)
      ?.block_body.header,
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

const rehashed = async (payload: SDKDa.DaPayload, header: SDK.Header) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    ...payload,
    block_body: { ...payload.block_body, header, header_hash: headerHash },
  };
};

const held = (store: LedgerStore, root: string) =>
  store.at(root).then(
    () => true,
    () => false,
  );

describe("the watcher's replayed ledger", { timeout: 60_000 }, () => {
  /** The fixture block, and a store holding its parent's ledger. */
  const fixtureChain = async () => {
    const block = (await normalFixture()).payload;
    const header = block.block_body.header;
    const confirmed = {
      headerHash: header.prevHeaderHash,
      utxosRoot: header.prevUtxosRoot,
    };
    const store = createLedgerStore(createMemoryLedgerStorage());
    await store.admit(parentEntries(), confirmed.utxosRoot);
    return { block, confirmed, store };
  };

  it("gives the node's root for the block the node imports", async () => {
    const f = await normalFixture();
    const node = await f.imported();
    if (node._tag !== "Right") throw new Error("the node refused the fixture");
    const { store } = await fixtureChain();
    const ledger = createReplayedLedger({ store });
    const entries = await ledger.ledgerAt(source([f.payload]))(
      f.payload.block_body.header_hash,
    );
    expect(entries).toBeDefined();
    const root = await entriesRoot(entries!);
    expect(root).toBe(node.right.after.root);
    expect(root).toBe(f.payload.block_body.header.utxosRoot);
    const pairs = (rows: readonly (readonly [Buffer, Buffer])[]) =>
      rows
        .map(
          ([outRef, output]) =>
            `${outRef.toString("hex")}:${output.toString("hex")}`,
        )
        .sort();
    const imported: (readonly [Buffer, Buffer])[] = [];
    for await (const { outRef, output } of node.right.after.entries())
      imported.push([outRef, output]);
    expect(
      pairs(
        (await entriesOf(entries!)).map(({ outRef, output }) => [
          outRef,
          output,
        ]),
      ),
    ).toEqual(pairs(imported));
  });

  it("answers from its store after a restart", async () => {
    const { block, store } = await fixtureChain();
    const headerHash = block.block_body.header_hash;
    const first = createReplayedLedger({ store });
    const asked: string[] = [];
    const live = source([block], asked);
    const before = await first.ledgerAt(live)(headerHash);
    expect(asked).toEqual([headerHash]);

    // Restart: a new ledger over the same store asks for no payload.
    const second = createReplayedLedger({ store });
    expect(await entriesOf((await second.ledgerAt(live)(headerHash))!)).toEqual(
      await entriesOf(before!),
    );
    expect(asked).toEqual([headerHash]);

    // The block merges: its ledger is held, and a restart then needs no
    // retained payload at all.
    const merged = { headerHash, utxosRoot: block.block_body.header.utxosRoot };
    expect(await second.ensure(merged, live)).toBe(true);
    const third = createReplayedLedger({ store });
    const none: string[] = [];
    const headerOnly: ReplayedLedgerSource = {
      ...source([], none),
      headerAt: live.headerAt,
    };
    expect(
      await entriesOf((await third.ledgerAt(headerOnly)(headerHash))!),
    ).toEqual(await entriesOf(before!));
    expect(none).toEqual([]);
  });

  it("needs no repair when L1 rolls back an unmerged block", async () => {
    const { block, confirmed, store } = await fixtureChain();
    const ledger = createReplayedLedger({ store });
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
    await ledger.ledgerAt(source([block]))(rolledBack);
    // After the rollback only the replacement is retained. It commits the
    // same ledger, which the store holds by root: no payload is asked for.
    const asked: string[] = [];
    const entries = await ledger.ledgerAt(source([replaced], asked))(
      replacement,
    );
    expect(await entriesRoot(entries!)).toBe(
      replaced.block_body.header.utxosRoot,
    );
    expect(asked).toEqual([]);
    expect(await held(store, confirmed.utxosRoot)).toBe(true);
  });

  it("reports a block whose replay does not open its utxos_root and derives nothing above it", async () => {
    const { block, store } = await fixtureChain();
    const refusals: ReplayedLedgerRefusal[] = [];
    const ledger = createReplayedLedger({
      store,
      onRefusal: (refusal) => refusals.push(refusal),
    });
    // A root no ledger the store holds opens.
    const claimed = "ab".repeat(32);
    const wrong = await rehashed(block, {
      ...block.block_body.header,
      utxosRoot: claimed,
    });
    const child = await rehashed(block, {
      ...block.block_body.header,
      prevHeaderHash: wrong.block_body.header_hash,
      prevUtxosRoot: claimed,
    });
    const blocks = source([wrong, child]);
    // The mismatching block's own replayed ledger is served unchecked, so the
    // classifier records the post-ledger mismatch; it is never admitted.
    const served = await ledger.ledgerAt(blocks)(wrong.block_body.header_hash);
    expect(await entriesRoot(served!)).toBe(block.block_body.header.utxosRoot);
    await expect(
      ledgerAfter({
        ledgerAt: ledger.ledgerAt(blocks),
        headerHash: wrong.block_body.header_hash,
        utxosRoot: claimed,
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
        // A source serving the tampered ledger at the root it really has.
        ledgerAt: async () =>
          await createLedgerStore(createMemoryLedgerStorage()).admit(
            tampered,
            await oracleLedgerRoot(tampered),
          ),
        headerHash: confirmed.headerHash,
        utxosRoot: confirmed.utxosRoot,
        label: "predecessor ledger",
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
    // A store never admits it under the committed root, so the block above it
    // then has no ledger (its parent's ledger is not held).
    const store = createLedgerStore(createMemoryLedgerStorage());
    await expect(
      store.admit(tampered, confirmed.utxosRoot),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
    const ledger = createReplayedLedger({ store });
    expect(
      await ledger.ledgerAt(source([block]))(block.block_body.header_hash),
    ).toBeUndefined();
  });
});

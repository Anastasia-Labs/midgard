/**
 * The replayed ledger taking in the confirmed head's ledger by its root
 * (`bootstrap`) instead of replaying up to it, with no verified head or where
 * the walk cannot derive the head's parent ledger: the head becomes the
 * verified head, reported unverified once, and the blocks above it replay
 * over that ledger. Each bootstrap here admits the fixture's own entries,
 * checked against the root.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { type LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
} from "../src/ledger-store/index.js";
import {
  createReplayedLedger,
  memoryVerifiedHead,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerUnverified,
  type ReplayedLedgerVerifiedHead,
} from "../src/replayed-ledger.js";
import { RetainedDaPayloadUnavailableError } from "../src/transition-trace/fetch.retained-da-payload-unavailable.js";
import {
  REPLAYED_LEDGER_CHAIN_PARENT,
  replayedLedgerChain,
  type ReplayedLedgerChainBlock,
  replayedLedgerChainBlockWith,
  replayedLedgerChainSource,
} from "./helpers/replayed-ledger-chain.js";

const head = ({ headerHash, header }: ReplayedLedgerChainBlock) => ({
  headerHash,
  utxosRoot: header.utxosRoot,
});

/** A source serving `blocks`, answering that no peer holds the payloads of
 * `missing`, and recording each payload request in `asked`. */
const sourceWithout = (
  blocks: readonly ReplayedLedgerChainBlock[],
  missing: readonly ReplayedLedgerChainBlock[],
  asked?: string[],
) =>
  replayedLedgerChainSource(blocks, {
    ...(asked === undefined ? {} : { asked }),
    onRequest: (headerHash) => {
      if (missing.some((block) => block.headerHash === headerHash))
        throw new RetainedDaPayloadUnavailableError(
          headerHash,
          `no payload of ${headerHash}`,
          "not_found",
        );
    },
  });

/**
 * A fresh store with no verified head, and keepers over it whose `bootstrap`
 * records each root it is asked for and, while `serving` holds a block's
 * ledger, admits it (the fixture's own entries, checked against the root).
 */
const freshScene = (
  verifiedHead: ReplayedLedgerVerifiedHead = memoryVerifiedHead(),
) => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  const refusals: ReplayedLedgerRefusal[] = [];
  const unverified: ReplayedLedgerUnverified[] = [];
  const bootstrapped: string[] = [];
  const serving = new Map<string, readonly LedgerEntry[]>();
  const bootstrap = async (utxosRoot: string) => {
    bootstrapped.push(utxosRoot);
    const entries = serving.get(utxosRoot);
    if (entries === undefined) return false;
    await store.admit(entries, utxosRoot);
    return true;
  };
  return {
    store,
    verifiedHead,
    refusals,
    unverified,
    bootstrapped,
    serve: (block: ReplayedLedgerChainBlock) =>
      serving.set(block.header.utxosRoot, block.entries),
    keeper: (withPort = true) =>
      createReplayedLedger({
        store,
        verifiedHead,
        onRefusal: (refusal) => refusals.push(refusal),
        onUnverified: (block) => unverified.push(block),
        ...(withPort ? { bootstrap } : {}),
      }),
  };
};

describe(
  "the replayed ledger taking in the confirmed head's ledger with no verified head",
  { timeout: 120_000 },
  () => {
    it("makes a head the bootstrap serves the verified head with no block replayed, and replays the block above it on that ledger", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = freshScene();
      s.serve(b0);
      const asked: string[] = [];
      const source = replayedLedgerChainSource(chain.blocks, { asked });
      const ledger = s.keeper();

      expect(await ledger.ensure(head(b0), source)).toBe(true);
      expect(await s.verifiedHead.read()).toBe(b0.headerHash);
      expect(asked).toEqual([]);
      expect(s.bootstrapped).toEqual([b0.header.utxosRoot]);
      expect(s.unverified).toEqual([
        { headerHash: b0.headerHash, cause: "ledger-snapshot" },
      ]);

      // The store holds b0's ledger only: b1 replays over it.
      expect(await ledger.ensure(head(b1), source)).toBe(true);
      expect(asked).toEqual([b1.headerHash]);
      expect(await s.verifiedHead.read()).toBe(b1.headerHash);
      expect(s.bootstrapped).toHaveLength(1);
      expect(s.unverified).toHaveLength(1);
      expect(s.refusals).toEqual([]);
    });

    it("walks as it would with no bootstrap when the bootstrap does not serve the head", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const outcome = async (withPort: boolean) => {
        const s = freshScene();
        // The store holds b0's ledger, and not the one below it.
        await s.store.admit(b0.entries, b0.header.utxosRoot);
        const asked: string[] = [];
        const source = replayedLedgerChainSource(chain.blocks, { asked });
        const held = await s.keeper(withPort).ensure(head(b1), source);
        return {
          held,
          asked,
          verified: await s.verifiedHead.read(),
          unverified: s.unverified,
          refusals: s.refusals,
          bootstrapped: s.bootstrapped,
        };
      };
      const without = await outcome(false);
      const declined = await outcome(true);
      expect(declined.bootstrapped).toEqual([b1.header.utxosRoot]);
      expect({ ...declined, bootstrapped: [] }).toEqual(without);
      expect(without.held).toBe(true);
    });

    it("does not ask the bootstrap while a verified head exists", async () => {
      const chain = await replayedLedgerChain(1);
      const b0 = chain.blocks[0]!;
      const s = freshScene();
      s.serve(b0);
      await s.store.admit(chain.base, chain.baseRoot);
      await s.verifiedHead.write(REPLAYED_LEDGER_CHAIN_PARENT);
      const asked: string[] = [];
      expect(
        await s
          .keeper()
          .ensure(head(b0), replayedLedgerChainSource(chain.blocks, { asked })),
      ).toBe(true);
      expect(asked).toEqual([b0.headerHash]);
      expect(s.bootstrapped).toEqual([]);
    });

    it("does not ask the bootstrap for the genesis head", async () => {
      const s = freshScene();
      expect(
        await s.keeper().ensure(
          {
            headerHash: SDK.GENESIS_HEADER_HASH,
            utxosRoot: EMPTY_LEDGER_ROOT,
          },
          replayedLedgerChainSource([]),
        ),
      ).toBe(true);
      expect(s.bootstrapped).toEqual([]);
    });

    it("refuses a confirmed root the head's header does not commit before asking the bootstrap", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = freshScene();
      s.serve(b1);
      expect(
        await s
          .keeper()
          .ensure(
            { headerHash: b0.headerHash, utxosRoot: b1.header.utxosRoot },
            replayedLedgerChainSource(chain.blocks),
          ),
      ).toBe(false);
      expect(
        s.refusals.map(({ headerHash, kind }) => [headerHash, kind]),
      ).toEqual([[b0.headerHash, "root-mismatch"]]);
      expect(s.bootstrapped).toEqual([]);
      expect(await s.verifiedHead.read()).toBeUndefined();
    });

    it("surfaces a failed verified-head write after the bootstrap served, and asks the bootstrap again on the next call", async () => {
      const chain = await replayedLedgerChain(1);
      const b0 = chain.blocks[0]!;
      const durable = memoryVerifiedHead();
      let failWrite = true;
      const s = freshScene({
        read: durable.read,
        write: async (headerHash) => {
          if (failWrite) throw new Error("disk full");
          await durable.write(headerHash);
        },
      });
      s.serve(b0);
      const ledger = s.keeper();
      const source = replayedLedgerChainSource(chain.blocks);
      await expect(ledger.ensure(head(b0), source)).rejects.toThrow(
        /write the verified head: disk full/,
      );
      expect(s.unverified).toEqual([]);

      failWrite = false;
      expect(await ledger.ensure(head(b0), source)).toBe(true);
      expect(s.bootstrapped).toEqual([
        b0.header.utxosRoot,
        b0.header.utxosRoot,
      ]);
      expect(await s.verifiedHead.read()).toBe(b0.headerHash);
    });
  },
);

describe(
  "the replayed ledger taking in the confirmed head's ledger where the walk cannot derive its parent's",
  { timeout: 120_000 },
  () => {
    it("takes in the head's ledger on a later call after a first call no peer served, and replays the block above it", async () => {
      const chain = await replayedLedgerChain(4);
      const [, b1, b2, b3] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = freshScene();
      const ledger = s.keeper();
      const asked: string[] = [];
      // The ledger below the chain is gone, and so are the low payloads.
      const source = sourceWithout(
        chain.blocks,
        chain.blocks.slice(0, 2),
        asked,
      );

      expect(await ledger.ensure(head(b2), source)).toBe(false);
      expect(await s.verifiedHead.read()).toBe(b1.headerHash);
      expect(s.bootstrapped).toEqual([b2.header.utxosRoot]);

      s.serve(b2);
      expect(await ledger.ensure(head(b2), source)).toBe(true);
      expect(await s.verifiedHead.read()).toBe(b2.headerHash);
      expect(s.bootstrapped).toEqual([
        b2.header.utxosRoot,
        b2.header.utxosRoot,
      ]);
      expect(s.unverified.at(-1)).toEqual({
        headerHash: b2.headerHash,
        cause: "ledger-snapshot",
      });

      asked.length = 0;
      expect(await ledger.ensure(head(b3), source)).toBe(true);
      expect(asked).toEqual([b3.headerHash]);
      expect(await s.verifiedHead.read()).toBe(b3.headerHash);
      expect(s.bootstrapped).toHaveLength(2);
      expect(s.refusals).toEqual([]);
    });

    it("asks once in a call whose first ask was not served and whose walk cannot derive the head's parent ledger", async () => {
      const chain = await replayedLedgerChain(2);
      const b1 = chain.blocks[1]!;
      const s = freshScene();
      expect(
        await s
          .keeper()
          .ensure(head(b1), replayedLedgerChainSource(chain.blocks)),
      ).toBe(false);
      expect(s.bootstrapped).toEqual([b1.header.utxosRoot]);
    });

    it("takes in the head's ledger past a verified head whose following payloads are no longer retained", async () => {
      const chain = await replayedLedgerChain(4);
      const [b0, b1, b2, b3] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = freshScene();
      await s.store.admit(b0.entries, b0.header.utxosRoot);
      await s.verifiedHead.write(b0.headerHash);
      s.serve(b3);

      expect(
        await s
          .keeper()
          .ensure(head(b3), sourceWithout(chain.blocks, [b1, b2])),
      ).toBe(true);
      expect(s.bootstrapped).toEqual([b3.header.utxosRoot]);
      expect(await s.verifiedHead.read()).toBe(b3.headerHash);
      expect(s.unverified).toEqual([
        { headerHash: b1.headerHash, cause: "payload-not-retained" },
        { headerHash: b2.headerHash, cause: "parent-ledger-not-held" },
        { headerHash: b3.headerHash, cause: "ledger-snapshot" },
      ]);
    });

    it("waits for the head's payload over a held parent ledger without asking", async () => {
      const chain = await replayedLedgerChain(1);
      const b0 = chain.blocks[0]!;
      const s = freshScene();
      s.serve(b0);
      await s.store.admit(chain.base, chain.baseRoot);
      await s.verifiedHead.write(REPLAYED_LEDGER_CHAIN_PARENT);

      await expect(
        s.keeper().ensure(head(b0), sourceWithout(chain.blocks, [b0])),
      ).rejects.toBeInstanceOf(RetainedDaPayloadUnavailableError);
      expect(s.bootstrapped).toEqual([]);
      expect(await s.verifiedHead.read()).toBe(REPLAYED_LEDGER_CHAIN_PARENT);
    });

    it("refuses a head whose events do not replay to its root without asking", async () => {
      const chain = await replayedLedgerChain(1);
      // The block's events under a header that commits its parent's root.
      const liar = await replayedLedgerChainBlockWith(chain.blocks[0]!, {
        utxosRoot: chain.baseRoot,
      });
      const s = freshScene();
      await s.store.admit(chain.base, chain.baseRoot);
      await s.verifiedHead.write(REPLAYED_LEDGER_CHAIN_PARENT);

      expect(
        await s.keeper().ensure(head(liar), replayedLedgerChainSource([liar])),
      ).toBe(false);
      expect(s.refusals.map(({ kind }) => kind)).toEqual(["root-mismatch"]);
      expect(s.bootstrapped).toEqual([]);
    });
  },
);

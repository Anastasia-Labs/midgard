/**
 * The replayed ledger verifying the confirmed chain by block: a confirmed
 * block is replayed over its parent's ledger even when the store already
 * holds the root it commits, from the verified head up, once per block; the
 * verified head survives a restart; and a block that cannot be replayed any
 * more is passed and reported instead of holding the head.
 */
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "../src/ledger-store/index.js";
import {
  createReplayedLedger,
  memoryVerifiedHead,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerUnverified,
} from "../src/replayed-ledger.js";
import {
  type RetainedDaPayloadAvailability,
  RetainedDaPayloadUnavailableError,
} from "../src/transition-trace/fetch.retained-da-payload-unavailable.js";
import {
  REPLAYED_LEDGER_CHAIN_PARENT,
  replayedLedgerChain,
  type ReplayedLedgerChainBlock,
  replayedLedgerChainBlockWith,
  replayedLedgerChainSource,
} from "./helpers/replayed-ledger-chain.js";

type Chain = Awaited<ReturnType<typeof replayedLedgerChain>>;

/**
 * A store holding the ledger below the chain's first block, a verified head
 * at that parent, and keepers over both: each `keeper()` is a process start.
 */
const scene = async (chain: Chain) => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  await store.persist(await store.admit(chain.base, chain.baseRoot));
  const verifiedHead = memoryVerifiedHead();
  await verifiedHead.write(REPLAYED_LEDGER_CHAIN_PARENT);
  const refusals: ReplayedLedgerRefusal[] = [];
  const unverified: ReplayedLedgerUnverified[] = [];
  return {
    store,
    verifiedHead,
    refusals,
    unverified,
    keeper: () =>
      createReplayedLedger({
        store,
        verifiedHead,
        onRefusal: (refusal) => refusals.push(refusal),
        onUnverified: (block) => unverified.push(block),
      }),
  };
};

const head = ({ headerHash, header }: ReplayedLedgerChainBlock) => ({
  headerHash,
  utxosRoot: header.utxosRoot,
});

const hashes = (blocks: readonly ReplayedLedgerChainBlock[]) =>
  blocks.map(({ headerHash }) => headerHash);

/** A source whose payload of `missing` is answered as `availability`. */
const sourceWithout = (
  blocks: readonly ReplayedLedgerChainBlock[],
  missing: ReplayedLedgerChainBlock,
  availability: RetainedDaPayloadAvailability,
) =>
  replayedLedgerChainSource(blocks, {
    onRequest: (headerHash) => {
      if (headerHash === missing.headerHash)
        throw new RetainedDaPayloadUnavailableError(
          headerHash,
          `no payload of ${headerHash}`,
          availability,
        );
    },
  });

describe(
  "the replayed ledger verifying the confirmed chain",
  {
    timeout: 120_000,
  },
  () => {
    it("refuses a confirmed head whose events do not replay to the held root it commits", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      // b1's events under a header that commits its parent's root, as an
      // empty block would: the store holds that root.
      const liar = await replayedLedgerChainBlockWith(b1, {
        utxosRoot: b0.header.utxosRoot,
      });
      const s = await scene(chain);
      const ledger = s.keeper();
      const source = replayedLedgerChainSource([b0, liar]);
      expect(await ledger.ensure(head(b0), source)).toBe(true);

      expect(await ledger.ensure(head(liar), source)).toBe(false);
      expect(
        s.refusals.map(({ headerHash, kind }) => [headerHash, kind]),
      ).toEqual([[liar.headerHash, "root-mismatch"]]);
      expect(await s.verifiedHead.read()).toBe(b0.headerHash);
    });

    it.each(["in the same process", "after a restart"] as const)(
      "refuses a confirmed block below an honest head whose ledger a lookup already replayed, %s",
      async (when) => {
        // b0 <- liar <- top. The liar commits b0's root; top starts from that
        // root and replays to its own.
        const chain = await replayedLedgerChain(2, [1]);
        const [b0, b1] = chain.blocks as [
          ReplayedLedgerChainBlock,
          ReplayedLedgerChainBlock,
        ];
        const liar = await replayedLedgerChainBlockWith(b1, {
          utxosRoot: b0.header.utxosRoot,
        });
        const top = await replayedLedgerChainBlockWith(chain.forks[0]!, {
          prevHeaderHash: liar.headerHash,
        });
        const s = await scene(chain);
        const source = replayedLedgerChainSource([b0, liar, top]);
        const first = s.keeper();
        expect(await first.ensure(head(b0), source)).toBe(true);
        // A classification's lookup replays top over the root the liar
        // commits, and never the liar.
        const replayed = await first.ledgerAt(source)(top.headerHash);
        expect(await ledgerEntriesRoot(replayed!)).toBe(top.header.utxosRoot);

        const ledger = when === "after a restart" ? s.keeper() : first;
        expect(await ledger.ensure(head(top), source)).toBe(false);
        expect(
          s.refusals.map(({ headerHash, kind }) => [headerHash, kind]),
        ).toEqual([[liar.headerHash, "root-mismatch"]]);
        expect(await s.verifiedHead.read()).toBe(b0.headerHash);
      },
    );

    it("refuses a confirmed head that starts from a held root other than its parent's", async () => {
      // b0 <- b1 <- stray, where stray starts from b0's root.
      const chain = await replayedLedgerChain(2, [1]);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const stray = await replayedLedgerChainBlockWith(chain.forks[0]!, {
        prevHeaderHash: b1.headerHash,
      });
      const s = await scene(chain);
      const ledger = s.keeper();
      const source = replayedLedgerChainSource([b0, b1, stray]);
      expect(await ledger.ensure(head(b1), source)).toBe(true);

      expect(await ledger.ensure(head(stray), source)).toBe(false);
      expect(
        s.refusals.map(({ headerHash, kind }) => [headerHash, kind]),
      ).toEqual([[stray.headerHash, "parent-mismatch"]]);
    });

    it("replays each confirmed block once: not one this process replayed, and after a restart only those above the verified head", async () => {
      const chain = await replayedLedgerChain(4);
      const [, b1, b2, b3] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = await scene(chain);
      const asked: string[] = [];
      const source = replayedLedgerChainSource(chain.blocks, { asked });

      // A lookup replays every block; confirming them replays none again.
      const first = s.keeper();
      await first.ledgerAt(source)(b3.headerHash);
      expect(asked).toEqual(hashes(chain.blocks));
      expect(await first.ensure(head(b1), source)).toBe(true);
      expect(asked).toEqual(hashes(chain.blocks));
      expect(await s.verifiedHead.read()).toBe(b1.headerHash);

      // Restart. The store holds every root, and this process replayed no
      // block: the blocks above the verified head are replayed, b0 and b1
      // are not.
      asked.length = 0;
      const second = s.keeper();
      expect(await second.ensure(head(b1), source)).toBe(true);
      expect(asked).toEqual([]);
      expect(await second.ensure(head(b3), source)).toBe(true);
      expect(asked).toEqual(hashes([b2, b3]));
      expect(await second.ensure(head(b3), source)).toBe(true);
      expect(asked).toEqual(hashes([b2, b3]));
      expect(s.refusals).toEqual([]);
      expect(s.unverified).toEqual([]);
    });

    it("moves the verified head up block by block and resumes above it", async () => {
      const chain = await replayedLedgerChain(3);
      const [b0, b1, b2] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = await scene(chain);
      const asked: string[] = [];
      let serveHead = false;
      const source = replayedLedgerChainSource(chain.blocks, {
        asked,
        serves: (headerHash) => serveHead || headerHash !== b2.headerHash,
      });
      // The head's payload is not served yet.
      expect(await s.keeper().ensure(head(b2), source)).toBe(false);
      expect(await s.verifiedHead.read()).toBe(b1.headerHash);
      expect(s.unverified).toEqual([]);

      serveHead = true;
      asked.length = 0;
      expect(await s.keeper().ensure(head(b2), source)).toBe(true);
      expect(asked).toEqual([b2.headerHash]);
      expect(await s.verifiedHead.read()).toBe(b2.headerHash);
      expect(hashes([b0])).not.toContain(await s.verifiedHead.read());
    });

    it("passes and reports a block below the head whose payload no source retains, and verifies the head above it", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = await scene(chain);
      // The store holds b0's ledger from an earlier run.
      await s.store.persist(
        await s.store.admit(b0.entries, b0.header.utxosRoot),
      );
      const asked: string[] = [];
      const source = replayedLedgerChainSource(chain.blocks, {
        asked,
        onRequest: (headerHash) => {
          if (headerHash === b0.headerHash)
            throw new RetainedDaPayloadUnavailableError(
              headerHash,
              `no payload of ${headerHash}`,
              "not_found",
            );
        },
      });
      expect(await s.keeper().ensure(head(b1), source)).toBe(true);
      expect(asked).toEqual(hashes([b0, b1]));
      expect(s.unverified).toEqual([
        { headerHash: b0.headerHash, cause: "payload-not-retained" },
      ]);
      expect(s.refusals).toEqual([]);
      expect(await s.verifiedHead.read()).toBe(b1.headerHash);
    });

    it("waits for a block below the head while a source has not answered for its payload", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = await scene(chain);
      await s.store.persist(
        await s.store.admit(b0.entries, b0.header.utxosRoot),
      );
      await expect(
        s
          .keeper()
          .ensure(head(b1), sourceWithout(chain.blocks, b0, "unreachable")),
      ).rejects.toBeInstanceOf(RetainedDaPayloadUnavailableError);
      expect(s.unverified).toEqual([]);
      expect(await s.verifiedHead.read()).toBe(REPLAYED_LEDGER_CHAIN_PARENT);
    });

    it("waits for the head's payload even when the store holds the root the head commits", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const s = await scene(chain);
      await s.store.persist(
        await s.store.admit(b1.entries, b1.header.utxosRoot),
      );
      await expect(
        s
          .keeper()
          .ensure(head(b1), sourceWithout(chain.blocks, b1, "not_found")),
      ).rejects.toBeInstanceOf(RetainedDaPayloadUnavailableError);
      expect(
        await s.keeper().ensure(
          head(b1),
          replayedLedgerChainSource(chain.blocks, {
            serves: (headerHash) => headerHash !== b1.headerHash,
          }),
        ),
      ).toBe(false);
      expect(s.unverified).toEqual([]);
      expect(await s.verifiedHead.read()).toBe(b0.headerHash);
    });

    it("with no verified head, verifies up from the lowest block the source knows and reports the one below it once", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const store = createLedgerStore(createMemoryLedgerStorage());
      // The store holds b0's ledger, and not the one below it.
      await store.persist(await store.admit(b0.entries, b0.header.utxosRoot));
      const verifiedHead = memoryVerifiedHead();
      const unverified: ReplayedLedgerUnverified[] = [];
      const ledger = createReplayedLedger({
        store,
        verifiedHead,
        onUnverified: (block) => unverified.push(block),
      });
      const asked: string[] = [];
      const source = replayedLedgerChainSource(chain.blocks, { asked });
      expect(await ledger.ensure(head(b1), source)).toBe(true);
      // b0 starts from a ledger that cannot be derived; b1 is replayed.
      expect(asked).toEqual([b1.headerHash]);
      expect(unverified).toEqual([
        { headerHash: b0.headerHash, cause: "parent-ledger-not-held" },
        { headerHash: REPLAYED_LEDGER_CHAIN_PARENT, cause: "header-not-held" },
      ]);
      expect(await verifiedHead.read()).toBe(b1.headerHash);
      expect(await ledger.ensure(head(b1), source)).toBe(true);
      expect(unverified).toHaveLength(2);
    });

    it("does not pass a head whose ledger the store does not hold", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      // Nothing is held: neither block's parent ledger can be derived.
      const verifiedHead = memoryVerifiedHead();
      const unverified: ReplayedLedgerUnverified[] = [];
      const ledger = createReplayedLedger({
        store: createLedgerStore(createMemoryLedgerStorage()),
        verifiedHead,
        onUnverified: (block) => unverified.push(block),
      });
      expect(
        await ledger.ensure(head(b1), replayedLedgerChainSource(chain.blocks)),
      ).toBe(false);
      expect(unverified.map(({ headerHash }) => headerHash)).not.toContain(
        b1.headerHash,
      );
      expect(await verifiedHead.read()).toBe(b0.headerHash);
    });
  },
);

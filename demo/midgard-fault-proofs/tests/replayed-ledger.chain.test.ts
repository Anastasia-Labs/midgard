/**
 * The replayed ledger over a chain of blocks: a restart answers from the
 * store, a fresh start replays forward one payload at a time and resumes
 * from the highest persisted block, and `retain` keeps exactly the window's
 * ledgers and those a replay in progress holds.
 */
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  LedgerNotRetained,
  type LedgerStore,
} from "../src/ledger-store/index.js";
import { createReplayedLedger } from "../src/replayed-ledger.js";
import {
  replayedLedgerChain,
  replayedLedgerChainSource,
} from "./helpers/replayed-ledger-chain.js";

/** A store holding the ledger below the chain's first block. */
const storeOver = async (
  chain: Awaited<ReturnType<typeof replayedLedgerChain>>,
) => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  await store.persist(await store.admit(chain.base, chain.baseRoot));
  return store;
};

const readable = (store: LedgerStore, root: string) =>
  store.at(root).then(
    () => true,
    (error: unknown) => {
      if (error instanceof LedgerNotRetained) return false;
      throw error;
    },
  );

describe("the replayed ledger over a chain", { timeout: 120_000 }, () => {
  it("answers a lookup after a restart without asking for a payload it already replayed", async () => {
    const chain = await replayedLedgerChain(3);
    const store = await storeOver(chain);
    const tip = chain.blocks[2]!;
    const asked: string[] = [];
    const first = await createReplayedLedger({ store }).ledgerAt(
      replayedLedgerChainSource(chain.blocks, { asked }),
    )(tip.headerHash);
    expect(await ledgerEntriesRoot(first!)).toBe(tip.header.utxosRoot);
    expect(asked).toEqual(chain.blocks.map(({ headerHash }) => headerHash));

    // Restart: a new keeper over the same store.
    const again: string[] = [];
    const restarted = createReplayedLedger({ store }).ledgerAt(
      replayedLedgerChainSource(chain.blocks, { asked: again }),
    );
    expect(await restarted(tip.headerHash)).toEqual(first);
    expect(
      await ledgerEntriesRoot((await restarted(chain.blocks[1]!.headerHash))!),
    ).toBe(chain.blocks[1]!.header.utxosRoot);
    expect(again).toEqual([]);
  });

  it("replays a fresh chain forward one payload at a time and resumes a failure from the highest persisted block", async () => {
    const chain = await replayedLedgerChain(5);
    const store = await storeOver(chain);
    const hashes = chain.blocks.map(({ headerHash }) => headerHash);
    const rootOf = new Map(
      chain.blocks.map((block) => [block.headerHash, block.header.utxosRoot]),
    );
    // A payload is outstanding from its request until its block's ledger
    // is persisted; each request finds none outstanding before it.
    const asked: string[] = [];
    const outstandingAtRequest: number[] = [];
    let serveThird = false;
    const source = replayedLedgerChainSource(chain.blocks, {
      asked,
      serves: (headerHash) => serveThird || headerHash !== hashes[2],
      onRequest: async (headerHash) => {
        let outstanding = 0;
        for (const earlier of asked.slice(0, -1))
          if (
            earlier !== headerHash &&
            !(await readable(store, rootOf.get(earlier)!))
          )
            outstanding += 1;
        outstandingAtRequest.push(outstanding);
      },
    });
    const ledger = createReplayedLedger({ store });

    // The third block's payload is not served yet: the lookup has no answer,
    // and the two blocks below it are persisted.
    expect(await ledger.ledgerAt(source)(hashes[4]!)).toBeUndefined();
    expect(asked).toEqual(hashes.slice(0, 3));
    expect(await readable(store, rootOf.get(hashes[1]!)!)).toBe(true);
    expect(await readable(store, rootOf.get(hashes[2]!)!)).toBe(false);

    // Once served, the walk resumes at the third block.
    serveThird = true;
    asked.length = 0;
    const entries = await ledger.ledgerAt(source)(hashes[4]!);
    expect(await ledgerEntriesRoot(entries!)).toBe(rootOf.get(hashes[4]!));
    expect(asked).toEqual(hashes.slice(2));
    expect(outstandingAtRequest.every((count) => count === 0)).toBe(true);
    expect(outstandingAtRequest).toHaveLength(6);
  });

  it("keeps the window's ledgers, the queue tip and the confirmed head, and drops the rest", async () => {
    const chain = await replayedLedgerChain(5);
    const store = await storeOver(chain);
    const ledger = createReplayedLedger({ store });
    const [b0, b1, b2, b3, b4] = chain.blocks as [
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
    ];
    await ledger.ledgerAt(replayedLedgerChainSource(chain.blocks))(
      b4.headerHash,
    );
    // b0 and b2 left the window; b1 is the confirmed head.
    expect(
      await ledger.retain({
        blocks: [b3.header, b4.header],
        newestRoot: b4.header.utxosRoot,
        confirmedRoot: b1.header.utxosRoot,
      }),
    ).toEqual(new Set());
    expect(await readable(store, chain.baseRoot)).toBe(false);
    expect(await readable(store, b0.header.utxosRoot)).toBe(false);
    await expect(store.at(b0.header.utxosRoot)).rejects.toBeInstanceOf(
      LedgerNotRetained,
    );
    // The predecessor of the oldest block inside the window.
    expect(await readable(store, b2.header.utxosRoot)).toBe(true);
    expect(await readable(store, b1.header.utxosRoot)).toBe(true);
    expect(await readable(store, b3.header.utxosRoot)).toBe(true);
    expect(await readable(store, b4.header.utxosRoot)).toBe(true);

    // An empty window: the tip stays readable.
    await ledger.retain({
      blocks: [],
      newestRoot: b4.header.utxosRoot,
      confirmedRoot: b4.header.utxosRoot,
    });
    expect(await readable(store, b4.header.utxosRoot)).toBe(true);
    for (const block of [b1, b2, b3])
      expect(await readable(store, block.header.utxosRoot)).toBe(false);
  });

  it("keeps the ledgers of a replay in progress that the window does not name, and the replay completes", async () => {
    const chain = await replayedLedgerChain(2);
    const store = await storeOver(chain);
    const ledger = createReplayedLedger({ store });
    const [b0, b1] = chain.blocks as [
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
    ];
    // The lookup replays b0, then waits at b1's payload fetch.
    let reached!: () => void;
    const atFetch = new Promise<void>((resolve) => {
      reached = resolve;
    });
    let serve!: () => void;
    const served = new Promise<void>((resolve) => {
      serve = resolve;
    });
    const lookup = ledger.ledgerAt(
      replayedLedgerChainSource(chain.blocks, {
        onRequest: async (headerHash) => {
          if (headerHash !== b1.headerHash) return;
          reached();
          await served;
        },
      }),
    )(b1.headerHash);
    await atFetch;

    // A window that names neither b1's parent ledger (b0's) nor b1's own,
    // nor any ledger sharing their outputs.
    expect(
      await ledger.retain({
        blocks: [],
        newestRoot: EMPTY_LEDGER_ROOT,
        confirmedRoot: EMPTY_LEDGER_ROOT,
      }),
    ).toEqual(new Set());
    serve();
    const entries = await lookup;
    expect(await ledgerEntriesRoot(entries!)).toBe(b1.header.utxosRoot);
    expect(await readable(store, b1.header.utxosRoot)).toBe(true);
    expect(await readable(store, b0.header.utxosRoot)).toBe(true);
  });

  it("keeps a canonical block's predecessor when a removed fork block sits before it in the window", async () => {
    // b0 <- b1 <- b2, and a removed fork f above b0. b1 left the window; f
    // and b2 are inside it.
    const chain = await replayedLedgerChain(3, [1]);
    const store = await storeOver(chain);
    const ledger = createReplayedLedger({ store });
    const [b0, b1, b2] = chain.blocks as [
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
    ];
    const f = chain.forks[0]!;
    const source = replayedLedgerChainSource([...chain.blocks, f]);
    await ledger.ledgerAt(source)(b2.headerHash);
    await ledger.ledgerAt(source)(f.headerHash);
    expect(f.header.prevUtxosRoot).toBe(b0.header.utxosRoot);
    expect(f.header.utxosRoot).not.toBe(b1.header.utxosRoot);
    await ledger.retain({
      blocks: [f.header, b2.header],
      newestRoot: b2.header.utxosRoot,
      confirmedRoot: b0.header.utxosRoot,
    });
    expect(await readable(store, b1.header.utxosRoot)).toBe(true);
    expect(await readable(store, f.header.utxosRoot)).toBe(true);
  });
});

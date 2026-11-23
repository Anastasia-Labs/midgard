/**
 * `ensure` hands its caller a turn to retain after each block of the walk.
 * A caller that retains its window from that turn keeps the store at the
 * window's ledgers across a long walk, on the memory and the SQLite adapter:
 * the walk's own ledger and one another lookup replays from are kept by
 * every turn,
 * the head's ledger answers afterwards, and no turn reports a lost root.
 * Without the turn, the walk holds every ledger it derived.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  LedgerNotRetained,
  type LedgerStorage,
  type LedgerStorageCounts,
  type LedgerStore,
} from "../src/ledger-store/index.js";
import { openSqliteLedgerStorage } from "../src/ledger-store/sqlite.js";
import {
  createReplayedLedger,
  memoryVerifiedHead,
} from "../src/replayed-ledger.js";
import {
  REPLAYED_LEDGER_CHAIN_PARENT,
  replayedLedgerChain,
  type ReplayedLedgerChainBlock,
  replayedLedgerChainSource,
} from "./helpers/replayed-ledger-chain.js";
import { entriesRoot } from "./support/fixture-ledger.js";

/** Blocks in the chain; every block spends one seed of the base ledger and
 * pays one output, so each ledger on the way has the same size. */
const LENGTH = 200;
/** Blocks in the caller's window: the chain's last ones. */
const WINDOW = 4;
/** A block half-way up the walk. */
const MIDDLE = LENGTH / 2;

type Chain = Awaited<ReturnType<typeof replayedLedgerChain>>;
let chain: Chain;
/** A block above `MIDDLE`'s that the chain does not continue from. */
let fork: ReplayedLedgerChainBlock;

beforeAll(async () => {
  chain = await replayedLedgerChain(LENGTH, [MIDDLE + 1]);
  fork = chain.forks[0]!;
}, 600_000);

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const adapters: readonly (readonly [
  string,
  () => Promise<LedgerStorage & { close?: () => void }>,
])[] = [
  ["memory", async () => createMemoryLedgerStorage()],
  [
    "SQLite",
    async () => {
      const directory = await mkdtemp(join(tmpdir(), "retain-turn-"));
      directories.push(directory);
      return openSqliteLedgerStorage(join(directory, "ledger.sqlite"));
    },
  ],
];

/** The counts of a memory store holding exactly `blocks`' ledgers. */
const countsHolding = async (
  blocks: readonly ReplayedLedgerChainBlock[],
): Promise<LedgerStorageCounts> => {
  const storage = createMemoryLedgerStorage();
  const store = createLedgerStore(storage);
  for (const { entries, header } of blocks)
    await store.admit(entries, header.utxosRoot);
  await store.retain(new Set(blocks.map(({ header }) => header.utxosRoot)));
  return await storage.counts();
};

const readable = (store: LedgerStore, root: string) =>
  store.at(root).then(
    () => true,
    (error: unknown) => {
      if (error instanceof LedgerNotRetained) return false;
      throw error;
    },
  );

/** A promise and the call that resolves it. */
const gate = () => {
  let open!: () => void;
  const opened = new Promise<void>((resolve) => {
    open = resolve;
  });
  return { open, opened };
};

describe.each(adapters)(
  "a long ensure walk over the %s adapter",
  { timeout: 600_000 },
  (_name, open) => {
    let storage: LedgerStorage & { close?: () => void };
    let store: LedgerStore;
    const tip = () => chain.blocks[LENGTH - 1]!;
    /** The window's blocks and the block below them, whose ledger the
     * lowest window block starts from. */
    const windowLedgers = () => chain.blocks.slice(LENGTH - WINDOW - 1);
    const window = () => ({
      blocks: chain.blocks.slice(LENGTH - WINDOW).map(({ header }) => header),
      newestRoot: tip().header.utxosRoot,
      confirmedRoot: tip().header.utxosRoot,
    });
    const head = () => ({
      headerHash: tip().headerHash,
      utxosRoot: tip().header.utxosRoot,
    });

    afterEach(() => storage.close?.());

    /** A store holding the ledger below the chain and a keeper whose
     * verified head is that ledger's block. */
    const opened = async () => {
      storage = await open();
      store = createLedgerStore(storage);
      await store.admit(chain.base, chain.baseRoot);
      const verifiedHead = memoryVerifiedHead();
      await verifiedHead.write(REPLAYED_LEDGER_CHAIN_PARENT);
      return createReplayedLedger({ store, verifiedHead });
    };

    it("holds the window's ledgers, not every ledger it derived, when the caller retains from each turn", async () => {
      const keeper = await opened();
      const turns: LedgerStorageCounts[] = [];
      const lost: string[] = [];

      expect(
        await keeper.ensure(
          head(),
          replayedLedgerChainSource(chain.blocks),
          async () => {
            lost.push(...(await keeper.retain(window())));
            turns.push(await storage.counts());
          },
        ),
      ).toBe(true);

      expect(await storage.counts()).toEqual(
        await countsHolding(windowLedgers()),
      );
      expect(lost).toEqual([]);
      // Flat across the walk: bounded by the window, not by blocks walked.
      expect(turns).toHaveLength(LENGTH);
      expect(Math.max(...turns.map(({ roots }) => roots))).toBeLessThanOrEqual(
        WINDOW + 1,
      );
      expect(turns[10]).toEqual(await countsHolding([chain.blocks[10]!]));
      expect(turns[MIDDLE]).toEqual(
        await countsHolding([chain.blocks[MIDDLE]!]),
      );
      for (const block of windowLedgers())
        expect(await readable(store, block.header.utxosRoot)).toBe(true);

      // The head's ledger answers from the store.
      const asked: string[] = [];
      const reached = await keeper.ledgerAt(
        replayedLedgerChainSource(chain.blocks, { asked }),
      )(tip().headerHash);
      expect(await entriesRoot(reached!)).toBe(tip().header.utxosRoot);
      expect(asked).toEqual([]);
    });

    it("holds every ledger it derived without the turn", async () => {
      const keeper = await opened();

      expect(
        await keeper.ensure(head(), replayedLedgerChainSource(chain.blocks)),
      ).toBe(true);

      expect((await storage.counts()).roots).toBe(LENGTH + 1);
    });

    it("answers a lookup that replays from the walk's ledger while later turns retain", async () => {
      const keeper = await opened();
      const start = chain.blocks[MIDDLE]!;
      const lookupFetching = gate();
      const walkPast = gate();
      let lookup: Promise<{ ledger: Ledger | undefined; root?: string }>;
      // The fork above the middle block replays from the middle block's
      // ledger, which the walk holds when the lookup starts, and waits at its
      // payload until the walk's turns have run past that ledger.
      const source = replayedLedgerChainSource([...chain.blocks, fork], {
        onRequest: async (headerHash) => {
          if (headerHash === chain.blocks[MIDDLE + 1]!.headerHash) {
            lookup = keeper
              .ledgerAt(source)(fork.headerHash)
              .then(async (ledger) => ({
                ledger,
                ...(ledger === undefined
                  ? {}
                  : { root: await entriesRoot(ledger) }),
              }));
            await lookupFetching.opened;
          }
          if (headerHash === fork.headerHash) {
            lookupFetching.open();
            await walkPast.opened;
          }
          // The walk waits here until the lookup has read its answer.
          if (headerHash === chain.blocks[MIDDLE + 6]!.headerHash) {
            expect(await readable(store, start.header.utxosRoot)).toBe(true);
            walkPast.open();
            await lookup;
          }
        },
      });

      expect(
        await keeper.ensure(head(), source, async () => {
          await keeper.retain(window());
        }),
      ).toBe(true);
      const answered = await lookup!;
      expect(answered.ledger?.root).toBe(fork.header.utxosRoot);
      expect(answered.root).toBe(fork.header.utxosRoot);
    });
  },
);

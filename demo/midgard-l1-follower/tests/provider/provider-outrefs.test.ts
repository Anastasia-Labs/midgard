import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  credentialToAddress,
  type OutRef as LucidOutRef,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import type {
  BlockSummary,
  FactStore,
  OutRef,
  TrackedSet,
} from "../../src/index.js";
import {
  L1FollowerProvider,
  L1ProviderTransientError,
} from "../../src/provider/index.js";
import type { SqlTx } from "../../src/sql/backend.js";
import { OUTPUTS_IN_CHUNK } from "../../src/store/reads.js";
import { testDatabases } from "../support/postgres.js";
import {
  built,
  DEVNET_LEDGER,
  fakeLedgerTransport,
  ledgerEntry,
  providerAdapters,
} from "../support/provider.js";
import { fill, ORIGIN, tx } from "../support/small-chain.js";

/**
 * `getUtxosByOutRef` against the real store of each dialect: request order,
 * repeats, live / spent / absent outrefs, the tracked scope of ledger
 * answers, a request above one statement's worth of outrefs, the abort
 * signal, and one snapshot for every stored row of a call.
 */
const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-provider-outrefs-"));

const hex = (bytes: Buffer): string => bytes.toString("hex");
const keyAddress = (byte: string): string =>
  credentialToAddress("Custom", { type: "Key", hash: byte.repeat(28) });
const wallet = keyAddress("ab");
const untracked = keyAddress("ee");
const ada = (n: number) => ({ lovelace: BigInt(n) * 1_000_000n });

/** Live tracked outputs BIG#0 … BIG#(COUNT-1); b2 spends the first SPENT_COUNT. */
const COUNT = 1_200;
const SPENT_COUNT = 10;
const BIG = fill(0xb1);
const SPENDER = fill(0xb2);
const LATE_SPENDER = fill(0xb3);
const WALLET_OUTPUT = built(
  { txHash: BIG, index: 0 },
  {
    address: wallet,
    assets: ada(2),
  },
).summary;
const big = (index: number): OutRef => ({ txHash: BIG, index });

const TRACKED: TrackedSet = {
  addresses: new Set([hex(WALLET_OUTPUT.address)]),
  paymentCredentials: new Set(),
  policies: new Set(),
};

/** On the ledger only: two untracked outputs and one in the tracked scope. */
const LEDGER_UNTRACKED_1 = built(
  { txHash: fill(0x71), index: 0 },
  { address: untracked, assets: ada(4) },
);
const LEDGER_TRACKED = built(
  { txHash: fill(0x72), index: 1 },
  { address: wallet, assets: ada(9) },
);
const LEDGER_UNTRACKED_2 = built(
  { txHash: fill(0x73), index: 2 },
  { address: untracked, assets: ada(5) },
);
/** A spent stored row the ledger still answers for, outside the tracked scope. */
const LEDGER_FOR_SPENT = built(big(0), { address: untracked, assets: ada(3) });
const NOWHERE: OutRef = { txHash: fill(0x99), index: 0 };

const blocks: BlockSummary[] = [
  {
    point: { slot: 101, hash: fill(0xc1) },
    height: 51,
    parentHash: ORIGIN.point.hash,
    txs: [
      tx(BIG, {
        inputs: [{ txHash: fill(0x01), index: 0 }],
        outputs: Array.from({ length: COUNT }, () => WALLET_OUTPUT),
      }),
    ],
  },
  {
    point: { slot: 103, hash: fill(0xc2) },
    height: 52,
    parentHash: fill(0xc1),
    txs: [
      tx(SPENDER, {
        inputs: Array.from({ length: SPENT_COUNT }, (_, index) => big(index)),
      }),
    ],
  },
];
/** Spends the last LATE_SPENT outputs; applied mid-read by the snapshot test. */
const LATE_SPENT = 10;
const lateBlock: BlockSummary = {
  point: { slot: 105, hash: fill(0xc3) },
  height: 53,
  parentHash: fill(0xc2),
  txs: [
    tx(LATE_SPENDER, {
      inputs: Array.from({ length: LATE_SPENT }, (_, offset) =>
        big(COUNT - LATE_SPENT + offset),
      ),
    }),
  ],
};

const lucidRef = (outRef: OutRef): LucidOutRef => ({
  txHash: hex(outRef.txHash),
  outputIndex: outRef.index,
});
const key = (outRef: OutRef): string => `${hex(outRef.txHash)}#${outRef.index}`;
const refs = (utxos: readonly UTxO[]): string[] =>
  utxos.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`);
const isStoredLive = (outRef: OutRef): boolean =>
  outRef.txHash.equals(BIG) && outRef.index >= SPENT_COUNT;
const isUntrackedOnLedger = (outRef: OutRef): boolean =>
  [LEDGER_UNTRACKED_1, LEDGER_UNTRACKED_2].some(
    (output) => key(output.outRef) === key(outRef),
  );

/** A fixed permutation (a seeded linear congruential shuffle). */
const shuffled = <T>(items: readonly T[], seed: number): T[] => {
  const out = [...items];
  let state = seed;
  for (let i = out.length - 1; i > 0; i -= 1) {
    state = (state * 1_103_515_245 + 12_345) % 2_147_483_648;
    const j = state % (i + 1);
    [out[i], out[j]] = [out[j]!, out[i]!];
  }
  return out;
};

/** Distinct in first-seen order: what the provider answers for. */
const firstSeen = (outRefs: readonly OutRef[]): OutRef[] => [
  ...new Map(outRefs.map((outRef) => [key(outRef), outRef])).values(),
];

/** The answer the semantics require for `request`. */
const expected = (request: readonly OutRef[]): string[] =>
  firstSeen(request)
    .filter((outRef) => isStoredLive(outRef) || isUntrackedOnLedger(outRef))
    .map(key);

type Observed = {
  transactions: number;
  queries: number;
  /** The distinct statement texts, when given. */
  texts?: Set<string>;
  /** Runs once, after the first statement through the store has its answer. */
  afterFirstQuery?: () => Promise<void>;
};

/** The store, with its transactions and their statements counted. */
const observed = (store: FactStore, seen: Observed): FactStore => ({
  ...store,
  transaction: (mode, run, options) => {
    seen.transactions += 1;
    const counting = (sqlTx: SqlTx): SqlTx => ({
      query: async (sql, params) => {
        seen.queries += 1;
        seen.texts?.add(sql);
        const rows = await sqlTx.query(sql, params);
        const hook = seen.afterFirstQuery;
        seen.afterFirstQuery = undefined;
        await hook?.();
        return rows;
      },
      exec: (sql) => sqlTx.exec(sql),
    });
    return store.transaction(mode, (sqlTx) => run(counting(sqlTx)), options);
  },
});

/** The provider, recording what it asks the ledger for. */
class RecordingProvider extends L1FollowerProvider {
  readonly ledgerAsked: string[][] = [];
  protected override async ledgerUtxosByTxIn(
    outRefs: readonly OutRef[],
    at: "build" | "tip",
  ) {
    this.ledgerAsked.push(outRefs.map(key));
    return await super.ledgerUtxosByTxIn(outRefs, at);
  }
}

let transport: L1NodeTransport;
beforeAll(async () => {
  transport = await fakeLedgerTransport(scratch, {
    answers: DEVNET_LEDGER.lsq,
    utxos: [
      LEDGER_UNTRACKED_1,
      LEDGER_TRACKED,
      LEDGER_UNTRACKED_2,
      LEDGER_FOR_SPENT,
    ].map(ledgerEntry),
  });
});
afterAll(async () => {
  await transport?.close();
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const absentInOrder = (request: readonly OutRef[]): string[] =>
  firstSeen(request)
    .filter((outRef) => !outRef.txHash.equals(BIG))
    .map(key);

describe.each(providerAdapters(databases, scratch))(
  "L1 follower provider outref reads ($name)",
  (adapter) => {
    let store: FactStore;
    const fresh = (seen: Observed = { transactions: 0, queries: 0 }) =>
      new RecordingProvider({ store: observed(store, seen), transport });
    beforeAll(async () => {
      store = await adapter.open(TRACKED);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      for (const block of blocks)
        expect(await store.applyBlock(block)).toMatchObject({
          kind: "applied",
        });
    });
    afterAll(async () => await store.close());

    it("answers live stored outrefs in request order, a repeat once at its first place", async () => {
      const live = shuffled(
        Array.from({ length: 40 }, (_, index) => big(SPENT_COUNT + index * 7)),
        7,
      );
      const request = [...live, live[3]!, live[0]!, live[17]!];
      const provider = fresh();
      const answer = await provider.getUtxosByOutRef(request.map(lucidRef));
      expect(refs(answer)).toEqual(live.map(key));
      expect(provider.ledgerAsked.flat()).toEqual([]);
    });

    it("omits a spent row without asking the ledger, asks it for absent outrefs in order, and keeps its tracked-scope outputs out", async () => {
      const request = [
        LEDGER_UNTRACKED_2.outRef,
        big(3),
        big(SPENT_COUNT + 5),
        NOWHERE,
        big(0),
        LEDGER_TRACKED.outRef,
        big(SPENT_COUNT),
        LEDGER_UNTRACKED_1.outRef,
        LEDGER_UNTRACKED_2.outRef,
        big(3),
      ];
      const provider = fresh();
      const answer = await provider.getUtxosByOutRef(request.map(lucidRef));
      expect(refs(answer)).toEqual([
        key(LEDGER_UNTRACKED_2.outRef),
        key(big(SPENT_COUNT + 5)),
        key(big(SPENT_COUNT)),
        key(LEDGER_UNTRACKED_1.outRef),
      ]);
      expect(answer.find((utxo) => utxo.outputIndex === 2)?.address).toBe(
        untracked,
      );
      expect(provider.ledgerAsked).toEqual([
        [
          key(LEDGER_UNTRACKED_2.outRef),
          key(NOWHERE),
          key(LEDGER_TRACKED.outRef),
          key(LEDGER_UNTRACKED_1.outRef),
        ],
      ]);
    });

    it("answers a request of every stored row and then some, shuffled and repeated, in one read transaction", async () => {
      const everything = [
        ...Array.from({ length: COUNT }, (_, index) => big(index)),
        LEDGER_UNTRACKED_1.outRef,
        LEDGER_TRACKED.outRef,
        LEDGER_UNTRACKED_2.outRef,
        NOWHERE,
      ];
      const request = shuffled(
        [...everything, ...everything.filter((_, index) => index % 9 === 0)],
        2_024,
      );
      expect(request.length).toBeGreaterThan(COUNT);
      const seen: Observed = { transactions: 0, queries: 0 };
      const provider = fresh(seen);
      const answer = await provider.getUtxosByOutRef(request.map(lucidRef));
      expect(refs(answer)).toEqual(expected(request));
      expect(answer).toHaveLength(COUNT - SPENT_COUNT + 2);
      expect(provider.ledgerAsked).toEqual([absentInOrder(request)]);
      expect(seen.transactions).toBe(1);
      expect(seen.queries).toBeGreaterThan(0);
    });

    it("makes no query for an empty request", async () => {
      const seen: Observed = { transactions: 0, queries: 0 };
      const provider = fresh(seen);
      expect(await provider.getUtxosByOutRef([])).toEqual([]);
      expect(seen.queries).toBe(0);
      expect(provider.ledgerAsked.flat()).toEqual([]);
    });

    it("ends the read of the facts at its abort signal, before the ledger is asked", async () => {
      const request = Array.from({ length: COUNT }, (_, index) =>
        lucidRef(big(index)),
      ).concat(lucidRef(NOWHERE));
      const before = new AbortController();
      before.abort(new Error("stopped before the read"));
      const early = fresh();
      await expect(
        early.getUtxosByOutRef(request, { signal: before.signal }),
      ).rejects.toMatchObject({
        constructor: L1ProviderTransientError,
        source: "store",
        reason: "stopped before the read",
      });
      expect(early.ledgerAsked).toEqual([]);

      const during = new AbortController();
      const seen: Observed = {
        transactions: 0,
        queries: 0,
        afterFirstQuery: async () => {
          during.abort(new Error("stopped during the read"));
        },
      };
      const late = fresh(seen);
      const reading = late.getUtxosByOutRef(request, {
        signal: during.signal,
      });
      if (adapter.name === "postgres") {
        // The abort gives up the connection: the next statement fails.
        await expect(reading).rejects.toMatchObject({
          constructor: L1ProviderTransientError,
          source: "store",
          reason: "stopped during the read",
        });
        expect(late.ledgerAsked).toEqual([]);
      } else {
        // SQLite checks the signal before it begins only: the read settles.
        expect(refs(await reading)).toHaveLength(COUNT - SPENT_COUNT);
      }
    });

    it("reads the stored rows a chunk of outrefs per statement", async () => {
      expect(COUNT).toBeGreaterThan(2 * OUTPUTS_IN_CHUNK);
      const request = Array.from({ length: COUNT }, (_, index) =>
        lucidRef(big(index)),
      ).concat(lucidRef(NOWHERE));
      const seen: Observed = { transactions: 0, queries: 0 };
      await fresh(seen).getUtxosByOutRef(request);
      expect(seen.transactions).toBe(1);
      expect(seen.queries).toBe(Math.ceil(request.length / OUTPUTS_IN_CHUNK));
    });

    it("pads a statement's outrefs to a power of two, so requests of 1 to 64 outrefs share seven statement texts, answered unchanged", async () => {
      const seen: Observed = { transactions: 0, queries: 0, texts: new Set() };
      for (let length = 1; length <= 64; length += 1) {
        const request = Array.from({ length }, (_, index) =>
          big(SPENT_COUNT + index),
        );
        const answer = await fresh(seen).getUtxosByOutRef(
          request.map(lucidRef),
        );
        expect(refs(answer)).toEqual(request.map(key));
      }
      expect(seen.texts?.size).toBe(7);
    });

    // Last: it applies a block that spends stored rows.
    it("reads every stored row of a call at one snapshot: a block applied mid-read is not seen", async () => {
      // In index order: the rows the block spends are read by a later
      // statement than the first.
      const request = Array.from({ length: COUNT }, (_, index) => big(index));
      let applying: Promise<unknown> | undefined;
      const seen: Observed = {
        transactions: 0,
        queries: 0,
        afterFirstQuery: async () => {
          applying = store.applyBlock(lateBlock);
          // Postgres: the write commits while the read's snapshot stays.
          // SQLite: one connection, so the write waits for the read to end.
          if (adapter.name === "postgres") await applying;
        },
      };
      const answer = await fresh(seen).getUtxosByOutRef(request.map(lucidRef));
      expect(await applying).toMatchObject({ kind: "applied" });
      expect(refs(answer)).toEqual(expected(request));
      expect(seen.transactions).toBe(1);
      expect(seen.queries).toBeGreaterThan(1);
      const after = await fresh().getUtxosByOutRef(request.map(lucidRef));
      expect(refs(after)).toEqual(
        expected(request).slice(0, COUNT - SPENT_COUNT - LATE_SPENT),
      );
    });
  },
);

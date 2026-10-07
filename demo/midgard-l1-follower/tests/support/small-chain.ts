import { join } from "node:path";

import { expect } from "vitest";

import {
  type BlockSummary,
  type FactStore,
  type FactStoreOptions,
  openPostgresFactStore,
  openSqliteFactStore,
  type OutputSummary,
  type Point,
  type TxSummary,
} from "../../src/index.js";
import type { testDatabases } from "./postgres.js";

/**
 * A small hand-built chain for the store unit tests:
 * origin(100) <- b1(101) <- b2(103) <- b3(105).
 */
export const fill = (byte: number, length = 32): Buffer =>
  Buffer.alloc(length, byte);
export const CRED = fill(0x11, 28);
export const TRACKED = Buffer.concat([Buffer.from([0x70]), CRED]);
export const UNTRACKED = Buffer.concat([Buffer.from([0x70]), fill(0x22, 28)]);
export const POLICY = fill(0x33, 28);
export const ORIGIN = { point: { slot: 100, hash: fill(0xa0) }, height: 50 };

export const output = (
  address: Buffer,
  lovelace: bigint,
  withToken = false,
): OutputSummary => ({
  address,
  paymentCredential: {
    hash: Buffer.from(address.subarray(1, 29)),
    isScript: true,
  },
  stakeCredential: null,
  lovelace,
  assets: withToken
    ? new Map([[POLICY.toString("hex"), new Map([["aa", 3n]])]])
    : new Map(),
  datumHash: null,
  datum: null,
  scriptRef: null,
});

export const tx = (hash: Buffer, fields: Partial<TxSummary>): TxSummary => ({
  hash,
  index: 0,
  isValid: true,
  bodyCbor: Buffer.from([0xa0]),
  witnessCbor: Buffer.from([0xa0]),
  auxCbor: null,
  inputs: [],
  referenceInputs: [],
  collaterals: [],
  outputs: [],
  collateralReturn: null,
  mint: new Map(),
  withdrawals: [],
  redeemers: [],
  invalidBefore: null,
  invalidAfter: null,
  ...fields,
});

export const TX1 = fill(0xb1);
export const TX2 = fill(0xb2);
export const TX3 = fill(0xb3);

/** origin(100) <- b1(101) <- b2(103) <- b3(105); tracked outputs are TX1#0, TX2#0, TX3#1. */
export const chain = (): BlockSummary[] => {
  const b1: BlockSummary = {
    point: { slot: 101, hash: fill(0xc1) },
    height: 51,
    parentHash: ORIGIN.point.hash,
    txs: [
      tx(TX1, {
        inputs: [{ txHash: fill(0x01), index: 0 }],
        outputs: [output(TRACKED, 5_000_000n, true), output(UNTRACKED, 1n)],
      }),
    ],
  };
  const b2: BlockSummary = {
    point: { slot: 103, hash: fill(0xc2) },
    height: 52,
    parentHash: b1.point.hash,
    txs: [
      tx(TX2, {
        inputs: [{ txHash: TX1, index: 0 }],
        outputs: [output(TRACKED, 2_000_000n)],
      }),
    ],
  };
  // A phase-2 failure: consumes its collateral and creates its collateral return at index outputs.length.
  const b3: BlockSummary = {
    point: { slot: 105, hash: fill(0xc3) },
    height: 53,
    parentHash: b2.point.hash,
    txs: [
      tx(TX3, {
        isValid: false,
        inputs: [{ txHash: fill(0x02), index: 0 }],
        collaterals: [{ txHash: TX2, index: 0 }],
        outputs: [output(UNTRACKED, 9n)],
        collateralReturn: output(TRACKED, 1_500_000n),
      }),
    ],
  };
  return [b1, b2, b3];
};

export const options = (k: number): FactStoreOptions => ({
  securityParameter: k,
  trackedSet: {
    addresses: new Set([TRACKED.toString("hex")]),
    paymentCredentials: new Set(),
    policies: new Set([POLICY.toString("hex")]),
  },
});

export type Adapter = Readonly<{
  name: "sqlite" | "postgres";
  /** A fresh store, plus a function that reopens a new store on the same database. */
  open: (
    k: number,
  ) => Promise<{ store: FactStore; reopen: () => FactStore; url?: string }>;
}>;

export const storeAdapters = (
  databases: ReturnType<typeof testDatabases>,
  scratch: string,
): readonly Adapter[] => [
  {
    name: "sqlite",
    open: async (k) => {
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      const reopen = (): FactStore =>
        openSqliteFactStore({ ...options(k), path });
      return { store: reopen(), reopen };
    },
  },
  {
    name: "postgres",
    open: async (k) => {
      const { url } = await databases.create();
      const reopen = (): FactStore =>
        openPostgresFactStore({
          ...options(k),
          connection: { connectionString: url },
        });
      return { store: reopen(), reopen, url };
    },
  },
];

export const started = async (adapter: Adapter, k = 2, blocks = 3) => {
  const opened = await adapter.open(k);
  expect(await opened.store.start()).toMatchObject({
    kind: "ready",
    cursor: null,
  });
  expect(await opened.store.initialize(ORIGIN)).toMatchObject({
    kind: "initialized",
  });
  for (const block of chain().slice(0, blocks))
    expect(await opened.store.applyBlock(block)).toMatchObject({
      kind: "applied",
    });
  return opened;
};

export const point = (block: BlockSummary | undefined): Point => {
  if (block === undefined) throw new Error("no block");
  return block.point;
};

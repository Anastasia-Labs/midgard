/**
 * The intent-journal test harness: a fact store (SQLite by default, or one
 * a test opens) with the intent projection over the simulated chain, the
 * same chain as a model, and the S6 reconciler with every dependency
 * answering "go".
 */
import { expect } from "vitest";

import {
  type BlockSummary,
  createIntentReconciler,
  decodeBlock,
  deriveIntentStatusesIn,
  type DialectName,
  type FactStore,
  type FactStoreOptions,
  intentJournalProjection,
  type IntentReconcilerOptions,
  type IntentState,
  type IntentStatus,
  openSqliteFactStore,
  type OutRef,
  projectionStoreOptions,
  type RecordIntentResult,
} from "../../src/index.js";
import {
  encodeSimTx,
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simTxHash,
  simUniverse,
} from "../../src/testing/index.js";
import { recordAtCurrentView } from "./record-at-view.js";

export const K = 3;
export const u = simUniverse();

export type Harness = {
  store: FactStore;
  chain: SimChain;
  /** The model's chain above the origin, as decoded blocks. */
  blocks: BlockSummary[];
  /** Appends a block of `txs` to the model and applies it to the store. */
  forward: (txs?: readonly SimTx[]) => Promise<void>;
  /** Rolls the model and the store back `depth` blocks. */
  backward: (depth: number) => Promise<void>;
  record: (
    tx: SimTx,
    extra?: Partial<{
      txCbor: Buffer;
      contentRef: Buffer;
      workflowKey: string;
    }>,
  ) => Promise<RecordIntentResult>;
  statuses: () => Promise<Map<string, IntentStatus>>;
  states: () => Promise<Map<string, IntentState>>;
  /** A funded tracked output, created in its own block. */
  fund: () => Promise<OutRef>;
};

/** The harness store's options for a dialect. */
export const harnessStoreOptions = (dialect: DialectName): FactStoreOptions =>
  projectionStoreOptions(
    [intentJournalProjection],
    { securityParameter: K, trackedSet: u.tracked },
    dialect,
  );

export const open = async (
  openStore: () => Promise<FactStore> | FactStore = () =>
    openSqliteFactStore({
      ...harnessStoreOptions("sqlite"),
      path: ":memory:",
    }),
): Promise<Harness> => {
  const store = await openStore();
  await store.start();
  const chain = new SimChain(u, SIM_ORIGIN);
  const blocks: BlockSummary[] = [];
  const harness: Harness = {
    store,
    chain,
    blocks,
    forward: async (txs = []) => {
      const { encoded } = chain.forward(txs);
      const block = decodeBlock(encoded.raw);
      blocks.push(block);
      const applied = await store.applyBlock(block);
      expect(applied.kind).toBe("applied");
    },
    backward: async (depth) => {
      chain.backward(depth);
      blocks.splice(blocks.length - depth, depth);
      const rewound = await store.rewind(chain.tip.point);
      expect(rewound.kind).toBe("rewound");
    },
    record: (tx, extra = {}) =>
      store.transaction("write", (sql) =>
        recordAtCurrentView(sql, store.dialect, {
          family: "commit",
          workflowKey:
            extra.workflowKey ?? `commit:${simTxHash(tx).toString("hex")}`,
          txCbor: extra.txCbor ?? encodeSimTx(tx),
          isOwnOutput: (output) => output.address.equals(u.trackedAddress),
          ...(extra.contentRef === undefined
            ? {}
            : { contentRef: extra.contentRef }),
        }),
      ),
    states: async () =>
      new Map(
        (
          await store.transaction("read", (sql) =>
            deriveIntentStatusesIn(sql, store.dialect),
          )
        ).states.map((s) => [s.intent.txHash.toString("hex"), s]),
      ),
    statuses: async () =>
      new Map(
        (
          await store.transaction("read", (sql) =>
            deriveIntentStatusesIn(sql, store.dialect),
          )
        ).states.map((s) => [s.intent.txHash.toString("hex"), s.status]),
      ),
    fund: async () => {
      const funding: SimTx = {
        inputs: [chain.outsideInput()],
        outputs: [{ address: u.trackedAddress, lovelace: 10_000_000n }],
        nonce: chain.nonce(),
      };
      await harness.forward([funding]);
      return { txHash: simTxHash(funding), index: 0 };
    },
  };
  return harness;
};

export const spend = (
  chain: SimChain,
  input: OutRef,
  extra: Partial<SimTx> = {},
): SimTx => ({
  inputs: [input],
  outputs: [
    { address: u.trackedAddress, lovelace: 4_000_000n },
    { address: u.untrackedAddress, lovelace: 1_000_000n },
  ],
  nonce: chain.nonce(),
  ...extra,
});

export const hex = (tx: SimTx): string => simTxHash(tx).toString("hex");

export const s6 = (
  store: FactStore,
  overrides: Partial<IntentReconcilerOptions> = {},
) =>
  createIntentReconciler({
    dialect: store.dialect,
    transaction: (mode, run) => store.transaction(mode, run),
    securityParameter: K,
    inMempool: () => Promise.resolve(false),
    wanted: () => Promise.resolve(true),
    submit: () => Promise.resolve({ kind: "accepted" }),
    ...overrides,
  });

export const actionOf = async (
  reconciler: ReturnType<typeof s6>,
  tx: SimTx,
): Promise<string | undefined> =>
  (await reconciler.reconcile()).entry(simTxHash(tx))?.action;

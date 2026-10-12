import { TransportRequestError } from "@al-ft/l1-node-transport";
import type {
  FraudProofRawL1Point,
  FraudProofRawL1Utxo,
} from "@al-ft/midgard-fault-proofs";
import {
  applyChainSyncEvent,
  decodeLedgerUtxos,
  type DialectName,
  type FactStore,
  type FactStoreOptions,
  openSqliteFactStore,
  type WalletLedger,
} from "@al-ft/midgard-l1-follower";
import {
  encodeTxBody,
  encodeUtxoAnswer,
  SIM_ORIGIN,
  SimChain,
  type SimOutput,
  simStoreOptions,
  type SimTx,
  simUniverse,
  type SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  watcherProjection,
  type WatcherProjectionDeployment,
  watcherUnitHistoryPolicies,
} from "../../src/l1-follower/projection.js";
import { createFollowerRawReads } from "../../src/l1-follower/raw-reads.js";
import { ledgerOutputsFromTransport } from "../../src/l1-follower/raw-reads.ledger.js";
import {
  type FollowerRawReads,
  rawPointOf,
  type RawRead,
} from "../../src/l1-follower/raw-reads.types.js";
import {
  commitTx,
  initTx,
  queueState,
  SIM_WATCHER_DEPLOYMENT,
} from "./l1-follower-state-queue-traffic.js";

/**
 * The hand-built simulator chain the follower raw-read tests (ticket W1)
 * read, and their oracle: the simulator's own transaction specs. The exact
 * bytes an output must read back as come from the spec's encoding, never
 * from the store.
 */

export const K = 3;
export const D = SIM_WATCHER_DEPLOYMENT;
const U = simUniverse();
export const T = U.trackedAddress;
export const C = U.credentialAddress;
export const X = U.untrackedAddress;
const UNIT_DATUM = Buffer.from("d87980", "hex");

export const hex = (bytes: Buffer): string => bytes.toString("hex");
export const bech32 = (bytes: Buffer): string => {
  const address = CML.Address.from_raw_bytes(bytes);
  try {
    return address.to_bech32();
  } finally {
    address.free();
  }
};

/** The output a spec creates at `index` (past the outputs: its collateral return), as the raw source reads it. */
export const expected = (
  tx: SimTx,
  txHash: string,
  index: number,
): FraudProofRawL1Utxo => {
  const body = CML.TransactionBody.from_cbor_bytes(encodeTxBody(tx));
  try {
    const sim = (
      index < tx.outputs.length ? tx.outputs[index] : tx.collateralReturn
    ) as SimOutput;
    const output =
      index < tx.outputs.length
        ? body.outputs().get(index)
        : (body.collateral_return() as CML.TransactionOutput);
    return {
      outRef: `${txHash}#${index.toString()}`,
      outputCbor: output.to_canonical_cbor_hex(),
      datumCbor: sim.datum === undefined ? null : hex(sim.datum),
      referenceScriptCbor: null,
    };
  } finally {
    body.free();
  }
};

/** A pre-origin wallet UTxO: the store only ever holds it as a seed row. */
export const SEED: SimUtxo = {
  outRef: { txHash: Buffer.alloc(32, 0x33), index: 0 },
  output: { address: T, lovelace: 5_000_000n },
};
export const SEED_LABEL = `${"33".repeat(32)}#0`;
export const seedExpected = (): FraudProofRawL1Utxo => ({
  ...expected({ inputs: [], outputs: [SEED.output], nonce: 0 }, "", 0),
  outRef: SEED_LABEL,
});

export const byOutRef = <T extends Readonly<{ outRef: string }>>(
  items: readonly T[],
): T[] =>
  [...items].sort((a, b) =>
    a.outRef < b.outRef ? -1 : a.outRef > b.outRef ? 1 : 0,
  );

export const okValue = <T>(read: RawRead<T>): T => {
  if (read.kind !== "ok")
    throw new Error(`expected ok, got ${read.reason}: ${read.detail}`);
  return read.value;
};

export const reasonOf = (read: RawRead<unknown>): string =>
  read.kind === "ok" ? "ok" : read.reason;

type Landed = Readonly<{ hashes: string[]; point: FraudProofRawL1Point }>;

/**
 * A store following a simulator chain, plus the node's ledger state at each
 * block of the current chain (for the LocalStateQuery seam): acquirable only
 * within k of the tip, as the node's volatile window.
 */
export const harness = async (
  deployment: WatcherProjectionDeployment = D,
  /** Opens the store on another dialect (default: in-memory SQLite). */
  open?: Readonly<{
    dialect: DialectName;
    store: (options: FactStoreOptions) => FactStore;
  }>,
) => {
  const options = simStoreOptions(
    [watcherProjection(deployment)],
    K,
    open?.dialect ?? "sqlite",
  );
  const store =
    open === undefined
      ? openSqliteFactStore({ ...options, path: ":memory:" })
      : open.store(options);
  expect((await store.start()).kind).toBe("ready");
  expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  const seeded = await store.insertSeedOutputs(
    SIM_ORIGIN.point,
    decodeLedgerUtxos(encodeUtxoAnswer([SEED])).map(({ outRef, output }) => ({
      outRef,
      output,
    })),
  );
  expect(seeded?.kind).toBe("seeded");
  const chain = new SimChain(U, SIM_ORIGIN, store.trackedSet(), [SEED]);
  const ledger = new Map<string, { height: number; utxos: SimUtxo[] }>([
    [hex(SIM_ORIGIN.point.hash), { height: SIM_ORIGIN.height, utxos: [SEED] }],
  ]);
  const tipPoint = (): FraudProofRawL1Point =>
    rawPointOf({
      slot: chain.tip.point.slot,
      hash: chain.tip.point.hash,
      height: chain.tip.height,
    });
  const onChain: string[] = [];
  const forward = async (txs: readonly SimTx[]): Promise<Landed> => {
    const { event, encoded } = chain.forward(txs);
    const applied = await applyChainSyncEvent(store, event);
    expect(applied.result.kind).toBe("applied");
    onChain.push(hex(encoded.hash));
    ledger.set(hex(encoded.hash), {
      height: chain.tip.height,
      utxos: chain.live(),
    });
    return { hashes: encoded.txHashes.map(hex), point: tipPoint() };
  };
  const backward = async (depth: number): Promise<void> => {
    const event = chain.backward(depth);
    expect((await applyChainSyncEvent(store, event)).result.kind).toBe(
      "rewound",
    );
    for (const hash of onChain.splice(-depth)) ledger.delete(hash);
  };
  const pruneAll = async (): Promise<number> => {
    for (;;) {
      const pruned = await store.prune(1_000);
      if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
      if (pruned.done) return pruned.prunedThroughSlot;
    }
  };
  const node: WalletLedger = {
    withLedgerState: async (at, use) => {
      const state =
        at !== "tip" && at.kind === "point" ? ledger.get(at.hash) : undefined;
      if (state === undefined || state.height < chain.tip.height - K)
        throw new TransportRequestError(
          "acquire_point_too_old",
          "the point is more than k blocks below the tip",
        );
      return await use({
        query: async (query) => {
          if (query.query !== "utxo_by_txin")
            throw new Error(`unexpected query ${query.query}`);
          const wanted = new Set(
            query.txIns.map(({ txId, index }) => `${txId}#${index.toString()}`),
          );
          return encodeUtxoAnswer(
            state.utxos.filter(({ outRef }) =>
              wanted.has(`${hex(outRef.txHash)}#${outRef.index.toString()}`),
            ),
          );
        },
      });
    },
  };
  const reads = (withLedger = false): FollowerRawReads =>
    createFollowerRawReads(store, {
      stateQueuePolicyId: deployment.stateQueueMint,
      unitHistoryPolicies: watcherUnitHistoryPolicies(deployment),
      ...(withLedger
        ? { ledgerOutputsAt: ledgerOutputsFromTransport(node) }
        : {}),
    });
  return { store, chain, forward, backward, pruneAll, reads, tipPoint, node };
};

/**
 * The fixture chain:
 * - b1: A pays T (A#0, with a datum), X (A#1, untracked) and C (A#2); U
 *   pays X only (never stored).
 * - b2: B spends A#0, A#1 and the seed, references A#2, pays T; F fails
 *   phase 2, consuming A#2 as collateral, its return to C (F#1).
 * - b3: V spends U#0 (no stored body) and pays T; G spends F#1 and pays C.
 * - b4: the protocol init; b5: a header commit.
 */
export const fixture = async () => {
  const h = await harness();
  const A: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [
      { address: T, lovelace: 2_000_000n, datum: UNIT_DATUM },
      { address: X, lovelace: 1_000_000n },
      { address: C, lovelace: 3_000_000n },
    ],
    nonce: h.chain.nonce(),
  };
  const Utx: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [{ address: X, lovelace: 4_000_000n }],
    nonce: h.chain.nonce(),
  };
  const b1 = await h.forward([A, Utx]);
  const [a, u] = b1.hashes as [string, string];
  const out = (txHash: string, index: number) => ({
    txHash: Buffer.from(txHash, "hex"),
    index,
  });
  const B: SimTx = {
    inputs: [out(a, 0), out(a, 1), SEED.outRef],
    referenceInputs: [out(a, 2)],
    outputs: [{ address: T, lovelace: 1_000_000n }],
    nonce: h.chain.nonce(),
  };
  const F: SimTx = {
    inputs: [out(a, 2)],
    collaterals: [out(a, 2)],
    outputs: [{ address: T, lovelace: 9_000_000n }],
    collateralReturn: { address: C, lovelace: 2_500_000n },
    isValid: false,
    nonce: h.chain.nonce(),
  };
  const b2 = await h.forward([B, F]);
  const [b, f] = b2.hashes as [string, string];
  const V: SimTx = {
    inputs: [out(u, 0)],
    outputs: [{ address: T, lovelace: 3_000_000n }],
    nonce: h.chain.nonce(),
  };
  const G: SimTx = {
    inputs: [out(f, 1)],
    outputs: [{ address: C, lovelace: 1_000_000n }],
    nonce: h.chain.nonce(),
  };
  const b3 = await h.forward([V, G]);
  const [v, g] = b3.hashes as [string, string];
  const b4 = await h.forward([initTx(D)]);
  const commit = commitTx(queueState(h.chain, D)!, D);
  const b5 = await h.forward([commit]);
  const header = [
    ...(commit.mint?.get(D.stateQueueMint)?.keys() ?? []),
  ][0]!.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
  return {
    ...h,
    specs: { A, U: Utx, B, F, V, G },
    hashes: { a, u, b, f, v, g, commit: b5.hashes[0]! },
    points: {
      p1: b1.point,
      p2: b2.point,
      p3: b3.point,
      p4: b4.point,
      p5: b5.point,
    },
    header,
  };
};

export type Fixture = Awaited<ReturnType<typeof fixture>>;

/** Moves the tip far enough that b1–b5 sit at or below the pruned window. */
export const pruneFixture = async (fx: Fixture): Promise<number> => {
  for (let i = 0; i < K + 4; i += 1) await fx.forward([]);
  const prunedThrough = await fx.pruneAll();
  expect(prunedThrough).toBeGreaterThanOrEqual(Number(fx.points.p5.slot));
  return prunedThrough;
};

export const random = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

import type { FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";
import {
  diffPruned,
  dumpRetained,
  dumpStore,
  simStoreOptions,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import { expect } from "vitest";

import { watcherProjection } from "../../src/l1-follower/projection.js";
import {
  C,
  D,
  harness,
  K,
  SEED,
  T,
  X,
} from "./l1-follower-raw-reads-fixture.js";
import {
  commitTx,
  initTx,
  queueState,
} from "./l1-follower-state-queue-traffic.js";

/**
 * Two follower stores fed the same simulator events (ticket W1, ruling 9):
 * `pruned` is pruned on demand, `reference` never is. After each prune the
 * pruned store must hold only rows of the reference and every row retention
 * keeps (`diffPruned`), across rollbacks before and after the pruning.
 */

const PINS =
  simStoreOptions([watcherProjection(D)], K, "sqlite").retentionPins ?? {};

type Landed = Readonly<{ hashes: string[]; point: FraudProofRawL1Point }>;

export const twin = async () => {
  const pruned = await harness();
  const reference = await harness();
  const forward = async (txs: readonly SimTx[]): Promise<Landed> => {
    const landed = await pruned.forward(txs);
    // The same events on both: the same chain, block for block.
    expect(await reference.forward(txs)).toEqual(landed);
    return landed;
  };
  const empty = async (count: number): Promise<FraudProofRawL1Point[]> => {
    const points: FraudProofRawL1Point[] = [];
    for (let i = 0; i < count; i += 1) points.push((await forward([])).point);
    return points;
  };
  const backward = async (depth: number): Promise<void> => {
    await pruned.backward(depth);
    await reference.backward(depth);
  };
  /** Prunes the pruned store; null when it holds exactly what retention keeps. */
  const pruneAndDiff = async (): Promise<
    Readonly<{ prunedThrough: number; diff: string | null }>
  > => {
    const prunedThrough = await pruned.pruneAll();
    const diff = diffPruned(
      await dumpStore(pruned.store),
      await dumpStore(reference.store),
      await dumpRetained(reference.store, PINS, prunedThrough),
    );
    return { prunedThrough, diff };
  };
  return {
    pruned,
    reference,
    chain: pruned.chain,
    forward,
    empty,
    backward,
    pruneAndDiff,
  };
};

export const outRef = (txHash: string, index: number) => ({
  txHash: Buffer.from(txHash, "hex"),
  index,
});

/**
 * The twin chain the ruling-9 test prunes and rolls back: A (b1) pays T#0,
 * T#1, C#2; B (b2) spends A#0 and the seed, pays C; the protocol init (b3)
 * and a header commit (b4); two empty blocks; R (b7) spends A#1 to X; one
 * empty block (tip b8).
 */
export const twinScenario = async () => {
  const w = await twin();
  const A: SimTx = {
    inputs: [w.chain.outsideInput()],
    outputs: [
      { address: T, lovelace: 2_000_000n },
      { address: T, lovelace: 3_000_000n },
      { address: C, lovelace: 4_000_000n },
    ],
    nonce: w.chain.nonce(),
  };
  const a = (await w.forward([A])).hashes[0]!;
  const B: SimTx = {
    inputs: [outRef(a, 0), SEED.outRef],
    outputs: [{ address: C, lovelace: 1_000_000n }],
    nonce: w.chain.nonce(),
  };
  const b = (await w.forward([B])).hashes[0]!;
  await w.forward([initTx(D)]);
  const commit = commitTx(queueState(w.chain, D)!, D);
  const landedCommit = await w.forward([commit]);
  const header = [...(commit.mint?.get(D.stateQueueMint)?.keys() ?? [])][0]!;
  const unit = `${D.stateQueueMint}${header}`;
  await w.empty(2);
  const R: SimTx = {
    inputs: [outRef(a, 1)],
    outputs: [{ address: X, lovelace: 3_000_000n }],
    nonce: w.chain.nonce(),
  };
  const landedR = await w.forward([R]);
  const r = landedR.hashes[0]!;
  await w.empty(1);
  return { w, A, a, B, b, unit, landedCommit, R, r, landedR };
};

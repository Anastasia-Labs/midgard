/**
 * The chain a lucid `Emulator` makes, as the follower reads it: an origin
 * (the ledger and slot at the emulator's first observation: its genesis, or
 * a restored ledger recorded without a chain) and one block per emulator
 * block height. A clock step that raises the height by n records n blocks:
 * the first holds the exact bytes of the submitted transactions the step
 * confirmed, in submission order, and the rest are empty.
 *
 * - `Emulator.prototype.submitTx`, `awaitBlock` and `awaitSlot` are wrapped
 *   when this module loads, so a chain is complete from the first
 *   submission. Every caller of the emulator (the follower provider, a test,
 *   a deployment) goes through them.
 * - The record is plain data in an own property of the emulator, so every
 *   snapshot of the emulator's state (`emulatorState`, a fixture shared
 *   across the run's processes) carries it, and a restored emulator's chain
 *   is the chain its ledger was made by. A rollback that keeps the clock
 *   (`rollBackEmulatorChain`) restores it with the ledger
 *   (`snapshotFollowedChain`, `restoreFollowedChain`). A ledger put back
 *   without its record (the ledger again holds an output a recorded block
 *   consumed) drops that block and every block after it.
 * - Block hashes are the hashes of the raw blocks the follower decodes
 *   (`encodeFollowerBlock`): a function of the transactions, the parent and
 *   the slot, so two emulators restored from one snapshot share the blocks
 *   it holds.
 */
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { decodeBlock } from "@al-ft/midgard-l1-follower";
import { Emulator, type UTxO } from "@lucid-evolution/lucid";

import {
  encodeFollowerBlock,
  stateOf,
  utxoAnswer,
} from "./follower-emulator.ledger.js";

const FOLLOWED = "followedChain";

/** The emulator's block spacing: one block height per 20 slots. */
export const EMULATOR_SLOTS_PER_BLOCK = 20;

export type FollowedBlock = Readonly<{
  hash: string;
  slot: number;
  /** The emulator's block height after the change that made this block. */
  emulatorHeight: number;
  /** The confirmed transactions' exact bytes, as hex. */
  txs: readonly string[];
  /** The ledger keys of the outputs the block consumed. */
  consumed: readonly string[];
}>;

export type FollowedChain = {
  readonly origin: Readonly<{
    slot: number;
    hash: string;
    /** The unspent ledger outputs at the origin. */
    ledger: readonly UTxO[];
  }>;
  blocks: FollowedBlock[];
  /** Accepted submissions not yet confirmed, in submission order. */
  pending: { hash: string; cbor: string }[];
};

type Following = Emulator & { [FOLLOWED]?: FollowedChain };

const originOf = (emulator: Emulator): FollowedChain["origin"] => {
  const state = stateOf(emulator);
  const ledger = Object.values(state.ledger).flatMap(({ utxo, spent }) =>
    spent ? [] : [structuredClone(utxo)],
  );
  const slot = Buffer.alloc(8);
  slot.writeBigUInt64BE(BigInt(state.slot));
  return {
    slot: state.slot,
    hash: computeHash32(Buffer.concat([utxoAnswer(ledger), slot])).toString(
      "hex",
    ),
    ledger,
  };
};

/**
 * Drops the blocks a restored emulator never made: those above its block
 * height (a state put back over a record it did not carry), and from the
 * first block whose consumed outputs its ledger holds again (a ledger put
 * back under a clock that kept running).
 */
const reconcile = (emulator: Emulator, chain: FollowedChain): void => {
  const ledger = stateOf(emulator).ledger;
  const kept = chain.blocks.findIndex(
    ({ emulatorHeight, consumed }) =>
      emulatorHeight > emulator.blockHeight ||
      consumed.some((key) => ledger[key] !== undefined),
  );
  if (kept >= 0) chain.blocks.splice(kept);
};

/** `emulator`'s chain, recording its origin at the first observation. */
export const followedChainOf = (emulator: Emulator): FollowedChain => {
  const following = emulator as Following;
  const chain = (following[FOLLOWED] ??= {
    origin: originOf(emulator),
    blocks: [],
    pending: [],
  });
  reconcile(emulator, chain);
  return chain;
};

/** The point of the block at `index` (-1: the origin). */
export const followedPoint = (
  chain: FollowedChain,
  index: number,
): Readonly<{ slot: number; hash: Buffer }> => {
  const at = index < 0 ? chain.origin : chain.blocks[index]!;
  return { slot: at.slot, hash: Buffer.from(at.hash, "hex") };
};

/** The raw block at `index`, as the local node serves it. */
export const followedBlockBytes = (
  chain: FollowedChain,
  index: number,
): Buffer => {
  const block = chain.blocks[index]!;
  return encodeFollowerBlock(
    block.txs,
    { hash: followedPoint(chain, index - 1).hash, height: index },
    block.slot,
  );
};

/** Appends the block holding `txs` at `slot` (above its parent's) to `chain`. */
const appendBlock = (
  chain: FollowedChain,
  emulatorHeight: number,
  slot: number,
  txs: readonly string[],
): void => {
  const index = chain.blocks.length;
  const parent = followedPoint(chain, index - 1);
  const at = Math.max(slot, parent.slot + 1);
  const raw = encodeFollowerBlock(
    txs,
    { hash: parent.hash, height: index },
    at,
  );
  const block = decodeBlock(raw);
  chain.blocks.push({
    hash: block.point.hash.toString("hex"),
    slot: at,
    emulatorHeight,
    txs,
    consumed: block.txs.flatMap((tx) =>
      (tx.isValid ? tx.inputs : tx.collaterals).map(
        ({ txHash, index }) => `${txHash.toString("hex")}${index.toString()}`,
      ),
    ),
  });
};

/**
 * Records the blocks a change of `emulator`'s height from `before` made: one
 * per height, as the emulator counts them (one per 20 slots), so the
 * followed chain is as dense as the emulator's clock says. The blocks are 20
 * slots apart, ending at the emulator's slot; the first holds the confirmed
 * transactions, as the emulator confirms them at the first new height.
 */
const recordBlock = (emulator: Emulator, before: number): void => {
  if (emulator.blockHeight <= before) return;
  const chain = followedChainOf(emulator);
  const history = stateOf(emulator).transactionHistory;
  const confirmed = chain.pending.filter(
    ({ hash }) => history[hash]?.status === "confirmed",
  );
  // A submission the emulator dropped is gone with its history entry.
  chain.pending = chain.pending.filter(
    ({ hash }) => history[hash]?.status === "pending",
  );
  for (let height = before + 1; height <= emulator.blockHeight; height += 1)
    appendBlock(
      chain,
      height,
      emulator.slot -
        EMULATOR_SLOTS_PER_BLOCK * (emulator.blockHeight - height),
      height === before + 1 ? confirmed.map(({ cbor }) => cbor) : [],
    );
};

const submitTx = Emulator.prototype.submitTx;
Emulator.prototype.submitTx = function (
  this: Emulator,
  cbor: string,
): Promise<string> {
  followedChainOf(this);
  return submitTx.call(this, cbor).then((hash) => {
    const chain = followedChainOf(this);
    if (!chain.pending.some((entry) => entry.hash === hash))
      chain.pending.push({ hash, cbor });
    return hash;
  });
};

const awaitBlock = Emulator.prototype.awaitBlock;
Emulator.prototype.awaitBlock = function (this: Emulator, height?: number) {
  followedChainOf(this);
  const before = this.blockHeight;
  awaitBlock.call(this, height);
  recordBlock(this, before);
};

const awaitSlot = Emulator.prototype.awaitSlot;
Emulator.prototype.awaitSlot = function (this: Emulator, length?: number) {
  followedChainOf(this);
  const before = this.blockHeight;
  awaitSlot.call(this, length);
  recordBlock(this, before);
};

/** The record as plain data, for a capture that copies fields one by one. */
export const snapshotFollowedChain = (emulator: Emulator): FollowedChain =>
  structuredClone(followedChainOf(emulator));

/** Gives `emulator` back a record `snapshotFollowedChain` took. */
export const restoreFollowedChain = (
  emulator: Emulator,
  snapshot: FollowedChain,
): void => {
  (emulator as Following)[FOLLOWED] = structuredClone(snapshot);
};

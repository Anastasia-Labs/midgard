import {
  type BlockPoint,
  chainPoint,
  type ChainTip,
  type RollBackward,
  type RollForward,
} from "@al-ft/l1-node-transport";

import { encodeOutRef, outRefKey } from "../codec.js";
import type { OutRef, Point, TrackedSet } from "../types.js";
import {
  encodeBlock,
  type EncodedBlock,
  type SimBlock,
  type SimOutput,
  type SimTx,
} from "./block-cbor.js";

/** The fixed addresses and policies the simulator's traffic uses. */
export type SimUniverse = Readonly<{
  tracked: TrackedSet;
  /** An enterprise script address in the tracked set. */
  trackedAddress: Buffer;
  /** A base address whose payment credential is tracked (stake part free). */
  credentialAddress: Buffer;
  /** An address nothing tracks. */
  untrackedAddress: Buffer;
  trackedPolicy: string;
  untrackedPolicy: string;
}>;

const fill = (byte: number, length: number): Buffer =>
  Buffer.alloc(length, byte);

export const simUniverse = (): SimUniverse => {
  const trackedAddress = Buffer.concat([Buffer.of(0x70), fill(0x5a, 28)]);
  const credential = fill(0x5b, 28);
  return {
    tracked: {
      addresses: new Set([trackedAddress.toString("hex")]),
      paymentCredentials: new Set([credential.toString("hex")]),
      policies: new Set([fill(0x5c, 28).toString("hex")]),
    },
    trackedAddress,
    credentialAddress: Buffer.concat([
      Buffer.of(0x00),
      credential,
      fill(0x5d, 28),
    ]),
    untrackedAddress: Buffer.concat([Buffer.of(0x70), fill(0x5e, 28)]),
    trackedPolicy: fill(0x5c, 28).toString("hex"),
    untrackedPolicy: fill(0x5f, 28).toString("hex"),
  };
};

export type SimUtxo = Readonly<{ outRef: OutRef; output: SimOutput }>;

type Undo = { added: string[]; removed: SimUtxo[] };

type Entry = Readonly<{
  block: SimBlock;
  encoded: EncodedBlock;
  point: Point;
  undo: Undo;
}>;

export type SimOrigin = Readonly<{ point: Point; height: number }>;

/** The N2C block type of a Conway block. */
export const CONWAY_BLOCK_TYPE = 7;

/** An outref as 68 hex characters (tx hash, then the u16 index). */
export const outRefHex = (outRef: OutRef): string =>
  encodeOutRef(outRef).toString("hex");

const blockPoint = (point: Point): BlockPoint =>
  chainPoint(BigInt(point.slot), point.hash.toString("hex"));

/**
 * The simulated node: its current chain above the origin, the full ledger
 * UTxO set at its tip (with an undo log per block) and the chain-sync event
 * sequence it serves. A rollback starts a new branch, so blocks at a height
 * already seen get different hashes.
 */
export class SimChain {
  private readonly entries: Entry[] = [];
  private readonly utxos = new Map<string, SimUtxo>();
  private seq = 0n;
  private branch = 0;
  private nonces = 0;

  constructor(
    readonly universe: SimUniverse,
    readonly origin: SimOrigin,
    private readonly tracked: TrackedSet = universe.tracked,
  ) {}

  get tip(): SimOrigin {
    const last = this.entries[this.entries.length - 1];
    return last === undefined
      ? this.origin
      : { point: last.point, height: last.block.height };
  }

  /** Blocks above the origin. */
  get length(): number {
    return this.entries.length;
  }

  /** The current chain's raw blocks, oldest first. */
  rawBlocks(): Buffer[] {
    return this.entries.map((entry) => entry.encoded.raw);
  }

  /** A fresh fee value, so two otherwise equal transactions differ. */
  nonce(): number {
    this.nonces += 1;
    return this.nonces;
  }

  /** A pre-origin UTxO nothing tracks, as an input for funding transactions. */
  outsideInput(): OutRef {
    const hash = Buffer.alloc(32);
    hash.writeUInt32BE(this.nonce(), 28);
    hash[0] = 0xee;
    return { txHash: hash, index: 0 };
  }

  isTracked(output: SimOutput): boolean {
    const type = (output.address[0] ?? 0xff) >> 4;
    return (
      this.tracked.addresses.has(output.address.toString("hex")) ||
      (type <= 7 &&
        output.address.length >= 29 &&
        this.tracked.paymentCredentials.has(
          output.address.subarray(1, 29).toString("hex"),
        ))
    );
  }

  isLive(outRef: OutRef): boolean {
    return this.utxos.has(outRefKey(outRef));
  }

  /** Every live UTxO, in insertion order. */
  live(): SimUtxo[] {
    return [...this.utxos.values()];
  }

  /** The keys of every live tracked UTxO, sorted (what the store must hold). */
  liveTracked(): string[] {
    return this.live()
      .filter((utxo) => this.isTracked(utxo.output))
      .map((utxo) => outRefHex(utxo.outRef))
      .sort();
  }

  private tipOf(point: Point, height: number): ChainTip {
    return { point: blockPoint(point), blockNo: BigInt(height) };
  }

  /** Appends a block of `txs` to the chain and serves its roll-forward. */
  forward(txs: readonly SimTx[]): Readonly<{
    event: RollForward;
    encoded: EncodedBlock;
  }> {
    const parent = this.tip;
    const height = parent.height + 1;
    const block: SimBlock = {
      height,
      slot: parent.point.slot + 1 + ((height + this.branch) % 2),
      prevHash: parent.point.hash,
      branch: this.branch,
      txs,
    };
    const encoded = encodeBlock(block);
    const undo: Undo = { added: [], removed: [] };
    txs.forEach((tx, index) => {
      const hash = encoded.txHashes[index] as Buffer;
      const valid = tx.isValid !== false;
      for (const outRef of valid ? tx.inputs : (tx.collaterals ?? [])) {
        const key = outRefKey(outRef);
        const utxo = this.utxos.get(key);
        if (utxo === undefined) continue;
        this.utxos.delete(key);
        undo.removed.push(utxo);
      }
      const created: SimUtxo[] = valid
        ? tx.outputs.map((output, i) => ({
            outRef: { txHash: hash, index: i },
            output,
          }))
        : tx.collateralReturn === undefined
          ? []
          : [
              {
                outRef: { txHash: hash, index: tx.outputs.length },
                output: tx.collateralReturn,
              },
            ];
      for (const utxo of created) {
        const key = outRefKey(utxo.outRef);
        this.utxos.set(key, utxo);
        undo.added.push(key);
      }
    });
    const point = { slot: block.slot, hash: encoded.hash };
    this.entries.push({ block, encoded, point, undo });
    this.seq += 1n;
    return {
      encoded,
      event: {
        kind: "roll_forward",
        seq: this.seq,
        point: blockPoint(point),
        blockNo: BigInt(height),
        blockType: CONWAY_BLOCK_TYPE,
        prevHash: parent.point.hash.toString("hex"),
        tip: this.tipOf(point, height),
        block: encoded.raw,
      },
    };
  }

  /**
   * Drops the top `depth` blocks, restoring the ledger, and serves the
   * roll-backward to the new tip. Later blocks belong to a new branch.
   */
  backward(depth: number): RollBackward {
    if (depth < 0 || depth > this.entries.length)
      throw new RangeError(`cannot roll back ${depth} of ${this.length}`);
    for (let i = 0; i < depth; i += 1) {
      const entry = this.entries.pop() as Entry;
      const added = new Set(entry.undo.added);
      for (const key of entry.undo.added) this.utxos.delete(key);
      for (const utxo of entry.undo.removed) {
        const key = outRefKey(utxo.outRef);
        // Created and spent inside the dropped blocks: it never existed before them.
        if (!added.has(key)) this.utxos.set(key, utxo);
      }
    }
    this.branch += 1;
    this.seq += 1n;
    const tip = this.tip;
    return {
      kind: "roll_backward",
      seq: this.seq,
      point: blockPoint(tip.point),
      tip: this.tipOf(tip.point, tip.height),
    };
  }
}

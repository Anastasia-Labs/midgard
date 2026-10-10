import {
  blake2b224,
  type BlockSummary,
  type OutputSummary,
  type OutRef,
  outRefKey,
  type Point,
  type ScriptRef,
  type TrackedSet,
  type TxSummary,
} from "../../src/index.js";
import type { Rng } from "../../src/testing/rng.js";

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** Fixed credentials and policies of the test universe. */
export type Universe = Readonly<{
  tracked: TrackedSet;
  trackedAddresses: readonly Buffer[];
  trackedCredential: Buffer;
  untrackedAddresses: readonly Buffer[];
  trackedPolicy: string;
  untrackedPolicy: string;
  scripts: readonly ScriptRef[];
}>;

export const makeUniverse = (rng: Rng): Universe => {
  const scriptCred = rng.bytes(28);
  const keyCred = rng.bytes(28);
  const trackedCredential = rng.bytes(28);
  const trackedAddresses = [
    Buffer.concat([Buffer.from([0x70]), scriptCred]),
    Buffer.concat([Buffer.from([0x60]), keyCred]),
  ];
  const untrackedAddresses = [
    Buffer.concat([Buffer.from([0x70]), rng.bytes(28)]),
    Buffer.concat([Buffer.from([0x00]), rng.bytes(28), rng.bytes(28)]),
  ];
  const trackedPolicy = hex(rng.bytes(28));
  const scripts = [0, 1, 2].map((index): ScriptRef => {
    const bytes = rng.bytes(16 + index);
    return {
      type: "plutus_v3",
      bytes,
      hash: blake2b224(Buffer.concat([Buffer.from([3]), bytes])),
    };
  });
  return {
    tracked: {
      addresses: new Set(trackedAddresses.map(hex)),
      paymentCredentials: new Set([hex(trackedCredential)]),
      policies: new Set([trackedPolicy]),
    },
    trackedAddresses,
    trackedCredential,
    untrackedAddresses,
    trackedPolicy,
    untrackedPolicy: hex(rng.bytes(28)),
    scripts,
  };
};

type Utxo = Readonly<{ outRef: OutRef; output: OutputSummary }>;

const randomAddress = (rng: Rng, universe: Universe): Buffer => {
  const roll = rng.int(5);
  if (roll === 0 || roll === 1) return rng.pick(universe.trackedAddresses);
  if (roll === 2)
    // The tracked payment credential with an arbitrary stake part (§5.2 item 13).
    return Buffer.concat([
      Buffer.from([0x00]),
      universe.trackedCredential,
      rng.bytes(28),
    ]);
  return rng.pick(universe.untrackedAddresses);
};

const credentialsOf = (
  address: Buffer,
): Pick<OutputSummary, "paymentCredential" | "stakeCredential"> => {
  const type = (address[0] ?? 0) >> 4;
  return {
    paymentCredential: {
      hash: Buffer.from(address.subarray(1, 29)),
      isScript: (type & 1) === 1,
    },
    stakeCredential:
      type <= 3 && address.length >= 57
        ? Buffer.from(address.subarray(29, 57))
        : null,
  };
};

export const randomOutput = (rng: Rng, universe: Universe): OutputSummary => {
  const address = randomAddress(rng, universe);
  const assets = new Map<string, Map<string, bigint>>();
  if (rng.chance(0.3))
    assets.set(
      rng.chance(0.5) ? universe.trackedPolicy : universe.untrackedPolicy,
      new Map([[hex(rng.bytes(rng.int(4))), BigInt(rng.range(1, 1000))]]),
    );
  const datumRoll = rng.int(4);
  return {
    address,
    ...credentialsOf(address),
    lovelace: BigInt(rng.range(1_000_000, 50_000_000)) * 1000n,
    assets,
    datumHash: datumRoll === 1 ? rng.bytes(32) : null,
    datum:
      datumRoll === 2
        ? Buffer.concat([Buffer.from([0x43]), rng.bytes(3)])
        : null,
    scriptRef: rng.chance(0.1) ? rng.pick(universe.scripts) : null,
  };
};

/**
 * Generates a random chain with forks. It keeps the full ledger UTxO set
 * (tracked and untracked) at its tip with an undo log per block, so txs
 * spend, reference and collateralise outputs that exist on the current
 * chain, including outputs created earlier in the same block.
 */
export class ChainGenerator {
  readonly blocks: BlockSummary[] = [];
  private readonly utxos = new Map<string, Utxo>();
  private readonly undo: { added: string[]; removed: Utxo[] }[] = [];

  constructor(
    private readonly rng: Rng,
    readonly universe: Universe,
    readonly origin: Readonly<{ point: Point; height: number }>,
    seeds: readonly Utxo[] = [],
  ) {
    for (const seed of seeds) this.utxos.set(outRefKey(seed.outRef), seed);
  }

  get tip(): Readonly<{ point: Point; height: number }> {
    const last = this.blocks[this.blocks.length - 1];
    return last === undefined
      ? this.origin
      : { point: last.point, height: last.height };
  }

  /** Blocks above the origin. */
  get length(): number {
    return this.blocks.length;
  }

  private takeInput(exclude: Set<string>): OutRef {
    const keys = [...this.utxos.keys()].filter((key) => !exclude.has(key));
    if (keys.length === 0 || this.rng.chance(0.08))
      return { txHash: this.rng.bytes(32), index: this.rng.int(3) }; // an untracked pre-existing UTxO
    const tracked = keys.filter((key) => {
      const utxo = this.utxos.get(key);
      return utxo !== undefined && this.isTracked(utxo.output);
    });
    const key =
      tracked.length > 0 && this.rng.chance(0.6)
        ? this.rng.pick(tracked)
        : this.rng.pick(keys);
    exclude.add(key);
    const utxo = this.utxos.get(key);
    if (utxo === undefined) throw new Error("utxo vanished");
    return utxo.outRef;
  }

  private isTracked(output: OutputSummary): boolean {
    return (
      this.universe.tracked.addresses.has(hex(output.address)) ||
      (output.paymentCredential !== null &&
        this.universe.tracked.paymentCredentials.has(
          hex(output.paymentCredential.hash),
        ))
    );
  }

  private makeTx(
    index: number,
    log: { added: string[]; removed: Utxo[] },
  ): TxSummary {
    const rng = this.rng;
    const hash = rng.bytes(32);
    const used = new Set<string>();
    const inputs = Array.from({ length: rng.range(1, 2) }, () =>
      this.takeInput(used),
    );
    const isValid = !rng.chance(0.12);
    const collaterals =
      isValid && rng.chance(0.7) ? [] : [this.takeInput(used)];
    const referenceInputs = rng.chance(0.25)
      ? [this.takeInput(new Set(used))]
      : [];
    const outputs = Array.from({ length: rng.range(1, 3) }, () =>
      randomOutput(rng, this.universe),
    );
    const collateralReturn = rng.chance(0.6)
      ? randomOutput(rng, this.universe)
      : null;
    const mint = new Map<string, Map<string, bigint>>();
    if (rng.chance(0.15))
      mint.set(
        rng.chance(0.6)
          ? this.universe.trackedPolicy
          : this.universe.untrackedPolicy,
        new Map([
          [hex(rng.bytes(2)), BigInt(rng.chance(0.2) ? -1 : rng.range(1, 5))],
        ]),
      );
    const consumed = isValid ? inputs : collaterals;
    for (const outRef of consumed) {
      const key = outRefKey(outRef);
      const utxo = this.utxos.get(key);
      if (utxo !== undefined) {
        this.utxos.delete(key);
        log.removed.push(utxo);
      }
    }
    const created = isValid
      ? outputs.map((output, i) => ({
          outRef: { txHash: hash, index: i },
          output,
        }))
      : collateralReturn === null
        ? []
        : [
            {
              outRef: { txHash: hash, index: outputs.length },
              output: collateralReturn,
            },
          ];
    for (const utxo of created) {
      const key = outRefKey(utxo.outRef);
      this.utxos.set(key, utxo);
      log.added.push(key);
    }
    return {
      hash,
      index,
      isValid,
      bodyCbor: rng.bytes(rng.range(8, 40)),
      witnessCbor: rng.bytes(rng.range(8, 40)),
      auxCbor: rng.chance(0.2) ? rng.bytes(6) : null,
      inputs: [...inputs].sort(
        (a, b) => Buffer.compare(a.txHash, b.txHash) || a.index - b.index,
      ),
      referenceInputs,
      collaterals,
      outputs,
      collateralReturn,
      mint,
      withdrawals: rng.chance(0.1)
        ? [
            {
              rewardAccount: Buffer.concat([
                Buffer.from([0xe0]),
                rng.bytes(28),
              ]),
              amount: BigInt(rng.range(0, 1000)),
            },
          ]
        : [],
      redeemers: rng.chance(0.3)
        ? [
            {
              purpose: "spend",
              index: 0,
              data: Buffer.from([0xd8, 0x79, 0x80]),
            },
          ]
        : [],
      invalidBefore: rng.chance(0.3) ? BigInt(rng.range(0, 1000)) : null,
      invalidAfter: rng.chance(0.5) ? BigInt(rng.range(1000, 100000)) : null,
    };
  }

  /** Extends the chain by one block. */
  extend(txCount = this.rng.range(0, 4)): BlockSummary {
    const parent = this.tip;
    const log = { added: [] as string[], removed: [] as Utxo[] };
    const txs = Array.from({ length: txCount }, (_, index) =>
      this.makeTx(index, log),
    );
    const block: BlockSummary = {
      point: {
        slot: parent.point.slot + this.rng.range(1, 3),
        hash: this.rng.bytes(32),
      },
      height: parent.height + 1,
      parentHash: parent.point.hash,
      txs,
    };
    this.blocks.push(block);
    this.undo.push(log);
    return block;
  }

  /** Drops the top `depth` blocks, restoring the UTxO set. Returns the new tip. */
  rollback(depth: number): Readonly<{ point: Point; height: number }> {
    for (let i = 0; i < depth; i += 1) {
      const log = this.undo.pop();
      if (log === undefined || this.blocks.pop() === undefined)
        throw new Error("rollback below origin");
      const added = new Set(log.added);
      for (const key of log.added) this.utxos.delete(key);
      for (const utxo of log.removed) {
        const key = outRefKey(utxo.outRef);
        // An output created and spent inside the dropped block never existed before it.
        if (!added.has(key)) this.utxos.set(key, utxo);
      }
    }
    return this.tip;
  }
}

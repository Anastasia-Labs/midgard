import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  type BlockSummary,
  type DerivationHook,
  type DialectName,
  encodeOutRef,
  type FactStoreOptions,
  type MigrationSet,
  type OutputSummary,
  type OutRef,
  type TemporalTableSpec,
  type TxSummary,
} from "../../src/index.js";

/** A small seeded PRNG (mulberry32). */
export class Rng {
  private state: number;
  constructor(seed: number) {
    this.state = seed >>> 0;
  }
  next(): number {
    this.state = (this.state + 0x6d2b79f5) >>> 0;
    let t = this.state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  }
  int(below: number): number {
    return Math.floor(this.next() * below);
  }
  bytes(length: number): Buffer {
    const out = Buffer.alloc(length);
    for (let i = 0; i < length; i += 4)
      out.writeUInt32BE(
        Math.floor(this.next() * 2 ** 32),
        Math.min(i, length - 4),
      );
    return out;
  }
}

const hex = (bytes: Buffer): string => bytes.toString("hex");

/**
 * Sample role projections for the benchmarks: a versioned per-address live
 * count maintained by delta (O(touched addresses) per block, never a scan),
 * and an append-only spend log. Both are rewound and pruned by the registry.
 */
export const BENCH_TABLES: readonly TemporalTableSpec[] = [
  {
    name: "bench_address_live",
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  {
    name: "bench_spend_log",
    shape: "append_only",
    slotColumn: "spent_slot",
    retention: { kind: "created_k_deep" },
  },
];

export const benchMigrations = (dialect: DialectName): MigrationSet => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return {
    namespace: "bench",
    migrations: [
      {
        id: "0001_bench_tables",
        sql: `
-- class: D-t; retention: current rows forever; closed rows once to_slot is k deep
CREATE TABLE bench_address_live (
  address ${bytes} NOT NULL,
  live_count ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8}
);
CREATE UNIQUE INDEX bench_address_live_current ON bench_address_live (address) WHERE to_slot IS NULL;
CREATE INDEX bench_address_live_from ON bench_address_live (from_slot);
CREATE INDEX bench_address_live_to ON bench_address_live (to_slot);

-- class: D-t; retention: once spent_slot is k deep
CREATE TABLE bench_spend_log (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  spent_slot ${int8} NOT NULL,
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX bench_spend_log_slot ON bench_spend_log (spent_slot);
`,
      },
    ],
  };
};

export const BENCH_DERIVATION: DerivationHook = {
  name: "bench",
  writes: ["bench_address_live", "bench_spend_log"],
  apply: async ({ tx, block, qualified }) => {
    const slot = block.point.slot;
    const delta = new Map<string, { address: Buffer; change: number }>();
    const bump = (address: Buffer, change: number): void => {
      const key = hex(address);
      const entry = delta.get(key) ?? { address, change: 0 };
      entry.change += change;
      delta.set(key, entry);
    };
    for (const entry of qualified) {
      for (const outRef of entry.spent) {
        await tx.query(
          "INSERT INTO bench_spend_log (tx_hash, output_index, spent_slot) VALUES (?, ?, ?)",
          [outRef.txHash, outRef.index, slot],
        );
        const row = (
          await tx.query(
            "SELECT address FROM l1_outputs WHERE tx_hash = ? AND output_index = ?",
            [outRef.txHash, outRef.index],
          )
        )[0];
        if (row !== undefined) bump(Buffer.from(row.address as Uint8Array), -1);
      }
      for (const { output } of entry.created) bump(output.address, 1);
      const first = entry.tx.inputs[0];
      if (entry.trackedMint && entry.tx.isValid && first !== undefined)
        await tx.query(
          "INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot) VALUES (?, ?, ?, ?) ON CONFLICT (kind, key) DO NOTHING",
          ["bench_mint", entry.tx.hash, encodeOutRef(first), slot],
        );
    }
    for (const { address, change } of [...delta.values()].sort((a, b) =>
      Buffer.compare(a.address, b.address),
    )) {
      if (change === 0) continue;
      const current = (
        await tx.query(
          "SELECT live_count FROM bench_address_live WHERE address = ? AND to_slot IS NULL",
          [address],
        )
      )[0];
      const before =
        current === undefined
          ? 0
          : Number(current.live_count as number | string);
      if (current !== undefined)
        await tx.query(
          "UPDATE bench_address_live SET to_slot = ? WHERE address = ? AND to_slot IS NULL",
          [slot, address],
        );
      await tx.query(
        "INSERT INTO bench_address_live (address, live_count, from_slot, to_slot) VALUES (?, ?, ?, NULL)",
        [address, before + change, slot],
      );
    }
  },
};

/** A synthetic chain with a fixed set of tracked addresses and a tracked policy. */
export class Workload {
  readonly rng: Rng;
  readonly trackedAddresses: readonly Buffer[];
  readonly untracked: Buffer;
  readonly policy: Buffer;
  readonly origin: Readonly<{
    point: { slot: number; hash: Buffer };
    height: number;
  }>;
  /**
   * Live tracked outrefs. `random` picks uniformly (swap-remove); `fifo`
   * spends the oldest first (`head` is the queue front), so every output
   * lives exactly live-set / spend-rate blocks, as deposits and orders do.
   */
  private live: OutRef[] = [];
  private head = 0;
  private tip: Readonly<{ slot: number; hash: Buffer; height: number }>;

  constructor(
    seed: number,
    readonly spendOrder: "random" | "fifo" = "random",
    addresses = 64,
  ) {
    this.rng = new Rng(seed);
    this.trackedAddresses = Array.from({ length: addresses }, () =>
      Buffer.concat([Buffer.from([0x70]), this.rng.bytes(28)]),
    );
    this.untracked = Buffer.concat([Buffer.from([0x70]), this.rng.bytes(28)]);
    this.policy = this.rng.bytes(28);
    this.origin = {
      point: { slot: 1_000, hash: this.rng.bytes(32) },
      height: 0,
    };
    this.tip = { ...this.origin.point, height: 0 };
  }

  options(k: number, dialect: DialectName): FactStoreOptions {
    return {
      securityParameter: k,
      trackedSet: {
        addresses: new Set(this.trackedAddresses.map(hex)),
        paymentCredentials: new Set(),
        policies: new Set([hex(this.policy)]),
      },
      temporalTables: BENCH_TABLES,
      migrations: [benchMigrations(dialect)],
      derivations: [BENCH_DERIVATION],
    };
  }

  get liveCount(): number {
    return this.live.length - this.head;
  }

  output(
    withToken: boolean,
    address?: Buffer,
    scriptRef = false,
  ): OutputSummary {
    const to =
      address ??
      this.trackedAddresses[this.rng.int(this.trackedAddresses.length)] ??
      this.untracked;
    const scriptBytes = this.rng.bytes(24);
    return {
      address: to,
      paymentCredential: {
        hash: Buffer.from(to.subarray(1, 29)),
        isScript: true,
      },
      stakeCredential: null,
      lovelace: 2_000_000n + BigInt(this.rng.int(1_000_000)),
      assets: withToken
        ? new Map([
            [
              hex(this.policy),
              new Map([["00", BigInt(1 + this.rng.int(100))]]),
            ],
          ])
        : new Map(),
      datumHash: null,
      datum:
        this.rng.int(2) === 0
          ? Buffer.from([0xd8, 0x79, 0x9f, 0x01, 0xff])
          : null,
      scriptRef: scriptRef
        ? { type: "plutus_v3", bytes: scriptBytes, hash: this.rng.bytes(28) }
        : null,
    };
  }

  private takeLive(): OutRef | null {
    if (this.spendOrder === "fifo") {
      const front = this.live[this.head];
      if (front === undefined) return null;
      this.head += 1;
      if (this.head > 4096 && this.head * 2 > this.live.length) {
        this.live = this.live.slice(this.head);
        this.head = 0;
      }
      return front;
    }
    if (this.live.length === 0) return null;
    const at = this.rng.int(this.live.length);
    const last = this.live.pop();
    if (last === undefined) return null;
    if (at === this.live.length) return last;
    const picked = this.live[at];
    this.live[at] = last;
    return picked ?? null;
  }

  private tx(
    index: number,
    inputs: OutRef[],
    outputs: OutputSummary[],
    mint: boolean,
  ): TxSummary {
    const hash = this.rng.bytes(32);
    for (const [i, output] of outputs.entries())
      if (output.address !== this.untracked)
        this.live.push({ txHash: hash, index: i });
    return {
      hash,
      index,
      isValid: true,
      bodyCbor: this.rng.bytes(200),
      witnessCbor: this.rng.bytes(120),
      auxCbor: null,
      inputs: inputs.sort(
        (a, b) => Buffer.compare(a.txHash, b.txHash) || a.index - b.index,
      ),
      referenceInputs: [],
      collaterals: [],
      outputs,
      collateralReturn: null,
      mint: mint
        ? new Map([[hex(this.policy), new Map([["00", 1n]])]])
        : new Map(),
      withdrawals: [],
      redeemers: [],
      invalidBefore: null,
      invalidAfter: null,
    };
  }

  private block(txs: TxSummary[]): BlockSummary {
    const point = {
      slot: this.tip.slot + 1 + this.rng.int(2),
      hash: this.rng.bytes(32),
    };
    const block: BlockSummary = {
      point,
      height: this.tip.height + 1,
      parentHash: this.tip.hash,
      txs,
    };
    this.tip = { ...point, height: block.height };
    return block;
  }

  /** A preload block: `txs` txs of `outputs` new tracked outputs each, spending nothing tracked. */
  preloadBlock(txs: number, outputs: number): BlockSummary {
    return this.block(
      Array.from({ length: txs }, (_, index) =>
        this.tx(
          index,
          [{ txHash: this.rng.bytes(32), index: 0 }],
          Array.from({ length: outputs }, () => this.output(false)),
          false,
        ),
      ),
    );
  }

  /**
   * A flow block: `txs` qualifying txs, each spending `spend` live tracked
   * outputs and creating `create` tracked outputs (one carrying a token)
   * plus one untracked output; every fifth tx mints under the tracked policy
   * (an event key) and every tenth carries a reference script.
   */
  flowBlock(txs: number, spend = 1, create = 2): BlockSummary {
    return this.block(
      Array.from({ length: txs }, (_, index) => {
        const inputs: OutRef[] = [];
        for (let i = 0; i < spend; i += 1) {
          const outRef = this.takeLive();
          if (outRef !== null) inputs.push(outRef);
        }
        if (inputs.length === 0)
          inputs.push({ txHash: this.rng.bytes(32), index: 0 });
        const outputs = [
          ...Array.from({ length: create }, (_, i) =>
            this.output(
              i === 0,
              undefined,
              i === create - 1 && index % 10 === 0,
            ),
          ),
          this.output(false, this.untracked),
        ];
        return this.tx(index, inputs, outputs, index % 5 === 0);
      }),
    );
  }
}

const here = dirname(fileURLToPath(import.meta.url));

/** Writes a JSON report under `bench/output/` (git-ignored). */
export const writeReport = (name: string, report: unknown): string => {
  const directory =
    process.env.L1_FOLLOWER_BENCH_OUTPUT ?? join(here, "..", "output");
  mkdirSync(directory, { recursive: true });
  const path = join(directory, `${name}.json`);
  writeFileSync(path, `${JSON.stringify(report, null, 2)}\n`);
  return path;
};

export const now = (): number => Number(process.hrtime.bigint()) / 1e6;

/** Least-squares fit y = a + b·x, with R². */
export const linearFit = (
  points: readonly (readonly [number, number])[],
): { intercept: number; slope: number; r2: number } => {
  const n = points.length;
  const mx = points.reduce((s, [x]) => s + x, 0) / n;
  const my = points.reduce((s, [, y]) => s + y, 0) / n;
  const sxx = points.reduce((s, [x]) => s + (x - mx) ** 2, 0);
  const sxy = points.reduce((s, [x, y]) => s + (x - mx) * (y - my), 0);
  const slope = sxx === 0 ? 0 : sxy / sxx;
  const intercept = my - slope * mx;
  const ssTot = points.reduce((s, [, y]) => s + (y - my) ** 2, 0);
  const ssRes = points.reduce(
    (s, [x, y]) => s + (y - (intercept + slope * x)) ** 2,
    0,
  );
  return { intercept, slope, r2: ssTot === 0 ? 1 : 1 - ssRes / ssTot };
};

export const median = (values: readonly number[]): number => {
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1
    ? (sorted[mid] ?? 0)
    : ((sorted[mid - 1] ?? 0) + (sorted[mid] ?? 0)) / 2;
};

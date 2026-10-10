/**
 * The ledger universe of the landed-block fork simulator (N3). Every landed
 * block at queue height `h` (genesis is 0) spends its parent's `X` output
 * and produces `Y_h`, `X_{h,b}` (`b` is the block's parity bit) and, every
 * third height, the deposit `D_h`'s ledger output; past height
 * `Y_LIFETIME` it also spends `Y_{h - Y_LIFETIME}` (another party's
 * transaction the node never saw). The post-state of any block at height
 * `h` with bit `b` is therefore
 *
 *   ledger(h, b) = pool ∪ {Y_j : h - Y_LIFETIME < j ≤ h} ∪ {D_j : j ≤ h} ∪ {X_{h,b}}
 *
 * whatever fork it is on, so its MPF root is precomputed once and a block
 * can commit a correct (or, for a bad block, the other bit's) root before
 * the simulator ever replays it. The pool is genesis outputs nothing but the
 * simulated mempool spends.
 */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { DepositsDB } from "../../src/database/index.js";
import type * as Ledger from "../../src/database/utils/ledger.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../src/mpf/ledger-hydration.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "../midgard-output-helpers.js";

export const SIM_LEDGER_ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";

/** The highest queue height the universe has roots for. */
export const H_MAX = 120;
export const POOL_SIZE = 8;
/**
 * How many heights a `Y` output stays unspent: blocks at `h` spend
 * `Y_{h - Y_LIFETIME}`. Long enough that the block including a batch's `X`
 * chain often folds before the batch's `Y` spend loses its input.
 */
export const Y_LIFETIME = 9;

export const hasDeposit = (h: number): boolean => h > 0 && h % 3 === 0;

export const simDigest = (label: string): Buffer =>
  createHash("sha256")
    .update("midgard-landed-blocks-sim")
    .update("\0")
    .update(label)
    .digest();

export const simOutput = (lovelace: bigint): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(SIM_LEDGER_ADDRESS),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  );

const entry = (label: string, lovelace: bigint): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(label), 0),
  output: simOutput(lovelace),
});

export type SimDeposit = Readonly<{
  h: number;
  row: DepositsDB.Entry;
  entry: Ledger.MinimalEntry;
}>;

const depositAt = (h: number): SimDeposit => {
  const id = Buffer.from(
    Data.to(
      {
        transactionId: simDigest(`deposit-l1:${h}`).toString("hex"),
        outputIndex: 0n,
      },
      SDK.OutputReference,
    ),
    "hex",
  );
  const row: DepositsDB.Entry = {
    [DepositsDB.Columns.ID]: id,
    [DepositsDB.Columns.INFO]: simDigest(`deposit-info:${h}`),
    [DepositsDB.Columns.INCLUSION_TIME]: new Date(
      Date.parse("2026-10-01T00:00:00.000Z") + h * 1_000,
    ),
    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: simDigest(`deposit-l1:${h}`),
    [DepositsDB.Columns.LEDGER_TX_ID]: simDigest(`deposit-ledger:${h}`),
    [DepositsDB.Columns.LEDGER_OUTPUT]: simOutput(3_000_000n + BigInt(h)),
    [DepositsDB.Columns.LEDGER_ADDRESS]: SIM_LEDGER_ADDRESS,
    [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
    [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
  };
  const ledger = Effect.runSync(DepositsDB.toLedgerEntry(row));
  return {
    h,
    row,
    entry: { outref: ledger.outref, output: ledger.output },
  };
};

export type SimUniverse = Readonly<{
  pool: readonly Ledger.MinimalEntry[];
  genesis: readonly Ledger.MinimalEntry[];
  y: (h: number) => Ledger.MinimalEntry;
  x: (h: number, b: number) => Ledger.MinimalEntry;
  /** Every `X` outref, hex: what a block spends from its parent. */
  xKeys: ReadonlySet<string>;
  deposit: (h: number) => SimDeposit;
  deposits: readonly SimDeposit[];
  /** The `Y` output blocks at height `h` spend besides their parent's `X`, if any. */
  ySpent: (h: number) => Ledger.MinimalEntry | undefined;
  ledger: (h: number, b: number) => Ledger.MinimalEntry[];
  root: (h: number, b: number) => string;
}>;

const required = <A>(value: A | undefined, what: string): A => {
  if (value === undefined) throw new Error(`the sim universe has no ${what}`);
  return value;
};

export const createSimUniverse = async (): Promise<SimUniverse> => {
  const pool = Array.from({ length: POOL_SIZE }, (_, index) =>
    entry(`pool:${index}`, 2_000_000n + BigInt(index)),
  );
  const x0 = entry("x:0", 1_500_000n);
  const ys = Array.from({ length: H_MAX }, (_, index) =>
    entry(`y:${index + 1}`, 4_000_000n + BigInt(index + 1)),
  );
  const xs = Array.from({ length: H_MAX }, (_, index) =>
    [0, 1].map((b) =>
      entry(`x:${index + 1}:${b}`, 5_000_000n + BigInt(2 * index + b)),
    ),
  );
  const depositsByHeight = new Map<number, SimDeposit>();
  for (let h = 1; h <= H_MAX; h++)
    if (hasDeposit(h)) depositsByHeight.set(h, depositAt(h));
  const y = (h: number) => required(ys[h - 1], `Y_${h}`);
  const x = (h: number, b: number) =>
    h === 0 ? x0 : required(xs[h - 1]?.[b], `X_${h},${b}`);
  const deposit = (h: number) => required(depositsByHeight.get(h), `D_${h}`);
  const ySpent = (h: number) =>
    h - Y_LIFETIME >= 1 ? y(h - Y_LIFETIME) : undefined;
  const ledger = (h: number, b: number): Ledger.MinimalEntry[] => {
    const entries = [...pool];
    for (let j = 1; j <= h; j++) {
      if (j > h - Y_LIFETIME) entries.push(y(j));
      if (hasDeposit(j)) entries.push(deposit(j).entry);
    }
    entries.push(x(h, b));
    return entries;
  };
  const roots: string[][] = [];
  for (let h = 0; h <= H_MAX; h++) {
    const pair: string[] = [];
    for (const b of h === 0 ? [0] : [0, 1])
      pair.push(
        await Effect.runPromise(
          computeLedgerMpfRootFromLedgerEntries(ledger(h, b)),
        ),
      );
    roots.push(h === 0 ? [pair[0]!, pair[0]!] : pair);
  }
  return {
    pool,
    genesis: ledger(0, 0),
    y,
    x,
    xKeys: new Set(
      [x0, ...xs.flat()].map((item) => item.outref.toString("hex")),
    ),
    deposit,
    deposits: [...depositsByHeight.values()],
    ySpent,
    ledger,
    root: (h, b) => required(roots[h]?.[b], `root(${h}, ${b})`),
  };
};

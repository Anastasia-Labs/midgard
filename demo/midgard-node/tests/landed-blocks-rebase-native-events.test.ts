/**
 * The rebase moves the native MPF by each landed step's events, read by key
 * from the step's parent ledger in the native owner's ledger store, never
 * from `confirmed_ledger` (the targets here carry an empty one): an output
 * produced at an out-ref the parent holds is deleted before it is
 * inserted, a parent a retain dropped is rebuilt from the retained root
 * before it, and a parent the store cannot give fails the move before any
 * fork.
 */
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { Cause, Effect, Exit } from "effect";
import { expect, it } from "vitest";

import type * as LedgerRows from "../src/database/utils/ledger.js";
import { moveNativeRoot } from "../src/landed-blocks/rebase.js";
import type { RebaseTarget } from "../src/landed-blocks/rebase-target.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import { EVENT_LOG_HEADER_BYTES } from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { simOutput } from "./helpers/landed-blocks-sim.universe.js";
import {
  entry,
  FRONTIER,
  hex,
  landedRow,
} from "./landed-blocks-rebase.fixture.js";

const A = entry("events:a", 2_000_000n);
const B = entry("events:b", 3_000_000n);
const C = entry("events:c", 4_000_000n);
const D = entry("events:d", 5_000_000n);
/** An output at `B`'s out-ref, replacing it. */
const B2 = { outref: B.outref, output: simOutput(9_000_000n) };

type Step = Readonly<{
  spent: readonly Buffer[];
  produced: readonly LedgerRows.MinimalEntry[];
}>;

const mutationsOf = ({ spent, produced }: Step, parent: Set<string>) => [
  ...[
    ...new Set([
      ...spent.map(hex),
      ...produced
        .map((item) => hex(item.outref))
        .filter((key) => parent.has(key)),
    ]),
  ].map((key) => ({
    type: "delete" as const,
    outRef: Buffer.from(key, "hex"),
  })),
  ...produced.map((item) => ({
    type: "insert" as const,
    outRef: item.outref,
    output: item.output,
  })),
];

/**
 * A store holding the ledger of `frontier`, the ledgers after each of
 * `steps`, and the target over them with an empty `confirmed_ledger`;
 * `keep(index)` says whether the store retains the ledger after `index`
 * steps.
 */
const chainOf = async (
  frontier: readonly LedgerRows.MinimalEntry[],
  steps: readonly Step[],
  keep: (index: number) => boolean,
) => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  let ledger: Ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(
    frontier.map((item) => ({
      type: "insert" as const,
      outRef: item.outref,
      output: item.output,
    })),
  );
  const held = new Set(frontier.map((item) => hex(item.outref)));
  const roots = [ledger.root];
  if (keep(0)) await store.persist(ledger);
  for (const [index, step] of steps.entries()) {
    ledger = await ledger.apply(mutationsOf(step, held));
    for (const outRef of step.spent) held.delete(hex(outRef));
    for (const item of step.produced) held.add(hex(item.outref));
    roots.push(ledger.root);
    if (keep(index + 1)) await store.persist(ledger);
  }
  const chain = steps.map((step, index) =>
    landedRow({
      headerHash: `b${index.toString()}`.repeat(28),
      parentHeaderHash:
        index === 0 ? FRONTIER : `b${(index - 1).toString()}`.repeat(28),
      parentUtxosRoot: roots[index]!,
      utxosRoot: roots[index + 1]!,
      spent: [...step.spent],
      produced: [...step.produced],
    }),
  );
  const target = {
    rows: chain,
    landed: {
      frontier: { headerHash: FRONTIER, utxosRoot: roots[0]! },
      confirmed: [],
      chain,
    },
    steps: chain.map((row) => ({
      headerHash: row.headerHash,
      utxosRoot: row.utxosRoot,
      kind: row.kind,
      spent: row.spent,
      produced: row.produced,
      row,
    })),
  } as unknown as RebaseTarget;
  return { store, roots, target };
};

/** Each event of a native event log, as `kind key-hex` lines. */
const eventsOf = (log: Buffer) => {
  const events: string[][] = [];
  let offset = EVENT_LOG_HEADER_BYTES;
  for (let event = 0; event < log.readUInt32LE(8); event++) {
    const ops: string[] = [];
    const count = log.readUInt32LE(offset);
    offset += 4;
    for (let op = 0; op < count; op++) {
      const kind = log.readUInt8(offset) === 1 ? "insert" : "delete";
      const keyLength = log.readUInt16LE(offset + 1);
      const valueLength = log.readUInt32LE(offset + 3);
      offset += 7;
      ops.push(`${kind} ${hex(log.subarray(offset, offset + keyLength))}`);
      offset += keyLength + valueLength;
    }
    events.push(ops);
  }
  return events;
};

/** A native owner over `store` at `durableRoot`, whose applied events reach
 * the next root of `roots`. */
const ownerOver = (
  store: LedgerStore,
  roots: readonly string[],
  durableRoot: string,
) => {
  const native = { durableRoot, forks: 0, logs: [] as string[][][] };
  const owner = {
    diagnostics: async () => ({ durableRoot: native.durableRoot }),
    fork: async (base: string) => {
      native.forks += 1;
      return { base };
    },
    applyEvents: async (handle: { base: string }, log: Buffer) => {
      native.logs.push(eventsOf(log));
      return { candidateRoot: roots[roots.indexOf(handle.base) + 1]! };
    },
    promote: async (handle: { base: string }) => {
      native.durableRoot = roots[roots.indexOf(handle.base) + 1]!;
    },
    discard: async () => undefined,
    ledger: (root: string) => store.at(root),
  } as unknown as NativeMpfOwnerService;
  return { native, owner };
};

const move = (owner: NativeMpfOwnerService, target: RebaseTarget) =>
  Effect.runPromiseExit(
    moveNativeRoot(owner, target, { assertCurrent: Effect.void }),
  );

const sorted = (...keys: Buffer[]) => keys.map(hex).sort();

it("deletes an output a step replaces, read by key from its parent's ledger in the store", async () => {
  const { store, roots, target } = await chainOf(
    [A, B],
    [{ spent: [A.outref], produced: [B2, C] }],
    () => true,
  );
  const { native, owner } = ownerOver(store, roots, roots[0]!);
  expect(Exit.isSuccess(await move(owner, target))).toBe(true);
  expect(native.logs).toEqual([
    [
      sorted(A.outref, B.outref).map((key) => `delete ${key}`),
      [`insert ${hex(B.outref)}`, `insert ${hex(C.outref)}`],
    ],
  ]);
  expect(native.durableRoot).toBe(roots[1]);
});

it("reads a parent a retain dropped rebuilt from the retained root before it", async () => {
  const { store, roots, target } = await chainOf(
    [A, B],
    [
      { spent: [A.outref], produced: [C] },
      { spent: [C.outref], produced: [B2, D] },
    ],
    (index) => index === 0,
  );
  const { native, owner } = ownerOver(store, roots, roots[0]!);
  expect(Exit.isSuccess(await move(owner, target))).toBe(true);
  expect(native.logs).toEqual([
    [[`delete ${hex(A.outref)}`], [`insert ${hex(C.outref)}`]],
    [
      sorted(B.outref, C.outref).map((key) => `delete ${key}`),
      [`insert ${hex(B.outref)}`, `insert ${hex(D.outref)}`],
    ],
  ]);
  expect(native.durableRoot).toBe(roots[2]);
});

it("fails before any fork when the store cannot give a step's parent", async () => {
  const { store, roots, target } = await chainOf(
    [A],
    [{ spent: [A.outref], produced: [C] }],
    () => false,
  );
  const { native, owner } = ownerOver(store, roots, roots[0]!);
  const exit = await move(owner, target);
  if (!Exit.isFailure(exit)) throw new Error("expected the move to fail");
  expect(Cause.pretty(exit.cause)).toContain(
    `the rebase cannot read the ledger at root ${roots[0]!} its next landed block applies to`,
  );
  expect(native.forks).toBe(0);
  expect(native.durableRoot).toBe(roots[0]);
});

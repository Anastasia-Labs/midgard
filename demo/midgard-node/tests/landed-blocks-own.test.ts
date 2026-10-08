/**
 * This node's own landed blocks (plan §7.3, §15 N3), processed against stub
 * ports on the node database: an own block is adopted from its journal and
 * never replayed, exactly once across runs; a journal that is abandoned or
 * does not describe the landed block holds by name and records nothing; a
 * merged own block folds into `confirmed_ledger` through its local merge
 * finalization only; a foreign block after it replays on the journal's
 * post-state, and one that misses its header's root is never adopted.
 */
import "./utils.js";

import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import type {
  LandedStateQueue,
  LandedStateQueueElement,
} from "../src/l1-state-queue/index.js";
import {
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_ABANDONED,
  LANDED_BLOCK_REBASE_PENDING,
} from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import type {
  LandedBlockPorts,
  OwnJournal,
} from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import type { ReplayInput } from "../src/landed-blocks/replay.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import type { Database } from "../src/services/database.js";
import { withHistoryWrite } from "../src/services/event-history-producer.js";
import { rootDatum } from "./helpers/landed-blocks-sim.traffic.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import {
  hex32,
  NODE_PREFIX,
  nodeDatum,
  ROOT_ASSET,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

const entry = (label: string, lovelace: bigint): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`own:${label}`), 0),
  output: simOutput(lovelace),
});

const G0 = entry("g0", 2_000_000n);
const G1 = entry("g1", 2_000_001n);
const A1 = entry("a1", 3_000_000n);
const F1 = entry("f1", 4_000_000n);
const GENESIS = [G0, G1];
const AFTER_A = [G1, A1];
const AFTER_F = [A1, F1];

const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

const sortedKeys = (entries: readonly Ledger.MinimalEntry[]) =>
  entries.map((item) => `${hex(item.outref)}=${hex(item.output)}`).sort();

const VIEW: View = {
  generation: 1,
  point: { slot: 1, hash: Buffer.alloc(32) },
  height: 1,
};

const element = (
  index: number,
  datum: Buffer,
  assetName: string,
  headerHash: string,
  endTimeMs: bigint,
): LandedStateQueueElement => ({
  outRef: `${hex32(index)}#0`,
  element: {
    utxo: {
      txHash: hex32(index),
      outputIndex: 0,
      address:
        "addr_test1wqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq",
      assets: { lovelace: 5_000_000n },
    },
    datum: SDK.linkedListDatumToNodeView(
      Data.from(datum.toString("hex"), SDK.LinkedListDatum),
      assetName,
    ),
    assetName,
  },
  headerHash,
  endTimeMs,
  daStatus: null,
  problems: [],
  created: null,
});

type Block = Readonly<{ hash: string; header: SDK.Header }>;

const block = (
  nonce: number,
  parent: Readonly<{ hash: string; root: string; endTime: bigint }>,
  utxosRoot: string,
): Block => {
  const header: SDK.Header = {
    ...simHeader(nonce, parent.hash),
    prevUtxosRoot: parent.root,
    utxosRoot,
    startTime: parent.endTime,
    endTime: parent.endTime + 1_000n,
  };
  return { hash: SDK.stateQueueHeaderHash(header), header };
};

/** The landed queue: the root's confirmed state, then `nodes` in order. */
const queueOf = (
  root: SDK.ConfirmedState,
  nodes: readonly Block[],
): LandedStateQueue => ({
  view: VIEW,
  healthy: true,
  reason: null,
  detail: null,
  root: element(
    0,
    rootDatum(root, nodes[0]?.hash ?? null),
    ROOT_ASSET,
    root.headerHash,
    root.endTime,
  ),
  nodes: nodes.map((node, index) =>
    element(
      index + 1,
      nodeDatum(node.header, "Unattested", nodes[index + 1]?.hash ?? null),
      NODE_PREFIX + node.hash,
      node.hash,
      node.header.endTime,
    ),
  ),
  strays: [],
  policyOutputCount: nodes.length + 1,
});

const confirmedAt = (target: Block | "genesis", root: string) =>
  target === "genesis"
    ? {
        headerHash: SDK.GENESIS_HEADER_HASH,
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
        utxoRoot: root,
        startTime: 0n,
        endTime: 0n,
        protocolVersion: 1n,
      }
    : {
        headerHash: target.hash,
        prevHeaderHash: target.header.prevHeaderHash,
        utxoRoot: target.header.utxosRoot,
        startTime: target.header.startTime,
        endTime: target.header.endTime,
        protocolVersion: 1n,
      };

type Harness = {
  journals: Map<string, OwnJournal>;
  completed: Set<string>;
  replays: ReplayInput[];
  finalized: string[];
  rebaseRequests: number;
  replayRoot: string | undefined;
};

const harness = (): Harness => ({
  journals: new Map(),
  completed: new Set(),
  replays: [],
  finalized: [],
  rebaseRequests: 0,
  replayRoot: undefined,
});

const ports = (state: Harness): LandedBlockPorts<never> => ({
  confirmView: () => Effect.succeed(true),
  write: (work) => withHistoryWrite(work),
  replay: (input) =>
    Effect.gen(function* () {
      state.replays.push(input);
      const entries = [
        ...input.parentEntries.filter((item) => !item.outref.equals(G1.outref)),
        F1,
      ];
      return {
        kind: "replayed",
        entries,
        root:
          state.replayRoot ??
          (yield* computeLedgerMpfRootFromLedgerEntries(entries)),
        depositIds: [],
        withdrawals: [],
        forcedIds: [],
        txIds: [],
      } as const;
    }),
  ownJournal: (headerHash) => Effect.succeed(state.journals.get(headerHash)),
  ownMergeCompleted: (headerHash) =>
    Effect.succeed(state.completed.has(headerHash)),
  // The local merge finalization: the journal's delta, then the job completes.
  finalizeOwnMerge: ({ headerHash }) =>
    withHistoryWrite(
      Effect.gen(function* () {
        const journal = state.journals.get(hex(headerHash))!;
        yield* ConfirmedLedgerDB.clearUTxOs([...journal.spent]);
        yield* ConfirmedLedgerDB.insertMultiple([
          ...(yield* ledgerRows(journal.produced, new Map())),
        ]);
        state.finalized.push(hex(headerHash));
        state.completed.add(hex(headerHash));
      }),
    ),
  genesis: ledgerRows(GENESIS, new Map()),
  requestRebase: () =>
    Effect.sync(() => {
      state.rebaseRequests += 1;
      return undefined;
    }),
});

type Fixture = Readonly<{
  genesisRoot: string;
  genesisState: SDK.ConfirmedState;
  a: Block;
  journalA: OwnJournal;
  f: Block;
}>;

const fixture = async (): Promise<Fixture> => {
  const genesisRoot = await rootOf(GENESIS);
  const a = block(
    1,
    { hash: SDK.GENESIS_HEADER_HASH, root: genesisRoot, endTime: 0n },
    await rootOf(AFTER_A),
  );
  const f = block(
    2,
    { hash: a.hash, root: a.header.utxosRoot, endTime: a.header.endTime },
    await rootOf(AFTER_F),
  );
  return {
    genesisRoot,
    genesisState: confirmedAt("genesis", genesisRoot),
    a,
    f,
    journalA: {
      status: "active",
      baseTailHeaderHash: SDK.GENESIS_HEADER_HASH,
      baseUtxosRoot: genesisRoot,
      expectedUtxosRoot: a.header.utxosRoot,
      spent: [G0.outref],
      produced: [A1],
      depositIds: [],
      forcedIds: [],
      txIds: [simDigest("own:tx-a")],
    },
  };
};

const inNode = <A>(work: Effect.Effect<A, unknown, Database>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* work;
      }),
    ),
  );

const confirmedKeys = ConfirmedLedgerDB.retrieve.pipe(
  Effect.map((entries) => sortedKeys(entries)),
);

describe("own landed blocks", () => {
  it("adopts an own block from its journal exactly once, never replaying it, and replays the next foreign block on its post-state", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        const process = (nodes: readonly Block[]) =>
          processLandedQueue(ports(state), queueOf(genesisState, nodes));
        for (let run = 0; run < 3; run++) {
          expect(yield* process([a])).toBeUndefined();
          const rows = yield* retrieveRows;
          expect(rows).toHaveLength(1);
          expect(rows[0]).toMatchObject({
            headerHash: a.hash,
            kind: "own",
            state: "processed",
            applied: true,
            parentHeaderHash: SDK.GENESIS_HEADER_HASH,
            utxosRoot: a.header.utxosRoot,
          });
          expect(rows[0]!.spent.map(hex)).toEqual([hex(G0.outref)]);
          expect(sortedKeys(rows[0]!.produced)).toEqual(sortedKeys([A1]));
        }
        expect(state.replays).toHaveLength(0);
        expect(state.rebaseRequests).toBe(0);
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));

        const held = yield* process([a, f]);
        expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect(state.replays.map((input) => input.headerHash)).toEqual([
          f.hash,
        ]);
        expect(sortedKeys(state.replays[0]!.parentEntries)).toEqual(
          sortedKeys(AFTER_A),
        );
        const rows = yield* retrieveRows;
        expect(
          rows.map((row) => [row.headerHash, row.kind, row.applied]),
        ).toEqual([
          [a.hash, "own", true],
          [f.hash, "foreign", false],
        ]);
        // Processed once: later runs neither replay nor re-record either block.
        yield* process([a, f]);
        expect(state.replays).toHaveLength(1);
        expect(yield* retrieveRows).toHaveLength(2);
      }),
    );
  }, 120_000);

  it("holds an own block whose journal is abandoned, recording and replaying nothing", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "abandoned" });
    await inNode(
      Effect.gen(function* () {
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_OWN_JOURNAL_ABANDONED);
        expect(held?.detail).toContain(a.hash);
        expect(yield* retrieveRows).toHaveLength(0);
        expect(state.replays).toHaveLength(0);
        // Revived: the next run adopts it.
        state.journals.set(a.hash, journalA);
        expect(
          yield* processLandedQueue(ports(state), queueOf(genesisState, [a])),
        ).toBeUndefined();
        expect(yield* retrieveRows).toHaveLength(1);
      }),
    );
  }, 120_000);

  it("never adopts an own block whose journal does not describe the landed block", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, {
      ...journalA,
      expectedUtxosRoot: f.header.utxosRoot,
    });
    await inNode(
      Effect.gen(function* () {
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_INVALID);
        expect(held?.detail).toContain(a.hash);
        expect(yield* retrieveRows).toHaveLength(0);
        expect(state.replays).toHaveLength(0);
      }),
    );
  }, 120_000);

  it("never adopts a foreign block that replays to another root than its header's", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    state.replayRoot = hex32(7);
    await inNode(
      Effect.gen(function* () {
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_INVALID);
        expect(held?.detail).toContain(f.hash);
        expect(held?.detail).toContain(hex32(7));
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
        expect(state.rebaseRequests).toBe(0);
      }),
    );
  }, 120_000);

  it("folds a merged own block into confirmed_ledger only through its local merge finalization", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const merged = confirmedAt(a, "");
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        // Merged, but not locally finalized: behind, by name.
        const behind = yield* processLandedQueue(
          ports(state),
          queueOf(merged, []),
        );
        expect(behind?.reason).toBe(CONFIRMED_LEDGER_BEHIND);
        expect(behind?.detail).toContain(a.hash);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));
        expect(state.finalized).toHaveLength(0);

        state.journals.set(a.hash, { ...journalA, status: "finalized" });
        expect(
          yield* processLandedQueue(ports(state), queueOf(merged, [])),
        ).toBeUndefined();
        expect(state.finalized).toEqual([a.hash]);
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: a.hash,
          utxosRoot: a.header.utxosRoot,
        });
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        expect(yield* retrieveRows).toHaveLength(0);
        expect(state.replays).toHaveLength(0);
      }),
    );
  }, 120_000);

  it("passes a merged own block the merge fiber already finalized without finalizing it again", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        // The merge fiber finalized it first.
        yield* ports(state).finalizeOwnMerge({
          headerHash: Buffer.from(a.hash, "hex"),
          headerUtxosRoot: a.header.utxosRoot,
        });
        expect(
          yield* processLandedQueue(
            ports(state),
            queueOf(confirmedAt(a, ""), []),
          ),
        ).toBeUndefined();
        expect(state.finalized).toEqual([a.hash]);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
      }),
    );
  }, 120_000);
});

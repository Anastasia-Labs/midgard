/**
 * The own-landed-block test's fixture (plan §7.3, §15 N3): a genesis
 * ledger, an own block A on it with its journal and a foreign block F on A,
 * their landed queue, and stub landed-block ports over the node database
 * that record replays and rebases (held pending: the driver's recompute
 * never runs here) and serve a set queue history.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import type {
  LandedStateQueue,
  LandedStateQueueElement,
} from "../src/l1-state-queue/index.js";
import { LANDED_BLOCK_REBASE_PENDING } from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import {
  type LandedBlockPorts,
  type OwnJournal,
} from "../src/landed-blocks/ports.js";
import type { ReplayInput } from "../src/landed-blocks/replay.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import type { Database } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
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

export const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export const entry = (
  label: string,
  lovelace: bigint,
): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`own:${label}`), 0),
  output: simOutput(lovelace),
});

export const G0 = entry("g0", 2_000_000n);
export const G1 = entry("g1", 2_000_001n);
export const A1 = entry("a1", 3_000_000n);
export const F1 = entry("f1", 4_000_000n);
export const GENESIS = [G0, G1];
export const AFTER_A = [G1, A1];
export const AFTER_F = [A1, F1];

export const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

export const sortedKeys = (entries: readonly Ledger.MinimalEntry[]) =>
  entries.map((item) => `${hex(item.outref)}=${hex(item.output)}`).sort();

export const VIEW: View = {
  generation: 1,
  point: { slot: 1, hash: Buffer.alloc(32) },
  height: 1,
};

export const element = (
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

export type Block = Readonly<{ hash: string; header: SDK.Header }>;

export const block = (
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
export const queueOf = (
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

export const confirmedAt = (target: Block | "genesis", root: string) =>
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

export type Harness = {
  journals: Map<string, OwnJournal>;
  /** The retained queue outputs `queueHistory` serves. */
  history: LandedStateQueueElement[];
  replays: ReplayInput[];
  rebaseRequests: number;
  replayRoot: string | undefined;
  /** Whether the follower is still at the run's view when it writes. */
  viewHeld: boolean;
};

export const harness = (): Harness => ({
  journals: new Map(),
  history: [],
  replays: [],
  rebaseRequests: 0,
  replayRoot: undefined,
  viewHeld: true,
});

export const ports = (state: Harness): LandedBlockPorts<never> => ({
  confirmView: () => Effect.sync(() => state.viewHeld),
  queueHistory: () => Effect.sync(() => state.history),
  write: (work) => withFollowerWrite(work),
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
  genesis: ledgerRows(GENESIS, new Map()),
  // A driver rebase this fixture records and never runs: it stays pending.
  rebase: () =>
    Effect.sync(() => {
      state.rebaseRequests += 1;
      return {
        reason: LANDED_BLOCK_REBASE_PENDING,
        detail: "the driver's rebase has not run",
      };
    }),
});

export type Fixture = Readonly<{
  genesisRoot: string;
  genesisState: SDK.ConfirmedState;
  a: Block;
  journalA: OwnJournal;
  f: Block;
}>;

export const fixture = async (): Promise<Fixture> => {
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
      withdrawals: [],
      forcedIds: [],
      txIds: [simDigest("own:tx-a")],
      revived: false,
    },
  };
};

export const inNode = <A>(work: Effect.Effect<A, unknown, Database>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* work;
      }),
    ),
  );

export const confirmedKeys = ConfirmedLedgerDB.retrieve.pipe(
  Effect.map((entries) => sortedKeys(entries)),
);

/**
 * The bench for foreign landed blocks on the node's ledger store: the
 * production foreign replayer over a follower store, landed-block ports
 * whose ledger store keeps trie nodes in memory and reads every output
 * through the node's output reader (`ledger-outputs.ts`), and the normal
 * fixture's signed transfer as a block on a chosen parent.
 */
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type LedgerStorage,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Metric, Runtime } from "effect";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { readLedgerOutputs } from "../src/landed-blocks/ledger-outputs.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { Frontier } from "../src/landed-blocks/store.js";
import type { Database } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";
import { NODE_PREFIX, nodeDatum } from "./helpers/state-queue-sim.fixtures.js";
import {
  type Block,
  element,
  harness,
  ports,
  queueOf,
} from "./landed-blocks-own.fixture.js";
import { replayerFor } from "./landed-blocks-replay-foreign.fixture.js";

/** A memory trie storage and a ledger store over it that reads outputs
 * from the node database only. */
export const nodeLedgers = Effect.gen(function* () {
  const runtime = yield* Effect.runtime<Database>();
  const storage: LedgerStorage = createMemoryLedgerStorage();
  const store: LedgerStore = createLedgerStore({
    ...storage,
    readOutputs: (outRefs) =>
      Runtime.runPromise(runtime)(readLedgerOutputs(outRefs)),
  });
  return { storage, store };
});

/** Has `store` retain the ledger of `entries`; its root. */
export const retainLedgerOf = async (
  store: LedgerStore,
  entries: readonly Ledger.MinimalEntry[],
): Promise<string> => {
  const ledger = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(
    entries.map(({ outref, output }) => ({
      type: "insert" as const,
      outRef: outref,
      output,
    })),
  );
  await store.persist(ledger);
  return ledger.root;
};

/** What `store.at(root)` ends in: "opened", or the refusal's name. */
export const opening = (store: LedgerStore, root: string) =>
  store.at(root).then(
    () => "opened",
    (error: unknown) => (error as Error).name,
  );

/** `confirmed_ledger` holds `entries` and the frontier is `frontier`. */
export const seed = (
  entries: readonly Ledger.MinimalEntry[],
  frontier: Readonly<{ headerHash: string; utxosRoot: string }>,
) =>
  withFollowerWrite(
    Effect.gen(function* () {
      yield* ConfirmedLedgerDB.insertMultiple([
        ...(yield* ledgerRows(entries, new Map())),
      ]);
      yield* Frontier.upsert(frontier);
    }),
  );

/**
 * The normal fixture's transfer as a block on the parent `parentHash`: it
 * starts at time 1 and spends `spent`, the one entry of its parent's ledger,
 * whose root is `prior`.
 */
export const spendingBlock = async (parentHash: string) => {
  const fixture = await normalFixture();
  const body = fixture.payload.block_body;
  const payload = await rebind({
    ...fixture.payload,
    block_body: {
      ...body,
      header: { ...body.header, prevHeaderHash: parentHash },
    },
  });
  return {
    payload,
    hash: payload.block_body.header_hash,
    header: payload.block_body.header,
    spent: {
      outref: fixture.spentEntry.outRef,
      output: fixture.spentEntry.output,
    } satisfies Ledger.MinimalEntry,
    prior: body.header.prevUtxosRoot,
  };
};

export const confirmedState = (
  headerHash: string,
  utxoRoot: string,
  endTime: bigint,
): SDK.ConfirmedState => ({
  headerHash,
  prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  utxoRoot,
  startTime: 0n,
  endTime,
  protocolVersion: 1n,
});

/** The landed queue at the follower's view: `root`, then `blocks` in order,
 * each with its availability status. */
export const landedQueue = async (
  follower: FactStore,
  root: SDK.ConfirmedState,
  blocks: readonly (Block &
    Readonly<{ status: SDK.DaAvailabilityStateQueueStatus }>)[],
) => ({
  ...queueOf(root, blocks),
  view: (await follower.currentView())!,
  nodes: blocks.map((block, index) =>
    element(
      index + 1,
      nodeDatum(block.header, block.status, blocks[index + 1]?.hash ?? null),
      NODE_PREFIX + block.hash,
      block.hash,
      block.header.endTime,
    ),
  ),
});

/** The own fixture's stub ports, with the production replayer over
 * `follower` and the ledger store `ledgers`. */
export const foreignPorts = (follower: FactStore, ledgers: LedgerStore) => {
  const replayer = replayerFor(follower);
  const production: LandedBlockPorts<
    Effect.Effect.Context<ReturnType<typeof replayer>>
  > = {
    ...ports(harness()),
    write: (work) => withFollowerWrite(work),
    replay: replayer,
    retainedLedger: (root) =>
      Effect.tryPromise({ try: () => ledgers.at(root), catch: (e) => e }),
    persistLedger: (ledger) =>
      Effect.tryPromise({
        try: () => ledgers.persist(ledger),
        catch: (e) => e,
      }),
  };
  return production;
};

/** Full scans of `confirmed_ledger` so far. */
export const fullScans = Metric.value(fullScanCounter).pipe(
  Effect.map(({ count }) => count),
);

/**
 * The node's landed rows for the validation-trace Open scene: the scene's
 * blocks as processed own rows on a confirmed frontier at the failing
 * block's parent, each with its payload retained; the ledgers the node's
 * store holds for them (each block's parent ledger and its post-state, as
 * processing persists them); and the node's wallet configuration for the
 * scene's operator.
 */
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type LedgerStorage,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import type {
  Block,
  Scene,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import { Effect } from "effect";

import { foreignRetainedDaInsert } from "../../src/da/foreign-retained-da.js";
import { DaPayloadsDB } from "../../src/database/index.js";
import { Frontier, insertRow } from "../../src/landed-blocks/store.js";
import type { DefenseLedgers } from "../../src/validation-trace-defense/material.js";
import { ledgerMap } from "./ledger-map.js";

type Entry = Readonly<{ outRef: Buffer; output: Buffer }>;
const minimal = (entries: readonly Entry[]) =>
  entries.map(({ outRef, output }) => ({ outref: outRef, output }));

/** The landed row's delta from the ledger of `before` to that of `after`. */
const delta = (before: readonly Entry[], after: readonly Entry[]) => {
  const parent = ledgerMap(minimal(before));
  const next = ledgerMap(minimal(after));
  return {
    spent: [...parent.keys()]
      .filter((key) => !next.has(key))
      .map((key) => Buffer.from(key, "hex")),
    produced: minimal(after).filter(
      ({ outref, output }) =>
        !parent.get(outref.toString("hex"))?.equals(output),
    ),
  };
};

/** The node's wallet configuration with `seed` as its operator seed. */
export const operatorConfig = (seed: string) => ({
  NETWORK: "Custom" as const,
  L1_OPERATOR_SEED_PHRASE: seed,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "",
  L1_SETTLEMENT_SEED_PHRASE: "",
  L1_REFERENCE_SCRIPT_SEED_PHRASE: "",
});

/**
 * Retains `block`'s payload as the node does. The fixture's body carries
 * no validation traces, while a retained row of a block with transactions
 * must count one per transaction under a non-empty root; the row takes
 * those from the header's counts and a stand-in root (the defense reads
 * only the payload).
 */
export const retain = (block: Block) => {
  const { header, headerHash } = block.evidence;
  return DaPayloadsDB.upsertAvailable(
    foreignRetainedDaInsert(
      headerHash,
      header,
      header.endTime,
      Buffer.from(block.payloadEnvelopeCbor),
      { validationTracesRoot: "5a".repeat(32) },
      {
        validationTraceCount:
          header.forcedTransactionCount + header.l2TransactionCount,
      },
    ),
  );
};

/** The confirmed frontier at the failing block's parent. */
export const frontierAtStart = (scene: Scene) =>
  Frontier.upsert({
    headerHash: scene.failing.evidence.header.prevHeaderHash,
    utxosRoot: scene.failing.evidence.header.prevUtxosRoot,
  });

/** The node's landed rows: the frontier at the failing block's parent,
 * then the scene's three blocks as processed own rows, each retained. */
export const seedLanded = (scene: Scene) =>
  Effect.gen(function* () {
    const { failing, passing, forced } = scene;
    yield* frontierAtStart(scene);
    for (const block of [failing, passing, forced]) {
      const { header, headerHash } = block.evidence;
      yield* insertRow({
        headerHash,
        parentHeaderHash: header.prevHeaderHash,
        parentUtxosRoot: header.prevUtxosRoot,
        utxosRoot: header.utxosRoot,
        kind: "own",
        state: "processed",
        applied: true,
        ...delta(block.predecessorEntries, block.postEntries),
        depositIds: [],
        withdrawals: [],
        forcedIds: [],
        txIds: [],
      });
      yield* retain(block);
    }
  });

/**
 * A ledger store over `storage` (memory by default) retaining the ledgers
 * of `roots` among the scene's blocks' parent ledgers and post-states;
 * every one of them by default.
 */
export const sceneLedgerStore = async (
  scene: Scene,
  {
    roots,
    storage = createMemoryLedgerStorage(),
  }: Readonly<{ roots?: readonly string[]; storage?: LedgerStorage }> = {},
): Promise<LedgerStore> => {
  const store = createLedgerStore(storage);
  const ledgers = new Map<string, readonly Entry[]>();
  for (const { evidence, predecessorEntries, postEntries } of [
    scene.failing,
    scene.passing,
    scene.forced,
  ]) {
    ledgers.set(evidence.header.prevUtxosRoot, predecessorEntries);
    ledgers.set(evidence.header.utxosRoot, postEntries);
  }
  for (const [root, entries] of ledgers)
    if (roots === undefined || roots.includes(root))
      await store.admit(entries, root);
  return store;
};

/** The defense's ledgers over `store`, as the native owner gives them. */
export const ledgersOf = (store: LedgerStore): DefenseLedgers => ({
  retainedLedger: (root) =>
    Effect.tryPromise({ try: () => store.at(root), catch: (cause) => cause }),
});

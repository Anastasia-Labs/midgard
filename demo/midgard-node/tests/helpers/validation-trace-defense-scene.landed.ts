/**
 * The node's landed rows for the validation-trace Open scene: the scene's
 * blocks as processed own rows on a confirmed frontier at the failing
 * block's parent, each with its payload retained; and the node's wallet
 * configuration for the scene's operator.
 */
import type {
  Block,
  Scene,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import { Effect } from "effect";

import { foreignRetainedDaInsert } from "../../src/da/foreign-retained-da.js";
import { ConfirmedLedgerDB, DaPayloadsDB } from "../../src/database/index.js";
import {
  ledgerMap,
  ledgerRows,
  netDelta,
} from "../../src/landed-blocks/ledger.js";
import { Frontier, insertRow } from "../../src/landed-blocks/store.js";

type Entry = Readonly<{ outRef: Buffer; output: Buffer }>;
const minimal = (entries: readonly Entry[]) =>
  entries.map(({ outRef, output }) => ({ outref: outRef, output }));

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
    yield* ConfirmedLedgerDB.insertMultiple(
      yield* ledgerRows(minimal(failing.predecessorEntries), new Map()),
    );
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
        ...netDelta(
          ledgerMap(minimal(block.predecessorEntries)),
          minimal(block.postEntries),
        ),
        depositIds: [],
        withdrawals: [],
        forcedIds: [],
        txIds: [],
      });
      yield* retain(block);
    }
  });

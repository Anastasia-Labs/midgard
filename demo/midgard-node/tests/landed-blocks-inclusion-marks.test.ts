/**
 * Inclusion marks on the pending tables (plan §7.3, N3; migration 0013), on
 * the node database through the production writes and the production
 * working-ledger rebase (`prepareLandedBlockRebase`, with the modelled
 * native MPF owner of `landed-blocks-rebase.fixture.ts`):
 *
 * - this node's local finalization of its own block, and the processing
 *   insert of a landed block, keep the block's rows in the pending tables,
 *   marked by the block; the rollback of the block clears the marks, so a
 *   later rebuild reads its members as pending and the batch closure
 *   rejects them with a rejected co-member;
 * - an own block that lands and folds with no rebase between leaves its
 *   receipt members recorded settled, so a later rejection of a co-member
 *   leaves them settled;
 * - commit selection reads no marked row.
 */

import {
  computeHash32,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxWitnessSetCanonical,
} from "@al-ft/midgard-core/codec";
import { computeMidgardTxIdFromCanonicalCbor } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import {
  ConfirmedLedgerDB,
  MempoolDB,
  ProcessedMempoolDB,
  TxUtils,
} from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { foldToRoot } from "../src/landed-blocks/fold.js";
import { LANDED_BLOCK_REBASE_FAILED } from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import { processRow, rollBackRows } from "../src/landed-blocks/settlements.js";
import {
  Frontier,
  type LandedBlockRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { withHistoryWrite } from "../src/services/event-history-producer.js";
import { finalizeConfirmedMergeTransaction } from "../src/transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import { selectCommitTxCandidates } from "../src/workers/utils/commit-block-planner.select-commit-tx-candidates.js";
import { finalizeCommittedBlockLocally } from "../src/workers/utils/commit-submission.js";
import {
  admitPending,
  type SimPendingTx,
} from "./helpers/landed-blocks-sim.mempool.js";
import { simOutput } from "./helpers/landed-blocks-sim.universe.js";
import {
  attempt,
  E0,
  E1,
  entry,
  freshNative,
  FRONTIER,
  hex,
  landedRow,
  type Native,
  processOf,
  R0,
  R1,
  receipt,
  rejections,
  released,
  root,
  run,
  settlements,
  sqlRun,
  unreversedReceipts,
} from "./landed-blocks-rebase.fixture.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { resetApplicationTables } from "./utils.js";

type Globals = Awaited<ReturnType<typeof processOf>>;

const OWN = "0a".repeat(28);
const FOREIGN = "c1".repeat(28);
const REPLACEMENT = "c3".repeat(28);
const NEXT = "c2".repeat(28);
const E2 = entry("e2", 4_000_000n);
const E3 = entry("e3", 5_000_000n);
const R2 = root(0x12);
const R3 = root(0x13);

const emptyList = Buffer.from([0x80]);

/**
 * A pending transaction whose id is its Midgard-native payload's (the
 * immutable store checks it); its ledger effect is the delta `spent` →
 * one output.
 */
const nativeTx = (
  label: number,
  spent: readonly Buffer[],
  at: number,
): SimPendingTx => {
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: emptyList,
    referenceInputsPreimageCbor: emptyList,
    outputsPreimageCbor: emptyList,
    fee: BigInt(label),
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: emptyList,
    requiredSignersPreimageCbor: emptyList,
    mintPreimageCbor: emptyList,
    scriptIntegrityHash: computeHash32(Buffer.from([0xf6])),
    auxiliaryDataHash: computeHash32(Buffer.from([0xf6])),
    networkId: 0n,
  };
  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor: emptyList,
    scriptTxWitsPreimageCbor: emptyList,
    redeemerTxWitsPreimageCbor: emptyList,
  };
  const cbor = Buffer.from(
    encodeMidgardNativeTxCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body,
      witnessSet,
      compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
    }),
  );
  const id = Buffer.from(computeMidgardTxIdFromCanonicalCbor(cbor));
  return {
    id,
    spent,
    produced: [
      {
        outref: makeOutRefCbor(id, 0),
        output: simOutput(6_000_000n + BigInt(label)),
      },
    ],
    at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 1_000),
    cbor,
  };
};

/** `confirmed_ledger` at the frontier holds `E0`; no landed block yet. */
const seedFrontier = async (globals: Globals) => {
  await released(globals);
  await run(
    globals,
    withHistoryWrite(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* ConfirmedLedgerDB.insertMultiple([
          ...(yield* ledgerRows([E0], new Map())),
        ]);
        yield* Frontier.upsert({ headerHash: FRONTIER, utxosRoot: R0 });
      }),
    ),
  );
};

/** The pending mempool rows, in the order commit selection reads them. */
const pendingPage = (globals: Globals) =>
  run(globals, MempoolDB.retrievePage({ limit: 100 })).then((page) =>
    page.entries.map((row) => hex(row[TxUtils.Columns.TX_ID])),
  );

/** This node's local finalization of its own block `header` including `txs`. */
const finalizeOwnLocally = async (
  globals: Globals,
  header: string,
  txs: readonly SimPendingTx[],
) => {
  const ids = new Set(txs.map((tx) => hex(tx.id)));
  const entries = (
    await run(globals, MempoolDB.retrievePage({ limit: 100 }))
  ).entries.filter((row) => ids.has(hex(row[TxUtils.Columns.TX_ID])));
  expect(entries).toHaveLength(txs.length);
  const transactionsMpf = {
    resetToEmpty: () => Effect.void,
  } as unknown as Parameters<typeof finalizeCommittedBlockLocally>[0];
  await released(globals);
  await run(
    globals,
    finalizeCommittedBlockLocally(
      transactionsMpf,
      entries,
      txs.map((tx) => tx.id),
      header,
      [],
      { useAmbientProcessedMempool: false },
    ),
  );
};

/** The processing insert of the landed block `row`. */
const processLanded = (globals: Globals, row: Partial<LandedBlockRow>) =>
  sqlRun(globals, () => processRow(landedRow(row)));

const sortedRejections = (pairs: (readonly [Buffer, string])[]) =>
  pairs
    .map(([id, code]) => [hex(id), code])
    .sort(([x], [y]) => (x! < y! ? -1 : 1));

describe("inclusion marks on the pending tables", { concurrent: false }, () => {
  it("rebuilds the members of a rolled-back own block as pending: the batch closure rejects its receipt co-member", async () => {
    // frontier (E0) -> own block O (E0 -> E1, includes b and d); receipt
    // {a, b}, a spends E1. O is rolled back; foreign Z (E0 -> E3) lands on
    // the frontier instead.
    const b = nativeTx(1, [], 1);
    const d = nativeTx(2, [], 2);
    const a = nativeTx(3, [E1.outref], 3);
    const native: Native = freshNative();
    const globals = await processOf(native);
    await seedFrontier(globals);
    await run(globals, admitPending([b, d]));
    await finalizeOwnLocally(globals, OWN, [b, d]);
    await run(globals, admitPending([a]));
    await receipt(globals, [a.id, b.id]);
    await processLanded(globals, {
      headerHash: OWN,
      kind: "own",
      txIds: [b.id, d.id],
    });
    // O's members are not pending while O holds them.
    expect(await pendingPage(globals)).toEqual([hex(a.id)]);
    const first = await attempt(globals);
    expect(first.failure).toBeUndefined();
    expect(await rejections(globals)).toEqual([]);

    // The production rollback of O, then the processing insert of Z.
    const left = await run(globals, retrieveRows);
    await sqlRun(globals, () => rollBackRows(left, []));
    expect(await settlements(globals)).toEqual([]);
    await processLanded(globals, {
      headerHash: REPLACEMENT,
      utxosRoot: R3,
      produced: [E3],
    });
    native.reaches = R3;
    const shown = await attempt(globals);
    expect(Exit.isSuccess(shown.exit)).toBe(true);
    expect(shown.failure).toBeUndefined();
    expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
    expect(shown.disposition).toBeUndefined();
    expect(shown.applied).toEqual([true]);
    expect(await rejections(globals)).toEqual(
      sortedRejections([
        [a.id, REBASE_REJECTIONS.direct.code],
        [b.id, REBASE_REJECTIONS.batch.code],
      ]),
    );
    expect(await unreversedReceipts(globals)).toBe(0);
    // d is pending again and applies on Z.
    expect(await pendingPage(globals)).toEqual([hex(d.id)]);
    expect(shown.working.sort()).toEqual(
      [hex(E3.outref), hex(d.produced[0]!.outref)].sort(),
    );
  });

  it("rebuilds the members of a rolled-back foreign block as pending: the batch closure rejects its receipt co-member", async () => {
    // frontier (E0) -> foreign X (E0 -> E1, includes b and d); receipt
    // {a, b}, a spends E1. X is rolled back; foreign Z (E0 -> E3) lands on
    // the frontier instead.
    const b = nativeTx(11, [], 1);
    const d = nativeTx(12, [], 2);
    const a = nativeTx(13, [E1.outref], 3);
    const native: Native = freshNative();
    const globals = await processOf(native);
    await seedFrontier(globals);
    await run(globals, admitPending([b, d, a]));
    await receipt(globals, [a.id, b.id]);
    await processLanded(globals, { headerHash: FOREIGN, txIds: [b.id, d.id] });
    const first = await attempt(globals);
    expect(first.failure).toBeUndefined();
    expect(first.applied).toEqual([true]);
    expect(await rejections(globals)).toEqual([]);
    expect(await settlements(globals)).toEqual([[hex(b.id), FOREIGN]]);
    // X's members are not pending while X holds them.
    expect(await pendingPage(globals)).toEqual([hex(a.id)]);

    const left = await run(globals, retrieveRows);
    await sqlRun(globals, () => rollBackRows(left, []));
    await processLanded(globals, {
      headerHash: REPLACEMENT,
      utxosRoot: R3,
      produced: [E3],
    });
    native.reaches = R3;
    const shown = await attempt(globals);
    expect(Exit.isSuccess(shown.exit)).toBe(true);
    expect(shown.failure).toBeUndefined();
    expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
    expect(shown.disposition).toBeUndefined();
    expect(shown.applied).toEqual([true]);
    expect(await rejections(globals)).toEqual(
      sortedRejections([
        [a.id, REBASE_REJECTIONS.direct.code],
        [b.id, REBASE_REJECTIONS.batch.code],
      ]),
    );
    expect(await unreversedReceipts(globals)).toBe(0);
    expect(await pendingPage(globals)).toEqual([hex(d.id)]);
    expect(shown.working.sort()).toEqual(
      [hex(E3.outref), hex(d.produced[0]!.outref)].sort(),
    );
  });

  it("keeps the members of an own block that landed and folded with no rebase between settled when a later rebuild rejects a co-member", async () => {
    // frontier (E0) -> own block O (E0 -> E1, includes b); receipt {a, b},
    // a spends E1. O lands and its merge finalizes with no rebase between;
    // foreign Y (E1 -> E2) on O then rejects a.
    const b = nativeTx(21, [], 1);
    const a = nativeTx(22, [E1.outref], 2);
    const native: Native = freshNative();
    const globals = await processOf(native);
    await seedFrontier(globals);
    await run(globals, admitPending([b]));
    await finalizeOwnLocally(globals, OWN, [b]);
    await run(globals, admitPending([a]));
    await receipt(globals, [a.id, b.id]);
    await processLanded(globals, {
      headerHash: OWN,
      kind: "own",
      txIds: [b.id],
    });

    // The own merge finalization, then the production fold past O.
    const before = await run(globals, ledgerRows([E0], new Map()));
    const after = await run(globals, ledgerRows([E1], new Map()));
    const ledgerRoot = (entries: readonly Ledger.Entry[]) =>
      run(globals, computeLedgerMpfRootFromLedgerEntries(entries));
    const delta = { spent: [E0.outref], produced: after };
    await released(globals);
    await run(
      globals,
      finalizeConfirmedMergeTransaction({
        headerHash: Buffer.from(OWN, "hex"),
        snapshot: {
          entries: after,
          baseRoot: await ledgerRoot(before),
          root: await ledgerRoot(after),
          deltaChain: [delta],
          delta,
        },
        projectedDepositEventIds: [],
        projectedWithdrawalEventIds: [],
        projectedForcedTransactionEventIds: [],
        includedTxIds: [b.id],
      }),
    );
    const foldPorts = {
      confirmView: () => Effect.succeed(true),
      write: <A, E, R>(work: Effect.Effect<A, E, R>) => withHistoryWrite(work),
      ownMergeCompleted: () => Effect.succeed(true),
    } as unknown as LandedBlockPorts<never>;
    await sqlRun(globals, () =>
      foldToRoot(foldPorts, {} as never, { headerHash: OWN, utxosRoot: R1 }),
    );
    expect(await run(globals, retrieveRows)).toEqual([]);

    await processLanded(globals, {
      headerHash: NEXT,
      parentHeaderHash: OWN,
      parentUtxosRoot: R1,
      utxosRoot: R2,
      spent: [E1.outref],
      produced: [E2],
    });
    native.reaches = R2;
    const shown = await attempt(globals);
    expect(Exit.isSuccess(shown.exit)).toBe(true);
    expect(shown.failure).toBeUndefined();
    expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
    expect(shown.disposition).toBeUndefined();
    expect(shown.applied).toEqual([true]);
    expect(shown.working).toEqual([hex(E2.outref)]);
    // a is rejected; b stays settled by O.
    expect(await rejections(globals)).toEqual([
      [hex(a.id), REBASE_REJECTIONS.direct.code],
    ]);
    expect(await settlements(globals)).toEqual([[hex(b.id), OWN]]);
    expect(await unreversedReceipts(globals)).toBe(0);
    expect(await pendingPage(globals)).toEqual([]);
  });

  it("selects no row a block marked for a new block", async () => {
    // m2 is in this node's locally finalized own block; foreign X includes
    // m1 and the processed-mempool row p1.
    const [m1, m2, m3] = [31, 32, 33].map((label, at) =>
      nativeTx(label, [], at + 1),
    );
    const [p1, p2] = [34, 35].map((label, at) => nativeTx(label, [], at + 4));
    const globals = await processOf(freshNative());
    await seedFrontier(globals);
    await run(globals, admitPending([m1!, m2!, m3!]));
    await sqlRun(globals, () =>
      ProcessedMempoolDB.insertTxs(
        [p1!, p2!].map((tx) => ({
          [TxUtils.Columns.TX_ID]: tx.id,
          [TxUtils.Columns.TX]: tx.cbor!,
        })),
      ),
    );
    await finalizeOwnLocally(globals, OWN, [m2!]);
    await processLanded(globals, {
      headerHash: FOREIGN,
      txIds: [m1!.id, p1!.id],
    });

    // The commit worker's selection reads.
    const select = async () => {
      const page = await run(globals, MempoolDB.retrievePage({ limit: 100 }));
      const processed = await run(globals, ProcessedMempoolDB.retrieve);
      return selectCommitTxCandidates({
        mempoolTxs: page.entries,
        processedMempoolTxs: processed,
      }).candidateTxHashes.map(hex);
    };
    expect(await select()).toEqual([hex(p2!.id)]);
    await sqlRun(globals, () => ProcessedMempoolDB.clearTxs([p2!.id]));
    expect(await select()).toEqual([hex(m3!.id)]);
    expect(await run(globals, MempoolDB.retrieveTxCount)).toBe(1n);
    expect(
      await run(globals, MempoolDB.retrieveTxCborsByHashes([m1!.id, m2!.id])),
    ).toEqual([]);
    // The rows stay in their tables until their blocks fold.
    const kept = await run(
      globals,
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool
          UNION ALL SELECT tx_id FROM processed_mempool`,
      ),
    );
    expect(kept.map((row) => hex(row.tx_id)).sort()).toEqual(
      [m1!, m2!, m3!, p1!].map((tx) => hex(tx.id)).sort(),
    );
  });
});

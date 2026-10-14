import "./utils.js";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { runPhaseAValidation } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  plutusCountdownSpend,
  type WallCapCandidate,
} from "../../midgard-validation/tests/phase-b.script-wall-cap.fixture.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  makePhaseBCandidate,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import {
  MempoolLedgerDB,
  MigrationRunner,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../src/database/index.js";
import { decideAdmissionBatch } from "../src/fibers/tx-queue-processor.classify-plutus-evaluation-failure.js";
import { collectAcceptedProgramEnvelopes } from "../src/fibers/tx-queue-processor.js";
import { persistAdmissionDecision } from "../src/fibers/tx-queue-processor.persist-admission-decision.js";
import { admissionToQueuedTx } from "../src/fibers/tx-queue-processor.run-phase-afor-batch.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * A transaction the node refuses for its own reasons stays queued with its
 * submitted bytes and is decided again later; a transaction rejected for its
 * own content stays rejected, and the same bytes submitted again change
 * nothing.
 */

// Tens of thousands of CEK steps: far longer than a 1 ms cap.
const SLOW_ITERATIONS = 2_000;
const CLAIM_WINDOW_MS = 30_000;

const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E>,
  );

const submit = (entry: WallCapCandidate) =>
  TxAdmissionsDB.admit({
    txId: entry.candidate.ledgerTx.txId,
    txCanonicalCbor: entry.candidate.submission.txCbor,
    programMaterialSidecarCbor:
      entry.candidate.submission.programMaterialSidecarCbor ??
      encodeMidgardCekProgramMaterialSidecar([]),
    submitSource: "native",
    currentBacklog: 0n,
    maxBacklog: 10,
  });

/** A plain transaction spending the first output of `parent`. */
const childOf = (parent: WallCapCandidate): WallCapCandidate => ({
  candidate: makePhaseBCandidate({
    spent: [outRefFromTxId(parent.candidate.ledgerTx.txId)],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
  }),
  preState: [],
});

const statusOf = (entry: WallCapCandidate) =>
  Effect.map(
    TxAdmissionsDB.getByTxId(entry.candidate.ledgerTx.txId),
    (row) => row?.[TxAdmissionsDB.Columns.STATUS],
  );

/** Puts the outputs `entries` spend into the mempool ledger. */
const fund = (entries: readonly WallCapCandidate[]) =>
  MempoolLedgerDB.insert(
    entries.flatMap((entry) =>
      entry.preState.map(([outRefHex, output]) => {
        const outRef = Buffer.from(outRefHex, "hex");
        return {
          [MempoolLedgerDB.Columns.TX_ID]: outRef.subarray(3, 35),
          [MempoolLedgerDB.Columns.OUTREF]: outRef,
          [MempoolLedgerDB.Columns.OUTPUT]: output,
          [MempoolLedgerDB.Columns.ADDRESS]: encodeMidgardAddressText(
            decodeMidgardTxOutput(output).address,
          ),
          [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
        };
      }),
    ),
  );

const sidecarOf = (txId: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly sidecar: Buffer }>`
      SELECT ${sql(TxAdmissionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)}
        AS sidecar
      FROM ${sql(TxAdmissionsDB.payloadTableName)}
      WHERE ${sql(TxAdmissionsDB.Columns.TX_ID)} = ${txId}`;
    return Buffer.from(rows[0]!.sidecar);
  });

/**
 * One queue tick over the given candidates: claims what is due (polling, as
 * the node's own loop re-claims on its next tick), decides the claimed batch
 * from the claimed payloads against the mempool ledger and persists the
 * decision.
 */
const tick = (entries: readonly WallCapCandidate[], scriptWallCapMs?: number) =>
  Effect.gen(function* () {
    const leaseOwner = `local-refusal-retry:${Math.random().toString(16).slice(2)}`;
    const startedAt = performance.now();
    let claimed = yield* TxAdmissionsDB.claimBatchLease({
      limit: entries.length,
      leaseOwner,
      leaseDurationMs: 30_000,
    });
    while (
      claimed.length === 0 &&
      performance.now() - startedAt < CLAIM_WINDOW_MS
    ) {
      yield* Effect.sleep("100 millis");
      claimed = yield* TxAdmissionsDB.claimBatchLease({
        limit: entries.length,
        leaseOwner,
        leaseDurationMs: 30_000,
      });
    }
    const rows = yield* TxAdmissionsDB.loadClaimedPayloads({
      claimed,
      leaseOwner,
    });
    const phaseA = yield* runPhaseAValidation(rows.map(admissionToQueuedTx), {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    });
    expect(phaseA.rejected).toHaveLength(0);
    const ledgerState = new Map(
      (yield* MempoolLedgerDB.retrieveSpendable).map((entry) => [
        entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
        entry[MempoolLedgerDB.Columns.OUTPUT],
      ]),
    );
    const decision = yield* decideAdmissionBatch({
      phaseA,
      pendingWithdrawalOutRefHexes: new Set(),
      ledgerState,
      phaseBConfig: {
        nowCardanoSlotNo: 100n,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        bucketConcurrency: 1,
        enforceScriptBudget: true,
        ...(scriptWallCapMs === undefined ? {} : { scriptWallCapMs }),
      },
    });
    yield* persistAdmissionDecision({
      rows: claimed,
      leaseOwner,
      decision,
      programEnvelopesByTxId: collectAcceptedProgramEnvelopes(
        decision.phaseB.accepted,
        ledgerState,
      ),
    });
    return claimed.length;
  });

beforeAll(async () => {
  await run(
    MigrationRunner.migrate({
      appVersion: "admission-local-refusal-retry-test",
      actor: "admission-local-refusal-retry-test",
    }),
  );
});

describe("admission of a transaction after a local refusal", () => {
  it("decides a transaction refused by the submit wall cap again, with its submitted bytes", async () => {
    const slow = plutusCountdownSpend(0xc1, SLOW_ITERATIONS);
    const txId = slow.candidate.ledgerTx.txId;
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* fund([slow]);
        expect((yield* submit(slow)).kind).toBe("new");

        expect(yield* tick([slow], 1)).toBe(1);
        const deferred = yield* TxAdmissionsDB.getByTxId(txId);
        expect(deferred?.[TxAdmissionsDB.Columns.STATUS]).toBe(
          TxAdmissionsDB.Status.Queued,
        );
        expect(yield* TxRejectionsDB.retrieveByTxId(txId)).toHaveLength(0);
        expect(yield* sidecarOf(txId)).toStrictEqual(
          slow.candidate.submission.programMaterialSidecarCbor,
        );

        expect(yield* tick([slow])).toBe(1);
        const admitted = yield* TxAdmissionsDB.getByTxId(txId);
        expect(admitted?.[TxAdmissionsDB.Columns.STATUS]).toBe(
          TxAdmissionsDB.Status.Accepted,
        );
      }),
    );
  }, 120_000);

  it("keeps a transaction spending the output of one refused by the submit wall cap queued with it", async () => {
    const slow = plutusCountdownSpend(0xc3, SLOW_ITERATIONS);
    const child = childOf(slow);
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* fund([slow]);
        expect((yield* submit(slow)).kind).toBe("new");
        expect((yield* submit(child)).kind).toBe("new");

        expect(yield* tick([slow, child], 1)).toBe(2);
        expect(yield* statusOf(slow)).toBe(TxAdmissionsDB.Status.Queued);
        expect(yield* statusOf(child)).toBe(TxAdmissionsDB.Status.Queued);
        expect(
          yield* TxRejectionsDB.retrieveByTxId(child.candidate.ledgerTx.txId),
        ).toHaveLength(0);

        expect(yield* tick([slow, child])).toBe(2);
        expect(yield* statusOf(slow)).toBe(TxAdmissionsDB.Status.Accepted);
        expect(yield* statusOf(child)).toBe(TxAdmissionsDB.Status.Accepted);
      }),
    );
  }, 120_000);

  it("keeps a transaction rejected for a missing input rejected when the same bytes are submitted again", async () => {
    const orphan = plutusCountdownSpend(0xc2, SLOW_ITERATIONS);
    const txId = orphan.candidate.ledgerTx.txId;
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        expect((yield* submit(orphan)).kind).toBe("new");

        expect(yield* tick([orphan])).toBe(1);
        const rejected = yield* TxAdmissionsDB.getByTxId(txId);
        expect(rejected?.[TxAdmissionsDB.Columns.STATUS]).toBe(
          TxAdmissionsDB.Status.Rejected,
        );

        expect((yield* submit(orphan)).kind).toBe("duplicate");
        const resubmitted = yield* TxAdmissionsDB.getByTxId(txId);
        expect(resubmitted?.[TxAdmissionsDB.Columns.STATUS]).toBe(
          TxAdmissionsDB.Status.Rejected,
        );
        expect(
          yield* TxAdmissionsDB.claimBatchLease({
            limit: 1,
            leaseOwner: "local-refusal-retry:resubmitted",
            leaseDurationMs: 30_000,
          }),
        ).toHaveLength(0);
      }),
    );
  }, 120_000);
});

import {
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  EMPTY_CBOR_LIST,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, vi } from "vitest";

import * as DaProducer from "../../src/da/libp2p-producer.js";
import {
  CekProgramMaterialDB,
  DaPayloadsDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import { classifyForcedTransactions } from "../../src/mpf/event-window.classify-forced-transactions.js";
import { programMaterialSidecarForEnvelopes } from "../../src/mpf/event-window.forced-verdict-for-rejection.js";
import { encodeTransactionRootValue } from "../../src/mpf/index.js";
import { NodeConfig } from "../../src/services/config.js";
import { finalizeCommittedBlockLocally } from "../../src/workers/utils/commit-submission.finalize-committed-block-locally.js";
import { buildJournalFixture } from "../da-payload.build-journal-fixture.js";
import { member, sourceRoot } from "../da-payload.record.js";
import {
  forcedEntry,
  makeOutput,
  makeSignedEffectfulTransaction,
} from "../forced-transactions.make-signed-effectful-transaction.js";
import { makeOutRefCbor } from "../midgard-output-helpers.js";
import {
  daPayloadInsertFixture,
  databaseFixtureBytes,
  isolatedDb,
  makeMaterialProofSubmitTx,
  readCekProgramMaterialStoreStats,
} from "./fixtures.js";
import { registerRetainedScriptMaterialQuotaTests } from "./retained-script-material-quota.js";

const scriptOutput = (attempt: ReturnType<typeof makeMaterialProofSubmitTx>) =>
  encodeMidgardTxOutput({
    ...decodeMidgardTxOutput(makeOutput(5_000_000n)),
    script_ref: {
      language: "MidgardV1",
      scriptBytes: encodeMidgardCekProgramEnvelope(attempt.envelope),
    },
  });
const owners = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<{
    readonly header_hash: Buffer;
  }>`SELECT DISTINCT header_hash FROM cek_program_material_retained_state_owners ORDER BY header_hash`;
});

export const registerRetainedScriptMaterialTests = () => {
  describe("retained L2 script material", () => {
    registerRetainedScriptMaterialQuotaTests(scriptOutput);
    it.effect(
      "does not charge retained live-state bytes to the bounded admission cache",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const first = makeMaterialProofSubmitTx(104);
            const next = makeMaterialProofSubmitTx(105);
            yield* CekProgramMaterialDB.persistVerifiedBundles(
              [first.envelope],
              [first.material],
              { kind: "admission", txId: first.txId },
            );
            const admissionBytes = Number(
              (yield* readCekProgramMaterialStoreStats).total_bytes,
            );
            const insert = daPayloadInsertFixture("live-pin-cache-cap");
            yield* DaPayloadsDB.upsertAvailable(insert);
            yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
              headerHash: insert.header_hash,
              outputs: [scriptOutput(first)],
              material: [],
            });
            yield* CekProgramMaterialDB.releaseAdmissionOwnership([first.txId]);
            const config = yield* NodeConfig;
            yield* CekProgramMaterialDB.persistVerifiedBundles(
              [next.envelope],
              [next.material],
              { kind: "admission", txId: next.txId },
            ).pipe(
              Effect.provideService(NodeConfig, {
                ...config,
                CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES: admissionBytes,
              }),
            );
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                next.envelope,
              ]),
            ).toEqual([next.material]);
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                first.envelope,
              ]),
            ).toEqual([first.material]);
          }),
        ),
    );

    it.effect(
      "pins journal script_refs before local finalization releases their admissions and classifies a later forced reference",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const original = makeMaterialProofSubmitTx(101);
            const output = scriptOutput(original);
            const base = decodeMidgardNativeTxFullFromCanonicalCbor(
              original.txCanonicalCbor,
            );
            const txCanonicalCbor = encodeMidgardNativeTxCanonical(
              materializeMidgardNativeTxFromCanonical({
                ...base,
                body: {
                  ...base.body,
                  outputsPreimageCbor: encodeCbor([output]),
                },
                witnessSet: {
                  ...base.witnessSet,
                  scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
                },
              }),
            );
            const txId = computeMidgardNativeTxId(
              decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
            );
            const attempt = { ...original, txCanonicalCbor, txId };
            const outref = makeOutRefCbor(attempt.txId);
            const transactionsRoot = yield* sourceRoot(
              SDK.ROOT_DOMAINS.transactionsV1,
              [
                [
                  attempt.txId,
                  encodeTransactionRootValue(
                    attempt.txCanonicalCbor,
                    MIDGARD_CONSENSUS_PROFILE,
                  ),
                ],
              ],
            );
            const validationTraceValue = encodeCbor([1n]);
            const validationTracesRoot = yield* sourceRoot(
              SDK.ROOT_DOMAINS.validationTraces,
              [[attempt.txId, validationTraceValue]],
            );
            const rawFixture = yield* Effect.promise(() =>
              buildJournalFixture({
                rootOverrides: { transactionsRoot, validationTracesRoot },
                utxoEntries: [[outref, output]],
                txEntries: [[attempt.txId, attempt.txCanonicalCbor]],
                transitionTraceEntries: [
                  [
                    databaseFixtureBytes("pin-trace-key", 32),
                    databaseFixtureBytes("pin-trace-value", 48),
                  ],
                ],
                eventToStepEntries: [
                  [
                    databaseFixtureBytes("pin-map-key", 32),
                    databaseFixtureBytes("pin-map-value", 48),
                  ],
                ],
              }),
            );
            const header = { ...rawFixture.header, validationTraceCount: 1n };
            const headerHash = Buffer.from(
              yield* SDK.hashBlockHeader(header),
              "hex",
            );
            const fixture = { ...rawFixture, headerHash };
            const record = {
              ...rawFixture.pending,
              [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: headerHash,
              [PendingBlockFinalizationsDB.Columns.HEADER_CBOR]: Buffer.from(
                Data.to(header as never, SDK.Header as never),
                "hex",
              ),
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_VALIDATION_TRACE_COUNT]: 1n,
              validationTraceMembers: [
                member(headerHash, attempt.txId, validationTraceValue, 0),
              ],
              txMembers: fixture.pending.txMembers.map((member) => ({
                ...member,
                [PendingBlockFinalizationsDB.MemberColumns
                  .CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: attempt.sidecarCbor,
              })),
            };
            yield* CekProgramMaterialDB.persistVerifiedBundles(
              [attempt.envelope],
              [attempt.material],
              { kind: "admission", txId: attempt.txId },
            );
            yield* MempoolDB.insertMultipleCore([
              {
                txId: attempt.txId,
                txCbor: attempt.txCanonicalCbor,
                spent: [],
                produced: [],
              },
            ]);
            const entries = (yield* MempoolDB.retrievePage({ limit: 1 }))
              .entries;
            let reset = false;
            const mpf = {
              resetToEmpty: () =>
                Effect.sync(() => {
                  reset = true;
                }),
            } as unknown as Parameters<typeof finalizeCommittedBlockLocally>[0];
            // Peer-delivery outbox setup is outside this SQL retention test.
            const outbox = vi
              .spyOn(DaProducer, "seedDaPayloadPublicationOutboxFromEnv")
              .mockReturnValue(Effect.void);
            yield* finalizeCommittedBlockLocally(
              mpf,
              entries,
              [attempt.txId],
              fixture.headerHash.toString("hex"),
              [],
              { useAmbientProcessedMempool: false, daPayloadRecord: record },
            ).pipe(Effect.ensuring(Effect.sync(() => outbox.mockRestore())));
            expect(reset).toBe(true);
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                attempt.envelope,
              ]),
            ).toEqual([attempt.material]);
            expect(yield* owners).toEqual([
              { header_hash: fixture.headerHash },
            ]);
            const sql = yield* SqlClient.SqlClient;
            expect(
              yield* sql`SELECT * FROM cek_program_material_admission_owners`,
            ).toEqual([]);
            const absentSpend = makeOutRefCbor(77);
            const transaction = makeSignedEffectfulTransaction(
              absentSpend,
              makeOutput(5_000_000n),
              { referenceInputs: [outref] },
            );
            const entry = yield* Effect.promise(() =>
              forcedEntry({ label: 7, transaction }),
            );
            const classified = yield* classifyForcedTransactions({
              entries: [entry],
              initialState: new Map([[outref.toString("hex"), output]]),
              effectiveEndTime: new Date("2026-07-23T12:01:00Z"),
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              validation: {
                expectedNetworkId: 0n,
                minFeeA: 0n,
                minFeeB: 0n,
                bucketConcurrency: 1,
                slotForUnixTime: () => 100n,
              },
              resolveProgramMaterialSidecar: programMaterialSidecarForEnvelopes,
            });
            expect(classified).toHaveLength(1);
            expect(classified[0].rejectionCode).toBe(RejectCodes.InputNotFound);
            expect(
              decodeMidgardCekProgramMaterialSidecar(
                classified[0].programMaterialSidecarCbor,
              ),
            ).toEqual([attempt.material]);
            // Reopening/restoring owners uses the retained payload and is idempotent.
            yield* CekProgramMaterialDB.restoreRetainedStatePins;
            yield* CekProgramMaterialDB.restoreRetainedStatePins;
            expect(yield* owners).toEqual([
              { header_hash: fixture.headerHash },
            ]);
          }),
        ),
    );

    it.effect(
      "keeps inherited live refs at the authenticated head and releases the last spent ref beyond its retention horizon",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const attempt = makeMaterialProofSubmitTx(102);
            const output = scriptOutput(attempt);
            const first = {
              ...daPayloadInsertFixture("live-pin-1"),
              block_start_time: new Date("2026-01-01"),
              block_end_time: new Date("2026-01-02"),
            };
            const second = {
              ...daPayloadInsertFixture("live-pin-2"),
              block_start_time: new Date("2026-01-02"),
              block_end_time: new Date("2026-01-03"),
            };
            const spent = {
              ...daPayloadInsertFixture("live-pin-spent"),
              block_start_time: new Date("2026-01-03"),
              block_end_time: new Date("2026-01-04"),
            };
            yield* DaPayloadsDB.upsertAvailable(first);
            yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
              headerHash: first.header_hash,
              outputs: [output, output],
              material: [attempt.material],
            });
            yield* DaPayloadsDB.upsertAvailable(second);
            yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
              headerHash: second.header_hash,
              outputs: [output],
              material: [],
            });
            const prune = (
              cutoff: Date,
              head: Buffer,
              live: readonly Buffer[] = [],
            ) =>
              DaPayloadsDB.pruneBeyondRetention({
                challengeableCutoff: cutoff,
                view: { confirmedHeadHash: head, liveQueueHeaderHashes: live },
                deploymentIdentityDigest: undefined,
              });
            expect(
              yield* prune(new Date("2027-01-01"), second.header_hash, [
                first.header_hash,
              ]),
            ).toBe(0);
            expect(
              yield* prune(new Date("2027-01-01"), second.header_hash),
            ).toBe(1);
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                attempt.envelope,
              ]),
            ).toEqual([attempt.material]);
            yield* DaPayloadsDB.upsertAvailable(spent);
            yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
              headerHash: spent.header_hash,
              outputs: [],
              material: [],
            });
            expect(yield* prune(second.block_end_time, spent.header_hash)).toBe(
              0,
            );
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                attempt.envelope,
              ]),
            ).toEqual([attempt.material]);
            expect(
              yield* prune(
                new Date(second.block_end_time.getTime() + 1),
                spent.header_hash,
              ),
            ).toBe(1);
            expect(yield* owners).toEqual([]);
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                attempt.envelope,
              ]).pipe(Effect.either),
            ).toMatchObject({
              _tag: "Left",
              left: {
                message:
                  "Durable CEK program material bundle is incomplete or malformed",
              },
            });
            const sql = yield* SqlClient.SqlClient;
            expect(
              yield* sql`SELECT * FROM cek_program_material_entries`,
            ).toEqual([]);
          }),
        ),
    );

    it.effect(
      "rolls back the DA row and its pins when a live ref has incomplete material",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const attempt = makeMaterialProofSubmitTx(103);
            const insert = daPayloadInsertFixture("live-pin-atomic");
            const sql = yield* SqlClient.SqlClient;
            const failed = yield* sql
              .withTransaction(
                Effect.gen(function* () {
                  yield* DaPayloadsDB.upsertAvailable(insert);
                  yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
                    headerHash: insert.header_hash,
                    outputs: [scriptOutput(attempt)],
                    material: [],
                  });
                }),
              )
              .pipe(Effect.either);
            expect(failed).toMatchObject({
              _tag: "Left",
              left: {
                message: "Live L2 script_ref has no complete retained material",
              },
            });
            expect(
              Option.isNone(
                yield* DaPayloadsDB.retrieveByHeaderHash(insert.header_hash),
              ),
            ).toBe(true);
            expect(yield* owners).toEqual([]);
          }),
        ),
    );
  });
};

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { decodeMidgardUtxo } from "@al-ft/lucid-midgard";
import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  midgardOutRefToCbor,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import {
  CML,
  coreToTxOutput,
  Data,
  Emulator,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Deferred, Effect, Option, Ref, Schedule } from "effect";
import { Level } from "level";
import { expect, vi } from "vitest";

import {
  buildTransferTxWithMinFee,
  fetchLocalUtxos,
  makeTransferMidgard,
  toQueuedTx,
} from "../../src/commands/submit-l2-transfer.js";
import type { BuiltTransferTx } from "../../src/commands/transfer-build-core.js";
import * as Authority from "../../src/database/eventHistoryAuthority.js";
import * as Journal from "../../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB, TxAdmissionsDB } from "../../src/database/index.js";
import { txQueueProcessorDrainOnce } from "../../src/fibers/tx-queue-processor.js";
import { decodeBoundEventHistoryLedgerSnapshot } from "../../src/l1-event-history-source.js";
import { Database } from "../../src/services/database.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import { validationPoolLayer } from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  mergeMaturityWindow,
  type ProductionHistoryFixtureRuntime,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runMergeUntilMerged,
  SDK,
  serializeStateQueueUTxO,
  stateQueueFetchConfig,
} from "../deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import {
  type AcceptedHistoryObservation,
  captureConfirmedHistoryObservations,
  historyOutputObservation,
  submitHistoryObservation,
} from "./history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./history-rollback-transport.js";
import { createMainnetEmulatorLucid } from "./mainnet-protocol-parameters.js";

const snapshotEmulator = (emulator: Emulator) =>
  structuredClone({
    ledger: emulator.ledger,
    mempool: emulator.mempool,
    chain: emulator.chain,
    blockHeight: emulator.blockHeight,
    slot: emulator.slot,
    time: emulator.time,
    protocolParameters: emulator.protocolParameters,
    datumTable: emulator.datumTable,
    treasury: emulator.treasury,
    transactionHistory: emulator.transactionHistory,
  });
const restoreEmulator = (
  emulator: Emulator,
  snapshot: ReturnType<typeof snapshotEmulator>,
) => {
  Object.assign(emulator, structuredClone(snapshot));
};
const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

/** Actual deployed validators, signed L1 receipts, signed L2 admission/validation,
 * production owner, inverse SQL, cache and native owner. Only chain ancestry,
 * genesis transport and rollback selection are controlled source models. This
 * does not exercise real network consensus or HTTP admission. Deposit-bearing
 * L1 commitments are real; the dependent L2 transfer remains unpublished. */
export const runHistoryOwnerRollbackJourney = async ({
  dependency = "spend",
  evidencePath,
  restartWhilePending = false,
}: {
  readonly dependency?: "spend" | "descendant" | "reference";
  readonly evidencePath?: string;
  readonly restartWhilePending?: boolean;
} = {}) => {
  let transport: ReturnType<typeof makeRollbackHistoryTransport> | undefined;
  type Lifecycle = Awaited<
    ReturnType<typeof openHistoryProductionOwnerLifecycle>
  >;
  let h: Awaited<ReturnType<Lifecycle["restartRuntime"]>> | undefined;
  let rollbackPointId: string | undefined;
  let nativeBaseline: string | undefined;
  let expectedNativeRoot: string | undefined;
  let forkObserver:
    | ReturnType<typeof captureConfirmedHistoryObservations>
    | undefined;
  const forkReceipts: AcceptedHistoryObservation[] = [];
  let acceptedTxId: Buffer | undefined;
  let admittedPayload:
    | {
        tx_canonical_cbor: Buffer;
        cek_program_material_sidecar_cbor: Buffer;
      }
    | undefined;
  let descendant:
    | {
        txId: Buffer;
        payload: {
          tx_canonical_cbor: Buffer;
          cek_program_material_sidecar_cbor: Buffer;
        };
        parentOutref: Buffer;
        originalReceiptSequence: string;
      }
    | undefined;
  const descendantState = () =>
    Effect.gen(function* () {
      if (descendant === undefined)
        throw new Error("Missing descendant identity");
      const sql = yield* SqlClient.SqlClient;
      const txId = descendant.txId;
      return {
        admissions: yield* sql<{
          status: string;
        }>`SELECT status FROM tx_admissions WHERE tx_id = ${txId}`,
        payloads: yield* sql<{
          tx_id: Buffer;
          tx_canonical_cbor: Buffer;
          cek_program_material_sidecar_cbor: Buffer | null;
        }>`SELECT tx_id, tx_canonical_cbor, cek_program_material_sidecar_cbor FROM tx_admission_payloads WHERE tx_id = ${txId}`,
        mempool: yield* sql<{
          tx_id: Buffer;
          tx: Buffer;
        }>`SELECT tx_id, tx FROM mempool WHERE tx_id = ${txId}`,
        deltas:
          yield* sql`SELECT tx_id FROM mempool_tx_deltas WHERE tx_id = ${txId}`,
        addressHistory:
          yield* sql`SELECT tx_id FROM address_history WHERE tx_id = ${txId}`,
        receipts: yield* sql<{
          sequence: string;
          tx_ids: Buffer[];
          binding_digest: Buffer;
          owner_generation: string;
          checkpoint_revision: string;
          head_hash: Buffer;
          snapshot_digest: Buffer;
          reversed_at_revision: string | null;
        }>`SELECT r.*, sequence::text, owner_generation::text, checkpoint_revision::text, reversed_at_revision::text
          FROM event_history_l2_ledger_receipts r WHERE ${txId} = ANY(tx_ids) ORDER BY r.sequence`,
      };
    });
  let baselineLedger: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [];
  let funding:
    | { eventId: Buffer; incarnationId: Buffer; originalDeposit: unknown }
    | undefined;
  let referenceInputOutref: Buffer | undefined;
  let oldEventId: Buffer | undefined;
  const prepared = await Effect.runPromise(Deferred.make<void>());
  const diagnostic: Record<string, unknown> = { stage: "startup" };
  try {
    const initial = await openHistoryProductionOwnerLifecycle({
      transportFactory: (recorded) => {
        // Preserve actual historical state-queue UTxOs as well as history-list
        // outputs. These bodies have already been accepted and checked against
        // their emulator outputs by the observation helper; replay only their
        // consumed inputs and produced outputs, never an operator SQL archive.
        // Keep the original recording intact and enrich only this private view.
        const ledger = new Map<
          string,
          AcceptedHistoryObservation["transaction"]["outputs"][number]
        >();
        const batches: typeof recorded.batches = [];
        const ref = (output: { txHash: string; outputIndex: number }) =>
          `${output.txHash}#${output.outputIndex}`;
        const appendObservedBatches = () => {
          for (const batch of recorded.batches.slice(batches.length)) {
            for (const { transaction, signedCbor } of batch.observations) {
              expect(transaction.spends).toBe("inputs");
              const body = CML.Transaction.from_cbor_hex(signedCbor).body();
              expect(CML.hash_transaction(body).to_hex()).toBe(
                transaction.txHash,
              );
              for (const input of transaction.inputs) ledger.delete(ref(input));
              for (const output of transaction.outputs)
                ledger.set(ref(output), structuredClone(output));
            }
            for (const output of batch.outputs) {
              const replayed = ledger.get(ref(output));
              if (replayed !== undefined) expect(replayed).toEqual(output);
              ledger.set(ref(output), structuredClone(output));
            }
            batches.push({
              ...batch,
              outputs: [...ledger.values()].map((output) =>
                structuredClone(output),
              ),
            });
          }
        };
        appendObservedBatches();
        const enriched = makeRollbackHistoryTransport({ ...recorded, batches });
        const appendAccepted = enriched.appendAccepted;
        enriched.appendAccepted = () => {
          appendObservedBatches();
          return appendAccepted();
        };
        transport = enriched;
        return enriched;
      },
      afterNativePreparation: (checkpoint) =>
        Effect.gen(function* () {
          if (checkpoint.head.id !== rollbackPointId) return;
          if (
            h === undefined ||
            acceptedTxId === undefined ||
            admittedPayload === undefined ||
            oldEventId === undefined
          )
            throw new Error("Missing rollback scenario identity");
          const sql = yield* SqlClient.SqlClient;
          const authority = yield* Authority.retrieve;
          expect(Option.isSome(authority) && authority.value.state).toBe(
            "recovering",
          );
          const cache = yield* MempoolLedgerCache;
          const cacheClaim = yield* Effect.either(
            cache.withClaimLock(Effect.void),
          );
          expect(cacheClaim._tag).toBe("Left");
          const producer = yield* Effect.either(
            h.production.owner.runProducer(() => Effect.void),
          );
          expect(producer._tag).toBe("Left");
          const native = yield* Ref.get(h.globals.NATIVE_MPF_OWNER);
          expect(native).toBeDefined();
          expect(
            (yield* Effect.promise(() => native!.diagnostics())).durableRoot,
          ).toBe(nativeBaseline);
          expect(
            yield* sql`SELECT outref FROM mempool_ledger ORDER BY outref`,
          ).toEqual(baselineLedger.map(({ outref }) => ({ outref })));
          if (funding !== undefined) {
            expect(
              yield* sql`SELECT * FROM mempool_ledger ORDER BY outref`,
            ).toEqual(baselineLedger);
            expect(
              yield* sql`SELECT * FROM deposits_utxos WHERE event_id = ${funding.eventId}`,
            ).toEqual([funding.originalDeposit]);
          }
          expect(
            yield* sql`SELECT event_id FROM deposits_utxos WHERE event_id = ${oldEventId}`,
          ).toEqual([]);
          expect(
            yield* sql`SELECT tx_id FROM mempool WHERE tx_id = ${acceptedTxId}`,
          ).toEqual([]);
          expect(
            yield* sql`SELECT tx_id FROM mempool_tx_deltas WHERE tx_id = ${acceptedTxId}`,
          ).toEqual([]);
          expect(
            yield* sql`SELECT tx_id FROM address_history WHERE tx_id = ${acceptedTxId}`,
          ).toEqual([]);
          const admissions = yield* sql<{
            status: string;
          }>`SELECT status FROM tx_admissions WHERE tx_id = ${acceptedTxId}`;
          expect(admissions).toEqual([{ status: "queued" }]);
          const restoredPayloads = yield* sql<{
            tx_id: Buffer;
            tx_canonical_cbor: Buffer;
            cek_program_material_sidecar_cbor: Buffer;
          }>`SELECT tx_id, tx_canonical_cbor, cek_program_material_sidecar_cbor
            FROM tx_admission_payloads WHERE tx_id = ${acceptedTxId}`;
          expect(restoredPayloads).toEqual([
            { tx_id: acceptedTxId, ...admittedPayload },
          ]);
          const receipts = yield* sql<{
            reversed_at_revision: string | null;
          }>`SELECT reversed_at_revision::text AS reversed_at_revision FROM event_history_l2_ledger_receipts WHERE ${acceptedTxId} = ANY(tx_ids)`;
          expect(receipts).toHaveLength(1);
          expect(receipts[0]!.reversed_at_revision).not.toBeNull();
          expect(receipts[0]!.reversed_at_revision).toBe(checkpoint.revision);
          if (descendant !== undefined) {
            const restored = yield* descendantState();
            expect(restored.admissions).toEqual([{ status: "queued" }]);
            expect(restored.payloads).toEqual([
              { tx_id: descendant.txId, ...descendant.payload },
            ]);
            expect(restored.mempool).toEqual([]);
            expect(restored.deltas).toEqual([]);
            expect(restored.addressHistory).toEqual([]);
            expect(restored.receipts).toHaveLength(1);
            expect(restored.receipts[0]!.sequence).toBe(
              descendant.originalReceiptSequence,
            );
            expect(restored.receipts[0]!.reversed_at_revision).toBe(
              checkpoint.revision,
            );
            diagnostic.descendantBeforeReady = {
              checkpointRevision: checkpoint.revision,
              ...restored,
            };
          }
          diagnostic.beforeReady = {
            checkpoint: checkpoint.head,
            checkpointRevision: checkpoint.revision,
            admissions,
            payloads: restoredPayloads,
            authorityState: Option.isSome(authority)
              ? authority.value.state
              : undefined,
            cacheClaimRefused: cacheClaim._tag === "Left",
            producerRefused: producer._tag === "Left",
            receipts,
            native: nativeBaseline,
          };
          yield* Deferred.succeed(prepared, undefined);
        }),
    });
    h = initial;
    const { fixture } = h;
    let { production, globals } = h;
    const source = transport!;
    const wallet = fixture.depositorLucid;
    const address = await wallet.wallet().address();
    const destination = await fixture.operatorLucid.wallet().address();
    let native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    let stoppedNativeMarker: string | undefined;
    expect(native).toBeDefined();
    nativeBaseline = (await native!.diagnostics()).durableRoot;
    expectedNativeRoot = nativeBaseline;
    const addresses = [
      h.binding.hubAddress,
      ...Object.values(h.binding.deployments).flatMap(
        ({ address, retentionAddress }) => [address, retentionAddress],
      ),
    ];
    const stateQueueAddress =
      fixture.contracts.stateQueue.spendingScriptAddress;
    const outputsAt = async (lucid: typeof wallet, includeStateQueue = true) =>
      (
        await Promise.all(
          (includeStateQueue
            ? [...addresses, stateQueueAddress]
            : addresses
          ).map((at) => lucid.utxosAt(at)),
        )
      )
        .flat()
        .map(historyOutputObservation);
    const stateQueueCaptures: {
      point: { id: string; slot: number };
      outputs: ReturnType<typeof historyOutputObservation>[];
    }[] = [];
    const assertStateQueueSnapshot = async (point: {
      id: string;
      slot: number;
    }) => {
      const at = source.points.find((entry) => entry.point.id === point.id);
      expect(at).toBeDefined();
      const byRef = (
        a: { txHash: string; outputIndex: number },
        b: { txHash: string; outputIndex: number },
      ) => a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex;
      const recordedQueue = at!.outputs
        .filter((output) => output.address === stateQueueAddress)
        .sort(byRef);
      const actualQueue = (
        await fixture.operatorLucid.utxosAt(stateQueueAddress)
      )
        .map(historyOutputObservation)
        .sort(byRef);
      expect(recordedQueue).toEqual(actualQueue);
      expect(actualQueue.length).toBeGreaterThan(0);
      stateQueueCaptures.push({
        point: { id: point.id, slot: point.slot },
        outputs: actualQueue,
      });
    };
    diagnostic.stateQueueCaptures = stateQueueCaptures;
    await assertStateQueueSnapshot(source.points.at(-1)!.point);
    const assertReady = async (
      point: { id: string; slot: number },
      expectedEntries?: number,
    ) => {
      const coverage = await Effect.runPromise(
        production.owner.awaitReadyAt(point).pipe(Effect.timeout("30 seconds")),
      );
      const checkpoint = await read(Journal.load(h!.binding));
      expect(checkpoint).not.toBeNull();
      const actual = await Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { id: point.id, slot: point.slot },
            addresses,
            outputs: await outputsAt(fixture.operatorLucid, false),
          },
          h!.binding,
        ),
      );
      expect(checkpoint!.capture.snapshotDigest).toBe(actual.snapshotDigest);
      expect(coverage.snapshotDigest).toBe(actual.snapshotDigest);
      await assertStateQueueSnapshot(point);
      const ledger = await read(
        production.cache.withPhaseBLock(
          Effect.gen(function* () {
            const state = yield* production.cache.currentState;
            const rows = yield* MempoolLedgerDB.retrieveSpendable;
            const cached = [...state]
              .map(([key, value]) => [key, value.toString("hex")])
              .sort();
            const durable = rows
              .map((row) => [
                row.outref.toString("hex"),
                row.output.toString("hex"),
              ])
              .sort();
            expect(cached).toEqual(durable);
            if (expectedEntries !== undefined)
              expect(durable).toHaveLength(expectedEntries);
            return { cached, durable };
          }),
        ),
      );
      native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
      expect(native).toBeDefined();
      expect((await native!.diagnostics()).durableRoot).toBe(
        expectedNativeRoot,
      );
      return { coverage, checkpoint, ledger };
    };

    const alignBeforeAdmission = async (
      runtime: ProductionHistoryFixtureRuntime,
    ) => {
      vi.setSystemTime(fixture.emulator.now());
      // Appoint/refresh the actual scheduler for the short authenticated-source
      // window before admission. The generic commit helper's longer fixture
      // reserve must not place its initial shift after the history horizon.
      await alignCommitSchedulerBeforeTestWorker({
        fixture,
        lucidService: h!.lucidService,
        targetEndTimeMs:
          fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      });
      await runtime.synchronize();
    };

    const commitDeposit = async (
      runtime: ProductionHistoryFixtureRuntime,
      receipts: readonly AcceptedHistoryObservation[],
    ) => {
      const beforeNative = await native!.diagnostics();
      const latestBlock = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const output = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService: h!.lucidService,
        latestBlock,
        nodeConfig: runtime.nodeConfig,
        production: { ...runtime, globals },
      });
      expect(await fixture.operatorLucid.awaitTx(output.submittedTxHash)).toBe(
        true,
      );
      // The submitted commit has already promoted its accepted native root.
      // Derive the next expectation from the actual signed header and retained
      // journal before source synchronization checks native/SQL/cache state.
      const submitted = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            header_hash: Buffer;
            expected_utxos_root: string;
            signed_tx_cbor: Buffer;
          }>`SELECT header_hash, expected_utxos_root, signed_tx_cbor
          FROM pending_block_finalizations
          WHERE intended_tx_hash = ${Buffer.from(output.submittedTxHash, "hex")}`;
        }),
      );
      expect(submitted).toHaveLength(1);
      const submittedJournal = submitted[0]!;
      const submittedBody = CML.Transaction.from_cbor_bytes(
        submittedJournal.signed_tx_cbor,
      ).body();
      expect(CML.hash_transaction(submittedBody).to_hex()).toBe(
        output.submittedTxHash,
      );
      const submittedHeaderHash = submittedJournal.header_hash.toString("hex");
      const submittedUnit =
        fixture.contracts.stateQueue.policyId +
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        submittedHeaderHash;
      const submittedOutputs = Array.from(
        { length: submittedBody.outputs().len() },
        (_, i) => coreToTxOutput(submittedBody.outputs().get(i)),
      ).filter((entry) => entry.assets[submittedUnit] === 1n);
      expect(submittedOutputs).toHaveLength(1);
      const submittedHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(
          SDK.linkedListDatumToNodeView(
            Data.from(submittedOutputs[0]!.datum!, SDK.LinkedListDatum),
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + submittedHeaderHash,
          ),
        ),
      );
      expect(
        await Effect.runPromise(SDK.hashBlockHeader(submittedHeader)),
      ).toBe(submittedHeaderHash);
      expect(submittedHeader.utxosRoot).toBe(
        submittedJournal.expected_utxos_root,
      );
      expectedNativeRoot = submittedHeader.utxosRoot;
      expect((await native!.diagnostics()).durableRoot).toBe(
        expectedNativeRoot,
      );
      await runtime.synchronize();
      await runBlockConfirmation(
        globals,
        fixture.contracts,
        h!.lucidService,
        runtime.nodeConfig,
        runtime,
      );
      const recovered = await runLocalFinalizationRecoveryWorker(
        globals,
        fixture.contracts,
        h!.lucidService,
        fixture.runtimeOverrides!.deploymentIdentity,
        runtime.nodeConfig,
        { ...runtime, globals },
      );
      expect(recovered.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
      if (recovered.type !== "SuccessfulLocalFinalizationRecoveryOutput")
        throw new Error(
          "Deposit commitment did not finalize through the native owner",
        );
      expect(recovered.mempoolTxsCount).toBe(0);
      const queue = await Effect.runPromise(
        SDK.fetchSortedStateQueueUTxOsProgram(
          fixture.operatorLucid,
          stateQueueFetchConfig(fixture.contracts),
        ),
      );
      const queued = queue.at(-1)!;
      const header = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(queued.datum),
      );
      const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
      expect(recovered.finalizedHeaderHash).toBe(headerHash);
      const acceptedReceipts = receipts.filter(
        ({ transaction }) => transaction.txHash === output.submittedTxHash,
      );
      expect(acceptedReceipts).toHaveLength(1);
      const receipt = acceptedReceipts[0]!;
      const body = CML.Transaction.from_cbor_hex(receipt.signedCbor).body();
      expect(CML.hash_transaction(body).to_hex()).toBe(output.submittedTxHash);
      const ttl = body.ttl();
      if (ttl === undefined)
        throw new Error("Signed commit lacks its bounded TTL");
      expect(fixture.operatorLucid.slotToUnixTime(Number(ttl)) - 1).toBe(
        Number(header.endTime),
      );
      const unit =
        fixture.contracts.stateQueue.policyId +
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        headerHash;
      const outputs = Array.from({ length: body.outputs().len() }, (_, i) =>
        coreToTxOutput(body.outputs().get(i)),
      ).filter((entry) => entry.assets[unit] === 1n);
      expect(outputs).toHaveLength(1);
      const signedHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(
          SDK.linkedListDatumToNodeView(
            Data.from(outputs[0]!.datum!, SDK.LinkedListDatum),
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
          ),
        ),
      );
      expect(signedHeader).toEqual(header);
      const durable = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return {
            journal: yield* sql<{
              header_hash: Buffer;
              signed_tx_cbor: Buffer;
              status: string;
              base_utxos_root: string;
              expected_utxos_root: string;
              intended_tx_hash: Buffer;
              submitted_tx_hash: Buffer;
            }>`SELECT * FROM pending_block_finalizations WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
            members:
              yield* sql`SELECT * FROM pending_block_finalization_deposits WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
            deposit: yield* sql<{
              projected_header_hash: Buffer;
            }>`SELECT projected_header_hash FROM deposits_utxos WHERE event_id = ${oldEventId!}`,
          };
        }),
      );
      expect(durable.journal).toHaveLength(1);
      const journal = durable.journal[0]!;
      expect(journal.status).toBe("finalized");
      expect(journal.signed_tx_cbor.toString("hex")).toBe(receipt.signedCbor);
      expect(journal.intended_tx_hash.toString("hex")).toBe(
        output.submittedTxHash,
      );
      expect(journal.submitted_tx_hash.toString("hex")).toBe(
        output.submittedTxHash,
      );
      expect(journal.base_utxos_root).toBe(beforeNative.durableRoot);
      expect(durable.members).toHaveLength(1);
      expect(durable.deposit).toEqual([
        { projected_header_hash: Buffer.from(headerHash, "hex") },
      ]);
      const afterNative = await native!.diagnostics();
      expect(afterNative.durableRoot).toBe(journal.expected_utxos_root);
      expect(afterNative.durableRoot).not.toBe(beforeNative.durableRoot);
      expectedNativeRoot = afterNative.durableRoot;
      return {
        output,
        recovered,
        header,
        headerHash,
        receipt,
        signedTtl: ttl.toString(),
        durable,
        beforeNative,
        afterNative,
      };
    };

    diagnostic.stage = "original-deposit";
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignBeforeAdmission(production);
    const depositConfig = {
      l2Address: address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
      referenceScripts: fixture.referenceScripts.deposit,
    };
    let stableJournal: unknown;
    if (dependency === "reference") {
      diagnostic.stage = "canonical-reference-funding-deposit-commit-and-merge";
      const stable = await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          wallet,
          fixture.contracts,
          depositConfig,
        ),
      );
      const signedStable = await stable.tx.sign.withWallet().complete();
      const stableHash = await signedStable.submit();
      expect(await wallet.awaitTx(stableHash)).toBe(true);
      await h.deployment.chain.awaitLedgerTime(
        stable.metadata.inclusionTime + 1000,
      );
      vi.setSystemTime(fixture.emulator.now());
      await h.synchronize();
      oldEventId = Buffer.from(stable.metadata.depositEventId, "hex");
      await assertReady(source.points.at(-1)!.point, 0);
      const stableCommit = await commitDeposit(production, h.receipts);
      stableJournal = stableCommit.durable.journal[0];
      await assertReady(source.points.at(-1)!.point, 1);
      await attestQueuedStateQueueHeader({
        fixture,
        lucidService: h.lucidService,
        globals,
        headerHash: stableCommit.headerHash,
      });
      await advanceEmulatorPastUnixTime(
        fixture,
        mergeMaturityWindow(
          fixture.operatorLucid,
          Number(stableCommit.header.endTime),
        ).readyAfterUnixTime,
      );
      vi.setSystemTime(fixture.emulator.now());
      const merged = await runMergeUntilMerged({
        fixture,
        lucidService: h.lucidService,
        globals,
        production,
      });
      expect(merged.postMergeSnapshot.topology.parsedNodeCount).toBe(1);
      const stableQueue = await Effect.runPromise(
        SDK.fetchSortedStateQueueUTxOsProgram(
          fixture.operatorLucid,
          stateQueueFetchConfig(fixture.contracts),
        ),
      );
      expect(stableQueue).toHaveLength(1);
      expect(stableQueue[0]!.assetName).toBe(SDK.STATE_QUEUE_ROOT_ASSET_NAME);
      const stableConfirmed = await Effect.runPromise(
        SDK.getConfirmedStateFromStateQueueDatum(stableQueue[0]!.datum),
      );
      expect(stableConfirmed.data.utxoRoot).toBe(
        stableCommit.afterNative.durableRoot,
      );
      nativeBaseline = stableCommit.afterNative.durableRoot;
      expectedNativeRoot = nativeBaseline;
      baselineLedger = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<MempoolLedgerDB.EntryWithTimeStamp>`SELECT * FROM mempool_ledger ORDER BY outref`;
        }),
      );
      expect(baselineLedger).toHaveLength(1);
      expect(baselineLedger[0]!.source_event_id).toEqual(oldEventId);
      const stableDeposit = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            history_incarnation_id: Buffer;
          }>`SELECT * FROM deposits_utxos WHERE event_id = ${oldEventId!}`;
        }),
      );
      expect(stableDeposit).toHaveLength(1);
      funding = {
        eventId: oldEventId,
        incarnationId: stableDeposit[0]!.history_incarnation_id,
        originalDeposit: stableDeposit[0],
      };
      diagnostic.referenceFunding = {
        stableHash,
        stableCommit,
        merged,
        stableConfirmed,
        funding,
        baselineLedger,
      };
      await assertReady(source.points.at(-1)!.point, 1);
      await advanceHistoryAdmissionClock(fixture, "deposit");
      await alignBeforeAdmission(production);
      diagnostic.stage = "original-deposit";
    }
    const ancestor = source.points.at(-1)!.point;
    const beforeDeposit = snapshotEmulator(fixture.emulator);
    expect(Object.keys(beforeDeposit.mempool)).toHaveLength(0);
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(
        wallet,
        fixture.contracts,
        depositConfig,
      ),
    );
    const signed = await built.tx.sign.withWallet().complete();
    const firstHash = await signed.submit();
    expect(await wallet.awaitTx(firstHash)).toBe(true);
    await h.deployment.chain.awaitLedgerTime(
      built.metadata.inclusionTime + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        fixture.operatorLucid,
        SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).deposit,
        ),
      ),
    );
    expect(deposits).toHaveLength(funding === undefined ? 1 : 2);
    const orphanDeposits = deposits.filter(
      (deposit) =>
        deposit.idCbor.toString("hex") === built.metadata.depositEventId,
    );
    expect(orphanDeposits).toHaveLength(1);
    oldEventId = orphanDeposits[0]!.idCbor;
    expect(oldEventId.toString("hex")).toBe(built.metadata.depositEventId);
    const originalRows = await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          history_incarnation_id: Buffer;
        }>`SELECT history_incarnation_id FROM deposits_utxos WHERE event_id = ${oldEventId!}`;
      }),
    );
    expect(originalRows).toHaveLength(1);
    const oldIncarnation = originalRows[0]!.history_incarnation_id;
    await assertReady(source.points.at(-1)!.point, baselineLedger.length);
    diagnostic.stage = "original-deposit-commit-confirm-native-finalize";
    const originalCommit = await commitDeposit(production, h.receipts);
    diagnostic.originalCommit = {
      ...originalCommit,
      ancestor,
      ancestorNativeRoot: nativeBaseline,
    };
    await assertReady(source.points.at(-1)!.point, baselineLedger.length + 1);
    expect(
      h.receipts.filter(({ transaction }) => transaction.txHash === firstHash),
    ).toHaveLength(1);

    diagnostic.stage = "signed-l2-admission-and-real-queue-drain";
    const availableUtxos = await h.command(fetchLocalUtxos(address));
    expect(availableUtxos).toHaveLength(baselineLedger.length + 1);
    const buildReferenceTransfer = async (): Promise<BuiltTransferTx> => {
      if (funding === undefined)
        throw new Error("Missing canonical reference funding");
      const stableInput = availableUtxos.filter((utxo) =>
        utxo.outrefCbor.equals(baselineLedger[0]!.outref),
      );
      const orphanInput = availableUtxos.filter(
        (utxo) => !utxo.outrefCbor.equals(baselineLedger[0]!.outref),
      );
      expect(stableInput).toHaveLength(1);
      expect(orphanInput).toHaveLength(1);
      referenceInputOutref = Buffer.from(orphanInput[0]!.outrefCbor);
      const orphanRow = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            outref: Buffer;
          }>`SELECT outref FROM mempool_ledger WHERE source_event_id = ${oldEventId!}`;
        }),
      );
      expect(orphanRow).toEqual([{ outref: referenceInputOutref }]);
      const signer = walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Preprod",
      }).paymentKey;
      const midgard = await makeTransferMidgard({
        senderAddress: address,
        signer,
        utxos: availableUtxos,
        network: "Preprod",
        networkId: 0n,
        minFeeA: production.nodeConfig.MIN_FEE_A,
        minFeeB: production.nodeConfig.MIN_FEE_B,
        consensusProfile:
          fixture.runtimeOverrides!.deploymentIdentity.consensusProfile,
      });
      const decode = (utxo: (typeof availableUtxos)[number]) =>
        decodeMidgardUtxo({
          outRef: { txHash: utxo.txHash, outputIndex: utxo.outputIndex },
          outRefCbor: utxo.outrefCbor,
          outputCbor: utxo.outputCbor,
        });
      const completed = await midgard
        .newTx()
        .collectFrom(stableInput.map(decode))
        .readFrom(orphanInput.map(decode))
        .addSigner(
          CML.PrivateKey.from_bech32(signer).to_public().hash().to_hex(),
        )
        .pay.ToAddress(destination, { lovelace: 4_000_000n })
        .complete({ changeAddress: address, feePolicy: "provider" });
      const signedReference = await completed.sign();
      const decoded = decodeMidgardSubmittedTxFromCanonicalCbor(
        signedReference.txCbor,
      );
      expect(
        decoded.ledgerTx.spendInputs.map((ref) =>
          Buffer.from(midgardOutRefToCbor(ref)),
        ),
      ).toEqual([baselineLedger[0]!.outref]);
      expect(
        decoded.ledgerTx.referenceInputs.map((ref) =>
          Buffer.from(midgardOutRefToCbor(ref)),
        ),
      ).toEqual([referenceInputOutref]);
      expect(Buffer.from(decoded.ledgerTx.txId)).toEqual(signedReference.txId);
      return {
        txId: signedReference.txId,
        txIdHex: signedReference.txId.toString("hex"),
        txCbor: signedReference.txCbor,
        txHex: signedReference.txCbor.toString("hex"),
        fee: signedReference.metadata.fee,
        senderAddress: address,
        destinationAddress: destination,
        selectedInputs: stableInput,
        requestedAssets: { lovelace: 4_000_000n },
        changeAssets: signedReference.metadata.changeAssets ?? {},
      };
    };
    const transfer = await (dependency === "reference"
      ? buildReferenceTransfer()
      : buildTransferTxWithMinFee({
          senderAddress: address,
          destinationAddress: destination,
          signer: walletFromSeed(fixture.depositorAccount.seedPhrase, {
            network: "Preprod",
          }).paymentKey,
          availableUtxos,
          requestedAssets: { lovelace: 4_000_000n },
          network: "Preprod",
          networkId: 0n,
          minFeeA: production.nodeConfig.MIN_FEE_A,
          minFeeB: production.nodeConfig.MIN_FEE_B,
          consensusProfile:
            fixture.runtimeOverrides!.deploymentIdentity.consensusProfile,
        }));
    acceptedTxId = transfer.txId;
    const queued = toQueuedTx(transfer);
    const sidecar = queued.programMaterialSidecarCbor;
    if (sidecar === null || sidecar === undefined)
      throw new Error(
        "Generated signed transfer requires its original CEK sidecar",
      );
    const programMaterialSidecarCbor = Buffer.from(sidecar);
    admittedPayload = {
      tx_canonical_cbor: Buffer.from(queued.txCbor),
      cek_program_material_sidecar_cbor: Buffer.from(
        programMaterialSidecarCbor,
      ),
    };
    await h.command(
      TxAdmissionsDB.admit({
        txId: queued.txId,
        txCanonicalCbor: queued.txCbor,
        programMaterialSidecarCbor,
        submitSource: "native",
        currentBacklog: 0n,
        maxBacklog: production.nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
      }),
    );
    await h.command(
      txQueueProcessorDrainOnce().pipe(Effect.provide(validationPoolLayer)),
    );
    const accepted = await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return {
          admission: yield* sql<{
            status: string;
          }>`SELECT status FROM tx_admissions WHERE tx_id = ${transfer.txId}`,
          mempool: yield* sql<{
            tx: Buffer;
          }>`SELECT tx FROM mempool WHERE tx_id = ${transfer.txId}`,
          receipt:
            yield* sql`SELECT * FROM event_history_l2_ledger_receipts WHERE ${transfer.txId} = ANY(tx_ids)`,
          pending: yield* sql`SELECT * FROM pending_block_finalizations`,
        };
      }),
    );
    expect(accepted.admission).toEqual([{ status: "accepted" }]);
    expect(accepted.mempool).toHaveLength(1);
    expect(accepted.mempool[0]!.tx).toEqual(transfer.txCbor);
    expect(accepted.receipt).toHaveLength(1);
    expect(accepted.receipt[0]!.reversed_at_revision).toBeNull();
    if (funding === undefined) {
      expect(accepted.pending).toHaveLength(1);
      expect(accepted.pending[0]).toEqual(originalCommit.durable.journal[0]);
    } else {
      expect(accepted.pending).toHaveLength(2);
      expect(accepted.pending).toContainEqual(
        originalCommit.durable.journal[0],
      );
      expect(accepted.pending).toContainEqual(stableJournal);
      const referenced = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return {
            references: yield* sql<{
              reference_outrefs: Buffer[];
            }>`SELECT reference_outrefs FROM event_history_l2_ledger_receipts WHERE ${transfer.txId} = ANY(tx_ids)`,
            before:
              yield* sql<MempoolLedgerDB.EntryWithTimeStamp>`SELECT l.* FROM event_history_l2_ledger_receipts r,
            LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.reference_before) l
            WHERE ${transfer.txId} = ANY(r.tx_ids)`,
            retained:
              yield* sql<MempoolLedgerDB.EntryWithTimeStamp>`SELECT * FROM mempool_ledger WHERE outref = ${referenceInputOutref!}`,
          };
        }),
      );
      expect(referenced.references).toEqual([
        { reference_outrefs: [referenceInputOutref] },
      ]);
      expect(referenced.before).toHaveLength(1);
      expect(referenced.before[0]!.source_event_id).toEqual(oldEventId);
      expect(referenced.retained).toEqual(referenced.before);
      diagnostic.referenceAccepted = referenced;
    }
    let writeBehind = await h.command(WriteBehind);
    expect(
      (await Effect.runPromise(writeBehind.depths)).totalDepth,
    ).toBeGreaterThan(0);
    diagnostic.accepted = { ...accepted, transfer, oldIncarnation, ancestor };
    await assertReady(
      source.points.at(-1)!.point,
      funding === undefined ? 2 : 3,
    );

    if (dependency === "descendant") {
      diagnostic.stage = "signed-descendant-admission-and-real-queue-drain";
      const parentOutputs = (
        await h.command(fetchLocalUtxos(destination))
      ).filter((utxo) => utxo.txHash === transfer.txIdHex);
      expect(parentOutputs).toHaveLength(1);
      expect(parentOutputs[0]!.assets).toEqual({ lovelace: 4_000_000n });
      const child = await buildTransferTxWithMinFee({
        senderAddress: destination,
        destinationAddress: address,
        signer: walletFromSeed(fixture.operatorAccount.seedPhrase, {
          network: "Preprod",
        }).paymentKey,
        availableUtxos: parentOutputs,
        requestedAssets: { lovelace: 1_000_000n },
        network: "Preprod",
        networkId: 0n,
        minFeeA: production.nodeConfig.MIN_FEE_A,
        minFeeB: production.nodeConfig.MIN_FEE_B,
        consensusProfile:
          fixture.runtimeOverrides!.deploymentIdentity.consensusProfile,
      });
      // selectedInputs is decoded from the completed, signed canonical body.
      expect(child.selectedInputs).toEqual(parentOutputs);
      const childQueued = toQueuedTx(child);
      const childSidecar = childQueued.programMaterialSidecarCbor;
      if (childSidecar === null || childSidecar === undefined)
        throw new Error(
          "Generated signed descendant requires its original CEK sidecar",
        );
      descendant = {
        txId: Buffer.from(childQueued.txId),
        payload: {
          tx_canonical_cbor: Buffer.from(childQueued.txCbor),
          cek_program_material_sidecar_cbor: Buffer.from(childSidecar),
        },
        parentOutref: Buffer.from(parentOutputs[0]!.outrefCbor),
        originalReceiptSequence: "",
      };
      await h.command(
        TxAdmissionsDB.admit({
          txId: descendant.txId,
          txCanonicalCbor: descendant.payload.tx_canonical_cbor,
          programMaterialSidecarCbor:
            descendant.payload.cek_program_material_sidecar_cbor,
          submitSource: "native",
          currentBacklog: 0n,
          maxBacklog: production.nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
        }),
      );
      await h.command(
        txQueueProcessorDrainOnce().pipe(Effect.provide(validationPoolLayer)),
      );
      const childAccepted = await read(descendantState());
      expect(childAccepted.admissions).toEqual([{ status: "accepted" }]);
      expect(childAccepted.mempool).toEqual([
        { tx_id: descendant.txId, tx: descendant.payload.tx_canonical_cbor },
      ]);
      expect(childAccepted.receipts).toHaveLength(1);
      const receipt = childAccepted.receipts[0]!;
      expect(receipt.reversed_at_revision).toBeNull();
      expect(receipt.tx_ids).toEqual([descendant.txId]);
      expect(BigInt(receipt.sequence)).toBeGreaterThan(
        BigInt(String(accepted.receipt[0]!.sequence)),
      );
      descendant.originalReceiptSequence = receipt.sequence;
      const retainedParent = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            outref: Buffer;
            tx_id: Buffer;
            source_event_id: Buffer | null;
          }>`SELECT l.outref, l.tx_id, l.source_event_id
          FROM event_history_l2_ledger_receipts r,
            LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_before) l
          WHERE r.sequence = ${receipt.sequence}`;
        }),
      );
      expect(retainedParent).toEqual([
        {
          outref: descendant.parentOutref,
          tx_id: transfer.txId,
          source_event_id: null,
        },
      ]);
      diagnostic.descendantAccepted = {
        ...childAccepted,
        transfer: child,
        retainedParent,
      };
      await assertReady(source.points.at(-1)!.point, 3);
    }

    diagnostic.stage = "source-rollback-awaiting-signed-intent-disposition";
    h.observer.restore();
    restoreEmulator(fixture.emulator, beforeDeposit);
    vi.setSystemTime(fixture.emulator.now());
    source.rollbackTo(ancestor.id);
    const waitForJournal = async (point: { id: string; slot: number }) =>
      read(
        Effect.gen(function* () {
          const checkpoint = yield* Journal.load(h!.binding);
          if (checkpoint?.head.id !== point.id)
            return yield* Effect.fail(
              new Error(
                "History source has not retained the requested frontier",
              ),
            );
          return checkpoint;
        }).pipe(
          Effect.retry(Schedule.spaced("10 millis")),
          Effect.timeout("30 seconds"),
        ),
      );
    await waitForJournal(ancestor);
    const assertPending = async () => {
      const authority = await read(Authority.retrieve);
      expect(Option.isSome(authority) && authority.value.state).toBe(
        "recovering",
      );
      const status = await Effect.runPromise(
        production.owner.reconciliationStatus,
      );
      expect(status?.status).toBe("pending");
      await expect(h!.runWithoutSynchronizing(Effect.void)).rejects.toThrow(
        "History source gate is closed",
      );
      expect(
        (
          await Effect.runPromise(
            Effect.either(production.cache.withClaimLock(Effect.void)),
          )
        )._tag,
      ).toBe("Left");
      // A still-valid orphaned signed commitment must not release its durable
      // accepted root or dependent L2 overlay just because its event vanished.
      const activeNative = await Effect.runPromise(
        Ref.get(globals.NATIVE_MPF_OWNER),
      );
      const heldNative =
        activeNative === undefined
          ? {
              durableRoot: stoppedNativeMarker,
              scope:
                "Retained Level root read after full service close; restarted native owner remains unopened while pending",
            }
          : await activeNative.diagnostics();
      if (activeNative === undefined) {
        expect(restartWhilePending).toBe(true);
        expect(stoppedNativeMarker).toBeDefined();
      }
      expect(heldNative.durableRoot).toBe(
        originalCommit.afterNative.durableRoot,
      );
      const retained = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return {
            admission:
              yield* sql`SELECT status FROM tx_admissions WHERE tx_id = ${transfer.txId}`,
            receipt:
              yield* sql`SELECT reversed_at_revision FROM event_history_l2_ledger_receipts WHERE ${transfer.txId} = ANY(tx_ids)`,
            journal:
              yield* sql`SELECT * FROM pending_block_finalizations WHERE header_hash = ${Buffer.from(originalCommit.headerHash, "hex")}`,
          };
        }),
      );
      expect(retained.admission).toEqual([{ status: "accepted" }]);
      expect(retained.receipt).toEqual([{ reversed_at_revision: null }]);
      expect(retained.journal).toEqual(originalCommit.durable.journal);
      return { authority, status, retained, native: heldNative };
    };
    diagnostic.pendingRollback = { ancestor, ...(await assertPending()) };
    if (restartWhilePending) {
      diagnostic.stage = "close-and-reopen-services-at-pending-rollback";
      const prior = h;
      const priorNative = native!;
      const priorGlobals = globals;
      const beforeRestartAuthority = await read(Authority.retrieve);
      expect(Option.isSome(beforeRestartAuthority)).toBe(true);
      if (Option.isNone(beforeRestartAuthority))
        throw new Error("Missing pre-restart authority");
      const retainedState = () =>
        read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const headerHash = Buffer.from(originalCommit.headerHash, "hex");
            return {
              journal:
                yield* sql`SELECT * FROM pending_block_finalizations WHERE header_hash = ${headerHash}`,
              members:
                yield* sql`SELECT * FROM pending_block_finalization_deposits WHERE header_hash = ${headerHash} ORDER BY member_id`,
              receipts:
                yield* sql`SELECT * FROM event_history_l2_ledger_receipts WHERE ${transfer.txId} = ANY(tx_ids) ORDER BY sequence`,
            };
          }),
        );
      const beforeRestart = await retainedState();
      expect(beforeRestart.journal).toEqual(originalCommit.durable.journal);
      expect(beforeRestart.members).toEqual(originalCommit.durable.members);
      expect(beforeRestart.receipts).toEqual(accepted.receipt);
      let stoppedState: Awaited<ReturnType<typeof retainedState>> | undefined;
      const levelPath = production.nodeConfig.LEDGER_MPF_DB_PATH;
      h = await initial.restartRuntime({
        synchronize: false,
        afterStop: async () => {
          // Full owner/native/runtime closure precedes this observational read.
          // Do not initialize, put, replay, repair or rewrite any retained root.
          await expect(priorNative.diagnostics()).rejects.toThrow();
          await expect(
            prior.runWithoutSynchronizing(Effect.void),
          ).rejects.toThrow("runtime generation is closed");
          const retained = new Level<string, unknown>(levelPath, {
            valueEncoding: "json",
            createIfMissing: false,
          });
          await retained.open();
          try {
            const marker = await retained.get("__root__");
            expect(marker).toBe(originalCommit.afterNative.durableRoot);
            if (typeof marker !== "string")
              throw new Error("Retained native root is not a string");
            stoppedNativeMarker = marker;
          } finally {
            await retained.close();
          }
          stoppedState = await retainedState();
          expect(stoppedState).toEqual(beforeRestart);
        },
      });
      ({ production, globals } = h);
      expect(globals).not.toBe(priorGlobals);
      expect(production.owner).not.toBe(prior.production.owner);
      expect(production.cache).not.toBe(prior.production.cache);
      expect(production.nodeConfig.LEDGER_MPF_DB_PATH).toBe(levelPath);
      native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
      expect(native).toBeUndefined();
      // Restart retains the pending head and follows this fork; it must not
      // append the sealed original recording or await Ready before returning.
      await Effect.runPromise(
        production.owner.reconciliationStatus.pipe(
          Effect.flatMap((status) =>
            status?.status === "pending"
              ? Effect.void
              : Effect.fail(
                  new Error("Restart has not retained pending reconciliation"),
                ),
          ),
          Effect.retry(Schedule.spaced("10 millis")),
          Effect.timeout("30 seconds"),
          Effect.raceFirst(
            production.owner.awaitReady.pipe(
              Effect.flatMap(() =>
                Effect.fail(
                  new Error(
                    "Restart published Ready before signed-intent disposition",
                  ),
                ),
              ),
            ),
          ),
        ),
      );
      const afterRestartAuthority = await read(Authority.retrieve);
      expect(Option.isSome(afterRestartAuthority)).toBe(true);
      if (Option.isNone(afterRestartAuthority))
        throw new Error("Missing restarted authority");
      expect(BigInt(afterRestartAuthority.value.generation)).toBeGreaterThan(
        BigInt(beforeRestartAuthority.value.generation),
      );
      const afterRestart = await retainedState();
      expect(afterRestart).toEqual(beforeRestart);
      const pending = await assertPending();
      diagnostic.pendingRestart = {
        scope:
          "Full production owner, Globals, cache, managed runtime and native child service close/reopen with retained SQL/Level; same test process and emulator, not an OS-process crash",
        levelPath,
        beforeAuthority: beforeRestartAuthority.value,
        afterAuthority: afterRestartAuthority.value,
        before: beforeRestart,
        stopped: stoppedState,
        after: afterRestart,
        retainedNativeRoot: stoppedNativeMarker,
        pending,
      };
    }

    let forkObservedHeight = fixture.emulator.blockHeight;
    const appendForkBatch = (
      batch: Parameters<typeof source.appendFork>[0],
    ) => {
      const point = source.appendFork(batch);
      forkObservedHeight = batch.observedHeight;
      return point;
    };
    const forkBatches: Parameters<typeof source.appendFork>[0][] = [];
    forkObserver = captureConfirmedHistoryObservations(
      fixture.operatorLucid,
      fixture.emulator,
      async (observations) => {
        forkReceipts.push(...observations);
        forkBatches.push({
          observations,
          observedSlot: fixture.emulator.slot,
          observedHeight: fixture.emulator.blockHeight,
          outputs: await outputsAt(fixture.operatorLucid),
        });
      },
    );
    type CommitAttempt = Parameters<
      NonNullable<ProductionHistoryFixtureRuntime["onCommitAttempt"]>
    >[0];
    const forkAttempts: CommitAttempt[] = [];
    const forkCoverage = new Map<string, CommitAttempt["coverage"]>();
    const forkSynchronize = async () => {
      await forkObserver!.flush();
      expect(forkObserver!.pendingCount()).toBe(0);
      expect(Object.keys(fixture.emulator.mempool)).toHaveLength(0);
      for (const batch of forkBatches.splice(0)) appendForkBatch(batch);
      if (fixture.emulator.slot > source.points.at(-1)!.point.slot) {
        if (fixture.emulator.blockHeight <= forkObservedHeight)
          fixture.emulator.awaitBlock(1);
        appendForkBatch({
          observations: [],
          observedSlot: fixture.emulator.slot,
          observedHeight: fixture.emulator.blockHeight,
          outputs: await outputsAt(fixture.operatorLucid),
        });
      }
      vi.setSystemTime(fixture.emulator.now());
      const ready = await assertReady(source.points.at(-1)!.point);
      forkCoverage.set(
        ready.coverage.checkpointRevision,
        structuredClone(ready.coverage),
      );
    };
    const forkProduction: ProductionHistoryFixtureRuntime = {
      ...production,
      synchronize: forkSynchronize,
      onCommitAttempt: (attempt) => {
        expect(forkCoverage.get(attempt.coverage.checkpointRevision)).toEqual(
          attempt.coverage,
        );
        expect(attempt.coverage.includedThroughMs).toBe(
          fixture.operatorLucid.slotToUnixTime(attempt.coverage.point.slot),
        );
        forkAttempts.push(structuredClone(attempt));
      },
    };
    const appendObservedInterval = async (expectRepair = false) => {
      const outputs = await outputsAt(fixture.operatorLucid);
      const point = appendForkBatch({
        observations: [],
        observedSlot: fixture.emulator.slot,
        observedHeight: fixture.emulator.blockHeight,
        outputs,
      });
      if (expectRepair) rollbackPointId = point.id;
      await assertStateQueueSnapshot(point);
      return point;
    };
    const signedTtl = Number(originalCommit.signedTtl);
    expect(Number.isSafeInteger(signedTtl)).toBe(true);
    if (fixture.emulator.slot <= signedTtl)
      fixture.emulator.awaitSlot(signedTtl + 1 - fixture.emulator.slot);
    fixture.emulator.awaitBlock(1);
    const firstExpiredPoint = await appendObservedInterval();
    await waitForJournal(firstExpiredPoint);
    diagnostic.expiredButUnfinalized = {
      point: firstExpiredPoint,
      ...(await assertPending()),
    };
    const finalityDepth = h.deployment.manifest.l1Finality.confirmationDepth;
    let dispositionPoint = firstExpiredPoint;
    for (let depth = 0; depth < finalityDepth; depth += 1) {
      fixture.emulator.awaitBlock(1);
      const last = depth === finalityDepth - 1;
      dispositionPoint = await appendObservedInterval(last);
      if (!last) {
        await waitForJournal(dispositionPoint);
        await assertPending();
      }
    }
    expect(dispositionPoint.slot).toBeGreaterThan(signedTtl);
    expect(dispositionPoint.height - firstExpiredPoint.height).toBe(
      finalityDepth,
    );
    diagnostic.dispositionCoverage = {
      firstExpiredPoint,
      dispositionPoint,
      signedTtl: originalCommit.signedTtl,
      finalityDepth,
    };
    // No fabricated release envelope or manual SQL repair: the production
    // owner must derive disposition from this authenticated fork and signed TTL.
    // Until that consumer exists, this positive assertion intentionally fails.
    await Effect.runPromise(
      Deferred.await(prepared).pipe(
        Effect.raceFirst(
          production.owner
            .awaitReadyAt(dispositionPoint)
            .pipe(Effect.flatMap(() => Effect.never)),
        ),
        Effect.timeout("30 seconds"),
      ),
    );
    expectedNativeRoot = nativeBaseline;
    diagnostic.rollback = await assertReady(
      dispositionPoint,
      baselineLedger.length,
    );
    if (restartWhilePending) {
      writeBehind = await h.runWithoutSynchronizing(WriteBehind);
      const recoveredPlans = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            intent: string;
            state: string;
          }>`SELECT intent, state FROM event_history_recovery_plans
          WHERE header_hash = ${Buffer.from(originalCommit.headerHash, "hex")}`;
        }),
      );
      expect(recoveredPlans).toHaveLength(1);
      expect(recoveredPlans[0]!.state).toBe("applied");
      expect(JSON.parse(recoveredPlans[0]!.intent)).toMatchObject({
        expectedRoot: stoppedNativeMarker,
        targetRoot: nativeBaseline,
        signedTransactionHash: originalCommit.output.submittedTxHash,
      });
      diagnostic.restartRecovered = {
        plans: recoveredPlans,
        native: await native!.diagnostics(),
      };
    }
    const restoredQueue = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    expect(restoredQueue).toHaveLength(1);
    const confirmedBase = restoredQueue[0]!;
    expect(confirmedBase.assetName).toBe(SDK.STATE_QUEUE_ROOT_ASSET_NAME);
    const confirmedState = await Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(confirmedBase.datum),
    );
    expect(confirmedState.data.utxoRoot).toBe(nativeBaseline);
    const restoredBoundary = Number(confirmedState.data.endTime);
    expect(Number.isSafeInteger(restoredBoundary)).toBe(true);
    const serializedConfirmedBase = await Effect.runPromise(
      serializeStateQueueUTxO(confirmedBase),
    );
    const recoveredGlobals = await Effect.runPromise(
      Effect.all({
        LATEST_LOCAL_BLOCK_END_TIME_MS: Ref.get(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
        ),
        AVAILABLE_CONFIRMED_BLOCK: Ref.get(globals.AVAILABLE_CONFIRMED_BLOCK),
        UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
        ),
        UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
        ),
        LOCAL_FINALIZATION_PENDING: Ref.get(globals.LOCAL_FINALIZATION_PENDING),
        AVAILABLE_LOCAL_FINALIZATION_BLOCK: Ref.get(
          globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
        ),
        BLOCKS_IN_QUEUE: Ref.get(globals.BLOCKS_IN_QUEUE),
      }),
    );
    expect(recoveredGlobals).toEqual({
      LATEST_LOCAL_BLOCK_END_TIME_MS: restoredBoundary,
      AVAILABLE_CONFIRMED_BLOCK: serializedConfirmedBase,
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: "",
      UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: 0,
      LOCAL_FINALIZATION_PENDING: false,
      AVAILABLE_LOCAL_FINALIZATION_BLOCK: "",
      BLOCKS_IN_QUEUE: 0,
    });
    diagnostic.recoveredGlobals = {
      point: dispositionPoint,
      confirmedBase,
      confirmedState,
      globals: recoveredGlobals,
    };
    expect((await Effect.runPromise(writeBehind.depths)).totalDepth).toBe(0);
    await Effect.runPromise(writeBehind.flushNow);
    await assertReady(dispositionPoint, baselineLedger.length);
    expect(
      await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql`SELECT tx_id FROM address_history WHERE tx_id = ${transfer.txId}`;
        }),
      ),
    ).toEqual([]);

    diagnostic.stage = "actual-fork-reinclusion";
    await alignBeforeAdmission(forkProduction);
    const fork = new Emulator(
      [],
      beforeDeposit.protocolParameters,
      beforeDeposit.treasury,
    );
    restoreEmulator(fork, snapshotEmulator(fixture.emulator));
    fork.awaitSlot(120);
    vi.setSystemTime(fork.now());
    const forkWallet = await createMainnetEmulatorLucid(fork, "Preprod");
    forkWallet.selectWallet.fromSeed(fixture.depositorAccount.seedPhrase);
    const replacement = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(
        forkWallet,
        fixture.contracts,
        {
          ...depositConfig,
          nonceInput: built.metadata.nonceInput,
        },
      ),
    );
    expect(replacement.metadata.depositEventId).toBe(
      built.metadata.depositEventId,
    );
    const forkReceipt = await submitHistoryObservation(
      forkWallet,
      replacement.tx,
    );
    forkReceipts.push(forkReceipt);
    const limits = h.deployment.manifest.cardanoProtocolParameters.snapshot;
    expect(forkReceipt.measurement.completeSignedBytes).toBeLessThanOrEqual(
      Number(limits.maxTxSize),
    );
    expect(forkReceipt.measurement.executionMemory).toBeLessThanOrEqual(
      BigInt(limits.maxTxExUnits.memory),
    );
    expect(forkReceipt.measurement.executionSteps).toBeLessThanOrEqual(
      BigInt(limits.maxTxExUnits.steps),
    );
    expect(forkReceipt.transaction.txHash).not.toBe(firstHash);
    const forkBatch = {
      observations: [forkReceipt],
      observedSlot: fork.slot,
      observedHeight: fork.blockHeight,
      outputs: await outputsAt(forkWallet),
    };
    restoreEmulator(fixture.emulator, snapshotEmulator(fork));
    vi.setSystemTime(fixture.emulator.now());
    const forkPoint = appendForkBatch(forkBatch);
    await Effect.runPromise(
      production.owner
        .awaitReadyAt(forkPoint)
        .pipe(Effect.timeout("30 seconds")),
    );
    const slots = Math.ceil(
      (replacement.metadata.inclusionTime + 1000 - fork.now()) / 1000,
    );
    if (slots > 0) fork.awaitSlot(slots);
    fork.awaitBlock(1);
    restoreEmulator(fixture.emulator, snapshotEmulator(fork));
    vi.setSystemTime(fixture.emulator.now());
    const eligiblePoint = appendForkBatch({
      observations: [],
      observedSlot: fork.slot,
      observedHeight: fork.blockHeight,
      outputs: await outputsAt(forkWallet),
    });
    diagnostic.reincluded = await assertReady(
      eligiblePoint,
      baselineLedger.length,
    );
    diagnostic.stage = "fresh-deposit-recommit-confirm-native-finalize";
    const freshCommit = await commitDeposit(forkProduction, forkReceipts);
    const freshCommittedPoint = source.points.at(-1)!.point;
    expect(
      forkAttempts.filter(
        ({ output }) =>
          output.type === "SubmittedAwaitingConfirmationOutput" &&
          output.submittedTxHash === freshCommit.output.submittedTxHash,
      ),
    ).toHaveLength(1);
    diagnostic.freshCommit = { ...freshCommit, attempts: forkAttempts };
    await assertReady(freshCommittedPoint, baselineLedger.length + 1);
    const identities = await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return {
          incarnations: yield* sql<{
            incarnation_id: Buffer;
            origin_canonical: boolean;
          }>`SELECT incarnation_id, origin_canonical FROM event_history_incarnations WHERE binding_digest = ${Buffer.from(h!.binding.digest, "hex")} AND event_id = ${oldEventId!} ORDER BY origin_canonical`,
          deposits: yield* sql<{
            history_incarnation_id: Buffer;
          }>`SELECT history_incarnation_id FROM deposits_utxos WHERE event_id = ${oldEventId!}`,
          admissions: yield* sql<{
            status: string;
          }>`SELECT status FROM tx_admissions WHERE tx_id = ${transfer.txId}`,
          payloads: yield* sql<{
            tx_id: Buffer;
            tx_canonical_cbor: Buffer;
            cek_program_material_sidecar_cbor: Buffer;
          }>`SELECT tx_id, tx_canonical_cbor, cek_program_material_sidecar_cbor
            FROM tx_admission_payloads WHERE tx_id = ${transfer.txId}`,
          receipts: yield* sql<{
            reversed_at_revision: string | null;
          }>`SELECT reversed_at_revision FROM event_history_l2_ledger_receipts WHERE ${transfer.txId} = ANY(tx_ids)`,
        };
      }),
    );
    expect(identities.incarnations).toHaveLength(2);
    expect(identities.incarnations[0]).toEqual({
      incarnation_id: oldIncarnation,
      origin_canonical: false,
    });
    expect(identities.incarnations[1]!.origin_canonical).toBe(true);
    expect(
      identities.incarnations[1]!.incarnation_id.equals(oldIncarnation),
    ).toBe(false);
    expect(identities.deposits).toEqual([
      { history_incarnation_id: identities.incarnations[1]!.incarnation_id },
    ]);
    expect(identities.admissions).toEqual([{ status: "queued" }]);
    expect(identities.payloads).toEqual([
      { tx_id: transfer.txId, ...admittedPayload },
    ]);
    expect(identities.receipts).toHaveLength(1);
    expect(identities.receipts[0]!.reversed_at_revision).not.toBeNull();
    if (descendant !== undefined) {
      const childQueued = await read(descendantState());
      expect(childQueued.admissions).toEqual([{ status: "queued" }]);
      expect(childQueued.payloads).toEqual([
        { tx_id: descendant.txId, ...descendant.payload },
      ]);
      expect(childQueued.mempool).toEqual([]);
      expect(childQueued.receipts).toHaveLength(1);
      expect(childQueued.receipts[0]!.sequence).toBe(
        descendant.originalReceiptSequence,
      );
      expect(childQueued.receipts[0]!.reversed_at_revision).toBe(
        String(identities.receipts[0]!.reversed_at_revision),
      );
      diagnostic.descendantQueued = childQueued;
    }
    diagnostic.final = {
      identities,
      forkReceipt,
      forkBatch,
      replacementMetadata: replacement.metadata,
      native: await native!.diagnostics(),
    };

    diagnostic.stage = "revalidate-original-signed-transfer";
    const beforeResume = await assertReady(
      freshCommittedPoint,
      baselineLedger.length + 1,
    );
    const authorityBeforeResume = await read(Authority.retrieve);
    expect(Option.isSome(authorityBeforeResume)).toBe(true);
    if (Option.isNone(authorityBeforeResume))
      throw new Error("Missing ready authority before resumed validation");
    // Reuse the restored durable admission. No new admit, signature, payload,
    // fabricated acceptance result or append to the original source recorder.
    await h.runWithoutSynchronizing(
      txQueueProcessorDrainOnce().pipe(Effect.provide(validationPoolLayer)),
    );
    await Effect.runPromise(writeBehind.flushNow);
    const resumed = await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return {
          admissions: yield* sql<{
            status: string;
          }>`SELECT status FROM tx_admissions
            WHERE tx_id = ${transfer.txId}`,
          mempool: yield* sql<{
            tx_id: Buffer;
            tx: Buffer;
          }>`SELECT tx_id, tx FROM mempool
            WHERE tx_id = ${transfer.txId}`,
          payloads: yield* sql<{
            tx_id: Buffer;
            tx_canonical_cbor: Buffer;
          }>`SELECT tx_id, tx_canonical_cbor
            FROM tx_admission_payloads WHERE tx_id = ${transfer.txId}`,
          receipts: yield* sql<{
            sequence: string;
            binding_digest: Buffer;
            owner_generation: string;
            checkpoint_revision: string;
            head_hash: Buffer;
            snapshot_digest: Buffer;
            reversed_at_revision: string | null;
          }>`SELECT r.*, sequence::text, owner_generation::text,
            checkpoint_revision::text, reversed_at_revision::text
            FROM event_history_l2_ledger_receipts r
            WHERE ${transfer.txId} = ANY(tx_ids) ORDER BY r.sequence`,
          origins: yield* sql<{
            sequence: string;
            event_id: Buffer;
            history_binding_digest: Buffer;
            history_incarnation_id: Buffer;
            origin_canonical: boolean;
          }>`SELECT r.sequence::text, d.event_id, d.history_binding_digest,
              d.history_incarnation_id, i.origin_canonical
            FROM event_history_l2_ledger_receipts r,
              LATERAL jsonb_populate_recordset(NULL::deposits_utxos, r.deposits_before) d
              JOIN event_history_incarnations i ON i.binding_digest = d.history_binding_digest
                AND i.incarnation_id = d.history_incarnation_id
            WHERE ${transfer.txId} = ANY(r.tx_ids) AND r.reversed_at_revision IS NULL`,
          originalPayloads: yield* sql<{
            sequence: string;
            tx_id: Buffer;
            tx_canonical_cbor: Buffer;
            cek_program_material_sidecar_cbor: Buffer;
          }>`SELECT r.sequence::text, p.tx_id, p.tx_canonical_cbor,
              p.cek_program_material_sidecar_cbor
            FROM event_history_l2_ledger_receipts r,
              LATERAL jsonb_populate_recordset(NULL::tx_admission_payloads, r.payloads_before) p
            WHERE ${transfer.txId} = ANY(r.tx_ids) AND r.reversed_at_revision IS NULL`,
          deposits: yield* sql<{
            status: string;
            history_incarnation_id: Buffer;
          }>`SELECT status,
            history_incarnation_id FROM deposits_utxos WHERE event_id = ${oldEventId!}`,
        };
      }),
    );
    expect(resumed.admissions).toEqual([{ status: "accepted" }]);
    expect(resumed.mempool).toEqual([
      { tx_id: transfer.txId, tx: queued.txCbor },
    ]);
    expect(resumed.payloads).toEqual([
      {
        tx_id: transfer.txId,
        tx_canonical_cbor: admittedPayload.tx_canonical_cbor,
      },
    ]);
    expect(resumed.receipts).toHaveLength(2);
    const priorReceipt = resumed.receipts[0]!;
    const freshReceipt = resumed.receipts[1]!;
    expect(priorReceipt.sequence).toBe(String(accepted.receipt[0]!.sequence));
    expect(priorReceipt.reversed_at_revision).toBe(
      String(identities.receipts[0]!.reversed_at_revision),
    );
    expect(priorReceipt.reversed_at_revision).not.toBeNull();
    expect(BigInt(freshReceipt.sequence)).toBeGreaterThan(
      BigInt(priorReceipt.sequence),
    );
    expect(freshReceipt.reversed_at_revision).toBeNull();
    expect(freshReceipt.binding_digest.toString("hex")).toBe(
      beforeResume.coverage.bindingDigest,
    );
    expect(freshReceipt.owner_generation).toBe(
      authorityBeforeResume.value.generation,
    );
    expect(freshReceipt.checkpoint_revision).toBe(
      beforeResume.coverage.checkpointRevision,
    );
    expect(freshReceipt.head_hash.toString("hex")).toBe(
      beforeResume.coverage.point.id,
    );
    expect(freshReceipt.snapshot_digest.toString("hex")).toBe(
      beforeResume.coverage.snapshotDigest,
    );
    const freshIncarnation = identities.incarnations[1]!.incarnation_id;
    const expectedOrigins = [
      {
        sequence: freshReceipt.sequence,
        event_id: oldEventId,
        history_binding_digest: Buffer.from(h.binding.digest, "hex"),
        history_incarnation_id: freshIncarnation,
        origin_canonical: true,
      },
    ];
    if (funding !== undefined)
      expectedOrigins.push({
        sequence: freshReceipt.sequence,
        event_id: funding.eventId,
        history_binding_digest: Buffer.from(h.binding.digest, "hex"),
        history_incarnation_id: funding.incarnationId,
        origin_canonical: true,
      });
    expect(
      [...resumed.origins].sort((a, b) =>
        Buffer.compare(a.event_id, b.event_id),
      ),
    ).toEqual(
      expectedOrigins.sort((a, b) => Buffer.compare(a.event_id, b.event_id)),
    );
    expect(
      descendant === undefined
        ? resumed.originalPayloads
        : resumed.originalPayloads.filter(({ tx_id }) =>
            tx_id.equals(transfer.txId),
          ),
    ).toEqual([
      {
        sequence: freshReceipt.sequence,
        tx_id: transfer.txId,
        ...admittedPayload,
      },
    ]);
    expect(resumed.deposits).toEqual([
      {
        status: funding === undefined ? "consumed" : "projected",
        history_incarnation_id: freshIncarnation,
      },
    ]);
    if (funding !== undefined) {
      const referenceResumed = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return {
            references: yield* sql<{
              reference_outrefs: Buffer[];
            }>`SELECT reference_outrefs FROM event_history_l2_ledger_receipts
            WHERE ${transfer.txId} = ANY(tx_ids) AND reversed_at_revision IS NULL`,
            before:
              yield* sql<MempoolLedgerDB.EntryWithTimeStamp>`SELECT l.* FROM event_history_l2_ledger_receipts r,
            LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.reference_before) l
            WHERE ${transfer.txId} = ANY(r.tx_ids) AND r.reversed_at_revision IS NULL`,
            retained:
              yield* sql<MempoolLedgerDB.EntryWithTimeStamp>`SELECT * FROM mempool_ledger WHERE outref = ${referenceInputOutref!}`,
            funding:
              yield* sql`SELECT * FROM deposits_utxos WHERE event_id = ${funding!.eventId}`,
            spentFunding:
              yield* sql`SELECT outref FROM mempool_ledger WHERE outref = ${baselineLedger[0]!.outref}`,
          };
        }),
      );
      expect(referenceResumed.references).toEqual([
        { reference_outrefs: [referenceInputOutref] },
      ]);
      expect(referenceResumed.before).toHaveLength(1);
      expect(referenceResumed.before[0]!.source_event_id).toEqual(oldEventId);
      expect(referenceResumed.retained).toEqual(referenceResumed.before);
      expect(referenceResumed.funding).toEqual([funding.originalDeposit]);
      expect(referenceResumed.spentFunding).toEqual([]);
      diagnostic.referenceResumed = referenceResumed;
    }
    if (descendant !== undefined) {
      const childResumed = await read(descendantState());
      expect(childResumed.admissions).toEqual([{ status: "accepted" }]);
      expect(childResumed.mempool).toEqual([
        { tx_id: descendant.txId, tx: descendant.payload.tx_canonical_cbor },
      ]);
      expect(
        childResumed.payloads.map(({ tx_id, tx_canonical_cbor }) => ({
          tx_id,
          tx_canonical_cbor,
        })),
      ).toEqual([
        {
          tx_id: descendant.txId,
          tx_canonical_cbor: descendant.payload.tx_canonical_cbor,
        },
      ]);
      expect(childResumed.receipts).toHaveLength(2);
      const oldChildReceipt = childResumed.receipts[0]!;
      const freshChildReceipt = childResumed.receipts[1]!;
      expect(oldChildReceipt.sequence).toBe(descendant.originalReceiptSequence);
      expect(oldChildReceipt.reversed_at_revision).toBe(
        priorReceipt.reversed_at_revision,
      );
      expect(freshChildReceipt.reversed_at_revision).toBeNull();
      expect(BigInt(freshChildReceipt.sequence)).toBeGreaterThan(
        BigInt(oldChildReceipt.sequence),
      );
      // Real queue batching may retain parent and child in one receipt, or in
      // ordered receipts. Do not change production batching to manufacture either.
      expect(BigInt(freshChildReceipt.sequence)).toBeGreaterThanOrEqual(
        BigInt(freshReceipt.sequence),
      );
      expect(freshChildReceipt.binding_digest).toEqual(
        freshReceipt.binding_digest,
      );
      expect(freshChildReceipt.owner_generation).toBe(
        authorityBeforeResume.value.generation,
      );
      expect(freshChildReceipt.checkpoint_revision).toBe(
        beforeResume.coverage.checkpointRevision,
      );
      expect(freshChildReceipt.head_hash.toString("hex")).toBe(
        beforeResume.coverage.point.id,
      );
      expect(freshChildReceipt.snapshot_digest.toString("hex")).toBe(
        beforeResume.coverage.snapshotDigest,
      );
      const originalPayloads = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            tx_id: Buffer;
            tx_canonical_cbor: Buffer;
            cek_program_material_sidecar_cbor: Buffer;
          }>`SELECT p.tx_id, p.tx_canonical_cbor, p.cek_program_material_sidecar_cbor
          FROM event_history_l2_ledger_receipts r,
            LATERAL jsonb_populate_recordset(NULL::tx_admission_payloads, r.payloads_before) p
          WHERE (${transfer.txId} = ANY(r.tx_ids) OR ${descendant!.txId} = ANY(r.tx_ids))
            AND r.reversed_at_revision IS NULL ORDER BY p.tx_id`;
        }),
      );
      expect(originalPayloads).toEqual(
        [
          { tx_id: transfer.txId, ...admittedPayload },
          { tx_id: descendant.txId, ...descendant.payload },
        ].sort((a, b) => Buffer.compare(a.tx_id, b.tx_id)),
      );
      const parentOutput = await read(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql`SELECT outref FROM mempool_ledger WHERE outref = ${descendant!.parentOutref}`;
        }),
      );
      expect(parentOutput).toEqual([]);
      diagnostic.descendantResumed = {
        ...childResumed,
        originalPayloads,
        parentOutput,
      };
    }
    const resumedReady = await assertReady(
      freshCommittedPoint,
      dependency === "spend" ? 2 : 3,
    );
    expect(resumedReady.coverage).toEqual(beforeResume.coverage);
    expect((await Effect.runPromise(writeBehind.depths)).totalDepth).toBe(0);
    diagnostic.resumed = {
      ...resumed,
      ready: resumedReady,
      native: await native!.diagnostics(),
      authorityBeforeResume: authorityBeforeResume.value,
      writeBehindDepths: await Effect.runPromise(writeBehind.depths),
    };
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    if (h !== undefined) {
      const active = h;
      const captureFailure = async (work: () => Promise<unknown>) => {
        try {
          return await work();
        } catch (cause) {
          return {
            captureError: inspect(cause, { depth: 5, colors: false }).slice(
              0,
              8000,
            ),
          };
        }
      };
      const sourcePoint = transport?.points.at(-1)?.point;
      const [reconciliationStatus, readiness, nativeState, sqlState] =
        await Promise.all([
          captureFailure(() =>
            Effect.runPromise(
              active.production.owner.reconciliationStatus.pipe(
                Effect.timeout("5 seconds"),
              ),
            ),
          ),
          captureFailure(() =>
            sourcePoint === undefined
              ? Promise.resolve({ unavailable: "No recorded source point" })
              : Effect.runPromise(
                  active.production.owner
                    .awaitReadyAt(sourcePoint)
                    .pipe(Effect.timeout("5 seconds")),
                ),
          ),
          captureFailure(() =>
            Effect.runPromise(
              Effect.gen(function* () {
                const owned = yield* Ref.get(active.globals.NATIVE_MPF_OWNER);
                return owned === undefined
                  ? undefined
                  : yield* Effect.promise(() => owned.diagnostics());
              }).pipe(Effect.timeout("5 seconds")),
            ),
          ),
          captureFailure(() =>
            read(
              Effect.gen(function* () {
                const sql = yield* SqlClient.SqlClient;
                const binding = Buffer.from(active.binding.digest, "hex");
                return {
                  recoveryPlans:
                    yield* sql`SELECT * FROM event_history_recovery_plans
              WHERE binding_digest = ${binding} ORDER BY updated_at DESC LIMIT 10`,
                  authority:
                    yield* sql`SELECT * FROM event_history_authority LIMIT 1`,
                  cursor:
                    yield* sql`SELECT binding_digest, manifest_id, anchor_hash,
              anchor_slot, anchor_height, head_hash, head_slot, head_height,
              head_application_revision, snapshot_digest, revision
              FROM event_history_cursor WHERE binding_digest = ${binding}`,
                  journals:
                    yield* sql`SELECT header_hash, status, intended_tx_hash,
              submitted_tx_hash, base_utxos_root, expected_utxos_root,
              base_tail_out_ref, block_start_time, block_end_time,
              octet_length(signed_tx_cbor) AS signed_tx_cbor_bytes, updated_at
              FROM pending_block_finalizations
              WHERE deployment_manifest_id = ${active.binding.manifestId}
              ORDER BY updated_at DESC LIMIT 10`,
                };
              }).pipe(Effect.timeout("5 seconds")),
            ),
          ),
        ]);
      diagnostic.failureState = {
        stage: diagnostic.stage,
        sourcePoint,
        reconciliationStatus,
        readiness,
        native: nativeState,
        sql: sqlState,
      };
    }
    throw error;
  } finally {
    try {
      const path =
        evidencePath ??
        (dependency === "spend"
          ? process.env.MIDGARD_L1_ROLLBACK_EVIDENCE_PATH
          : undefined);
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual signed accepted L1 deposit commitments and unpublished dependent L2 transfer, real queue and production owner inverse; synthetic ancestry/genesis/source rollback. No real network rollback or HTTP admission.",
              dependency,
              restartWhilePending,
              manifestId: h?.deployment.manifest.manifestId,
              blueprintSha256: h?.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h?.deploymentInfoSha256,
              binding: h?.binding,
              genesis: h?.genesis,
              protocolParameters:
                h?.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              branches: transport?.branches,
              points: transport?.points,
              requests: transport?.requests,
              receipts: h?.receipts,
              forkReceipts,
            },
            (_key, value) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ) + "\n",
        );
      }
    } finally {
      try {
        forkObserver?.restore();
        await h?.close();
      } finally {
        vi.useRealTimers();
      }
    }
  }
};

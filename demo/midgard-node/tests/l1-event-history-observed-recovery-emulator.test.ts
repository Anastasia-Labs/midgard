import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput, Data, Emulator } from "@lucid-evolution/lucid";
import { Deferred, Effect, Option, Ref, Schedule } from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { decodeBoundEventHistoryLedgerSnapshot } from "../src/l1-event-history-source.js";
import { Database } from "../src/services/database.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import { WriteBehind } from "../src/services/write-behind.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  type ProductionHistoryFixtureRuntime,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  SDK,
  serializeStateQueueUTxO,
  stateQueueFetchConfig,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  type AcceptedHistoryObservation,
  historyOutputObservation,
} from "./helpers/history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./helpers/history-rollback-transport.js";

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

// Real published deployment, production owner/commit/confirmation/native undo.
// Only source genesis/ancestry and the selected emulator rollback are synthetic.
// No SQL status/receipt/native mutation creates the observed branch, and no local
// finalization worker or DA writer is called before rollback.
it("recovers a genuinely observed deposit-only signed commitment before local finalization", async () => {
  let transport: ReturnType<typeof makeRollbackHistoryTransport> | undefined;
  let h:
    | Awaited<ReturnType<typeof openHistoryProductionOwnerLifecycle>>
    | undefined;
  let rollbackPointId: string | undefined;
  let nativeBaseline: string | undefined;
  let expectedNativeRoot: string | undefined;
  let headerHash: Buffer | undefined;
  let eventId: Buffer | undefined;
  let retainedRecoveryId: string | undefined;
  const prepared = await Effect.runPromise(Deferred.make<void>());
  const diagnostic: Record<string, unknown> = { stage: "startup" };
  const state = Effect.gen(function* () {
    if (headerHash === undefined || eventId === undefined)
      throw new Error("Missing observed identity");
    const sql = yield* SqlClient.SqlClient;
    return {
      journal: yield* sql<{
        status: string;
        state_queue_lease_token: string;
        signed_tx_cbor: Buffer;
        prepared_tx_hash: Buffer;
        intended_tx_hash: Buffer;
        submitted_tx_hash: Buffer | null;
        updated_at: Date;
        observed_confirmed_at_ms: string | null;
        base_utxos_root: string;
        expected_utxos_root: string;
      }>`SELECT * FROM pending_block_finalizations WHERE header_hash=${headerHash}`,
      members:
        yield* sql`SELECT * FROM pending_block_finalization_deposits WHERE header_hash=${headerHash} ORDER BY ordinal`,
      deposits: yield* sql<{
        projected_header_hash: Buffer | null;
        history_incarnation_id: Buffer;
        status: string;
      }>`SELECT * FROM deposits_utxos WHERE event_id=${eventId}`,
      ledger: yield* sql`SELECT * FROM mempool_ledger ORDER BY outref`,
      confirmed: yield* sql`SELECT * FROM confirmed_ledger ORDER BY outref`,
      jobs: yield* sql`SELECT * FROM local_mutation_jobs WHERE job_id=${`local_block_finalization:${headerHash.toString("hex")}`}`,
      da: yield* sql`SELECT * FROM da_payloads WHERE header_hash=${headerHash}`,
      leases: yield* sql<{
        token: string;
        status: string;
        released_at: Date | null;
      }>`SELECT * FROM state_queue_mutation_leases ORDER BY token`,
      inverseReceipts:
        yield* sql`SELECT * FROM event_history_l2_ledger_receipts ORDER BY sequence`,
      plans: yield* sql<{
        recovery_id: Buffer;
        intent: string;
        state: string;
        checkpoint_revision: string;
      }>`SELECT *, checkpoint_revision::text FROM event_history_recovery_plans ORDER BY recovery_id`,
      engine: yield* sql<{
        root_hex: string;
      }>`SELECT * FROM mpf_engine_state WHERE store_name='ledger'`,
    };
  });
  try {
    h = await openHistoryProductionOwnerLifecycle({
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
          if (h === undefined) throw new Error("Missing runtime");
          const authority = yield* Authority.retrieve;
          expect(Option.isSome(authority) && authority.value.state).toBe(
            "recovering",
          );
          const cache = yield* MempoolLedgerCache;
          const cacheClaim = yield* Effect.either(
            cache.withClaimLock(Effect.void),
          );
          const producer = yield* Effect.either(
            h.production.owner.runProducer(() => Effect.void),
          );
          expect(cacheClaim._tag).toBe("Left");
          expect(producer._tag).toBe("Left");
          const owner = yield* Ref.get(h.globals.NATIVE_MPF_OWNER);
          expect(owner).toBeDefined();
          const native = yield* Effect.promise(() => owner!.diagnostics());
          expect(native.durableRoot).toBe(nativeBaseline);
          const restored = yield* state;
          expect(restored.journal).toHaveLength(1);
          expect(restored.journal[0]!.status).toBe("abandoned");
          expect(restored.jobs).toEqual([]);
          expect(restored.da).toEqual([]);
          expect(restored.inverseReceipts).toEqual([]);
          expect(restored.ledger).toEqual([]);
          expect(restored.confirmed).toEqual([]);
          expect(restored.deposits).toEqual([]);
          expect(restored.plans).toHaveLength(1);
          expect(restored.plans[0]!.state).toBe("applied");
          retainedRecoveryId = restored.plans[0]!.recovery_id.toString("hex");
          expect(retainedRecoveryId).toBe(
            createHash("sha256")
              .update(restored.plans[0]!.intent)
              .digest("hex"),
          );
          diagnostic.beforeReady = {
            checkpoint: checkpoint.head,
            revision: checkpoint.revision,
            authority,
            cacheClaimRefused: true,
            producerRefused: true,
            native,
            restored,
          };
          yield* Deferred.succeed(prepared, undefined);
        }),
    });
    const { fixture, production, globals } = h;
    const source = transport!;
    const wallet = fixture.depositorLucid;
    const address = await wallet.wallet().address();
    const native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
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

    diagnostic.stage = "deposit-admission";
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignBeforeAdmission(production);
    const ancestor = source.points.at(-1)!.point;
    const beforeDeposit = snapshotEmulator(fixture.emulator);
    expect(Object.keys(beforeDeposit.mempool)).toHaveLength(0);
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    const admissionHash = await signed.submit();
    expect(await wallet.awaitTx(admissionHash)).toBe(true);
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
    expect(deposits).toHaveLength(1);
    eventId = deposits[0]!.idCbor;
    expect(eventId.toString("hex")).toBe(built.metadata.depositEventId);
    expect(deposits[0]!.originalAssets).toEqual({ lovelace: 12_000_000n });
    await assertReady(source.points.at(-1)!.point, 0);
    diagnostic.admission = {
      metadata: built.metadata,
      eventId,
      originalAssets: deposits[0]!.originalAssets,
      ancestor,
      signedCbor: signed.toCBOR(),
    };

    diagnostic.stage = "commit-and-observe-without-local-finalization";
    const beforeNative = await native!.diagnostics();
    const latestBlock = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const output = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService: h.lucidService,
      latestBlock,
      nodeConfig: production.nodeConfig,
      production: { ...production, globals },
    });
    expect(await fixture.operatorLucid.awaitTx(output.submittedTxHash)).toBe(
      true,
    );
    const queue = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    const header = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(queue.at(-1)!.datum),
    );
    headerHash = Buffer.from(
      await Effect.runPromise(SDK.hashBlockHeader(header)),
      "hex",
    );
    expectedNativeRoot = header.utxosRoot;
    expect(expectedNativeRoot).not.toBe(nativeBaseline);
    expect(header.prevUtxosRoot).toBe(nativeBaseline);
    expect((await native!.diagnostics()).durableRoot).toBe(expectedNativeRoot);
    await production.synchronize();
    await runBlockConfirmation(
      globals,
      fixture.contracts,
      h.lucidService,
      production.nodeConfig,
      production,
    );
    // Deliberately stop here: confirmation is genuine, local finalization has
    // never been invoked. A status UPDATE would not demonstrate this branch.
    const observed = await read(state);
    expect(observed.journal).toHaveLength(1);
    const journal = observed.journal[0]!;
    expect(journal.status).toBe("observed_waiting_stability");
    expect(journal.observed_confirmed_at_ms).not.toBeNull();
    // The ordinary successful commit helper releases its real lease. This does
    // not exercise a process crash retaining an active token, and no lease row
    // is manufactured to create that separate recovery case.
    const observedLease = observed.leases.filter(
      ({ token }) => token === journal.state_queue_lease_token,
    );
    expect(observedLease).toHaveLength(1);
    expect(observedLease[0]!.status).toBe("released");
    expect(observedLease[0]!.released_at).not.toBeNull();
    const unrelatedLeases = observed.leases.filter(
      ({ token }) => token !== journal.state_queue_lease_token,
    );
    expect(observed.members).toHaveLength(1);
    expect(observed.deposits).toHaveLength(1);
    expect(observed.deposits[0]!.projected_header_hash).toEqual(headerHash);
    expect(observed.deposits[0]!.status).toBe("projected");
    expect(observed.confirmed).toEqual([]);
    expect(observed.jobs).toEqual([]);
    expect(observed.da).toEqual([]);
    expect(observed.plans).toEqual([]);
    expect(observed.inverseReceipts).toEqual([]);
    const accepted = h.receipts.filter(
      (r) => r.transaction.txHash === output.submittedTxHash,
    );
    expect(accepted).toHaveLength(1);
    const receipt = accepted[0]!;
    const body = CML.Transaction.from_cbor_hex(receipt.signedCbor).body();
    expect(CML.hash_transaction(body).to_hex()).toBe(output.submittedTxHash);
    expect(journal.signed_tx_cbor.toString("hex")).toBe(receipt.signedCbor);
    expect(journal.prepared_tx_hash.toString("hex")).toBe(
      output.submittedTxHash,
    );
    expect(journal.intended_tx_hash.toString("hex")).toBe(
      output.submittedTxHash,
    );
    expect(journal.submitted_tx_hash?.toString("hex")).toBe(
      output.submittedTxHash,
    );
    const ttl = body.ttl();
    const lower = body.validity_interval_start();
    expect(ttl).toBeDefined();
    expect(lower).toBeDefined();
    if (ttl === undefined || lower === undefined)
      throw new Error("Missing bounded signed interval");
    expect(BigInt(ancestor.slot)).toBeLessThanOrEqual(lower);
    expect(fixture.operatorLucid.slotToUnixTime(Number(ttl)) - 1).toBe(
      Number(header.endTime),
    );
    const unit =
      fixture.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash.toString("hex");
    const signedOutputs = Array.from({ length: body.outputs().len() }, (_, i) =>
      coreToTxOutput(body.outputs().get(i)),
    ).filter((o) => o.assets[unit] === 1n);
    expect(signedOutputs).toHaveLength(1);
    const signedHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(
        SDK.linkedListDatumToNodeView(
          Data.from(signedOutputs[0]!.datum!, SDK.LinkedListDatum),
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash.toString("hex"),
        ),
      ),
    );
    expect(signedHeader).toEqual(header);
    await assertReady(source.points.at(-1)!.point, 1);
    const afterNative = await native!.diagnostics();
    expect(journal.base_utxos_root).toBe(beforeNative.durableRoot);
    expect(journal.expected_utxos_root).toBe(afterNative.durableRoot);
    diagnostic.observed = {
      output,
      header,
      headerHash,
      receipt,
      signedTtl: ttl.toString(),
      signedStart: lower.toString(),
      beforeNative,
      afterNative,
      state: observed,
    };

    diagnostic.stage = "rollback-before-expiry";
    h.observer.restore();
    restoreEmulator(fixture.emulator, beforeDeposit);
    vi.setSystemTime(fixture.emulator.now());
    source.rollbackTo(ancestor.id);
    const waitForJournal = (point: { id: string; slot: number }) =>
      read(
        Journal.load(h!.binding).pipe(
          Effect.flatMap((checkpoint) =>
            checkpoint?.head.id === point.id
              ? Effect.succeed(checkpoint)
              : Effect.fail(
                  new Error("Source has not retained requested point"),
                ),
          ),
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
      const claim = await Effect.runPromise(
        Effect.either(production.cache.withClaimLock(Effect.void)),
      );
      expect(claim._tag).toBe("Left");
      const retained = await read(state);
      expect(retained.journal).toEqual(observed.journal);
      expect(retained.members).toEqual(observed.members);
      expect(retained.deposits).toEqual(observed.deposits);
      expect(retained.ledger).toEqual(observed.ledger);
      expect(retained.confirmed).toEqual(observed.confirmed);
      expect(retained.leases).toEqual(observed.leases);
      expect(retained.jobs).toEqual([]);
      expect(retained.da).toEqual([]);
      expect(retained.plans).toEqual([]);
      expect(retained.inverseReceipts).toEqual([]);
      const held = await native!.diagnostics();
      expect(held.durableRoot).toBe(header.utxosRoot);
      return {
        authority,
        status,
        retained,
        native: held,
        cacheClaimRefused: true,
        producerRefused: true,
      };
    };
    diagnostic.pendingRollback = { ancestor, ...(await assertPending()) };
    const appendInterval = async (last = false) => {
      const point = source.appendFork({
        observations: [],
        observedSlot: fixture.emulator.slot,
        observedHeight: fixture.emulator.blockHeight,
        outputs: await outputsAt(fixture.operatorLucid),
      });
      if (last) rollbackPointId = point.id;
      await assertStateQueueSnapshot(point);
      return point;
    };
    const signedTtl = Number(ttl);
    expect(Number.isSafeInteger(signedTtl)).toBe(true);
    if (fixture.emulator.slot <= signedTtl)
      fixture.emulator.awaitSlot(signedTtl + 1 - fixture.emulator.slot);
    fixture.emulator.awaitBlock(1);
    const firstExpiredPoint = await appendInterval();
    await waitForJournal(firstExpiredPoint);
    diagnostic.expiredButUnfinalized = {
      point: firstExpiredPoint,
      ...(await assertPending()),
    };
    const finalityDepth = h.deployment.manifest.l1Finality.confirmationDepth;
    expect(finalityDepth).toBeGreaterThan(0);
    let dispositionPoint = firstExpiredPoint;
    for (let depth = 0; depth < finalityDepth; depth++) {
      fixture.emulator.awaitBlock(1);
      const last = depth === finalityDepth - 1;
      dispositionPoint = await appendInterval(last);
      if (!last) {
        await waitForJournal(dispositionPoint);
        await assertPending();
      }
    }
    expect(dispositionPoint.height - firstExpiredPoint.height).toBe(
      finalityDepth,
    );
    expect(dispositionPoint.slot).toBeGreaterThan(signedTtl);
    diagnostic.coverage = {
      ancestor,
      firstExpiredPoint,
      dispositionPoint,
      finalityDepth,
      signedTtl: ttl.toString(),
    };
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
    diagnostic.ready = await assertReady(dispositionPoint, 0);
    const restored = await read(state);
    expect(restored.journal).toHaveLength(1);
    expect(restored.journal[0]).toEqual({
      ...journal,
      status: "abandoned",
      updated_at: restored.journal[0]!.updated_at,
    });
    expect(restored.members).toEqual(observed.members);
    expect(
      restored.leases.filter(
        ({ token }) => token === journal.state_queue_lease_token,
      ),
    ).toEqual(observedLease);
    expect(
      restored.leases.filter(
        ({ token }) => token !== journal.state_queue_lease_token,
      ),
    ).toEqual(unrelatedLeases);
    expect(restored.deposits).toEqual([]);
    expect(restored.ledger).toEqual([]);
    expect(restored.confirmed).toEqual([]);
    expect(restored.jobs).toEqual([]);
    expect(restored.da).toEqual([]);
    expect(restored.inverseReceipts).toEqual([]);
    expect(restored.plans).toHaveLength(1);
    const plan = restored.plans[0]!;
    expect(plan.state).toBe("applied");
    expect(plan.recovery_id.toString("hex")).toBe(retainedRecoveryId);
    const intent = JSON.parse(plan.intent);
    expect(intent.expectedRoot).toBe(header.utxosRoot);
    expect(intent.targetRoot).toBe(nativeBaseline);
    expect(intent.headerHash).toBe(headerHash.toString("hex"));
    expect(intent.signedTransactionHash).toBe(output.submittedTxHash);
    expect(intent.signedTransactionCborSha256).toBe(
      createHash("sha256")
        .update(Buffer.from(receipt.signedCbor, "hex"))
        .digest("hex"),
    );
    expect(restored.engine).toHaveLength(1);
    expect(restored.engine[0]!.root_hex).toBe(nativeBaseline);
    diagnostic.restored = {
      state: restored,
      native: await native!.diagnostics(),
    };
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

    // Read the existing service under the Ready producer fence without asking
    // the original, now sealed recorder to synchronize the selected fork.
    const writeBehind = await h.runWithoutSynchronizing(WriteBehind);
    expect((await Effect.runPromise(writeBehind.depths)).totalDepth).toBe(0);
    expect((await read(Authority.retrieve)).pipe(Option.getOrThrow).state).toBe(
      "ready",
    );
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    if (h !== undefined && headerHash !== undefined && eventId !== undefined) {
      try {
        diagnostic.failureState = await read(state);
      } catch (cause) {
        diagnostic.failureStateError = inspect(cause, { depth: 10 });
      }
      try {
        const owner = await Effect.runPromise(
          Ref.get(h.globals.NATIVE_MPF_OWNER),
        );
        diagnostic.failureNative = await owner?.diagnostics();
      } catch (cause) {
        diagnostic.failureNativeError = inspect(cause, { depth: 10 });
      }
    }
    throw error;
  } finally {
    try {
      const path = process.env.MIDGARD_OBSERVED_RECOVERY_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual accepted signed deposit commitment and genuine ObservedWaitingStability before local finalization, production source owner/native recovery. Synthetic genesis/ancestry/emulator rollback; no live consensus, local status mutation, job/DA creation or OS crash.",
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
            },
            (_key, value) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ) + "\n",
        );
      }
    } finally {
      try {
        await h?.close();
      } finally {
        vi.useRealTimers();
      }
    }
  }
});

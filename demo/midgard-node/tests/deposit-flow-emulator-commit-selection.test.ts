import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it, vi } from "vitest";

import { withHistoryWrite } from "../src/services/event-history-producer.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  attestQueuedStateQueueHeader,
  buildTransferTx,
  canonicalSlotConfigForLucid,
  CML,
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  commitWorkerProgram,
  configureEmulatorDaRuntimeManifest,
  confirmedLedgerFullScanCounter,
  ContractDeploymentIdentity,
  createHash,
  Data,
  Database,
  decodeNodeUtxo,
  Effect,
  EMPTY_PROGRAM_MATERIAL_SIDECAR,
  EMULATOR_DEPLOYMENT_IDENTITY,
  fetchLatestCommittedBlock,
  ForcedTransactionsDB,
  getStateQueueDatumEndTime,
  initializeNodeRuntime,
  initializeProtocol,
  Ledger,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  makeNodeConfigForFixture,
  materializeConfirmedLedgerSnapshot,
  MempoolDB,
  MempoolLedgerDB,
  Metric,
  MIDGARD_CONSENSUS_PROFILE,
  type NodeConfigDep,
  type NodeUtxo,
  Option,
  PendingBlockFinalizationsDB,
  processedTxFromValidatedTx,
  type QueuedTx,
  randomUUID,
  resetActiveRuntimePaths,
  retainAndAttestSubmittedHeader,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runNodeDatabaseEffect,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  runUnownedNativeCommit,
  SDK,
  SqlClient,
  submitDepositAndRefreshBarriers,
  TxAdmissionsDB,
  TxUtils,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { insertForcedEntriesWithOrders } from "./helpers/emulator-l1-follower.forced-orders.js";

describe("deposit flow emulator", { concurrent: false }, () => {
  // 900s leaves headroom for the full real-contract workflow. Protocol
  // bring-up publishes the 153-target reference-script roster in 39 planned
  // batches (with size-driven splits where required).
  it("hydrates periodic and off commit candidates when stateful work is selected", async () => {
    await configureEmulatorDaRuntimeManifest();

    const runCase = async (payloadRootCheck: "periodic" | "off") => {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      const baseNodeConfig = await makeNodeConfigForFixture(fixture);
      const nodeConfig: NodeConfigDep = {
        ...baseNodeConfig,
        MPF_PAYLOAD_ROOT_CHECK: payloadRootCheck,
        MPF_RECORD_CORPUS: "",
        COMMIT_MAX_L2_TX_COUNT: 1,
        MIN_FEE_A: 0n,
        MIN_FEE_B: 0n,
      };
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      // Two real deposits provide an authenticated base snapshot with two
      // independent UTxOs for the normal and forced spends below.
      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 13_000_000n,
      });
      const baseBlock = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const baseCommit = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: baseBlock,
        nodeConfig,
      });
      await fixture.operatorLucid.awaitTx(baseCommit.submittedTxHash);
      await retainAndAttestSubmittedHeader({
        fixture,
        lucidService,
        globals,
        headerHash: baseCommit.submittedHeaderHash,
        submittedTxHash: baseCommit.submittedTxHash,
      });
      await advanceEmulatorPastUnixTime(fixture, baseCommit.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));

      const baseJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(baseJournal)).toBe(true);
      if (Option.isNone(baseJournal)) {
        throw new Error("Expected the authenticated base journal");
      }
      const baseSnapshot = await runNodeDatabaseEffect(
        materializeConfirmedLedgerSnapshot(baseJournal.value),
      );
      expect(baseSnapshot.root).toBe(
        baseJournal.value[
          PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
        ],
      );
      expect(baseSnapshot.entries.length).toBeGreaterThanOrEqual(2);
      const senderAddress = await fixture.depositorLucid.wallet().address();
      const sourceUtxos = baseSnapshot.entries
        .map((entry) =>
          decodeNodeUtxo({
            outref: entry[Ledger.Columns.OUTREF].toString("hex"),
            outputCbor: entry[Ledger.Columns.OUTPUT].toString("hex"),
          }),
        )
        .filter((utxo) => utxo.address === senderAddress)
        .slice(0, 2);
      expect(sourceUtxos).toHaveLength(2);
      const signer = CML.PrivateKey.from_bech32(
        walletFromSeed(fixture.depositorAccount.seedPhrase, {
          network: "Custom",
        }).paymentKey,
      );
      const destinationAddress = await fixture.referenceScriptsLucid
        .wallet()
        .address();

      // Every later run builds on the confirmed, locally finalized base.
      await fixture.operatorLucid.awaitTx(baseCommit.submittedTxHash);
      await runBlockConfirmation(globals, fixture.contracts, lucidService);
      const baseRecovery = await runLocalFinalizationRecoveryWorker(
        globals,
        fixture.contracts,
        lucidService,
      );
      expect(baseRecovery.type).toBe(
        "SuccessfulLocalFinalizationRecoveryOutput",
      );

      // With no selected normal or forced work the worker stops before it
      // hydrates any commit base; the full confirmed-ledger scan stays
      // untouched.
      const controlScanBefore = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      const controlOutput = await Effect.runPromise(
        runUnownedNativeCommit(
          fixture.contracts,
          lucidService,
          nodeConfig,
          {
            data: {
              availableConfirmedBlock: "",
              availableLocalFinalizationBlock: "",
              currentBlockStartTimeMs: baseCommit.blockEndTimeMs,
              forcedValidationSlotConfig: canonicalSlotConfigForLucid(
                lucidService.api,
              ),
              ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
              localFinalizationPending: false,
              mempoolTxsCountSoFar: 0,
              sizeOfProcessedTxsSoFar: 0,
            },
          },
          (nativeInput) =>
            commitWorkerProgram(
              fixture.contracts,
              lucidService,
              nativeInput,
              undefined,
              nodeConfig,
            ),
        ).pipe(
          Effect.provideService(
            ContractDeploymentIdentity,
            EMULATOR_DEPLOYMENT_IDENTITY,
          ),
          Effect.provide(Database.layer),
        ),
      );
      const controlScanAfter = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      expect(controlOutput.type).toBe("NothingToCommitOutput");
      expect(controlScanAfter.count).toBe(controlScanBefore.count);

      const eventTime = new Date(
        Math.max(Date.now(), baseCommit.blockEndTimeMs + 1),
      );
      const normalTransfer = await buildTransferTx({
        senderAddress,
        destinationAddress,
        signer,
        selectedInputs: [sourceUtxos[0]!],
        requestedAssets: { lovelace: 2_000_000n },
        networkId: 0n,
      });
      const forcedTransfer = await buildTransferTx({
        senderAddress,
        destinationAddress,
        signer,
        selectedInputs: [sourceUtxos[1]!],
        requestedAssets: { lovelace: 2_000_000n },
        networkId: 0n,
      });

      const queuedNormal = {
        txId: normalTransfer.txId,
        txCbor: normalTransfer.txCbor,
        programMaterialSidecarCbor: EMPTY_PROGRAM_MATERIAL_SIDECAR,
        arrivalSeq: 0n,
        createdAt: eventTime,
      } satisfies QueuedTx;
      const phaseA = await Effect.runPromise(
        runPhaseAValidation([queuedNormal], {
          expectedNetworkId: 0n,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
        }),
      );
      expect(phaseA.rejected).toEqual([]);
      const initialLedger = new Map(
        baseSnapshot.entries.map((entry) => [
          entry[Ledger.Columns.OUTREF].toString("hex"),
          entry[Ledger.Columns.OUTPUT],
        ]),
      );
      const phaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(phaseA.accepted, initialLedger, {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
          enforceScriptBudget: true,
        }),
      );
      expect(phaseB.rejected).toEqual([]);
      const processedNormal = phaseB.accepted.map(
        processedTxFromValidatedTx,
      )[0];
      if (processedNormal === undefined) {
        throw new Error("Expected the normal spend to pass validation");
      }

      await runNodeDatabaseEffect(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* TxAdmissionsDB.tryInsert({
            txId: normalTransfer.txId,
            txCanonicalCbor: normalTransfer.txCbor,
            programMaterialSidecarCbor: EMPTY_PROGRAM_MATERIAL_SIDECAR,
            submitSource: "native",
          });
          yield* withHistoryWrite(
            sql.withTransaction(
              MempoolDB.insertMultipleCore([processedNormal]),
            ),
          );
          yield* sql`UPDATE ${sql(MempoolDB.tableName)}
            SET time_stamp_tz = ${eventTime}
            WHERE ${sql(TxUtils.Columns.TX_ID)} = ${normalTransfer.txId}`;
        }),
      );

      const forcedEncoding = await Effect.runPromise(
        ForcedTransactionsDB.encodeForcedInclusionValueV1({
          nativeTxCbor: encodeMidgardForcedTxCanonical(
            decodeMidgardNativeTxFullFromCanonicalCbor(forcedTransfer.txCbor),
          ),
          verdict: "ForcedTxValid",
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      );
      const forcedEventId = Buffer.from(
        Data.to(
          {
            transactionId: "f1".repeat(32),
            outputIndex: 0n,
          },
          SDK.OutputReference,
        ),
        "hex",
      );
      const forcedSidecar = EMPTY_PROGRAM_MATERIAL_SIDECAR;
      const forcedEntry: ForcedTransactionsDB.Entry = {
        [ForcedTransactionsDB.Columns.TX_ORDER_ID]: forcedEventId,
        [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(
          32,
          0x42,
        ),
        [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: 0,
        [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.alloc(32, 0x43),
        [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from("01", "hex"),
        [ForcedTransactionsDB.Columns.TX_ID]: forcedEncoding.txId,
        [ForcedTransactionsDB.Columns.TX_COMPACT]: forcedEncoding.txCompact,
        [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]:
          forcedEncoding.value,
        [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
          MIDGARD_CONSENSUS_PROFILE.profileId,
        [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]:
          encodeMidgardForcedTxCanonical(
            decodeMidgardNativeTxFullFromCanonicalCbor(forcedTransfer.txCbor),
          ),
        [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
          forcedEncoding.transactionCommitment,
        [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
          forcedSidecar,
        [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
          createHash("sha256").update(forcedSidecar).digest(),
        [ForcedTransactionsDB.Columns.INCLUSION_TIME]: eventTime,
        [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [ForcedTransactionsDB.Columns.STATUS]:
          ForcedTransactionsDB.Status.Awaiting,
      };
      await runNodeDatabaseEffect(
        insertForcedEntriesWithOrders(
          [forcedEntry],
          fixture.operatorLucid.currentSlot(),
        ),
      );

      // Both events sit inside the worker's retrieval window and its
      // tx-order ingestion barrier.
      await advanceEmulatorPastUnixTime(fixture, eventTime.getTime() + 1_000);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const scanBefore = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      const stateful = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        ),
        nodeConfig,
      });
      const scanAfter = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      expect(scanAfter.count - scanBefore.count).toBeGreaterThan(0n);
      expect(stateful.mempoolTxsCount).toBe(1);

      const active = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(active)).toBe(true);
      if (Option.isNone(active)) {
        throw new Error("Expected the stateful candidate journal");
      }
      expect(active.value.mempoolTxIds).toEqual([normalTransfer.txId]);
      expect(active.value.forcedTransactionEventIds).toEqual([forcedEventId]);
      expect(active.value.depositEventIds).toEqual([]);
      expect(active.value.withdrawalEventIds).toEqual([]);
      expect(
        active.value[
          PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT
        ],
      ).toBe(1n);
      expect(
        active.value[
          PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTION_COUNT
        ],
      ).toBe(1n);
      expect(active.value.forcedTransactionMembers).toHaveLength(1);
      const forcedJournalMember =
        ForcedTransactionsDB.decodeForcedTransactionJournalMember(
          active.value.forcedTransactionMembers[0]![
            PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR
          ],
        );
      const forcedSource = Data.from(
        forcedJournalMember.sourceValueCbor.toString("hex"),
        SDK.ForcedInclusionTxV1,
      );
      expect(forcedSource.verdict).toBe("ForcedTxValid");

      const postState = await runNodeDatabaseEffect(
        materializeConfirmedLedgerSnapshot(active.value),
      );
      expect(postState.entries.length).toBeGreaterThan(0);
      expect(postState.root).toBe(stateful.submittedUtxosRoot);
      expect(
        active.value[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
      ).toBe(postState.root);
    };

    for (const payloadRootCheck of ["periodic", "off"] as const) {
      await runCase(payloadRootCheck);
    }
  }, 900_000);

  // 900s leaves headroom for the full real-contract workflow. Protocol
  // bring-up publishes the 153-target reference-script roster in 39 planned
  // batches (with size-driven splits where required).
  it("commits the globally oldest transactions from a backlog deeper than three retrieval pages and anchors max endTime", async () => {
    const previousPageSize = process.env.MEMPOOL_RETRIEVE_PAGE_SIZE;
    process.env.MEMPOOL_RETRIEVE_PAGE_SIZE = "2";
    try {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      // Eight real deposits give the backlog eight independent L2 UTxOs in the
      // committed ledger; the native owner admits no configured genesis that
      // differs from the committed state-queue root.
      const globals = await makeGlobalsService();
      const nodeConfig = await makeNodeConfigForFixture(fixture);
      for (let index = 0; index < 8; index += 1) {
        await submitDepositAndRefreshBarriers({
          fixture,
          lucidService,
          globals,
          lovelace: 10_000_000n,
        });
      }
      const fundingCommit = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        ),
        nodeConfig,
      });
      await fixture.operatorLucid.awaitTx(fundingCommit.submittedTxHash);
      await runBlockConfirmation(globals, fixture.contracts, lucidService);
      const fundingRecovery = await runLocalFinalizationRecoveryWorker(
        globals,
        fixture.contracts,
        lucidService,
      );
      expect(fundingRecovery.type).toBe(
        "SuccessfulLocalFinalizationRecoveryOutput",
      );
      await attestQueuedStateQueueHeader({
        fixture,
        lucidService,
        globals,
        headerHash: fundingCommit.submittedHeaderHash,
      });
      await advanceEmulatorPastUnixTime(fixture, fundingCommit.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));

      const senderAddress = await fixture.depositorLucid.wallet().address();
      const senderSigner = CML.PrivateKey.from_bech32(
        walletFromSeed(fixture.depositorAccount.seedPhrase, {
          network: "Custom",
        }).paymentKey,
      );
      const destination = walletFromSeed(
        "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
        { network: "Preprod" },
      );
      const sourceUtxos: NodeUtxo[] = (
        await runNodeDatabaseEffect(
          MempoolLedgerDB.retrieveByAddress(senderAddress),
        )
      ).map((entry) =>
        decodeNodeUtxo({
          outref: entry[Ledger.Columns.OUTREF].toString("hex"),
          outputCbor: entry[Ledger.Columns.OUTPUT].toString("hex"),
        }),
      );
      expect(sourceUtxos).toHaveLength(8);
      const built = await Promise.all(
        sourceUtxos.map((source) =>
          buildTransferTx({
            senderAddress,
            destinationAddress: destination.address,
            signer: senderSigner,
            selectedInputs: [source],
            requestedAssets: { lovelace: 1_000_000n },
            networkId: 0n,
          }),
        ),
      );
      const queued: QueuedTx[] = built.map((tx, index) => ({
        txId: tx.txId,
        txCbor: tx.txCbor,
        programMaterialSidecarCbor: EMPTY_PROGRAM_MATERIAL_SIDECAR,
        arrivalSeq: BigInt(index),
        createdAt: new Date(Date.now() - 10_000 + index),
      }));
      const phaseA = await Effect.runPromise(
        runPhaseAValidation(queued, {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
        }),
      );
      expect(phaseA.rejected).toEqual([]);
      const initialLedger = new Map(
        sourceUtxos.map((source) => [
          source.outrefCbor.toString("hex"),
          source.outputCbor,
        ]),
      );
      const phaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(phaseA.accepted, initialLedger, {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        }),
      );
      expect(phaseB.rejected).toEqual([]);
      const processed = phaseB.accepted.map(processedTxFromValidatedTx);
      const latestBlock = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const tipEndTimeMs = await getStateQueueDatumEndTime(latestBlock.datum);
      // Identical arrival timestamps make "globally oldest" tie-break on the
      // canonical txId order the assertions below expect; the shared instant
      // still sits strictly after the confirmed tip's semantic end time.
      const oldestBacklogTimeMs = Math.max(
        Date.now() - 8_000,
        tipEndTimeMs + 1,
      );
      const timestamps = processed.map(() => new Date(oldestBacklogTimeMs));
      const processedInCanonicalBacklogOrder = [...processed].sort(
        (left, right) => Buffer.compare(left.txId, right.txId),
      );
      // Advance the emulator clock past the seeded arrival instant so the
      // worker's mempool retrieval window covers the whole backlog even when
      // the tip anchor pushed the instant into the future.
      await advanceEmulatorPastUnixTime(fixture, oldestBacklogTimeMs + 1_000);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const durableAdmissions = await runNodeDatabaseEffect(
        Effect.forEach(
          queued,
          (tx) =>
            TxAdmissionsDB.tryInsert({
              txId: tx.txId,
              txCanonicalCbor: tx.txCbor,
              programMaterialSidecarCbor:
                tx.programMaterialSidecarCbor ?? EMPTY_PROGRAM_MATERIAL_SIDECAR,
              submitSource: "native",
            }),
          { concurrency: 1 },
        ),
      );
      expect(durableAdmissions.every((entry) => entry !== null)).toBe(true);
      await runNodeDatabaseEffect(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          // The commit worker's canonical V1 revalidation requires a durable
          // program-material sidecar in tx_admission_payloads for every
          // mempool transaction, so mirror the production admission write.
          yield* Effect.forEach(
            queued,
            (tx) =>
              TxAdmissionsDB.tryInsert({
                txId: tx.txId,
                txCanonicalCbor: tx.txCbor,
                programMaterialSidecarCbor: tx.programMaterialSidecarCbor!,
                submitSource: "native",
              }),
            { concurrency: 1 },
          );
          yield* withHistoryWrite(
            sql.withTransaction(MempoolDB.insertMultipleCore(processed)),
          );
          for (let index = 0; index < processed.length; index += 1) {
            yield* sql`UPDATE ${sql(MempoolDB.tableName)}
              SET time_stamp_tz = ${timestamps[index]!}
              WHERE tx_id = ${processed[index]!.txId}`;
          }
        }),
      );

      const cacheHitsBefore = await Effect.runPromise(
        Metric.value(commitTxDeltaCacheHitCounter),
      );
      const fallbackBefore = await Effect.runPromise(
        Metric.value(commitTxDeltaFallbackDecodedCounter),
      );
      const output = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock,
        nodeConfig,
      });
      const cacheHitsAfter = await Effect.runPromise(
        Metric.value(commitTxDeltaCacheHitCounter),
      );
      const fallbackAfter = await Effect.runPromise(
        Metric.value(commitTxDeltaFallbackDecodedCounter),
      );
      expect(output.mempoolTxsCount).toBe(2);
      expect(cacheHitsAfter.count - cacheHitsBefore.count).toBe(0n);
      expect(fallbackAfter.count - fallbackBefore.count).toBe(2n);
      const active = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(active)).toBe(true);
      if (Option.isSome(active)) {
        expect(active.value.mempoolTxIds).toStrictEqual(
          processedInCanonicalBacklogOrder.slice(0, 2).map((tx) => tx.txId),
        );
      }
      expect(output.blockEndTimeMs).toBeGreaterThanOrEqual(
        timestamps[1]!.getTime(),
      );
    } finally {
      if (previousPageSize === undefined) {
        delete process.env.MEMPOOL_RETRIEVE_PAGE_SIZE;
      } else {
        process.env.MEMPOOL_RETRIEVE_PAGE_SIZE = previousPageSize;
      }
    }
  }, 900_000);
});

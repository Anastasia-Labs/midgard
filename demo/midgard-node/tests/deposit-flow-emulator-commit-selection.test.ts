import { describe, expect, it, vi } from "vitest";

import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
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
  DepositsDB,
  Effect,
  EMPTY_PROGRAM_MATERIAL_SIDECAR,
  EMULATOR_DEPLOYMENT_IDENTITY,
  expectHeaderRootsToMatchCandidate,
  fetchLatestCommittedBlock,
  fetchStateQueueSnapshotProgram,
  ForcedTransactionsDB,
  getStateQueueDatumEndTime,
  initializeNodeRuntime,
  initializeProtocol,
  Ledger,
  LedgerUtils,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  makeMidgardTxOutput,
  makeNodeConfigForFixture,
  makeOutRefCbor,
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
  Queue,
  type QueuedTx,
  Ref,
  resetActiveRuntimePaths,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runNodeDatabaseEffect,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  runSpeculativeWorkerWithInstruction,
  SDK,
  type SpeculativeCandidateSummary,
  type SpeculativeCommitWorkerInstruction,
  speculativeWorkerInputFromActiveJournal,
  SqlClient,
  StateQueueMutationLeasesDB,
  submitDepositAndRefreshBarriers,
  TxAdmissionsDB,
  TxUtils,
  type UserEventBarrierWatermarks,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";

describe.sequential("deposit flow emulator", () => {
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
        MPF_ENGINE: "overlay",
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

      // Exercise the root+aggregate fast path with no selected normal or
      // forced work; the full confirmed-ledger scan must remain untouched.
      const controlEndTimeMs = Math.max(
        Date.now(),
        baseCommit.blockEndTimeMs + 1,
      );
      const controlWatermarks: UserEventBarrierWatermarks = {
        depositMs: controlEndTimeMs,
        withdrawalMs: controlEndTimeMs,
        txOrderMs: controlEndTimeMs,
        refreshedAtMs: controlEndTimeMs,
      };
      const controlWorkerInput = await speculativeWorkerInputFromActiveJournal(
        controlWatermarks,
        canonicalSlotConfigForLucid(lucidService.api),
      );
      const controlScanBefore = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      let controlCandidate: SpeculativeCandidateSummary | undefined;
      const controlOutput = await Effect.runPromise(
        commitWorkerProgram(
          fixture.contracts,
          lucidService,
          controlWorkerInput,
          (candidate) => {
            controlCandidate = candidate;
            return Effect.succeed({
              type: "InvalidateSpeculativeCandidate",
              reason: "T1",
            } satisfies SpeculativeCommitWorkerInstruction);
          },
          nodeConfig,
          () => Effect.succeed(lucidService as any),
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
      expect([
        "NothingToCommitOutput",
        "SpeculativeCandidateInvalidatedOutput",
      ]).toContain(controlOutput.type);
      if (controlCandidate !== undefined) {
        expect(controlCandidate.expectedL2TransactionCount).toBe(0);
        expect(controlCandidate.expectedUserEventCounts).toEqual({
          deposits: 0,
          forcedTransactions: 0,
          withdrawals: 0,
        });
      }
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
          yield* sql.withTransaction(
            MempoolDB.insertMultipleCore([processedNormal]),
          );
          yield* sql`UPDATE ${sql(MempoolDB.tableName)}
            SET time_stamp_tz = ${eventTime}
            WHERE ${sql(TxUtils.Columns.TX_ID)} = ${normalTransfer.txId}`;
        }),
      );

      const forcedEncoding = await Effect.runPromise(
        ForcedTransactionsDB.encodeForcedInclusionValue({
          nativeTxCbor: forcedTransfer.txCbor,
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
      await runNodeDatabaseEffect(
        ForcedTransactionsDB.insertEntries([
          {
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
            [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]: "TxIsValid",
            [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
              MIDGARD_CONSENSUS_PROFILE.profileId,
            [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]:
              forcedTransfer.txCbor,
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
          },
        ]),
      );

      const eventWatermarkMs = eventTime.getTime() + 60_000;
      const watermarks: UserEventBarrierWatermarks = {
        depositMs: eventWatermarkMs,
        withdrawalMs: eventWatermarkMs,
        txOrderMs: eventWatermarkMs,
        refreshedAtMs: eventWatermarkMs,
      };
      const scanBefore = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        nodeConfig,
        onReady: (candidate) =>
          Effect.gen(function* () {
            yield* Effect.promise(() =>
              fixture.operatorLucid.awaitTx(baseCommit.submittedTxHash),
            );
            yield* Effect.promise(() =>
              runBlockConfirmation(globals, fixture.contracts, lucidService),
            );
            const stateQueueLeaseToken =
              yield* StateQueueMutationLeasesDB.acquire({
                holder: `hydration-regression-${payloadRootCheck}`,
              });
            const snapshot = yield* fetchStateQueueSnapshotProgram(
              lucidService.api,
              fixture.contracts.stateQueue,
              "commit_preflight",
            );
            const localFinalizationBlock = yield* Ref.get(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            );
            expect(candidate.expectedL2TransactionCount).toBe(1);
            return {
              type: "SubmitSpeculativeCandidate",
              confirmedBlock: snapshot.tailCommitBase.utxo,
              stateQueueLeaseToken,
              baseSnapshotId: snapshot.snapshotId,
              stateQueueHasUnmergedTail:
                snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
              localFinalizationBlock:
                localFinalizationBlock === ""
                  ? undefined
                  : localFinalizationBlock,
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });
      const speculativeLease = await runNodeDatabaseEffect(
        StateQueueMutationLeasesDB.retrieveActive(),
      );
      if (
        speculativeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER] ===
        `hydration-regression-${payloadRootCheck}`
      ) {
        await runNodeDatabaseEffect(
          StateQueueMutationLeasesDB.release(
            speculativeLease[StateQueueMutationLeasesDB.Columns.TOKEN],
          ),
        );
      }
      const scanAfter = await Effect.runPromise(
        Metric.value(confirmedLedgerFullScanCounter),
      );
      expect(scanAfter.count - scanBefore.count).toBeGreaterThan(0n);
      expect(speculative.output.type).toBe(
        "SubmittedAwaitingConfirmationOutput",
      );
      expect(speculative.candidate.expectedL2TransactionCount).toBe(1);

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
        SDK.ForcedInclusionTx,
      );
      expect(forcedSource.verdict).toBe("ForcedTxValid");

      const postState = await runNodeDatabaseEffect(
        materializeConfirmedLedgerSnapshot(active.value),
      );
      expect(postState.entries.length).toBeGreaterThan(0);
      expect(postState.root).toBe(speculative.candidate.roots.utxos);
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

      const sender = walletFromSeed(
        "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
        { network: "Preprod" },
      );
      const destination = walletFromSeed(
        "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
        { network: "Preprod" },
      );
      const sourceUtxos: NodeUtxo[] = [];
      const sourceLedger: LedgerUtils.Entry[] = [];
      for (let index = 0; index < 8; index += 1) {
        const txHash = (index + 1).toString(16).padStart(64, "0");
        const outrefCbor = makeOutRefCbor(txHash, 0);
        const outputCbor = Buffer.from(
          makeMidgardTxOutput(
            CML.Address.from_bech32(sender.address),
            CML.Value.from_coin(10_000_000n),
          ).to_cbor_bytes(),
        );
        sourceUtxos.push({
          txHash,
          outputIndex: 0,
          outrefCbor,
          outputCbor,
          address: sender.address,
          assets: { lovelace: 10_000_000n },
        });
        sourceLedger.push({
          [LedgerUtils.Columns.TX_ID]: Buffer.from(txHash, "hex"),
          [LedgerUtils.Columns.OUTREF]: outrefCbor,
          [LedgerUtils.Columns.OUTPUT]: outputCbor,
          [LedgerUtils.Columns.ADDRESS]: sender.address,
        });
      }
      const baseNodeConfig = await makeNodeConfigForFixture(fixture);
      const nodeConfig: NodeConfigDep = {
        ...baseNodeConfig,
        GENESIS_UTXOS: sourceUtxos.map(
          ({ txHash, outputIndex, address, assets }) => ({
            txHash,
            outputIndex,
            address,
            assets,
          }),
        ),
      };
      const built = await Promise.all(
        sourceUtxos.map((source) =>
          buildTransferTx({
            senderAddress: sender.address,
            destinationAddress: destination.address,
            signer: CML.PrivateKey.from_bech32(sender.paymentKey),
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
          yield* MempoolLedgerDB.insert(sourceLedger);
          yield* sql.withTransaction(MempoolDB.insertMultipleCore(processed));
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

  it("builds N+1 before N confirmation and submits the exact ready candidate on the direct wake path", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
      });

      await advanceEmulatorPastUnixTime(fixture, blockN.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const { watermarks } = await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 13_000_000n,
        projectToLedger: false,
      });

      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: () =>
          Effect.gen(function* () {
            yield* Effect.promise(() =>
              fixture.operatorLucid.awaitTx(blockN.submittedTxHash),
            );
            yield* Effect.promise(() =>
              runBlockConfirmation(globals, fixture.contracts, lucidService),
            );
            const leaseToken = yield* StateQueueMutationLeasesDB.acquire({
              holder: "speculative-emulator-happy",
            });
            const snapshot = yield* fetchStateQueueSnapshotProgram(
              lucidService.api,
              fixture.contracts.stateQueue,
              "commit_preflight",
            );
            const localFinalizationBlock = yield* Ref.get(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            );
            return {
              type: "SubmitSpeculativeCandidate",
              confirmedBlock: snapshot.tailCommitBase.utxo,
              stateQueueLeaseToken: leaseToken,
              baseSnapshotId: snapshot.snapshotId,
              stateQueueHasUnmergedTail:
                snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
              localFinalizationBlock:
                localFinalizationBlock === ""
                  ? undefined
                  : localFinalizationBlock,
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });

      expect(speculative.candidate.baseHeaderHash).toBe(
        blockN.submittedHeaderHash,
      );
      expect(speculative.candidate.expectedUserEventCounts.deposits).toBe(1);
      expect(speculative.lucidAcquisitions).toBe(1);
      expect(speculative.output.type).toBe(
        "SubmittedAwaitingConfirmationOutput",
      );
      if (speculative.output.type !== "SubmittedAwaitingConfirmationOutput") {
        throw new Error(
          `Expected speculative submission, got ${speculative.output.type}`,
        );
      }
      expect(speculative.output.submittedUtxosRoot).toBe(
        speculative.candidate.roots.utxos,
      );
      expect(speculative.output.speculativeExecution).toEqual({
        candidateId: speculative.candidate.candidateId,
        baseHydrationPassesBeforeReady: 1,
        mpfProcessingPassesBeforeReady: 1,
        baseHydrationPassesAfterReady: 0,
        mpfProcessingPassesAfterReady: 0,
      });

      // Lucid 0.6's Emulator correctly hides a confirmed UTxO as soon as a
      // pending transaction spends it. Assert the public transaction states
      // instead of depending on the old spent-ledger visibility bug.
      expect(
        (await fixture.operatorLucid.transactionStatus(blockN.submittedTxHash))
          .status,
      ).toBe("confirmed");
      expect(
        (
          await fixture.operatorLucid.transactionStatus(
            speculative.output.submittedTxHash,
          )
        ).status,
      ).toBe("pending");

      const activeJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(activeJournal)).toBe(true);
      if (Option.isNone(activeJournal)) {
        throw new Error("Expected submitted N+1 journal before confirmation");
      }
      expect(
        activeJournal.value[
          PendingBlockFinalizationsDB.Columns.HEADER_HASH
        ].toString("hex"),
      ).toBe(speculative.output.submittedHeaderHash);
      expect(
        activeJournal.value[
          PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
        ]?.toString("hex"),
      ).toBe(speculative.output.submittedTxHash);
      expect(
        activeJournal.value[PendingBlockFinalizationsDB.Columns.STATUS],
      ).toBe(
        PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
      );
      const durableSubmittedHeader = Data.from(
        activeJournal.value[
          PendingBlockFinalizationsDB.Columns.HEADER_CBOR
        ].toString("hex"),
        SDK.Header as never,
      ) as SDK.Header;
      expectHeaderRootsToMatchCandidate(
        durableSubmittedHeader,
        speculative.candidate,
      );
      const independentlyMaterializedPostState = await runNodeDatabaseEffect(
        materializeConfirmedLedgerSnapshot(activeJournal.value),
      );
      expect(independentlyMaterializedPostState.root).toBe(
        speculative.candidate.roots.utxos,
      );

      await fixture.operatorLucid.awaitTx(speculative.output.submittedTxHash);
      const latestBlock = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const latestHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(latestBlock.datum),
      );
      expectHeaderRootsToMatchCandidate(latestHeader, speculative.candidate);

      const directWake = await Effect.runPromise(
        Queue.poll(globals.COMMIT_SUBMIT_WAKE_QUEUE),
      );
      expect(Option.isSome(directWake)).toBe(true);
      if (Option.isSome(directWake)) {
        expect(directWake.value.confirmedHeaderHash).toBe(
          blockN.submittedHeaderHash,
        );
      }
      const submittedCandidateDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(
          new Date(speculative.candidate.endTimeMs),
        ),
      );
      const submittedCandidateDeposit = submittedCandidateDeposits.find(
        (entry) =>
          entry[DepositsDB.Columns.INCLUSION_TIME].getTime() >
          blockN.blockEndTimeMs,
      );
      expect(submittedCandidateDeposit?.[DepositsDB.Columns.STATUS]).toBe(
        DepositsDB.Status.Projected,
      );
      expect(
        submittedCandidateDeposit?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
      ).toBeNull();
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);
});

import { describe, expect, it, vi } from "vitest";

import {
  advanceEmulatorPastLatestBlockEndTime,
  buildUnsignedDepositTxFromFundingContextProgram,
  CML,
  configureEmulatorDaRuntimeManifest,
  DA_TRANSPORT_LIMITS_V1,
  DaPayloadsDB,
  Data,
  Database,
  DepositDraftDatumWithWitnessSchema,
  DepositsDB,
  Effect,
  expectedAuthenticatedEventRoot,
  extractDraftDepositOutput,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  Globals,
  initializeNodeRuntime,
  initializeProtocol,
  LucidService,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  makeNodeConfigForFixture,
  MempoolLedgerDB,
  MidgardContracts,
  NodeConfig,
  PendingBlockFinalizationsDB,
  projectDepositsToMempoolLedger,
  reconcileVisibleDepositUTxOs,
  Ref,
  resetActiveRuntimePaths,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  SDK,
  seedLatestLocalBlockBoundaryOnStartup,
  submitDepositWithDiagnostics,
  submitSignedDepositTxWithHarnessWorkaround,
  toUnit,
  unwrapDaPayloadV1,
  UserEventsUtils,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";

describe.sequential("deposit flow emulator", () => {
  it("builds an unsigned deposit tx from explicit external wallet context that the user wallet can sign and submit", async () => {
    const fixture = await makeFixture();
    await initializeProtocol(fixture);

    const fundingAddress = await fixture.depositorLucid.wallet().address();
    const depositAddress = fixture.contracts.deposit.spendingScriptAddress;
    const fundingUtxos = await fixture.depositorLucid.wallet().getUtxos();
    const config = {
      l2Address: fundingAddress,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    } as const;

    const built = await Effect.runPromise(
      buildUnsignedDepositTxFromFundingContextProgram(
        fixture.depositorLucid,
        fixture.contracts,
        {
          ...config,
          fundingAddress,
          fundingUtxos,
          referenceScripts: fixture.referenceScripts.deposit,
        },
      ),
    );
    const builtTx = CML.Transaction.from_cbor_hex(built.unsignedTxCbor);
    const builtDepositOutput = extractDraftDepositOutput({
      tx: builtTx,
      depositAddress,
      depositPolicyId: fixture.contracts.deposit.policyId,
    });
    const expectedWitnessHash = builtDepositOutput.datum.witness;
    const signed = await Effect.runPromise(
      fixture.depositorLucid
        .fromTx(built.unsignedTxCbor)
        .sign.withWallet()
        .completeProgram(),
    );
    const txHash = await submitSignedDepositTxWithHarnessWorkaround({
      lucid: fixture.depositorLucid,
      signedTx: signed,
      expectedWitnessHash,
    });
    const depositUtxos = await fixture.depositorLucid.utxosAt(depositAddress);
    const deposited = depositUtxos.find(
      (utxo) => (utxo.assets[builtDepositOutput.depositAuthUnit] ?? 0n) === 1n,
    );

    expect(built.unsignedTxCbor).toMatch(/^[0-9a-f]+$/);
    expect(txHash).toEqual(signed.toHash());
    expect(deposited).toBeDefined();
    expect(deposited!.address).toEqual(depositAddress);
    expect(deposited!.assets.lovelace).toEqual(config.lovelace);
    expect(deposited!.assets[builtDepositOutput.depositAuthUnit]).toEqual(1n);

    const depositedDatum = Data.from(
      deposited!.datum ?? "",
      DepositDraftDatumWithWitnessSchema,
    );
    expect(depositedDatum.witness).toEqual(expectedWitnessHash);
    expect(depositedDatum.inclusion_time).toEqual(
      builtDepositOutput.datum.inclusion_time,
    );
  });

  // 900s leaves headroom for the full real-contract workflow. Protocol
  // bring-up publishes the 153-target reference-script roster in 39 planned
  // batches (with size-driven splits where required).
  it("commits a realistic deposit-only block through the live worker core and real scheduler refresh path", async () => {
    await resetActiveRuntimePaths();
    await initializeNodeRuntime();
    // The local-finalization recovery path seeds the libp2p DA publication
    // outbox, which requires the deployment runtime manifest since canonical
    // V1 made libp2p DA mandatory for block finalization.
    await configureEmulatorDaRuntimeManifest();

    const fixture = await makeFixture();
    await initializeProtocol(fixture);

    const lucidService = await makeLucidRuntimeService(fixture);
    const schedulerBeforeCommit = await fetchSchedulerDatum(fixture);
    expect(schedulerBeforeCommit).toEqual(SDK.INITIAL_SCHEDULER_DATUM);

    await advanceEmulatorPastLatestBlockEndTime(fixture);

    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const l2Address = await fixture.depositorLucid.wallet().address();
    const depositTxHash = await submitDepositWithDiagnostics(fixture, {
      l2Address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });
    expect(depositTxHash).toHaveLength(64);

    const fetchedDepositUtxos = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.deposit.spendingScriptAddress,
        eventPolicyId: fixture.contracts.deposit.policyId,
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);

    const depositUtxo = fetchedDepositUtxos[0]!;
    const depositAuthUnit = toUnit(
      fixture.contracts.deposit.policyId,
      depositUtxo.assetName,
    );
    const inclusionSlot =
      fixture.operatorLucid.unixTimeToSlot(
        Number(depositUtxo.datum.inclusion_time),
      ) + 1;
    fixture.emulator.awaitSlot(inclusionSlot);

    vi.setSystemTime(new Date(fixture.emulator.now()));

    const depositEntries = await runNodeDatabaseEffect(
      DepositsDB.retrieveAllEntries(),
    );
    expect(depositEntries).toHaveLength(0);

    const utxosBeforeProjection = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(utxosBeforeProjection.utxoCount).toEqual(0);

    const globalsBeforeCommit = await makeGlobalsService();
    await runNodeCommandProgram(
      reconcileVisibleDepositUTxOs({
        inclusionTimeUpperBound: BigInt(Date.now()),
      }),
      { fixture, lucidService, globals: globalsBeforeCommit },
    );
    await runNodeCommandProgram(projectDepositsToMempoolLedger, {
      fixture,
      lucidService,
      globals: globalsBeforeCommit,
    });

    const rawUtxosAfterBackgroundProjection = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveByAddress(l2Address),
    );
    expect(rawUtxosAfterBackgroundProjection).toHaveLength(1);

    const spendableUtxosAfterBackgroundProjection = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(spendableUtxosAfterBackgroundProjection.utxoCount).toEqual(0);

    const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const commitOutput = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: latestBlockBeforeCommit,
    });

    expect(commitOutput.mempoolTxsCount).toEqual(0);

    const depositEntriesAfterSubmission = await runNodeDatabaseEffect(
      DepositsDB.retrieveAllEntries(),
    );
    expect(depositEntriesAfterSubmission).toHaveLength(1);
    const depositEntry = depositEntriesAfterSubmission[0]!;
    expect(
      depositEntry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH]?.toString("hex"),
    ).toEqual(depositTxHash);
    expect(depositEntry[DepositsDB.Columns.STATUS]).toEqual(
      DepositsDB.Status.Projected,
    );
    expect(depositEntry[DepositsDB.Columns.PROJECTED_HEADER_HASH]).toBeNull();

    const projectedUtxosBeforeConfirmation = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedUtxosBeforeConfirmation.utxoCount).toEqual(0);

    const activePendingAfterSubmission = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    expect(activePendingAfterSubmission._tag).toBe("Some");
    if (activePendingAfterSubmission._tag !== "Some") {
      throw new Error("Expected an active pending-finalization journal record");
    }
    expect(
      activePendingAfterSubmission.value[
        PendingBlockFinalizationsDB.Columns.STATUS
      ],
    ).toBe(
      PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
    );

    const expectedDepositsRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      depositEntriesAfterSubmission.map((entry) => ({
        key: entry[UserEventsUtils.Columns.ID],
        value: entry[UserEventsUtils.Columns.INFO],
      })),
    );

    await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
    const restartedGlobals = await makeGlobalsService();

    const preConfirmationLocalFinalizationPending = await Effect.runPromise(
      Ref.get(restartedGlobals.LOCAL_FINALIZATION_PENDING),
    );
    const preConfirmationRecoverableBlock = await Effect.runPromise(
      Ref.get(restartedGlobals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
    );
    expect(preConfirmationLocalFinalizationPending).toBe(false);
    expect(preConfirmationRecoverableBlock).toBe("");

    await runBlockConfirmation(
      restartedGlobals,
      fixture.contracts,
      lucidService,
    );

    const observedPendingFinalization = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    expect(observedPendingFinalization._tag).toBe("Some");
    if (observedPendingFinalization._tag !== "Some") {
      throw new Error(
        "Expected the pending-finalization journal to remain active until local recovery completes",
      );
    }
    expect(
      observedPendingFinalization.value[
        PendingBlockFinalizationsDB.Columns.STATUS
      ],
    ).toBe(PendingBlockFinalizationsDB.Status.ObservedWaitingStability);

    const localFinalizationPendingAfterObservation = await Effect.runPromise(
      Ref.get(restartedGlobals.LOCAL_FINALIZATION_PENDING),
    );
    const recoverableConfirmedBlockAfterObservation = await Effect.runPromise(
      Ref.get(restartedGlobals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
    );
    const localBoundaryAfterObservation = await Effect.runPromise(
      Ref.get(restartedGlobals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    );
    expect(localFinalizationPendingAfterObservation).toBe(true);
    expect(recoverableConfirmedBlockAfterObservation).not.toBe("");
    expect(localBoundaryAfterObservation).toBe(commitOutput.blockEndTimeMs);

    const projectedUtxosAfterConfirmation = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedUtxosAfterConfirmation.utxoCount).toEqual(1);
    expect(projectedUtxosAfterConfirmation.totals.lovelace).toEqual(
      12_000_000n,
    );
    expect(projectedUtxosAfterConfirmation.utxos[0]?.address).toEqual(
      l2Address,
    );
    expect(
      projectedUtxosAfterConfirmation.utxos[0]?.assets[depositAuthUnit],
    ).toBeUndefined();
    expect(projectedUtxosAfterConfirmation.utxos[0]?.datum).toBeUndefined();

    const recoveryOutput = await runLocalFinalizationRecoveryWorker(
      restartedGlobals,
      fixture.contracts,
      lucidService,
    );
    expect(recoveryOutput.type).toBe(
      "SuccessfulLocalFinalizationRecoveryOutput",
    );

    const latestBlockAfterCommit = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const latestHeader = await Effect.runPromise(
      SDK.getHeaderV1FromStateQueueDatum(latestBlockAfterCommit.datum),
    );
    const latestHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeaderV1(latestHeader),
    );
    const daPayloadAfterRecovery = await runNodeDatabaseEffect(
      DaPayloadsDB.retrieveByHeaderHash(Buffer.from(latestHeaderHash, "hex")),
    );
    expect(daPayloadAfterRecovery._tag).toBe("Some");
    if (daPayloadAfterRecovery._tag !== "Some") {
      throw new Error("Expected a DA payload row for the finalized block");
    }
    const daPayloadRow = daPayloadAfterRecovery.value;
    const daPayloadEnvelope = await unwrapDaPayloadV1(
      daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_CBOR],
      { maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes },
    );
    const daPayload = SDK.decodeDaPayloadV1(daPayloadEnvelope.innerBytes);
    expect(daPayload.block_body.header_hash).toEqual(latestHeaderHash);
    expect(
      daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_SHA256].toString("hex"),
    ).toEqual(
      SDK.daPayloadHashHex(daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_CBOR]),
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.UTXOS_ROOT]).toEqual(
      latestHeader.utxosRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.TRANSACTIONS_ROOT]).toEqual(
      latestHeader.transactionsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]).toEqual(
      latestHeader.forcedTransactionsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.DEPOSITS_ROOT]).toEqual(
      latestHeader.depositsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.WITHDRAWALS_ROOT]).toEqual(
      latestHeader.withdrawalsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]).toEqual(
      latestHeader.transitionTraceRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]).toEqual(
      latestHeader.eventToStepRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]).toEqual(
      latestHeader.totalEventCount,
    );
    const schedulerAfterCommit = await fetchSchedulerDatum(fixture);
    const depositEntriesAfterCommit = await runNodeDatabaseEffect(
      DepositsDB.retrieveAllEntries(),
    );
    expect(depositEntriesAfterCommit).toHaveLength(1);
    expect(
      depositEntriesAfterCommit[0]?.[
        DepositsDB.Columns.PROJECTED_HEADER_HASH
      ]?.toString("hex"),
    ).toEqual(latestHeaderHash);
    expect(depositEntriesAfterCommit[0]?.[DepositsDB.Columns.STATUS]).toEqual(
      DepositsDB.Status.Projected,
    );

    const activePendingFinalization = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    expect(activePendingFinalization._tag).toBe("None");

    const localBoundaryAfterRecovery = await Effect.runPromise(
      Ref.get(restartedGlobals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    );
    expect(localBoundaryAfterRecovery).toBe(commitOutput.blockEndTimeMs);

    const coldStartGlobals = await makeGlobalsService();
    const coldStartNodeConfig = await makeNodeConfigForFixture(fixture);
    await Effect.runPromise(
      seedLatestLocalBlockBoundaryOnStartup.pipe(
        Effect.provideService(Globals, coldStartGlobals),
        Effect.provideService(LucidService, lucidService as any),
        Effect.provideService(MidgardContracts, fixture.contracts as any),
        Effect.provideService(NodeConfig, coldStartNodeConfig),
        Effect.provide(Database.layer),
      ),
    );
    const coldStartBoundary = await Effect.runPromise(
      Ref.get(coldStartGlobals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    );
    expect(coldStartBoundary).toBe(commitOutput.blockEndTimeMs);

    expect(latestBlockAfterCommit.utxo.txHash).toEqual(
      commitOutput.submittedTxHash,
    );
    expect(latestHeader.depositsRoot).toEqual(expectedDepositsRoot);
    expect(schedulerAfterCommit).not.toEqual(SDK.INITIAL_SCHEDULER_DATUM);
    expect(
      schedulerAfterCommit === SDK.INITIAL_SCHEDULER_DATUM
        ? undefined
        : typeof schedulerAfterCommit === "object" &&
            schedulerAfterCommit !== null &&
            "ActiveOperator" in schedulerAfterCommit
          ? schedulerAfterCommit.ActiveOperator.operator
          : undefined,
    ).toEqual(fixture.operatorKeyHash);
  }, 900_000);
});

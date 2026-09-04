import { describe, expect, it, vi } from "vitest";

import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  assetsToValue,
  attestQueuedStateQueueHeader,
  BlocksDB,
  buildTransferTx,
  CML,
  commitConfirmRecoverAndMerge,
  concludePayoutProgram,
  configureEmulatorDaRuntimeManifest,
  Data,
  Database,
  decodeNodeUtxo,
  Effect,
  encodeMidgardCekProgramMaterialSidecar,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  fetchWithdrawalsOnceProgram,
  findUtxoWithUnit,
  ImmutableDB,
  initializeNodeRuntime,
  initializePayoutProgram,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  MempoolDB,
  MempoolLedgerDB,
  mergeMaturityWindow,
  NodeConfig,
  paymentCredentialOf,
  payoutStatusProgram,
  processedTxFromValidatedTx,
  type QueuedTx,
  randomUUID,
  reserveUtxosProgram,
  resetActiveRuntimePaths,
  resolveEventSettlementProofProgram,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runMergeUntilMerged,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  SDK,
  signWithdrawalBody,
  SqlClient,
  stateQueueFetchConfig,
  submitDepositWithDiagnostics,
  submitWithdrawalWithDiagnostics,
  toUnit,
  TxAdmissionsDB,
  utxosProgram,
  walletFromSeed,
  WithdrawalsDB,
  withdrawalStatusProgram,
  WriteBehindLive,
} from "./deposit-flow-emulator-shared.js";

describe.sequential("deposit flow emulator", () => {
  it("merges a committed deposit-only block into confirmed state and spawns settlement with real contracts", async () => {
    await resetActiveRuntimePaths();
    await initializeNodeRuntime();
    await configureEmulatorDaRuntimeManifest();

    const fixture = await makeFixture();
    await initializeProtocol(fixture);

    const lucidService = await makeLucidRuntimeService(fixture);
    await advanceEmulatorPastLatestBlockEndTime(fixture);

    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const l2Address = await fixture.depositorLucid.wallet().address();
    await submitDepositWithDiagnostics(fixture, {
      l2Address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });

    const fetchedDepositUtxos = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.deposit.spendingScriptAddress,
        eventPolicyId: fixture.contracts.deposit.policyId,
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);

    const inclusionSlot =
      fixture.operatorLucid.unixTimeToSlot(
        Number(fetchedDepositUtxos[0]!.datum.inclusion_time),
      ) + 1;
    fixture.emulator.awaitSlot(inclusionSlot);
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const commitOutput = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: latestBlockBeforeCommit,
    });

    await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);

    const globalsAfterCommit = await makeGlobalsService();
    await runBlockConfirmation(
      globalsAfterCommit,
      fixture.contracts,
      lucidService,
    );
    await runLocalFinalizationRecoveryWorker(
      globalsAfterCommit,
      fixture.contracts,
      lucidService,
    );

    const sortedStateQueueBeforeMerge = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    expect(sortedStateQueueBeforeMerge).toHaveLength(2);

    const queuedBlockBeforeMerge = sortedStateQueueBeforeMerge[1]!;
    expect(
      Object.keys(sortedStateQueueBeforeMerge[0]!.utxo.assets).filter(
        (unit) => unit !== "lovelace",
      ),
    ).toHaveLength(1);
    expect(
      Object.keys(queuedBlockBeforeMerge.utxo.assets).filter(
        (unit) => unit !== "lovelace",
      ),
    ).toHaveLength(1);
    const queuedHeaderBeforeMerge = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(queuedBlockBeforeMerge.datum),
    );
    const queuedHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(queuedHeaderBeforeMerge),
    );
    expect(queuedBlockBeforeMerge.datum.key).toEqual({
      Key: { key: queuedHeaderHash },
    });
    expect(queuedHeaderBeforeMerge.depositsRoot).not.toEqual(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );

    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals: globalsAfterCommit,
      headerHash: queuedHeaderHash,
    });

    const confirmedBeforeMerge = await Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(
        sortedStateQueueBeforeMerge[0]!.datum,
      ),
    );
    expect(confirmedBeforeMerge.link).not.toEqual("Empty");

    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(
        fixture.operatorLucid,
        Number(queuedHeaderBeforeMerge.endTime),
      ).readyAfterUnixTime,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const mergeResult = await runMergeUntilMerged({
      fixture,
      lucidService,
      globals: globalsAfterCommit,
    });
    expect(mergeResult.postMergeSnapshot.topology.parsedNodeCount).toBe(1);

    const sortedStateQueueAfterMerge = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    expect(sortedStateQueueAfterMerge).toHaveLength(1);

    const confirmedAfterMerge = await Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(
        sortedStateQueueAfterMerge[0]!.datum,
      ),
    );
    expect(confirmedAfterMerge.link).toEqual("Empty");
    expect(confirmedAfterMerge.data.headerHash).toEqual(queuedHeaderHash);
    expect(confirmedAfterMerge.data.prevHeaderHash).toEqual(
      confirmedBeforeMerge.data.headerHash,
    );
    expect(confirmedAfterMerge.data.utxoRoot).toEqual(
      queuedHeaderBeforeMerge.utxosRoot,
    );
    expect(confirmedAfterMerge.data.startTime).toEqual(
      confirmedBeforeMerge.data.startTime,
    );
    expect(confirmedAfterMerge.data.endTime).toEqual(
      queuedHeaderBeforeMerge.endTime,
    );

    const burnedHeaderUnit = toUnit(
      fixture.contracts.stateQueue.policyId,
      queuedBlockBeforeMerge.assetName,
    );
    const burnedHeaderUtxos = await fixture.operatorLucid.utxosAtWithUnit(
      fixture.contracts.stateQueue.spendingScriptAddress,
      burnedHeaderUnit,
    );
    expect(burnedHeaderUtxos).toHaveLength(0);

    const settlementUnit = toUnit(
      fixture.contracts.settlement.policyId,
      queuedHeaderHash,
    );
    const settlementUtxos = await fixture.operatorLucid.utxosAtWithUnit(
      fixture.contracts.settlement.spendingScriptAddress,
      settlementUnit,
    );
    expect(settlementUtxos).toHaveLength(1);
    expect(settlementUtxos[0]!.assets[settlementUnit]).toEqual(1n);

    const settlementDatum = Data.from(
      settlementUtxos[0]!.datum!,
      SDK.SettlementDatum,
    );
    expect(settlementDatum).toEqual({
      deposits_root: queuedHeaderBeforeMerge.depositsRoot,
      withdrawals_root: queuedHeaderBeforeMerge.withdrawalsRoot,
      forced_transactions_root: queuedHeaderBeforeMerge.forcedTransactionsRoot,
      transactions_root: queuedHeaderBeforeMerge.transactionsRoot,
      resolution_claim: null,
    });
  }, 900_000);

  it("runs deposit, reserve absorption, withdrawal commitment, and payout to conclusion", async () => {
    await resetActiveRuntimePaths();
    await initializeNodeRuntime();
    await configureEmulatorDaRuntimeManifest();

    const fixture = await makeFixture();
    await initializeProtocol(fixture);
    const lucidService = await makeLucidRuntimeService(fixture);
    const globals = await makeGlobalsService();
    await advanceEmulatorPastLatestBlockEndTime(fixture);

    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const schedulerBeforeJourneyCommit = await fetchSchedulerDatum(fixture);
    expect(schedulerBeforeJourneyCommit).toEqual(SDK.INITIAL_SCHEDULER_DATUM);

    const l2Address = await fixture.depositorLucid.wallet().address();
    await submitDepositWithDiagnostics(fixture, {
      l2Address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });

    const fetchedDepositUtxos = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.deposit.spendingScriptAddress,
        eventPolicyId: fixture.contracts.deposit.policyId,
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);
    const depositUtxo = fetchedDepositUtxos[0]!;
    fixture.emulator.awaitSlot(
      fixture.operatorLucid.unixTimeToSlot(
        Number(depositUtxo.datum.inclusion_time),
      ) + 1,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const depositBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
    });
    const schedulerAfterJourneyCommit = await fetchSchedulerDatum(fixture);
    expect(schedulerAfterJourneyCommit).not.toEqual(
      schedulerBeforeJourneyCommit,
    );
    expect(
      typeof schedulerAfterJourneyCommit === "object" &&
        schedulerAfterJourneyCommit !== null &&
        "ActiveOperator" in schedulerAfterJourneyCommit
        ? schedulerAfterJourneyCommit.ActiveOperator.operator
        : undefined,
    ).toEqual(fixture.operatorKeyHash);
    const emptyProtocolRoot = SDK.EMPTY_MERKLE_TREE_ROOT;
    const expectedDepositRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      [{ key: depositUtxo.idCbor, value: depositUtxo.infoCbor }],
    );
    expect(depositBlock.queuedHeader.depositsRoot).not.toEqual(
      emptyProtocolRoot,
    );
    expect(depositBlock.queuedHeader.depositsRoot).toEqual(expectedDepositRoot);
    expect(depositBlock.queuedHeader.withdrawalsRoot).toEqual(
      emptyProtocolRoot,
    );
    const depositEventIdHex = depositUtxo.idCbor.toString("hex");
    const depositResolution = await runNodeCommandProgram(
      resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId: Buffer.from(depositUtxo.idCbor),
      }),
      { fixture, lucidService, globals },
    );
    expect(depositResolution.root).toEqual(expectedDepositRoot);
    expect(depositResolution.settlementRefInput.txHash).toEqual(
      depositBlock.settlementUtxo.txHash,
    );
    await ensureSeparateCollateralUtxo(fixture.operatorLucid);
    const absorb = await runNodeCommandProgram(
      absorbConfirmedDepositToReserveProgram({ eventId: depositEventIdHex }),
      { fixture, lucidService, globals },
    );
    expect(absorb.details.depositOutRef).toEqual(
      `${depositUtxo.utxo.txHash}#${depositUtxo.utxo.outputIndex.toString()}`,
    );
    const reserveAfterAbsorb = (
      await fixture.operatorLucid.utxosAt(
        fixture.contracts.reserve.spendingScriptAddress,
      )
    ).find((utxo) => utxo.assets.lovelace === 12_000_000n);
    if (reserveAfterAbsorb === undefined) {
      throw new Error(
        "Deposit absorption did not create a 12 ADA reserve UTxO",
      );
    }
    const reserveSummary = await runNodeCommandProgram(reserveUtxosProgram, {
      fixture,
      lucidService,
      globals,
    });
    expect(reserveSummary.utxos).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          outRef: `${reserveAfterAbsorb.txHash}#${reserveAfterAbsorb.outputIndex.toString()}`,
          datum: "NoDatum",
          hasReferenceScript: false,
          spendable: true,
        }),
      ]),
    );

    const projectedDepositUtxos = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedDepositUtxos.utxoCount).toEqual(1);
    const projectedDepositEntries = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveSpendableByAddress(l2Address),
    );
    expect(projectedDepositEntries).toHaveLength(1);
    const projectedDepositEntry = projectedDepositEntries[0]!;
    const l2TransferSource = decodeNodeUtxo({
      outref:
        projectedDepositEntry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
      outputCbor:
        projectedDepositEntry[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
    });
    expect(
      `${l2TransferSource.txHash}#${l2TransferSource.outputIndex.toString()}`,
    ).toEqual(
      `${projectedDepositUtxos.utxos[0]!.txHash}#${projectedDepositUtxos.utxos[0]!.outputIndex.toString()}`,
    );
    const withdrawalPrivateKey = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Custom",
      }).paymentKey,
    );
    const l2RecipientAddress = await fixture.referenceScriptsLucid
      .wallet()
      .address();
    const builtL2Transfer = await buildTransferTx({
      senderAddress: l2Address,
      destinationAddress: l2RecipientAddress,
      signer: withdrawalPrivateKey,
      selectedInputs: [l2TransferSource],
      requestedAssets: { lovelace: 2_000_000n },
      networkId: 0n,
    });
    const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar(
      [],
    );
    const admittedL2Transfer = await runNodeDatabaseEffect(
      TxAdmissionsDB.admit({
        txId: builtL2Transfer.txId,
        txCanonicalCbor: builtL2Transfer.txCbor,
        programMaterialSidecarCbor,
        submitSource: "native",
        currentBacklog: 0n,
        maxBacklog: 1,
      }),
    );
    expect(admittedL2Transfer.kind).toBe("new");
    expect(admittedL2Transfer.entry[TxAdmissionsDB.Columns.STATUS]).toBe(
      TxAdmissionsDB.Status.Queued,
    );

    const l2TransferLeaseOwner = `deposit-flow:${randomUUID()}`;
    const claimL2TransferOnce = () =>
      runNodeDatabaseEffect(
        TxAdmissionsDB.claimBatchLease({
          limit: 1,
          leaseOwner: l2TransferLeaseOwner,
          leaseDurationMs: 30_000,
        }),
      );
    // The node's own claim loop treats an empty claim as an ordinary tick
    // outcome and re-claims on its next tick (see the `claimedLeases.length
    // === 0` branch in src/fibers/tx-queue-processor.ts), so requiring the
    // very first attempt to succeed asserts more than the production contract
    // guarantees and made this journey intermittently red under load. Poll
    // under the same lease owner for a bounded window instead. An admission
    // that never becomes claimable is still a hard failure, and it reports the
    // durable row state so a genuine liveness defect cannot hide here.
    // performance.now() is deliberate: Date is faked for this test.
    const claimDeadlineMs = 30_000;
    const claimStartedAt = performance.now();
    let claimedL2Transfers = await claimL2TransferOnce();
    let claimAttempts = 1;
    while (
      claimedL2Transfers.length === 0 &&
      performance.now() - claimStartedAt < claimDeadlineMs
    ) {
      await new Promise((resolve) => setTimeout(resolve, 100));
      claimedL2Transfers = await claimL2TransferOnce();
      claimAttempts += 1;
    }
    if (claimedL2Transfers.length === 0) {
      const admissionRows = await runNodeDatabaseEffect(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql`
            SELECT
              encode(tx_id, 'hex') AS tx_id,
              status::text AS status,
              arrival_seq::text AS arrival_seq,
              lease_owner,
              next_attempt_at,
              NOW() AS db_now,
              (next_attempt_at <= NOW()) AS claimable
            FROM ${sql(TxAdmissionsDB.tableName)}
            ORDER BY arrival_seq
          `;
        }),
      );
      throw new Error(
        `Durable admission never became claimable after ${claimAttempts.toString()} attempts across ${claimDeadlineMs.toString()}ms: expectedTxId=${builtL2Transfer.txId.toString("hex")} rows=${JSON.stringify(admissionRows)}`,
      );
    }
    expect(claimedL2Transfers).toHaveLength(1);
    const loadedL2Transfers = await runNodeDatabaseEffect(
      TxAdmissionsDB.loadClaimedPayloads({
        claimed: claimedL2Transfers,
        leaseOwner: l2TransferLeaseOwner,
      }),
    );
    expect(loadedL2Transfers).toHaveLength(1);
    const queuedL2Transfer: QueuedTx = {
      txId: loadedL2Transfers[0]![TxAdmissionsDB.Columns.TX_ID],
      txCbor: loadedL2Transfers[0]![TxAdmissionsDB.Columns.TX_CANONICAL_CBOR],
      programMaterialSidecarCbor:
        loadedL2Transfers[0]![
          TxAdmissionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
        ],
      arrivalSeq: loadedL2Transfers[0]![TxAdmissionsDB.Columns.ARRIVAL_SEQ],
      createdAt: loadedL2Transfers[0]![TxAdmissionsDB.Columns.FIRST_SEEN_AT],
    };
    const phaseA = await Effect.runPromise(
      runPhaseAValidation([queuedL2Transfer], {
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    expect(phaseA.rejected).toEqual([]);
    expect(phaseA.accepted).toHaveLength(1);
    const preTransferLedgerEntries = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveSpendable,
    );
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map(
          preTransferLedgerEntries.map((entry) => [
            entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
            entry[MempoolLedgerDB.Columns.OUTPUT],
          ]),
        ),
        {
          nowCardanoSlotNo: BigInt(fixture.operatorLucid.currentSlot()),
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        },
      ),
    );
    expect(phaseB.rejected).toEqual([]);
    expect(phaseB.accepted).toHaveLength(1);
    const processedL2Transfers = phaseB.accepted.map(
      processedTxFromValidatedTx,
    );
    await Effect.runPromise(
      Effect.scoped(
        TxAdmissionsDB.markAccepted({
          rows: claimedL2Transfers,
          leaseOwner: l2TransferLeaseOwner,
          processedTxs: processedL2Transfers,
        }).pipe(
          Effect.provide(WriteBehindLive),
          Effect.provide(Database.layer),
          Effect.provide(NodeConfig.layer),
        ),
      ),
    );
    const acceptedL2Transfer = await runNodeDatabaseEffect(
      TxAdmissionsDB.getByTxId(builtL2Transfer.txId),
    );
    expect(acceptedL2Transfer?.[TxAdmissionsDB.Columns.STATUS]).toBe(
      TxAdmissionsDB.Status.Accepted,
    );
    expect(await runNodeDatabaseEffect(MempoolDB.retrieveTxCount)).toBe(1n);
    const alignedMempoolTimestamp = new Date(
      Number(depositBlock.queuedHeader.endTime) + 1,
    );
    expect(alignedMempoolTimestamp.getTime()).toBeLessThanOrEqual(Date.now());
    const alignedMempoolRows = await runNodeDatabaseEffect(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql`
          UPDATE ${sql(MempoolDB.tableName)}
          SET time_stamp_tz = ${alignedMempoolTimestamp}
          WHERE tx_id = ${builtL2Transfer.txId}
          RETURNING tx_id
        `;
      }),
    );
    expect(alignedMempoolRows).toHaveLength(1);
    const projectedSenderAfterAdmission = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedSenderAfterAdmission.utxoCount).toEqual(1);
    expect(projectedSenderAfterAdmission.totals.lovelace).toEqual(10_000_000n);
    const projectedRecipientAfterAdmission = await Effect.runPromise(
      utxosProgram(l2RecipientAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedRecipientAfterAdmission.utxoCount).toEqual(1);
    expect(projectedRecipientAfterAdmission.totals.lovelace).toEqual(
      2_000_000n,
    );

    const l2TransactionBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
      expectedL2TxIds: [builtL2Transfer.txId],
    });
    expect(l2TransactionBlock.commitOutput.mempoolTxsCount).toEqual(1);
    expect(l2TransactionBlock.queuedHeader.transactionsRoot).not.toEqual(
      emptyProtocolRoot,
    );
    expect(
      await runNodeDatabaseEffect(
        BlocksDB.retrieveTxHashesByHeaderHash(
          Buffer.from(l2TransactionBlock.queuedHeaderHash, "hex"),
        ),
      ),
    ).toEqual([]);
    expect(
      await runNodeDatabaseEffect(
        ImmutableDB.retrieveTxCborByHash(builtL2Transfer.txId),
      ),
    ).toEqual(builtL2Transfer.txCbor);
    expect(await runNodeDatabaseEffect(MempoolDB.retrieveTxCount)).toBe(0n);

    const projectedSenderAfterMerge = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedSenderAfterMerge.utxoCount).toEqual(1);
    expect(projectedSenderAfterMerge.totals.lovelace).toEqual(10_000_000n);
    const projectedRecipientAfterMerge = await Effect.runPromise(
      utxosProgram(l2RecipientAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedRecipientAfterMerge.utxoCount).toEqual(1);
    expect(projectedRecipientAfterMerge.totals.lovelace).toEqual(2_000_000n);
    const l2WithdrawalTarget = projectedSenderAfterMerge.utxos[0]!;
    const l2PaymentCredential = paymentCredentialOf(l2Address);
    if (l2PaymentCredential?.type !== "Key") {
      throw new Error("Expected withdrawal target L2 owner to be a key hash");
    }
    const l1AddressData = await Effect.runPromise(
      SDK.addressDataFromBech32(l2Address),
    );
    const withdrawalBody: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: l2WithdrawalTarget.txHash,
        outputIndex: BigInt(l2WithdrawalTarget.outputIndex),
      },
      l2_owner: l2PaymentCredential.hash,
      l2_value: assetsToValue({ lovelace: 10_000_000n }),
      l1_address: l1AddressData,
      l1_datum: "NoDatum",
    };
    const submittedWithdrawal = await submitWithdrawalWithDiagnostics(fixture, {
      body: withdrawalBody,
      signature: signWithdrawalBody(withdrawalPrivateKey, withdrawalBody),
      refundAddress: l1AddressData,
      refundDatum: "NoDatum",
    });

    const fetchedWithdrawalUtxos = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.withdrawal.spendingScriptAddress,
        eventPolicyId: fixture.contracts.withdrawal.policyId,
      }),
    );
    expect(fetchedWithdrawalUtxos).toHaveLength(1);
    const withdrawalUtxo = fetchedWithdrawalUtxos[0]!;
    expect(submittedWithdrawal.withdrawalEventId).toEqual(
      withdrawalUtxo.idCbor.toString("hex"),
    );

    fixture.emulator.awaitSlot(
      fixture.operatorLucid.unixTimeToSlot(
        Number(withdrawalUtxo.datum.inclusion_time),
      ) + 1,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const withdrawalFetch = await runNodeCommandProgram(
      fetchWithdrawalsOnceProgram,
      { fixture, lucidService, globals },
    );
    expect(withdrawalFetch.reconciledCount).toEqual(1);
    const withdrawalFetchAgain = await runNodeCommandProgram(
      fetchWithdrawalsOnceProgram,
      { fixture, lucidService, globals },
    );
    expect(withdrawalFetchAgain.reconciledCount).toEqual(1);

    const withdrawalBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
    });
    expect(withdrawalBlock.queuedHeader.withdrawalsRoot).not.toEqual(
      emptyProtocolRoot,
    );

    const withdrawalEntries = await runNodeDatabaseEffect(
      WithdrawalsDB.retrieveAllEntries(),
    );
    expect(withdrawalEntries).toHaveLength(1);
    expect(withdrawalEntries[0]?.[WithdrawalsDB.Columns.VALIDITY]).toEqual(
      WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    const withdrawalRootKeyValues = await Effect.runPromise(
      Effect.forEach(withdrawalEntries, WithdrawalsDB.toRootKeyValue),
    );
    const expectedWithdrawalRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalRootKeyValues,
    );
    expect(withdrawalBlock.queuedHeader.withdrawalsRoot).toEqual(
      expectedWithdrawalRoot,
    );
    const withdrawalEventIdHex = withdrawalUtxo.idCbor.toString("hex");
    const withdrawalResolution = await runNodeCommandProgram(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: Buffer.from(withdrawalUtxo.idCbor),
      }),
      { fixture, lucidService, globals },
    );
    expect(withdrawalResolution.root).toEqual(expectedWithdrawalRoot);
    if (withdrawalResolution.kind !== "withdrawal") {
      throw new Error("Expected withdrawal settlement proof resolution.");
    }
    expect(withdrawalResolution.validity).toEqual(
      WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    expect(withdrawalResolution.settlementRefInput.txHash).toEqual(
      withdrawalBlock.settlementUtxo.txHash,
    );
    const withdrawalStatus = await runNodeCommandProgram(
      withdrawalStatusProgram({
        eventId: Buffer.from(withdrawalUtxo.idCbor),
      }),
      { fixture, lucidService, globals },
    );
    expect(withdrawalStatus.status).toEqual(WithdrawalsDB.Status.Finalized);
    expect(withdrawalStatus.validity).toEqual(
      WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    expect(withdrawalStatus.settlementOutRef).not.toBeNull();
    const projectedAfterWithdrawal = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedAfterWithdrawal.utxoCount).toEqual(0);

    const initialize = await runNodeCommandProgram(
      initializePayoutProgram({ eventId: withdrawalEventIdHex }),
      { fixture, lucidService, globals },
    );
    await ensureSeparateCollateralUtxo(fixture.operatorLucid);
    expect(initialize.details.withdrawalOutRef).toEqual(
      `${withdrawalUtxo.utxo.txHash}#${withdrawalUtxo.utxo.outputIndex.toString()}`,
    );
    const initializedStatus = await runNodeCommandProgram(
      payoutStatusProgram(withdrawalEventIdHex),
      { fixture, lucidService, globals },
    );
    expect(["initialized", "partially_funded"]).toContain(
      initializedStatus.phase,
    );
    const payoutUnit = initializedStatus.payoutUnit;
    const initializedPayout = findUtxoWithUnit(
      await fixture.operatorLucid.utxosAt(
        fixture.contracts.payout.spendingScriptAddress,
      ),
      payoutUnit,
    );
    expect(initializedPayout.assets[payoutUnit]).toEqual(1n);

    const addFunds = await runNodeCommandProgram(
      addReserveFundsToPayoutProgram({ eventId: withdrawalEventIdHex }),
      { fixture, lucidService, globals },
    );
    expect(addFunds.details.reserveOutRef).toEqual(
      `${reserveAfterAbsorb.txHash}#${reserveAfterAbsorb.outputIndex.toString()}`,
    );
    const fundedStatus = await runNodeCommandProgram(
      payoutStatusProgram(withdrawalEventIdHex),
      { fixture, lucidService, globals },
    );
    expect(fundedStatus.phase).toEqual("funded");
    const fundedPayout = findUtxoWithUnit(
      await fixture.operatorLucid.utxosAt(
        fixture.contracts.payout.spendingScriptAddress,
      ),
      payoutUnit,
    );
    expect(fundedPayout.assets.lovelace).toEqual(10_000_000n);

    const conclude = await runNodeCommandProgram(
      concludePayoutProgram({ eventId: withdrawalEventIdHex }),
      { fixture, lucidService, globals },
    );
    expect(conclude.details.payoutUnit).toEqual(payoutUnit);
    const concludedStatus = await runNodeCommandProgram(
      payoutStatusProgram(withdrawalEventIdHex),
      { fixture, lucidService, globals },
    );
    expect(concludedStatus.phase).toEqual("concluded");

    expect(
      (
        await fixture.operatorLucid.utxosAt(
          fixture.contracts.payout.spendingScriptAddress,
        )
      ).some((utxo) => utxo.assets[payoutUnit] === 1n),
    ).toBe(false);
    expect(
      (await fixture.operatorLucid.utxosAt(l2Address)).some(
        (utxo) => utxo.assets.lovelace === 10_000_000n,
      ),
    ).toBe(true);
    // This end-to-end journey measured 198s alone but 395s-423s when it runs
    // last in the full file, so the previous 420s budget left ~6% headroom and
    // timed out on slower machines. The budget is a harness allowance, not an
    // invariant: a genuine hang still fails here, just later.
  }, 900_000);
});

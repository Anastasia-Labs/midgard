import "node:crypto";
import "node:fs/promises";
import "node:http";
import "node:path";
import "@al-ft/midgard-validation";
import "@effect/platform";
import "vitest";
import "../src/commands/listen-router.js";
import "../src/commands/state-reconciliation.js";
import "../src/commands/submit-withdrawal.js";
import "../src/database/index.js";
import "../src/fibers/tx-queue-processor.js";
import "../src/mpf/index.js";
import "../src/services/event-history-producer.js";
import "./deposit-flow-emulator-shared.js";
import "./deposit-flow-emulator-merge-payout.admit-l2-tx.js";

import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { RejectCodes } from "@al-ft/midgard-validation";
import { describe, expect, it, vi } from "vitest";

import { MutationJobsDB, TxRejectionsDB } from "../src/database/index.js";
import { ADMISSION_REJECT_CODE_PENDING_WITHDRAWAL_INPUT } from "../src/fibers/tx-queue-processor.js";
import { COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT } from "../src/mpf/index.js";
import {
  admitAndAcceptL2Tx,
  admitL2Tx,
  expectReconciled,
  submitWithdrawalRefusal,
} from "./deposit-flow-emulator-merge-payout.admit-l2-tx.js";
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
  ensureSeparateCollateralUtxo,
  expectDaCommitteeAcceptsPersistedPayload,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  findUtxoWithUnit,
  ImmutableDB,
  ingestEmulatorEventsUnowned,
  initializeNodeRuntime,
  initializePayoutProgram,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  makeNodeConfigForFixture,
  MempoolDB,
  MempoolLedgerDB,
  mergeMaturityWindow,
  paymentCredentialOf,
  payoutStatusProgram,
  reserveUtxosProgram,
  resetActiveRuntimePaths,
  resolveEventSettlementProofProgram,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runMergeUntilMerged,
  runNodeCommandProgram,
  runNodeDatabaseEffect,
  SDK,
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
} from "./deposit-flow-emulator-shared.js";

describe("deposit flow emulator", { concurrent: false }, () => {
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
        ...SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).deposit,
        ),
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);

    const inclusionSlot =
      fixture.operatorLucid.unixTimeToSlot(
        Number(fetchedDepositUtxos[0]!.facts.inclusion_time),
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
    expect(mergeResult.postMergeSnapshot.blockCount).toBe(0);

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

  it("merges three queued blocks oldest-first on the scheduled path and holds the final tail while a failed local mutation job remains", async () => {
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
    // The live operator runs with a batch threshold of 2, so the scheduled
    // fiber merges by threshold while two or more blocks are queued and only
    // the last block goes through the final-tail rule.
    const nodeConfig = {
      ...(await makeNodeConfigForFixture(fixture)),
      MIN_QUEUE_LENGTH_FOR_MERGING: 2,
    };
    const harness = { fixture, lucidService, globals };
    const l2Address = await fixture.depositorLucid.wallet().address();

    const queuedHeaders: SDK.Header[] = [];
    for (const lovelace of [4_000_000n, 5_000_000n, 6_000_000n]) {
      // Each deposit must be admitted after the previous header's end time,
      // or it is due for a block that is already committed.
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await submitDepositWithDiagnostics(fixture, {
        l2Address,
        l2Datum: null,
        lovelace,
        additionalAssets: {},
      });
      const latestInclusionTime = (
        await Effect.runPromise(
          SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
            ...SDK.eventHistoryDeploymentFromContracts(
              SDK.requireEventHistoryContracts(fixture.contracts).deposit,
            ),
          }),
        )
      ).reduce(
        (latest, deposit) =>
          deposit.facts.inclusion_time > latest
            ? deposit.facts.inclusion_time
            : latest,
        0n,
      );
      fixture.emulator.awaitSlot(
        fixture.operatorLucid.unixTimeToSlot(Number(latestInclusionTime)) + 1,
      );
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const commitOutput = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        ),
      });
      await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
      await runBlockConfirmation(globals, fixture.contracts, lucidService);
      const recovery = await runLocalFinalizationRecoveryWorker(
        globals,
        fixture.contracts,
        lucidService,
      );
      expect(recovery.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
      const queue = await Effect.runPromise(
        SDK.fetchSortedStateQueueUTxOsProgram(
          fixture.operatorLucid,
          stateQueueFetchConfig(fixture.contracts),
        ),
      );
      const header = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(queue[queue.length - 1]!.datum),
      );
      // Attest before time moves on: an unattested block past its window
      // pauses every later commit.
      await attestQueuedStateQueueHeader({
        fixture,
        lucidService,
        globals,
        headerHash: await Effect.runPromise(SDK.hashBlockHeader(header)),
      });
      queuedHeaders.push(header);
    }
    const queuedHeaderHashes = await Promise.all(
      queuedHeaders.map((header) =>
        Effect.runPromise(SDK.hashBlockHeader(header)),
      ),
    );
    expect(new Set(queuedHeaderHashes).size).toBe(3);
    const queueBeforeMerges = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    expect(queueBeforeMerges).toHaveLength(4);
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(
        fixture.operatorLucid,
        Number(queuedHeaders[2]!.endTime),
      ).readyAfterUnixTime,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));
    await expectReconciled("three blocks queued, none merged", harness);

    const confirmedHeaderHash = async () => {
      const queue = await Effect.runPromise(
        SDK.fetchSortedStateQueueUTxOsProgram(
          fixture.operatorLucid,
          stateQueueFetchConfig(fixture.contracts),
        ),
      );
      const confirmed = await Effect.runPromise(
        SDK.getConfirmedStateFromStateQueueDatum(queue[0]!.datum),
      );
      return { queueLength: queue.length - 1, confirmed: confirmed.data };
    };

    for (const index of [0, 1]) {
      const merged = await runMergeUntilMerged({
        ...harness,
        force: false,
        nodeConfig,
      });
      if (merged.status !== "merged") throw new Error("unreachable");
      expect(merged.trigger).toBe("threshold");
      expect(merged.headerHash).toBe(queuedHeaderHashes[index]);
      const after = await confirmedHeaderHash();
      expect(after.queueLength).toBe(2 - index);
      expect(after.confirmed.headerHash).toBe(queuedHeaderHashes[index]);
      expect(after.confirmed.utxoRoot).toBe(queuedHeaders[index]!.utxosRoot);
      await expectReconciled(
        `after scheduled merge ${(index + 1).toString()} of 3`,
        harness,
      );
    }

    // A failed local mutation job is unfinished local work: the final tail
    // must wait (the tail may still have a successor to build on it).
    const failedJobId = `${MutationJobsDB.Kind.ConfirmedMergeFinalization}:sweep-failed-job`;
    await runNodeDatabaseEffect(
      MutationJobsDB.start({
        jobId: failedJobId,
        kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
      }).pipe(
        Effect.zipRight(MutationJobsDB.markFailed(failedJobId, "injected")),
      ),
    );
    await expect(
      runMergeUntilMerged({ ...harness, force: false, nodeConfig }),
    ).rejects.toThrow(/skipped_pending_local_work/);
    expect((await confirmedHeaderHash()).confirmed.headerHash).toBe(
      queuedHeaderHashes[1],
    );
    await runNodeDatabaseEffect(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM ${sql(MutationJobsDB.tableName)} WHERE job_id = ${failedJobId}`;
      }),
    );

    const tail = await runMergeUntilMerged({
      ...harness,
      force: false,
      nodeConfig,
    });
    if (tail.status !== "merged") throw new Error("unreachable");
    expect(tail.trigger).toBe("final_tail_auto_merge");
    expect(tail.headerHash).toBe(queuedHeaderHashes[2]);
    const afterTail = await confirmedHeaderHash();
    expect(afterTail.queueLength).toBe(0);
    expect(afterTail.confirmed.headerHash).toBe(queuedHeaderHashes[2]);
    expect(afterTail.confirmed.utxoRoot).toBe(queuedHeaders[2]!.utxosRoot);
    for (const headerHash of queuedHeaderHashes) {
      const settlementUnit = toUnit(
        fixture.contracts.settlement.policyId,
        headerHash,
      );
      expect(
        await fixture.operatorLucid.utxosAtWithUnit(
          fixture.contracts.settlement.spendingScriptAddress,
          settlementUnit,
        ),
      ).toHaveLength(1);
    }
    await expectReconciled("after the final-tail merge", harness);
  }, 900_000);

  it("runs deposit, reserve absorption, withdrawal commitment, and payout to conclusion", async () => {
    await resetActiveRuntimePaths();
    await initializeNodeRuntime();
    await configureEmulatorDaRuntimeManifest();

    const fixture = await makeFixture();
    const acceptedTransactions: string[] = [];
    const submit = fixture.emulator.submitTx.bind(fixture.emulator);
    fixture.emulator.submitTx = async (cbor) => {
      const txHash = await submit(cbor);
      acceptedTransactions.push(cbor);
      return txHash;
    };
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
        ...SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).deposit,
        ),
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);
    const depositUtxo = fetchedDepositUtxos[0]!;
    fixture.emulator.awaitSlot(
      fixture.operatorLucid.unixTimeToSlot(
        Number(depositUtxo.facts.inclusion_time),
      ) + 1,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const depositBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
    });
    await expectReconciled("after deposit block merge", {
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
    await expectReconciled("after absorb-confirmed-deposit-to-reserve", {
      fixture,
      lucidService,
      globals,
    });
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
    const depositorKeyHash = paymentCredentialOf(l2Address)?.hash;
    const recipientKeyHash = paymentCredentialOf(l2RecipientAddress)?.hash;
    // `submit-withdrawal` refuses an output whose producing L2 tx has not
    // committed. A projected deposit output has no producing L2 tx, so the
    // command passes that check and stops at the signer check instead.
    expect(
      await submitWithdrawalRefusal(
        { fixture, lucidService, globals },
        {
          l2OutRef: `${l2TransferSource.txHash}#${l2TransferSource.outputIndex.toString()}`,
          walletSeedPhrase: fixture.referenceScriptsAccount.seedPhrase,
          l1Address: l2Address,
        },
      ),
    ).toBe(
      `Selected L2 UTxO is owned by ${depositorKeyHash}, not withdrawal signer ${recipientKeyHash}.`,
    );
    const builtL2Transfer = await buildTransferTx({
      senderAddress: l2Address,
      destinationAddress: l2RecipientAddress,
      signer: withdrawalPrivateKey,
      selectedInputs: [l2TransferSource],
      requestedAssets: { lovelace: 2_000_000n },
      networkId: 0n,
    });
    await admitAndAcceptL2Tx(
      fixture,
      builtL2Transfer,
      new Date(Number(depositBlock.queuedHeader.endTime) + 1),
    );
    expect(await runNodeDatabaseEffect(MempoolDB.retrieveTxCount)).toBe(1n);
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
    await expectReconciled("after L2 transfer block merge", {
      fixture,
      lucidService,
      globals,
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
    const [withdrawalTargetEntry] = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveSpendableByAddress(l2Address),
    );
    const withdrawalTargetOutRefHex =
      withdrawalTargetEntry![MempoolLedgerDB.Columns.OUTREF].toString("hex");
    const withdrawalTargetSource = decodeNodeUtxo({
      outref: withdrawalTargetOutRefHex,
      outputCbor:
        withdrawalTargetEntry![MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
    });
    const buildWithdrawalTargetSpend = (lovelace: bigint) =>
      buildTransferTx({
        senderAddress: l2Address,
        destinationAddress: l2RecipientAddress,
        signer: withdrawalPrivateKey,
        selectedInputs: [withdrawalTargetSource],
        requestedAssets: { lovelace },
        networkId: 0n,
      });
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
      signature: SDK.signWithdrawalBody(withdrawalPrivateKey, withdrawalBody),
      refundAddress: l1AddressData,
      refundDatum: "NoDatum",
    });

    const fetchedWithdrawalUtxos = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(fixture.depositorLucid, {
        ...SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).withdrawal,
        ),
      }),
    );
    expect(fetchedWithdrawalUtxos).toHaveLength(1);
    const withdrawalUtxo = fetchedWithdrawalUtxos[0]!;
    expect(submittedWithdrawal.withdrawalEventId).toEqual(
      withdrawalUtxo.idCbor.toString("hex"),
    );

    fixture.emulator.awaitSlot(
      fixture.operatorLucid.unixTimeToSlot(
        Number(withdrawalUtxo.facts.inclusion_time),
      ) + 1,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    // A spend of the withdrawal target admitted after the withdrawal landed
    // on L1 but before the node observed it. The withdrawal block rejects it
    // at commit, and its admitted mempool_ledger effects must leave with it.
    const withdrawalTargetSpend = await buildWithdrawalTargetSpend(3_000_000n);
    await admitAndAcceptL2Tx(
      fixture,
      withdrawalTargetSpend,
      new Date(Number(withdrawalUtxo.facts.inclusion_time) + 1),
    );
    const projectedRecipientAfterSpend = await Effect.runPromise(
      utxosProgram(l2RecipientAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedRecipientAfterSpend.totals.lovelace).toEqual(5_000_000n);
    // The recipient then spends that pending output together with its
    // committed 2 ADA output in the same block window. The block rejects this
    // spend too, and the committed input must come back to mempool_ledger.
    const recipientEntries = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveSpendableByAddress(l2RecipientAddress),
    );
    expect(recipientEntries).toHaveLength(2);
    const outRefAndOutput = (
      entries: readonly {
        readonly [MempoolLedgerDB.Columns.OUTREF]: Buffer;
        readonly [MempoolLedgerDB.Columns.OUTPUT]: Buffer;
      }[],
    ) =>
      entries.map((entry) => [
        entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
        entry[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
      ]);
    const committedRecipientEntries = recipientEntries.filter((entry) =>
      entry[MempoolLedgerDB.Columns.OUTREF]
        .toString("hex")
        .includes(builtL2Transfer.txId.toString("hex")),
    );
    expect(committedRecipientEntries).toHaveLength(1);
    const [committedRecipientUtxo, pendingRecipientUtxo] = [
      committedRecipientEntries[0]!,
      recipientEntries.find(
        (entry) => !committedRecipientEntries.includes(entry),
      )!,
    ].map((entry) =>
      decodeNodeUtxo({
        outref: entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
        outputCbor: entry[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
      }),
    );
    // The recipient cannot withdraw the output of the still-pending spend:
    // the withdrawal would be classified against a ledger without it. The
    // command refuses before it builds any L1 transaction.
    const acceptedTransactionsBeforeRefusal = acceptedTransactions.length;
    expect(
      await submitWithdrawalRefusal(
        { fixture, lucidService, globals },
        {
          l2OutRef: `${pendingRecipientUtxo!.txHash}#${pendingRecipientUtxo!.outputIndex.toString()}`,
          walletSeedPhrase: fixture.referenceScriptsAccount.seedPhrase,
          l1Address: l2RecipientAddress,
        },
      ),
    ).toBe(
      `Producing L2 tx ${withdrawalTargetSpend.txId.toString("hex")} is accepted, not committed; wait for the producing tx to commit before withdrawing its output.`,
    );
    expect(acceptedTransactions).toHaveLength(
      acceptedTransactionsBeforeRefusal,
    );
    // Its committed output passes that check and stops at the signer check.
    expect(
      await submitWithdrawalRefusal(
        { fixture, lucidService, globals },
        {
          l2OutRef: `${committedRecipientUtxo!.txHash}#${committedRecipientUtxo!.outputIndex.toString()}`,
          walletSeedPhrase: fixture.depositorAccount.seedPhrase,
          l1Address: l2Address,
        },
      ),
    ).toBe(
      `Selected L2 UTxO is owned by ${recipientKeyHash}, not withdrawal signer ${depositorKeyHash}.`,
    );
    const recipientPrivateKey = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.referenceScriptsAccount.seedPhrase, {
        network: "Custom",
      }).paymentKey,
    );
    const cascadedRecipientSpend = await buildTransferTx({
      senderAddress: l2RecipientAddress,
      destinationAddress: l2RecipientAddress,
      signer: recipientPrivateKey,
      selectedInputs: recipientEntries.map((entry) =>
        decodeNodeUtxo({
          outref: entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
          outputCbor: entry[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
        }),
      ),
      requestedAssets: { lovelace: 4_000_000n },
      networkId: 0n,
    });
    await admitAndAcceptL2Tx(
      fixture,
      cascadedRecipientSpend,
      new Date(Number(withdrawalUtxo.facts.inclusion_time) + 2),
    );
    const projectedRecipientAfterCascadedSpend = await Effect.runPromise(
      utxosProgram(l2RecipientAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedRecipientAfterCascadedSpend.utxoCount).toEqual(2);
    expect(projectedRecipientAfterCascadedSpend.totals.lovelace).toEqual(
      5_000_000n,
    );

    const withdrawalFetch = await runNodeCommandProgram(
      ingestEmulatorEventsUnowned(fixture, { globals }),
      { fixture, lucidService, globals },
    );
    expect(withdrawalFetch.inserted).toEqual(1);
    const withdrawalFetchAgain = await runNodeCommandProgram(
      ingestEmulatorEventsUnowned(fixture, { globals }),
      { fixture, lucidService, globals },
    );
    expect(withdrawalFetchAgain.inserted).toEqual(0);
    // Once the withdrawal is pending, admission refuses a new spend of its
    // outref durably, before it touches mempool_ledger.
    expect([
      ...(await runNodeDatabaseEffect(
        WithdrawalsDB.retrievePendingLedgerOutRefHexes,
      )),
    ]).toEqual([withdrawalTargetOutRefHex]);
    const ledgerBeforeLateSpend = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveSpendable,
    );
    const lateWithdrawalTargetSpend =
      await buildWithdrawalTargetSpend(4_000_000n);
    const lateSpendAdmission = await admitL2Tx(
      fixture,
      lateWithdrawalTargetSpend,
    );
    expect(lateSpendAdmission?.[TxAdmissionsDB.Columns.STATUS]).toBe(
      TxAdmissionsDB.Status.Rejected,
    );
    expect(
      (
        await runNodeDatabaseEffect(
          TxRejectionsDB.retrieveByTxId(lateWithdrawalTargetSpend.txId),
        )
      ).map((row) => [
        row[TxRejectionsDB.Columns.REJECT_CODE],
        row[TxRejectionsDB.Columns.REJECT_DETAIL],
      ]),
    ).toEqual([
      [
        ADMISSION_REJECT_CODE_PENDING_WITHDRAWAL_INPUT,
        `Transaction spends L2 outref ${withdrawalTargetOutRefHex}, which a pending withdrawal names`,
      ],
    ]);
    expect(
      await runNodeDatabaseEffect(MempoolLedgerDB.retrieveSpendable),
    ).toEqual(ledgerBeforeLateSpend);

    const withdrawalBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
    });
    await expectReconciled("after withdrawal block merge", {
      fixture,
      lucidService,
      globals,
    });
    expect(
      (
        await runNodeDatabaseEffect(
          TxRejectionsDB.retrieveByTxId(withdrawalTargetSpend.txId),
        )
      ).map((row) => row[TxRejectionsDB.Columns.REJECT_CODE]),
    ).toEqual([COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT]);
    expect(
      (
        await runNodeDatabaseEffect(
          TxRejectionsDB.retrieveByTxId(cascadedRecipientSpend.txId),
        )
      ).map((row) => row[TxRejectionsDB.Columns.REJECT_CODE]),
    ).toEqual([RejectCodes.InputNotFound]);
    expect(await runNodeDatabaseEffect(MempoolDB.retrieveTxCount)).toBe(0n);
    expect(
      outRefAndOutput(
        await runNodeDatabaseEffect(
          MempoolLedgerDB.retrieveSpendableByAddress(l2RecipientAddress),
        ),
      ),
    ).toEqual(outRefAndOutput(committedRecipientEntries));
    const projectedRecipientAfterWithdrawal = await Effect.runPromise(
      utxosProgram(l2RecipientAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedRecipientAfterWithdrawal.totals.lovelace).toEqual(
      2_000_000n,
    );
    expect(withdrawalBlock.queuedHeader.withdrawalsRoot).not.toEqual(
      emptyProtocolRoot,
    );
    const committeeVerifiedWithdrawalPayload =
      await expectDaCommitteeAcceptsPersistedPayload({
        headerHash: withdrawalBlock.queuedHeaderHash,
        l1Header: withdrawalBlock.queuedHeader,
      });
    expect(committeeVerifiedWithdrawalPayload.counts.withdrawalCount).toBe(1n);
    expect(committeeVerifiedWithdrawalPayload.roots.withdrawalsRoot).toBe(
      withdrawalBlock.queuedHeader.withdrawalsRoot,
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
    await expectReconciled("after initialize-payout", {
      fixture,
      lucidService,
      globals,
    });

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
    await expectReconciled("after add-reserve-funds-to-payout", {
      fixture,
      lucidService,
      globals,
    });

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
    await expectReconciled("after conclude-payout", {
      fixture,
      lucidService,
      globals,
    });

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
    const evidenceDirectory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
    if (evidenceDirectory !== undefined) {
      const parameters = fixture.operatorLucid.config().protocolParameters!;
      const blueprint = await readFile(
        process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      );
      const transactions = acceptedTransactions.map((signedCbor) => {
        const tx = CML.Transaction.from_cbor_hex(signedCbor);
        const redeemers = tx.witness_set().redeemers()?.to_flat_format();
        let memory = 0n;
        let steps = 0n;
        for (let index = 0; index < (redeemers?.len() ?? 0); index++) {
          memory += redeemers!.get(index).ex_units().mem();
          steps += redeemers!.get(index).ex_units().steps();
        }
        expect(signedCbor.length / 2).toBeLessThanOrEqual(parameters.maxTxSize);
        expect(memory).toBeLessThanOrEqual(parameters.maxTxExMem);
        expect(steps).toBeLessThanOrEqual(parameters.maxTxExSteps);
        return {
          txHash: CML.hash_transaction(tx.body()).to_hex(),
          signedCbor,
          signedBytes: signedCbor.length / 2,
          feeLovelace: tx.body().fee(),
          memory,
          steps,
        };
      });
      await mkdir(evidenceDirectory, { recursive: true });
      await writeFile(
        join(evidenceDirectory, "node-complete-history-journey.json"),
        JSON.stringify(
          {
            scope:
              "Complete applied node emulator journey, not live acceptance",
            blueprintSha256: createHash("sha256")
              .update(blueprint)
              .digest("hex"),
            protocolParameters: parameters,
            history: SDK.requireEventHistoryContracts(fixture.contracts),
            hubPolicyId: fixture.contracts.hubOracle.policyId,
            stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
            settlementPolicyId: fixture.contracts.settlement.policyId,
            payoutPolicyId: fixture.contracts.payout.policyId,
            reserveHash: fixture.contracts.reserve.spendingScriptHash,
            depositHeader: depositBlock.queuedHeaderHash,
            withdrawalHeader: withdrawalBlock.queuedHeaderHash,
            depositEventId: depositEventIdHex,
            withdrawalEventId: withdrawalEventIdHex,
            transactions,
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
    // This end-to-end journey measured 198s alone but 395s-423s when it runs
    // last in the full file, so the previous 420s budget left ~6% headroom and
    // timed out on slower machines. The budget is a harness allowance, not an
    // invariant: a genuine hang still fails here, just later.
  }, 900_000);
});

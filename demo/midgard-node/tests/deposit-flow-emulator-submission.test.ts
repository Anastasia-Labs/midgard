import "./helpers/follower-emulator-installed.js";

import { describe, expect, it, vi } from "vitest";

import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  DA_TRANSPORT_LIMITS,
  DaPayloadsDB,
  DepositsDB,
  Effect,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
  Ref,
  runCommitWorkerUntilSubmitted,
  runNodeDatabaseEffect,
  SDK,
  submitDepositWithDiagnostics,
  toUnit,
  unwrapDaPayload,
  UserEventsUtils,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

describe("deposit flow emulator", { concurrent: false }, () => {
  it("commits a realistic deposit-only block through the live worker core and applies it when it lands", async () => {
    const initial = await openProductionLifecycle();
    let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
    const { fixture, lucidService, globals, production } = initial;

    try {
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      await ensureSeparateCollateralUtxo(fixture.depositorLucid);
      await advanceHistoryAdmissionClock(fixture, "deposit");
      await alignCommitSchedulerBeforeTestWorker({
        fixture,
        lucidService,
        targetEndTimeMs:
          fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      });
      await h.synchronize();

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
          ...SDK.eventHistoryDeploymentFromContracts(
            SDK.requireEventHistoryContracts(fixture.contracts).deposit,
          ),
        }),
      );
      expect(fetchedDepositUtxos).toHaveLength(1);

      const depositUtxo = fetchedDepositUtxos[0]!;
      const depositAuthUnit = toUnit(
        fixture.contracts.deposit.policyId,
        depositUtxo.assetName,
      );
      await h.deployment.chain.awaitLedgerTime(
        Number(depositUtxo.facts.inclusion_time) + 1000,
      );
      vi.setSystemTime(fixture.emulator.now());

      const depositEntries = await runNodeDatabaseEffect(
        DepositsDB.retrieveAllEntries(),
      );
      expect(depositEntries).toHaveLength(0);

      const utxosBeforeProjection = await runNodeDatabaseEffect(
        utxosProgram(l2Address),
      );
      expect(utxosBeforeProjection.utxoCount).toEqual(0);

      await h.synchronize();

      const rawUtxosAfterBackgroundProjection = await runNodeDatabaseEffect(
        MempoolLedgerDB.retrieveByAddress(l2Address),
      );
      expect(rawUtxosAfterBackgroundProjection).toHaveLength(1);

      const spendableUtxosAfterBackgroundProjection =
        await runNodeDatabaseEffect(utxosProgram(l2Address));
      expect(spendableUtxosAfterBackgroundProjection.utxoCount).toEqual(0);

      const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const commitOutput = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: latestBlockBeforeCommit,
        nodeConfig: production.nodeConfig,
        production: { ...production, globals },
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

      const projectedUtxosBeforeLanding = await runNodeDatabaseEffect(
        utxosProgram(l2Address),
      );
      expect(projectedUtxosBeforeLanding.utxoCount).toEqual(0);

      // Submitted, not landed: the node's journal holds the commit's signed
      // intent, and its own block derives unlanded until the commit lands.
      const submittedHeaderHash = Buffer.from(
        commitOutput.submittedHeaderHash,
        "hex",
      );
      expect(
        await runNodeDatabaseEffect(
          PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
            submittedHeaderHash,
          ),
        ),
      ).toBe(PendingBlockFinalizationsDB.OwnBlockStatus.Unlanded);

      const expectedDepositsRoot = await expectedAuthenticatedEventRoot(
        SDK.ROOT_DOMAINS.deposits,
        depositEntriesAfterSubmission.map((entry) => ({
          key: entry[UserEventsUtils.Columns.ID],
          value: entry[UserEventsUtils.Columns.INFO],
        })),
      );

      await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
      await h.applyLanded(commitOutput.submittedHeaderHash);

      const projectedUtxosAfterLanding = await runNodeDatabaseEffect(
        utxosProgram(l2Address),
      );
      expect(projectedUtxosAfterLanding.utxoCount).toEqual(1);
      expect(projectedUtxosAfterLanding.totals.lovelace).toEqual(12_000_000n);
      expect(projectedUtxosAfterLanding.utxos[0]?.address).toEqual(l2Address);
      expect(
        projectedUtxosAfterLanding.utxos[0]?.assets[depositAuthUnit],
      ).toBeUndefined();
      expect(projectedUtxosAfterLanding.utxos[0]?.datum).toBeUndefined();

      const latestBlockAfterCommit = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const latestHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(latestBlockAfterCommit.datum),
      );
      const latestHeaderHash = await Effect.runPromise(
        SDK.hashBlockHeader(latestHeader),
      );
      const daPayloadAfterLanding = await runNodeDatabaseEffect(
        DaPayloadsDB.retrieveByHeaderHash(Buffer.from(latestHeaderHash, "hex")),
      );
      expect(daPayloadAfterLanding._tag).toBe("Some");
      if (daPayloadAfterLanding._tag !== "Some") {
        throw new Error("Expected a DA payload row for the landed block");
      }
      const daPayloadRow = daPayloadAfterLanding.value;
      const daPayloadEnvelope = await unwrapDaPayload(
        daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_CBOR],
        { maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes },
      );
      const daPayload = SDK.decodeDaPayload(daPayloadEnvelope.innerBytes);
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
      expect(
        daPayloadRow[DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT],
      ).toEqual(latestHeader.forcedTransactionsRoot);
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

      expect(
        await runNodeDatabaseEffect(
          PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
            submittedHeaderHash,
          ),
        ),
      ).toBe(PendingBlockFinalizationsDB.OwnBlockStatus.Applied);

      // A cold start seeds the local block boundary from the landed block.
      h = await initial.restartRuntime();
      const coldStartBoundary = await Effect.runPromise(
        Ref.get(h.globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
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
    } finally {
      try {
        await h.close();
      } finally {
        vi.useRealTimers();
      }
    }
  }, 900_000);
});

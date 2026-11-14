import "./helpers/follower-emulator-installed.js";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it, vi } from "vitest";

import { WriteBehind } from "../src/services/write-behind.js";
import { admitAndAcceptL2Tx } from "./deposit-flow-emulator-merge-payout.admit-l2-tx.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  attestQueuedStateQueueHeader,
  buildTransferTx,
  CML,
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  confirmedLedgerFullScanCounter,
  Data,
  decodeNodeUtxo,
  Effect,
  fetchLatestCommittedBlock,
  ForcedTransactionsDB,
  getStateQueueDatumEndTime,
  Ledger,
  materializeJournalLedger,
  MempoolLedgerDB,
  Metric,
  MIDGARD_CONSENSUS_PROFILE,
  type NodeConfigDep,
  type NodeUtxo,
  Option,
  PendingBlockFinalizationsDB,
  retainAndAttestSubmittedHeader,
  runCommitWorker,
  runCommitWorkerUntilSubmitted,
  runNodeDatabaseEffect,
  SDK,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  submitDeposit,
} from "./helpers/correction-admission-scenario.js";
import { insertForcedEntriesWithOrders } from "./helpers/emulator-l1-follower.forced-orders.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

describe("deposit flow emulator", { concurrent: false }, () => {
  // 900s leaves headroom for the full real-contract workflow. Protocol
  // bring-up publishes the 153-target reference-script roster in 39 planned
  // batches (with size-driven splits where required).
  it.each(["periodic", "off"] as const)(
    "hydrates %s commit candidates when stateful work is selected",
    async (payloadRootCheck) => {
      const h = await openProductionLifecycle();
      const { fixture, lucidService, globals, production } = h;
      try {
        const nodeConfig: NodeConfigDep = {
          ...production.nodeConfig,
          MPF_PAYLOAD_ROOT_CHECK: payloadRootCheck,
          MPF_RECORD_CORPUS: "",
          COMMIT_MAX_L2_TX_COUNT: 1,
          MIN_FEE_A: 0n,
          MIN_FEE_B: 0n,
        };
        const owned = { ...production, globals, nodeConfig };
        await advanceEmulatorPastLatestBlockEndTime(fixture);

        // Two real deposits provide an authenticated base snapshot with two
        // independent UTxOs for the normal and forced spends below.
        await submitDeposit(h, 12_000_000n);
        const inclusionTime = await submitDeposit(h, 13_000_000n);
        await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
        vi.setSystemTime(fixture.emulator.now());
        await h.synchronize();
        const baseBlock = await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        );
        const baseCommit = await runCommitWorkerUntilSubmitted({
          fixture,
          lucidService,
          latestBlock: baseBlock,
          nodeConfig,
          production: owned,
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
          PendingBlockFinalizationsDB.retrieveByHeaderHash(
            Buffer.from(baseCommit.submittedHeaderHash, "hex"),
          ),
        );
        expect(Option.isSome(baseJournal)).toBe(true);
        if (Option.isNone(baseJournal)) {
          throw new Error("Expected the authenticated base journal");
        }
        const baseSnapshot = await runNodeDatabaseEffect(
          materializeJournalLedger(baseJournal.value),
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

        // Every later run builds on the landed base, applied as the node
        // applies its own block.
        await h.applyLanded(baseCommit.submittedHeaderHash);

        // With no selected normal or forced work the worker stops before it
        // hydrates any commit base; the full confirmed-ledger scan stays
        // untouched. The node follows the chain first: landed processing
        // reads the ledger on its own.
        await h.synchronize();
        const controlScanBefore = await Effect.runPromise(
          Metric.value(confirmedLedgerFullScanCounter),
        );
        const controlOutput = await runCommitWorker(
          fixture.contracts,
          lucidService,
          await fetchLatestCommittedBlock(
            fixture.operatorLucid,
            fixture.contracts,
          ),
          nodeConfig,
          fixture.runtimeOverrides!.deploymentIdentity,
          owned,
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

        await admitAndAcceptL2Tx(h, normalTransfer, eventTime);

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
          [ForcedTransactionsDB.Columns.INCLUSION_TIME]: eventTime,
          [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
          [ForcedTransactionsDB.Columns.STATUS]:
            ForcedTransactionsDB.Status.Awaiting,
        };
        await runNodeDatabaseEffect(
          insertForcedEntriesWithOrders([forcedEntry], fixture.operatorLucid),
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
          production: owned,
        });
        const scanAfter = await Effect.runPromise(
          Metric.value(confirmedLedgerFullScanCounter),
        );
        expect(scanAfter.count - scanBefore.count).toBeGreaterThan(0n);
        expect(stateful.mempoolTxsCount).toBe(1);

        const active = await runNodeDatabaseEffect(
          PendingBlockFinalizationsDB.retrieveByHeaderHash(
            Buffer.from(stateful.submittedHeaderHash, "hex"),
          ),
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
            PendingBlockFinalizationsDB.Columns
              .EXPECTED_FORCED_TRANSACTION_COUNT
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
          materializeJournalLedger(active.value),
        );
        expect(postState.entries.length).toBeGreaterThan(0);
        expect(postState.root).toBe(stateful.submittedUtxosRoot);
        expect(
          active.value[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
        ).toBe(postState.root);
      } finally {
        await closeLifecycle(h);
      }
    },
    900_000,
  );

  // 900s leaves headroom for the full real-contract workflow. Protocol
  // bring-up publishes the 153-target reference-script roster in 39 planned
  // batches (with size-driven splits where required).
  it("commits the globally oldest transactions from a backlog deeper than three retrieval pages and anchors max endTime", async () => {
    const previousPageSize = process.env.MEMPOOL_RETRIEVE_PAGE_SIZE;
    process.env.MEMPOOL_RETRIEVE_PAGE_SIZE = "2";
    try {
      const h = await openProductionLifecycle();
      const { fixture, lucidService, globals, production } = h;
      try {
        await advanceEmulatorPastLatestBlockEndTime(fixture);

        // Eight real deposits give the backlog eight independent L2 UTxOs in the
        // committed ledger.
        let inclusionTime = 0;
        for (let index = 0; index < 8; index += 1) {
          inclusionTime = await submitDeposit(h, 10_000_000n);
        }
        await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
        vi.setSystemTime(fixture.emulator.now());
        await h.synchronize();
        const fundingCommit = await runCommitWorkerUntilSubmitted({
          fixture,
          lucidService,
          latestBlock: await fetchLatestCommittedBlock(
            fixture.operatorLucid,
            fixture.contracts,
          ),
          nodeConfig: production.nodeConfig,
          production: { ...production, globals },
        });
        await fixture.operatorLucid.awaitTx(fundingCommit.submittedTxHash);
        await h.applyLanded(fundingCommit.submittedHeaderHash);
        await attestQueuedStateQueueHeader({
          fixture,
          lucidService,
          globals,
          headerHash: fundingCommit.submittedHeaderHash,
        });
        await advanceEmulatorPastUnixTime(
          fixture,
          fundingCommit.blockEndTimeMs,
        );
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
        const latestBlock = await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        );
        const tipEndTimeMs = await getStateQueueDatumEndTime(latestBlock.datum);
        // One instant after the tip's end: oldest follows admission (queued) order.
        const oldestBacklogTimeMs = Math.max(
          Date.now() - 8_000,
          tipEndTimeMs + 1,
        );
        // Advance the emulator clock past the seeded arrival instant so the
        // worker's mempool retrieval window covers the whole backlog even when
        // the tip anchor pushed the instant into the future.
        await advanceEmulatorPastUnixTime(fixture, oldestBacklogTimeMs + 1_000);
        vi.setSystemTime(new Date(fixture.emulator.now()));
        // The node admits the backlog one transaction at a time, in `built`
        // order, every row dated at the same instant.
        for (const tx of built)
          await admitAndAcceptL2Tx(h, tx, new Date(oldestBacklogTimeMs));
        // Admission records each transaction's ledger delta behind its own
        // write; the commit reads the deltas once they are durable.
        await h.runWithoutSynchronizing(
          Effect.flatMap(WriteBehind, (writeBehind) => writeBehind.flushNow),
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
          nodeConfig: production.nodeConfig,
          production: { ...production, globals },
        });
        const cacheHitsAfter = await Effect.runPromise(
          Metric.value(commitTxDeltaCacheHitCounter),
        );
        const fallbackAfter = await Effect.runPromise(
          Metric.value(commitTxDeltaFallbackDecodedCounter),
        );
        expect(output.mempoolTxsCount).toBe(2);
        expect(cacheHitsAfter.count - cacheHitsBefore.count).toBe(2n);
        expect(fallbackAfter.count - fallbackBefore.count).toBe(0n);
        const active = await runNodeDatabaseEffect(
          PendingBlockFinalizationsDB.retrieveByHeaderHash(
            Buffer.from(output.submittedHeaderHash, "hex"),
          ),
        );
        expect(Option.isSome(active)).toBe(true);
        if (Option.isSome(active)) {
          expect(active.value.mempoolTxIds).toStrictEqual(
            built.slice(0, 2).map((tx) => tx.txId),
          );
        }
        expect(output.blockEndTimeMs).toBeGreaterThanOrEqual(
          oldestBacklogTimeMs,
        );
      } finally {
        await closeLifecycle(h);
      }
    } finally {
      if (previousPageSize === undefined) {
        delete process.env.MEMPOOL_RETRIEVE_PAGE_SIZE;
      } else {
        process.env.MEMPOOL_RETRIEVE_PAGE_SIZE = previousPageSize;
      }
    }
  }, 900_000);
});

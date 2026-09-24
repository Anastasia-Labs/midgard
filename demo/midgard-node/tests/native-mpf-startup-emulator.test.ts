import { mkdirSync, writeFileSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import type { TxSignBuilder, UTxO } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import { MempoolLedgerDB } from "../src/database/index.js";
import type { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.js";
import { initializeArchitectureGOwner } from "../src/services/native-mpf-startup.js";
import { fetchStateQueueSnapshotProgram } from "../src/services/state-queue-topology.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  assetsToValue,
  attestQueuedStateQueueHeader,
  CML,
  commitConfirmRecoverAndMerge,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  mergeMaturityWindow,
  paymentCredentialOf,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runMergeUntilMerged,
  SDK,
  utxosProgram,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

/** Actual public admissions and native promotion establish the empty restart
 * state. Network transport labels remain synthetic, as in the shared fixture. */
it("restarts the production native initializer after withdrawal empties an unmerged and merged ledger without replaying genesis, and refuses a missing initialized store", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  const { fixture, lucidService, globals, production } = h;
  const context = { fixture, lucidService, globals, production };
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const histories = SDK.requireEventHistoryContracts(fixture.contracts);
  const replacements: ProductionNativeMpfOwnerService[] = [];
  const missingRoot = await mkdtemp(
    join(tmpdir(), "midgard-native-startup-missing-"),
  );
  const diagnostic: Record<string, unknown> = { stage: "deposit" };
  const native = async () => {
    const value = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    if (value === undefined) throw new Error("Expected live native owner");
    return value;
  };
  const ledger = () =>
    h.command(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return {
          spendable: yield* MempoolLedgerDB.retrieveSpendable,
          rows: yield* sql`SELECT outref, output, source_event_id FROM mempool_ledger ORDER BY outref`,
          marker:
            yield* sql`SELECT store_name, migration_version, root_hex, audit_diverged FROM mpf_engine_state WHERE store_name = 'ledger'`,
        };
      }),
    );
  const queue = () =>
    Effect.runPromise(
      fetchStateQueueSnapshotProgram(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
        "startup",
      ),
    );
  const submit = async (built: { tx: TxSignBuilder }) => {
    const signed = await built.tx.sign.withWallet().complete();
    const hash = await signed.submit();
    expect(await wallet.awaitTx(hash)).toBe(true);
    wallet.overrideUTxOs(await wallet.utxosAt(address));
    await h.synchronize();
    return hash;
  };
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    const deposit = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    diagnostic.depositHash = await submit(deposit);
    const admitted = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        fixture.operatorLucid,
        SDK.eventHistoryDeploymentFromContracts(histories.deposit),
      ),
    );
    expect(admitted).toHaveLength(1);
    await h.deployment.chain.awaitLedgerTime(
      Number(admitted[0]!.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    diagnostic.depositSettlement = await commitConfirmRecoverAndMerge(context);
    const l2 = await h.command(utxosProgram(address));
    expect(l2.utxoCount).toBe(1);
    expect(l2.totals.lovelace).toBe(12_000_000n);
    const target = l2.utxos[0]!;
    const nonemptyNative = await (await native()).diagnostics();
    expect(nonemptyNative.durableRoot).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    diagnostic.nonemptyNative = nonemptyNative;
    // Explicit reload sentinel: this is the actual subsequently withdrawn L2
    // output, not a claim that the published empty genesis originally held it.
    const sentinel: UTxO = {
      txHash: target.txHash,
      outputIndex: target.outputIndex,
      address,
      assets: { lovelace: 12_000_000n },
    };
    const addressData = await Effect.runPromise(
      SDK.addressDataFromBech32(address),
    );
    const body: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: target.txHash,
        outputIndex: BigInt(target.outputIndex),
      },
      l2_owner: paymentCredentialOf(address).hash,
      l2_value: assetsToValue(sentinel.assets),
      l1_address: addressData,
      l1_datum: "NoDatum",
    };
    const key = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Preprod",
      }).paymentKey,
    );
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "withdrawal");
    const withdrawal = await Effect.runPromise(
      SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
        wallet,
        fixture.contracts,
        {
          body,
          signature: SDK.signWithdrawalBody(key, body),
          refundAddress: addressData,
          refundDatum: "NoDatum",
          referenceScripts: fixture.referenceScripts.withdrawal,
        },
      ),
    );
    diagnostic.withdrawalHash = await submit(withdrawal);
    const withdrawals = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(
        fixture.operatorLucid,
        SDK.eventHistoryDeploymentFromContracts(histories.withdrawal),
      ),
    );
    expect(withdrawals).toHaveLength(1);
    await h.deployment.chain.awaitLedgerTime(
      Number(withdrawals[0]!.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    diagnostic.stage = "withdrawal-native-promotion";
    const commit = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      ),
      nodeConfig: production.nodeConfig,
      production: { ...production, globals },
    });
    expect(await fixture.operatorLucid.awaitTx(commit.submittedTxHash)).toBe(
      true,
    );
    await h.synchronize();
    await runBlockConfirmation(
      globals,
      fixture.contracts,
      lucidService,
      production.nodeConfig,
      production,
    );
    const recovery = await runLocalFinalizationRecoveryWorker(
      globals,
      fixture.contracts,
      lucidService,
      fixture.runtimeOverrides!.deploymentIdentity,
      production.nodeConfig,
      { ...production, globals },
    );
    expect(recovery.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
    diagnostic.withdrawalCommit = { commit, recovery };
    expect((await (await native()).diagnostics()).durableRoot).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    expect((await ledger()).spendable).toHaveLength(0);
    const unmerged = await queue();
    expect(unmerged.topology.parsedNodeCount).toBe(2);
    expect(unmerged.tailCommitBase.roots.utxosRoot).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    const restart = async (stage: string) => {
      diagnostic.stage = stage;
      const before = await ledger();
      const beforeQueue = await queue();
      await (await native()).close();
      const restarted = await h.command(
        initializeArchitectureGOwner(globals, {
          ...production.nodeConfig,
          GENESIS_UTXOS: [sentinel],
        }),
      );
      if (restarted === undefined)
        throw new Error("Expected restarted native owner");
      replacements.push(restarted);
      expect((await restarted.diagnostics()).durableRoot).toBe(
        SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      expect(await ledger()).toEqual(before);
      expect(await queue()).toEqual(beforeQueue);
      diagnostic[stage] = {
        before,
        after: await ledger(),
        queue: beforeQueue,
        native: await restarted.diagnostics(),
        configuredGenesisSentinel: sentinel,
      };
    };
    diagnostic.ownerBeforeRestart = await h.evidence();
    await restart("restart-unmerged-empty");
    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals,
      headerHash: unmerged.tailCommitBase.headerHash!,
    });
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(
        fixture.operatorLucid,
        unmerged.tailCommitBase.blockEndTimeMs,
      ).readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());
    const merged = await runMergeUntilMerged({
      fixture,
      lucidService,
      globals,
      production,
    });
    expect(merged.postMergeSnapshot.topology.parsedNodeCount).toBe(1);
    await restart("restart-merged-empty");
    diagnostic.stage = "missing-initialized-store";
    const beforeMissing = await ledger();
    const rejected = await h.command(
      Effect.either(
        initializeArchitectureGOwner(globals, {
          ...production.nodeConfig,
          GENESIS_UTXOS: [sentinel],
          LEDGER_MPF_DB_PATH: join(missingRoot, "absent-ledger"),
          MPF_NATIVE_OWNER_SIDECAR_PATH: join(missingRoot, "absent.sidecar"),
        }),
      ),
    );
    if (rejected._tag === "Right" && rejected.right !== undefined)
      replacements.push(rejected.right);
    diagnostic.missingStoreResult =
      rejected._tag === "Left"
        ? inspect(rejected.left, { depth: 20 })
        : "unexpected native owner created";
    expect(rejected._tag).toBe("Left");
    expect(await ledger()).toEqual(beforeMissing);
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    try {
      const path = process.env.MIDGARD_NATIVE_STARTUP_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual public deposit/withdrawal and native promotion to empty; production initializer restarts under same source owner and real unmerged/merged queue. Nonempty reload genesis is explicitly a sentinel, not the deployment genesis. Synthetic network transport ancestry.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              binding: h.binding,
              diagnostic,
              receipts: h.receipts,
              transitions: h.transitions,
              nativeBinarySha256:
                production.nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
            },
            (_key, value) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ) + "\n",
        );
      }
    } finally {
      for (const replacement of replacements) await replacement.close();
      await h.close();
      await rm(missingRoot, { recursive: true, force: true });
      vi.useRealTimers();
    }
  }
});

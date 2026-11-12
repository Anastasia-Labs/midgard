import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/workers/utils/commit-end-time.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceEmulatorToDueWork,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  CML,
  Data,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  mergeMaturityWindow,
  runCommitWorker,
  runMergeUntilMerged,
  SDK,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openNodeServices } from "./helpers/commit-replacement-state-queue.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";
import {
  expectSignedCommit,
  readSignedCommitJournal,
} from "./helpers/signed-commit-journal.js";

/** The provider accepts the real, locally evaluated commitment before its
 * response is lost. Network ancestry remains the harness's synthetic transport;
 * this case does not simulate restart or a state-queue pointer continuation. */
it("retains the exact durable signed commitment after an accepted submission loses its response, then applies it at landing and merges it", async () => {
  const h = await openProductionLifecycle();
  const { fixture, lucidService, globals, production } = h;
  const lucid = fixture.operatorLucid;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const diagnostic: Record<string, unknown> = { stage: "deposit-admission" };
  const observingSubmit = fixture.emulator.submitTx;
  const responseLoss =
    "fixture response lost after emulator accepted signed commitment";
  let providerCalls = 0;
  let accepted:
    | {
        txHash: string;
        signedCbor: string;
        header: SDK.Header;
        headerHash: string;
      }
    | undefined;

  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signedDeposit = await built.tx.sign.withWallet().complete();
    const depositHash = await signedDeposit.submit();
    expect(await wallet.awaitTx(depositHash)).toBe(true);
    await h.synchronize();
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).deposit,
        ),
      ),
    );
    expect(deposits).toHaveLength(1);
    const deposit = deposits[0]!;
    expect(deposit.originalAssets).toEqual({ lovelace: 12_000_000n });
    await h.deployment.chain.awaitLedgerTime(
      Number(deposit.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const latestBefore = await fetchLatestCommittedBlock(
      lucid,
      fixture.contracts,
    );
    let receiptsBefore = h.receipts.length;
    diagnostic.deposit = { depositHash, deposit, latestBefore };
    diagnostic.stage = "accepted-response-loss";

    fixture.emulator.submitTx = async (signedCbor) => {
      const transaction = CML.Transaction.from_cbor_hex(signedCbor);
      const body = transaction.body();
      const mint = body.mint();
      const policy = CML.ScriptHash.from_hex(
        fixture.contracts.stateQueue.policyId,
      );
      const assets = mint?.get_assets(policy);
      if (assets === undefined)
        return observingSubmit.call(fixture.emulator, signedCbor);
      providerCalls += 1;
      expect(providerCalls).toBe(1);
      const txHash = CML.hash_transaction(body).to_hex();
      const journal = await readSignedCommitJournal(txHash);
      expectSignedCommit(journal, txHash, signedCbor);
      expect(journal.status).toBe(Pending.OwnBlockStatus.Unlanded);
      const { row } = journal;
      const headerHash = row[Pending.Columns.HEADER_HASH].toString("hex");
      const unit =
        fixture.contracts.stateQueue.policyId +
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        headerHash;
      const outputs = Array.from({ length: body.outputs().len() }, (_, index) =>
        coreToTxOutput(body.outputs().get(index)),
      ).filter((output) => output.assets[unit] === 1n);
      expect(outputs).toHaveLength(1);
      const header = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(
          SDK.linkedListDatumToNodeView(
            Data.from(outputs[0]!.datum!, SDK.LinkedListDatum),
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
          ),
        ),
      );
      expect(await Effect.runPromise(SDK.hashBlockHeader(header))).toBe(
        headerHash,
      );
      expect(
        Data.from(row[Pending.Columns.HEADER_CBOR].toString("hex"), SDK.Header),
      ).toEqual(header);
      diagnostic.beforeProvider = {
        row,
        txHash,
        signedCbor,
        header,
        observedSlot: fixture.emulator.slot,
      };
      const returnedHash = await observingSubmit.call(
        fixture.emulator,
        signedCbor,
      );
      expect(returnedHash).toBe(txHash);
      accepted = { txHash, signedCbor, header, headerHash };
      diagnostic.accepted = accepted;
      diagnostic.providerStatusAfterAcceptance =
        await lucid.transactionStatus(txHash);
      throw new Error(responseLoss);
    };

    // The event wait can place the commitment beyond the current scheduler
    // window. The history-owned worker then registers due scheduler work and
    // leaves the refresh to the block-commitment fiber's pre-lease alignment,
    // which runs at the due slot before the worker is re-run. No state-queue
    // mint reaches the provider before that re-run.
    // The node's services as `runNode` provides them: the follower's
    // journal records the commit's signed bytes before they reach the
    // provider, over the wallet view the node signs with.
    const node = await openNodeServices(h);
    let output: Awaited<ReturnType<typeof runCommitWorker>>;
    const dueWorkOutputs: unknown[] = [];
    try {
      for (let attempt = 1; ; attempt += 1) {
        // Scheduler refreshes are L1 receipts of their own; the worker run
        // that reaches the provider must add none.
        receiptsBefore = h.receipts.length;
        output = await runCommitWorker(
          fixture.contracts,
          node.nodeLucid,
          latestBefore,
          production.nodeConfig,
          fixture.runtimeOverrides!.deploymentIdentity,
          { ...production, globals },
          node.journal,
        );
        if (output.type !== "RegisteredDueWorkOutput") break;
        expect(providerCalls).toBe(0);
        expect(attempt).toBeLessThan(4);
        dueWorkOutputs.push(output);
        await advanceEmulatorToDueWork(fixture, output.dueWork);
        await alignCommitSchedulerBeforeTestWorker({
          fixture,
          lucidService,
          targetEndTimeMs:
            fixture.emulator.now() + COMMIT_MINIMUM_FUTURE_BUFFER_MS,
        });
        await h.synchronize();
      }
    } finally {
      fixture.emulator.submitTx = observingSubmit;
      await node.close();
    }
    diagnostic.dueWorkOutputs = dueWorkOutputs;
    diagnostic.workerOutput = output;
    expect(providerCalls).toBe(1);
    expect(accepted).toBeDefined();
    if (accepted === undefined)
      throw new Error("Commitment did not reach actual provider acceptance");
    expect(output.type).toBe("FailureOutput");
    if (output.type !== "FailureOutput")
      throw new Error("Lost response must remain unacknowledged");
    expect(output.error).toContain(
      "Signed commit intent retained for canonical reconciliation",
    );
    expect(output.error).toContain(responseLoss);
    const pending = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(pending, accepted.txHash, accepted.signedCbor);
    expect(pending.status).toBe(Pending.OwnBlockStatus.Unlanded);
    expect(h.receipts).toHaveLength(receiptsBefore);
    expect(h.observer.pendingCount()).toBe(1);
    expect((await lucid.transactionStatus(accepted.txHash)).status).toBe(
      "pending",
    );
    diagnostic.afterResponseLoss = pending;

    diagnostic.stage = "landing";
    expect(await lucid.awaitTx(accepted.txHash)).toBe(true);
    await h.applyLanded(accepted.headerHash);
    const confirmedReceipts = h.receipts.filter(
      ({ transaction }) => transaction.txHash === accepted!.txHash,
    );
    expect(confirmedReceipts).toHaveLength(1);
    expect(confirmedReceipts[0]!.signedCbor).toBe(accepted.signedCbor);
    const canonical = await fetchLatestCommittedBlock(lucid, fixture.contracts);
    expect(
      await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(canonical.datum),
      ),
    ).toEqual(accepted.header);
    expect(accepted.header.depositsRoot).toBe(
      await expectedAuthenticatedEventRoot(SDK.ROOT_DOMAINS.deposits, [
        { key: deposit.idCbor, value: deposit.infoCbor },
      ]),
    );
    // The lost response never recorded the submission: the journal keeps
    // its signed intent, and landing applied it.
    const applied = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(applied, accepted.txHash, accepted.signedCbor);
    expect(applied.status).toBe(Pending.OwnBlockStatus.Applied);
    const native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    expect(native).toBeDefined();
    expect((await native!.diagnostics()).durableRoot).toBe(
      accepted.header.utxosRoot,
    );
    const l2 = await h.command(utxosProgram(address));
    expect(l2.utxoCount).toBe(1);
    expect(l2.totals.lovelace).toBe(12_000_000n);
    diagnostic.landing = {
      journal: applied,
      native: await native!.diagnostics(),
      l2,
    };

    diagnostic.stage = "mature-merge";
    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals,
      headerHash: accepted.headerHash,
    });
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(lucid, Number(accepted.header.endTime))
        .readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());
    const merged = await runMergeUntilMerged({
      fixture,
      lucidService,
      globals,
      production,
    });
    expect(merged.postMergeSnapshot.blockCount).toBe(0);
    const settled = await lucid.utxosAtWithUnit(
      fixture.contracts.settlement.spendingScriptAddress,
      fixture.contracts.settlement.policyId + accepted.headerHash,
    );
    expect(settled).toHaveLength(1);
    const finalRow = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(finalRow, accepted.txHash, accepted.signedCbor);
    expect(finalRow.status).toBe(Pending.OwnBlockStatus.Folded);
    expect(
      h.receipts.filter(
        ({ transaction }) => transaction.txHash === accepted!.txHash,
      ),
    ).toHaveLength(1);
    expect(providerCalls).toBe(1);
    expect(h.observer.pendingCount()).toBe(0);
    diagnostic.final = { journal: finalRow, settlement: settled[0], merged };
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    fixture.emulator.submitTx = observingSubmit;
    try {
      const path = process.env.MIDGARD_SIGNED_INTENT_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual emulator acceptance followed by one lost provider response; independent durable pre-provider SQL read and production landed-block application/mature merge. Synthetic transport ancestry. No restart or state-queue continuation coverage.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              providerCalls,
              receipts: h.receipts,
              production: await h.evidence(),
            },
            (_key, value) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ) + "\n",
        );
      }
    } finally {
      try {
        await h.close();
      } finally {
        vi.useRealTimers();
      }
    }
  }
});

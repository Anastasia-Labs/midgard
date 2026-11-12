import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Option, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Database } from "../src/services/database.js";
import {
  FOLLOWER_VIEW_STALE,
  followerWriteHoldOf,
  runAtFollowerView,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/workers/utils/commit-end-time.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorToDueWork,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  CML,
  Data,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  runCommitWorker,
  SDK,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openNodeServices } from "./helpers/commit-replacement-state-queue.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";
import {
  expectSignedCommit,
  readSignedCommitJournal,
} from "./helpers/signed-commit-journal.js";

/** Recreate the follower-change driver, Globals, cache, managed runtime and native child using
 * retained SQL/Level state after a real provider response loss. The test process
 * and accepted emulator chain remain alive; this is not an OS-process crash. */
it("recreates production services after accepted response loss, preserves the intent, then applies the landed header on the restarted node", async () => {
  const initial = await openProductionLifecycle();
  let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
  const { fixture, lucidService } = initial;
  let { globals, production } = h;
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
      // The journal is live before its commit reaches the provider.
      const live = await Effect.runPromise(
        Pending.retrieveLive().pipe(Effect.provide(Database.layer)),
      );
      expect(
        Option.getOrThrow(live)[Pending.Columns.HEADER_HASH].toString("hex"),
      ).toBe(headerHash);
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
    const node = await openNodeServices(initial);
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

    diagnostic.stage = "service-restart-before-landing-processed";
    const beforeRestart = await h.evidence();
    expect(beforeRestart.native?.durableRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(beforeRestart.native?.durableRoot).not.toBe(
      accepted.header.utxosRoot,
    );
    diagnostic.beforeRestart = beforeRestart;
    // L1 confirms independently; the node has not followed it yet.
    expect(await lucid.awaitTx(accepted.txHash)).toBe(true);
    const beforeDisposal = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(beforeDisposal, accepted.txHash, accepted.signedCbor);
    expect(beforeDisposal.status).toBe(Pending.OwnBlockStatus.Unlanded);
    h = await initial.restartRuntime();
    ({ globals, production } = h);
    expect(h.globals).not.toBe(initial.globals);
    expect(h.production.cache).not.toBe(initial.production.cache);
    expect(h.production.nodeConfig).toEqual(initial.production.nodeConfig);
    expect(h.fixture).toBe(initial.fixture);
    expect(h.deployment).toBe(initial.deployment);
    await expect(initial.synchronize()).rejects.toThrow(
      "runtime generation is closed",
    );
    // The stopped generation's producers are refused: the next generation's
    // driver took a new gate epoch.
    const oldProducer = await Effect.runPromise(
      runAtFollowerView(Effect.succeed(true)).pipe(
        Effect.either,
        Effect.provideService(Globals, initial.globals),
        Effect.provide(Database.layer),
      ),
    );
    expect(oldProducer._tag).toBe("Left");
    if (oldProducer._tag === "Left")
      expect(followerWriteHoldOf(oldProducer.left)?.reason).toBe(
        FOLLOWER_VIEW_STALE,
      );
    const restartedEvidence = await h.evidence();
    expect(restartedEvidence.generation).toBe(1);
    expect(restartedEvidence.native?.ownerEpoch).not.toEqual(
      beforeRestart.native?.ownerEpoch,
    );
    // The restarted node's first synchronization takes the landed block in:
    // its rebase applies it, and the journal keeps its signed intent.
    expect(restartedEvidence.native?.durableRoot).toBe(
      accepted.header.utxosRoot,
    );
    const restartedApplied = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(restartedApplied, accepted.txHash, accepted.signedCbor);
    expect(restartedApplied.status).toBe(Pending.OwnBlockStatus.Applied);
    expect(
      await Effect.runPromise(
        Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      ),
    ).toBe("");
    diagnostic.afterRestart = {
      journal: restartedApplied,
      evidence: restartedEvidence,
      oldProducer,
    };
    diagnostic.stage = "landed";
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
    // Taken in once: the hook, run again, holds nothing and changes nothing.
    await h.applyLanded(accepted.headerHash);
    expect(await readSignedCommitJournal(accepted.txHash)).toEqual(
      restartedApplied,
    );
    const native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    expect(native).toBeDefined();
    expect((await native!.diagnostics()).durableRoot).toBe(
      accepted.header.utxosRoot,
    );
    const l2 = await h.command(utxosProgram(address));
    expect(l2.utxoCount).toBe(1);
    expect(l2.totals.lovelace).toBe(12_000_000n);
    diagnostic.landed = {
      journal: await readSignedCommitJournal(accepted.txHash),
      native: await native!.diagnostics(),
      l2,
    };

    const finalRow = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(finalRow, accepted.txHash, accepted.signedCbor);
    expect(finalRow.status).toBe(Pending.OwnBlockStatus.Applied);
    expect(providerCalls).toBe(1);
    expect(h.observer.pendingCount()).toBe(0);
    diagnostic.final = { journal: finalRow };
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    fixture.emulator.submitTx = observingSubmit;
    try {
      const path = process.env.MIDGARD_SIGNED_INTENT_RESTART_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual emulator acceptance followed by one lost provider response, disposal and recreation of the follower-change driver/Globals/cache/runtime/native child using unchanged SQL/Level/deployment state, whose first synchronization applies the landed header. Synthetic transport ancestry; no OS-process kill, queue continuation, or production failure-to-lease classification coverage.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              providerCalls,
              receipts: h.receipts,
              production: await h.evidence().catch((cause: unknown) => ({
                unavailableAfterFailedRestart: inspect(cause, { depth: 20 }),
              })),
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

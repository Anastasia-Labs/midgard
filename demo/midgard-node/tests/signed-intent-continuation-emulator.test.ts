import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  resolveHistoryCommitBlockTime,
} from "../src/workers/utils/commit-end-time.js";
import { resolveCurrentOperatorSchedulerWindow } from "../src/workers/utils/scheduler-refresh.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceEmulatorToDueWork,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  CML,
  commitExplicitBlockHeaderProgram,
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
import {
  signedIntentContinuationChildEvents,
  signedIntentContinuationChildRoots,
} from "./helpers/signed-intent-continuation-child-da.js";

/** Real accepted commitment with a lost provider response, applied when it
 * lands, then an authorized event-only child recreating the parent outref.
 * Synthetic transport ancestry; the parent's journal stays as landing left
 * it. */
it("retains the original signed intent through landing and an accepted queue pointer continuation", async () => {
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

    // Due scheduler work is aligned before the next owned worker attempt;
    // no state-queue mint reaches the provider before that re-run.
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

    diagnostic.stage = "original-lands";
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
    const priorToContinuation = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(
      priorToContinuation,
      accepted.txHash,
      accepted.signedCbor,
    );
    expect(priorToContinuation.status).toBe(Pending.OwnBlockStatus.Applied);
    const nativeBefore = await Effect.runPromise(
      Ref.get(globals.NATIVE_MPF_OWNER),
    );
    expect(nativeBefore).toBeDefined();
    const nativeBeforeContinuation = await nativeBefore!.diagnostics();
    expect(nativeBeforeContinuation.durableRoot).toBe(
      accepted.header.utxosRoot,
    );
    expect(canonical.utxo.txHash).toBe(accepted.txHash);
    expect(canonical.datum.next).toBe("Empty");

    diagnostic.stage = "authorized-child";
    await advanceEmulatorPastUnixTime(fixture, Number(accepted.header.endTime));
    vi.setSystemTime(fixture.emulator.now());
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
    // At the parent's TTL the follower driver leaves the applied journal
    // and its signed intent as they are.
    const recordedAtTtl = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(recordedAtTtl, accepted.txHash, accepted.signedCbor);
    expect(recordedAtTtl).toEqual(priorToContinuation);
    // The child takes the latest planner end allowed by the Q61 attestation
    // fence, history horizon, submit validity and current scheduler window.
    const childWindow = await Effect.runPromise(
      resolveCurrentOperatorSchedulerWindow(lucid, fixture.contracts),
    );
    if (childWindow === undefined)
      throw new Error("The operator has no current scheduler window");
    const childSlot = fixture.emulator.slot;
    const parentEndTimeMs = Number(accepted.header.endTime);
    const childEndTime = resolveHistoryCommitBlockTime({
      lucid,
      currentSlot: childSlot,
      latestEndTime: parentEndTimeMs,
      nowMs: fixture.emulator.now(),
      minimumFutureBufferMs: HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      schedulerWindowEndTimeMs: childWindow.endTimeMs,
      appendFenceEndTimeMs:
        parentEndTimeMs + Number(SDK.DA_ATTESTATION_TIMEOUT_MS) - 1,
    });
    diagnostic.childEndTime = childEndTime;
    expect(childEndTime.status).toBe("fits");
    // The child preserves the actual parent ledger without repeating its
    // deposit; its one event is a placeholder withdrawal.
    const childRoots = await h.command(
      signedIntentContinuationChildRoots(
        signedIntentContinuationChildEvents(accepted.header.utxosRoot),
      ),
    );
    const beforeChildReceiptCount = h.receipts.length;
    expect(Object.keys(fixture.emulator.mempool)).toHaveLength(0);
    const child = await h.command(
      commitExplicitBlockHeaderProgram({
        utxosRoot: accepted.header.utxosRoot,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        ...childRoots,
        withdrawalCount: 1n,
        endTimeMs: childEndTime.resolvedEndTime - 1,
        awaitConfirmation: true,
      }),
    );
    expect(child.submittedTxHash).not.toBe(accepted.txHash);
    expect(h.receipts).toHaveLength(beforeChildReceiptCount + 1);
    const childReceipts = h.receipts.filter(
      ({ transaction }) => transaction.txHash === child.submittedTxHash,
    );
    expect(childReceipts).toHaveLength(1);
    const childSignedCbor = childReceipts[0]!.signedCbor;
    const childBody = CML.Transaction.from_cbor_hex(childSignedCbor).body();
    expect(CML.hash_transaction(childBody).to_hex()).toBe(
      child.submittedTxHash,
    );
    const childInputs = Array.from(
      { length: childBody.inputs().len() },
      (_, index) => {
        const input = childBody.inputs().get(index);
        return {
          txHash: input.transaction_id().to_hex(),
          outputIndex: Number(input.index()),
        };
      },
    );
    expect(childInputs).toContainEqual({
      txHash: canonical.utxo.txHash,
      outputIndex: canonical.utxo.outputIndex,
    });
    expect(await lucid.utxosByOutRef([canonical.utxo])).toHaveLength(0);
    const parentUnit =
      fixture.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      accepted.headerHash;
    const continued = await lucid.utxosAtWithUnit(
      fixture.contracts.stateQueue.spendingScriptAddress,
      parentUnit,
    );
    expect(continued).toHaveLength(1);
    const parent = continued[0]!;
    expect(parent.txHash).toBe(child.submittedTxHash);
    expect(parent.assets).toEqual(canonical.utxo.assets);
    const parentView = SDK.linkedListDatumToNodeView(
      Data.from(parent.datum!, SDK.LinkedListDatum),
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + accepted.headerHash,
    );
    expect(parentView).toEqual({
      ...canonical.datum,
      next: { Key: { key: child.headerHash } },
    });
    expect(
      await Effect.runPromise(SDK.getHeaderFromStateQueueDatum(parentView)),
    ).toEqual(accepted.header);
    const signedContinuation = coreToTxOutput(
      childBody.outputs().get(parent.outputIndex),
    );
    expect(signedContinuation.address).toBe(parent.address);
    expect(signedContinuation.assets).toEqual(parent.assets);
    expect(signedContinuation.datum).toBe(parent.datum);
    const childNode = await fetchLatestCommittedBlock(lucid, fixture.contracts);
    expect(childNode.utxo.txHash).toBe(child.submittedTxHash);
    const childHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(childNode.datum),
    );
    expect(await Effect.runPromise(SDK.hashBlockHeader(childHeader))).toBe(
      child.headerHash,
    );
    expect(childHeader).toMatchObject({
      prevHeaderHash: accepted.headerHash,
      prevUtxosRoot: accepted.header.utxosRoot,
      utxosRoot: accepted.header.utxosRoot,
      startTime: accepted.header.endTime,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      ...childRoots,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositCount: 0n,
      withdrawalCount: 1n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      validationTraceCount: 0n,
    });
    expect(childHeader.endTime).toBeGreaterThan(accepted.header.endTime);
    expect(childNode.datum.next).toBe("Empty");
    const afterChild = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(afterChild, accepted.txHash, accepted.signedCbor);
    expect(afterChild).toEqual(recordedAtTtl);
    expect((await nativeBefore!.diagnostics()).durableRoot).toBe(
      nativeBeforeContinuation.durableRoot,
    );
    diagnostic.continuation = {
      original: canonical,
      child,
      childHeader,
      childSignedCbor,
      childInputs,
      continuedParent: parent,
      afterChild,
      nativeBeforeContinuation,
      nativeAfterChild: await nativeBefore!.diagnostics(),
      submissionMode:
        "Authorized operator explicit event-only child builder, not automatic pending worker",
    };
    diagnostic.stage = "after-child";
    // The node follows the chain past the child, another block to this
    // node, never taken for the transaction that signed the parent: the
    // parent's journal stays as landing left it.
    const l2 = await h.command(utxosProgram(address));
    expect(l2.utxoCount).toBe(1);
    expect(l2.totals.lovelace).toBe(12_000_000n);
    expect(await readSignedCommitJournal(accepted.txHash)).toEqual(
      recordedAtTtl,
    );
    const native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    expect(native).toBeDefined();
    expect((await native!.diagnostics()).durableRoot).toBe(
      accepted.header.utxosRoot,
    );
    diagnostic.afterChild = {
      journal: await readSignedCommitJournal(accepted.txHash),
      native: await native!.diagnostics(),
      l2,
    };

    const finalRow = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(finalRow, accepted.txHash, accepted.signedCbor);
    // The lost response never recorded the submission, and the recreated
    // outref names the child: the journal keeps its intended hash only.
    expect(finalRow.status).toBe(Pending.OwnBlockStatus.Applied);
    expect(
      h.receipts.filter(
        ({ transaction }) => transaction.txHash === accepted!.txHash,
      ),
    ).toHaveLength(1);
    expect(providerCalls).toBe(1);
    expect(h.observer.pendingCount()).toBe(0);
    diagnostic.final = {
      journal: finalRow,
      continuedParent: parent,
      child,
      native: await native!.diagnostics(),
    };
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    fixture.emulator.submitTx = observingSubmit;
    try {
      const path = process.env.MIDGARD_SIGNED_INTENT_CONTINUATION_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual emulator acceptance followed by one lost provider response; independent durable pre-provider SQL read; the parent applied when it lands; real authorized explicit event-only child submission recreates the parent queue outref. Original signed intent identity preserved through landing and the child's submission; the child is not taken in. Synthetic transport ancestry. No automatic-worker child, service restart, OS kill, or production FailureOutput mutation-lease classification coverage.",
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

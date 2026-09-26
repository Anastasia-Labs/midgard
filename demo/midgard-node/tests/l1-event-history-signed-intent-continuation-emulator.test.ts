import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Database } from "../src/services/database.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  CML,
  commitExplicitBlockHeaderProgram,
  Data,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  runBlockConfirmation,
  runCommitWorker,
  runLocalFinalizationRecoveryWorker,
  SDK,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

/** The provider accepts the real, locally evaluated commitment before its
 * response is lost. Network ancestry remains the harness's synthetic transport;
 * an authorized operator appends a real empty child, which recreates the
 * parent's node outref. The child can only follow the parent's end time, which
 * is the parent commit's signed validity upper bound, so the history owner's
 * signed-intent reconciliation has already recorded the parent's observation
 * from its own node (whichever lands wins). The child uses the explicit
 * builder, not the pending automatic worker. */
it("retains the original signed intent through an accepted queue pointer continuation before local confirmation", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
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

  // A separate Effect/SQL connection proves the intent transaction has committed
  // before provider submission, rather than observing an inherited transaction.
  const readIntent = (txHash: string) =>
    Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows =
          yield* sql<Pending.Row>`SELECT * FROM pending_block_finalizations
        WHERE intended_tx_hash = ${Buffer.from(txHash, "hex")}`;
        expect(rows).toHaveLength(1);
        return rows[0]!;
      }).pipe(Effect.provide(Database.layer)),
    );
  const assertIntent = (
    row: Pending.Row,
    txHash: string,
    signedCbor: string,
  ) => {
    expect(row[Pending.Columns.PREPARED_TX_HASH]?.toString("hex")).toBe(txHash);
    expect(row[Pending.Columns.INTENDED_TX_HASH]?.toString("hex")).toBe(txHash);
    expect(row[Pending.Columns.SIGNED_TX_CBOR]?.toString("hex")).toBe(
      signedCbor,
    );
    expect(
      CML.hash_transaction(
        CML.Transaction.from_cbor_hex(signedCbor).body(),
      ).to_hex(),
    ).toBe(txHash);
    expect(row[Pending.Columns.STATUS]).not.toBe(Pending.Status.Abandoned);
  };

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
    wallet.overrideUTxOs(await wallet.utxosAt(address));
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
    const receiptsBefore = h.receipts.length;
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
      const row = await readIntent(txHash);
      assertIntent(row, txHash, signedCbor);
      expect(row[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
      expect(row[Pending.Columns.STATUS]).toBe(
        Pending.Status.PendingSubmission,
      );
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

    let output: Awaited<ReturnType<typeof runCommitWorker>>;
    try {
      output = await runCommitWorker(
        fixture.contracts,
        lucidService,
        latestBefore,
        production.nodeConfig,
        fixture.runtimeOverrides!.deploymentIdentity,
        { ...production, globals },
      );
    } finally {
      fixture.emulator.submitTx = observingSubmit;
    }
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
    const pending = await readIntent(accepted.txHash);
    assertIntent(pending, accepted.txHash, accepted.signedCbor);
    expect(pending[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
    expect(pending[Pending.Columns.STATUS]).toBe(
      Pending.Status.PendingSubmission,
    );
    expect(h.receipts).toHaveLength(receiptsBefore);
    expect(h.observer.pendingCount()).toBe(1);
    expect((await lucid.transactionStatus(accepted.txHash)).status).toBe(
      "pending",
    );
    diagnostic.afterResponseLoss = pending;

    diagnostic.stage = "confirm-original-on-ledger-only";
    expect(await lucid.awaitTx(accepted.txHash)).toBe(true);
    await h.synchronize();
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
    const priorToContinuation = await readIntent(accepted.txHash);
    assertIntent(priorToContinuation, accepted.txHash, accepted.signedCbor);
    expect(priorToContinuation[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
    expect(priorToContinuation[Pending.Columns.STATUS]).toBe(
      Pending.Status.PendingSubmission,
    );
    const nativeBefore = await Effect.runPromise(
      Ref.get(globals.NATIVE_MPF_OWNER),
    );
    expect(nativeBefore).toBeDefined();
    const nativeBeforeContinuation = await nativeBefore!.diagnostics();
    expect(nativeBeforeContinuation.durableRoot).not.toBe(
      accepted.header.utxosRoot,
    );
    expect(canonical.utxo.txHash).toBe(accepted.txHash);
    expect(canonical.datum.next).toBe("Empty");

    diagnostic.stage = "authorized-empty-child-before-node-confirmation";
    await advanceEmulatorPastUnixTime(fixture, Number(accepted.header.endTime));
    vi.setSystemTime(fixture.emulator.now());
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
    // This source point reaches the parent commit's TTL. Its node is on the
    // queue at the transaction that signed it, so the history owner records
    // that observation; the signed intent is kept.
    const recordedAtTtl = await readIntent(accepted.txHash);
    assertIntent(recordedAtTtl, accepted.txHash, accepted.signedCbor);
    expect(
      recordedAtTtl[Pending.Columns.SUBMITTED_TX_HASH]?.toString("hex"),
    ).toBe(accepted.txHash);
    expect(recordedAtTtl[Pending.Columns.STATUS]).toBe(
      Pending.Status.ObservedWaitingStability,
    );
    // No user or L2 transaction is submitted after the parent. The empty child
    // preserves its actual UTxO root; it does not repeat the parent's deposit.
    const beforeChildReceiptCount = h.receipts.length;
    expect(Object.keys(fixture.emulator.mempool)).toHaveLength(0);
    const child = await h.command(
      commitExplicitBlockHeaderProgram({
        utxosRoot: accepted.header.utxosRoot,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        l2TransactionCount: 0n,
        endTimeMs: fixture.emulator.now() + SDK.EVENT_WAIT_DURATION_MS - 1,
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
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositCount: 0n,
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    });
    expect(childHeader.endTime).toBeGreaterThan(accepted.header.endTime);
    expect(childNode.datum.next).toBe("Empty");
    const priorToConfirmation = await readIntent(accepted.txHash);
    assertIntent(priorToConfirmation, accepted.txHash, accepted.signedCbor);
    expect(priorToConfirmation).toEqual(recordedAtTtl);
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
      priorToConfirmation,
      nativeBeforeContinuation,
      nativeBeforeConfirmation: await nativeBefore!.diagnostics(),
      submissionMode:
        "Authorized operator explicit empty-child builder, not automatic pending worker",
    };
    diagnostic.stage = "canonical-confirmation-through-continuation";
    await runBlockConfirmation(
      globals,
      fixture.contracts,
      lucidService,
      production.nodeConfig,
      production,
    );
    const observed = await readIntent(accepted.txHash);
    assertIntent(observed, accepted.txHash, accepted.signedCbor);
    // Confirmation through the recreated outref must not acknowledge the
    // child as though it were the transaction that signed the parent.
    // Confirmation may touch the row's timestamp; nothing else changes.
    expect({ ...observed, updated_at: undefined }).toEqual({
      ...recordedAtTtl,
      updated_at: undefined,
    });
    expect(
      observed[Pending.Columns.SUBMITTED_TX_HASH]?.toString("hex"),
    ).not.toBe(child.submittedTxHash);
    diagnostic.afterConfirmation = observed;
    diagnostic.stage = "local-finalization";
    const recovery = await runLocalFinalizationRecoveryWorker(
      globals,
      fixture.contracts,
      lucidService,
      fixture.runtimeOverrides!.deploymentIdentity,
      production.nodeConfig,
      { ...production, globals },
    );
    expect(recovery.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
    if (recovery.type !== "SuccessfulLocalFinalizationRecoveryOutput")
      throw new Error("Canonical intent requires native local finalization");
    expect(recovery.finalizedHeaderHash).toBe(accepted.headerHash);
    await h.synchronize();
    const native = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    expect(native).toBeDefined();
    expect((await native!.diagnostics()).durableRoot).toBe(
      accepted.header.utxosRoot,
    );
    const l2 = await h.command(utxosProgram(address));
    expect(l2.utxoCount).toBe(1);
    expect(l2.totals.lovelace).toBe(12_000_000n);
    diagnostic.localFinalization = {
      recovery,
      journal: await readIntent(accepted.txHash),
      native: await native!.diagnostics(),
      l2,
    };

    const finalRow = await readIntent(accepted.txHash);
    assertIntent(finalRow, accepted.txHash, accepted.signedCbor);
    expect(finalRow[Pending.Columns.SUBMITTED_TX_HASH]?.toString("hex")).toBe(
      accepted.txHash,
    );
    expect(finalRow[Pending.Columns.STATUS]).toBe(Pending.Status.Finalized);
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
                "Actual emulator acceptance followed by one lost provider response; independent durable pre-provider SQL read; real authorized explicit empty-child submission recreates the parent queue outref before node acknowledgement/confirmation. Original signed intent identity preserved through canonical observation and native local finalization. Synthetic transport ancestry. No automatic-worker child, service restart, OS kill, or production FailureOutput mutation-lease classification coverage.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              providerCalls,
              receipts: h.receipts,
              transitions: h.transitions,
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

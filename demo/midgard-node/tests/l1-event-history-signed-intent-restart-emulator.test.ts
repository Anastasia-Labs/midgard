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
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  CML,
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

/** Recreate source owner, Globals, cache, managed runtime and native child using
 * retained SQL/Level state after a real provider response loss. The test process
 * and accepted emulator chain remain alive; this is not an OS-process crash. */
it("recreates production services after accepted response loss, preserves intent without promotion, then confirms and finalizes the canonical header", async () => {
  const initial = await openHistoryProductionOwnerLifecycle();
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

    diagnostic.stage = "service-restart-before-local-confirmation";
    const beforeRestart = await h.evidence();
    expect(beforeRestart.native?.durableRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(beforeRestart.native?.durableRoot).not.toBe(
      accepted.header.utxosRoot,
    );
    diagnostic.beforeRestart = beforeRestart;
    // L1 confirms independently; no node confirmation/acknowledgement has run.
    expect(await lucid.awaitTx(accepted.txHash)).toBe(true);
    const beforeDisposal = await readIntent(accepted.txHash);
    assertIntent(beforeDisposal, accepted.txHash, accepted.signedCbor);
    expect(beforeDisposal[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
    h = await initial.restartRuntime();
    ({ globals, production } = h);
    expect(h.globals).not.toBe(initial.globals);
    expect(h.production.owner).not.toBe(initial.production.owner);
    expect(h.production.cache).not.toBe(initial.production.cache);
    expect(h.production.nodeConfig).toEqual(initial.production.nodeConfig);
    expect(h.fixture).toBe(initial.fixture);
    expect(h.binding).toBe(initial.binding);
    await expect(initial.synchronize()).rejects.toThrow(
      "runtime generation is closed",
    );
    const oldProducer = await Effect.runPromise(
      initial.production.owner
        .runProducer(() => Effect.succeed(true))
        .pipe(Effect.either, Effect.provide(Database.layer)),
    );
    expect(oldProducer._tag).toBe("Left");
    const restartedEvidence = await h.evidence();
    expect(restartedEvidence.generation).toBe(1);
    expect(restartedEvidence.native?.ownerEpoch).not.toEqual(
      beforeRestart.native?.ownerEpoch,
    );
    expect(restartedEvidence.native?.durableRoot).toBe(
      beforeRestart.native?.durableRoot,
    );
    expect(restartedEvidence.native?.durableRoot).not.toBe(
      accepted.header.utxosRoot,
    );
    const restartedPending = await readIntent(accepted.txHash);
    assertIntent(restartedPending, accepted.txHash, accepted.signedCbor);
    expect(restartedPending[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
    expect(restartedPending[Pending.Columns.STATUS]).toBe(
      Pending.Status.PendingSubmission,
    );
    expect(
      await Effect.runPromise(
        Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      ),
    ).toBe(accepted.txHash);
    expect(
      await Effect.runPromise(Ref.get(globals.LOCAL_FINALIZATION_PENDING)),
    ).toBe(true);
    diagnostic.afterRestart = {
      journal: restartedPending,
      evidence: restartedEvidence,
      oldProducer,
    };
    diagnostic.stage = "canonical-confirmation";
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
    const priorToConfirmation = await readIntent(accepted.txHash);
    assertIntent(priorToConfirmation, accepted.txHash, accepted.signedCbor);
    expect(priorToConfirmation[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
    await runBlockConfirmation(
      globals,
      fixture.contracts,
      lucidService,
      production.nodeConfig,
      production,
    );
    const observed = await readIntent(accepted.txHash);
    assertIntent(observed, accepted.txHash, accepted.signedCbor);
    expect(observed[Pending.Columns.SUBMITTED_TX_HASH]?.toString("hex")).toBe(
      accepted.txHash,
    );
    expect(observed[Pending.Columns.STATUS]).toBe(
      Pending.Status.ObservedWaitingStability,
    );
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
                "Actual emulator acceptance followed by one lost provider response, disposal and recreation of production source owner/Globals/cache/runtime/native child using unchanged SQL/Level/deployment state, then canonical confirmation/native finalization. Synthetic transport ancestry; no OS-process kill, queue continuation, or production failure-to-lease classification coverage.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              providerCalls,
              receipts: h.receipts,
              transitions: h.transitions,
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

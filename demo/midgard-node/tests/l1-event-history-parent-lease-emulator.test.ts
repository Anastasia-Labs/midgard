import { randomUUID } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import * as Leases from "../src/database/stateQueueMutationLeases.js";
import { classifyCommitWorkerOutputForMutationLease } from "../src/fibers/commit-worker-failure-classification.js";
import { Database } from "../src/services/database.js";
import { HistoryProducer } from "../src/services/event-history-producer.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import { captureCommitWorkerFailure } from "../src/workers/commit-block-header.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  canonicalSlotConfigForLucid,
  CML,
  commitWorkerProgram,
  ContractDeploymentIdentity,
  Data,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchLatestCommittedBlock,
  getStateQueueDatumEndTime,
  mergeMaturityWindow,
  runBlockConfirmation,
  runLocalFinalizationRecoveryWorker,
  runMergeUntilMerged,
  SDK,
  serializeStateQueueUTxO,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

/** In-process composition of the real worker, production owner, parent failure
 * classifier and SQL mutation lease. Provider acceptance and response loss are
 * real emulator actions. Transport ancestry is synthetic; Worker IPC, parent
 * listen scheduling, merge contention and process restart are not exercised. */
it("fails the parent operation lease after accepted response loss while a later lease preserves and then reconciles the signed intent", async () => {
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

  const attempts: { token: string; output?: WorkerOutput }[] = [];
  const runParentComposedAttempt = async (latestBlock: SDK.StateQueueUTxO) => {
    const availableConfirmedBlock = await Effect.runPromise(
      serializeStateQueueUTxO(latestBlock),
    );
    const currentBlockStartTimeMs = await getStateQueueDatumEndTime(
      latestBlock.datum,
    );
    return Effect.runPromise(
      Leases.tryWithLease(
        "history-parent-lease-fixture",
        (stateQueueLeaseToken) => {
          const attempt: (typeof attempts)[number] = {
            token: stateQueueLeaseToken,
          };
          attempts.push(attempt);
          return production.owner.runProducer(
            (token, assertCurrent, coverage) =>
              Effect.gen(function* () {
                const native = yield* Ref.get(globals.NATIVE_MPF_OWNER);
                if (native === undefined)
                  return yield* Effect.die("Missing native owner");
                const port = native.createWorkerPort();
                const output = yield* commitWorkerProgram(
                  fixture.contracts,
                  lucidService,
                  {
                    data: {
                      availableConfirmedBlock,
                      availableLocalFinalizationBlock: "",
                      currentBlockStartTimeMs,
                      forcedValidationSlotConfig: canonicalSlotConfigForLucid(
                        lucidService.api,
                      ),
                      ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
                      localFinalizationPending: false,
                      mempoolTxsCountSoFar: 0,
                      sizeOfProcessedTxsSoFar: 0,
                      stateQueueLeaseToken,
                    },
                    history: { token, coverage },
                    nativeMpf: {
                      port,
                      durableRoot: (yield* Effect.promise(() =>
                        native.diagnostics(),
                      )).durableRoot,
                      ownerBinarySha256:
                        production.nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
                    },
                  },
                  undefined,
                  production.nodeConfig,
                ).pipe(
                  Effect.provideService(HistoryProducer, { token, coverage }),
                  Effect.provideService(MempoolLedgerCache, production.cache),
                  Effect.ensuring(Effect.sync(() => port.close())),
                  captureCommitWorkerFailure,
                );
                attempt.output = output;
                yield* assertCurrent;
                // The real parent classifies before publishing a promotion or globals.
                // These two attempts must fail before any successful output publication.
                return yield* classifyCommitWorkerOutputForMutationLease({
                  output,
                  stateQueueLeaseToken,
                  retrieveJournalEvidence: (leaseToken) =>
                    Pending.retrieveByStateQueueLeaseToken(leaseToken).pipe(
                      Effect.map((rows) =>
                        rows.map((row) => ({
                          headerHash: row[Pending.Columns.HEADER_HASH],
                          submittedTxHash:
                            row[Pending.Columns.SUBMITTED_TX_HASH],
                          status: row[Pending.Columns.STATUS],
                        })),
                      ),
                    ),
                });
              }),
          );
        },
      ).pipe(
        Effect.either,
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make(
            fixture.runtimeOverrides!.deploymentIdentity,
          ),
        ),
        Effect.provide(Database.layer),
      ),
    );
  };
  const readLease = (token: string) =>
    Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows =
          yield* sql<Leases.Entry>`SELECT * FROM state_queue_mutation_leases WHERE token = ${token}`;
        expect(rows).toHaveLength(1);
        return rows[0]!;
      }).pipe(Effect.provide(Database.layer)),
    );
  // Compare all journal rows, including child payloads, rather than only status.
  const readJournalSnapshot = () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const tables = yield* sql<{
          tablename: string;
        }>`SELECT tablename FROM pg_tables WHERE schemaname = current_schema() AND (tablename = 'pending_block_finalizations' OR tablename LIKE 'pending_block_finalization_%') ORDER BY tablename`;
        expect(tables.length).toBeGreaterThan(1);
        const snapshot: Record<string, readonly unknown[]> = {};
        for (const { tablename } of tables) {
          snapshot[tablename] =
            yield* sql`SELECT to_jsonb(t) AS row FROM ${sql(tablename)} t ORDER BY to_jsonb(t)::text`;
        }
        return snapshot;
      }).pipe(Effect.provide(Database.layer)),
    );

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

    const firstAttempt = await runParentComposedAttempt(latestBefore);
    expect(firstAttempt._tag).toBe("Left");
    if (firstAttempt._tag !== "Left")
      throw new Error("Parent must reject retained-intent worker failure");
    expect(inspect(firstAttempt.left, { depth: 10 })).toContain(
      "Commitment worker failed after durable mutation preparation may have started",
    );
    expect(attempts).toHaveLength(1);
    const firstLease = await readLease(attempts[0]!.token);
    expect(firstLease[Leases.Columns.STATUS]).toBe(Leases.Status.Failed);
    expect(firstLease[Leases.Columns.RELEASED_AT]).not.toBeNull();
    expect(firstLease[Leases.Columns.LAST_ERROR]).toContain(
      "durable mutation preparation",
    );
    const output = attempts[0]!.output;
    if (output === undefined)
      throw new Error("Worker did not return typed failure");
    diagnostic.parentFailure = { firstAttempt, firstLease };
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

    expect(pending[Pending.Columns.STATE_QUEUE_LEASE_TOKEN]).toBe(
      attempts[0]!.token,
    );
    const retainedJournal = await readJournalSnapshot();
    const providerCallsBeforeSecondAttempt = providerCalls;
    diagnostic.stage = "second-operation-before-confirmation";
    const secondAttempt = await runParentComposedAttempt(latestBefore);
    diagnostic.secondAttemptOutcome = {
      result: secondAttempt,
      failure:
        secondAttempt._tag === "Left"
          ? inspect(secondAttempt.left, { depth: 20, colors: false })
          : undefined,
      providerCalls,
    };
    expect(attempts).toHaveLength(2);
    expect(attempts[1]!.token).not.toBe(attempts[0]!.token);
    expect(secondAttempt._tag).toBe("Right");
    if (secondAttempt._tag !== "Right" || secondAttempt.right._tag !== "Ran") {
      throw new Error(
        "A later operation must acquire the lease and run the real worker",
      );
    }
    expect(secondAttempt.right.value.type).toBe("FailureOutput");
    if (secondAttempt.right.value.type !== "FailureOutput")
      throw new Error("Replacement attempt must refuse durable preparation");
    expect(secondAttempt.right.value.error).toMatch(
      /Failed to prepare pending block finalization|Refusing to prepare a new pending block while another active pending-finalization record exists/,
    );
    expect(attempts[1]!.output?.type).toBe("FailureOutput");
    expect(providerCalls).toBe(1);
    const afterSecondJournal = await readJournalSnapshot();
    expect(afterSecondJournal).toEqual(retainedJournal);
    const secondLease = await readLease(attempts[1]!.token);
    expect(secondLease[Leases.Columns.STATUS]).toBe(Leases.Status.Released);
    expect(secondLease[Leases.Columns.RELEASED_AT]).not.toBeNull();
    expect((await lucid.transactionStatus(accepted.txHash)).status).toBe(
      "pending",
    );
    expect(h.receipts).toHaveLength(receiptsBefore);
    diagnostic.secondOperation = {
      secondAttempt,
      secondLease,
      journalBefore: retainedJournal,
      journalAfter: afterSecondJournal,
      providerCallsBefore: providerCallsBeforeSecondAttempt,
      providerCallsAfter: providerCalls,
    };
    fixture.emulator.submitTx = observingSubmit;

    diagnostic.stage = "canonical-confirmation";
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
    const priorToConfirmation = await readIntent(accepted.txHash);
    assertIntent(priorToConfirmation, accepted.txHash, accepted.signedCbor);
    expect(priorToConfirmation[Pending.Columns.SUBMITTED_TX_HASH]).toBeNull();
    let reconciliationLeaseToken: string | undefined;
    const reconciliation = await Effect.runPromise(
      Leases.tryWithLease("history-parent-lease-reconciliation", (token) => {
        reconciliationLeaseToken = token;
        return Effect.tryPromise(() =>
          runBlockConfirmation(
            globals,
            fixture.contracts,
            lucidService,
            production.nodeConfig,
            production,
          ),
        );
      }).pipe(Effect.provide(Database.layer)),
    );
    expect(reconciliation._tag).toBe("Ran");
    expect(reconciliationLeaseToken).toBeDefined();
    expect(reconciliationLeaseToken).not.toBe(attempts[0]!.token);
    const reconciliationLease = await readLease(reconciliationLeaseToken!);
    expect(reconciliationLease[Leases.Columns.STATUS]).toBe(
      Leases.Status.Released,
    );
    diagnostic.reconciliationLease = reconciliationLease;
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
    expect(merged.postMergeSnapshot.topology.parsedNodeCount).toBe(1);
    const settled = await lucid.utxosAtWithUnit(
      fixture.contracts.settlement.spendingScriptAddress,
      fixture.contracts.settlement.policyId + accepted.headerHash,
    );
    expect(settled).toHaveLength(1);
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
    diagnostic.final = { journal: finalRow, settlement: settled[0], merged };
    diagnostic.stage = "complete";
  } catch (error) {
    diagnostic.failure = inspect(error, { depth: 20, colors: false });
    throw error;
  } finally {
    fixture.emulator.submitTx = observingSubmit;
    try {
      const path = process.env.MIDGARD_PARENT_LEASE_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual emulator acceptance and lost response; real in-process parent classifier/lease/SQL composition under production history owner; later operation and canonical reconciliation/native finalization/mature merge. Synthetic ancestry. No actual Worker IPC, full listen scheduling, native parent failure catcher, merge contention or restart acceptance.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              providerCalls,
              attempts,
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

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
import { classifyCommitWorkerOutputAgainstLeaseJournal } from "../src/fibers/commit-worker-failure-classification.js";
import { Database } from "../src/services/database.js";
import {
  FollowerWrite,
  runAtFollowerView,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { IntentJournal } from "../src/services/intent-journal.js";
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
  runMergeUntilMerged,
  SDK,
  serializeStateQueueUTxO,
  utxosProgram,
} from "./deposit-flow-emulator-shared.js";
import { openNodeServices } from "./helpers/commit-replacement-state-queue.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";
import {
  expectSignedCommit,
  readSignedCommitJournal,
} from "./helpers/signed-commit-journal.js";

/** The real worker, follower-change driver, parent failure classifier and SQL
 * mutation lease, in process; provider acceptance and response loss are real
 * emulator actions (no worker IPC, listen, merge contention or restart). */
it("fails the parent operation lease after accepted response loss while a later lease preserves the signed intent, then applies it at landing", async () => {
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

  const attempts: { token: string; output?: WorkerOutput }[] = [];
  const runParentComposedAttempt = async (
    latestBlock: SDK.StateQueueUTxO,
    node: Awaited<ReturnType<typeof openNodeServices>>,
  ) => {
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
          return runAtFollowerView(
            Effect.gen(function* () {
              const permit = yield* FollowerWrite;
              const native = yield* Ref.get(globals.NATIVE_MPF_OWNER);
              if (native === undefined)
                return yield* Effect.die("Missing native owner");
              const port = native.createWorkerPort();
              const output = yield* commitWorkerProgram(
                fixture.contracts,
                node.nodeLucid,
                {
                  data: {
                    availableConfirmedBlock,
                    currentBlockStartTimeMs,
                    forcedValidationSlotConfig: canonicalSlotConfigForLucid(
                      lucidService.api,
                    ),
                    ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
                    mempoolTxsCountSoFar: 0,
                    sizeOfProcessedTxsSoFar: 0,
                    stateQueueLeaseToken,
                  },
                  history: permit,
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
                Effect.provideService(MempoolLedgerCache, production.cache),
                Effect.ensuring(Effect.sync(() => port.close())),
                captureCommitWorkerFailure,
              );
              attempt.output = output;
              // The real parent classifies before publishing a promotion or globals.
              // These two attempts must fail before any successful output publication.
              return yield* classifyCommitWorkerOutputAgainstLeaseJournal(
                globals,
                output,
                stateQueueLeaseToken,
              );
            }),
          ).pipe(Effect.provideService(Globals, globals));
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
        Effect.provideService(IntentJournal, node.journal),
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

    // The node's services as `runNode` provides them: the follower's
    // journal records the commit's signed bytes before they reach the
    // provider, over the wallet view the node signs with.
    const node = await openNodeServices(h);
    const firstAttempt = await runParentComposedAttempt(latestBefore, node);
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
    const pending = await readSignedCommitJournal(accepted.txHash);
    expectSignedCommit(pending, accepted.txHash, accepted.signedCbor);
    expect(pending.status).toBe(Pending.OwnBlockStatus.Unlanded);
    expect(h.receipts).toHaveLength(receiptsBefore);
    expect(h.observer.pendingCount()).toBe(1);
    expect((await lucid.transactionStatus(accepted.txHash)).status).toBe(
      "pending",
    );
    diagnostic.afterResponseLoss = pending;

    expect(pending.row[Pending.Columns.STATE_QUEUE_LEASE_TOKEN]).toBe(
      attempts[0]!.token,
    );
    const retainedJournal = await readJournalSnapshot();
    const providerCallsBeforeSecondAttempt = providerCalls;
    diagnostic.stage = "second-operation-before-confirmation";
    const secondAttempt = await runParentComposedAttempt(latestBefore, node);
    await node.close();
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
    // The lost response's retained signed intent is reconciled before the
    // candidate build's first queue read and stops any preparation.
    expect(secondAttempt.right.value.error).toContain(
      Pending.LIVE_JOURNAL_REFUSAL,
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
      const path = process.env.MIDGARD_PARENT_LEASE_EVIDENCE_PATH;
      if (path !== undefined) {
        mkdirSync(dirname(path), { recursive: true });
        writeFileSync(
          path,
          JSON.stringify(
            {
              scope:
                "Actual emulator acceptance and lost response; real in-process parent classifier/lease/SQL composition under the follower-change driver; later operation, landed-block application and mature merge. Synthetic ancestry. No actual Worker IPC, full listen scheduling, native parent failure catcher, merge contention or restart acceptance.",
              manifestId: h.deployment.manifest.manifestId,
              blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
              deploymentInfoSha256: h.deploymentInfoSha256,
              protocolParameters:
                h.deployment.manifest.cardanoProtocolParameters,
              diagnostic,
              providerCalls,
              attempts,
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

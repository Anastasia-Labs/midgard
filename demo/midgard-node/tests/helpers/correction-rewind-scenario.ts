import { inspect } from "node:util";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option } from "effect";
import { expect, vi } from "vitest";

import type { NodeUtxo } from "../../src/commands/command-utils.js";
import {
  buildTransferTxWithMinFee,
  fetchLocalUtxos,
  toQueuedTx,
} from "../../src/commands/submit-l2-transfer.js";
import type { BuiltTransferTx } from "../../src/commands/transfer-build-core.js";
import {
  ledgerReceiptIsIncomplete,
  orphanHasPublishedDependency,
} from "../../src/database/eventHistoryLedgerRepair.js";
import { TxAdmissionsDB } from "../../src/database/index.js";
import * as MutationJobs from "../../src/database/mutationJobs.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../../src/fibers/attestation-timeout-correction.js";
import { txQueueProcessorDrainOnce } from "../../src/fibers/tx-queue-processor.js";
import { Database } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import type { StateQueueCorrectionObserverSource } from "../../src/services/state-queue-correction-observer.js";
import { validationPoolLayer } from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  createHash,
  Data,
  EMPTY_PROGRAM_MATERIAL_SIDECAR,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  ForcedTransactionsDB,
  paymentCredentialOf,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  SDK,
  walletFromSeed,
} from "../deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import { prepareTimedOutTailRemoval } from "./history-timeout-correction-fixture.js";

export const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

export type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;
type Handle = Pick<
  Lifecycle,
  "fixture" | "lucidService" | "globals" | "production" | "synchronize"
>;
type Removal = Awaited<
  ReturnType<Awaited<ReturnType<typeof prepareTimedOutTailRemoval>>["submit"]>
>;

/** Submit one deposit as the running node's users do; returns its L2
 * inclusion time. */
export const submitDeposit = async (h: Handle, lovelace: bigint) => {
  const { fixture, lucidService } = h;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  // A scheduler refresh pins its predicted change; this flow spends the same
  // wallet before the next one.
  lucidService.api.clearUTxOOverride();
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
      lovelace,
      additionalAssets: {},
      referenceScripts: fixture.referenceScripts.deposit,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  expect(await wallet.awaitTx(await signed.submit())).toBe(true);
  wallet.overrideUTxOs(await wallet.utxosAt(address));
  await h.synchronize();
  return built.metadata.inclusionTime;
};

/** Confirm the committed block and run its local finalization with the
 * production commit worker, as the running node does. Resolves to the
 * worker's output; a worker refusal rejects. */
const runLocalFinalization = async (h: Handle) => {
  const { fixture, lucidService, globals, production } = h;
  await runBlockConfirmation(
    globals,
    fixture.contracts,
    lucidService,
    production.nodeConfig,
    production,
  );
  return runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    fixture.runtimeOverrides!.deploymentIdentity,
    production.nodeConfig,
    { ...production, globals },
  );
};

/** Confirm and locally finalize the block `headerHash`, which must succeed. */
export const finalizeLocally = async (h: Handle, headerHash: string) => {
  const finalized = await runLocalFinalization(h);
  expect(finalized.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
  if (finalized.type !== "SuccessfulLocalFinalizationRecoveryOutput")
    throw new Error("The block must be locally finalized");
  expect(finalized.finalizedHeaderHash).toBe(headerHash);
  await h.synchronize();
  return finalized.finalizedHeaderHash;
};

/** An outref no ledger holds: a spend of it can never apply to any base. */
export const ABSENT_OUTREF_HEX = "ab".repeat(34);

/** The live f5215638 defect, reproduced deterministically: the journal's
 * ledger delta spends an outref absent from its authenticated base, so every
 * local finalization attempt fails before its SQL mutation. */
const corruptLedgerDelta = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = Buffer.from(headerHash, "hex");
      const [row] = yield* sql<{ ledger_delta_spent: unknown }>`SELECT
        ledger_delta_spent FROM pending_block_finalizations
        WHERE header_hash = ${header}`;
      const stored = row!.ledger_delta_spent;
      const spent = (
        typeof stored === "string" ? JSON.parse(stored) : stored
      ) as string[];
      // Written back the way the journal writes it.
      const rows = yield* sql`UPDATE pending_block_finalizations
        SET ledger_delta_spent = ${JSON.stringify([...spent, ABSENT_OUTREF_HEX])}
        WHERE header_hash = ${header}
        RETURNING header_hash`;
      expect(rows).toHaveLength(1);
    }),
  );

export const readLocalFinalizationJob = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<MutationJobs.Entry>`SELECT * FROM
        local_mutation_jobs WHERE job_id = ${MutationJobs.localBlockFinalizationJobId(
          headerHash,
        )}`;
      return rows[0];
    }),
  );

/** Commit the next block on the current state-queue tail with the production
 * owner, as the running node does, then confirm it and locally finalize it.
 * With `failed`, local finalization fails deterministically (the live
 * f5215638 state): a failed job row, the journal still pending, and the
 * ledger never advanced. */
const commitLocallyFinalizedBlock = async (
  h: Pick<Lifecycle, "deployment"> & Handle,
  inclusionTime: number,
  localFinalization: "completed" | "failed" = "completed",
) => {
  const { fixture, lucidService, globals, production } = h;
  await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await h.synchronize();
  const committed = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  expect(await fixture.operatorLucid.awaitTx(committed.submittedTxHash)).toBe(
    true,
  );
  await h.synchronize();
  if (localFinalization === "completed")
    return finalizeLocally(h, committed.submittedHeaderHash);
  const headerHash = committed.submittedHeaderHash;
  await corruptLedgerDelta(headerHash);
  // The commit worker retries a failed finalization while its block is live;
  // two attempts fail the same way.
  for (let attempt = 1; attempt <= 2; attempt += 1) {
    const outcome = await runLocalFinalization(h).then(
      (output) => output,
      (error: unknown) => ({
        type: "Rejected" as const,
        error: inspect(error, { depth: 20 }),
      }),
    );
    expect(outcome.type).not.toBe("SuccessfulLocalFinalizationRecoveryOutput");
    const job = await readLocalFinalizationJob(headerHash);
    expect(
      job?.[MutationJobs.Columns.STATUS],
      inspect(outcome, { depth: 20 }),
    ).toBe(MutationJobs.Status.Failed);
    expect(job?.[MutationJobs.Columns.ATTEMPTS]).toBe(attempt);
    // The job's last error names the underlying reason, not only the
    // DatabaseError's summary.
    expect(job?.[MutationJobs.Columns.LAST_ERROR]).toContain(
      "Pending-finalization ledger delta is invalid for its authenticated base",
    );
    expect(job?.[MutationJobs.Columns.LAST_ERROR]).toContain(
      `ledger delta spends an outref absent from its authenticated base: ${ABSENT_OUTREF_HEX}`,
    );
  }
  await h.synchronize();
  return headerHash;
};

/**
 * Commit the next block with the production commit worker and hand its signed
 * commit to L1, which then loses it: the transaction leaves the emulator
 * mempool without ever landing, as a commit does when a conflicting removal of
 * its parent wins. The journal keeps the submission and its signed intent.
 */
export const submitUnlandedBlock = async (
  h: Pick<Lifecycle, "deployment" | "observer"> & Handle,
  inclusionTime: number,
) => {
  const { fixture, lucidService, globals, production } = h;
  await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await h.synchronize();
  const committed = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  dropPendingEmulatorTransaction(fixture.emulator, committed.submittedTxHash);
  h.observer.forgetDropped(committed.submittedTxHash);
  // A wallet view pinned to the lost commit's predicted change is stale.
  lucidService.api.clearUTxOOverride();
  fixture.operatorLucid.clearUTxOOverride();
  expect(
    (await fixture.operatorLucid.transactionStatus(committed.submittedTxHash))
      .status,
  ).not.toBe("confirmed");
  await h.synchronize();
  return committed;
};

type EmulatorLedger = Record<
  string,
  { utxo: { txHash: string }; spent: boolean }
>;

/** Drop the only pending transaction from the emulator: its outputs leave the
 * mempool and the ledger inputs it marked spent are unspent again. Between
 * blocks, every spent ledger entry belongs to a pending transaction. */
export const dropPendingEmulatorTransaction = (
  emulator: Lifecycle["fixture"]["emulator"],
  txHash: string,
) => {
  const state = emulator as unknown as {
    ledger: EmulatorLedger;
    mempool: EmulatorLedger;
    transactionHistory: Record<string, { status: string }>;
  };
  const pending = Object.entries(state.transactionHistory).filter(
    ([, status]) => status.status === "pending",
  );
  expect(pending.map(([hash]) => hash)).toEqual([txHash]);
  for (const [outRef, entry] of Object.entries(state.mempool)) {
    expect(entry.utxo.txHash).toBe(txHash);
    delete state.mempool[outRef];
  }
  for (const entry of Object.values(state.ledger)) entry.spent = false;
  delete state.transactionHistory[txHash];
};

/** Advance to just after the next operator shift starts. */
const advanceToNextShift = async (h: Handle) => {
  const { fixture } = h;
  const scheduler = await Effect.runPromise(
    SDK.fetchSchedulerUTxOProgram(fixture.operatorLucid, {
      schedulerAddress: fixture.contracts.scheduler.spendingScriptAddress,
      schedulerPolicyId: fixture.contracts.scheduler.policyId,
    }),
  );
  if (scheduler.datum === "NoActiveOperators")
    throw new Error("The first scheduler appointment is missing");
  const shift = SELECTED_DEPLOYMENT_PROFILE.timing.operator_shift_ms;
  let next = Number(scheduler.datum.ActiveOperator.start_time) + shift;
  while (next <= fixture.emulator.now()) next += shift;
  await advanceEmulatorPastUnixTime(fixture, next + 1_000);
  vi.setSystemTime(new Date(fixture.emulator.now()));
};

type ContentHandle = Pick<Lifecycle, "deployment" | "command"> & Handle;

/** Every spendable L2 output of the depositor, as the node's own ledger
 * reports it. */
export const depositorL2Utxos = async (h: ContentHandle) =>
  h.command(fetchLocalUtxos(await h.fixture.depositorLucid.wallet().address()));

/** A depositor self-transfer of `lovelace` spending exactly `inputs`. */
export const buildDepositorTransfer = async (
  h: ContentHandle,
  inputs: readonly NodeUtxo[],
  lovelace: bigint,
) => {
  const { fixture, production } = h;
  const address = await fixture.depositorLucid.wallet().address();
  return buildTransferTxWithMinFee({
    senderAddress: address,
    destinationAddress: address,
    signer: walletFromSeed(fixture.depositorAccount.seedPhrase, {
      network: "Preprod",
    }).paymentKey,
    availableUtxos: inputs,
    requestedAssets: { lovelace },
    network: "Preprod",
    networkId: 0n,
    minFeeA: production.nodeConfig.MIN_FEE_A,
    minFeeB: production.nodeConfig.MIN_FEE_B,
    consensusProfile:
      fixture.runtimeOverrides!.deploymentIdentity.consensusProfile,
  });
};

/** Queue signed L2 transactions in the node's durable admission queue, as
 * `/submit` does, then drain the queue once: transactions queued together are
 * accepted in one batch, under one inverse receipt. Returns each admission
 * status, in order. */
export const admitTransfersTogether = async (
  h: ContentHandle,
  builts: readonly BuiltTransferTx[],
) => {
  for (const built of builts) {
    const queued = toQueuedTx(built);
    await h.command(
      TxAdmissionsDB.admit({
        txId: queued.txId,
        txCanonicalCbor: queued.txCbor,
        programMaterialSidecarCbor: Buffer.from(
          queued.programMaterialSidecarCbor!,
        ),
        submitSource: "native",
        currentBacklog: 0n,
        maxBacklog: h.production.nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
      }),
    );
  }
  await h.command(
    txQueueProcessorDrainOnce().pipe(Effect.provide(validationPoolLayer)),
  );
  await alignMempoolToEmulatorClock(h);
  const rows = await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{ tx_id: Buffer; status: string }>`
        SELECT tx_id, status FROM tx_admissions
        WHERE tx_id IN ${sql.in(builts.map(({ txId }) => txId))}`;
    }),
  );
  return builts.map(
    ({ txId }) => rows.find((row) => row.tx_id.equals(txId))?.status,
  );
};

/** Admit a signed L2 transaction through the node's durable admission queue
 * and drain it once, as `/submit` does; returns its admission status. */
export const admitTransfer = async (h: ContentHandle, built: BuiltTransferTx) =>
  (await admitTransfersTogether(h, [built]))[0];

/** Persist the write-behind address history now. */
export const flushWriteBehind = (h: Pick<Lifecycle, "command">) =>
  h.command(Effect.flatMap(WriteBehind, (writeBehind) => writeBehind.flushNow));

/**
 * Every durable trace of accepting `txIds`: their admissions, rejections,
 * address history and acceptance receipts, plus both ledger-repair wedges an
 * acceptance left behind would raise. `publishedDependencies` lists each of
 * `depositEventIds` whose incarnation an unreversed receipt consumed while
 * part of its batch left the mempool; `incompleteReceipts` lists every
 * unreversed receipt holding one of `txIds` that the repair would select (it
 * holds a mempool transaction) but cannot invert as one unpublished batch.
 */
export const readAcceptanceTraces = (input: {
  readonly txIds: readonly Buffer[];
  readonly depositEventIds: readonly Buffer[];
}) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const ids = [...input.txIds];
      const hex = (value: Buffer) => value.toString("hex");
      const admissions = yield* sql<{
        tx_id: Buffer;
        status: string;
        reject_code: string | null;
      }>`SELECT tx_id, status, reject_code FROM tx_admissions
        WHERE tx_id IN ${sql.in(ids)}`;
      const rejections = yield* sql<{ tx_id: Buffer; reject_code: string }>`
        SELECT tx_id, reject_code FROM tx_rejections
        WHERE tx_id IN ${sql.in(ids)}`;
      const addressHistory = yield* sql<{ tx_id: Buffer }>`
        SELECT DISTINCT tx_id FROM address_history
        WHERE tx_id IN ${sql.in(ids)}`;
      const receipts = yield* sql<{
        sequence: string;
        tx_ids: readonly Buffer[];
        reversed: boolean;
        selected: boolean;
      }>`SELECT r.sequence::text, r.tx_ids,
          r.reversed_at_revision IS NOT NULL AS reversed,
          EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = ANY(r.tx_ids)) AS selected
        FROM event_history_l2_ledger_receipts r
        WHERE EXISTS (SELECT 1 FROM unnest(r.tx_ids) AS t(tx_id)
          WHERE t.tx_id IN ${sql.in(ids)})
        ORDER BY r.sequence`;
      const incompleteReceipts: string[] = [];
      for (const receipt of receipts)
        if (
          !receipt.reversed &&
          receipt.selected &&
          (yield* ledgerReceiptIsIncomplete(receipt.sequence))
        )
          incompleteReceipts.push(receipt.sequence);
      const publishedDependencies: string[] = [];
      for (const eventId of input.depositEventIds) {
        const incarnations = yield* sql<{
          history_binding_digest: Buffer;
          history_incarnation_id: Buffer;
        }>`SELECT history_binding_digest, history_incarnation_id
          FROM deposits_utxos WHERE event_id = ${eventId}`;
        expect(incarnations).toHaveLength(1);
        const { history_binding_digest, history_incarnation_id } =
          incarnations[0]!;
        if (
          yield* orphanHasPublishedDependency(
            history_binding_digest,
            history_incarnation_id,
          )
        )
          publishedDependencies.push(hex(eventId));
      }
      return {
        admissions: Object.fromEntries(
          admissions.map((row) => [
            hex(row.tx_id),
            { status: row.status, code: row.reject_code },
          ]),
        ),
        rejections: Object.fromEntries(
          rejections.map((row) => [hex(row.tx_id), row.reject_code]),
        ),
        addressHistory: addressHistory.map((row) => hex(row.tx_id)).sort(),
        receipts: receipts.map((row) => ({
          txIds: row.tx_ids.map(hex).sort(),
          reversed: row.reversed,
        })),
        incompleteReceipts,
        publishedDependencies,
      };
    }),
  );

/**
 * Mempool rows are stamped by the database's real clock, while the emulator
 * fixture fakes `Date` at its own, earlier, chain time; the commit worker only
 * selects rows stamped at or before its (faked) start. Re-stamp every row
 * from the future at the emulator's current time, as the two clocks agree in
 * production.
 */
const alignMempoolToEmulatorClock = (h: Handle) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const now = new Date(h.fixture.emulator.now());
      yield* sql`UPDATE mempool SET time_stamp_tz = ${now}
        WHERE time_stamp_tz > ${now}`;
    }),
  );

/** The one output of `built` paying `lovelace` back to the depositor. */
export const outputOf = async (
  h: ContentHandle,
  built: BuiltTransferTx,
  lovelace: bigint,
) => {
  const outputs = (await depositorL2Utxos(h)).filter(
    (utxo) =>
      utxo.txHash === built.txId.toString("hex") &&
      utxo.assets.lovelace === lovelace,
  );
  expect(outputs).toHaveLength(1);
  return outputs[0]!;
};

/** Submit an L1 withdrawal of one of the depositor's L2 outputs. */
const submitWithdrawal = async (h: ContentHandle, target: NodeUtxo) => {
  const { fixture, lucidService } = h;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  lucidService.api.clearUTxOOverride();
  await ensureSeparateCollateralUtxo(wallet);
  await advanceHistoryAdmissionClock(fixture, "withdrawal");
  await alignCommitSchedulerBeforeTestWorker({
    fixture,
    lucidService,
    targetEndTimeMs:
      fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  });
  await h.synchronize();
  const addressData = await Effect.runPromise(
    SDK.addressDataFromBech32(address),
  );
  const body: SDK.WithdrawalBody = {
    l2_outref: {
      transactionId: target.txHash,
      outputIndex: BigInt(target.outputIndex),
    },
    l2_owner: paymentCredentialOf(address).hash,
    l2_value: assetsToValue(target.assets),
    l1_address: addressData,
    l1_datum: "NoDatum",
  };
  const key = CML.PrivateKey.from_bech32(
    walletFromSeed(fixture.depositorAccount.seedPhrase, { network: "Preprod" })
      .paymentKey,
  );
  const built = await Effect.runPromise(
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
  const signed = await built.tx.sign.withWallet().complete();
  expect(await wallet.awaitTx(await signed.submit())).toBe(true);
  wallet.overrideUTxOs(await wallet.utxosAt(address));
  await h.synchronize();
  return built.metadata.inclusionTime;
};

/** A forced transaction spending one of the depositor's L2 outputs, as the
 * tx-order watcher records a valid order. */
const insertForcedTransfer = async (
  h: ContentHandle,
  built: BuiltTransferTx,
) => {
  const { fixture } = h;
  const consensusProfile =
    fixture.runtimeOverrides!.deploymentIdentity.consensusProfile;
  const nativeTxCbor = encodeMidgardForcedTxCanonical(
    decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor),
  );
  const encoding = await Effect.runPromise(
    ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: "ForcedTxValid",
      consensusProfile,
    }),
  );
  const eventId = Buffer.from(
    Data.to(
      { transactionId: "f1".repeat(32), outputIndex: 0n },
      SDK.OutputReference,
    ),
    "hex",
  );
  const inclusionTime = new Date(fixture.emulator.now());
  await h.command(
    ForcedTransactionsDB.insertEntries([
      {
        [ForcedTransactionsDB.Columns.TX_ORDER_ID]: eventId,
        [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(
          32,
          0x42,
        ),
        [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: 0,
        [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.alloc(32, 0x43),
        [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from("01", "hex"),
        [ForcedTransactionsDB.Columns.TX_ID]: encoding.txId,
        [ForcedTransactionsDB.Columns.TX_COMPACT]: encoding.txCompact,
        [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoding.value,
        [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
          consensusProfile.profileId,
        [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: nativeTxCbor,
        [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
          encoding.transactionCommitment,
        [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
          EMPTY_PROGRAM_MATERIAL_SIDECAR,
        [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
          createHash("sha256").update(EMPTY_PROGRAM_MATERIAL_SIDECAR).digest(),
        [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
        [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [ForcedTransactionsDB.Columns.STATUS]:
          ForcedTransactionsDB.Status.Awaiting,
      },
    ]),
  );
  return {
    eventId,
    txId: encoding.txId,
    inclusionTime: inclusionTime.getTime(),
  };
};

/** Lovelace of the four merged deposits and of the removed block's one. */
export const CONTENT_AMOUNTS = {
  transfer: 20_000_000n,
  withdrawal: 12_000_000n,
  forced: 15_000_000n,
  independent: 9_000_000n,
  reopenedDeposit: 17_000_000n,
  transferPayment: 5_000_000n,
  forcedPayment: 4_000_000n,
} as const;

/**
 * A merged block of four deposits, then the block the correction removes:
 * an L2 transfer, a withdrawal and a forced transaction spending three of the
 * merged deposits' outputs, and a new deposit. Committed, confirmed and
 * locally finalized, never attested. The fourth merged output stays unspent
 * for a pending transaction independent of the removed block.
 */
const buildFullContentRemovedBlock = async (h: ContentHandle) => {
  const { fixture, lucidService, globals, production } = h;
  const merged = [
    await submitDeposit(h, CONTENT_AMOUNTS.transfer),
    await submitDeposit(h, CONTENT_AMOUNTS.withdrawal),
    await submitDeposit(h, CONTENT_AMOUNTS.forced),
    await submitDeposit(h, CONTENT_AMOUNTS.independent),
  ];
  await h.deployment.chain.awaitLedgerTime(Math.max(...merged) + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await h.synchronize();
  await commitConfirmRecoverAndMerge({
    fixture,
    lucidService,
    globals,
    production,
  });
  await h.synchronize();
  const byAmount = async (lovelace: bigint) => {
    const found = (await depositorL2Utxos(h)).filter(
      (utxo) => utxo.assets.lovelace === lovelace,
    );
    expect(found).toHaveLength(1);
    return found[0]!;
  };
  const transferInput = await byAmount(CONTENT_AMOUNTS.transfer);
  const withdrawn = await byAmount(CONTENT_AMOUNTS.withdrawal);
  const forcedInput = await byAmount(CONTENT_AMOUNTS.forced);
  const independentInput = await byAmount(CONTENT_AMOUNTS.independent);
  const depositTime = await submitDeposit(h, CONTENT_AMOUNTS.reopenedDeposit);
  const withdrawalTime = await submitWithdrawal(h, withdrawn);
  const transfer = await buildDepositorTransfer(
    h,
    [transferInput],
    CONTENT_AMOUNTS.transferPayment,
  );
  expect(await admitTransfer(h, transfer)).toBe("accepted");
  const forcedTx = await buildDepositorTransfer(
    h,
    [forcedInput],
    CONTENT_AMOUNTS.forcedPayment,
  );
  const forced = await insertForcedTransfer(h, forcedTx);
  const headerHash = await commitLocallyFinalizedBlock(
    h,
    Math.max(depositTime, withdrawalTime, forced.inclusionTime),
  );
  return {
    headerHash,
    transfer,
    withdrawn,
    independentInput,
    forced: { ...forced, built: forcedTx },
  };
};

/**
 * The live preprod shape: deposit blocks committed, confirmed and locally
 * finalized by the production owner, never attested, then removed on L1 by
 * accepted attestation-timeout corrections of the queue tail. The observer
 * cursor is bootstrapped on the pre-removal queue, as the running node's
 * fiber had it. Actual deployed validators and emulator-confirmed
 * transactions; only chain-point names and observer transport are synthetic.
 */
export const openCorrectionRewindScenario = async ({
  blocks,
  localFinalization = "completed",
  unlandedTail = false,
  content = false,
}: {
  readonly blocks: number;
  /** `failed`: the one removed block's local finalization failed (the live
   * f5215638 state) instead of completing. */
  readonly localFinalization?: "completed" | "failed";
  /** With one block: the removed block carries an L2 transfer, a
   * withdrawal, a forced transaction and a deposit, over a merged block that
   * funded them (see `buildFullContentRemovedBlock`). */
  readonly content?: boolean;
  /** With two blocks: the second block's commit is submitted but never lands
   * (`headers[1]` is then an unlanded descendant of `headers[0]`). */
  readonly unlandedTail?: boolean;
}) => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { fixture } = h;
    const identity = fixture.runtimeOverrides!.deploymentIdentity;
    const manifestId = identity.manifestId;
    if (manifestId === undefined)
      throw new Error("The fixture deployment must be manifest-bound");
    const requiredFinalityDepth = BigInt(
      h.deployment.manifest.l1Finality.confirmationDepth,
    );
    expect(requiredFinalityDepth).toBeGreaterThan(1n);
    // The shared worker shard keeps earlier suites' observer and recovery
    // plan rows; the production lifecycle reset does not own them.
    await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM state_queue_terminal_observer_states`;
        yield* sql`DELETE FROM event_history_recovery_plans`;
      }),
    );
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    const headers: string[] = [];
    let removedContent:
      | Awaited<ReturnType<typeof buildFullContentRemovedBlock>>
      | undefined;
    if (content) {
      if (blocks !== 1 || unlandedTail || localFinalization !== "completed")
        throw new Error(
          "A full-content scenario removes one landed, locally finalized block",
        );
      removedContent = await buildFullContentRemovedBlock(h);
      headers.push(removedContent.headerHash);
    } else if (blocks === 1) {
      const inclusion = await submitDeposit(h, 12_000_000n);
      headers.push(
        await commitLocallyFinalizedBlock(h, inclusion, localFinalization),
      );
    } else if (blocks === 2 && localFinalization === "completed") {
      // A two-block unattested suffix exists only if the second block is
      // committed before the first one's DA attestation timeout: the node
      // refuses to commit on an expired unattested tail. Each commit also
      // needs its whole validity range inside one operator shift, so both
      // commits happen early in a fresh shift, with the second deposit
      // submitted before the first commit and included only after the first
      // block's end.
      const first = await submitDeposit(h, 12_000_000n);
      await advanceToNextShift(h);
      const second = await submitDeposit(h, 13_000_000n);
      headers.push(await commitLocallyFinalizedBlock(h, first));
      expect((await readJournal(headers[0]!)).depositEventIds).toHaveLength(1);
      headers.push(
        unlandedTail
          ? (await submitUnlandedBlock(h, second)).submittedHeaderHash
          : await commitLocallyFinalizedBlock(h, second),
      );
    } else
      throw new Error(
        "Scenario supports one or two removed blocks, and a failed local finalization only for one",
      );
    if (unlandedTail && blocks !== 2)
      throw new Error("An unlanded tail needs a removable parent block");
    const removals: Removal[] = [];
    const fetchConfig = {
      stateQueueAddress: fixture.contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
    };
    const readQueue = async () =>
      Promise.all(
        (
          await Effect.runPromise(
            SDK.fetchSortedStateQueueUTxOsProgram(
              fixture.operatorLucid,
              fetchConfig,
            ),
          )
        ).map(async (node, index) => ({
          headerHash:
            index === 0
              ? null
              : await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(node)),
          outRef: `${node.utxo.txHash}#${node.utxo.outputIndex}`,
        })),
      );
    /** While set, the observer's authenticated view reports a rollback of
     * the first removal below its release depth: the queue is its pre-state
     * again and the removal transaction is absent. */
    let rolledBack = false;
    // Every accepted removal, replayed from whatever cursor the observer holds.
    const source: StateQueueCorrectionObserverSource = {
      readQueue: async () =>
        rolledBack
          ? (removals[0]!.checkpoint.previousQueue as Awaited<
              ReturnType<typeof readQueue>
            >)
          : readQueue(),
      observeTransitions: async (previous) => {
        const start = removals.findIndex(
          ({ checkpoint }) =>
            JSON.stringify(checkpoint.previousQueue) ===
            JSON.stringify(previous),
        );
        if (start < 0)
          throw new Error("No accepted removal extends the cursor");
        return removals.slice(start).map(({ checkpoint }) => checkpoint);
      },
      canonicalDepth: async (transition) => {
        const removal = removals.find(
          ({ checkpoint }) =>
            checkpoint.transactionHash === transition.transactionHash,
        );
        if (removal === undefined)
          throw new Error("Missing accepted correction receipt");
        if (rolledBack) return null;
        const status = await fixture.operatorLucid.transactionStatus(
          transition.transactionHash,
        );
        if (status.status !== "confirmed") return null;
        return BigInt(
          fixture.emulator.blockHeight - removal.acceptedHeight + 1,
        );
      },
    };
    /** One correction-fiber tick. A refusal is rethrown with its full cause
     * chain, the same text the fiber logs. */
    const tick = async (
      globals: Globals,
      options: { readonly rewindThroughHistoryOwner?: boolean } = {},
    ) => {
      const exit = await Effect.runPromiseExit(
        reconcileStateQueueCorrections({
          source,
          deploymentIdentityDigest: manifestId,
          stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
          requiredFinalityDepth,
          deploymentManifest: identity.manifest,
          ledgerDeltaLogMax:
            h.production.nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
          rewindThroughHistoryOwner: options.rewindThroughHistoryOwner ?? true,
        }).pipe(
          Effect.provideService(Globals, globals),
          Effect.provide(Database.layer),
        ),
      );
      if (Exit.isSuccess(exit)) return exit.value;
      throw new Error(Cause.pretty(exit.cause));
    };
    expect((await tick(h.globals)).status).toBe("bootstrapped");
    expect((await readObserver()).cursorQueue).toEqual(await readQueue());
    /** Remove the current queue tail, which must be `headerHash`. The
     * removal is an ordinary L1 transaction from the test wallet, so it can
     * also be submitted while the node is down (`observe: false`). */
    const removeTail = async (
      headerHash: string,
      { observe = true }: { readonly observe?: boolean } = {},
    ) => {
      const removal = await prepareTimedOutTailRemoval({
        fixture,
        targetHeaderHash: headerHash,
        deploymentIdentityDigest: manifestId,
      });
      const removed = await removal.submit();
      removals.push(removed);
      if (observe) await h.synchronize();
      return removed;
    };
    /** Advance until every accepted removal reaches the release depth;
     * `observe: false` advances L1 only, as while the node is down. */
    const awaitRemovalFinality = async (
      handle: Handle = h,
      { observe = true }: { readonly observe?: boolean } = {},
    ) => {
      const latest = Math.max(...removals.map((r) => r.acceptedHeight));
      const needed =
        Number(requiredFinalityDepth) -
        (fixture.emulator.blockHeight - latest + 1);
      if (needed > 0) fixture.emulator.awaitBlock(needed);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      if (observe) await handle.synchronize();
    };
    /** The next source block: a forward append at an open gate, which is what
     * notices an owed rewind in production (one L1 block later). */
    const nextSourceBlock = async (handle: Handle = h) => {
      fixture.emulator.awaitBlock(1);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await handle.synchronize();
    };
    /** The next source block while the owed rewind is refused: the owner
     * journals it and keeps its gate closed, so nothing waits for readiness. */
    const nextSourceBlockWhileRefused = async (
      handle: Pick<Lifecycle, "appendTipWhileGateClosed"> = h,
    ) => {
      fixture.emulator.awaitBlock(1);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      return handle.appendTipWhileGateClosed();
    };
    return {
      h,
      headers,
      deposit: (lovelace: bigint, handle: Handle = h) =>
        submitDeposit(handle, lovelace),
      removals,
      manifestId,
      requiredFinalityDepth,
      tick,
      removeTail,
      awaitRemovalFinality,
      nextSourceBlock,
      nextSourceBlockWhileRefused,
      readQueue,
      removedContent,
      /** Report a post-admission rollback of the first removal to the
       * correction observer (see `rolledBack`). */
      simulateRemovalRollback: () => {
        if (removals.length !== 1)
          throw new Error("Rollback simulation supports one removal");
        rolledBack = true;
      },
      authority: {
        manifestId,
        stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
        requiredFinalityDepth,
      },
    };
  } catch (error) {
    await h.close();
    vi.useRealTimers();
    throw error;
  }
};

export const readDeposits = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM deposits_utxos
        ORDER BY inclusion_time`;
      return rows.map((row) => ({
        status: row.status,
        projectedHeader: row.projected_header_hash?.toString("hex") ?? null,
      }));
    }),
  );

export const readJournal = (headerHash: string) =>
  read(Pending.retrieveByHeaderHash(Buffer.from(headerHash, "hex"))).then(
    (row) => Option.getOrThrow(row),
  );

export const readObserverRow = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        deployment_identity_digest: Buffer;
        state_queue_policy_id: Buffer;
        state_digest: Buffer;
        state_record: unknown;
      }>`SELECT deployment_identity_digest, state_queue_policy_id, state_digest,
          state_record FROM state_queue_terminal_observer_states`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

export const readObserver = async () => {
  const record = (await readObserverRow()).state_record;
  return (typeof record === "string" ? JSON.parse(record) : record) as {
    cursorQueue: unknown;
    admitted: readonly { transactionHash: string; transitionDigest: string }[];
    stateDigest: string;
  };
};

/** A crash after local reconciliation but before the observer saved: the
 * durable cursor is still the pre-removal one. */
export const restoreObserverRow = (
  row: Awaited<ReturnType<typeof readObserverRow>>,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const record =
        typeof row.state_record === "string"
          ? row.state_record
          : JSON.stringify(row.state_record);
      yield* sql`UPDATE state_queue_terminal_observer_states
        SET state_digest = ${row.state_digest}, state_record = ${record}
        WHERE deployment_identity_digest = ${row.deployment_identity_digest}`;
    }),
  );

export const readRecoveryPlans = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        state: string;
        intent: string;
      }>`SELECT state, intent FROM event_history_recovery_plans
        ORDER BY created_at`;
      return rows.map((row) => ({
        state: row.state,
        intent: JSON.parse(row.intent) as {
          domain: string;
          headerHash: string;
          members?: readonly { headerHash: string; transitionDigest: string }[];
          expectedRoot: string;
          targetRoot: string;
        },
      }));
    }),
  );

export const readSqlLedgerRoot = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        root_hex: string;
        utxo_payload_entry_count: number | string | null;
      }>`SELECT root_hex, utxo_payload_entry_count FROM mpf_engine_state
        WHERE store_name = 'ledger'`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

/** Commit the next block with the production commit worker and wait for its
 * L1 acceptance. */
export const commitNextBlock = async (h: Handle) => {
  const { fixture, lucidService, globals, production } = h;
  const next = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  expect(await fixture.operatorLucid.awaitTx(next.submittedTxHash)).toBe(true);
  return next;
};

/** Commit, confirm and locally finalize the next block with the production
 * owner; returns its header hash. */
export const commitAndLocallyFinalizeNextBlock = async (
  h: Pick<Lifecycle, "deployment"> & Handle,
) => {
  await alignMempoolToEmulatorClock(h);
  return commitLocallyFinalizedBlock(h, h.fixture.emulator.now() - 1000);
};

export const closeLifecycle = async (h: Pick<Lifecycle, "close">) => {
  try {
    await h.close();
  } finally {
    vi.useRealTimers();
  }
};

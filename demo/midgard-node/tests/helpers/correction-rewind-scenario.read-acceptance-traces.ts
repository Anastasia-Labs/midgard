import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import type { NodeUtxo } from "../../src/commands/command-utils.js";
import { buildTransferTxWithMinFee } from "../../src/commands/submit-l2-transfer.js";
import type { BuiltTransferTx } from "../../src/commands/transfer-build-core.js";
import {
  ledgerReceiptIsIncomplete,
  orphanHasPublishedDependency,
} from "../../src/database/eventHistoryLedgerRepair.js";
import { TxAdmissionsDB } from "../../src/database/index.js";
import { txQueueProcessorDrainOnce } from "../../src/fibers/tx-queue-processor.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import { validationPoolLayer } from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import {
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  assetsToValue,
  CML,
  ensureSeparateCollateralUtxo,
  paymentCredentialOf,
  SDK,
  walletFromSeed,
} from "../deposit-flow-emulator-shared.js";
import {
  type ContentHandle,
  type Handle,
  type Lifecycle,
  read,
} from "./correction-rewind-scenario.commit-locally-finalized-block.js";
import { fetchLocalUtxos, toQueuedTx } from "./local-l2-transfer.js";

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
export const alignMempoolToEmulatorClock = (h: Handle) =>
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
export const submitWithdrawal = async (h: ContentHandle, target: NodeUtxo) => {
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

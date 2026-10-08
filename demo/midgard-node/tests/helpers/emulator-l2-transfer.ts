/**
 * L2 transfers on a production-owner emulator lifecycle, as the node's users
 * make them: the depositor's spendable L2 outputs from the node's own ledger,
 * a signed depositor self-transfer, and admission through the node's durable
 * admission queue (`/submit`), drained until each row is terminal.
 */
import { setTimeout as sleep } from "node:timers/promises";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { NodeUtxo } from "../../src/commands/command-utils.js";
import { buildTransferTxWithMinFee } from "../../src/commands/submit-l2-transfer.js";
import type { BuiltTransferTx } from "../../src/commands/transfer-build-core.js";
import { TxAdmissionsDB } from "../../src/database/index.js";
import { txQueueProcessorDrainOnce } from "../../src/fibers/tx-queue-processor.js";
import { Database } from "../../src/services/database.js";
import { validationPoolLayer } from "../../src/services/validation-pool.js";
import { walletFromSeed } from "../deposit-flow-emulator-shared.js";
import type { Lifecycle } from "./correction-admission-scenario.js";
import { fetchLocalUtxos, toQueuedTx } from "./local-l2-transfer.js";

type TransferHandle = Pick<Lifecycle, "fixture" | "production" | "command">;

const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

/** Every spendable L2 output of the depositor, as the node's own ledger
 * reports it. */
export const depositorL2Utxos = async (
  h: Pick<Lifecycle, "fixture" | "command">,
) =>
  h.command(fetchLocalUtxos(await h.fixture.depositorLucid.wallet().address()));

/** A depositor self-transfer of `lovelace` spending exactly `inputs`. */
export const buildDepositorTransfer = async (
  h: Pick<Lifecycle, "fixture" | "production">,
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

/** How long admission waits for its rows to become terminal, on the
 * monotonic clock: callers may fake `Date`. */
const ADMISSION_SETTLE_MS = 60_000;
const ADMISSION_SETTLE_POLL_MS = 50;

/**
 * Mempool rows are stamped by the database's real clock, while the emulator
 * fixture fakes `Date` at its own, earlier, chain time; the commit worker only
 * selects rows stamped at or before its (faked) start. Re-stamp every row
 * from the future at the emulator's current time, as the two clocks agree in
 * production.
 */
const alignMempoolToEmulatorClock = (h: Pick<Lifecycle, "fixture">) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const now = new Date(h.fixture.emulator.now());
      yield* sql`UPDATE mempool SET time_stamp_tz = ${now}
        WHERE time_stamp_tz > ${now}`;
    }),
  );

/** Queue signed L2 transactions in the node's durable admission queue, as
 * `/submit` does, then drain the queue until each is terminal: transactions
 * queued together are accepted in one batch, under one inverse receipt.
 * Returns each admission status, in order; throws when a row is still not
 * terminal after `ADMISSION_SETTLE_MS`. */
export const admitTransfersTogether = async (
  h: TransferHandle,
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
  return settleAdmissions(
    h,
    builts.map(({ txId }) => txId),
  );
};

/** Drain the node's durable admission queue until each of `txIds` is
 * terminal, as the node's processor would; returns each admission status, in
 * order. Throws when a row is still not terminal after `ADMISSION_SETTLE_MS`. */
export const settleAdmissions = async (
  h: Pick<Lifecycle, "fixture" | "command">,
  txIds: readonly Buffer[],
) => {
  const readStatuses = () =>
    read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{ tx_id: Buffer; status: string }>`
          SELECT tx_id, status FROM tx_admissions
          WHERE tx_id IN ${sql.in(txIds)}`;
      }),
    );
  // A drain whose slot a woken background drain already holds returns at
  // once (the processor coalesces wakeups), leaving the rows to that drain.
  // Drain again until every row is terminal, as the node's processor would.
  const deadline = performance.now() + ADMISSION_SETTLE_MS;
  for (;;) {
    await h.command(
      txQueueProcessorDrainOnce().pipe(Effect.provide(validationPoolLayer)),
    );
    const unsettled = (await readStatuses()).filter(
      ({ status }) => status !== "accepted" && status !== "rejected",
    );
    if (unsettled.length === 0) break;
    if (performance.now() >= deadline) {
      const rows = unsettled.map(
        ({ tx_id, status }) => `${tx_id.toString("hex")} ${status}`,
      );
      throw new Error(
        `Admission left ${rows.join(", ")} non-terminal after ${String(ADMISSION_SETTLE_MS / 1000)} s of draining`,
      );
    }
    await sleep(ADMISSION_SETTLE_POLL_MS);
  }
  await alignMempoolToEmulatorClock(h);
  const rows = await readStatuses();
  return txIds.map(
    (txId) => rows.find((row) => row.tx_id.equals(txId))?.status,
  );
};

/** Admit a signed L2 transaction through the node's durable admission queue,
 * as `/submit` does, and drain until it is terminal; returns its admission
 * status. */
export const admitTransfer = async (
  h: TransferHandle,
  built: BuiltTransferTx,
) => (await admitTransfersTogether(h, [built]))[0];

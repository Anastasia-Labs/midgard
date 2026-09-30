import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { expect, vi } from "vitest";

import type { BuiltTransferTx } from "../../src/commands/transfer-build-core.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import {
  commitConfirmRecoverAndMerge,
  createHash,
  Data,
  EMPTY_PROGRAM_MATERIAL_SIDECAR,
  ForcedTransactionsDB,
  SDK,
} from "../deposit-flow-emulator-shared.js";
import {
  commitLocallyFinalizedBlock,
  type ContentHandle,
  read,
  submitDeposit,
} from "./correction-rewind-scenario.commit-locally-finalized-block.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  depositorL2Utxos,
  submitWithdrawal,
} from "./correction-rewind-scenario.read-acceptance-traces.js";

/** A forced transaction spending one of the depositor's L2 outputs, as the
 * tx-order watcher records a valid order. */
export const insertForcedTransfer = async (
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
export const buildFullContentRemovedBlock = async (h: ContentHandle) => {
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

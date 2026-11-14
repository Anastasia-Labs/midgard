import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";

import {
  DaPayloadsDB,
  DepositsDB,
  WithdrawalsDB,
} from "../../src/database/index.js";
import {
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
} from "./fixtures.bytes.js";
import { address1 } from "./fixtures.ledger.js";

let depositFixtureSequence = 0;

export const makeDepositEntry = (
  overrides: Partial<DepositsDB.Entry> = {},
): DepositsDB.Entry => {
  const fixtureIndex = depositFixtureSequence;
  depositFixtureSequence += 1;
  const fixtureLabel = `entry-${fixtureIndex.toString().padStart(4, "0")}`;
  const eventId =
    overrides[DepositsDB.Columns.ID] ??
    databaseOutputReferenceId(`deposits.${fixtureLabel}`, fixtureIndex);
  return {
    [DepositsDB.Columns.ID]: eventId,
    [DepositsDB.Columns.INFO]:
      overrides[DepositsDB.Columns.INFO] ??
      databaseFixtureBytes(`deposits.${fixtureLabel}.info`, 48),
    [DepositsDB.Columns.INCLUSION_TIME]:
      overrides[DepositsDB.Columns.INCLUSION_TIME] ??
      new Date(depositFixtureBaseTimeMs + fixtureIndex),
    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]:
      overrides[DepositsDB.Columns.DEPOSIT_L1_TX_HASH] ??
      databaseTxHash(`deposits.${fixtureLabel}.l1-tx`),
    [DepositsDB.Columns.LEDGER_TX_ID]:
      overrides[DepositsDB.Columns.LEDGER_TX_ID] ??
      databaseTxHash(`deposits.${fixtureLabel}.ledger-tx`),
    [DepositsDB.Columns.LEDGER_OUTPUT]:
      overrides[DepositsDB.Columns.LEDGER_OUTPUT] ??
      databaseFixtureBytes(`deposits.${fixtureLabel}.ledger-output`, 80),
    [DepositsDB.Columns.LEDGER_ADDRESS]:
      overrides[DepositsDB.Columns.LEDGER_ADDRESS] ?? address1,
    [DepositsDB.Columns.PROJECTED_HEADER_HASH]:
      overrides[DepositsDB.Columns.PROJECTED_HEADER_HASH] ?? null,
    [DepositsDB.Columns.STATUS]:
      overrides[DepositsDB.Columns.STATUS] ?? DepositsDB.Status.Awaiting,
  };
};

export const daPayloadInsertFixture = (
  label: string,
): DaPayloadsDB.InsertInput => {
  const headerHash = databaseFixtureBytes(`${label}-header`, 28);
  const payload = databaseFixtureBytes(`${label}-payload`, 96);
  return {
    [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
    [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
    [DaPayloadsDB.Columns.VERSION]: 1,
    [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
    [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
      .update(payload)
      .digest(),
    [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
    [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
    [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
    [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(),
    [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(),
    [DaPayloadsDB.Columns.BLOCK_TIME]: new Date(),
  };
};

export const makeHistoryWithdrawalEntry = (): WithdrawalsDB.Entry => {
  return {
    [WithdrawalsDB.Columns.ID]: databaseOutputReferenceId(
      "history-pointer-withdrawal",
    ),
    [WithdrawalsDB.Columns.RAW_EVENT_INFO]: databaseFixtureBytes(
      "speculative-memory-withdrawal-raw",
      96,
    ),
    [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: null,
    [WithdrawalsDB.Columns.INCLUSION_TIME]: new Date(
      "2026-04-13T18:00:00.000Z",
    ),
    [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: databaseTxHash(
      "speculative-memory-withdrawal-l1",
    ),
    [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 0,
    [WithdrawalsDB.Columns.ASSET_NAME]: databaseFixtureBytes(
      "speculative-memory-withdrawal-asset",
      32,
    ),
    [WithdrawalsDB.Columns.L2_OUTREF]: databaseOutputReferenceId(
      "speculative-memory-withdrawal-l2",
      1n,
    ),
    [WithdrawalsDB.Columns.L2_OWNER]: databaseFixtureBytes(
      "speculative-memory-withdrawal-owner",
      28,
    ),
    [WithdrawalsDB.Columns.L2_VALUE]: databaseFixtureBytes(
      "speculative-memory-withdrawal-value",
      48,
    ),
    [WithdrawalsDB.Columns.L1_ADDRESS]: databaseFixtureBytes(
      "speculative-memory-withdrawal-address",
      32,
    ),
    [WithdrawalsDB.Columns.L1_DATUM]: databaseFixtureBytes(
      "speculative-memory-withdrawal-datum",
      16,
    ),
    [WithdrawalsDB.Columns.REFUND_ADDRESS]: databaseFixtureBytes(
      "speculative-memory-withdrawal-refund-address",
      32,
    ),
    [WithdrawalsDB.Columns.REFUND_DATUM]: databaseFixtureBytes(
      "speculative-memory-withdrawal-refund-datum",
      16,
    ),
    [WithdrawalsDB.Columns.VALIDITY]: null,
    [WithdrawalsDB.Columns.CLASSIFICATION_REVISION]: 0,
    [WithdrawalsDB.Columns.REOPENED_FROM_HEADER_HASH]: null,
    [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {},
    [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: null,
    [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Awaiting,
  };
};

export const depositFixtureBaseTimeMs = Date.parse("2026-04-13T17:28:10.000Z");

import {
  DepositsDB,
  DepositSubmissionAttemptsDB,
} from "../../src/database/index.js";
import {
  address1,
  databaseOutputReferenceId,
  depositFixtureBaseTimeMs,
} from "./fixtures.make-history-withdrawal-entry.js";
import {
  databaseFixtureBytes,
  databaseTxHash,
} from "./fixtures.make-material-proof-submit-tx.js";

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

export const makeDepositSubmissionAttempt = ({
  txHash = databaseTxHash("deposit-submission.default"),
  eventId = databaseOutputReferenceId("deposit-submission.default"),
}: {
  readonly txHash?: Buffer;
  readonly eventId?: Buffer;
} = {}): DepositSubmissionAttemptsDB.InsertSubmittedInput => ({
  [DepositSubmissionAttemptsDB.Columns.TX_HASH]: txHash,
  [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: eventId,
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]:
    `${txHash.toString("hex")}#0`,
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]: address1,
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]: "1000000",
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]: {
    lovelace: "1000000",
  },
  [DepositSubmissionAttemptsDB.Columns.METADATA]: {
    depositAddress: address1,
    depositEventId: eventId.toString("hex"),
    depositAssetName: "00".repeat(32),
    depositAuthUnit: `${"11".repeat(28)}${"00".repeat(32)}`,
    nonceInput: {
      txHash: databaseTxHash("deposit-submission.nonce").toString("hex"),
      outputIndex: 0,
    },
    validTo: 1_800_000_000_000,
    inclusionTime: 1_800_000_060_000,
    structuralLovelace: "0",
    orderOutputIndex: 0,
    l2DatumCbor: null,
    transactionCbor: "84a3008001800200a0f5f6",
  },
  [DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS]: [
    `${databaseTxHash("deposit-submission.funding").toString("hex")}#0`,
  ],
});

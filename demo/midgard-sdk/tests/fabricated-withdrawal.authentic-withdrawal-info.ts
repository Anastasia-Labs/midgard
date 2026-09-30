import {
  type FabricatedWithdrawalStep02State,
  type FabricatedWithdrawalStep03State,
  fabricatedWithdrawalStep03State,
  type FabricatedWithdrawalStep04State,
  fabricatedWithdrawalStep04State,
} from "../src/fraud-proof/fabricated-withdrawal.js";
import { type WithdrawalInfo } from "../src/ledger-state.js";
import { type EventHistoryPayload } from "../src/user-events/history.js";
import { eventHistoryCommitment } from "../src/user-events/history-proof.js";
import { type WithdrawalOrderDatum } from "../src/user-events/withdrawal.js";

// ## Fixture twins
//
// `step-01.ak`'s `authentic_withdrawal_id_v1`, `fabricated_withdrawal_id_v1`,
// `authentic_withdrawal_info_v1`, `diverted_withdrawal_info_v1`,
// `forged_signature_withdrawal_info_v1`,
// `revalidated_withdrawal_info_v1`, and `step-02.ak`'s
// `authentic_withdrawal_datum_v1` / `authentic_inclusion_time_v1`.

export const AUTHENTIC_WITHDRAWAL_ID = {
  transactionId:
    "8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b",
  outputIndex: 2n,
};

export const FABRICATED_WITHDRAWAL_ID = {
  transactionId:
    "3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a",
  outputIndex: 0n,
};

const L1_PAYOUT_ADDRESS = {
  paymentCredential: {
    PublicKeyCredential: [
      "2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b",
    ] as [string],
  },
  stakeCredential: null,
};

const DIVERTED_L1_ADDRESS = {
  paymentCredential: {
    PublicKeyCredential: [
      "5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d",
    ] as [string],
  },
  stakeCredential: null,
};

export const AUTHENTIC_WITHDRAWAL_INFO: WithdrawalInfo = {
  body: {
    l2_outref: {
      transactionId:
        "7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e",
      outputIndex: 1n,
    },
    l2_owner: "9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c",
    l2_value: new Map([
      [
        "4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b",
        new Map([["6d6964676172642d746f6b656e", 42n]]),
      ],
    ]),
    l1_address: L1_PAYOUT_ADDRESS,
    l1_datum: "NoDatum",
  },
  signature: [
    "adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad",
    "bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebe",
  ],
  validity: "WithdrawalIsValid",
};

/** The payout redirected to another L1 address — a fabricated `body`. */
export const DIVERTED_WITHDRAWAL_INFO: WithdrawalInfo = {
  ...AUTHENTIC_WITHDRAWAL_INFO,
  body: { ...AUTHENTIC_WITHDRAWAL_INFO.body, l1_address: DIVERTED_L1_ADDRESS },
};

/** An authorisation the owner never produced — a fabricated `signature`. */
export const FORGED_SIGNATURE_WITHDRAWAL_INFO: WithdrawalInfo = {
  ...AUTHENTIC_WITHDRAWAL_INFO,
  signature: [
    "adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad",
    "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0",
  ],
};

/**
 * The authentic body and signature under the verdict an honest block stamps when
 * the referenced L2 output does not exist. The operator owns that verdict
 * (decision 0007), so this is not a fabrication and must not convict.
 */
export const REVALIDATED_WITHDRAWAL_INFO: WithdrawalInfo = {
  ...AUTHENTIC_WITHDRAWAL_INFO,
  validity: "NonExistentWithdrawalUtxo",
};

/** `step-02.ak`'s `authentic_inclusion_time_v1`. */
export const AUTHENTIC_INCLUSION_TIME = 15n;

export const AUTHENTIC_WITHDRAWAL_EVENT_DATUM: WithdrawalOrderDatum = {
  event: { id: AUTHENTIC_WITHDRAWAL_ID, info: AUTHENTIC_WITHDRAWAL_INFO },
  inclusion_time: AUTHENTIC_INCLUSION_TIME,
  witness: "57575757575757575757575757575757575757575757575757575757",
  refund_address: L1_PAYOUT_ADDRESS,
  refund_datum: "NoDatum",
};

// ## Measured Aiken constants
//
// The challenged blocks are `step-01.ak`'s `fabricated_identity_block_v1` (FI,
// the nonexistent-identity scenario), `mismatched_content_block_v1` (MM, the
// content-mismatch scenario) and `authentic_withdrawal_block_v1` (AU, the valid
// block). Their header windows are `start_time = 10`, `end_time = 20` (inherited
// from `native_binding_fixture_v1`).

export const HEADER_START_TIME = 10n;

export const HEADER_END_TIME = 20n;

export const KEY_AUTHENTIC_WITHDRAWAL_ID =
  "d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff";

export const KEY_FABRICATED_WITHDRAWAL_ID =
  "d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff";

export const VALUE_AUTHENTIC_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

/**
 * `step-01.ak`'s `withdrawal_content_hash_v1` of each fixture — the commitment
 * over `(body, signature)` only. The authentic order and the revalidated one
 * share a hash on purpose: they differ only in the operator-owned `validity`
 * verdict (decision 0007).
 */
export const HASH_AUTHENTIC_WITHDRAWAL_CONTENT =
  "283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f";

export const HASH_DIVERTED_WITHDRAWAL_CONTENT =
  "8e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a3";

export const HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT =
  "20b97ac437a93d4f4fc7c0cf4c8a5b84840b408d836723c98f663cc2e8376f22";

export const NONCE_AUTHENTIC_WITHDRAWAL_ID =
  "630f633bd50fa6888cf4e56be119c4970c013d0c7a45216b7eed46960fac800b";

export const DATUM_AUTHENTIC_WITHDRAWAL_EVENT =
  "d8799fd8799fd8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ffff0f581c57575757575757575757575757575757575757575757575757575757d8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff";

export const HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM =
  "b5e4fa1c72a874ec61778f2e29dc4cc326313b3bc581bc64738fd45f1d9a9a70";

export const FI_WITHDRAWALS_PHAS_ROOT =
  "7e6bcae06cc23954a14d0d2070b40be71abb631bc845a82640c4e8ad3bac7138";

export const FI_WITHDRAWALS_ROOT =
  "520d2e1a48bd0ba1c6424899fc91f0572ad5049e84a912a2c28e841ae3a1d88b";

export const FI_HEADER_HASH =
  "3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b4";

export const FI_THREAD_TOKEN_ASSET_NAME =
  "0000000c3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b4";

export const MM_WITHDRAWALS_PHAS_ROOT =
  "9b82564d9ec08f4d54a61982cc5b26972cb0c4ff6ead2d03da141bb0d9ef6b42";

export const MM_WITHDRAWALS_ROOT =
  "ddf6c2b73b0a5be5c6afcb11cbb8c47ecec36a856231911288306a01e411bbed";

export const MM_HEADER_HASH =
  "39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a";

export const MM_THREAD_TOKEN_ASSET_NAME =
  "0000000c39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a";

export const AU_WITHDRAWALS_PHAS_ROOT =
  "f15ac1acdd0df79c30da7d61d4ff84cb5116a1b99c203d58976d3c10465d3ce7";

export const AU_WITHDRAWALS_ROOT =
  "cc7c414fb11977f998c502f0bead1868a4fc2c743142ae62071f23dcf15543e8";

/** `step_02.State` of the nonexistent-identity scenario. */
export const FI_STEP_02_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b40a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff5820283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12fff";

/** `step_03.State` of the nonexistent-identity scenario. */
export const FI_STEP_03_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b40a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff5820283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12fd87980ff";

/** `step_04.State` of the nonexistent-identity scenario. */
export const FI_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b40a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ffd87980ff";

/** `step_02.State` of the content-mismatch scenario. */
export const MM_STEP_02_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a0a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff58208e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a3ff";

/** `step_03.State` of the content-mismatch scenario. */
export const MM_STEP_03_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a0a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff58208e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a3d87a9fd8799f581c30303030303030303030303030303030303030303030303030303030d87a80d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff0f5820537808b384b40793ff2082cc2570120d7d9687041b1b2b7daf48de4dab00db645820ad7eb588061e3a3d90e1ded245c9896e896590ba2dcb368ca2ad3328b4177179ffffff";

/** `step_04.State` of the content-mismatch scenario. */
export const MM_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a0a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd87a9f58208e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a35820283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f0fffff";

export const HISTORY_PAYLOAD: EventHistoryPayload = {
  WithdrawalPayload: {
    event: AUTHENTIC_WITHDRAWAL_EVENT_DATUM.event,
    refund_address: AUTHENTIC_WITHDRAWAL_EVENT_DATUM.refund_address,
    refund_datum: AUTHENTIC_WITHDRAWAL_EVENT_DATUM.refund_datum,
  },
};

export const ORIGINAL_ASSETS = new Map([["", new Map([["", 3_000_000n]])]]);

export const HISTORY_COMMITMENT = eventHistoryCommitment(
  "30".repeat(28),
  "Withdrawal",
  {
    event_id: AUTHENTIC_WITHDRAWAL_ID,
    inclusion_time: AUTHENTIC_INCLUSION_TIME,
  },
  HISTORY_PAYLOAD,
  ORIGINAL_ASSETS,
);

export const HISTORY_OPENING_CBOR =
  "d87a9fd87a9fd8799fd8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ffffd8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ffa140a1401a002dc6c0ff";

// ## Handoff builders under test

export const fiStep02State: FabricatedWithdrawalStep02State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: FABRICATED_WITHDRAWAL_ID,
  committed_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
};

export const mmStep02State: FabricatedWithdrawalStep02State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
  committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
};

export const fiStep03State: FabricatedWithdrawalStep03State =
  fabricatedWithdrawalStep03State(fiStep02State, "WithdrawalIdentityAbsent");

export const mmStep03State: FabricatedWithdrawalStep03State =
  fabricatedWithdrawalStep03State(mmStep02State, {
    WithdrawalEventObserved: {
      commitment: HISTORY_COMMITMENT,
    },
  });

export const fiStep04State: FabricatedWithdrawalStep04State =
  fabricatedWithdrawalStep04State(
    fiStep03State,
    "NonexistentWithdrawalIdentity",
  );

export const mmStep04State: FabricatedWithdrawalStep04State =
  fabricatedWithdrawalStep04State(mmStep03State, {
    MismatchedWithdrawalContent: {
      committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
      authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
      event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
    },
  });

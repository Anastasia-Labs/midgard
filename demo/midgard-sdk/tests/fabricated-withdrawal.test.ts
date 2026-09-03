/**
 * Aiken/TypeScript byte twins for the `fabricated-withdrawal` family (Goal task
 * `Q40`).
 *
 * Every expectation below is an **absolute** hex constant measured out of the
 * Aiken family modules
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2,3,4}.ak`
 * (via `cbor.serialise` / `utils.serialise_and_hash_32` /
 * `user_events.out_ref_to_nonce` / `transition_trace.commit_counted_root` over
 * that family's own test fixtures). Nothing here compares one TypeScript
 * derivation against another: if either side's encoding moves, the literal stops
 * matching.
 *
 * The family is reached by direct module import rather than through
 * `src/fraud-proof/catalogue.ts`, because the `fabricatedWithdrawal` catalogue
 * category is not registered yet.
 *
 * A withdrawal leaf is the first fraud-proof leaf in this family series whose
 * value embeds a `Value` map, so these twins also pin the definite-versus-
 * indefinite map difference between Lucid's typed encoder and Plutus'
 * `serialiseData`: the raw Lucid bytes are asserted **not** to match the on-chain
 * ones, and the normalised bytes are asserted to match exactly.
 */
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { OutputReference } from "../src/common.js";
import {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
  type FabricatedWithdrawalStep02State,
  FabricatedWithdrawalStep02State as FabricatedWithdrawalStep02StateType,
  type FabricatedWithdrawalStep03State,
  FabricatedWithdrawalStep03State as FabricatedWithdrawalStep03StateType,
  fabricatedWithdrawalStep03State,
  type FabricatedWithdrawalStep04State,
  FabricatedWithdrawalStep04State as FabricatedWithdrawalStep04StateType,
  fabricatedWithdrawalStep04State,
  fabricatedWithdrawalThreadTokenAssetName,
  isFabricatedWithdrawalFault,
  withdrawalEventDatumBytes,
  withdrawalEventDatumCommitment,
  withdrawalEventNonce,
  withdrawalInfoCommitment,
} from "../src/fraud-proof/fabricated-withdrawal.js";
import {
  type WithdrawalInfo,
  WithdrawalInfo as WithdrawalInfoType,
} from "../src/ledger-state.js";
import {
  commitCountedRootProgram,
  ROOT_DOMAINS,
} from "../src/transition-trace.js";
import { type WithdrawalOrderDatum } from "../src/user-events/withdrawal.js";

// ## Fixture twins
//
// `step-01.ak`'s `authentic_withdrawal_id_v1`, `fabricated_withdrawal_id_v1`,
// `authentic_withdrawal_info_v1`, `diverted_withdrawal_info_v1`,
// `forged_signature_withdrawal_info_v1`,
// `overridden_validity_withdrawal_info_v1`, and `step-02.ak`'s
// `authentic_withdrawal_datum_v1` / `authentic_inclusion_time_v1`.

const AUTHENTIC_WITHDRAWAL_ID = {
  transactionId:
    "8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b",
  outputIndex: 2n,
};

const FABRICATED_WITHDRAWAL_ID = {
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

const AUTHENTIC_WITHDRAWAL_INFO: WithdrawalInfo = {
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
const DIVERTED_WITHDRAWAL_INFO: WithdrawalInfo = {
  ...AUTHENTIC_WITHDRAWAL_INFO,
  body: { ...AUTHENTIC_WITHDRAWAL_INFO.body, l1_address: DIVERTED_L1_ADDRESS },
};

/** An authorisation the owner never produced — a fabricated `signature`. */
const FORGED_SIGNATURE_WITHDRAWAL_INFO: WithdrawalInfo = {
  ...AUTHENTIC_WITHDRAWAL_INFO,
  signature: [
    "adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad",
    "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0",
  ],
};

/** A verdict the authentic event never carried — a fabricated `validity`. */
const OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO: WithdrawalInfo = {
  ...AUTHENTIC_WITHDRAWAL_INFO,
  validity: "NonExistentWithdrawalUtxo",
};

/** `step-02.ak`'s `authentic_inclusion_time_v1`. */
const AUTHENTIC_INCLUSION_TIME = 15n;

const AUTHENTIC_WITHDRAWAL_EVENT_DATUM: WithdrawalOrderDatum = {
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

const HEADER_START_TIME = 10n;
const HEADER_END_TIME = 20n;

const KEY_AUTHENTIC_WITHDRAWAL_ID =
  "d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff";
const KEY_FABRICATED_WITHDRAWAL_ID =
  "d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff";

const VALUE_AUTHENTIC_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

const HASH_AUTHENTIC_WITHDRAWAL_INFO =
  "f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a0";
const HASH_DIVERTED_WITHDRAWAL_INFO =
  "6d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de973";
const HASH_FORGED_SIGNATURE_WITHDRAWAL_INFO =
  "a3b578b5798f5dd0fd76e68a612d9d8d4af873908c1855e73e33fdb340402939";
const HASH_OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO =
  "56b23f1caeca79d65bf2dcc91c4e1f47d7904b03b442481491edd3acae9f64a8";

const NONCE_AUTHENTIC_WITHDRAWAL_ID =
  "630f633bd50fa6888cf4e56be119c4970c013d0c7a45216b7eed46960fac800b";

const DATUM_AUTHENTIC_WITHDRAWAL_EVENT =
  "d8799fd8799fd8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ffff0f581c57575757575757575757575757575757575757575757575757575757d8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff";
const HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM =
  "b5e4fa1c72a874ec61778f2e29dc4cc326313b3bc581bc64738fd45f1d9a9a70";

const FI_WITHDRAWALS_PHAS_ROOT =
  "7e6bcae06cc23954a14d0d2070b40be71abb631bc845a82640c4e8ad3bac7138";
const FI_WITHDRAWALS_ROOT =
  "520d2e1a48bd0ba1c6424899fc91f0572ad5049e84a912a2c28e841ae3a1d88b";
const FI_HEADER_HASH =
  "735fcb9ab869fa81efc508ca11991963a774ae8024658d6de1889967";
const FI_THREAD_TOKEN_ASSET_NAME =
  "0000000c735fcb9ab869fa81efc508ca11991963a774ae8024658d6de1889967";

const MM_WITHDRAWALS_PHAS_ROOT =
  "9b82564d9ec08f4d54a61982cc5b26972cb0c4ff6ead2d03da141bb0d9ef6b42";
const MM_WITHDRAWALS_ROOT =
  "ddf6c2b73b0a5be5c6afcb11cbb8c47ecec36a856231911288306a01e411bbed";
const MM_HEADER_HASH =
  "44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd4728182";
const MM_THREAD_TOKEN_ASSET_NAME =
  "0000000c44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd4728182";

const AU_WITHDRAWALS_PHAS_ROOT =
  "f15ac1acdd0df79c30da7d61d4ff84cb5116a1b99c203d58976d3c10465d3ce7";
const AU_WITHDRAWALS_ROOT =
  "cc7c414fb11977f998c502f0bead1868a4fc2c743142ae62071f23dcf15543e8";

/** `step_02.State` of the nonexistent-identity scenario. */
const FI_STEP_02_STATE_CBOR =
  "d8799f581c735fcb9ab869fa81efc508ca11991963a774ae8024658d6de18899670a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff5820f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a0ff";
/** `step_03.State` of the nonexistent-identity scenario. */
const FI_STEP_03_STATE_CBOR =
  "d8799f581c735fcb9ab869fa81efc508ca11991963a774ae8024658d6de18899670a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff5820f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a0d87980ff";
/** `step_04.State` of the nonexistent-identity scenario. */
const FI_STEP_04_STATE_CBOR =
  "d8799f581c735fcb9ab869fa81efc508ca11991963a774ae8024658d6de18899670a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ffd87980ff";

/** `step_02.State` of the content-mismatch scenario. */
const MM_STEP_02_STATE_CBOR =
  "d8799f581c44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd47281820a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff58206d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de973ff";
/** `step_03.State` of the content-mismatch scenario. */
const MM_STEP_03_STATE_CBOR =
  "d8799f581c44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd47281820a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff58206d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de973d87a9f5820b5e4fa1c72a874ec61778f2e29dc4cc326313b3bc581bc64738fd45f1d9a9a700fffff";
/** `step_04.State` of the content-mismatch scenario. */
const MM_STEP_04_STATE_CBOR =
  "d8799f581c44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd47281820a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd87a9f58206d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de9735820f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a00fffff";

// ## Handoff builders under test

const fiStep02State: FabricatedWithdrawalStep02State = {
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: FABRICATED_WITHDRAWAL_ID,
  committed_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
};

const mmStep02State: FabricatedWithdrawalStep02State = {
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
  committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
};

const fiStep03State: FabricatedWithdrawalStep03State =
  fabricatedWithdrawalStep03State(fiStep02State, "WithdrawalIdentityAbsent");

const mmStep03State: FabricatedWithdrawalStep03State =
  fabricatedWithdrawalStep03State(mmStep02State, {
    WithdrawalEventObserved: {
      event_datum_hash: HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
      event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
    },
  });

const fiStep04State: FabricatedWithdrawalStep04State =
  fabricatedWithdrawalStep04State(
    fiStep03State,
    "NonexistentWithdrawalIdentity",
  );

const mmStep04State: FabricatedWithdrawalStep04State =
  fabricatedWithdrawalStep04State(mmStep03State, {
    MismatchedWithdrawalContent: {
      committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
      authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
      event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
    },
  });

describe("fabricated-withdrawal v1 byte twins", () => {
  it("encodes the committed withdrawal leaf key exactly as Aiken serialises a WithdrawalId", () => {
    expect(committedWithdrawalKeyBytes(AUTHENTIC_WITHDRAWAL_ID)).toBe(
      KEY_AUTHENTIC_WITHDRAWAL_ID,
    );
    expect(committedWithdrawalKeyBytes(FABRICATED_WITHDRAWAL_ID)).toBe(
      KEY_FABRICATED_WITHDRAWAL_ID,
    );
    // A `WithdrawalId` carries no map, so the key is the one place where the raw
    // Lucid encoding already agrees with `serialiseData`.
    expect(Data.to(AUTHENTIC_WITHDRAWAL_ID, OutputReference)).toBe(
      KEY_AUTHENTIC_WITHDRAWAL_ID,
    );
  });

  it("encodes the committed withdrawal leaf value exactly as Aiken does, and only after normalising Lucid's indefinite maps", () => {
    expect(committedWithdrawalValueBytes(AUTHENTIC_WITHDRAWAL_INFO)).toBe(
      VALUE_AUTHENTIC_WITHDRAWAL_INFO,
    );
    // The reason the helper normalises: Lucid writes the `l2_value` map in
    // indefinite form, which is *not* what the on-chain `cbor.serialise` of the
    // same typed leaf produces, so the un-normalised bytes would be a leaf no
    // step could reproduce.
    const rawLucid = Data.to(AUTHENTIC_WITHDRAWAL_INFO, WithdrawalInfoType);
    expect(rawLucid).not.toBe(VALUE_AUTHENTIC_WITHDRAWAL_INFO);
    expect(rawLucid).toContain("bf");
    expect(aikenSerialisedPlutusDataCbor(rawLucid)).toBe(
      VALUE_AUTHENTIC_WITHDRAWAL_INFO,
    );
  });

  it("commits body, signature and validity fidelity in one 32-byte hash, exactly as Aiken does", () => {
    expect(
      Effect.runSync(withdrawalInfoCommitment(AUTHENTIC_WITHDRAWAL_INFO)),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_INFO);
    expect(
      Effect.runSync(withdrawalInfoCommitment(DIVERTED_WITHDRAWAL_INFO)),
    ).toBe(HASH_DIVERTED_WITHDRAWAL_INFO);
    expect(
      Effect.runSync(
        withdrawalInfoCommitment(FORGED_SIGNATURE_WITHDRAWAL_INFO),
      ),
    ).toBe(HASH_FORGED_SIGNATURE_WITHDRAWAL_INFO);
    expect(
      Effect.runSync(
        withdrawalInfoCommitment(OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO),
      ),
    ).toBe(HASH_OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO);
    // Each of the three fabrications is distinguishable from the authentic order
    // and from the other two, which is what lets one inequality settle all three.
    expect(
      new Set([
        HASH_AUTHENTIC_WITHDRAWAL_INFO,
        HASH_DIVERTED_WITHDRAWAL_INFO,
        HASH_FORGED_SIGNATURE_WITHDRAWAL_INFO,
        HASH_OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO,
      ]).size,
    ).toBe(4);
  });

  it("derives the withdrawal event NFT nonce exactly as Aiken's out_ref_to_nonce does", () => {
    expect(Effect.runSync(withdrawalEventNonce(AUTHENTIC_WITHDRAWAL_ID))).toBe(
      NONCE_AUTHENTIC_WITHDRAWAL_ID,
    );
  });

  it("encodes the authentic withdrawal event datum and step-02's retained commitment exactly as Aiken does", () => {
    expect(withdrawalEventDatumBytes(AUTHENTIC_WITHDRAWAL_EVENT_DATUM)).toBe(
      DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    );
    expect(
      Effect.runSync(
        withdrawalEventDatumCommitment(AUTHENTIC_WITHDRAWAL_EVENT_DATUM),
      ),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM);
    // The event datum's five fields include the refund path, so the commitment
    // covers material the leaf value does not.
    expect(DATUM_AUTHENTIC_WITHDRAWAL_EVENT).toContain(
      VALUE_AUTHENTIC_WITHDRAWAL_INFO.slice(6),
    );
  });

  it("commits the challenged headers' counted withdrawals_root and thread token asset names exactly as Aiken does", () => {
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.withdrawals,
          phasRoot: FI_WITHDRAWALS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(FI_WITHDRAWALS_ROOT);
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.withdrawals,
          phasRoot: MM_WITHDRAWALS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(MM_WITHDRAWALS_ROOT);
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.withdrawals,
          phasRoot: AU_WITHDRAWALS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(AU_WITHDRAWALS_ROOT);
    expect(FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID).toBe("0000000c");
    expect(fabricatedWithdrawalThreadTokenAssetName(FI_HEADER_HASH)).toBe(
      FI_THREAD_TOKEN_ASSET_NAME,
    );
    expect(fabricatedWithdrawalThreadTokenAssetName(MM_HEADER_HASH)).toBe(
      MM_THREAD_TOKEN_ASSET_NAME,
    );
  });

  it("encodes the step-01 to step-02 and step-02 to step-03 handoffs exactly as the Aiken validators produce them", () => {
    expect(Data.to(fiStep02State, FabricatedWithdrawalStep02StateType)).toBe(
      FI_STEP_02_STATE_CBOR,
    );
    expect(Data.to(mmStep02State, FabricatedWithdrawalStep02StateType)).toBe(
      MM_STEP_02_STATE_CBOR,
    );
    expect(Data.to(fiStep03State, FabricatedWithdrawalStep03StateType)).toBe(
      FI_STEP_03_STATE_CBOR,
    );
    expect(Data.to(mmStep03State, FabricatedWithdrawalStep03StateType)).toBe(
      MM_STEP_03_STATE_CBOR,
    );
    // The two verdicts are different constructors of the same enum, so the
    // step-03 opening that pairs with one cannot be re-used against the other.
    expect(FI_STEP_03_STATE_CBOR).not.toBe(MM_STEP_03_STATE_CBOR);
    expect(FI_STEP_03_STATE_CBOR.endsWith("d87980ff")).toBe(true);
    expect(MM_STEP_03_STATE_CBOR).toContain("d87a9f");
  });

  it("encodes the step-03 to step-04 handoff and settles the fault rule exactly as the Aiken step-04 validator does", () => {
    expect(Data.to(fiStep04State, FabricatedWithdrawalStep04StateType)).toBe(
      FI_STEP_04_STATE_CBOR,
    );
    expect(Data.to(mmStep04State, FabricatedWithdrawalStep04StateType)).toBe(
      MM_STEP_04_STATE_CBOR,
    );
    // The rule twin of `fabricated_withdrawal_fault_is_established_v1`.
    expect(isFabricatedWithdrawalFault(fiStep04State)).toBe(true);
    expect(isFabricatedWithdrawalFault(mmStep04State)).toBe(true);
    // A header committing exactly the authentic order is not a fault, and an
    // authentic event outside the challenged block's window is not this block's
    // fault, whichever side of the window it falls on.
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
            authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(false);
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
            authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
            event_inclusion_time: HEADER_END_TIME + 1n,
          },
        },
      }),
    ).toBe(false);
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
            authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
            event_inclusion_time: HEADER_START_TIME,
          },
        },
      }),
    ).toBe(false);
    // A forged signature or an overridden validity convicts on the same rule.
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_info_hash:
              HASH_FORGED_SIGNATURE_WITHDRAWAL_INFO,
            authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(true);
    expect(
      isFabricatedWithdrawalFault({
        ...mmStep04State,
        fault: {
          MismatchedWithdrawalContent: {
            committed_withdrawal_info_hash:
              HASH_OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO,
            authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(true);
    // The identity fault is a bare constructor and the content fault a
    // three-field one, so a conviction cannot be re-labelled on the wire.
    expect(FI_STEP_04_STATE_CBOR.endsWith("d87980ff")).toBe(true);
    expect(MM_STEP_04_STATE_CBOR).toContain("d87a9f");
  });
});

import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FabricatedWithdrawalL1Witness } from "../src/prepare-fabricated-withdrawal.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";

// ## Aiken-measured fixture twins
//
// `step-01.ak`'s `authentic_withdrawal_id_v1` / `fabricated_withdrawal_id_v1` /
// `authentic_withdrawal_info_v1` / `diverted_withdrawal_info_v1` /
// `forged_signature_withdrawal_info_v1` /
// `revalidated_withdrawal_info_v1`, and `step-02.ak`'s
// `authentic_withdrawal_datum_v1` / `authentic_inclusion_time_v1`.

export const AUTHENTIC_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: "8b".repeat(32),
  outputIndex: 2n,
};

export const FABRICATED_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: "3a".repeat(32),
  outputIndex: 0n,
};

export const KEY_AUTHENTIC_WITHDRAWAL_ID =
  "d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff";

export const KEY_FABRICATED_WITHDRAWAL_ID =
  "d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff";

export const VALUE_AUTHENTIC_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

export const VALUE_DIVERTED_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5dffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

/**
 * `step-01.ak`'s `withdrawal_content_hash_v1` — the commitment over the leaf's
 * `(body, signature)` only. Decision 0007 keeps the operator-owned `validity`
 * verdict out of it, so `revalidated_withdrawal_info_v1` hashes to exactly
 * `HASH_AUTHENTIC_WITHDRAWAL_CONTENT`.
 */
export const HASH_AUTHENTIC_WITHDRAWAL_CONTENT =
  "283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f";

export const HASH_DIVERTED_WITHDRAWAL_CONTENT =
  "8e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a3";

export const HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT =
  "20b97ac437a93d4f4fc7c0cf4c8a5b84840b408d836723c98f663cc2e8376f22";

/** `user_events.out_ref_to_nonce(authentic_withdrawal_id_v1)`. */
export const NONCE_AUTHENTIC_WITHDRAWAL_ID =
  "630f633bd50fa6888cf4e56be119c4970c013d0c7a45216b7eed46960fac800b";

export const DATUM_AUTHENTIC_WITHDRAWAL_EVENT =
  "d8799fd8799fd8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ffff0f581c57575757575757575757575757575757575757575757575757575757d8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff";

export const FI_WITHDRAWALS_PHAS_ROOT =
  "7e6bcae06cc23954a14d0d2070b40be71abb631bc845a82640c4e8ad3bac7138";

export const FI_WITHDRAWALS_ROOT =
  "520d2e1a48bd0ba1c6424899fc91f0572ad5049e84a912a2c28e841ae3a1d88b";

export const MM_WITHDRAWALS_PHAS_ROOT =
  "9b82564d9ec08f4d54a61982cc5b26972cb0c4ff6ead2d03da141bb0d9ef6b42";

export const MM_WITHDRAWALS_ROOT =
  "ddf6c2b73b0a5be5c6afcb11cbb8c47ecec36a856231911288306a01e411bbed";

export const AU_WITHDRAWALS_PHAS_ROOT =
  "f15ac1acdd0df79c30da7d61d4ff84cb5116a1b99c203d58976d3c10465d3ce7";

export const AU_WITHDRAWALS_ROOT =
  "cc7c414fb11977f998c502f0bead1868a4fc2c743142ae62071f23dcf15543e8";

/** The Aiken fixtures' header window, inherited from `native_binding_fixture_v1`. */
export const HEADER_START_TIME = 10n;

export const HEADER_END_TIME = 20n;

export const AUTHENTIC_INCLUSION_TIME = 15n;

export const FI_HEADER_HASH =
  "3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b4";

export const MM_HEADER_HASH =
  "39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a";

/** `step_04.State` of each Aiken scenario, byte for byte. */
export const FI_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b40a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ffd87980ff";

export const MM_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a0a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd87a9f58208e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a35820283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f0fffff";

export const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

/**
 * The hub oracle's `withdrawal` policy and its `deposit` policy, kept distinct on
 * purpose: the step-02 authenticator must read the former. The Aiken fixture hub
 * datum cannot make that distinction — `hub_reference_input_with_nft_policy` sets
 * every field to the same placeholder — so this is the surface where the correct
 * field read is actually measured.
 */
export const WITHDRAWAL_POLICY_ID = h28(0x19);

export const DEPOSIT_POLICY_ID = h28(0x18);

// ## Challenged-block fixtures

export type WithdrawalLeafEntry = {
  readonly key: string;
  readonly value: string;
};

export const FI_LEAF: WithdrawalLeafEntry = {
  key: KEY_FABRICATED_WITHDRAWAL_ID,
  value: VALUE_AUTHENTIC_WITHDRAWAL_INFO,
};

export const MM_LEAF: WithdrawalLeafEntry = {
  key: KEY_AUTHENTIC_WITHDRAWAL_ID,
  value: VALUE_DIVERTED_WITHDRAWAL_INFO,
};

export const AU_LEAF: WithdrawalLeafEntry = {
  key: KEY_AUTHENTIC_WITHDRAWAL_ID,
  value: VALUE_AUTHENTIC_WITHDRAWAL_INFO,
};

export type WithdrawalsBlockFixture = {
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly withdrawalsRoot: string;
  readonly withdrawalsPhasRoot: string;
  readonly withdrawalCount: bigint;
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly payloadEnvelopeCbor: Buffer;
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
};

/**
 * Re-commits a canonical block so its `withdrawals` source set is exactly `leaves`,
 * then re-derives the counted `withdrawals_root`, the header and the header hash —
 * the shape a faulty operator actually publishes. `withdrawalCountOverride` lies
 * about the cardinality only, leaving the committed root honest.
 */
export const buildWithdrawalsBlockFixture = async ({
  leaves,
  withdrawalCountOverride,
}: {
  readonly leaves: readonly WithdrawalLeafEntry[];
  readonly withdrawalCountOverride?: bigint;
}): Promise<WithdrawalsBlockFixture> => {
  const base = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    startTime: HEADER_START_TIME,
    endTime: HEADER_END_TIME,
    transactionsRootMode: "nativeCompact",
  });
  const counted = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    leaves.map(({ key, value }) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const withdrawalCount = withdrawalCountOverride ?? counted.count;
  const header: SDK.Header = {
    ...base.header,
    withdrawalsRoot: counted.root,
    withdrawalCount,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const entries: SDK.DaPayloadEntry[] = leaves
    .map(({ key, value }): SDK.DaPayloadEntry => [key, value])
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      withdrawals: entries,
      counts: { ...base.payload.block_body.counts, withdrawalCount },
    },
  };
  return {
    header,
    headerHash,
    withdrawalsRoot: counted.root,
    withdrawalsPhasRoot: counted.phasRoot,
    withdrawalCount,
    entries,
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    observation: authenticatedHeaderObservation({
      ...base,
      header,
      headerHash,
    }),
  };
};

export const l1Observation = (
  overrides: Partial<SDK.AuthenticatedL1Observation> = {},
): SDK.AuthenticatedL1Observation => ({
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "watcher-local-node",
    grade: "security",
  },
  chainPoint: { slot: 4242n, blockHash: h32(9) },
  confirmationDepth: 12,
  ...overrides,
});

export const historyEnvironment = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
  retentionAddress: credentialToAddress("Preview", {
    type: "Script",
    hash: h28(0xee),
  }),
};

export const historyWitness = (
  anchor: UTxO,
): FabricatedWithdrawalL1Witness => ({
  observation: l1Observation(),
  hubOraclePolicyId: h28(0x16),
  hubOracleUtxo: hubOracleUtxoFixture(),
  network: "Preview",
  history: historyEnvironment,
  anchor,
});

// ## Step-02 UTxO fixtures
//
// The public history verifier reads the withdrawal policy out of
// the **authentic hub oracle datum**, so the policy is never a caller's claim;
// these literals exist to exercise exactly that read, with the deposit policy set
// to a different value so reading the wrong field cannot pass.

const hubScriptAddress = (byte: number): SDK.AddressData => ({
  paymentCredential: { ScriptCredential: [h28(byte)] as [string] },
  stakeCredential: null,
});

const hubOracleDatumWithWithdrawalPolicy = (
  withdrawalScriptHash: string,
): SDK.HubOracleDatum => ({
  registered_operators: h28(0x11),
  active_operators: h28(0x12),
  retired_operators: h28(0x13),
  scheduler: h28(0x14),
  state_queue: h28(0x15),
  fraud_proof_catalogue: h28(0x16),
  fraud_proof: h28(0x17),
  deposit: DEPOSIT_POLICY_ID,
  withdrawal: withdrawalScriptHash,
  tx_order: h28(0x1a),
  settlement: h28(0x1b),
  payout: h28(0x1c),
  registered_operators_addr: hubScriptAddress(0x11),
  active_operators_addr: hubScriptAddress(0x12),
  retired_operators_addr: hubScriptAddress(0x13),
  scheduler_addr: hubScriptAddress(0x14),
  state_queue_addr: hubScriptAddress(0x15),
  fraud_proof_catalogue_addr: hubScriptAddress(0x16),
  fraud_proof_addr: hubScriptAddress(0x17),
  deposit_addr: hubScriptAddress(0x18),
  withdrawal_addr: hubScriptAddress(0x19),
  tx_order_addr: hubScriptAddress(0x1a),
  settlement_addr: hubScriptAddress(0x1b),
  reserve_addr: hubScriptAddress(0x1c),
  payout_addr: hubScriptAddress(0x1d),
  reserve_observer: h28(0x1e),
});

export const syntheticUtxo = ({
  txIdByte,
  outputIndex,
  datum,
  assets,
}: {
  readonly txIdByte: number;
  readonly outputIndex: number;
  readonly datum: string;
  readonly assets: Record<string, bigint>;
}): UTxO => ({
  txHash: h32(txIdByte),
  outputIndex,
  address: "addr_test1_synthetic",
  assets: { lovelace: 5_000_000n, ...assets },
  datum,
});

export const hubOracleUtxoFixture = (
  withdrawalScriptHash = WITHDRAWAL_POLICY_ID,
): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa1,
    outputIndex: 0,
    datum: Data.to(
      hubOracleDatumWithWithdrawalPolicy(withdrawalScriptHash),
      SDK.HubOracleDatum,
    ),
    assets: { [toUnit(h28(0x16), SDK.HUB_ORACLE_ASSET_NAME)]: 1n },
  }),
  address: credentialToAddress("Preview", { type: "Script", hash: h28(0x16) }),
});

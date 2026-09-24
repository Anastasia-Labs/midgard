/**
 * `fabricated-withdrawal` family (Goal task `Q40`) — evidence builder, L1 witness
 * authentication and submit-side re-derivation.
 *
 * The family is reached by **direct module import**: the `fabricatedWithdrawal`
 * catalogue category and its CLI wiring are parent-owned integration surfaces
 * that land with catalogue registration (#617), so nothing here goes through
 * `src/index.js`, `fraud-proof/catalogue.ts` or `bin.ts`.
 *
 * Every committed-leaf, commitment, nonce and handoff constant below is the
 * value **measured out of the Aiken family modules**
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2,3,4}.ak`
 * and pinned in `demo/midgard-sdk/tests/fabricated-withdrawal.test.ts`. The
 * committed `withdrawals_root`s and the step-04 handoff bytes asserted here are
 * therefore Aiken-measured absolutes, not one TypeScript derivation compared
 * against another.
 *
 * The three challenged blocks are the Aiken fixtures' own scenarios:
 *
 * - **FI** (`fabricated_identity_block_v1`) commits `(FABRICATED_WITHDRAWAL_ID ->
 *   AUTHENTIC_WITHDRAWAL_INFO)`, an identity no withdrawal event ever had;
 * - **MM** (`mismatched_content_block_v1`) commits `(AUTHENTIC_WITHDRAWAL_ID ->
 *   DIVERTED_WITHDRAWAL_INFO)`, the authentic identity with a diverted payout
 *   address; and
 * - **AU** (`authentic_withdrawal_block_v1`) commits the authentic pair, and is the
 *   valid block this family must refuse to convict.
 *
 * All three are re-committed into a real `DaPayload` here, because
 * `tests/helpers/canonical-block-evidence-fixture.ts` hard-wires an empty
 * withdrawal source set (`withdrawals: []`, `withdrawalCount: 0n`).
 *
 * Unlike the deposit twin, a withdrawal leaf value embeds a `Value` map, so the
 * definite-versus-indefinite Plutus map difference between Lucid's encoder and
 * `serialise_data` is load-bearing in this family. Two tests below pin it directly:
 * indefinite leaf bytes are refused as non-canonical, and an indefinite *event
 * datum* opening is accepted because the on-chain step re-serialises whatever wire
 * form it receives.
 */
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { authenticateFabricatedHistoryWitness } from "../src/fabricated-history-witness.js";
import {
  classifyFabricatedWithdrawalFault,
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  type FabricatedWithdrawalL1Witness,
  FabricatedWithdrawalRejection,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../src/prepare-fabricated-withdrawal.js";
import {
  deriveFabricatedWithdrawalStep01Handoff,
  parseSubmitFabricatedWithdrawalInclusion,
} from "../src/submit-fabricated-withdrawal-step-01.js";
import { deriveFabricatedWithdrawalStep03Handoff } from "../src/submit-fabricated-withdrawal-step-03.js";
import { assertFabricatedWithdrawalStep04Finalizable } from "../src/submit-fabricated-withdrawal-step-04.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  createFabricatedWithdrawalEvidenceAuthority,
  requireFabricatedWithdrawalArtifact,
} from "../src/workflow/fabricated-withdrawal-evidence.js";
import { normalizeJournalJson } from "../src/workflow/journal.js";
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

const AUTHENTIC_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: "8b".repeat(32),
  outputIndex: 2n,
};

const FABRICATED_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: "3a".repeat(32),
  outputIndex: 0n,
};

const KEY_AUTHENTIC_WITHDRAWAL_ID =
  "d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff";
const KEY_FABRICATED_WITHDRAWAL_ID =
  "d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ff";

const VALUE_AUTHENTIC_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";
const VALUE_DIVERTED_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5dffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

/**
 * `step-01.ak`'s `withdrawal_content_hash_v1` — the commitment over the leaf's
 * `(body, signature)` only. Decision 0007 keeps the operator-owned `validity`
 * verdict out of it, so `revalidated_withdrawal_info_v1` hashes to exactly
 * `HASH_AUTHENTIC_WITHDRAWAL_CONTENT`.
 */
const HASH_AUTHENTIC_WITHDRAWAL_CONTENT =
  "283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f";
const HASH_DIVERTED_WITHDRAWAL_CONTENT =
  "8e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a3";
const HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT =
  "20b97ac437a93d4f4fc7c0cf4c8a5b84840b408d836723c98f663cc2e8376f22";

/** `user_events.out_ref_to_nonce(authentic_withdrawal_id_v1)`. */
const NONCE_AUTHENTIC_WITHDRAWAL_ID =
  "630f633bd50fa6888cf4e56be119c4970c013d0c7a45216b7eed46960fac800b";

const DATUM_AUTHENTIC_WITHDRAWAL_EVENT =
  "d8799fd8799fd8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ffff0f581c57575757575757575757575757575757575757575757575757575757d8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff";

const FI_WITHDRAWALS_PHAS_ROOT =
  "7e6bcae06cc23954a14d0d2070b40be71abb631bc845a82640c4e8ad3bac7138";
const FI_WITHDRAWALS_ROOT =
  "520d2e1a48bd0ba1c6424899fc91f0572ad5049e84a912a2c28e841ae3a1d88b";
const MM_WITHDRAWALS_PHAS_ROOT =
  "9b82564d9ec08f4d54a61982cc5b26972cb0c4ff6ead2d03da141bb0d9ef6b42";
const MM_WITHDRAWALS_ROOT =
  "ddf6c2b73b0a5be5c6afcb11cbb8c47ecec36a856231911288306a01e411bbed";
const AU_WITHDRAWALS_PHAS_ROOT =
  "f15ac1acdd0df79c30da7d61d4ff84cb5116a1b99c203d58976d3c10465d3ce7";
const AU_WITHDRAWALS_ROOT =
  "cc7c414fb11977f998c502f0bead1868a4fc2c743142ae62071f23dcf15543e8";

/** The Aiken fixtures' header window, inherited from `native_binding_fixture_v1`. */
const HEADER_START_TIME = 10n;
const HEADER_END_TIME = 20n;
const AUTHENTIC_INCLUSION_TIME = 15n;

const FI_HEADER_HASH =
  "3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b4";
const MM_HEADER_HASH =
  "39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a";

/** `step_04.State` of each Aiken scenario, byte for byte. */
const FI_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c3644888b6b3bbab155df4b7b6572e33c50f78407422d4558d19418b40a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ffd87980ff";
const MM_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c39e9477c2cde2da7f1830e232c2d3ece94d4e3760f4b46cc1477334a0a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd87a9f58208e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a35820283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f0fffff";

const DA_PROVENANCE: SDK.EvidenceProvenance = {
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
const WITHDRAWAL_POLICY_ID = h28(0x19);
const DEPOSIT_POLICY_ID = h28(0x18);

// ## Challenged-block fixtures

type WithdrawalLeafEntry = { readonly key: string; readonly value: string };

const FI_LEAF: WithdrawalLeafEntry = {
  key: KEY_FABRICATED_WITHDRAWAL_ID,
  value: VALUE_AUTHENTIC_WITHDRAWAL_INFO,
};
const MM_LEAF: WithdrawalLeafEntry = {
  key: KEY_AUTHENTIC_WITHDRAWAL_ID,
  value: VALUE_DIVERTED_WITHDRAWAL_INFO,
};
const AU_LEAF: WithdrawalLeafEntry = {
  key: KEY_AUTHENTIC_WITHDRAWAL_ID,
  value: VALUE_AUTHENTIC_WITHDRAWAL_INFO,
};

type WithdrawalsBlockFixture = {
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
const buildWithdrawalsBlockFixture = async ({
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

const l1Observation = (
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

const historyEnvironment = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
  retentionAddress: credentialToAddress("Preview", {
    type: "Script",
    hash: h28(0xee),
  }),
};
const historyWitness = (anchor: UTxO): FabricatedWithdrawalL1Witness => ({
  observation: l1Observation(),
  hubOraclePolicyId: h28(0x16),
  hubOracleUtxo: hubOracleUtxoFixture(),
  network: "Preview",
  history: historyEnvironment,
  anchor,
});
const rootHistoryUtxo = (): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa3,
    outputIndex: 0,
    datum: Data.to(
      {
        position: "Root",
        next: null,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
    assets: { [WITHDRAWAL_POLICY_ID]: 1n },
  }),
  address: credentialToAddress("Preview", {
    type: "Script",
    hash: WITHDRAWAL_POLICY_ID,
  }),
});
const absentIdentityWitness = (
  authenticated = true,
): FabricatedWithdrawalL1Witness => {
  const anchor = rootHistoryUtxo();
  return historyWitness(
    authenticated ? anchor : { ...anchor, assets: { lovelace: 5_000_000n } },
  );
};

const presentEventWitness = ({
  observedEventAssetName = NONCE_AUTHENTIC_WITHDRAWAL_ID,
  eventDatumCbor = DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
}: {
  readonly observedEventAssetName?: string;
  readonly eventDatumCbor?: string;
} = {}): FabricatedWithdrawalL1Witness =>
  historyWitness(
    withdrawalEventUtxoFixture({
      assetName: observedEventAssetName,
      datum: Data.from(eventDatumCbor, SDK.WithdrawalOrderDatum),
    }),
  );

const l1AddressOf = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] as [string] },
  stakeCredential: null,
});

/** `step-01.ak`'s `authentic_withdrawal_info_v1`, as typed data. */
const authenticWithdrawalInfo = (): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: { transactionId: "7e".repeat(32), outputIndex: 1n },
    l2_owner: h28(0x9c),
    l2_value: new Map([
      [h28(0x4b), new Map([["6d6964676172642d746f6b656e", 42n]])],
    ]),
    l1_address: l1AddressOf(0x2b),
    l1_datum: "NoDatum",
  },
  signature: ["ad".repeat(32), "be".repeat(64)],
  validity: "WithdrawalIsValid",
});

/** A canonical withdrawal event datum with an arbitrary identity and window. */
const withdrawalEventDatum = ({
  id = AUTHENTIC_WITHDRAWAL_ID,
  info = authenticWithdrawalInfo(),
  inclusionTime = AUTHENTIC_INCLUSION_TIME,
}: {
  readonly id?: SDK.OutputReference;
  readonly info?: SDK.WithdrawalInfo;
  readonly inclusionTime?: bigint;
} = {}): SDK.WithdrawalOrderDatum => ({
  event: { id, info },
  inclusion_time: inclusionTime,
  witness: h28(0x57),
  refund_address: l1AddressOf(0x2b),
  refund_datum: "NoDatum",
});

/** Legacy content fixture encoded canonically before conversion to a history payload. */
const eventDatumBytes = (datum: SDK.WithdrawalOrderDatum): string =>
  SDK.withdrawalEventDatumBytes(datum);

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

const syntheticUtxo = ({
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

const hubOracleUtxoFixture = (
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

const withdrawalEventUtxoFixture = ({
  policyId = WITHDRAWAL_POLICY_ID,
  assetName = NONCE_AUTHENTIC_WITHDRAWAL_ID,
  datum = withdrawalEventDatum(),
}: {
  readonly policyId?: string;
  readonly assetName?: string;
  readonly datum?: SDK.WithdrawalOrderDatum;
} = {}): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa2,
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [NONCE_AUTHENTIC_WITHDRAWAL_ID] },
        next: null,
        protected_until: 0n,
        payload: {
          Order: {
            facts: {
              event_id: datum.event.id,
              inclusion_time: datum.inclusion_time,
              location: { Inline: { payload: historyPayload(datum) } },
              structural_lovelace: 2_000_000n,
              structural_refund_key: h28(0x44),
            },
          },
        },
      },
      SDK.EventHistoryNode,
    ),
    assets: { [toUnit(policyId, assetName)]: 1n },
  }),
  address: credentialToAddress("Preview", {
    type: "Script",
    hash: WITHDRAWAL_POLICY_ID,
  }),
});

/** Read-only public-output fixture. A nonce lookup must never authorize absence. */
const historyLucid = (nodes: readonly UTxO[]): LucidEvolution =>
  ({
    utxosByOutRef: async () => {
      throw new Error("Unexpected identity-nonce lookup");
    },
    utxosAtWithUnit: async (address: string, unit: string) => {
      const hub = hubOracleUtxoFixture();
      return hub.address === address && hub.assets[unit] === 1n ? [hub] : [];
    },
    utxosAt: async (address: string) =>
      nodes.filter((u) => u.address === address),
  }) as unknown as LucidEvolution;

// ## Measured-state twins for the submit-side handoffs
//
// Built from the Aiken constants rather than from a local block, so the step-04
// handoff bytes can be compared against the Aiken scenarios' exact CBOR.

const historyAssets: SDK.Value = new Map([["", new Map([["", 3_000_000n]])]]);
const historyPayload = (
  datum: SDK.WithdrawalOrderDatum,
): SDK.EventHistoryPayload => ({
  WithdrawalPayload: {
    event: datum.event,
    refund_address: datum.refund_address,
    refund_datum: datum.refund_datum,
  },
});
const historyOpening = (
  datum = withdrawalEventDatum(),
  assets = historyAssets,
) =>
  replacePlutusConstrFieldCbor(
    Data.to(
      {
        RetainedEventData: {
          payload: historyPayload(datum),
          original_assets: assets,
        },
      },
      SDK.FabricatedWithdrawalAuthenticContentOpening,
    ),
    [0],
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(historyPayload(datum), SDK.EventHistoryPayload),
    ),
  );
const historyCommitment = SDK.eventHistoryCommitment(
  "30".repeat(28),
  "Withdrawal",
  {
    event_id: AUTHENTIC_WITHDRAWAL_ID,
    inclusion_time: AUTHENTIC_INCLUSION_TIME,
  },
  historyPayload(withdrawalEventDatum()),
  historyAssets,
);

const fiStep03State: SDK.FabricatedWithdrawalStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: FABRICATED_WITHDRAWAL_ID,
  committed_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  verdict: "WithdrawalIdentityAbsent",
};

const mmStep03State: SDK.FabricatedWithdrawalStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
  committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
  verdict: {
    WithdrawalEventObserved: {
      commitment: historyCommitment,
    },
  },
};

describe("Q40 fabricated-withdrawal evidence admission", () => {
  it("admits a withdrawals-bearing block and extracts its committed leaves", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      },
    );
    expect(evidence.grade).toBe("security");
    expect(evidence.provenance.l1.trustClass).toBe("authenticated_cardano_l1");
    expect(evidence.provenance.da.trustClass).toBe(
      "public_or_permissionless_da",
    );
    expect(evidence.headerHash).toBe(fixture.headerHash);
    // The counted withdrawals_root of the MM scenario, measured in Aiken.
    expect(evidence.committedWithdrawalsRoot).toBe(MM_WITHDRAWALS_ROOT);
    expect(evidence.withdrawalCount).toBe(1n);
    expect(evidence.headerStartTime).toBe(HEADER_START_TIME);
    expect(evidence.headerEndTime).toBe(HEADER_END_TIME);
    expect(evidence.entries).toEqual([
      [KEY_AUTHENTIC_WITHDRAWAL_ID, VALUE_DIVERTED_WITHDRAWAL_INFO],
    ]);
  });

  it("refuses operator-private DA provenance", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: {
          trustClass: "operator_admin_api",
          sourceId: "node-admin",
          grade: "diagnostic",
          diagnosticLabel: "operator diagnostic",
        },
      }),
    ).rejects.toBeInstanceOf(SDK.CanonicalEvidenceRejection);
  });

  it("refuses a payload whose embedded header is not the observed one", async () => {
    const observed = await buildWithdrawalsBlockFixture({
      leaves: [MM_LEAF],
    });
    const other = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    expect(other.headerHash).not.toBe(observed.headerHash);
    await expect(
      fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
        observation: observed.observation,
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
  });
});

describe("fabricated-withdrawal production evidence authority", () => {
  const canonicalEvidence = async (
    fixture: WithdrawalsBlockFixture,
  ): Promise<CanonicalBlockEvidence> => {
    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
        minimumConfirmationDepth: 1,
      },
    );
    const withdrawals = evidence.entries.map(([keyCbor, valueCbor]) => ({
      key: Data.from(keyCbor, SDK.OutputReference),
      value: Data.from(valueCbor, SDK.WithdrawalInfo),
      keyBytes: Buffer.from(keyCbor, "hex"),
      valueBytes: Buffer.from(valueCbor, "hex"),
    }));
    return {
      ...evidence,
      observation: fixture.observation,
      header: fixture.header,
      reconstruction: { withdrawals },
    } as unknown as CanonicalBlockEvidence;
  };

  it.each([false, true])(
    "uses the governed withdrawal spending address for ordinary lookup and readmission (stake credential: %s)",
    async (withStake) => {
      const hubPolicy = h28(0x16);
      const hubUnit = toUnit(hubPolicy, SDK.HUB_ORACLE_ASSET_NAME);
      const hubAddress = credentialToAddress("Preview", {
        type: "Script",
        hash: hubPolicy,
      });
      const eventAddress = credentialToAddress(
        "Preview",
        { type: "Script", hash: h28(0xe1) },
        withStake ? { type: "Key", hash: h28(0xe2) } : undefined,
      );
      const hubDatum = {
        ...Data.from(hubOracleUtxoFixture().datum!, SDK.HubOracleDatum),
        withdrawal_addr: await Effect.runPromise(
          SDK.addressDataFromBech32(eventAddress),
        ),
      };
      const hub = {
        ...hubOracleUtxoFixture(),
        address: hubAddress,
        datum: Data.to(hubDatum, SDK.HubOracleDatum),
        assets: { lovelace: 5_000_000n, [hubUnit]: 1n },
      };
      const event = { ...withdrawalEventUtxoFixture(), address: eventAddress };
      const eventUnit = toUnit(
        hubDatum.withdrawal,
        NONCE_AUTHENTIC_WITHDRAWAL_ID,
      );
      const mintPolicyAddress = credentialToAddress("Preview", {
        type: "Script",
        hash: hubDatum.withdrawal,
      });
      expect(eventAddress).not.toBe(mintPolicyAddress);
      expect(event.assets[eventUnit]).toBe(1n);
      let liveEventAddress = eventAddress;
      const queries: string[] = [];
      const authority = createFabricatedWithdrawalEvidenceAuthority({
        history: historyEnvironment,
        lucid: {
          utxosByOutRef: async () => {
            throw new Error("Unexpected identity-nonce lookup");
          },
          utxosAtWithUnit: async (address: string, unit: string) =>
            address === hubAddress && unit === hubUnit ? [hub] : [],
          utxosAt: async (address: string) => {
            queries.push(address);
            return address === liveEventAddress
              ? [{ ...event, address: liveEventAddress }]
              : [];
          },
        } as unknown as LucidEvolution,
        network: "Preview",
        hubOraclePolicyId: hubPolicy,
        minimumConfirmationDepth: 1,
      });
      const ordinary = await buildWithdrawalsBlockFixture({
        leaves: [
          {
            key: KEY_AUTHENTIC_WITHDRAWAL_ID,
            value: VALUE_AUTHENTIC_WITHDRAWAL_INFO,
          },
        ],
      });
      const ordinaryEvidence = await canonicalEvidence(ordinary);
      await expect(
        authority.detect(ordinaryEvidence, h28(0x44)),
      ).resolves.toEqual([]);
      // Reuse the existing mismatch case solely to exercise persisted-event lookup.
      const existing = await buildWithdrawalsBlockFixture({
        leaves: [MM_LEAF],
      });
      const detections = await authority.detect(
        await canonicalEvidence(existing),
        h28(0x44),
      );
      expect(detections).toHaveLength(1);
      expect(detections[0]!.artifact.authenticContent.openingCbor).toBe(
        historyOpening(),
      );
      expect(detections[0]!.artifact.authenticContent.openingCbor).not.toBe(
        event.datum,
      );
      await expect(
        authority.readmit(
          JSON.parse(
            JSON.stringify(normalizeJournalJson(detections[0]!.artifact)),
          ),
        ),
      ).resolves.toEqual(detections[0]!.artifact);
      expect(queries).toContain(eventAddress);
      expect(queries).not.toContain(mintPolicyAddress);
      liveEventAddress = mintPolicyAddress;
      await expect(
        authority.detect(ordinaryEvidence, h28(0x44)),
      ).rejects.toThrow("no unique authenticated witness");
      await expect(authority.readmit(detections[0]!.artifact)).rejects.toThrow(
        "no unique authenticated witness",
      );
      liveEventAddress = eventAddress;
      const altered = withdrawalEventDatum();
      altered.event.info.body.l2_owner = h28(0xe3);
      event.datum = withdrawalEventUtxoFixture({ datum: altered }).datum;
      await expect(authority.readmit(detections[0]!.artifact)).rejects.toThrow(
        "History facts changed before capture",
      );
    },
  );

  it("roundtrips an authenticated list-absence fault through journal normalization", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    const authority = createFabricatedWithdrawalEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([rootHistoryUtxo()]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    const detections = await authority.detect(
      await canonicalEvidence(fixture),
      h28(0x44),
    );
    expect(detections).toHaveLength(1);
    expect(detections[0]!.detection.violationId).toBe("fabricated-withdrawal");
    expect(detections[0]!.artifact.l1Evidence).toEqual({
      kind: "absent_identity",
      historyOutRef: `${h32(0xa3)}#0`,
      retainedDataOutRef: null,
    });
    const readmitted = await authority.readmit(
      JSON.parse(JSON.stringify(normalizeJournalJson(detections[0]!.artifact))),
    );
    expect(
      requireFabricatedWithdrawalArtifact(
        readmitted,
        h28(0x44),
        fixture.headerHash,
      ),
    ).toBe(readmitted);
    await expect(
      authority.readmit(
        normalizeJournalJson({
          ...readmitted,
          withdrawalInclusion: {
            ...readmitted.withdrawalInclusion,
            withdrawalsPhasRoot: h32(0x77),
          },
        }),
      ),
    ).rejects.toThrow(/digest mismatch/u);
    await expect(
      authority.readmit({ ...readmitted, withdrawalIndex: 1 }),
    ).rejects.toThrow(/digest mismatch/u);
    expect(() =>
      requireFabricatedWithdrawalArtifact(
        { ...readmitted },
        h28(0x44),
        fixture.headerHash,
      ),
    ).toThrow(/not re-authenticated/u);
  });
});

describe("Q40 fabricated-withdrawal proof plan", () => {
  it("builds a nonexistent-identity plan from an authenticated absence witness", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    // The Aiken-measured roots of `fabricated_identity_block_v1`.
    expect(fixture.withdrawalsPhasRoot).toBe(FI_WITHDRAWALS_PHAS_ROOT);
    expect(fixture.withdrawalsRoot).toBe(FI_WITHDRAWALS_ROOT);

    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      },
    );
    const outputDir = await mkdtemp(
      join(tmpdir(), "q40-fabricated-withdrawal-"),
    );
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
      withdrawalCount: evidence.withdrawalCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: absentIdentityWitness(),
      outputDir,
    });

    expect(plan.violationId).toBe("fabricated-withdrawal");
    expect(plan.fraudCategoryId).toBe("0000000c");
    expect(plan.threadTokenAssetName).toBe(`0000000c${fixture.headerHash}`);
    expect(plan.withdrawalsPhasRoot).toBe(FI_WITHDRAWALS_PHAS_ROOT);
    expect(plan.committedWithdrawalsRoot).toBe(FI_WITHDRAWALS_ROOT);
    expect(plan.classification.verdict).toBe("WithdrawalIdentityAbsent");
    expect(plan.classification.fault).toBe("NonexistentWithdrawalIdentity");
    expect(plan.step02State).toEqual({
      stateQueuePolicyId: h28(0x15),
      challengedHeaderHash: fixture.headerHash,
      headerStartTime: "10",
      headerEndTime: "20",
      committedWithdrawalIdCbor: KEY_FABRICATED_WITHDRAWAL_ID,
      committedWithdrawalContentHash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
    });
    // An absence proof has no retained content to open at step 03.
    expect(plan.authenticContent.openingCbor).toBeNull();
    expect(
      plan.withdrawalInclusion.withdrawalMembershipProofCbor.length,
    ).toBeGreaterThan(0);
    expect(plan.files).toBeDefined();
    expect(
      JSON.parse(await readFile(plan.files!.withdrawalInclusionPath, "utf8")),
    ).toEqual(plan.withdrawalInclusion);
  });

  it("builds a content-mismatch plan from an authenticated present-event witness", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    // The Aiken-measured roots of `mismatched_content_block_v1`.
    expect(fixture.withdrawalsPhasRoot).toBe(MM_WITHDRAWALS_PHAS_ROOT);
    expect(fixture.withdrawalsRoot).toBe(MM_WITHDRAWALS_ROOT);

    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedWithdrawalsRoot: fixture.withdrawalsRoot,
      withdrawalCount: fixture.withdrawalCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
      committedWithdrawalIdCbor: KEY_AUTHENTIC_WITHDRAWAL_ID,
    });

    expect(plan.classification.verdict).toEqual({
      WithdrawalEventObserved: {
        commitment: { ...historyCommitment, policy: WITHDRAWAL_POLICY_ID },
      },
    });
    expect(plan.classification.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(plan.challengedLeaf.committedWithdrawalContentHash).toBe(
      HASH_DIVERTED_WITHDRAWAL_CONTENT,
    );
    expect(plan.authenticContent.openingCbor).toBe(historyOpening());
  });

  it("refuses leaves that do not open the committed counted withdrawals_root, in the root or in the cardinality", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    // Root arm: the supplied leaf is not the one the header committed.
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [
          [KEY_FABRICATED_WITHDRAWAL_ID, VALUE_AUTHENTIC_WITHDRAWAL_INFO],
        ],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "withdrawals_root_mismatch" });

    // Cardinality arm: the header's own `withdrawal_count` disagrees with the
    // rebuilt leaf count, which is the half of the counted-root check a
    // root-only comparison would miss.
    const lied = await buildWithdrawalsBlockFixture({
      leaves: [MM_LEAF],
      withdrawalCountOverride: 7n,
    });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: lied.headerHash,
        committedWithdrawalsRoot: MM_WITHDRAWALS_ROOT,
        withdrawalCount: lied.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: lied.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({ code: "withdrawals_root_mismatch" });
  });

  it("refuses an empty withdrawal source set and a pinned leaf the header never committed", async () => {
    const empty = await buildWithdrawalsBlockFixture({ leaves: [] });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: empty.headerHash,
        committedWithdrawalsRoot: empty.withdrawalsRoot,
        withdrawalCount: empty.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "no_committed_withdrawal_leaf" });

    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: absentIdentityWitness(),
        committedWithdrawalIdCbor: KEY_FABRICATED_WITHDRAWAL_ID,
      }),
    ).rejects.toMatchObject({ code: "leaf_not_committed" });
  });

  it("refuses a committed leaf whose bytes are not the serialise_data form the on-chain membership check recomputes", async () => {
    // Lucid's typed encoder writes the `l2_value` map in indefinite form, which is
    // not what `cbor.serialise` produces for the same typed leaf. A block that
    // committed those bytes commits a leaf no step can reproduce, so the family
    // refuses it rather than building a proof that cannot verify on chain.
    const rawLucidValue = Data.to(
      {
        ...authenticWithdrawalInfo(),
        body: {
          ...authenticWithdrawalInfo().body,
          l1_address: l1AddressOf(0x5d),
        },
      },
      SDK.WithdrawalInfo,
    );
    expect(rawLucidValue).not.toBe(VALUE_DIVERTED_WITHDRAWAL_INFO);
    const fixture = await buildWithdrawalsBlockFixture({
      leaves: [{ key: KEY_AUTHENTIC_WITHDRAWAL_ID, value: rawLucidValue }],
    });
    // The counted root is honest about the bytes the operator published, so the
    // refusal has to come from the leaf decoder, not from the root gate.
    expect(fixture.withdrawalsRoot).not.toBe(MM_WITHDRAWALS_ROOT);
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({ code: "non_canonical_da_payload" });
  });
});

describe("Q40 fabricated-withdrawal L1 witness authentication", () => {
  const leafOf = async (
    leaf: WithdrawalLeafEntry,
    witness: FabricatedWithdrawalL1Witness,
  ) => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [leaf] });
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedWithdrawalsRoot: fixture.withdrawalsRoot,
      withdrawalCount: fixture.withdrawalCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness,
    });
    return plan.challengedLeaf;
  };

  it("refuses an absence claim without an authenticated history token, and any witness that is not authenticated L1 security-grade evidence", async () => {
    const leaf = await leafOf(FI_LEAF, absentIdentityWitness());
    // A gap-shaped datum without its list NFT cannot authenticate absence.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: absentIdentityWitness(false),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: {
          ...absentIdentityWitness(),
          observation: l1Observation({
            provenance: {
              trustClass: "operator_admin_api",
              sourceId: "node-admin",
              grade: "diagnostic",
              diagnosticLabel: "operator diagnostic",
            },
          }),
        },
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
  });

  it("refuses a present-event witness that is not bound to the committed identity", async () => {
    const leaf = await leafOf(MM_LEAF, presentEventWitness());
    // The observed asset name is not `out_ref_to_nonce(committed_withdrawal_id)`.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({ observedEventAssetName: h32(0x4d) }),
      }),
    ).rejects.toMatchObject({
      code: "history_witness_invalid",
    });
    // The retained datum names a different withdrawal identity.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({
          eventDatumCbor: eventDatumBytes(
            withdrawalEventDatum({ id: FABRICATED_WITHDRAWAL_ID }),
          ),
        }),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
  });

  it("refuses to challenge the authentic block, whose header committed exactly the authentic order", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [AU_LEAF] });
    // The Aiken-measured roots of `authentic_withdrawal_block_v1`.
    expect(fixture.withdrawalsPhasRoot).toBe(AU_WITHDRAWALS_PHAS_ROOT);
    expect(fixture.withdrawalsRoot).toBe(AU_WITHDRAWALS_ROOT);
    const attempt = async () =>
      await prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      });
    await expect(attempt()).rejects.toBeInstanceOf(
      FabricatedWithdrawalRejection,
    );
    await expect(attempt()).rejects.toMatchObject({
      code: "authentic_content_matches_commitment",
    });
  });

  it("refuses to challenge a block whose committed leaf differs from the authentic order only in its operator-owned validity verdict", async () => {
    // Decision 0007: the L1 order datum's `WithdrawalIsValid` is a placeholder,
    // so the honest verdict for a withdrawal whose L2 output does not exist is
    // the operator's own claim, not fabricated content. This is the honest
    // control of every withdrawal fixture: it must survive this family with the
    // verdict it actually earned, and a wrong verdict belongs to
    // `withdrawalMistag`.
    const revalidated: SDK.WithdrawalInfo = {
      ...authenticWithdrawalInfo(),
      validity: "NonExistentWithdrawalUtxo",
    };
    const value = SDK.committedWithdrawalValueBytes(revalidated);
    expect(value).not.toBe(VALUE_AUTHENTIC_WITHDRAWAL_INFO);
    expect(
      await Effect.runPromise(SDK.withdrawalContentCommitment(revalidated)),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_CONTENT);
    const fixture = await buildWithdrawalsBlockFixture({
      leaves: [{ key: KEY_AUTHENTIC_WITHDRAWAL_ID, value }],
    });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({
      name: "FabricatedWithdrawalRejectionV1",
      code: "authentic_content_matches_commitment",
    });
  });

  it("proves an authentic event ineligible for the challenged block, on either side of the window", async () => {
    const leaf = await leafOf(MM_LEAF, presentEventWitness());
    for (const inclusionTime of [HEADER_START_TIME, HEADER_END_TIME + 1n]) {
      await expect(
        classifyFabricatedWithdrawalFault({
          leaf,
          headerStartTime: HEADER_START_TIME,
          headerEndTime: HEADER_END_TIME,
          witness: presentEventWitness({
            eventDatumCbor: eventDatumBytes(
              withdrawalEventDatum({ inclusionTime }),
            ),
          }),
        }),
      ).resolves.toMatchObject({
        fault: {
          IneligibleWithdrawalEvent: { event_inclusion_time: inclusionTime },
        },
      });
    }
  });
});

it("authenticates an equal-key filler as absence without counting its funds or using nonce liveness", async () => {
  const anchor = withdrawalEventUtxoFixture();
  const node = Data.from(anchor.datum!, SDK.EventHistoryNode);
  const filler = {
    ...anchor,
    datum: Data.to(
      { ...node, payload: { Filler: { refund_key: h28(0x44) } } },
      SDK.EventHistoryNode,
    ),
  };
  const result = await authenticateFabricatedHistoryWitness(
    historyWitness(filler),
    "Withdrawal",
    AUTHENTIC_WITHDRAWAL_ID,
  );
  expect(result.witness.kind).toBe("Absent");
  expect(result.captured).toBeUndefined();
  expect(result.witness.anchor.utxo.assets.lovelace).toBe(5_000_000n);
});

describe("Q40 fabricated-withdrawal submit-side re-derivation", () => {
  it("re-derives the step-01 handoff from the on-chain header and refuses a PHAS root or leaf encoding that does not open it", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedWithdrawalsRoot: fixture.withdrawalsRoot,
      withdrawalCount: fixture.withdrawalCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
    });
    const inclusion = parseSubmitFabricatedWithdrawalInclusion(
      plan.withdrawalInclusion,
    );
    const handoff = await deriveFabricatedWithdrawalStep01Handoff({
      stateQueuePolicyId: h28(0x15),
      header: fixture.header,
      headerHash: fixture.headerHash,
      inclusion,
    });
    expect(handoff.committedWithdrawal.domain).toBe(
      SDK.ROOT_DOMAINS.withdrawals,
    );
    expect(handoff.committedWithdrawal.root).toBe(MM_WITHDRAWALS_ROOT);
    expect(handoff.committedWithdrawal.phas_root).toBe(
      MM_WITHDRAWALS_PHAS_ROOT,
    );
    expect(handoff.committedWithdrawal.count).toBe(1n);
    expect(handoff.step02State).toEqual({
      state_queue_policy: h28(0x15),
      challenged_header_hash: fixture.headerHash,
      header_start_time: HEADER_START_TIME,
      header_end_time: HEADER_END_TIME,
      committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
      committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
    });

    await expect(
      deriveFabricatedWithdrawalStep01Handoff({
        stateQueuePolicyId: h28(0x15),
        header: fixture.header,
        headerHash: fixture.headerHash,
        inclusion: {
          ...inclusion,
          withdrawalsPhasRoot: FI_WITHDRAWALS_PHAS_ROOT,
        },
      }),
    ).rejects.toThrow(/does not open the committed withdrawals_root/u);

    // The submit side refuses leaf bytes in Lucid's indefinite-map form too: the
    // membership check on chain hashes the `serialise_data` bytes.
    await expect(
      deriveFabricatedWithdrawalStep01Handoff({
        stateQueuePolicyId: h28(0x15),
        header: fixture.header,
        headerHash: fixture.headerHash,
        inclusion: {
          ...inclusion,
          committedWithdrawalInfoCbor: Data.to(
            {
              ...authenticWithdrawalInfo(),
              body: {
                ...authenticWithdrawalInfo().body,
                l1_address: l1AddressOf(0x5d),
              },
            },
            SDK.WithdrawalInfo,
          ),
        },
      }),
    ).rejects.toThrow(/is not in serialiseData form/u);
  });

  it("authenticates the withdrawal event UTxO through the hub oracle's withdrawal policy, not its deposit policy", async () => {
    const authenticate = (anchor: UTxO) =>
      authenticateFabricatedHistoryWitness(
        historyWitness(anchor),
        "Withdrawal",
        AUTHENTIC_WITHDRAWAL_ID,
      );
    const authenticated = await authenticate(withdrawalEventUtxoFixture());
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...historyWitness(withdrawalEventUtxoFixture()),
          hubOracleUtxo: {
            ...hubOracleUtxoFixture(),
            assets: { lovelace: 5_000_000n },
          },
        },
        "Withdrawal",
        AUTHENTIC_WITHDRAWAL_ID,
      ),
    ).rejects.toThrow("authentic inline hub oracle");
    await expect(
      authenticate({
        ...withdrawalEventUtxoFixture(),
        address: credentialToAddress("Preview", {
          type: "Key",
          hash: h28(0x44),
        }),
      }),
    ).rejects.toThrow("Invalid authenticated history output shape");

    expect(authenticated.deployment.policyId).toBe(WITHDRAWAL_POLICY_ID);
    expect(authenticated.witness.anchor.key).toBe(
      NONCE_AUTHENTIC_WITHDRAWAL_ID,
    );
    expect(authenticated.captured?.commitment).toEqual({
      ...historyCommitment,
      policy: WITHDRAWAL_POLICY_ID,
    });
    expect(authenticated.captured?.originalAssets).toEqual(historyAssets);

    // The hub oracle's *deposit* policy is a different event family's policy, so
    // an event NFT minted under it is not an authentic withdrawal event even
    // though the asset name is the authentic nonce.
    await expect(
      authenticate(withdrawalEventUtxoFixture({ policyId: DEPOSIT_POLICY_ID })),
    ).rejects.toThrow(/no unique authenticated witness/u);
    // The authentic policy and nonce, but a datum for another identity.
    await expect(
      authenticate(
        withdrawalEventUtxoFixture({
          datum: withdrawalEventDatum({ id: FABRICATED_WITHDRAWAL_ID }),
        }),
      ),
    ).rejects.toThrow(/identity differs/u);
  });

  it("opens step-02's retained commitment into the Aiken scenarios' exact step-04 handoffs, for both fidelity fabrications", async () => {
    const absent = await deriveFabricatedWithdrawalStep03Handoff({
      state: fiStep03State,
    });
    expect(absent.opening).toBe("NoAuthenticContent");
    expect(absent.fault).toBe("NonexistentWithdrawalIdentity");
    expect(
      Data.to(absent.step04State, SDK.FabricatedWithdrawalStep04State),
    ).toBe(FI_STEP_04_STATE_CBOR);

    const present = await deriveFabricatedWithdrawalStep03Handoff({
      state: mmStep03State,
      openingCbor: historyOpening(),
    });
    expect(present.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(
      Data.to(present.step04State, SDK.FabricatedWithdrawalStep04State),
    ).toBe(MM_STEP_04_STATE_CBOR);

    // One 32-byte inequality settles body and signature: a forged signature
    // convicts on the same rule as the diverted payout body.
    const forged = await deriveFabricatedWithdrawalStep03Handoff({
      state: {
        ...mmStep03State,
        committed_withdrawal_content_hash:
          HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
      },
      openingCbor: historyOpening(),
    });
    expect(forged.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash:
          HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });

    // Lucid's indefinite-map wire form of the same event datum is accepted,
    // because the on-chain step re-serialises the redeemer before hashing it —
    // the one place this family must *not* demand byte-identical CBOR.
    const rawLucidDatum = Data.to(
      withdrawalEventDatum(),
      SDK.WithdrawalOrderDatum,
    );
    expect(rawLucidDatum).not.toBe(DATUM_AUTHENTIC_WITHDRAWAL_EVENT);
    const normalized = await deriveFabricatedWithdrawalStep03Handoff({
      state: mmStep03State,
      openingCbor: historyOpening(
        Data.from(rawLucidDatum, SDK.WithdrawalOrderDatum),
      ),
    });
    expect(normalized.fault).toEqual(present.fault);
  });

  it("refuses step-03 openings that do not pair with the verdict or are not the authenticated bytes, and refuses to finalize a misfiled or unestablished conviction", async () => {
    // A present-event verdict opened as an absence would convert a content
    // dispute into the strictly stronger non-existence conviction.
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({ state: mmStep03State }),
    ).rejects.toThrow(/requires its retained payload and original Value/u);
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({
        state: fiStep03State,
        openingCbor: historyOpening(),
      }),
    ).rejects.toThrow(/absence admits no retained event opening/u);
    // Only the hash equality makes supplied bytes authentic.
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({
        state: mmStep03State,
        openingCbor: historyOpening(
          withdrawalEventDatum(),
          new Map([["", new Map([["", 3_000_001n]])]]),
        ),
      }),
    ).rejects.toThrow(/does not match the authenticated history commitment/u);

    const established = SDK.fabricatedWithdrawalStep04State(mmStep03State, {
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(() =>
      assertFabricatedWithdrawalStep04Finalizable({
        state: established,
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).not.toThrow();
    // Filed against a header the thread token does not name.
    expect(() =>
      assertFabricatedWithdrawalStep04Finalizable({
        state: established,
        fraudulentHeaderHash: FI_HEADER_HASH,
      }),
    ).toThrow(/thread state names challenged header/u);
    // An authentic event outside the challenged block's window is not this
    // block's fault, so it can never become a permanent conviction.
    expect(() =>
      assertFabricatedWithdrawalStep04Finalizable({
        state: {
          ...established,
          fault: {
            MismatchedWithdrawalContent: {
              committed_withdrawal_content_hash:
                HASH_DIVERTED_WITHDRAWAL_CONTENT,
              authentic_withdrawal_content_hash:
                HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
              event_inclusion_time: HEADER_END_TIME + 1n,
            },
          },
        },
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).toThrow(/not an established fabricated-withdrawal fault/u);
  });
});

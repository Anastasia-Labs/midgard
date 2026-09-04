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

import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
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
import { authenticateFabricatedWithdrawalEventUtxo } from "../src/submit-fabricated-withdrawal-step-02.js";
import { deriveFabricatedWithdrawalStep03Handoff } from "../src/submit-fabricated-withdrawal-step-03.js";
import { assertFabricatedWithdrawalStep04Finalizable } from "../src/submit-fabricated-withdrawal-step-04.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  createFabricatedWithdrawalEvidenceAuthority,
  requireFabricatedWithdrawalArtifact,
} from "../src/workflow/fabricated-withdrawal-evidence.js";
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
// `overridden_validity_withdrawal_info_v1`, and `step-02.ak`'s
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

const HASH_AUTHENTIC_WITHDRAWAL_INFO =
  "f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a0";
const HASH_DIVERTED_WITHDRAWAL_INFO =
  "6d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de973";
const HASH_FORGED_SIGNATURE_WITHDRAWAL_INFO =
  "a3b578b5798f5dd0fd76e68a612d9d8d4af873908c1855e73e33fdb340402939";
const HASH_OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO =
  "56b23f1caeca79d65bf2dcc91c4e1f47d7904b03b442481491edd3acae9f64a8";

/** `user_events.out_ref_to_nonce(authentic_withdrawal_id_v1)`. */
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
  "735fcb9ab869fa81efc508ca11991963a774ae8024658d6de1889967";
const MM_HEADER_HASH =
  "44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd4728182";

/** `step_04.State` of each Aiken scenario, byte for byte. */
const FI_STEP_04_STATE_CBOR =
  "d8799f581c735fcb9ab869fa81efc508ca11991963a774ae8024658d6de18899670a14d8799f58203a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a00ffd87980ff";
const MM_STEP_04_STATE_CBOR =
  "d8799f581c44201f07972dae5999a6a5f5b8659c0ac65fb96168f3035dd47281820a14d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd87a9f58206d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de9735820f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a00fffff";

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

const absentIdentityWitness = (
  liveOutputReferences: readonly SDK.OutputReference[] = [
    FABRICATED_WITHDRAWAL_ID,
  ],
): FabricatedWithdrawalL1Witness => ({
  kind: "absent_identity",
  observation: l1Observation(),
  liveOutputReferences,
});

const presentEventWitness = ({
  observedEventAssetName = NONCE_AUTHENTIC_WITHDRAWAL_ID,
  eventDatumCbor = DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
}: {
  readonly observedEventAssetName?: string;
  readonly eventDatumCbor?: string;
} = {}): FabricatedWithdrawalL1Witness => ({
  kind: "present_event",
  observation: l1Observation(),
  withdrawalEventPolicyId: WITHDRAWAL_POLICY_ID,
  observedEventAssetName,
  eventDatumCbor,
});

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

/** The `serialise_data` bytes of a withdrawal event datum — what both steps hash. */
const eventDatumBytes = (datum: SDK.WithdrawalOrderDatum): string =>
  SDK.withdrawalEventDatumBytes(datum);

// ## Step-02 UTxO fixtures
//
// `authenticateFabricatedWithdrawalEventUtxo` reads the withdrawal policy out of
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
): UTxO =>
  syntheticUtxo({
    txIdByte: 0xa1,
    outputIndex: 0,
    datum: Data.to(
      hubOracleDatumWithWithdrawalPolicy(withdrawalScriptHash),
      SDK.HubOracleDatum,
    ),
    assets: {},
  });

const withdrawalEventUtxoFixture = ({
  policyId = WITHDRAWAL_POLICY_ID,
  assetName = NONCE_AUTHENTIC_WITHDRAWAL_ID,
  datum = withdrawalEventDatum(),
}: {
  readonly policyId?: string;
  readonly assetName?: string;
  readonly datum?: SDK.WithdrawalOrderDatum;
} = {}): UTxO =>
  syntheticUtxo({
    txIdByte: 0xa2,
    outputIndex: 1,
    datum: Data.to(datum, SDK.WithdrawalOrderDatum),
    assets: { [toUnit(policyId, assetName)]: 1n },
  });

// ## Measured-state twins for the submit-side handoffs
//
// Built from the Aiken constants rather than from a local block, so the step-04
// handoff bytes can be compared against the Aiken scenarios' exact CBOR.

const fiStep03State: SDK.FabricatedWithdrawalStep03State = {
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: FABRICATED_WITHDRAWAL_ID,
  committed_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
  verdict: "WithdrawalIdentityAbsent",
};

const mmStep03State: SDK.FabricatedWithdrawalStep03State = {
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
  committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
  verdict: {
    WithdrawalEventObserved: {
      event_datum_hash: HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
      event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
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

  it("derives and re-admits an authenticated live-identity fault", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    const authority = createFabricatedWithdrawalEvidenceAuthority({
      lucid: {
        utxosByOutRef: async () => [
          syntheticUtxo({
            txIdByte: 0x3a,
            outputIndex: 0,
            datum: "d87980",
            assets: {},
          }),
        ],
      } as unknown as LucidEvolution,
      network: "Preview",
      hubOraclePolicyId: WITHDRAWAL_POLICY_ID,
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
      unspentOutRef: `${FABRICATED_WITHDRAWAL_ID.transactionId}#0`,
    });
    const readmitted = await authority.readmit(detections[0]!.artifact);
    expect(
      requireFabricatedWithdrawalArtifact(
        readmitted,
        h28(0x44),
        fixture.headerHash,
      ),
    ).toBe(readmitted);
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
      challengedHeaderHash: fixture.headerHash,
      headerStartTime: "10",
      headerEndTime: "20",
      committedWithdrawalIdCbor: KEY_FABRICATED_WITHDRAWAL_ID,
      committedWithdrawalInfoHash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
    });
    // An absence proof has no retained content to open at step 03.
    expect(plan.authenticContent.eventDatumCbor).toBeNull();
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
        event_datum_hash: HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(plan.classification.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
        authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(plan.challengedLeaf.committedWithdrawalInfoHash).toBe(
      HASH_DIVERTED_WITHDRAWAL_INFO,
    );
    expect(plan.authenticContent.eventDatumCbor).toBe(
      DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    );
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

  it("refuses an absence claim that rests on a consumed UTxO, and any witness that is not authenticated L1 security-grade evidence", async () => {
    const leaf = await leafOf(FI_LEAF, absentIdentityWitness());
    // The committed identity is absent from the authenticated live set, so its
    // absence cannot be established: no fallback, no downgrade.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: absentIdentityWitness([AUTHENTIC_WITHDRAWAL_ID]),
      }),
    ).rejects.toMatchObject({ code: "consumed_live_utxo_fallback_refused" });
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
    ).rejects.toBeInstanceOf(SDK.CanonicalEvidenceRejection);
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
      code: "withdrawal_identity_observation_mismatch",
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
    ).rejects.toMatchObject({ code: "event_identity_mismatch" });
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

  it("refuses an authentic event that was not due for the challenged block, on either side of the window", async () => {
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
      ).rejects.toMatchObject({
        name: "FabricatedWithdrawalRejectionV1",
        code: "event_not_due_for_block",
      });
    }
  });
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
      challenged_header_hash: fixture.headerHash,
      header_start_time: HEADER_START_TIME,
      header_end_time: HEADER_END_TIME,
      committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
      committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
    });

    await expect(
      deriveFabricatedWithdrawalStep01Handoff({
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
    const state: SDK.FabricatedWithdrawalStep02State = {
      challenged_header_hash: MM_HEADER_HASH,
      header_start_time: HEADER_START_TIME,
      header_end_time: HEADER_END_TIME,
      committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
      committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
    };
    const authenticated = await authenticateFabricatedWithdrawalEventUtxo({
      state,
      hubOracleUtxo: hubOracleUtxoFixture(),
      eventUtxo: withdrawalEventUtxoFixture(),
    });
    expect(authenticated.withdrawalPolicyId).toBe(WITHDRAWAL_POLICY_ID);
    expect(authenticated.expectedEventAssetName).toBe(
      NONCE_AUTHENTIC_WITHDRAWAL_ID,
    );
    expect(authenticated.eventDatumHash).toBe(
      HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
    );

    // The hub oracle's *deposit* policy is a different event family's policy, so
    // an event NFT minted under it is not an authentic withdrawal event even
    // though the asset name is the authentic nonce.
    await expect(
      authenticateFabricatedWithdrawalEventUtxo({
        state,
        hubOracleUtxo: hubOracleUtxoFixture(),
        eventUtxo: withdrawalEventUtxoFixture({ policyId: DEPOSIT_POLICY_ID }),
      }),
    ).rejects.toThrow(/does not carry the authentic withdrawal event NFT/u);
    // The authentic policy and nonce, but a datum for another identity.
    await expect(
      authenticateFabricatedWithdrawalEventUtxo({
        state,
        hubOracleUtxo: hubOracleUtxoFixture(),
        eventUtxo: withdrawalEventUtxoFixture({
          datum: withdrawalEventDatum({ id: FABRICATED_WITHDRAWAL_ID }),
        }),
      }),
    ).rejects.toThrow(/not the committed identity/u);
  });

  it("opens step-02's retained commitment into the Aiken scenarios' exact step-04 handoffs, for all three fidelity fabrications", async () => {
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
      eventDatumCbor: DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    });
    expect(present.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
        authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(
      Data.to(present.step04State, SDK.FabricatedWithdrawalStep04State),
    ).toBe(MM_STEP_04_STATE_CBOR);

    // One 32-byte inequality settles body, signature and validity: a forged
    // signature and an overridden validity verdict convict on the same rule as
    // the diverted payout body.
    for (const committedHash of [
      HASH_FORGED_SIGNATURE_WITHDRAWAL_INFO,
      HASH_OVERRIDDEN_VALIDITY_WITHDRAWAL_INFO,
    ]) {
      const handoff = await deriveFabricatedWithdrawalStep03Handoff({
        state: {
          ...mmStep03State,
          committed_withdrawal_info_hash: committedHash,
        },
        eventDatumCbor: DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
      });
      expect(handoff.fault).toEqual({
        MismatchedWithdrawalContent: {
          committed_withdrawal_info_hash: committedHash,
          authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
          event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
        },
      });
    }

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
      eventDatumCbor: rawLucidDatum,
    });
    expect(normalized.fault).toEqual(present.fault);
  });

  it("refuses step-03 openings that do not pair with the verdict or are not the authenticated bytes, and refuses to finalize a misfiled or unestablished conviction", async () => {
    // A present-event verdict opened as an absence would convert a content
    // dispute into the strictly stronger non-existence conviction.
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({ state: mmStep03State }),
    ).rejects.toThrow(/does not pair|non-existence conviction/u);
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({
        state: fiStep03State,
        eventDatumCbor: DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
      }),
    ).rejects.toThrow(/does not pair with the L1 verdict/u);
    // Only the hash equality makes supplied bytes authentic.
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({
        state: mmStep03State,
        eventDatumCbor: eventDatumBytes(
          withdrawalEventDatum({ inclusionTime: 16n }),
        ),
      }),
    ).rejects.toThrow(/not the commitment/u);

    const established = SDK.fabricatedWithdrawalStep04State(mmStep03State, {
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
        authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
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
              committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
              authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
              event_inclusion_time: HEADER_END_TIME + 1n,
            },
          },
        },
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).toThrow(/not an established fabricated-withdrawal fault/u);
  });
});

import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FabricatedDepositL1Witness } from "../src/prepare-fabricated-deposit.js";
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
// `step-01.ak`'s `authentic_deposit_id_v1` / `fabricated_deposit_id_v1` /
// `authentic_deposit_info_v1` / `diverted_deposit_info_v1`, and `step-02.ak`'s
// `authentic_deposit_datum_v1` / `authentic_inclusion_time_v1`.

export const AUTHENTIC_DEPOSIT_ID: SDK.OutputReference = {
  transactionId: "7a".repeat(32),
  outputIndex: 3n,
};

export const FABRICATED_DEPOSIT_ID: SDK.OutputReference = {
  transactionId: "5c".repeat(32),
  outputIndex: 0n,
};

export const KEY_AUTHENTIC_DEPOSIT_ID =
  "d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff";

export const KEY_FABRICATED_DEPOSIT_ID =
  "d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ff";

export const VALUE_AUTHENTIC_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ff";

export const VALUE_DIVERTED_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";

export const HASH_AUTHENTIC_DEPOSIT_INFO =
  "89ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863";

export const HASH_DIVERTED_DEPOSIT_INFO =
  "0ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335";

/** `user_events.out_ref_to_nonce(authentic_deposit_id_v1)`. */
export const NONCE_AUTHENTIC_DEPOSIT_ID =
  "db496846395df718772b56f398cc7c7882869ddc0154fd035d63da1c3e95dd06";

export const DATUM_AUTHENTIC_DEPOSIT_EVENT =
  "d8799fd8799fd8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ffff0f581c57575757575757575757575757575757575757575757575757575757ff";

export const FI_DEPOSITS_PHAS_ROOT =
  "b0374d9482ece991566bebfa200b6577eaeed4a2bcc56e25eb28e8d4f06655b4";

export const FI_DEPOSITS_ROOT =
  "60b531d1961d33baf3b6e83da728b0fc1497faf43f78e2cdaf9e03aae9959890";

export const MM_DEPOSITS_PHAS_ROOT =
  "4b0c3a7234e798d045b06088ab4933c71e22d74781c9457f022987bf8e416c22";

export const MM_DEPOSITS_ROOT =
  "880ba7ceb072fce058c5e8f9adbbe9b5bcc3efdcb53ec82039f142f577c47ab4";

/** The Aiken fixtures' header window, inherited from `native_binding_fixture_v1`. */
export const HEADER_START_TIME = 10n;

export const HEADER_END_TIME = 20n;

export const AUTHENTIC_INCLUSION_TIME = 15n;

export const FI_HEADER_HASH =
  "6a404d9de58a96111da77453168c29f4d23007592856b93e06b6bb46";

export const MM_HEADER_HASH =
  "b50943cc7ac3d1b46b37e1b33223419dcb3d4dbd03564f4966918ec6";

/** `step_04.State` of each Aiken scenario, byte for byte. */
export const FI_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c6a404d9de58a96111da77453168c29f4d23007592856b93e06b6bb460a14d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ffd87980ff";

export const MM_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581cb50943cc7ac3d1b46b37e1b33223419dcb3d4dbd03564f4966918ec60a14d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd87a9f58200ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335582089ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c68630fffff";

export const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

export const DEPOSIT_POLICY_ID = h28(0x18);

// ## Challenged-block fixtures

type DepositLeafEntry = { readonly key: string; readonly value: string };

export const FI_LEAF: DepositLeafEntry = {
  key: KEY_FABRICATED_DEPOSIT_ID,
  value: VALUE_AUTHENTIC_DEPOSIT_INFO,
};

export const MM_LEAF: DepositLeafEntry = {
  key: KEY_AUTHENTIC_DEPOSIT_ID,
  value: VALUE_DIVERTED_DEPOSIT_INFO,
};

export type DepositsBlockFixture = {
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly depositsRoot: string;
  readonly depositsPhasRoot: string;
  readonly depositCount: bigint;
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly payloadEnvelopeCbor: Buffer;
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
};

/**
 * Re-commits a canonical block so its `deposits` source set is exactly `leaves`,
 * then re-derives the counted `deposits_root`, the header and the header hash —
 * the shape a faulty operator actually publishes. `depositCountOverride` lies
 * about the cardinality only, leaving the committed root honest.
 */
export const buildDepositsBlockFixture = async ({
  leaves,
  depositCountOverride,
}: {
  readonly leaves: readonly DepositLeafEntry[];
  readonly depositCountOverride?: bigint;
}): Promise<DepositsBlockFixture> => {
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
    SDK.ROOT_DOMAINS.deposits,
    leaves.map(({ key, value }) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const depositCount = depositCountOverride ?? counted.count;
  const header: SDK.Header = {
    ...base.header,
    depositsRoot: counted.root,
    depositCount,
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
      deposits: entries,
      counts: { ...base.payload.block_body.counts, depositCount },
    },
  };
  return {
    header,
    headerHash,
    depositsRoot: counted.root,
    depositsPhasRoot: counted.phasRoot,
    depositCount,
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

export const historyWitness = (anchor: UTxO): FabricatedDepositL1Witness => ({
  observation: l1Observation(),
  hubOraclePolicyId: h28(0x16),
  hubOracleUtxo: hubOracleUtxoFixture(),
  network: "Preview",
  history: historyEnvironment,
  anchor,
});

export const rootHistoryUtxo = (): UTxO => ({
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
    assets: { [DEPOSIT_POLICY_ID]: 1n },
  }),
  address: credentialToAddress("Preview", {
    type: "Script",
    hash: DEPOSIT_POLICY_ID,
  }),
});

export const absentIdentityWitness = (
  authenticated = true,
): FabricatedDepositL1Witness => {
  const anchor = rootHistoryUtxo();
  return historyWitness(
    authenticated ? anchor : { ...anchor, assets: { lovelace: 5_000_000n } },
  );
};

// ## Step-02 UTxO fixtures
//
// The public history verifier reads the deposit policy out of the
// **authentic hub oracle datum**, so the policy is never a caller's claim; these
// literals exist to exercise exactly that read.

const hubScriptAddress = (byte: number): SDK.AddressData => ({
  paymentCredential: { ScriptCredential: [h28(byte)] as [string] },
  stakeCredential: null,
});

const hubOracleDatumWithDepositPolicy = (
  depositScriptHash: string,
): SDK.HubOracleDatum => ({
  registered_operators: h28(0x11),
  active_operators: h28(0x12),
  retired_operators: h28(0x13),
  scheduler: h28(0x14),
  state_queue: h28(0x15),
  fraud_proof_catalogue: h28(0x16),
  fraud_proof: h28(0x17),
  deposit: depositScriptHash,
  withdrawal: h28(0x19),
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
  depositScriptHash = DEPOSIT_POLICY_ID,
): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa1,
    outputIndex: 0,
    datum: Data.to(
      hubOracleDatumWithDepositPolicy(depositScriptHash),
      SDK.HubOracleDatum,
    ),
    assets: { [toUnit(h28(0x16), SDK.HUB_ORACLE_ASSET_NAME)]: 1n },
  }),
  address: credentialToAddress("Preview", { type: "Script", hash: h28(0x16) }),
});

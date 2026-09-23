/**
 * `fabricated-deposit` family (Goal task `Q39`) — evidence builder, L1 witness
 * authentication and submit-side re-derivation.
 *
 * The family is reached by **direct module import**: the `fabricatedDeposit`
 * catalogue category and its CLI wiring are parent-owned integration surfaces
 * that land with catalogue registration (#617), so nothing here goes through
 * `src/index.js`, `fraud-proof/catalogue.ts` or `bin.ts`.
 *
 * Every committed-leaf, commitment, nonce and handoff constant below is the
 * value **measured out of the Aiken family modules**
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-0{1,2,3,4}.ak`
 * and pinned in `demo/midgard-sdk/tests/fabricated-deposit.test.ts`. The
 * committed `deposits_root`s and the step-04 handoff bytes asserted here are
 * therefore Aiken-measured absolutes, not one TypeScript derivation compared
 * against another.
 *
 * The two challenged blocks are the Aiken fixtures' own scenarios:
 *
 * - **FI** (`fabricated_identity_block_v1`) commits `(FABRICATED_DEPOSIT_ID ->
 *   AUTHENTIC_DEPOSIT_INFO)`, an identity no deposit event ever had; and
 * - **MM** (`mismatched_content_block_v1`) commits `(AUTHENTIC_DEPOSIT_ID ->
 *   DIVERTED_DEPOSIT_INFO)`, the authentic identity with diverted content.
 *
 * Both are re-committed into a real `DaPayload` here, because
 * `tests/helpers/canonical-block-evidence-fixture.ts` hard-wires an empty
 * deposit source set.
 */
import { createHash } from "node:crypto";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

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
  classifyFabricatedDepositFault,
  fabricatedDepositBlockEvidenceFromVerifiedPayload,
  type FabricatedDepositL1Witness,
  FabricatedDepositRejection,
  prepareFabricatedDepositFromCommittedLeaves,
} from "../src/prepare-fabricated-deposit.js";
import {
  deriveFabricatedDepositStep01Handoff,
  parseSubmitFabricatedDepositInclusion,
} from "../src/submit-fabricated-deposit-step-01.js";
import { deriveFabricatedDepositStep03Handoff } from "../src/submit-fabricated-deposit-step-03.js";
import { assertFabricatedDepositStep04Finalizable } from "../src/submit-fabricated-deposit-step-04.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  createFabricatedDepositEvidenceAuthority,
  FABRICATED_DEPOSIT_ARTIFACT,
  type FabricatedDepositArtifact,
  requireFabricatedDepositArtifact,
} from "../src/workflow/fabricated-deposit-evidence.js";
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
// `step-01.ak`'s `authentic_deposit_id_v1` / `fabricated_deposit_id_v1` /
// `authentic_deposit_info_v1` / `diverted_deposit_info_v1`, and `step-02.ak`'s
// `authentic_deposit_datum_v1` / `authentic_inclusion_time_v1`.

const AUTHENTIC_DEPOSIT_ID: SDK.OutputReference = {
  transactionId: "7a".repeat(32),
  outputIndex: 3n,
};

const FABRICATED_DEPOSIT_ID: SDK.OutputReference = {
  transactionId: "5c".repeat(32),
  outputIndex: 0n,
};

const KEY_AUTHENTIC_DEPOSIT_ID =
  "d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff";
const KEY_FABRICATED_DEPOSIT_ID =
  "d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ff";

const VALUE_AUTHENTIC_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ff";
const VALUE_DIVERTED_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";

const HASH_AUTHENTIC_DEPOSIT_INFO =
  "89ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863";
const HASH_DIVERTED_DEPOSIT_INFO =
  "0ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335";

/** `user_events.out_ref_to_nonce(authentic_deposit_id_v1)`. */
const NONCE_AUTHENTIC_DEPOSIT_ID =
  "db496846395df718772b56f398cc7c7882869ddc0154fd035d63da1c3e95dd06";

const DATUM_AUTHENTIC_DEPOSIT_EVENT =
  "d8799fd8799fd8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ffff0f581c57575757575757575757575757575757575757575757575757575757ff";

const FI_DEPOSITS_PHAS_ROOT =
  "b0374d9482ece991566bebfa200b6577eaeed4a2bcc56e25eb28e8d4f06655b4";
const FI_DEPOSITS_ROOT =
  "60b531d1961d33baf3b6e83da728b0fc1497faf43f78e2cdaf9e03aae9959890";
const MM_DEPOSITS_PHAS_ROOT =
  "4b0c3a7234e798d045b06088ab4933c71e22d74781c9457f022987bf8e416c22";
const MM_DEPOSITS_ROOT =
  "880ba7ceb072fce058c5e8f9adbbe9b5bcc3efdcb53ec82039f142f577c47ab4";

/** The Aiken fixtures' header window, inherited from `native_binding_fixture_v1`. */
const HEADER_START_TIME = 10n;
const HEADER_END_TIME = 20n;
const AUTHENTIC_INCLUSION_TIME = 15n;

const FI_HEADER_HASH =
  "6a404d9de58a96111da77453168c29f4d23007592856b93e06b6bb46";
const MM_HEADER_HASH =
  "b50943cc7ac3d1b46b37e1b33223419dcb3d4dbd03564f4966918ec6";

/** `step_04.State` of each Aiken scenario, byte for byte. */
const FI_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581c6a404d9de58a96111da77453168c29f4d23007592856b93e06b6bb460a14d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ffd87980ff";
const MM_STEP_04_STATE_CBOR =
  "d8799f581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb581cb50943cc7ac3d1b46b37e1b33223419dcb3d4dbd03564f4966918ec60a14d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd87a9f58200ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335582089ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c68630fffff";

const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

const DEPOSIT_POLICY_ID = h28(0x18);

// ## Challenged-block fixtures

type DepositLeafEntry = { readonly key: string; readonly value: string };

const FI_LEAF: DepositLeafEntry = {
  key: KEY_FABRICATED_DEPOSIT_ID,
  value: VALUE_AUTHENTIC_DEPOSIT_INFO,
};
const MM_LEAF: DepositLeafEntry = {
  key: KEY_AUTHENTIC_DEPOSIT_ID,
  value: VALUE_DIVERTED_DEPOSIT_INFO,
};

type DepositsBlockFixture = {
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
const buildDepositsBlockFixture = async ({
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
const historyWitness = (anchor: UTxO): FabricatedDepositL1Witness => ({
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
    assets: { [DEPOSIT_POLICY_ID]: 1n },
  }),
  address: credentialToAddress("Preview", {
    type: "Script",
    hash: DEPOSIT_POLICY_ID,
  }),
});
const absentIdentityWitness = (
  authenticated = true,
): FabricatedDepositL1Witness => {
  const anchor = rootHistoryUtxo();
  return historyWitness(
    authenticated ? anchor : { ...anchor, assets: { lovelace: 5_000_000n } },
  );
};

const presentEventWitness = ({
  observedEventAssetName = NONCE_AUTHENTIC_DEPOSIT_ID,
  eventDatumCbor = DATUM_AUTHENTIC_DEPOSIT_EVENT,
}: {
  readonly observedEventAssetName?: string;
  readonly eventDatumCbor?: string;
} = {}): FabricatedDepositL1Witness =>
  historyWitness(
    depositEventUtxoFixture({
      assetName: observedEventAssetName,
      datum: Data.from(eventDatumCbor, SDK.DepositDatum),
    }),
  );

/** A canonical `DepositDatum` with an arbitrary identity, content and window. */
const depositEventDatum = ({
  id = AUTHENTIC_DEPOSIT_ID,
  paymentKeyByte = 0x1c,
  inclusionTime = AUTHENTIC_INCLUSION_TIME,
}: {
  readonly id?: SDK.OutputReference;
  readonly paymentKeyByte?: number;
  readonly inclusionTime?: bigint;
} = {}): SDK.DepositDatum => ({
  event: {
    id,
    info: {
      l2_address: {
        paymentCredential: {
          PublicKeyCredential: [h28(paymentKeyByte)] as [string],
        },
        stakeCredential: null,
      },
      l2_network_id: 0n,
      l2_datum: null,
    },
  },
  inclusion_time: inclusionTime,
  witness: h28(0x57),
});

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

const hubOracleUtxoFixture = (depositScriptHash = DEPOSIT_POLICY_ID): UTxO => ({
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

const depositEventUtxoFixture = ({
  policyId = DEPOSIT_POLICY_ID,
  assetName = NONCE_AUTHENTIC_DEPOSIT_ID,
  datum = depositEventDatum(),
}: {
  readonly policyId?: string;
  readonly assetName?: string;
  readonly datum?: SDK.DepositDatum;
} = {}): UTxO => ({
  ...syntheticUtxo({
    txIdByte: 0xa2,
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [NONCE_AUTHENTIC_DEPOSIT_ID] },
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
    hash: DEPOSIT_POLICY_ID,
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
const historyPayload = (datum: SDK.DepositDatum): SDK.EventHistoryPayload => ({
  DepositPayload: { event: datum.event },
});
const historyOpening = (datum = depositEventDatum(), assets = historyAssets) =>
  Data.to(
    {
      RetainedEventData: {
        payload: historyPayload(datum),
        original_assets: assets,
      },
    },
    SDK.FabricatedDepositAuthenticContentOpening,
  );
const historyCommitment = SDK.eventHistoryCommitment(
  "30".repeat(28),
  "Deposit",
  {
    event_id: AUTHENTIC_DEPOSIT_ID,
    inclusion_time: AUTHENTIC_INCLUSION_TIME,
  },
  historyPayload(depositEventDatum()),
  historyAssets,
);

const fiStep03State: SDK.FabricatedDepositStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_deposit_id: FABRICATED_DEPOSIT_ID,
  committed_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
  verdict: "DepositIdentityAbsent",
};

const mmStep03State: SDK.FabricatedDepositStep03State = {
  state_queue_policy: "bb".repeat(28),
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_deposit_id: AUTHENTIC_DEPOSIT_ID,
  committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
  verdict: {
    DepositEventObserved: {
      commitment: historyCommitment,
    },
  },
};

describe("Q39 fabricated-deposit evidence admission", () => {
  it("admits a deposits-bearing block and extracts its committed leaves", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
      observation: fixture.observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.grade).toBe("security");
    expect(evidence.provenance.l1.trustClass).toBe("authenticated_cardano_l1");
    expect(evidence.provenance.da.trustClass).toBe(
      "public_or_permissionless_da",
    );
    expect(evidence.headerHash).toBe(fixture.headerHash);
    // The counted deposits_root of the MM scenario, measured in Aiken.
    expect(evidence.committedDepositsRoot).toBe(MM_DEPOSITS_ROOT);
    expect(evidence.depositCount).toBe(1n);
    expect(evidence.headerStartTime).toBe(HEADER_START_TIME);
    expect(evidence.headerEndTime).toBe(HEADER_END_TIME);
    expect(evidence.entries).toEqual([
      [KEY_AUTHENTIC_DEPOSIT_ID, VALUE_DIVERTED_DEPOSIT_INFO],
    ]);
  });

  it("refuses operator-private DA provenance", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      fabricatedDepositBlockEvidenceFromVerifiedPayload({
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
    const observed = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const other = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    expect(other.headerHash).not.toBe(observed.headerHash);
    await expect(
      fabricatedDepositBlockEvidenceFromVerifiedPayload({
        observation: observed.observation,
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
  });
});

describe("Q39 fabricated-deposit proof plan", () => {
  it("builds a nonexistent-identity plan from an authenticated absence witness", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    // The Aiken-measured roots of `fabricated_identity_block_v1`.
    expect(fixture.depositsPhasRoot).toBe(FI_DEPOSITS_PHAS_ROOT);
    expect(fixture.depositsRoot).toBe(FI_DEPOSITS_ROOT);

    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
      observation: fixture.observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    const outputDir = await mkdtemp(join(tmpdir(), "q39-fabricated-deposit-"));
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedDepositsRoot: evidence.committedDepositsRoot,
      depositCount: evidence.depositCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: absentIdentityWitness(),
      outputDir,
    });

    expect(plan.violationId).toBe("fabricated-deposit");
    expect(plan.fraudCategoryId).toBe("0000000b");
    expect(plan.threadTokenAssetName).toBe(`0000000b${fixture.headerHash}`);
    expect(plan.depositsPhasRoot).toBe(FI_DEPOSITS_PHAS_ROOT);
    expect(plan.committedDepositsRoot).toBe(FI_DEPOSITS_ROOT);
    expect(plan.classification.verdict).toBe("DepositIdentityAbsent");
    expect(plan.classification.fault).toBe("NonexistentDepositIdentity");
    expect(plan.step02State).toEqual({
      stateQueuePolicyId: h28(0x15),
      challengedHeaderHash: fixture.headerHash,
      headerStartTime: "10",
      headerEndTime: "20",
      committedDepositIdCbor: KEY_FABRICATED_DEPOSIT_ID,
      committedDepositInfoHash: HASH_AUTHENTIC_DEPOSIT_INFO,
    });
    // An absence proof has no retained content to open at step 03.
    expect(plan.authenticContent.openingCbor).toBeNull();
    expect(
      plan.depositInclusion.depositMembershipProofCbor.length,
    ).toBeGreaterThan(0);
    expect(plan.files).toBeDefined();
    expect(
      JSON.parse(await readFile(plan.files!.depositInclusionPath, "utf8")),
    ).toEqual(plan.depositInclusion);
  });

  it("builds a content-mismatch plan from an authenticated present-event witness", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    // The Aiken-measured roots of `mismatched_content_block_v1`.
    expect(fixture.depositsPhasRoot).toBe(MM_DEPOSITS_PHAS_ROOT);
    expect(fixture.depositsRoot).toBe(MM_DEPOSITS_ROOT);

    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
      committedDepositIdCbor: KEY_AUTHENTIC_DEPOSIT_ID,
    });

    expect(plan.classification.verdict).toEqual({
      DepositEventObserved: {
        commitment: { ...historyCommitment, policy: DEPOSIT_POLICY_ID },
      },
    });
    expect(plan.classification.fault).toEqual({
      MismatchedDepositContent: {
        committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
        authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(plan.challengedLeaf.committedDepositInfoHash).toBe(
      HASH_DIVERTED_DEPOSIT_INFO,
    );
    expect(plan.authenticContent.openingCbor).toBe(historyOpening());
  });

  it("refuses leaves that do not open the committed counted deposits_root, in the root or in the cardinality", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    // Root arm: the supplied leaf is not the one the header committed.
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedDepositsRoot: fixture.depositsRoot,
        depositCount: fixture.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [[KEY_FABRICATED_DEPOSIT_ID, VALUE_AUTHENTIC_DEPOSIT_INFO]],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "deposits_root_mismatch" });

    // Cardinality arm: the header's own `deposit_count` disagrees with the
    // rebuilt leaf count, which is the half of the counted-root check a
    // root-only comparison would miss.
    const lied = await buildDepositsBlockFixture({
      leaves: [MM_LEAF],
      depositCountOverride: 7n,
    });
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: lied.headerHash,
        committedDepositsRoot: MM_DEPOSITS_ROOT,
        depositCount: lied.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: lied.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({ code: "deposits_root_mismatch" });
  });

  it("refuses an empty deposit source set and a pinned leaf the header never committed", async () => {
    const empty = await buildDepositsBlockFixture({ leaves: [] });
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: empty.headerHash,
        committedDepositsRoot: empty.depositsRoot,
        depositCount: empty.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "no_committed_deposit_leaf" });

    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedDepositsRoot: fixture.depositsRoot,
        depositCount: fixture.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: absentIdentityWitness(),
        committedDepositIdCbor: KEY_FABRICATED_DEPOSIT_ID,
      }),
    ).rejects.toMatchObject({ code: "leaf_not_committed" });
  });
});

describe("Q39 fabricated-deposit L1 witness authentication", () => {
  const fiLeaf = async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: absentIdentityWitness(),
    });
    return plan.challengedLeaf;
  };

  const mmLeaf = async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
    });
    return plan.challengedLeaf;
  };

  it("refuses an absence claim without an authenticated history token, and any witness that is not authenticated L1 security-grade evidence", async () => {
    const leaf = await fiLeaf();
    // A gap-shaped datum without its list NFT cannot authenticate absence.
    await expect(
      classifyFabricatedDepositFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: absentIdentityWitness(false),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
    await expect(
      classifyFabricatedDepositFault({
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
    const leaf = await mmLeaf();
    // The observed asset name is not `out_ref_to_nonce(committed_deposit_id)`.
    await expect(
      classifyFabricatedDepositFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({ observedEventAssetName: h32(0x4d) }),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
    // The retained datum names a different deposit identity.
    await expect(
      classifyFabricatedDepositFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({
          eventDatumCbor: Data.to(
            depositEventDatum({ id: FABRICATED_DEPOSIT_ID }),
            SDK.DepositDatum,
          ),
        }),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
  });

  it("refuses to challenge a header that committed exactly the authentic content", async () => {
    const fixture = await buildDepositsBlockFixture({
      leaves: [
        { key: KEY_AUTHENTIC_DEPOSIT_ID, value: VALUE_AUTHENTIC_DEPOSIT_INFO },
      ],
    });
    const attempt = async () =>
      await prepareFabricatedDepositFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedDepositsRoot: fixture.depositsRoot,
        depositCount: fixture.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      });
    await expect(attempt()).rejects.toBeInstanceOf(FabricatedDepositRejection);
    await expect(attempt()).rejects.toMatchObject({
      code: "authentic_content_matches_commitment",
    });
  });

  it("proves an authentic event ineligible for the challenged block, on either side of the window", async () => {
    const leaf = await mmLeaf();
    for (const inclusionTime of [HEADER_START_TIME, HEADER_END_TIME + 1n]) {
      const datum = depositEventDatum({ inclusionTime });
      await expect(
        classifyFabricatedDepositFault({
          leaf,
          headerStartTime: HEADER_START_TIME,
          headerEndTime: HEADER_END_TIME,
          witness: presentEventWitness({
            eventDatumCbor: Data.to(datum, SDK.DepositDatum),
          }),
        }),
      ).resolves.toMatchObject({
        fault: {
          IneligibleDepositEvent: { event_inclusion_time: inclusionTime },
        },
      });
    }
  });
});

describe("Q39 fabricated-deposit production evidence authority", () => {
  const artifactDigestForTest = (
    value: Omit<FabricatedDepositArtifact, "artifactDigest">,
  ): string =>
    createHash("sha256")
      .update(FABRICATED_DEPOSIT_ARTIFACT)
      .update("\0")
      .update(value.headerHash)
      .update("\0")
      .update(value.owner)
      .update("\0")
      .update(value.depositIndex.toString())
      .update("\0")
      .update(JSON.stringify(value.depositInclusion))
      .update("\0")
      .update(JSON.stringify(value.authenticContent))
      .update("\0")
      .update(JSON.stringify(value.l1Evidence))
      .digest("hex");

  const canonicalEvidence = async (
    fixture: DepositsBlockFixture,
  ): Promise<CanonicalBlockEvidence> => {
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
      observation: fixture.observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
      minimumConfirmationDepth: 1,
    });
    const deposits = evidence.entries.map(([keyCbor, valueCbor]) => ({
      key: Data.from(keyCbor, SDK.OutputReference),
      value: Data.from(valueCbor, SDK.DepositInfo),
      keyBytes: Buffer.from(keyCbor, "hex"),
      valueBytes: Buffer.from(valueCbor, "hex"),
    }));
    return {
      ...evidence,
      observation: fixture.observation,
      header: fixture.header,
      reconstruction: { deposits },
    } as unknown as CanonicalBlockEvidence;
  };

  it("roundtrips an absence fault through journal normalization and rejects artifact tampering", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    const authority = createFabricatedDepositEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([rootHistoryUtxo()]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    const evidence = await canonicalEvidence(fixture);
    const detections = await authority.detect(evidence, h28(0x44));
    expect(detections).toHaveLength(1);
    expect(detections[0]!.detection.violationId).toBe("fabricated-deposit");
    expect(detections[0]!.artifact.l1Evidence).toEqual({
      kind: "absent_identity",
      historyOutRef: `${h32(0xa3)}#0`,
      retainedDataOutRef: null,
    });
    await expect(
      authority.readmit(
        JSON.parse(
          JSON.stringify(normalizeJournalJson(detections[0]!.artifact)),
        ),
      ),
    ).resolves.toEqual(detections[0]!.artifact);
    await expect(
      authority.readmit(
        normalizeJournalJson({
          ...detections[0]!.artifact,
          depositInclusion: {
            ...detections[0]!.artifact.depositInclusion,
            depositsPhasRoot: h32(0x77),
          },
        }),
      ),
    ).rejects.toThrow(/digest mismatch/u);
    await expect(
      authority.readmit({
        ...detections[0]!.artifact,
        depositIndex: 1,
      }),
    ).rejects.toThrow(/digest mismatch/u);
    const { artifactDigest: _digest, ...body } = detections[0]!.artifact;
    const substitutedBody = {
      ...body,
      authenticContent: { openingCbor: historyOpening() },
      l1Evidence: {
        kind: "present_event" as const,
        historyOutRef: `${h32(0x77)}#0`,
        retainedDataOutRef: null,
      },
    };
    await expect(
      authority.readmit({
        ...substitutedBody,
        artifactDigest: artifactDigestForTest(substitutedBody),
      }),
    ).rejects.toThrow(/History facts changed before capture/u);
    await expect(
      authority.readmit({ ...detections[0]!.artifact, extra: true }),
    ).rejects.toThrow(/unknown, missing, or non-string/u);
    await expect(
      authority.readmit(
        Object.assign(Object.create(null), detections[0]!.artifact),
      ),
    ).rejects.toThrow(/unknown, missing, or non-string/u);
    expect(() =>
      requireFabricatedDepositArtifact(
        { ...detections[0]!.artifact },
        h28(0x44),
        fixture.headerHash,
      ),
    ).toThrow(/not re-authenticated/u);
  });

  it.each([false, true])(
    "uses the governed deposit spending address for ordinary lookup and readmission (stake credential: %s)",
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
        deposit_addr: await Effect.runPromise(
          SDK.addressDataFromBech32(eventAddress),
        ),
      };
      const hub = {
        ...hubOracleUtxoFixture(),
        address: hubAddress,
        datum: Data.to(hubDatum, SDK.HubOracleDatum),
        assets: { lovelace: 5_000_000n, [hubUnit]: 1n },
      };
      const event = { ...depositEventUtxoFixture(), address: eventAddress };
      const eventUnit = toUnit(hubDatum.deposit, NONCE_AUTHENTIC_DEPOSIT_ID);
      const mintPolicyAddress = credentialToAddress("Preview", {
        type: "Script",
        hash: hubDatum.deposit,
      });
      expect(eventAddress).not.toBe(mintPolicyAddress);
      expect(event.assets[eventUnit]).toBe(1n);
      let liveEventAddress = eventAddress;
      const queries: string[] = [];
      const authority = createFabricatedDepositEvidenceAuthority({
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
      const ordinary = await buildDepositsBlockFixture({
        leaves: [
          {
            key: KEY_AUTHENTIC_DEPOSIT_ID,
            value: VALUE_AUTHENTIC_DEPOSIT_INFO,
          },
        ],
      });
      const ordinaryEvidence = await canonicalEvidence(ordinary);
      await expect(
        authority.detect(ordinaryEvidence, h28(0x44)),
      ).resolves.toEqual([]);
      // Reuse the existing mismatch case solely to exercise persisted-event lookup.
      const existing = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
      const detections = await authority.detect(
        await canonicalEvidence(existing),
        h28(0x44),
      );
      expect(detections).toHaveLength(1);
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
    },
  );

  it("returns no detection for an authentic due event whose content matches the block", async () => {
    const fixture = await buildDepositsBlockFixture({
      leaves: [
        { key: KEY_AUTHENTIC_DEPOSIT_ID, value: VALUE_AUTHENTIC_DEPOSIT_INFO },
      ],
    });
    const authority = createFabricatedDepositEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([depositEventUtxoFixture()]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    expect(
      await authority.detect(await canonicalEvidence(fixture), h28(0x44)),
    ).toEqual([]);
  });

  it("requires an authenticated gap for arbitrary and historically consumed identities", async () => {
    const authority = createFabricatedDepositEvidenceAuthority({
      history: historyEnvironment,
      lucid: historyLucid([]),
      network: "Preview",
      hubOraclePolicyId: h28(0x16),
      minimumConfirmationDepth: 1,
    });
    for (const leaves of [
      [FI_LEAF],
      [
        {
          key: KEY_AUTHENTIC_DEPOSIT_ID,
          value: VALUE_AUTHENTIC_DEPOSIT_INFO,
        },
      ],
    ]) {
      const fixture = await buildDepositsBlockFixture({ leaves });
      const authenticated = createFabricatedDepositEvidenceAuthority({
        history: historyEnvironment,
        lucid: historyLucid([rootHistoryUtxo()]),
        network: "Preview",
        hubOraclePolicyId: h28(0x16),
        minimumConfirmationDepth: 1,
      });
      expect(
        await authenticated.detect(await canonicalEvidence(fixture), h28(0x44)),
      ).toHaveLength(1);
      await expect(
        authority.detect(await canonicalEvidence(fixture), h28(0x44)),
      ).rejects.toThrow(/no unique authenticated witness/u);
    }
  });
});

it("authenticates an equal-key filler as absence without counting its funds or using nonce liveness", async () => {
  const anchor = depositEventUtxoFixture();
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
    "Deposit",
    AUTHENTIC_DEPOSIT_ID,
  );
  expect(result.witness.kind).toBe("Absent");
  expect(result.captured).toBeUndefined();
  expect(result.witness.anchor.utxo.assets.lovelace).toBe(5_000_000n);
});

describe("Q39 fabricated-deposit submit-side re-derivation", () => {
  it("re-derives the step-01 handoff from the on-chain header and refuses a PHAS root that does not open it", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
    });
    const inclusion = parseSubmitFabricatedDepositInclusion(
      plan.depositInclusion,
    );
    const handoff = await deriveFabricatedDepositStep01Handoff({
      stateQueuePolicyId: h28(0x15),
      header: fixture.header,
      headerHash: fixture.headerHash,
      inclusion,
    });
    expect(handoff.committedDeposit.domain).toBe(SDK.ROOT_DOMAINS.deposits);
    expect(handoff.committedDeposit.root).toBe(MM_DEPOSITS_ROOT);
    expect(handoff.committedDeposit.phas_root).toBe(MM_DEPOSITS_PHAS_ROOT);
    expect(handoff.committedDeposit.count).toBe(1n);
    expect(handoff.step02State).toEqual({
      state_queue_policy: h28(0x15),
      challenged_header_hash: fixture.headerHash,
      header_start_time: HEADER_START_TIME,
      header_end_time: HEADER_END_TIME,
      committed_deposit_id: AUTHENTIC_DEPOSIT_ID,
      committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
    });

    await expect(
      deriveFabricatedDepositStep01Handoff({
        stateQueuePolicyId: h28(0x15),
        header: fixture.header,
        headerHash: fixture.headerHash,
        inclusion: { ...inclusion, depositsPhasRoot: FI_DEPOSITS_PHAS_ROOT },
      }),
    ).rejects.toThrow(/does not open the committed deposits_root/u);
  });

  it("authenticates the deposit event UTxO through the hub oracle's deposit policy", async () => {
    const authenticate = (anchor: UTxO) =>
      authenticateFabricatedHistoryWitness(
        historyWitness(anchor),
        "Deposit",
        AUTHENTIC_DEPOSIT_ID,
      );
    const authenticated = await authenticate(depositEventUtxoFixture());
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...historyWitness(depositEventUtxoFixture()),
          hubOracleUtxo: {
            ...hubOracleUtxoFixture(),
            assets: { lovelace: 5_000_000n },
          },
        },
        "Deposit",
        AUTHENTIC_DEPOSIT_ID,
      ),
    ).rejects.toThrow("authentic inline hub oracle");
    await expect(
      authenticate({
        ...depositEventUtxoFixture(),
        address: credentialToAddress("Preview", {
          type: "Key",
          hash: h28(0x44),
        }),
      }),
    ).rejects.toThrow("Invalid authenticated history output shape");

    expect(authenticated.deployment.policyId).toBe(DEPOSIT_POLICY_ID);
    expect(authenticated.witness.anchor.key).toBe(NONCE_AUTHENTIC_DEPOSIT_ID);
    expect(authenticated.captured?.commitment).toEqual({
      ...historyCommitment,
      policy: DEPOSIT_POLICY_ID,
    });
    expect(authenticated.captured?.originalAssets).toEqual(historyAssets);

    // A foreign policy is refused even though the asset name is the authentic
    // nonce: the policy comes from the hub oracle, not from the prover.
    await expect(
      authenticate(depositEventUtxoFixture({ policyId: h28(0x99) })),
    ).rejects.toThrow(/no unique authenticated witness/u);
    // The authentic policy and nonce, but a datum for another identity.
    await expect(
      authenticate(
        depositEventUtxoFixture({
          datum: depositEventDatum({ id: FABRICATED_DEPOSIT_ID }),
        }),
      ),
    ).rejects.toThrow(/identity differs/u);
  });

  it("opens step-02's retained commitment into the Aiken scenarios' exact step-04 handoffs", async () => {
    const absent = await deriveFabricatedDepositStep03Handoff({
      state: fiStep03State,
    });
    expect(absent.opening).toBe("NoAuthenticContent");
    expect(absent.fault).toBe("NonexistentDepositIdentity");
    expect(Data.to(absent.step04State, SDK.FabricatedDepositStep04State)).toBe(
      FI_STEP_04_STATE_CBOR,
    );

    const present = await deriveFabricatedDepositStep03Handoff({
      state: mmStep03State,
      openingCbor: historyOpening(),
    });
    expect(present.fault).toEqual({
      MismatchedDepositContent: {
        committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
        authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(Data.to(present.step04State, SDK.FabricatedDepositStep04State)).toBe(
      MM_STEP_04_STATE_CBOR,
    );
  });

  it("refuses step-03 openings that do not pair with the verdict or are not the authenticated bytes", async () => {
    // A present-event verdict opened as an absence would convert a content
    // dispute into the strictly stronger non-existence conviction.
    await expect(
      deriveFabricatedDepositStep03Handoff({ state: mmStep03State }),
    ).rejects.toThrow(/requires its retained payload and original Value/u);
    await expect(
      deriveFabricatedDepositStep03Handoff({
        state: fiStep03State,
        openingCbor: historyOpening(),
      }),
    ).rejects.toThrow(/absence admits no retained event opening/u);
    // Only the hash equality makes supplied bytes authentic.
    await expect(
      deriveFabricatedDepositStep03Handoff({
        state: mmStep03State,
        openingCbor: historyOpening(
          depositEventDatum({ paymentKeyByte: 0x3e }),
        ),
      }),
    ).rejects.toThrow(/does not match the authenticated history commitment/u);
  });

  it("refuses to finalize a misfiled conviction or an unestablished fault", async () => {
    const established: SDK.FabricatedDepositStep04State =
      SDK.fabricatedDepositStep04State(mmStep03State, {
        MismatchedDepositContent: {
          committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
          authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
          event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
        },
      });
    expect(() =>
      assertFabricatedDepositStep04Finalizable({
        state: established,
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).not.toThrow();
    // Filed against a header the thread token does not name.
    expect(() =>
      assertFabricatedDepositStep04Finalizable({
        state: established,
        fraudulentHeaderHash: FI_HEADER_HASH,
      }),
    ).toThrow(/thread state names challenged header/u);
    // An authentic event outside the challenged block's window is not this
    // block's fault, so it can never become a permanent conviction.
    expect(() =>
      assertFabricatedDepositStep04Finalizable({
        state: {
          ...established,
          fault: {
            MismatchedDepositContent: {
              committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
              authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
              event_inclusion_time: HEADER_END_TIME + 1n,
            },
          },
        },
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).toThrow(/not an established fabricated-deposit fault/u);
  });
});

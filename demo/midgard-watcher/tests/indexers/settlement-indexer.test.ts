import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { isDeepStrictEqual } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  DepositSpendRedeemer,
  MerkleRootSchema,
  PayoutDatum,
  PayoutMintRedeemer,
  PayoutSpendRedeemer,
  ProofSchema,
  ReserveSpendRedeemer,
  SettlementDatum,
  SettlementMintRedeemer,
  SettlementSpendRedeemer,
  StateQueueRedeemer,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  WithdrawalOrderDatum,
  WithdrawalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  encodeWatcherSettlementIndexerResult,
  encodeWatcherSettlementIndexerState,
  evaluateWatcherSettlementIndexer as evaluateWatcherSettlementIndexerRaw,
  makeWatcherSettlementIndexerPolicy,
  makeWatcherSettlementObservation,
  makeWatcherSettlementResourceFromProtocolUtxo,
  makeWatcherSettlementSnapshot,
  makeWatcherSettlementSubject,
  parseWatcherSettlementIndexerResult as parseWatcherSettlementIndexerResultRaw,
  parseWatcherSettlementIndexerState as parseWatcherSettlementIndexerStateRaw,
  WATCHER_SETTLEMENT_INDEXER_BOUNDS,
  WATCHER_SETTLEMENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
  type WatcherSettlementIndexerPolicy,
  type WatcherSettlementIndexerState,
  type WatcherSettlementObservation,
  type WatcherSettlementPublicContext,
  type WatcherSettlementSnapshot,
  type WatcherSettlementSubject,
  type WatcherSettlementTransition,
  type WatcherSettlementTransitionKind,
} from "../../src/indexers/settlement-indexer.js";
import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContext,
  encodeWatcherNormalizedL1Block,
  establishWatcherExternalProviderTransport,
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block as normalizeWatcherL1BlockRaw,
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherAuthenticatedL1Provider,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency as evaluateWatcherMultiProviderConsistencyRaw } from "../../src/l1/multi-provider-consistency.js";
import {
  evaluateWatcherPostFinalityRecovery as evaluateWatcherPostFinalityRecoveryRaw,
  evaluateWatcherRollback as evaluateWatcherRollbackRaw,
  makeWatcherRollbackBootstrapState,
  type WatcherPostFinalityRecoveryInput,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  encodeWatcherDurableStore,
  journalWatcherProtocolUtxoTransition,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  watcherCanonicalJson,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  type WatcherProtocolUtxo,
  watcherSha256CanonicalJson,
} from "../../src/storage/durable-store.js";
import {
  h28,
  h32,
  makeDeploymentAuthority,
  WATCHER_AUTHORITY_RELEASE_DIGEST as RELEASE_DIGEST,
  WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT as RULE_BUNDLE_COMMITMENT,
} from "../support/deployment-authority-fixture.js";
import { makeWatcherTlsTransportFixture } from "../support/tls-transport-fixture.js";

type Mutable = Record<string, any>;
const transportContexts: WatcherL1TransportAttestationContext[] = [];
const tlsServers: Server[] = [];
const tlsIdentityByProviderId = new Map<string, string>();
const transportEndpointByProviderId = new Map<string, string>();
let transportFixtureDirectory = "";

const makeTlsTransportFixture = async (providerId: string) =>
  await makeWatcherTlsTransportFixture(
    transportFixtureDirectory,
    tlsServers,
    providerId,
  );

const transportFor = (
  authenticatedProvider: unknown,
): WatcherL1TransportAttestationContext => {
  const matching = transportContexts.filter((context) => {
    const details = watcherL1TransportAttestationDetails(context);
    return (
      details !== null &&
      isDeepStrictEqual(details.provider, authenticatedProvider)
    );
  });
  if (matching.length !== 1) {
    throw new Error("test provider has no unique live transport attestation");
  }
  return matching[0]!;
};

const normalizeWatcherL1Block = (
  authenticatedProvider: unknown,
  observation: unknown,
  session?: Parameters<typeof normalizeWatcherL1BlockRaw>[2],
) =>
  normalizeWatcherL1BlockRaw(
    transportFor(authenticatedProvider),
    observation,
    session,
  );

const evaluateWatcherMultiProviderConsistency = (
  configuredSource: unknown,
  observations: unknown,
) =>
  evaluateWatcherMultiProviderConsistencyRaw(
    configuredSource,
    observations,
    transportContexts,
  );

const parseWatcherSettlementIndexerState = (
  value: unknown,
  policyValue: unknown,
  restartContexts: readonly WatcherSettlementPublicContext["restartContexts"][number][] = [],
  restartRollbackContexts: Parameters<
    typeof parseWatcherSettlementIndexerStateRaw
  >[4] = [],
) =>
  parseWatcherSettlementIndexerStateRaw(
    value,
    policyValue,
    transportContexts,
    restartContexts,
    restartRollbackContexts,
  );

const evaluateWatcherSettlementIndexer = (
  policyValue: unknown,
  previousStateValue: unknown,
  observationValue: unknown,
  publicContextValue: unknown,
) =>
  evaluateWatcherSettlementIndexerRaw(
    policyValue,
    previousStateValue,
    observationValue,
    publicContextValue,
    transportContexts,
  );

const evaluateWatcherRollback = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointAuthorityInput?: unknown,
) =>
  evaluateWatcherRollbackRaw(
    policyInput,
    storeInput,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
    previousRollbackStateInput,
    rollbackBootstrapStateInput,
    trustedCheckpointAuthorityInput,
    transportContexts,
  );

const evaluateWatcherPostFinalityRecovery = (
  input: WatcherPostFinalityRecoveryInput,
) =>
  evaluateWatcherPostFinalityRecoveryRaw({
    ...input,
    transportAttestations: transportContexts,
  });

const parseWatcherSettlementIndexerResult = (
  value: unknown,
  context: Omit<
    Parameters<typeof parseWatcherSettlementIndexerResultRaw>[1],
    "transportAttestations"
  >,
) =>
  parseWatcherSettlementIndexerResultRaw(value, {
    ...context,
    transportAttestations: transportContexts,
  });

const ROOT = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const settlementAsset = "aa";
const operator = h28("91");
const HUB_REFERENCE_OUT_REF = `${h32("7a")}#0`;
const SETTLEMENT_REFERENCE_OUT_REF = `${h32("7b")}#0`;

const deploymentAuthorityFixture = makeDeploymentAuthority();
const applied = deploymentAuthorityFixture.policy.appliedScriptHashes;
const deploymentAuthority = {
  signedIdentity: deploymentAuthorityFixture.signedIdentity,
  policy: deploymentAuthorityFixture.policy,
  trustRoots: deploymentAuthorityFixture.trustRoots,
  result: deploymentAuthorityFixture.result,
};

const enterpriseScriptAddress = (scriptHash: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.from(scriptHash, "hex")]),
  ).to_hex();

const enterprisePublicKeyAddress = (keyHash: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x60]), Buffer.from(keyHash, "hex")]),
  ).to_hex();

const hubOraclePolicyId = applied.hubOracleMint!;
const settlementPolicyId = applied.settlementMint!;
const depositPolicyId = applied.depositMint!;
const settlementSpendScriptHash = applied.settlementSpend!;
const reserveSpendScriptHash = applied.reserveSpend!;
const payoutPolicyId = applied.payoutMint!;
const payoutSpendScriptHash = applied.payoutSpend!;
const withdrawalPolicyId = applied.withdrawalMint!;
const withdrawalSpendScriptHash = applied.withdrawalSpend!;
const activeOperatorPolicyId = applied.activeOperatorsMint!;
const retiredOperatorPolicyId = applied.retiredOperatorsMint!;
const stateQueuePolicyId = applied.stateQueueMint!;

const hubReferenceAssets = CML.MultiAsset.new();
hubReferenceAssets.set(
  CML.ScriptHash.from_hex(hubOraclePolicyId),
  CML.AssetName.from_hex(""),
  1n,
);
const hubReferenceOutputHex = CML.TransactionOutput.new(
  CML.Address.from_hex(enterpriseScriptAddress(applied.hubOracleMint!)),
  CML.Value.new(2_000_000n, hubReferenceAssets),
  null,
  null,
).to_canonical_cbor_hex();
const hubReferenceUtxo: WatcherProtocolUtxo = {
  outRef: HUB_REFERENCE_OUT_REF,
  role: "hub_oracle",
  chainPointId: h32("00"),
  output: makeWatcherDurablePayload(hubReferenceOutputHex),
};

const bootstrapStore = makeWatcherDurableStore({
  deploymentMarker: deploymentAuthorityFixture.marker,
  revision: "0",
  records: {
    l1Observations: [],
    chainPoints: [
      {
        chainPointId: h32("00"),
        providerId: "provider-a",
        blockHash: h32("01"),
        slot: "1",
        blockNo: "1",
        depth: "10",
      },
    ],
    protocolUtxos: [hubReferenceUtxo],
    daProofInputs: [],
    reconstructedStates: [],
    decisions: [],
    faults: [],
    submissions: [],
    confirmations: [],
    retries: [],
    deadlines: [],
    correctionResults: [],
  },
});

const policy = makeWatcherSettlementIndexerPolicy({
  network: "Preprod",
  releaseEvidenceDigest: RELEASE_DIGEST,
  deploymentMarker: deploymentAuthorityFixture.marker,
  hubOraclePolicyId,
  depositPolicyId,
  settlementPolicyId,
  settlementSpendScriptHash,
  reserveSpendScriptHash,
  payoutPolicyId,
  payoutSpendScriptHash,
  withdrawalPolicyId,
  withdrawalSpendScriptHash,
  activeOperatorPolicyId,
  retiredOperatorPolicyId,
  stateQueuePolicyId,
  settlementAddressHex: enterpriseScriptAddress(settlementSpendScriptHash),
  reserveAddressHex: enterpriseScriptAddress(reserveSpendScriptHash),
  payoutAddressHex: enterpriseScriptAddress(payoutSpendScriptHash),
  withdrawalAddressHex: enterpriseScriptAddress(withdrawalSpendScriptHash),
  bootstrapStoreDigest: watcherDurableStoreBytesSha256(
    encodeWatcherDurableStore(bootstrapStore),
  ),
  deploymentTrustRootId: deploymentAuthorityFixture.result.trustRootId,
  requiredFinalityDepth: "2",
  maximumHistoryEntries: "32",
  maximumRollbackEntries: "16",
  maximumRetryAttempts: "2",
}) as WatcherSettlementIndexerPolicy;

const makeExternalFinalityPolicy = () =>
  makeWatcherFinalityPolicy(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "development",
      targetNetwork: "Preprod",
      l1: {
        source: {
          sourceMode: "external_providers",
          providers: [
            {
              identity: "provider-a",
              operatorIdentitySha256: h32("97"),
              endpoint:
                transportEndpointByProviderId.get("provider-a") ??
                "https://cardano-a.example",
            },
            {
              identity: "provider-b",
              operatorIdentitySha256: h32("98"),
              endpoint:
                transportEndpointByProviderId.get("provider-b") ??
                "https://cardano-b.example",
            },
          ],
        },
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
        finality: {
          depth: Number(policy.requiredFinalityDepth),
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: Number(policy.requiredFinalityDepth),
          },
        },
      },
      da: {
        peers: [
          {
            identity: "da-peer-a",
            multiaddr:
              "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
          },
        ],
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
      },
      storage: {
        driver: "sqlite",
        path: "/var/lib/midgard-watcher/watcher.sqlite",
        rollbackAuthorityKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
        },
      },
      proverWallet: {
        keySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_PROVER_KEY",
        },
      },
      deadlines: {
        daFetchMs: 60_000,
        daPublishMs: 60_000,
        proofConstructMs: 300_000,
        proofSubmitMs: 120_000,
      },
    },
    {
      manifestId: policy.deploymentMarker.manifestId,
      network: policy.network,
      trustRootId: policy.deploymentTrustRootId,
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
      programCommitments: { validation: h32("55") },
      durableMarker: policy.deploymentMarker,
    },
  )!;

let finalityPolicy: NonNullable<ReturnType<typeof makeWatcherFinalityPolicy>>;

const settlementDatum = (
  claim: { resolution_time: bigint; operator: string } | null = null,
): string =>
  Data.to(
    {
      deposits_root: ROOT,
      withdrawals_root: ROOT,
      forced_transactions_root: ROOT,
      transactions_root: ROOT,
      resolution_claim: claim,
    },
    SettlementDatum,
  );

const cardanoValue = (
  lovelace: bigint,
  assets: readonly {
    policyId: string;
    assetName: string;
    quantity: bigint;
  }[],
): CML.Value => {
  const multiAsset = CML.MultiAsset.new();
  for (const asset of assets) {
    multiAsset.set(
      CML.ScriptHash.from_hex(asset.policyId),
      CML.AssetName.from_hex(asset.assetName),
      asset.quantity,
    );
  }
  return CML.Value.new(lovelace, multiAsset);
};

const cardanoOutput = (
  addressHex: string,
  lovelace: bigint,
  assets: readonly {
    policyId: string;
    assetName: string;
    quantity: bigint;
  }[],
  datumCbor: string | null,
): string =>
  CML.TransactionOutput.new(
    CML.Address.from_hex(addressHex),
    cardanoValue(lovelace, assets),
    datumCbor === null
      ? null
      : CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
    null,
  ).to_canonical_cbor_hex();

const settlementOutput = (claim = false): string =>
  cardanoOutput(
    policy.settlementAddressHex,
    2_000_000n,
    [
      {
        policyId: settlementPolicyId,
        assetName: settlementAsset,
        quantity: 1n,
      },
    ],
    settlementDatum(claim ? { resolution_time: 2_000n, operator } : null),
  );

const stateQueueOutput = (): string =>
  cardanoOutput(
    enterpriseScriptAddress(applied.stateQueueSpend!),
    2_000_000n,
    [],
    null,
  );

const stateQueueMergeRedeemer = (
  confirmedStateInputOutRef: string,
  settlementRedeemerIndex = 0n,
): string => {
  const [transactionId, outputIndex] = confirmedStateInputOutRef.split("#");
  return Data.to(
    {
      MergeToConfirmedStateV1: {
        yield_to_ref_input_index: 0n,
        header_node_key: settlementAsset,
        confirmed_state_input_outref: {
          transactionId: transactionId!,
          outputIndex: BigInt(outputIndex!),
        },
        confirmed_state_output_index: 1n,
        m_settlement_redeemer_index: settlementRedeemerIndex,
        merged_block_withdrawals_root: ROOT,
        merged_block_forced_transactions_root: ROOT,
        merged_block_transactions_root: ROOT,
        merged_block_deposits_root: ROOT,
        merged_block_transition_trace_root: ROOT,
        merged_block_event_to_step_root: ROOT,
        merged_block_validation_traces_root: ROOT,
        merged_block_withdrawal_count: 0n,
        merged_block_forced_transaction_count: 0n,
        merged_block_l2_transaction_count: 0n,
        merged_block_deposit_count: 0n,
        merged_block_total_event_count: 0n,
        merged_block_transition_step_count: 0n,
        merged_block_validation_trace_count: 0n,
      },
    },
    StateQueueRedeemer,
  );
};

const transactionBody = (
  inputs: readonly string[],
  outputHexes: readonly string[],
  mint: readonly {
    policyId: string;
    assetName: string;
    quantity: bigint;
  }[],
): string => {
  const bodyInputs = CML.TransactionInputList.new();
  for (const outRef of inputs) {
    const [transactionId, outputIndex] = outRef.split("#");
    bodyInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(transactionId!),
        BigInt(outputIndex!),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  for (const output of outputHexes) {
    outputs.add(CML.TransactionOutput.from_cbor_hex(output));
  }
  const body = CML.TransactionBody.new(bodyInputs, outputs, 170_000n);
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(HUB_REFERENCE_OUT_REF.split("#")[0]!),
      0n,
    ),
  );
  referenceInputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(SETTLEMENT_REFERENCE_OUT_REF.split("#")[0]!),
      0n,
    ),
  );
  body.set_reference_inputs(referenceInputs);
  if (mint.length > 0) {
    const minted = CML.Mint.new();
    for (const asset of mint) {
      minted.set(
        CML.ScriptHash.from_hex(asset.policyId),
        CML.AssetName.from_hex(asset.assetName),
        asset.quantity,
      );
    }
    body.set_mint(minted);
  }
  body.set_script_data_hash(SETTLEMENT_SCRIPT_DATA_HASH);
  return body.to_canonical_cbor_hex();
};

const SETTLEMENT_SCRIPT_DATA_HASH = CML.ScriptDataHash.from_raw_bytes(
  Buffer.alloc(32, 0x7a),
);

const settlementRedeemerTag = (purpose: string): CML.RedeemerTag => {
  switch (purpose) {
    case "spend":
      return CML.RedeemerTag.Spend;
    case "mint":
      return CML.RedeemerTag.Mint;
    case "certificate":
      return CML.RedeemerTag.Cert;
    case "withdrawal":
      return CML.RedeemerTag.Reward;
    default:
      throw new Error(`unsupported test redeemer purpose: ${purpose}`);
  }
};

const settlementSubject = (
  outRef: string | null,
  status: WatcherSettlementSubject["status"],
  attempt = "0",
  failureCode: string | null = null,
  terminalTransactionHash: string | null = null,
): WatcherSettlementSubject =>
  makeWatcherSettlementSubject({
    subjectId: settlementAsset,
    subjectKind: "settlement",
    status,
    resourceOutRef: outRef,
    relatedSubjectId: null,
    resolutionTime: status === "open" || status === "resolved" ? null : "2000",
    operatorVkey: status === "open" || status === "resolved" ? null : operator,
    attempt,
    terminalTransactionHash,
    failureCode,
  })!;

const emptySnapshot = (): WatcherSettlementSnapshot =>
  makeWatcherSettlementSnapshot({ resources: [], subjects: [] })!;

const provider = {
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  network: "Preprod",
  providerId: "provider-a",
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: h32("97"),
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256: h32("77"),
  },
} as const;

const externalSource = {
  sourceMode: "external_providers",
  network: "Preprod",
  providers: [
    {
      providerId: "provider-a",
      operatorIdentitySha256: h32("97"),
      endpoint: "https://cardano-a.example",
    },
    {
      providerId: "provider-b",
      operatorIdentitySha256: h32("98"),
      endpoint: "https://cardano-b.example",
    },
  ],
} as const;

const rollbackProvider = (
  providerId: string,
  identityByte: string,
): WatcherAuthenticatedL1Provider => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  network: "Preprod",
  providerId,
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: h32(identityByte === "77" ? "97" : "98"),
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256:
      tlsIdentityByProviderId.get(providerId) ?? h32(identityByte),
  },
});

beforeAll(async () => {
  transportFixtureDirectory = await mkdtemp(
    join("/dev/shm", "midgard-w15-settlement-"),
  );
  for (const [providerId, operatorIdentitySha256] of [
    ["provider-a", h32("97")],
    ["provider-b", h32("98")],
  ] as const) {
    const fixture = await makeTlsTransportFixture(providerId);
    const endpoint = `https://localhost:${fixture.port}`;
    tlsIdentityByProviderId.set(providerId, fixture.identitySha256);
    transportEndpointByProviderId.set(providerId, endpoint);
    const configuredProvider = externalSource.providers.find(
      ({ providerId: configuredProviderId }) =>
        configuredProviderId === providerId,
    );
    if (configuredProvider === undefined) {
      throw new Error("missing external-provider fixture policy");
    }
    (configuredProvider as Mutable).endpoint = endpoint;
    if (providerId === provider.providerId) {
      (provider.authentication as Mutable).publicIdentitySha256 =
        fixture.identitySha256;
    }
    transportContexts.push(
      await establishWatcherExternalProviderTransport({
        network: "Preprod",
        providerId,
        operatorIdentitySha256,
        endpoint,
        caPem: fixture.certificate,
        expectedTlsPublicIdentitySha256: fixture.identitySha256,
        connectTimeoutMs: 2_000,
      }),
    );
  }
  finalityPolicy = makeExternalFinalityPolicy();
});

afterAll(async () => {
  for (const context of transportContexts) {
    closeWatcherL1TransportAttestationContext(context);
  }
  for (const server of tlsServers) server.close();
  await rm(transportFixtureDirectory, { recursive: true, force: true });
});

type RollbackPoint = Readonly<{
  blockHash: string;
  parentBlockHash: string | null;
  slot: string;
  blockNo: string;
  depth: string;
}>;

const rawBlock = (
  authenticatedProvider: WatcherAuthenticatedL1Provider,
  point: RollbackPoint,
  transaction: Mutable,
): Mutable => ({
  schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  network: "Preprod",
  providerId: authenticatedProvider.providerId,
  chainPoint: point,
  transactions: [transaction],
});

const persistedObservation = (value: WatcherNormalizedL1Block) => ({
  observationId: value.observationDigest,
  providerId: value.provider.providerId,
  chainPointId: value.chainPoint.chainPointId,
  payload: makeWatcherDurablePayload(
    encodeWatcherNormalizedL1Block(value).toString("hex"),
  ),
});

const persistedChainPoints = (
  values: readonly WatcherNormalizedL1Block[],
  preferredProviderId = "provider-a",
) => {
  const points = new Map<string, WatcherDurableStore["chainPoints"][number]>();
  for (const value of values) {
    if (
      !points.has(value.chainPoint.chainPointId) ||
      value.provider.providerId === preferredProviderId
    ) {
      points.set(value.chainPoint.chainPointId, {
        chainPointId: value.chainPoint.chainPointId,
        providerId: value.provider.providerId,
        blockHash: value.chainPoint.blockHash,
        slot: value.chainPoint.slot,
        blockNo: value.chainPoint.blockNo,
        depth: value.chainPoint.depth,
      });
    }
  }
  return [...points.values()];
};

/**
 * Durable stores canonicalize `chainPoints` by sorting on `chainPointId`, which
 * is a content hash. Positional access therefore carries no chain ordering, so
 * the newest point has to be selected by (blockNo, slot) the same way
 * `compareChainPointOrder` does inside the durable store.
 */
const latestChainPoint = (store: WatcherDurableStore) =>
  store.chainPoints.reduce((latest, candidate) =>
    BigInt(candidate.blockNo) > BigInt(latest.blockNo) ||
    (BigInt(candidate.blockNo) === BigInt(latest.blockNo) &&
      BigInt(candidate.slot) > BigInt(latest.slot))
      ? candidate
      : latest,
  );

let serial = 0;

type Bundle = Readonly<{
  observation: WatcherSettlementObservation;
  context: WatcherSettlementPublicContext;
  restartBinding: WatcherSettlementPublicContext["restartContexts"][number];
  store: ReturnType<typeof makeWatcherDurableStore>;
  finalityState: ReturnType<typeof evaluateWatcherFinality>["state"];
  finalityResult: ReturnType<typeof evaluateWatcherFinality>;
}>;

type SettlementFinalityLineage = NonNullable<
  WatcherSettlementPublicContext["finalityAuthority"]
>["lineage"];

const settlementFinalityLineageByStateDigest = new Map<
  string,
  SettlementFinalityLineage
>();

const bundle = (input: {
  policyOverride?: WatcherSettlementIndexerPolicy;
  kind: WatcherSettlementTransitionKind;
  previousState: WatcherSettlementIndexerState | null;
  snapshot: WatcherSettlementSnapshot;
  inputs?: readonly string[];
  outputHexes?: readonly string[];
  redeemers?: readonly {
    purpose: "spend" | "mint" | "certificate" | "withdrawal";
    index: string;
    cborHex: string;
  }[];
  mint?: readonly {
    policyId: string;
    assetName: string;
    quantity: bigint;
  }[];
  transition?: Partial<WatcherSettlementTransition>;
  restartBindings?: readonly WatcherSettlementPublicContext["restartContexts"][number][];
  restartRollbackBindings?: readonly WatcherSettlementPublicContext["restartRollbackContexts"][number][];
  protocolUtxos?: readonly WatcherProtocolUtxo[];
  authenticatedProvider?: WatcherAuthenticatedL1Provider;
  l1Observation?: Mutable;
  sourceDurableStore?: WatcherDurableStore;
  durableStore?: WatcherDurableStore;
  journalSpentAtChainPointId?: string;
  previousFinalityState?: unknown;
  rollbackAuthority?: WatcherSettlementPublicContext["rollbackAuthority"];
  predecessorStateDigest?: string;
  chainPointOffset?: bigint;
  parentBlockHash?: string | null;
  transactionIsValid?: boolean;
  forceEmptyBlock?: boolean;
  transactionPosition?: number;
}): Bundle => {
  const activePolicy = input.policyOverride ?? policy;
  serial += 1;
  const hasTransaction =
    !["retry", "mark_stuck", "mark_invalid", "rollback"].includes(input.kind) &&
    input.forceEmptyBlock !== true;
  const body = CML.TransactionBody.from_cbor_hex(
    transactionBody(
      input.inputs ?? [h32("81") + "#0"],
      input.outputHexes ?? [],
      input.mint ?? [],
    ),
  );
  if ((input.redeemers?.length ?? 0) > 0) {
    body.set_script_data_hash(SETTLEMENT_SCRIPT_DATA_HASH);
  }
  const bodyHex = body.to_canonical_cbor_hex();
  const transactionHash = computeHash32(Buffer.from(bodyHex, "hex")).toString(
    "hex",
  );
  const utxos = (input.outputHexes ?? []).map((outputHex, outputIndex) => {
    const output = CML.TransactionOutput.from_cbor_hex(outputHex);
    const canonicalOutputHex = output.to_canonical_cbor_hex();
    const datumHex = output.datum()?.as_datum()?.to_canonical_cbor_hex();
    return {
      outRef: `${transactionHash}#${outputIndex.toString()}`,
      outputIndex: outputIndex.toString(),
      output: makeWatcherL1PublicBytes(canonicalOutputHex),
      datum:
        datumHex === undefined
          ? null
          : {
              datumHash: computeHash32(Buffer.from(datumHex, "hex")).toString(
                "hex",
              ),
              bytes: makeWatcherL1PublicBytes(datumHex),
            },
      referenceScript: null,
    };
  });
  const authenticatedProvider = input.authenticatedProvider ?? provider;
  const previousStore = input.restartBindings?.at(-1)?.durableStore as
    | WatcherDurableStore
    | undefined;
  const sourceStore =
    input.sourceDurableStore ?? previousStore ?? bootstrapStore;
  const parentHash =
    [...sourceStore.chainPoints]
      .sort((left, right) =>
        BigInt(left.blockNo) < BigInt(right.blockNo) ? 1 : -1,
      )
      .at(0)?.blockHash ?? null;
  const parentPoint = [...sourceStore.chainPoints]
    .sort((left, right) =>
      BigInt(left.blockNo) < BigInt(right.blockNo) ? 1 : -1,
    )
    .at(0);
  const canonicalRedeemers = (input.redeemers ?? []).map((redeemer) => ({
    purpose: redeemer.purpose,
    index: redeemer.index,
    bytes: makeWatcherL1PublicBytes(
      CML.PlutusData.from_cbor_hex(redeemer.cborHex).to_canonical_cbor_hex(),
    ),
  }));
  const witnessSet = CML.TransactionWitnessSet.new();
  if (canonicalRedeemers.length > 0) {
    const redeemers = CML.LegacyRedeemerList.new();
    for (const redeemer of canonicalRedeemers) {
      redeemers.add(
        CML.LegacyRedeemer.new(
          settlementRedeemerTag(redeemer.purpose),
          BigInt(redeemer.index),
          CML.PlutusData.from_cbor_hex(redeemer.bytes.bytesHex),
          CML.ExUnits.new(0n, 0n),
        ),
      );
    }
    witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  }
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    input.transactionIsValid ?? true,
    undefined,
  );
  const l1Observation: Mutable =
    input.l1Observation ??
    ({
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
      network: "Preprod",
      providerId: authenticatedProvider.providerId,
      chainPoint: {
        blockHash: h32(((serial + 50) % 240).toString(16).padStart(2, "0")),
        parentBlockHash: input.parentBlockHash ?? parentHash,
        slot: (parentPoint === undefined
          ? BigInt(1_000 + serial)
          : BigInt(parentPoint.slot) + (input.chainPointOffset ?? 1n)
        ).toString(),
        blockNo: (parentPoint === undefined
          ? 100n
          : BigInt(parentPoint.blockNo) + (input.chainPointOffset ?? 1n)
        ).toString(),
        depth: "1",
      },
      transactions: hasTransaction
        ? [
            {
              txHash: transactionHash,
              transactionIndex: "0",
              fullTransaction: makeWatcherL1PublicBytes(
                fullTransaction.to_canonical_cbor_hex(),
              ),
              body: makeWatcherL1PublicBytes(bodyHex),
              witnessSet: makeWatcherL1PublicBytes(
                witnessSet.to_canonical_cbor_hex(),
              ),
              utxos: input.transactionIsValid === false ? [] : utxos,
              scripts: [],
              datums: [],
              redeemers: canonicalRedeemers,
            },
          ]
        : [],
    } satisfies Mutable);
  const normalized = normalizeWatcherL1Block(
    authenticatedProvider,
    l1Observation,
  );
  const indexedProtocolUtxos = (input.protocolUtxos ?? []).map(
    (utxo) =>
      sourceStore.protocolUtxos.find(
        ({ outRef }) => outRef === utxo.outRef,
      ) ?? {
        ...utxo,
        chainPointId: normalized.chainPoint.chainPointId,
      },
  );
  const protocolUtxos = [
    ...sourceStore.protocolUtxos.filter(({ role }) => role === "hub_oracle"),
    ...indexedProtocolUtxos,
  ];
  const observationRecord = {
    observationId: normalized.observationDigest,
    providerId: normalized.provider.providerId,
    chainPointId: normalized.chainPoint.chainPointId,
    payload: makeWatcherDurablePayload(
      encodeWatcherNormalizedL1Block(normalized).toString("hex"),
    ),
  };
  const chainPointRecord = {
    chainPointId: normalized.chainPoint.chainPointId,
    providerId: normalized.provider.providerId,
    blockHash: normalized.chainPoint.blockHash,
    slot: normalized.chainPoint.slot,
    blockNo: normalized.chainPoint.blockNo,
    depth: normalized.chainPoint.depth,
  };
  const nextL1Observations = [
    ...sourceStore.l1Observations,
    ...(sourceStore.l1Observations.some(
      ({ observationId }) => observationId === normalized.observationDigest,
    ) === true
      ? []
      : [observationRecord]),
  ];
  const nextChainPoints = [
    ...sourceStore.chainPoints,
    ...(sourceStore.chainPoints.some(
      ({ chainPointId }) => chainPointId === normalized.chainPoint.chainPointId,
    ) === true
      ? []
      : [chainPointRecord]),
  ];
  const protocolJournal = journalWatcherProtocolUtxoTransition({
    sourceStore,
    nextChainPoints,
    nextProtocolUtxos: protocolUtxos,
    spentAtChainPointId:
      input.journalSpentAtChainPointId ?? normalized.chainPoint.chainPointId,
  });
  const store =
    input.durableStore ??
    makeWatcherDurableStore({
      deploymentMarker: activePolicy.deploymentMarker,
      revision: (BigInt(sourceStore.revision) + 1n).toString(),
      records: {
        l1Observations: nextL1Observations,
        chainPoints: nextChainPoints,
        protocolUtxos: protocolJournal.protocolUtxos,
        spentProtocolUtxos: protocolJournal.spentProtocolUtxos,
        daProofInputs: sourceStore.daProofInputs,
        reconstructedStates: sourceStore.reconstructedStates,
        decisions: sourceStore.decisions,
        faults: sourceStore.faults,
        submissions: sourceStore.submissions,
        confirmations: sourceStore.confirmations,
        retries: sourceStore.retries,
        deadlines: sourceStore.deadlines,
        correctionResults: sourceStore.correctionResults,
      },
    });
  const transition: WatcherSettlementTransition = {
    kind: input.kind,
    subjectId: null,
    relatedSubjectId: null,
    consumedOutRefs: [],
    producedOutRefs: [],
    requiredRedeemers: [],
    exactMint: [],
    failureCode: null,
    retryAttempt: null,
    ...input.transition,
  };
  const observation = makeWatcherSettlementObservation({
    policyDigest: activePolicy.policyDigest,
    network: activePolicy.network,
    releaseEvidenceDigest: activePolicy.releaseEvidenceDigest,
    deploymentMarker: activePolicy.deploymentMarker,
    pointDigest: normalized.chainPoint.pointDigest,
    chainPointId: normalized.chainPoint.chainPointId,
    blockHash: normalized.chainPoint.blockHash,
    slot: normalized.chainPoint.slot,
    blockNo: normalized.chainPoint.blockNo,
    transactionHash: hasTransaction
      ? (normalized.transactions[input.transactionPosition ?? 0]?.txHash ??
        null)
      : null,
    sourceObservationDigest: normalized.observationDigest,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStore(store),
    ),
    predecessorStateDigest:
      input.predecessorStateDigest ?? input.previousState?.stateDigest ?? null,
    transition,
    snapshot: input.snapshot,
  });
  expect(observation).not.toBeNull();
  const otherProvider = rollbackProvider("provider-b", "78");
  const otherL1Observation = structuredClone(l1Observation);
  otherL1Observation.providerId = otherProvider.providerId;
  const finalityObservations = [
    { authenticatedProvider, l1Observation },
    {
      authenticatedProvider: otherProvider,
      l1Observation: otherL1Observation,
    },
  ];
  const configuredSource = externalSource;
  const activeFinalityPolicy = finalityPolicy;
  const consistency = evaluateWatcherMultiProviderConsistency(
    configuredSource,
    finalityObservations.map(({ authenticatedProvider, l1Observation }) =>
      normalizeWatcherL1Block(authenticatedProvider, l1Observation),
    ),
  );
  const previousFinalityState = input.previousFinalityState ?? null;
  const previousStateDigest =
    previousFinalityState !== null &&
    typeof previousFinalityState === "object" &&
    "stateDigest" in previousFinalityState &&
    typeof previousFinalityState.stateDigest === "string"
      ? previousFinalityState.stateDigest
      : null;
  const lineage =
    previousStateDigest === null
      ? []
      : (settlementFinalityLineageByStateDigest.get(previousStateDigest) ?? []);
  const finalityResult = evaluateWatcherFinality(
    activeFinalityPolicy,
    previousFinalityState,
    consistency,
  );
  if (finalityResult.state !== null) {
    settlementFinalityLineageByStateDigest.set(
      finalityResult.state.stateDigest,
      [
        ...lineage,
        {
          observations: finalityObservations,
          consistency,
          result: finalityResult,
        },
      ],
    );
  }
  const finalityAuthority =
    input.kind === "rollback"
      ? null
      : {
          policy: activeFinalityPolicy,
          lineage,
          previousState: previousFinalityState,
          observations: finalityObservations,
          consistency,
          result: finalityResult,
        };
  const context: WatcherSettlementPublicContext = {
    schemaVersion: WATCHER_SETTLEMENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
    authenticatedProvider,
    l1Observation,
    sourceDurableStore: sourceStore,
    durableStore: store,
    deploymentAuthority,
    finalityAuthority,
    rollbackAuthority: input.rollbackAuthority ?? null,
    restartContexts: input.restartBindings ?? [],
    restartRollbackContexts: input.restartRollbackBindings ?? [],
  };
  const contextDigest = sha256CanonicalForTest({
    policyDigest: activePolicy.policyDigest,
    observationDigest: observation!.observationDigest,
    normalizedObservationDigest: normalized.observationDigest,
    sourceStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStore(sourceStore),
    ),
    storeDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStore(store),
    ),
    deploymentAuthorityDigest: sha256CanonicalForTest(deploymentAuthority),
    finalityResultDigest: finalityAuthority?.result.resultDigest ?? null,
  });
  return {
    observation: observation!,
    context,
    restartBinding: {
      publicContextDigest: contextDigest,
      authenticatedProvider,
      l1Observation,
      sourceDurableStore: sourceStore,
      durableStore: store,
      deploymentAuthority,
      finalityAuthority,
    },
    store,
    finalityState: finalityResult.state,
    finalityResult,
  };
};

const sha256CanonicalForTest = (value: unknown): string => {
  const canonical = (candidate: any): string => {
    if (
      candidate === null ||
      typeof candidate === "boolean" ||
      typeof candidate === "string"
    ) {
      return JSON.stringify(candidate);
    }
    if (Array.isArray(candidate)) {
      return `[${candidate.map(canonical).join(",")}]`;
    }
    return `{${Object.keys(candidate)
      .sort()
      .map((key) => `${JSON.stringify(key)}:${canonical(candidate[key])}`)
      .join(",")}}`;
  };
  return createHash("sha256").update(canonical(value), "utf8").digest("hex");
};

const accepted = (
  state: WatcherSettlementIndexerState | null,
  evidence: Bundle,
  activePolicy: WatcherSettlementIndexerPolicy = policy,
): WatcherSettlementIndexerState => {
  const result = evaluateWatcherSettlementIndexer(
    activePolicy,
    state,
    evidence.observation,
    evidence.context,
  );
  expect(result.action, JSON.stringify(result)).toBe("accept");
  expect(result.state).not.toBeNull();
  expect(
    parseWatcherSettlementIndexerResult(result, {
      policy: activePolicy,
      previousState: state,
      observation: evidence.observation,
      publicContext: evidence.context,
    }),
  ).toEqual(result);
  return result.state!;
};

const protocolUtxo = (
  outRef: string,
  outputHex: string,
): WatcherProtocolUtxo => ({
  outRef,
  role: "settlement",
  chainPointId: h32("00"),
  output: makeWatcherDurablePayload(outputHex),
});

const scenarioBootstrap = (
  protocolUtxos: readonly WatcherProtocolUtxo[],
  spentProtocolUtxos: WatcherDurableStore["spentProtocolUtxos"] = [],
): Readonly<{
  policy: WatcherSettlementIndexerPolicy;
  store: WatcherDurableStore;
}> => {
  const store = makeWatcherDurableStore({
    deploymentMarker: policy.deploymentMarker,
    revision: "0",
    records: {
      l1Observations: [],
      chainPoints:
        protocolUtxos.length === 0
          ? []
          : [
              {
                chainPointId: h32("00"),
                providerId: "provider-a",
                blockHash: h32("01"),
                slot: "1",
                blockNo: "1",
                depth: "10",
              },
            ],
      protocolUtxos: [hubReferenceUtxo, ...protocolUtxos],
      spentProtocolUtxos,
      daProofInputs: [],
      reconstructedStates: [],
      decisions: [],
      faults: [],
      submissions: [],
      confirmations: [],
      retries: [],
      deadlines: [],
      correctionResults: [],
    },
  });
  const scenarioPolicy = makeWatcherSettlementIndexerPolicy({
    ...policy,
    bootstrapStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStore(store),
    ),
  });
  expect(scenarioPolicy).not.toBeNull();
  return { policy: scenarioPolicy!, store };
};

const spawnSequence = (
  options: Readonly<{
    emptyBlockGap?: boolean;
  }> = {},
) => {
  const authenticatedProvider = provider;
  const bootstrapEvidence = bundle({
    kind: "bootstrap",
    previousState: null,
    snapshot: emptySnapshot(),
    authenticatedProvider,
  });
  const bootstrapState = accepted(null, bootstrapEvidence);
  const gapEvidence =
    options.emptyBlockGap === true
      ? bundle({
          kind: "retry",
          previousState: bootstrapState,
          snapshot: emptySnapshot(),
          transition: {
            subjectId: "empty-block-gap",
            retryAttempt: "1",
            failureCode: "empty_block_gap",
          },
          restartBindings: [bootstrapEvidence.restartBinding],
          authenticatedProvider,
          forceEmptyBlock: true,
        })
      : null;
  const gapPoint =
    gapEvidence === null
      ? null
      : ((gapEvidence.context.l1Observation as Mutable).chainPoint as Mutable);
  const outputHex = settlementOutput();
  const queueOutputHex = stateQueueOutput();
  const inputs = [h32("85") + "#0"];
  const mint = [
    {
      policyId: settlementPolicyId,
      assetName: settlementAsset,
      quantity: 1n,
    },
    {
      policyId: stateQueuePolicyId,
      assetName: `4d424c43${settlementAsset}`,
      quantity: -1n,
    },
  ] as const;
  const bodyHex = transactionBody(inputs, [outputHex, queueOutputHex], mint);
  const transactionHash = computeHash32(Buffer.from(bodyHex, "hex")).toString(
    "hex",
  );
  const outRef = `${transactionHash}#0`;
  const durable = protocolUtxo(outRef, outputHex);
  const resource = makeWatcherSettlementResourceFromProtocolUtxo(
    policy,
    durable,
  )!;
  const snapshot = makeWatcherSettlementSnapshot({
    resources: [resource],
    subjects: [settlementSubject(outRef, "open")],
  })!;
  const spawnRedeemer = Data.to(
    {
      Spawn: {
        settlement_id: settlementAsset,
        output_index: 0n,
        state_queue_merge_redeemer_index: 1n,
        hub_ref_input_index: 0n,
      },
    },
    SettlementMintRedeemer,
  );
  const spawnEvidence = bundle({
    kind: "spawn_settlement",
    previousState: bootstrapState,
    snapshot,
    inputs,
    outputHexes: [outputHex, queueOutputHex],
    mint,
    redeemers: [
      { purpose: "mint", index: "0", cborHex: spawnRedeemer },
      {
        purpose: "mint",
        index: "1",
        cborHex: stateQueueMergeRedeemer(inputs[0]!),
      },
    ],
    transition: {
      subjectId: settlementAsset,
      producedOutRefs: [outRef],
      requiredRedeemers: [
        {
          purpose: "mint",
          index: "0",
          schema: "settlement_mint",
          constructor: "Spawn",
        },
        {
          purpose: "mint",
          index: "1",
          schema: "state_queue_mint",
          constructor: "MergeToConfirmedStateV1",
        },
      ],
      exactMint: [
        {
          policyId: settlementPolicyId,
          assetName: settlementAsset,
          quantity: "1",
        },
        {
          policyId: stateQueuePolicyId,
          assetName: `4d424c43${settlementAsset}`,
          quantity: "-1",
        },
      ],
    },
    restartBindings: [bootstrapEvidence.restartBinding],
    protocolUtxos: [durable],
    authenticatedProvider,
    previousFinalityState: gapEvidence?.finalityState,
    chainPointOffset: gapEvidence === null ? undefined : 2n,
    parentBlockHash: gapPoint === null ? undefined : String(gapPoint.blockHash),
  });
  const spawnState = accepted(bootstrapState, spawnEvidence);
  return {
    bootstrapEvidence,
    bootstrapState,
    gapEvidence,
    spawnEvidence,
    spawnState,
    outRef,
    durable,
  };
};

const postFinalitySettlementEvidence = (
  rawObservation: Mutable,
  depth: string,
) => {
  const primaryProvider = provider;
  const primaryRaw = {
    ...structuredClone(rawObservation),
    providerId: primaryProvider.providerId,
    chainPoint: {
      ...(structuredClone(rawObservation.chainPoint) as Mutable),
      depth,
    },
  };
  const observations = [
    normalizeWatcherL1Block(primaryProvider, primaryRaw),
    normalizeWatcherL1Block(rollbackProvider("provider-b", "78"), {
      ...structuredClone(primaryRaw),
      providerId: "provider-b",
    }),
  ];
  const consistency = evaluateWatcherMultiProviderConsistency(
    externalSource,
    observations,
  );
  expect(consistency).toMatchObject({
    status: "agreed",
    sourceMode: "external_providers",
    independentProviderCount: 2,
  });
  return { primaryRaw, observations, consistency };
};

const postFinalitySettlementRecoveryBundle = (
  sequence: ReturnType<typeof spawnSequence>,
) => {
  const selectedFinalityPolicy = finalityPolicy;
  const commonRaw = (sequence.gapEvidence?.context.l1Observation ??
    sequence.bootstrapEvidence.context.l1Observation) as Mutable;
  const orphanRaw = sequence.spawnEvidence.context.l1Observation as Mutable;
  const common = postFinalitySettlementEvidence(commonRaw, "0");
  const orphanPending = postFinalitySettlementEvidence(orphanRaw, "1");
  const orphanFinalized = postFinalitySettlementEvidence(
    orphanRaw,
    selectedFinalityPolicy.confirmationDepth,
  );
  const replacementProvider = provider;
  const replacementRaw: Mutable = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
    network: "Preprod",
    providerId: replacementProvider.providerId,
    chainPoint: {
      blockHash: h32("e9"),
      parentBlockHash: common.observations[0]!.chainPoint.blockHash,
      slot: (BigInt(common.observations[0]!.chainPoint.slot) + 2n).toString(),
      blockNo: (
        BigInt(common.observations[0]!.chainPoint.blockNo) + 1n
      ).toString(),
      depth: "0",
    },
    transactions: structuredClone(
      (sequence.spawnEvidence.context.l1Observation as Mutable).transactions,
    ),
  };
  const replacement = postFinalitySettlementEvidence(replacementRaw, "0");
  const replacementTail = [2, 3].map((offset) => {
    const prior = offset === 2 ? replacement.observations[0]! : undefined;
    const raw: Mutable = {
      ...structuredClone(replacementRaw),
      chainPoint: {
        blockHash: h32(`c${offset.toString()}`),
        parentBlockHash: prior?.chainPoint.blockHash ?? h32("c2"),
        slot: (
          BigInt(common.observations[0]!.chainPoint.slot) + BigInt(offset + 1)
        ).toString(),
        blockNo: (
          BigInt(common.observations[0]!.chainPoint.blockNo) + BigInt(offset)
        ).toString(),
        depth: "0",
      },
      transactions: [],
    };
    return {
      raw,
      evidence: postFinalitySettlementEvidence(raw, "0"),
    };
  });
  const pending = evaluateWatcherFinality(
    selectedFinalityPolicy,
    null,
    orphanPending.consistency,
  );
  expect(pending.action).toBe("observe_pending");
  const finalized = evaluateWatcherFinality(
    selectedFinalityPolicy,
    pending.state,
    orphanFinalized.consistency,
  );
  expect(finalized.action).toBe("finalize");
  const contradiction = evaluateWatcherFinality(
    selectedFinalityPolicy,
    finalized.state,
    replacementTail.at(-1)!.evidence.consistency,
  );
  expect(contradiction.action).toBe("quarantine_incident");

  const baseStore = sequence.spawnEvidence.store;
  const persisted = [
    ...common.observations,
    ...orphanPending.observations,
    ...orphanFinalized.observations,
    ...replacement.observations,
    ...replacementTail.flatMap(({ evidence }) => evidence.observations),
  ];
  const sourceStore = makeWatcherDurableStore({
    deploymentMarker: baseStore.deploymentMarker,
    revision: (BigInt(baseStore.revision) + 1n).toString(),
    records: {
      ...baseStore,
      l1Observations: [
        ...new Map(
          [
            ...baseStore.l1Observations,
            ...persisted.map(persistedObservation),
          ].map((entry) => [entry.observationId, entry]),
        ).values(),
      ],
      chainPoints: [
        ...new Map(
          [
            ...baseStore.chainPoints,
            ...persistedChainPoints(persisted, provider.providerId),
          ].map((entry) => [entry.chainPointId, entry]),
        ).values(),
      ],
    },
  });
  const rollbackBootstrapState = makeWatcherRollbackBootstrapState(
    selectedFinalityPolicy,
    sourceStore,
    finalized.state,
  )!;
  const incident = evaluateWatcherRollback(
    selectedFinalityPolicy,
    sourceStore,
    finalized.state,
    replacementTail.at(-1)!.evidence.consistency,
    contradiction,
    rollbackBootstrapState,
    rollbackBootstrapState,
  );
  expect(incident).toMatchObject({
    action: "quarantine_incident",
    protocolDecision: "quarantined",
  });
  const recoveryInput: WatcherPostFinalityRecoveryInput = {
    policy: selectedFinalityPolicy,
    sourceStore: incident.nextStore,
    currentStore: incident.nextStore,
    quarantinedRollbackState: incident.rollbackState,
    rollbackBootstrapState,
    previousCanonicalPath: [common.consistency, orphanFinalized.consistency],
    replacementCanonicalPath: [
      common.consistency,
      replacement.consistency,
      ...replacementTail.map(({ evidence }) => evidence.consistency),
    ],
    previousRecoveryState: null,
  };
  const recovery = evaluateWatcherPostFinalityRecovery(recoveryInput);
  expect(recovery).toMatchObject({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    recoveryState: {
      path: {
        commonAncestorPointDigest:
          common.observations[0]!.chainPoint.pointDigest,
        replacementTipPointDigest:
          replacementTail.at(-1)!.evidence.observations[0]!.chainPoint
            .pointDigest,
      },
    },
  });
  expect(recovery.nextStore).not.toBeNull();
  const recoveryEvidence = bundle({
    kind: "rollback",
    previousState: sequence.spawnState,
    predecessorStateDigest: sequence.spawnState.stateDigest,
    snapshot: emptySnapshot(),
    restartBindings: [
      sequence.bootstrapEvidence.restartBinding,
      sequence.spawnEvidence.restartBinding,
    ],
    authenticatedProvider: replacementProvider,
    l1Observation: common.primaryRaw,
    sourceDurableStore: incident.nextStore!,
    durableStore: recovery.nextStore!,
    journalSpentAtChainPointId:
      replacementTail.at(-1)!.evidence.observations[0]!.chainPoint.chainPointId,
    rollbackAuthority: {
      result: recovery,
      context: recoveryInput,
    },
  });
  return {
    common,
    replacement,
    replacementRaw,
    replacementProvider,
    sourceStore,
    incident,
    recoveryInput,
    recovery,
    recoveryEvidence,
  };
};

describe("authenticated settlement, reserve, and payout indexer", () => {
  it("bootstraps from a W10/W03-authenticated empty topology", () => {
    const evidence = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, evidence);
    expect(state.activeHistory).toHaveLength(1);
    expect(state.snapshot.resources).toEqual([]);
  });

  it("encodes state and result bytes through the hardened canonical-JSON walk", () => {
    const evidence = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const result = evaluateWatcherSettlementIndexer(
      policy,
      null,
      evidence.observation,
      evidence.context,
    );
    expect(result.action).toBe("accept");
    expect(result.state).not.toBeNull();
    expect(
      encodeWatcherSettlementIndexerState(result.state!).toString("utf8"),
    ).toBe(watcherCanonicalJson(result.state));
    expect(encodeWatcherSettlementIndexerResult(result).toString("utf8")).toBe(
      watcherCanonicalJson(result),
    );
    // Numbers must survive the walk verbatim: the previous indexer-local walk
    // had no number branch and serialized them as `{}`, which collapsed
    // numerically distinct values into one digest.
    expect(
      watcherCanonicalJson({ retries: 3, nested: [1, { depth: 2 }] }),
    ).toBe('{"nested":[1,{"depth":2}],"retries":3}');
    expect(watcherSha256CanonicalJson({ retries: 3 })).not.toBe(
      watcherSha256CanonicalJson({ retries: 4 }),
    );
  });

  it("preserves an existing foreign role but rejects inserting an ordinary output under it", () => {
    const outputHex = stateQueueOutput();
    const retainedForeign: WatcherProtocolUtxo = {
      outRef: `${h32("7e")}#0`,
      role: "state_queue",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(outputHex),
    };
    const scenario = scenarioBootstrap([retainedForeign]);
    const retained = bundle({
      policyOverride: scenario.policy,
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      sourceDurableStore: scenario.store,
      protocolUtxos: [retainedForeign],
    });
    expect(
      accepted(null, retained, scenario.policy).snapshot.resources,
    ).toEqual([]);

    const bodyHex = transactionBody([h32("81") + "#0"], [outputHex], []);
    const transactionHash = computeHash32(Buffer.from(bodyHex, "hex")).toString(
      "hex",
    );
    const wrongRole: WatcherProtocolUtxo = {
      outRef: `${transactionHash}#0`,
      role: "state_queue",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(outputHex),
    };
    const evidence = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      outputHexes: [outputHex],
      protocolUtxos: [wrongRole],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        evidence.observation,
        evidence.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("rejects external-provider two-step competing forks and oversized W12 evidence before indexing", () => {
    const authenticatedProvider = provider;
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      inputs: [h32("c2") + "#0"],
      authenticatedProvider,
    });
    const oversized = structuredClone(initial.context) as Mutable;
    const authority = oversized.finalityAuthority as Mutable;
    authority.observations = Array.from({ length: 17 }, () =>
      structuredClone(authority.observations[0]),
    );
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        initial.observation,
        oversized,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    const cyclic = structuredClone(initial.context) as Mutable;
    cyclic.finalityAuthority.lineage = [cyclic.finalityAuthority];
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        initial.observation,
        cyclic,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    const oversizedSparse = structuredClone(initial.context) as Mutable;
    const sparseLineage: unknown[] = [];
    sparseLineage.length =
      WATCHER_SETTLEMENT_INDEXER_BOUNDS.evidenceContainerEntries + 1;
    oversizedSparse.finalityAuthority.lineage = sparseLineage;
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        initial.observation,
        oversizedSparse,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    const veryWide = structuredClone(initial.context) as Mutable;
    veryWide.untrusted = Object.fromEntries(
      Array.from(
        {
          length:
            WATCHER_SETTLEMENT_INDEXER_BOUNDS.evidenceContainerEntries + 1,
        },
        (_, index) => [`field_${index.toString()}`, "x"],
      ),
    );
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        initial.observation,
        veryWide,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const invalid = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      inputs: [h32("c6") + "#0"],
      outputHexes: [settlementOutput()],
      mint: [
        {
          policyId: settlementPolicyId,
          assetName: settlementAsset,
          quantity: 1n,
        },
      ],
      authenticatedProvider,
      transactionIsValid: false,
    });
    expect(
      normalizeWatcherL1Block(
        authenticatedProvider,
        invalid.context.l1Observation,
      ).transactions[0],
    ).toMatchObject({ isValid: false, utxos: [] });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        invalid.observation,
        invalid.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const state = accepted(null, initial);
    const twoStepFork = bundle({
      kind: "bootstrap",
      previousState: state,
      snapshot: emptySnapshot(),
      inputs: [h32("c4") + "#0"],
      restartBindings: [initial.restartBinding],
      authenticatedProvider,
      chainPointOffset: 2n,
    });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        state,
        twoStepFork.observation,
        twoStepFork.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["stale_chain_point"],
    });
  });

  it("authenticates external-provider settlement ancestry across an empty intermediate block", () => {
    const sequence = spawnSequence({ emptyBlockGap: true });
    expect(sequence.spawnState.snapshot).toMatchObject({
      resources: [{ role: "settlement" }],
      subjects: [{ status: "open" }],
    });
  });

  it("orders same-block transaction-backed transitions by their canonical transactionIndex", () => {
    const sequence = spawnSequence();
    const bootstrapState = accepted(null, sequence.bootstrapEvidence);
    const bootstrapRaw = structuredClone(
      sequence.bootstrapEvidence.context.l1Observation,
    ) as Mutable;
    const spawnRaw = structuredClone(
      sequence.spawnEvidence.context.l1Observation,
    ) as Mutable;
    const priorTransaction = structuredClone(
      bootstrapRaw.transactions[0],
    ) as Mutable;
    priorTransaction.transactionIndex = "0";
    const currentTransaction = structuredClone(
      spawnRaw.transactions[0],
    ) as Mutable;
    currentTransaction.transactionIndex = "1";
    const sameBlockRaw = structuredClone(spawnRaw) as Mutable;
    sameBlockRaw.chainPoint = structuredClone(bootstrapRaw.chainPoint);
    sameBlockRaw.transactions = [priorTransaction, currentTransaction];
    const sameBlock = bundle({
      kind: "spawn_settlement",
      previousState: bootstrapState,
      snapshot: sequence.spawnEvidence.observation.snapshot,
      transition: sequence.spawnEvidence.observation.transition,
      restartBindings: [sequence.bootstrapEvidence.restartBinding],
      protocolUtxos: [sequence.durable],
      l1Observation: sameBlockRaw,
      transactionPosition: 1,
    });
    expect(sameBlock.observation.pointDigest).toBe(
      sequence.bootstrapEvidence.observation.pointDigest,
    );
    expect(accepted(bootstrapState, sameBlock).snapshot).toEqual(
      sequence.spawnState.snapshot,
    );

    const reversedRaw = structuredClone(sameBlockRaw) as Mutable;
    reversedRaw.transactions = [
      { ...structuredClone(currentTransaction), transactionIndex: "0" },
      { ...structuredClone(priorTransaction), transactionIndex: "1" },
    ];
    const reversed = bundle({
      kind: "spawn_settlement",
      previousState: bootstrapState,
      snapshot: sequence.spawnEvidence.observation.snapshot,
      transition: sequence.spawnEvidence.observation.transition,
      restartBindings: [sequence.bootstrapEvidence.restartBinding],
      protocolUtxos: [sequence.durable],
      l1Observation: reversedRaw,
      transactionPosition: 0,
    });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        bootstrapState,
        reversed.observation,
        reversed.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["stale_chain_point"],
    });

    const forgedOrdinalContext = structuredClone(sameBlock.context) as Mutable;
    forgedOrdinalContext.l1Observation.transactions[1].transactionIndex = "7";
    for (const candidate of forgedOrdinalContext.finalityAuthority
      .observations) {
      candidate.l1Observation.transactions[1].transactionIndex = "7";
    }
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        bootstrapState,
        sameBlock.observation,
        forgedOrdinalContext,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("rejects self-rehashed deployment/provider forgeries and binds pre/post-finality outcomes", () => {
    const pending = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    expect(pending.finalityResult).toMatchObject({
      action: "observe_pending",
      protocolDecision: "hold",
    });
    expect(accepted(null, pending).snapshot).toEqual(emptySnapshot());

    const forgedPolicy = makeWatcherSettlementIndexerPolicy({
      ...policy,
      settlementPolicyId: h28("f0"),
      settlementSpendScriptHash: h28("f1"),
      settlementAddressHex: enterpriseScriptAddress(h28("f1")),
    })!;
    const forged = bundle({
      policyOverride: forgedPolicy,
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    expect(
      evaluateWatcherSettlementIndexer(
        forgedPolicy,
        null,
        forged.observation,
        forged.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const disagreement = structuredClone(pending.context) as Mutable;
    disagreement.finalityAuthority.observations[1].l1Observation.transactions =
      [];
    const disagreementBlocks = disagreement.finalityAuthority.observations.map(
      (candidate: Mutable) =>
        normalizeWatcherL1Block(
          candidate.authenticatedProvider,
          candidate.l1Observation,
        ),
    );
    disagreement.finalityAuthority.consistency =
      evaluateWatcherMultiProviderConsistency(
        externalSource,
        disagreementBlocks,
      );
    disagreement.finalityAuthority.result = evaluateWatcherFinality(
      finalityPolicy,
      null,
      disagreement.finalityAuthority.consistency,
    );
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        pending.observation,
        disagreement,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const finalRaw = structuredClone(pending.context.l1Observation) as Mutable;
    finalRaw.chainPoint.depth = policy.requiredFinalityDepth;
    const finalized = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      authenticatedProvider: provider,
      l1Observation: finalRaw,
      previousFinalityState: pending.finalityState,
    });
    expect(finalized.finalityResult).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
    });
    expect(accepted(null, finalized).snapshot).toEqual(emptySnapshot());

    const contradictoryRaw = structuredClone(finalRaw) as Mutable;
    contradictoryRaw.chainPoint.blockHash = h32("f2");
    contradictoryRaw.chainPoint.slot = (
      BigInt(contradictoryRaw.chainPoint.slot) + 1n
    ).toString();
    contradictoryRaw.chainPoint.blockNo = (
      BigInt(contradictoryRaw.chainPoint.blockNo) + 1n
    ).toString();
    const contradiction = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      authenticatedProvider: provider,
      l1Observation: contradictoryRaw,
      previousFinalityState: finalized.finalityState,
    });
    expect(contradiction.finalityResult).toMatchObject({
      action: "quarantine_incident",
      protocolDecision: "quarantined",
    });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        null,
        contradiction.observation,
        contradiction.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("authenticates canonical Cardano output CBOR and SDK Settlement datum/redeemer CBOR", () => {
    const bootstrapEvidence = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const bootstrapState = accepted(null, bootstrapEvidence);
    const outputHex = settlementOutput();
    const queueOutputHex = stateQueueOutput();
    const inputOutRef = h32("82") + "#0";
    const bodyHex = transactionBody(
      [inputOutRef],
      [outputHex, queueOutputHex],
      [
        {
          policyId: settlementPolicyId,
          assetName: settlementAsset,
          quantity: 1n,
        },
        {
          policyId: stateQueuePolicyId,
          assetName: `4d424c43${settlementAsset}`,
          quantity: -1n,
        },
      ],
    );
    const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
    const outRef = `${txHash}#0`;
    const durable = protocolUtxo(outRef, outputHex);
    const resource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      durable,
    );
    expect(resource).not.toBeNull();
    const snapshot = makeWatcherSettlementSnapshot({
      resources: [resource!],
      subjects: [settlementSubject(outRef, "open")],
    })!;
    const spawnRedeemer = Data.to(
      {
        Spawn: {
          settlement_id: settlementAsset,
          output_index: 0n,
          state_queue_merge_redeemer_index: 1n,
          hub_ref_input_index: 0n,
        },
      },
      SettlementMintRedeemer,
    );
    const evidence = bundle({
      kind: "spawn_settlement",
      previousState: bootstrapState,
      snapshot,
      inputs: [inputOutRef],
      outputHexes: [outputHex, queueOutputHex],
      mint: [
        {
          policyId: settlementPolicyId,
          assetName: settlementAsset,
          quantity: 1n,
        },
        {
          policyId: stateQueuePolicyId,
          assetName: `4d424c43${settlementAsset}`,
          quantity: -1n,
        },
      ],
      redeemers: [
        { purpose: "mint", index: "0", cborHex: spawnRedeemer },
        {
          purpose: "mint",
          index: "1",
          cborHex: stateQueueMergeRedeemer(inputOutRef),
        },
      ],
      transition: {
        subjectId: settlementAsset,
        producedOutRefs: [outRef],
        requiredRedeemers: [
          {
            purpose: "mint",
            index: "0",
            schema: "settlement_mint",
            constructor: "Spawn",
          },
          {
            purpose: "mint",
            index: "1",
            schema: "state_queue_mint",
            constructor: "MergeToConfirmedStateV1",
          },
        ],
        exactMint: [
          {
            policyId: settlementPolicyId,
            assetName: settlementAsset,
            quantity: "1",
          },
          {
            policyId: stateQueuePolicyId,
            assetName: `4d424c43${settlementAsset}`,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [bootstrapEvidence.restartBinding],
      protocolUtxos: [durable],
    });
    expect(evidence.observation.transactionHash).toBe(txHash);
    expect(accepted(bootstrapState, evidence).snapshot).toEqual(snapshot);

    const unauthenticatedHubOutput = CML.TransactionOutput.new(
      CML.Address.from_hex(enterpriseScriptAddress(applied.hubOracleMint!)),
      CML.Value.new(2_000_000n, CML.MultiAsset.new()),
      null,
      null,
    ).to_canonical_cbor_hex();
    const unauthenticatedHub = {
      ...hubReferenceUtxo,
      output: makeWatcherDurablePayload(unauthenticatedHubOutput),
    };
    const unauthenticatedHubStore = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: bootstrapStore.revision,
      records: {
        ...bootstrapStore,
        protocolUtxos: [unauthenticatedHub],
      },
    });
    const unauthenticatedHubPolicy = makeWatcherSettlementIndexerPolicy({
      ...policy,
      bootstrapStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStore(unauthenticatedHubStore),
      ),
    })!;
    const badBootstrap = bundle({
      policyOverride: unauthenticatedHubPolicy,
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      sourceDurableStore: unauthenticatedHubStore,
    });
    const badBootstrapState = accepted(
      null,
      badBootstrap,
      unauthenticatedHubPolicy,
    );
    const badHubEvidence = bundle({
      policyOverride: unauthenticatedHubPolicy,
      kind: "spawn_settlement",
      previousState: badBootstrapState,
      snapshot,
      inputs: [inputOutRef],
      outputHexes: [outputHex, queueOutputHex],
      mint: [
        {
          policyId: settlementPolicyId,
          assetName: settlementAsset,
          quantity: 1n,
        },
        {
          policyId: stateQueuePolicyId,
          assetName: `4d424c43${settlementAsset}`,
          quantity: -1n,
        },
      ],
      redeemers: [
        { purpose: "mint", index: "0", cborHex: spawnRedeemer },
        {
          purpose: "mint",
          index: "1",
          cborHex: stateQueueMergeRedeemer(inputOutRef),
        },
      ],
      transition: evidence.observation.transition,
      restartBindings: [badBootstrap.restartBinding],
      protocolUtxos: [durable],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        unauthenticatedHubPolicy,
        badBootstrapState,
        badHubEvidence.observation,
        badHubEvidence.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["redeemer_mismatch"],
    });
  });

  it("rejects a watcher-only/non-Cardano output and a malformed adjacent datum", () => {
    const fake = protocolUtxo(h32("83") + "#0", "7b7d");
    expect(
      makeWatcherSettlementResourceFromProtocolUtxo(policy, fake),
    ).toBeNull();
    const adjacentDatum = Data.to(new Constr(1, []));
    const malformed = protocolUtxo(
      h32("84") + "#0",
      cardanoOutput(
        policy.settlementAddressHex,
        2_000_000n,
        [
          {
            policyId: settlementPolicyId,
            assetName: settlementAsset,
            quantity: 1n,
          },
        ],
        adjacentDatum,
      ),
    );
    expect(
      makeWatcherSettlementResourceFromProtocolUtxo(policy, malformed),
    ).toBeNull();
  });

  it("authenticates exact reserve, payout, and withdrawal topology with their production datum languages", () => {
    const addressData = {
      paymentCredential: {
        PublicKeyCredential: [h28("52")] as [string],
      },
      stakeCredential: null,
    };
    const l2Value = new Map([
      ["", new Map([["", 7_000_000n]])],
      [h28("66"), new Map([["01", 3n]])],
    ]);
    const payoutDatum = Data.to(
      {
        l2_value: l2Value,
        l1_address: addressData,
        l1_datum: "NoDatum",
      },
      PayoutDatum,
    );
    const payoutAsset = "ab";
    const payout = {
      outRef: h32("a1") + "#0",
      role: "payout" as const,
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.payoutAddressHex,
          7_000_000n,
          [
            {
              policyId: payoutPolicyId,
              assetName: payoutAsset,
              quantity: 1n,
            },
          ],
          payoutDatum,
        ),
      ),
    };
    const withdrawalDatum = Data.to(
      {
        event: {
          id: { transactionId: h32("61"), outputIndex: 0n },
          info: {
            body: {
              l2_outref: { transactionId: h32("62"), outputIndex: 1n },
              l2_owner: h28("63"),
              l2_value: l2Value,
              l1_address: addressData,
              l1_datum: "NoDatum",
            },
            signature: [h32("64"), h32("65")],
            validity: "WithdrawalIsValid",
          },
        },
        inclusion_time: 1_000n,
        witness: h28("67"),
        refund_address: addressData,
        refund_datum: "NoDatum",
      },
      WithdrawalOrderDatum,
    );
    const withdrawal = {
      outRef: h32("a2") + "#0",
      role: "withdrawal" as const,
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.withdrawalAddressHex,
          3_000_000n,
          [
            {
              policyId: withdrawalPolicyId,
              assetName: "ac",
              quantity: 1n,
            },
          ],
          withdrawalDatum,
        ),
      ),
    };
    const reserve = {
      outRef: h32("a3") + "#0",
      role: "reserve" as const,
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.reserveAddressHex,
          20_000_000n,
          [{ policyId: h28("68"), assetName: "02", quantity: 5n }],
          null,
        ),
      ),
    };
    expect(
      makeWatcherSettlementResourceFromProtocolUtxo(policy, payout),
    ).toMatchObject({ datumKind: "payout", identityAssetName: payoutAsset });
    expect(
      makeWatcherSettlementResourceFromProtocolUtxo(policy, withdrawal),
    ).toMatchObject({ datumKind: "withdrawal", identityAssetName: "ac" });
    expect(
      makeWatcherSettlementResourceFromProtocolUtxo(policy, reserve),
    ).toMatchObject({ datumKind: "no_datum", role: "reserve" });
  });

  it("absorbs a deposit into a new reserve output with its exact NFT burn and membership redeemers", () => {
    const depositAsset = "af";
    const depositOutRef = h32("cb") + "#0";
    const depositOutput = cardanoOutput(
      enterpriseScriptAddress(applied.depositSpend!),
      4_000_000n,
      [
        { policyId: h28("68"), assetName: "01", quantity: 2n },
        {
          policyId: depositPolicyId,
          assetName: depositAsset,
          quantity: 1n,
        },
      ],
      null,
    );
    const depositDurable: WatcherProtocolUtxo = {
      outRef: depositOutRef,
      role: "deposit",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(depositOutput),
    };
    const scenario = scenarioBootstrap([depositDurable]);
    const initial = bundle({
      policyOverride: scenario.policy,
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      sourceDurableStore: scenario.store,
      protocolUtxos: [depositDurable],
    });
    const initialState = accepted(null, initial, scenario.policy);
    const reserveOutput = cardanoOutput(
      policy.reserveAddressHex,
      4_000_000n,
      [{ policyId: h28("68"), assetName: "01", quantity: 2n }],
      null,
    );
    const mint = [
      {
        policyId: depositPolicyId,
        assetName: depositAsset,
        quantity: -1n,
      },
    ] as const;
    const body = transactionBody([depositOutRef], [reserveOutput], mint);
    const transactionHash = computeHash32(Buffer.from(body, "hex")).toString(
      "hex",
    );
    const reserveOutRef = `${transactionHash}#0`;
    const reserveDurable: WatcherProtocolUtxo = {
      outRef: reserveOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(reserveOutput),
    };
    const reserveResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      reserveDurable,
    )!;
    const reserveSubject = makeWatcherSettlementSubject({
      subjectId: "reserve-deposit-af",
      subjectKind: "reserve",
      status: "reserve_active",
      resourceOutRef: reserveOutRef,
      relatedSubjectId: depositAsset,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: null,
      failureCode: null,
    })!;
    const snapshot = makeWatcherSettlementSnapshot({
      resources: [reserveResource],
      subjects: [reserveSubject],
    })!;
    const membershipProof = {
      domain: "DepositsRootDomain" as const,
      root: ROOT,
      phas_root: h32("cc"),
      count: 1n,
      key: "aa",
      value: "bb",
      proof: [],
    };
    const depositSpend = Data.to(
      {
        input_index: 0n,
        output_index: 0n,
        hub_ref_input_index: 0n,
        settlement_ref_input_index: 1n,
        mint_redeemer_index: 1n,
        membership_proof: membershipProof,
        inclusion_proof_script_withdraw_redeemer_index: 3n,
      },
      DepositSpendRedeemer,
    );
    const depositBurn = Data.to(
      {
        BurnEventNFT: {
          nonce_asset_name: depositAsset,
          witness_unregistration_redeemer_index: 2n,
        },
      },
      UserEventMintRedeemer,
    );
    const witness = Data.to(
      { MintOrBurn: { targetPolicy: depositPolicyId } },
      UserEventWitnessPublishRedeemer,
    );
    const membership = Data.to(
      [
        membershipProof.phas_root,
        membershipProof.key,
        membershipProof.value,
        membershipProof.proof,
      ] as never,
      Data.Tuple([MerkleRootSchema, Data.Bytes(), Data.Bytes(), ProofSchema]),
    );
    const absorbed = bundle({
      policyOverride: scenario.policy,
      kind: "absorb_to_reserve",
      previousState: initialState,
      snapshot,
      inputs: [depositOutRef],
      outputHexes: [reserveOutput],
      mint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: depositSpend },
        { purpose: "mint", index: "0", cborHex: depositBurn },
        { purpose: "certificate", index: "0", cborHex: witness },
        { purpose: "withdrawal", index: "0", cborHex: membership },
      ],
      transition: {
        subjectId: depositAsset,
        consumedOutRefs: [depositOutRef],
        producedOutRefs: [reserveOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "deposit_spend",
            constructor: "DepositSpend",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "user_event_mint",
            constructor: "BurnEventNFT",
          },
          {
            purpose: "certificate",
            index: "0",
            schema: "event_witness",
            constructor: "MintOrBurn",
          },
          {
            purpose: "withdrawal",
            index: "0",
            schema: "membership_withdrawal",
            constructor: "MembershipProof",
          },
        ],
        exactMint: [
          {
            policyId: depositPolicyId,
            assetName: depositAsset,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [initial.restartBinding],
      protocolUtxos: [depositDurable, reserveDurable],
    });
    expect(accepted(initialState, absorbed, scenario.policy).snapshot).toEqual(
      snapshot,
    );
  });

  it("refunds an invalid withdrawal only to its exact datum-bound address and value", () => {
    const withdrawalAsset = "b0";
    const refundAddressData = {
      paymentCredential: {
        PublicKeyCredential: [h28("53")] as [string],
      },
      stakeCredential: null,
    };
    const withdrawalDatum = Data.to(
      {
        event: {
          id: { transactionId: h32("d4"), outputIndex: 0n },
          info: {
            body: {
              l2_outref: { transactionId: h32("d5"), outputIndex: 1n },
              l2_owner: h28("d6"),
              l2_value: new Map([["", new Map([["", 3_000_000n]])]]),
              l1_address: refundAddressData,
              l1_datum: "NoDatum",
            },
            signature: [h32("d7"), h32("d8")],
            validity: "WithdrawalIsValid",
          },
        },
        inclusion_time: 1_000n,
        witness: h28("d9"),
        refund_address: refundAddressData,
        refund_datum: "NoDatum",
      },
      WithdrawalOrderDatum,
    );
    const withdrawalOutRef = h32("da") + "#0";
    const withdrawalDurable: WatcherProtocolUtxo = {
      outRef: withdrawalOutRef,
      role: "withdrawal",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.withdrawalAddressHex,
          3_000_000n,
          [
            {
              policyId: withdrawalPolicyId,
              assetName: withdrawalAsset,
              quantity: 1n,
            },
          ],
          withdrawalDatum,
        ),
      ),
    };
    const withdrawalResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      withdrawalDurable,
    )!;
    const pendingSubject = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "withdrawal",
      status: "withdrawal_pending",
      resourceOutRef: withdrawalOutRef,
      relatedSubjectId: null,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: null,
      failureCode: null,
    })!;
    const initialSnapshot = makeWatcherSettlementSnapshot({
      resources: [withdrawalResource],
      subjects: [pendingSubject],
    })!;
    const scenario = scenarioBootstrap([withdrawalDurable]);
    const initial = bundle({
      policyOverride: scenario.policy,
      kind: "bootstrap",
      previousState: null,
      snapshot: initialSnapshot,
      protocolUtxos: [withdrawalDurable],
      sourceDurableStore: scenario.store,
    });
    const initialState = accepted(null, initial, scenario.policy);
    const refundOutput = cardanoOutput(
      enterprisePublicKeyAddress(h28("53")),
      3_000_000n,
      [],
      null,
    );
    const mint = [
      {
        policyId: withdrawalPolicyId,
        assetName: withdrawalAsset,
        quantity: -1n,
      },
    ] as const;
    const body = transactionBody([withdrawalOutRef], [refundOutput], mint);
    const transactionHash = computeHash32(Buffer.from(body, "hex")).toString(
      "hex",
    );
    const refundedSubject = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "withdrawal",
      status: "refunded",
      resourceOutRef: null,
      relatedSubjectId: null,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: transactionHash,
      failureCode: null,
    })!;
    const terminalSnapshot = makeWatcherSettlementSnapshot({
      resources: [],
      subjects: [refundedSubject],
    })!;
    const spend = Data.to(
      {
        input_index: 0n,
        output_index: 0n,
        hub_ref_input_index: 0n,
        settlement_ref_input_index: 1n,
        burn_redeemer_index: 1n,
        payout_mint_redeemer_index: 0n,
        membership_proof: {
          domain: "WithdrawalsRootDomain",
          root: ROOT,
          phas_root: h32("db"),
          count: 1n,
          key: "aa",
          value: "bb",
          proof: [],
        },
        inclusion_proof_script_withdraw_redeemer_index: 3n,
        purpose: {
          Refund: {
            validity_override: "IncorrectWithdrawalSignature",
          },
        },
      },
      WithdrawalSpendRedeemer,
    );
    const burn = Data.to(
      {
        BurnEventNFT: {
          nonce_asset_name: withdrawalAsset,
          witness_unregistration_redeemer_index: 2n,
        },
      },
      UserEventMintRedeemer,
    );
    const witness = Data.to(
      { MintOrBurn: { targetPolicy: withdrawalPolicyId } },
      UserEventWitnessPublishRedeemer,
    );
    const membership = Data.to(
      [h32("db"), "aa", "bb", []] as never,
      Data.Tuple([MerkleRootSchema, Data.Bytes(), Data.Bytes(), ProofSchema]),
    );
    const transition = {
      subjectId: withdrawalAsset,
      consumedOutRefs: [withdrawalOutRef],
      requiredRedeemers: [
        {
          purpose: "spend" as const,
          index: "0",
          schema: "withdrawal_spend" as const,
          constructor: "Refund" as const,
        },
        {
          purpose: "mint" as const,
          index: "0",
          schema: "user_event_mint" as const,
          constructor: "BurnEventNFT" as const,
        },
        {
          purpose: "certificate" as const,
          index: "0",
          schema: "event_witness" as const,
          constructor: "MintOrBurn" as const,
        },
        {
          purpose: "withdrawal" as const,
          index: "0",
          schema: "membership_withdrawal" as const,
          constructor: "MembershipProof" as const,
        },
      ],
      exactMint: [
        {
          policyId: withdrawalPolicyId,
          assetName: withdrawalAsset,
          quantity: "-1",
        },
      ],
    };
    const refunded = bundle({
      policyOverride: scenario.policy,
      kind: "refund_withdrawal",
      previousState: initialState,
      snapshot: terminalSnapshot,
      inputs: [withdrawalOutRef],
      outputHexes: [refundOutput],
      mint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spend },
        { purpose: "mint", index: "0", cborHex: burn },
        { purpose: "certificate", index: "0", cborHex: witness },
        { purpose: "withdrawal", index: "0", cborHex: membership },
      ],
      transition,
      restartBindings: [initial.restartBinding],
      protocolUtxos: [],
    });
    expect(accepted(initialState, refunded, scenario.policy).snapshot).toEqual(
      terminalSnapshot,
    );
    const underpaidOutput = cardanoOutput(
      enterprisePublicKeyAddress(h28("53")),
      2_000_000n,
      [],
      null,
    );
    const underpaidBody = transactionBody(
      [withdrawalOutRef],
      [underpaidOutput],
      mint,
    );
    const underpaidHash = computeHash32(
      Buffer.from(underpaidBody, "hex"),
    ).toString("hex");
    const underpaidSubject = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "withdrawal",
      status: "refunded",
      resourceOutRef: null,
      relatedSubjectId: null,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: underpaidHash,
      failureCode: null,
    })!;
    const underpaid = bundle({
      policyOverride: scenario.policy,
      kind: "refund_withdrawal",
      previousState: initialState,
      snapshot: makeWatcherSettlementSnapshot({
        resources: [],
        subjects: [underpaidSubject],
      })!,
      inputs: [withdrawalOutRef],
      outputHexes: [underpaidOutput],
      mint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spend },
        { purpose: "mint", index: "0", cborHex: burn },
        { purpose: "certificate", index: "0", cborHex: witness },
        { purpose: "withdrawal", index: "0", cborHex: membership },
      ],
      transition,
      restartBindings: [initial.restartBinding],
      protocolUtxos: [],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        initialState,
        underpaid.observation,
        underpaid.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["value_mismatch"],
    });
  });

  it("initializes and reserve-funds a payout with every production redeemer and exact target-value status", () => {
    const withdrawalAsset = "ae";
    const addressData = {
      paymentCredential: {
        PublicKeyCredential: [h28("52")] as [string],
      },
      stakeCredential: null,
    };
    const l2Value = new Map([["", new Map([["", 7_000_000n]])]]);
    const withdrawalDatum = Data.to(
      {
        event: {
          id: { transactionId: h32("c1"), outputIndex: 0n },
          info: {
            body: {
              l2_outref: { transactionId: h32("c2"), outputIndex: 1n },
              l2_owner: h28("c3"),
              l2_value: l2Value,
              l1_address: addressData,
              l1_datum: "NoDatum",
            },
            signature: [h32("c4"), h32("c5")],
            validity: "WithdrawalIsValid",
          },
        },
        inclusion_time: 1_000n,
        witness: h28("c6"),
        refund_address: addressData,
        refund_datum: "NoDatum",
      },
      WithdrawalOrderDatum,
    );
    const withdrawalOutRef = h32("c7") + "#0";
    const withdrawalDurable: WatcherProtocolUtxo = {
      outRef: withdrawalOutRef,
      role: "withdrawal",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.withdrawalAddressHex,
          2_000_000n,
          [
            {
              policyId: withdrawalPolicyId,
              assetName: withdrawalAsset,
              quantity: 1n,
            },
          ],
          withdrawalDatum,
        ),
      ),
    };
    const reserveOutRef = h32("c8") + "#0";
    const reserveDurable: WatcherProtocolUtxo = {
      outRef: reserveOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(
        cardanoOutput(policy.reserveAddressHex, 20_000_000n, [], null),
      ),
    };
    const withdrawalResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      withdrawalDurable,
    )!;
    const reserveResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      reserveDurable,
    )!;
    const withdrawalPending = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "withdrawal",
      status: "withdrawal_pending",
      resourceOutRef: withdrawalOutRef,
      relatedSubjectId: null,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: null,
      failureCode: null,
    })!;
    const reserveSubject = (outRef: string): WatcherSettlementSubject =>
      makeWatcherSettlementSubject({
        subjectId: "reserve-main",
        subjectKind: "reserve",
        status: "reserve_active",
        resourceOutRef: outRef,
        relatedSubjectId: null,
        resolutionTime: null,
        operatorVkey: null,
        attempt: "0",
        terminalTransactionHash: null,
        failureCode: null,
      })!;
    const sourceSnapshot = makeWatcherSettlementSnapshot({
      resources: [withdrawalResource, reserveResource],
      subjects: [withdrawalPending, reserveSubject(reserveOutRef)],
    })!;
    const scenario = scenarioBootstrap([withdrawalDurable, reserveDurable]);
    const bootstrapEvidence = bundle({
      policyOverride: scenario.policy,
      kind: "bootstrap",
      previousState: null,
      snapshot: sourceSnapshot,
      protocolUtxos: [withdrawalDurable, reserveDurable],
      sourceDurableStore: scenario.store,
    });
    const bootstrapState = accepted(null, bootstrapEvidence, scenario.policy);

    const payoutDatum = Data.to(
      {
        l2_value: l2Value,
        l1_address: addressData,
        l1_datum: "NoDatum",
      },
      PayoutDatum,
    );
    const initialPayoutOutput = cardanoOutput(
      policy.payoutAddressHex,
      2_000_000n,
      [
        {
          policyId: payoutPolicyId,
          assetName: withdrawalAsset,
          quantity: 1n,
        },
      ],
      payoutDatum,
    );
    const initializeMint = [
      {
        policyId: payoutPolicyId,
        assetName: withdrawalAsset,
        quantity: 1n,
      },
      {
        policyId: withdrawalPolicyId,
        assetName: withdrawalAsset,
        quantity: -1n,
      },
    ] as const;
    const initializeBody = transactionBody(
      [withdrawalOutRef],
      [initialPayoutOutput],
      initializeMint,
    );
    const initializeHash = computeHash32(
      Buffer.from(initializeBody, "hex"),
    ).toString("hex");
    const initialPayoutOutRef = `${initializeHash}#0`;
    const initialPayoutDurable: WatcherProtocolUtxo = {
      outRef: initialPayoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(initialPayoutOutput),
    };
    const initialPayoutResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      initialPayoutDurable,
    )!;
    const resolvedWithdrawal = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "withdrawal",
      status: "resolved",
      resourceOutRef: null,
      relatedSubjectId: null,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: initializeHash,
      failureCode: null,
    })!;
    const payoutInitializing = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "payout",
      status: "payout_initializing",
      resourceOutRef: initialPayoutOutRef,
      relatedSubjectId: withdrawalAsset,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: null,
      failureCode: null,
    })!;
    const initializedSnapshot = makeWatcherSettlementSnapshot({
      resources: [initialPayoutResource, reserveResource],
      subjects: [
        payoutInitializing,
        reserveSubject(reserveOutRef),
        resolvedWithdrawal,
      ],
    })!;
    const membershipProof = {
      domain: "WithdrawalsRootDomain" as const,
      root: ROOT,
      phas_root: h32("ca"),
      count: 1n,
      key: "aa",
      value: "bb",
      proof: [],
    };
    const initializeSpendRedeemer = Data.to(
      {
        input_index: 0n,
        output_index: 0n,
        hub_ref_input_index: 0n,
        settlement_ref_input_index: 1n,
        burn_redeemer_index: 2n,
        payout_mint_redeemer_index: 1n,
        membership_proof: membershipProof,
        inclusion_proof_script_withdraw_redeemer_index: 4n,
        purpose: "InitializePayout",
      },
      WithdrawalSpendRedeemer,
    );
    const payoutMintRedeemer = Data.to(
      {
        MintPayout: {
          withdrawal_utxo_out_ref: {
            transactionId: h32("c7"),
            outputIndex: 0n,
          },
          withdrawal_input_index: 0n,
          withdrawal_spend_redeemer_index: 0n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutMintRedeemer,
    );
    const withdrawalBurnRedeemer = Data.to(
      {
        BurnEventNFT: {
          nonce_asset_name: withdrawalAsset,
          witness_unregistration_redeemer_index: 3n,
        },
      },
      UserEventMintRedeemer,
    );
    const witnessRedeemer = Data.to(
      { MintOrBurn: { targetPolicy: withdrawalPolicyId } },
      UserEventWitnessPublishRedeemer,
    );
    const membershipRedeemer = Data.to(
      [
        membershipProof.phas_root,
        membershipProof.key,
        membershipProof.value,
        membershipProof.proof,
      ] as never,
      Data.Tuple([MerkleRootSchema, Data.Bytes(), Data.Bytes(), ProofSchema]),
    );
    const initialized = bundle({
      policyOverride: scenario.policy,
      kind: "initialize_payout",
      previousState: bootstrapState,
      snapshot: initializedSnapshot,
      inputs: [withdrawalOutRef],
      outputHexes: [initialPayoutOutput],
      mint: initializeMint,
      redeemers: [
        {
          purpose: "spend",
          index: "0",
          cborHex: initializeSpendRedeemer,
        },
        { purpose: "mint", index: "0", cborHex: payoutMintRedeemer },
        {
          purpose: "mint",
          index: "1",
          cborHex: withdrawalBurnRedeemer,
        },
        { purpose: "certificate", index: "0", cborHex: witnessRedeemer },
        {
          purpose: "withdrawal",
          index: "0",
          cborHex: membershipRedeemer,
        },
      ],
      transition: {
        subjectId: withdrawalAsset,
        relatedSubjectId: withdrawalAsset,
        consumedOutRefs: [withdrawalOutRef],
        producedOutRefs: [initialPayoutOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "withdrawal_spend",
            constructor: "InitializePayout",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "payout_mint",
            constructor: "MintPayout",
          },
          {
            purpose: "mint",
            index: "1",
            schema: "user_event_mint",
            constructor: "BurnEventNFT",
          },
          {
            purpose: "certificate",
            index: "0",
            schema: "event_witness",
            constructor: "MintOrBurn",
          },
          {
            purpose: "withdrawal",
            index: "0",
            schema: "membership_withdrawal",
            constructor: "MembershipProof",
          },
        ],
        exactMint: initializeMint.map((asset) => ({
          ...asset,
          quantity: asset.quantity.toString(),
        })),
      },
      restartBindings: [bootstrapEvidence.restartBinding],
      protocolUtxos: [initialPayoutDurable, reserveDurable],
    });
    const initializedState = accepted(
      bootstrapState,
      initialized,
      scenario.policy,
    );
    expect(
      initializedState.snapshot.subjects.find(
        ({ subjectKind }) => subjectKind === "payout",
      ),
    ).toMatchObject({ status: "payout_initializing" });

    const fundedPayoutOutput = cardanoOutput(
      policy.payoutAddressHex,
      7_000_000n,
      [
        {
          policyId: payoutPolicyId,
          assetName: withdrawalAsset,
          quantity: 1n,
        },
      ],
      payoutDatum,
    );
    const reserveChangeOutput = cardanoOutput(
      policy.reserveAddressHex,
      15_000_000n,
      [],
      null,
    );
    const fundingBody = transactionBody(
      [initialPayoutOutRef, reserveOutRef],
      [fundedPayoutOutput, reserveChangeOutput],
      [],
    );
    const fundingHash = computeHash32(Buffer.from(fundingBody, "hex")).toString(
      "hex",
    );
    const fundedPayoutOutRef = `${fundingHash}#0`;
    const reserveChangeOutRef = `${fundingHash}#1`;
    const fundedPayoutDurable: WatcherProtocolUtxo = {
      outRef: fundedPayoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(fundedPayoutOutput),
    };
    const reserveChangeDurable: WatcherProtocolUtxo = {
      outRef: reserveChangeOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(reserveChangeOutput),
    };
    const fundedPayoutResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      fundedPayoutDurable,
    )!;
    const reserveChangeResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      reserveChangeDurable,
    )!;
    const fundedPayoutSubject = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "payout",
      status: "payout_funded",
      resourceOutRef: fundedPayoutOutRef,
      relatedSubjectId: withdrawalAsset,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: null,
      failureCode: null,
    })!;
    const fundedSnapshot = makeWatcherSettlementSnapshot({
      resources: [fundedPayoutResource, reserveChangeResource],
      subjects: [
        fundedPayoutSubject,
        reserveSubject(reserveChangeOutRef),
        resolvedWithdrawal,
      ],
    })!;
    const addFundsRedeemer = Data.to(
      {
        AddFunds: {
          payout_input_index: 0n,
          payout_output_index: 0n,
          reserve_input_index: 1n,
          reserve_change_output_index: 1n,
          reserve_spend_redeemer_index: 1n,
          payout_spend_redeemer_index: 0n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutSpendRedeemer,
    );
    const reserveSpendRedeemer = Data.to(
      {
        reserve_input_index: 1n,
        payout_input_index: 0n,
        payout_spend_redeemer_index: 0n,
        hub_ref_input_index: 0n,
      },
      ReserveSpendRedeemer,
    );
    const funded = bundle({
      policyOverride: scenario.policy,
      kind: "fund_payout",
      previousState: initializedState,
      snapshot: fundedSnapshot,
      inputs: [initialPayoutOutRef, reserveOutRef],
      outputHexes: [fundedPayoutOutput, reserveChangeOutput],
      redeemers: [
        { purpose: "spend", index: "0", cborHex: addFundsRedeemer },
        { purpose: "spend", index: "1", cborHex: reserveSpendRedeemer },
      ],
      transition: {
        subjectId: withdrawalAsset,
        consumedOutRefs: [initialPayoutOutRef, reserveOutRef],
        producedOutRefs: [fundedPayoutOutRef, reserveChangeOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "payout_spend",
            constructor: "AddFunds",
          },
          {
            purpose: "spend",
            index: "1",
            schema: "reserve_spend",
            constructor: "Spend",
          },
        ],
      },
      restartBindings: [
        bootstrapEvidence.restartBinding,
        initialized.restartBinding,
      ],
      protocolUtxos: [fundedPayoutDurable, reserveChangeDurable],
    });
    const fundedState = accepted(initializedState, funded, scenario.policy);
    expect(fundedState.snapshot.subjects).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          subjectKind: "payout",
          status: "payout_funded",
        }),
        expect.objectContaining({
          subjectKind: "reserve",
          status: "reserve_active",
        }),
      ]),
    );

    const wrongSiblingIndexRedeemer = Data.to(
      {
        AddFunds: {
          payout_input_index: 0n,
          payout_output_index: 0n,
          reserve_input_index: 1n,
          reserve_change_output_index: 1n,
          reserve_spend_redeemer_index: 0n,
          payout_spend_redeemer_index: 0n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutSpendRedeemer,
    );
    const wrongSiblingIndex = bundle({
      policyOverride: scenario.policy,
      kind: "fund_payout",
      previousState: initializedState,
      snapshot: fundedSnapshot,
      inputs: [initialPayoutOutRef, reserveOutRef],
      outputHexes: [fundedPayoutOutput, reserveChangeOutput],
      redeemers: [
        {
          purpose: "spend",
          index: "0",
          cborHex: wrongSiblingIndexRedeemer,
        },
        { purpose: "spend", index: "1", cborHex: reserveSpendRedeemer },
      ],
      transition: funded.observation.transition,
      restartBindings: [
        bootstrapEvidence.restartBinding,
        initialized.restartBinding,
      ],
      protocolUtxos: [fundedPayoutDurable, reserveChangeDurable],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        initializedState,
        wrongSiblingIndex.observation,
        wrongSiblingIndex.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["redeemer_mismatch"],
    });

    const unbalancedReserveOutput = cardanoOutput(
      policy.reserveAddressHex,
      14_000_000n,
      [],
      null,
    );
    const unbalancedBody = transactionBody(
      [initialPayoutOutRef, reserveOutRef],
      [fundedPayoutOutput, unbalancedReserveOutput],
      [],
    );
    const unbalancedHash = computeHash32(
      Buffer.from(unbalancedBody, "hex"),
    ).toString("hex");
    const unbalancedPayoutOutRef = `${unbalancedHash}#0`;
    const unbalancedReserveOutRef = `${unbalancedHash}#1`;
    const unbalancedPayoutDurable: WatcherProtocolUtxo = {
      outRef: unbalancedPayoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(fundedPayoutOutput),
    };
    const unbalancedReserveDurable: WatcherProtocolUtxo = {
      outRef: unbalancedReserveOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(unbalancedReserveOutput),
    };
    const unbalancedSnapshot = makeWatcherSettlementSnapshot({
      resources: [
        makeWatcherSettlementResourceFromProtocolUtxo(
          policy,
          unbalancedPayoutDurable,
        )!,
        makeWatcherSettlementResourceFromProtocolUtxo(
          policy,
          unbalancedReserveDurable,
        )!,
      ],
      subjects: [
        makeWatcherSettlementSubject({
          subjectId: fundedPayoutSubject.subjectId,
          subjectKind: fundedPayoutSubject.subjectKind,
          status: fundedPayoutSubject.status,
          resourceOutRef: unbalancedPayoutOutRef,
          relatedSubjectId: fundedPayoutSubject.relatedSubjectId,
          resolutionTime: fundedPayoutSubject.resolutionTime,
          operatorVkey: fundedPayoutSubject.operatorVkey,
          attempt: fundedPayoutSubject.attempt,
          terminalTransactionHash: fundedPayoutSubject.terminalTransactionHash,
          failureCode: fundedPayoutSubject.failureCode,
        })!,
        reserveSubject(unbalancedReserveOutRef),
        resolvedWithdrawal,
      ],
    })!;
    const unbalanced = bundle({
      policyOverride: scenario.policy,
      kind: "fund_payout",
      previousState: initializedState,
      snapshot: unbalancedSnapshot,
      inputs: [initialPayoutOutRef, reserveOutRef],
      outputHexes: [fundedPayoutOutput, unbalancedReserveOutput],
      redeemers: [
        { purpose: "spend", index: "0", cborHex: addFundsRedeemer },
        { purpose: "spend", index: "1", cborHex: reserveSpendRedeemer },
      ],
      transition: {
        ...funded.observation.transition,
        producedOutRefs: [unbalancedPayoutOutRef, unbalancedReserveOutRef],
      },
      restartBindings: [
        bootstrapEvidence.restartBinding,
        initialized.restartBinding,
      ],
      protocolUtxos: [unbalancedPayoutDurable, unbalancedReserveDurable],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        initializedState,
        unbalanced.observation,
        unbalanced.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["value_mismatch"],
    });

    const destinationOutput = cardanoOutput(
      enterprisePublicKeyAddress(h28("52")),
      7_000_000n,
      [],
      null,
    );
    const concludeMint = [
      {
        policyId: payoutPolicyId,
        assetName: withdrawalAsset,
        quantity: -1n,
      },
    ] as const;
    const concludeBody = transactionBody(
      [fundedPayoutOutRef],
      [destinationOutput],
      concludeMint,
    );
    const concludeHash = computeHash32(
      Buffer.from(concludeBody, "hex"),
    ).toString("hex");
    const paidPayoutSubject = makeWatcherSettlementSubject({
      subjectId: withdrawalAsset,
      subjectKind: "payout",
      status: "paid",
      resourceOutRef: null,
      relatedSubjectId: withdrawalAsset,
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: concludeHash,
      failureCode: null,
    })!;
    const terminalSnapshot = makeWatcherSettlementSnapshot({
      resources: [reserveChangeResource],
      subjects: [
        paidPayoutSubject,
        reserveSubject(reserveChangeOutRef),
        resolvedWithdrawal,
      ],
    })!;
    const concludeSpendRedeemer = Data.to(
      {
        ConcludeWithdrawal: {
          payout_input_index: 0n,
          l1_output_index: 0n,
          burn_redeemer_index: 1n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutSpendRedeemer,
    );
    const concludeBurnRedeemer = Data.to(
      {
        BurnPayout: {
          payout_input_index: 0n,
          payout_asset_name: withdrawalAsset,
          payout_spend_redeemer_index: 0n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutMintRedeemer,
    );
    const concluded = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: fundedState,
      snapshot: terminalSnapshot,
      inputs: [fundedPayoutOutRef],
      outputHexes: [destinationOutput],
      mint: concludeMint,
      redeemers: [
        {
          purpose: "spend",
          index: "0",
          cborHex: concludeSpendRedeemer,
        },
        { purpose: "mint", index: "0", cborHex: concludeBurnRedeemer },
      ],
      transition: {
        subjectId: withdrawalAsset,
        consumedOutRefs: [fundedPayoutOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "payout_spend",
            constructor: "ConcludeWithdrawal",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "payout_mint",
            constructor: "BurnPayout",
          },
        ],
        exactMint: [
          {
            policyId: payoutPolicyId,
            assetName: withdrawalAsset,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [
        bootstrapEvidence.restartBinding,
        initialized.restartBinding,
        funded.restartBinding,
      ],
      protocolUtxos: [reserveChangeDurable],
    });
    expect(
      accepted(fundedState, concluded, scenario.policy).snapshot.subjects,
    ).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          subjectKind: "payout",
          status: "paid",
        }),
        expect.objectContaining({
          subjectKind: "withdrawal",
          status: "resolved",
        }),
      ]),
    );
  });

  it("reconciles an exactly funded payout through its real burn and terminal L1 payment", () => {
    const addressData = {
      paymentCredential: {
        PublicKeyCredential: [h28("52")] as [string],
      },
      stakeCredential: null,
    };
    const payoutAsset = "ad";
    const l2Value = new Map([["", new Map([["", 7_000_000n]])]]);
    const payoutDatum = Data.to(
      {
        l2_value: l2Value,
        l1_address: addressData,
        l1_datum: "NoDatum",
      },
      PayoutDatum,
    );
    const payoutOutputHex = cardanoOutput(
      policy.payoutAddressHex,
      7_000_000n,
      [
        {
          policyId: payoutPolicyId,
          assetName: payoutAsset,
          quantity: 1n,
        },
      ],
      payoutDatum,
    );
    const payoutOutRef = h32("b1") + "#0";
    const payoutDurable: WatcherProtocolUtxo = {
      outRef: payoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(payoutOutputHex),
    };
    const payoutResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      payoutDurable,
    )!;
    const payoutSubject = makeWatcherSettlementSubject({
      subjectId: payoutAsset,
      subjectKind: "payout",
      status: "payout_funded",
      resourceOutRef: payoutOutRef,
      relatedSubjectId: "withdrawal-ad",
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: null,
      failureCode: null,
    })!;
    const fundedSnapshot = makeWatcherSettlementSnapshot({
      resources: [payoutResource],
      subjects: [payoutSubject],
    })!;
    const unrelatedSpentProtocolUtxo = {
      outRef: `${h32("b4")}#1`,
      role: "state_queue" as const,
      chainPointId: h32("00"),
      output: makeWatcherDurablePayload(stateQueueOutput()),
      spentAtChainPointId: h32("00"),
    };
    const scenario = scenarioBootstrap(
      [payoutDurable],
      [unrelatedSpentProtocolUtxo],
    );
    const initial = bundle({
      policyOverride: scenario.policy,
      kind: "bootstrap",
      previousState: null,
      snapshot: fundedSnapshot,
      protocolUtxos: [payoutDurable],
      sourceDurableStore: scenario.store,
    });
    const fundedState = accepted(null, initial, scenario.policy);
    const destinationOutput = cardanoOutput(
      enterprisePublicKeyAddress(h28("52")),
      7_000_000n,
      [],
      null,
    );
    const burnMint = [
      {
        policyId: payoutPolicyId,
        assetName: payoutAsset,
        quantity: -1n,
      },
    ] as const;
    const bodyHex = transactionBody(
      [payoutOutRef],
      [destinationOutput],
      burnMint,
    );
    const transactionHash = computeHash32(Buffer.from(bodyHex, "hex")).toString(
      "hex",
    );
    const paidSubject = makeWatcherSettlementSubject({
      subjectId: payoutAsset,
      subjectKind: "payout",
      status: "paid",
      resourceOutRef: null,
      relatedSubjectId: "withdrawal-ad",
      resolutionTime: null,
      operatorVkey: null,
      attempt: "0",
      terminalTransactionHash: transactionHash,
      failureCode: null,
    })!;
    const terminalSnapshot = makeWatcherSettlementSnapshot({
      resources: [],
      subjects: [paidSubject],
    })!;
    const spendRedeemer = Data.to(
      {
        ConcludeWithdrawal: {
          payout_input_index: 0n,
          l1_output_index: 0n,
          burn_redeemer_index: 1n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutSpendRedeemer,
    );
    const burnRedeemer = Data.to(
      {
        BurnPayout: {
          payout_input_index: 0n,
          payout_asset_name: payoutAsset,
          payout_spend_redeemer_index: 0n,
          hub_ref_input_index: 0n,
        },
      },
      PayoutMintRedeemer,
    );
    const terminal = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: fundedState,
      snapshot: terminalSnapshot,
      inputs: [payoutOutRef],
      outputHexes: [destinationOutput],
      mint: burnMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spendRedeemer },
        { purpose: "mint", index: "0", cborHex: burnRedeemer },
      ],
      transition: {
        subjectId: payoutAsset,
        consumedOutRefs: [payoutOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "payout_spend",
            constructor: "ConcludeWithdrawal",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "payout_mint",
            constructor: "BurnPayout",
          },
        ],
        exactMint: [
          {
            policyId: payoutPolicyId,
            assetName: payoutAsset,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [initial.restartBinding],
      protocolUtxos: [],
    });
    const underpaidOutput = cardanoOutput(
      enterprisePublicKeyAddress(h28("52")),
      6_000_000n,
      [],
      null,
    );
    const underpaidBody = transactionBody(
      [payoutOutRef],
      [underpaidOutput],
      burnMint,
    );
    const underpaidHash = computeHash32(
      Buffer.from(underpaidBody, "hex"),
    ).toString("hex");
    const underpaidSubject = makeWatcherSettlementSubject({
      subjectId: paidSubject.subjectId,
      subjectKind: paidSubject.subjectKind,
      status: paidSubject.status,
      resourceOutRef: paidSubject.resourceOutRef,
      relatedSubjectId: paidSubject.relatedSubjectId,
      resolutionTime: paidSubject.resolutionTime,
      operatorVkey: paidSubject.operatorVkey,
      attempt: paidSubject.attempt,
      terminalTransactionHash: underpaidHash,
      failureCode: paidSubject.failureCode,
    })!;
    const underpaidSnapshot = makeWatcherSettlementSnapshot({
      resources: [],
      subjects: [underpaidSubject],
    })!;
    const underpaid = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: fundedState,
      snapshot: underpaidSnapshot,
      inputs: [payoutOutRef],
      outputHexes: [underpaidOutput],
      mint: burnMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spendRedeemer },
        { purpose: "mint", index: "0", cborHex: burnRedeemer },
      ],
      transition: {
        subjectId: payoutAsset,
        consumedOutRefs: [payoutOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "payout_spend",
            constructor: "ConcludeWithdrawal",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "payout_mint",
            constructor: "BurnPayout",
          },
        ],
        exactMint: [
          {
            policyId: payoutPolicyId,
            assetName: payoutAsset,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [initial.restartBinding],
      protocolUtxos: [],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        fundedState,
        underpaid.observation,
        underpaid.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["value_mismatch"],
    });
    expect(terminal.observation.transactionHash).toBe(transactionHash);
    const terminalState = accepted(fundedState, terminal, scenario.policy);
    expect(terminalState.snapshot.subjects[0]).toMatchObject({
      status: "paid",
      terminalTransactionHash: transactionHash,
    });

    const rollbackProviderA = rollbackProvider("provider-a", "77");
    const rollbackProviderB = rollbackProvider("provider-b", "78");
    const fundedHead = fundedState.activeHistory.at(-1)!.observation;
    const oldRawA = structuredClone(terminal.context.l1Observation) as Mutable;
    oldRawA.providerId = rollbackProviderA.providerId;
    oldRawA.chainPoint = {
      blockHash: h32("e1"),
      parentBlockHash: fundedHead.blockHash,
      slot: (BigInt(fundedHead.slot) + 1n).toString(),
      blockNo: (BigInt(fundedHead.blockNo) + 1n).toString(),
      depth: "1",
    };
    const oldRawB = structuredClone(oldRawA) as Mutable;
    oldRawB.providerId = rollbackProviderB.providerId;
    const replacementRawA = structuredClone(oldRawA) as Mutable;
    replacementRawA.chainPoint = {
      blockHash: h32("e2"),
      parentBlockHash: fundedHead.blockHash,
      slot: (BigInt(fundedHead.slot) + 2n).toString(),
      blockNo: (BigInt(fundedHead.blockNo) + 1n).toString(),
      depth: "1",
    };
    const replacementRawB = structuredClone(replacementRawA) as Mutable;
    replacementRawB.providerId = rollbackProviderB.providerId;
    const oldA = normalizeWatcherL1Block(rollbackProviderA, oldRawA);
    const oldB = normalizeWatcherL1Block(rollbackProviderB, oldRawB);
    const replacementA = normalizeWatcherL1Block(
      rollbackProviderA,
      replacementRawA,
    );
    const replacementB = normalizeWatcherL1Block(
      rollbackProviderB,
      replacementRawB,
    );
    const rollbackBlocks = [oldA, oldB, replacementA, replacementB];
    const rollbackChainPoints = [
      ...initial.store.chainPoints,
      ...persistedChainPoints(rollbackBlocks),
    ];
    const terminalJournal = journalWatcherProtocolUtxoTransition({
      sourceStore: initial.store,
      nextChainPoints: rollbackChainPoints,
      nextProtocolUtxos: initial.store.protocolUtxos.filter(
        ({ role }) => role === "hub_oracle",
      ),
      spentAtChainPointId: oldA.chainPoint.chainPointId,
    });
    const rollbackSourceStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: (BigInt(initial.store.revision) + 1n).toString(),
      records: {
        l1Observations: [
          ...initial.store.l1Observations,
          ...rollbackBlocks.map(persistedObservation),
        ],
        chainPoints: rollbackChainPoints,
        protocolUtxos: terminalJournal.protocolUtxos,
        spentProtocolUtxos: terminalJournal.spentProtocolUtxos,
        daProofInputs: initial.store.daProofInputs,
        reconstructedStates: initial.store.reconstructedStates,
        decisions: initial.store.decisions,
        faults: initial.store.faults,
        submissions: initial.store.submissions,
        confirmations: initial.store.confirmations,
        retries: initial.store.retries,
        deadlines: initial.store.deadlines,
        correctionResults: initial.store.correctionResults,
      },
    });
    const terminalForRollback = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: fundedState,
      snapshot: terminalSnapshot,
      inputs: [payoutOutRef],
      outputHexes: [destinationOutput],
      mint: burnMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spendRedeemer },
        { purpose: "mint", index: "0", cborHex: burnRedeemer },
      ],
      transition: terminal.observation.transition,
      restartBindings: [initial.restartBinding],
      authenticatedProvider: rollbackProviderA,
      l1Observation: oldRawA,
      durableStore: rollbackSourceStore,
    });
    const terminalRollbackState = accepted(
      fundedState,
      terminalForRollback,
      scenario.policy,
    );
    const omittedArchiveStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: rollbackSourceStore.revision,
      records: {
        ...rollbackSourceStore,
        spentProtocolUtxos: [],
      },
    });
    const omittedArchive = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: fundedState,
      snapshot: terminalSnapshot,
      inputs: [payoutOutRef],
      outputHexes: [destinationOutput],
      mint: burnMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spendRedeemer },
        { purpose: "mint", index: "0", cborHex: burnRedeemer },
      ],
      transition: terminal.observation.transition,
      restartBindings: [initial.restartBinding],
      authenticatedProvider: rollbackProviderA,
      l1Observation: oldRawA,
      durableStore: omittedArchiveStore,
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        fundedState,
        omittedArchive.observation,
        omittedArchive.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    const substitutedArchiveEntry = {
      ...terminalJournal.spentProtocolUtxos[0]!,
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.payoutAddressHex,
          6_000_000n,
          [
            {
              policyId: payoutPolicyId,
              assetName: payoutAsset,
              quantity: 1n,
            },
          ],
          payoutDatum,
        ),
      ),
    };
    const substitutedArchiveStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: rollbackSourceStore.revision,
      records: {
        ...rollbackSourceStore,
        spentProtocolUtxos: [substitutedArchiveEntry],
      },
    });
    const substitutedArchive = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: fundedState,
      snapshot: terminalSnapshot,
      inputs: [payoutOutRef],
      outputHexes: [destinationOutput],
      mint: burnMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spendRedeemer },
        { purpose: "mint", index: "0", cborHex: burnRedeemer },
      ],
      transition: terminal.observation.transition,
      restartBindings: [initial.restartBinding],
      authenticatedProvider: rollbackProviderA,
      l1Observation: oldRawA,
      durableStore: substitutedArchiveStore,
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        fundedState,
        substitutedArchive.observation,
        substitutedArchive.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    const priorFinalityState = terminalForRollback.finalityState!;
    const replacementConsistency = evaluateWatcherMultiProviderConsistency(
      externalSource,
      [replacementA, replacementB],
    );
    const replacementFinality = evaluateWatcherFinality(
      finalityPolicy,
      priorFinalityState,
      replacementConsistency,
    );
    expect(replacementFinality.action).toBe("rewind_pending");
    const rollbackBootstrap = makeWatcherRollbackBootstrapState(
      finalityPolicy,
      rollbackSourceStore,
      priorFinalityState,
    )!;
    const rollbackVerificationContext = {
      policy: finalityPolicy,
      sourceStore: rollbackSourceStore,
      previousFinalityState: priorFinalityState,
      consistency: replacementConsistency,
      finalityResult: replacementFinality,
      previousRollbackState: rollbackBootstrap,
      rollbackBootstrapState: rollbackBootstrap,
    };
    const authoritativeRollback = evaluateWatcherRollback(
      finalityPolicy,
      rollbackSourceStore,
      priorFinalityState,
      replacementConsistency,
      replacementFinality,
      rollbackBootstrap,
      rollbackBootstrap,
    );
    expect(
      authoritativeRollback,
      JSON.stringify(authoritativeRollback),
    ).toMatchObject({
      action: "apply_rewind",
      protocolDecision: "resume_pending",
    });
    expect(authoritativeRollback.nextStore).not.toBeNull();
    const retainedPayout = initial.store.protocolUtxos.find(
      ({ outRef }) => outRef === payoutOutRef,
    )!;
    const restoredStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: authoritativeRollback.nextStore!.revision,
      records: {
        ...authoritativeRollback.nextStore!,
        protocolUtxos: [
          ...authoritativeRollback.nextStore!.protocolUtxos.filter(
            ({ role }) => role === "hub_oracle",
          ),
          retainedPayout,
        ],
      },
    });
    const rollbackEvidence = bundle({
      policyOverride: scenario.policy,
      kind: "rollback",
      previousState: terminalRollbackState,
      snapshot: fundedSnapshot,
      restartBindings: [
        initial.restartBinding,
        terminalForRollback.restartBinding,
      ],
      authenticatedProvider: rollbackProviderA,
      l1Observation: replacementRawA,
      durableStore: restoredStore,
      rollbackAuthority: {
        result: authoritativeRollback,
        context: rollbackVerificationContext,
      },
    });
    const restoredState = accepted(
      terminalRollbackState,
      rollbackEvidence,
      scenario.policy,
    );
    expect(restoredState.snapshot).toEqual(fundedSnapshot);
    expect(restoredStore.spentProtocolUtxos).toContainEqual(
      unrelatedSpentProtocolUtxo,
    );
    const rollbackBinding = {
      resultDigest: authoritativeRollback.resultDigest,
      context: rollbackVerificationContext,
    };
    expect(
      parseWatcherSettlementIndexerState(
        JSON.parse(JSON.stringify(restoredState)),
        scenario.policy,
        [
          initial.restartBinding,
          terminalForRollback.restartBinding,
          rollbackEvidence.restartBinding,
        ],
        [rollbackBinding],
      ),
    ).toEqual(restoredState);

    const omittedRestoredStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: authoritativeRollback.nextStore!.revision,
      records: {
        ...authoritativeRollback.nextStore!,
        protocolUtxos: authoritativeRollback.nextStore!.protocolUtxos.filter(
          ({ role }) => role === "hub_oracle",
        ),
      },
    });
    const omittedRestoration = bundle({
      policyOverride: scenario.policy,
      kind: "rollback",
      previousState: terminalRollbackState,
      snapshot: fundedSnapshot,
      restartBindings: [
        initial.restartBinding,
        terminalForRollback.restartBinding,
      ],
      authenticatedProvider: rollbackProviderA,
      l1Observation: replacementRawA,
      durableStore: omittedRestoredStore,
      rollbackAuthority: {
        result: authoritativeRollback,
        context: rollbackVerificationContext,
      },
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        terminalRollbackState,
        omittedRestoration.observation,
        omittedRestoration.context,
      ),
    ).toMatchObject({ action: "reject" });

    const substitutedPayout: WatcherProtocolUtxo = {
      ...retainedPayout,
      output: makeWatcherDurablePayload(
        cardanoOutput(
          policy.payoutAddressHex,
          6_000_000n,
          [
            {
              policyId: payoutPolicyId,
              assetName: payoutAsset,
              quantity: 1n,
            },
          ],
          payoutDatum,
        ),
      ),
    };
    const substitutedStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: authoritativeRollback.nextStore!.revision,
      records: {
        ...authoritativeRollback.nextStore!,
        protocolUtxos: [
          ...authoritativeRollback.nextStore!.protocolUtxos.filter(
            ({ role }) => role === "hub_oracle",
          ),
          substitutedPayout,
        ],
      },
    });
    const substitutedRestoration = bundle({
      policyOverride: scenario.policy,
      kind: "rollback",
      previousState: terminalRollbackState,
      snapshot: fundedSnapshot,
      restartBindings: [
        initial.restartBinding,
        terminalForRollback.restartBinding,
      ],
      authenticatedProvider: rollbackProviderA,
      l1Observation: replacementRawA,
      durableStore: substitutedStore,
      rollbackAuthority: {
        result: authoritativeRollback,
        context: rollbackVerificationContext,
      },
    });
    expect(
      evaluateWatcherSettlementIndexer(
        scenario.policy,
        terminalRollbackState,
        substitutedRestoration.observation,
        substitutedRestoration.context,
      ),
    ).toMatchObject({ action: "reject" });

    const reIncludedJournal = journalWatcherProtocolUtxoTransition({
      sourceStore: restoredStore,
      nextChainPoints: restoredStore.chainPoints,
      nextProtocolUtxos: restoredStore.protocolUtxos.filter(
        ({ role }) => role === "hub_oracle",
      ),
      spentAtChainPointId: replacementA.chainPoint.chainPointId,
    });
    const reIncludedStore = makeWatcherDurableStore({
      deploymentMarker: scenario.policy.deploymentMarker,
      revision: (BigInt(restoredStore.revision) + 1n).toString(),
      records: {
        ...restoredStore,
        protocolUtxos: reIncludedJournal.protocolUtxos,
        spentProtocolUtxos: reIncludedJournal.spentProtocolUtxos,
      },
    });
    const reIncludedTerminal = bundle({
      policyOverride: scenario.policy,
      kind: "conclude_payout",
      previousState: restoredState,
      snapshot: terminalSnapshot,
      inputs: [payoutOutRef],
      outputHexes: [destinationOutput],
      mint: burnMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: spendRedeemer },
        { purpose: "mint", index: "0", cborHex: burnRedeemer },
      ],
      transition: terminal.observation.transition,
      restartBindings: [
        initial.restartBinding,
        terminalForRollback.restartBinding,
        rollbackEvidence.restartBinding,
      ],
      restartRollbackBindings: [rollbackBinding],
      authenticatedProvider: rollbackProviderA,
      l1Observation: replacementRawA,
      durableStore: reIncludedStore,
    });
    expect(
      accepted(restoredState, reIncludedTerminal, scenario.policy).snapshot,
    ).toEqual(terminalSnapshot);
  });

  it("rejects mint/redeemer mutation and omitted topology", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, initial);
    const bad = bundle({
      kind: "spawn_settlement",
      previousState: state,
      snapshot: emptySnapshot(),
      transition: {
        subjectId: settlementAsset,
        exactMint: [
          {
            policyId: settlementPolicyId,
            assetName: settlementAsset,
            quantity: "1",
          },
        ],
      },
      restartBindings: [initial.restartBinding],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        state,
        bad.observation,
        bad.context,
      ).action,
    ).toBe("reject");
  });

  it("rejects a self-consistent W03 subset that drops retained durable lineage", () => {
    const spawned = spawnSequence();
    const source = spawned.spawnEvidence.context
      .sourceDurableStore as WatcherDurableStore;
    const currentObservationId =
      spawned.spawnEvidence.observation.sourceObservationDigest;
    const subsetStore = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: spawned.spawnEvidence.store.revision,
      records: {
        l1Observations: spawned.spawnEvidence.store.l1Observations.filter(
          ({ observationId }) => observationId === currentObservationId,
        ),
        chainPoints: spawned.spawnEvidence.store.chainPoints.filter(
          ({ chainPointId }) =>
            chainPointId === spawned.spawnEvidence.observation.chainPointId ||
            chainPointId === hubReferenceUtxo.chainPointId,
        ),
        protocolUtxos: spawned.spawnEvidence.store.protocolUtxos,
        daProofInputs: spawned.spawnEvidence.store.daProofInputs,
        reconstructedStates: spawned.spawnEvidence.store.reconstructedStates,
        decisions: spawned.spawnEvidence.store.decisions,
        faults: spawned.spawnEvidence.store.faults,
        submissions: spawned.spawnEvidence.store.submissions,
        confirmations: spawned.spawnEvidence.store.confirmations,
        retries: spawned.spawnEvidence.store.retries,
        deadlines: spawned.spawnEvidence.store.deadlines,
        correctionResults: spawned.spawnEvidence.store.correctionResults,
      },
    });
    expect(source.l1Observations.length).toBeGreaterThan(0);
    const {
      schemaVersion: _schemaVersion,
      observationDigest: _observationDigest,
      ...observationFields
    } = spawned.spawnEvidence.observation;
    const forgedObservation = makeWatcherSettlementObservation({
      ...observationFields,
      durableStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStore(subsetStore),
      ),
    })!;
    const forgedContext = {
      ...spawned.spawnEvidence.context,
      durableStore: subsetStore,
    };
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        accepted(null, spawned.bootstrapEvidence),
        forgedObservation,
        forgedContext,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["durable_evidence_mismatch"],
    });
  });

  it("indexes a real resolution claim, retries it deterministically, then marks it stuck at the configured bound", () => {
    const spawned = spawnSequence();
    const claimedOutputHex = settlementOutput(true);
    const activeNodeOutputHex = cardanoOutput(
      enterpriseScriptAddress(applied.activeOperatorsSpend!),
      2_000_000n,
      [],
      null,
    );
    const attachInputs = [spawned.outRef, h32("b8") + "#0"];
    const attachBodyHex = transactionBody(
      attachInputs,
      [claimedOutputHex, activeNodeOutputHex],
      [],
    );
    const attachTransactionHash = computeHash32(
      Buffer.from(attachBodyHex, "hex"),
    ).toString("hex");
    const claimedOutRef = `${attachTransactionHash}#0`;
    const claimedDurable = protocolUtxo(claimedOutRef, claimedOutputHex);
    const claimedResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      claimedDurable,
    )!;
    const claimedSnapshot = makeWatcherSettlementSnapshot({
      resources: [claimedResource],
      subjects: [settlementSubject(claimedOutRef, "claimed")],
    })!;
    const attachRedeemer = Data.to(
      {
        AttachResolutionClaim: {
          settlement_input_index: 0n,
          settlement_output_index: 0n,
          hub_ref_input_index: 0n,
          active_operators_node_input_index: 1n,
          active_operators_redeemer_index: 1n,
          operator,
          scheduler_ref_input_index: 1n,
        },
      },
      SettlementSpendRedeemer,
    );
    const bondHoldRedeemer = Data.to(
      {
        UpdateBondHoldNewSettlement: {
          active_operator: operator,
          active_node_input_index: 1n,
          active_node_output_index: 1n,
          hub_oracle_ref_input_index: 0n,
          settlement_input_index: 0n,
          settlement_redeemer_index: 0n,
          resolution_time: 2_000n,
        },
      },
      ActiveOperatorSpendRedeemer,
    );
    const attachEvidence = bundle({
      kind: "attach_resolution_claim",
      previousState: spawned.spawnState,
      snapshot: claimedSnapshot,
      inputs: attachInputs,
      outputHexes: [claimedOutputHex, activeNodeOutputHex],
      redeemers: [
        { purpose: "spend", index: "0", cborHex: attachRedeemer },
        { purpose: "spend", index: "1", cborHex: bondHoldRedeemer },
      ],
      transition: {
        subjectId: settlementAsset,
        consumedOutRefs: [spawned.outRef],
        producedOutRefs: [claimedOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "settlement_spend",
            constructor: "AttachResolutionClaim",
          },
          {
            purpose: "spend",
            index: "1",
            schema: "active_operator_spend",
            constructor: "UpdateBondHoldNewSettlement",
          },
        ],
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
      ],
      protocolUtxos: [claimedDurable],
    });
    const claimedState = accepted(spawned.spawnState, attachEvidence);
    const retryOneSnapshot = makeWatcherSettlementSnapshot({
      resources: [claimedResource],
      subjects: [
        settlementSubject(claimedOutRef, "retrying", "1", "provider_timeout"),
      ],
    })!;
    const retryOne = bundle({
      kind: "retry",
      previousState: claimedState,
      snapshot: retryOneSnapshot,
      transition: {
        subjectId: settlementAsset,
        failureCode: "provider_timeout",
        retryAttempt: "1",
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
        attachEvidence.restartBinding,
      ],
      protocolUtxos: [claimedDurable],
    });
    const retryOneState = accepted(claimedState, retryOne);
    const retryTwoSnapshot = makeWatcherSettlementSnapshot({
      resources: [claimedResource],
      subjects: [
        settlementSubject(claimedOutRef, "retrying", "2", "provider_timeout"),
      ],
    })!;
    const retryTwo = bundle({
      kind: "retry",
      previousState: retryOneState,
      snapshot: retryTwoSnapshot,
      transition: {
        subjectId: settlementAsset,
        failureCode: "provider_timeout",
        retryAttempt: "2",
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
        attachEvidence.restartBinding,
        retryOne.restartBinding,
      ],
      protocolUtxos: [claimedDurable],
    });
    const retryTwoState = accepted(retryOneState, retryTwo);
    const stuckSnapshot = makeWatcherSettlementSnapshot({
      resources: [claimedResource],
      subjects: [
        settlementSubject(claimedOutRef, "stuck", "2", "stuck_retry_exhausted"),
      ],
    })!;
    const stuck = bundle({
      kind: "mark_stuck",
      previousState: retryTwoState,
      snapshot: stuckSnapshot,
      transition: {
        subjectId: settlementAsset,
        failureCode: "stuck_retry_exhausted",
        retryAttempt: "2",
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
        attachEvidence.restartBinding,
        retryOne.restartBinding,
        retryTwo.restartBinding,
      ],
      protocolUtxos: [claimedDurable],
    });
    const stuckState = accepted(retryTwoState, stuck);
    expect(stuckState.snapshot.subjects[0]).toMatchObject({
      status: "stuck",
      attempt: "2",
      failureCode: "stuck_retry_exhausted",
    });
    const invalidSnapshot = makeWatcherSettlementSnapshot({
      resources: [claimedResource],
      subjects: [
        settlementSubject(
          claimedOutRef,
          "invalid",
          "2",
          "invalid_retry_terminal",
        ),
      ],
    })!;
    const invalid = bundle({
      kind: "mark_invalid",
      previousState: stuckState,
      snapshot: invalidSnapshot,
      transition: {
        subjectId: settlementAsset,
        failureCode: "invalid_retry_terminal",
        retryAttempt: "2",
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
        attachEvidence.restartBinding,
        retryOne.restartBinding,
        retryTwo.restartBinding,
        stuck.restartBinding,
      ],
      protocolUtxos: [claimedDurable],
    });
    expect(accepted(stuckState, invalid).snapshot.subjects[0]).toMatchObject({
      status: "invalid",
      failureCode: "invalid_retry_terminal",
    });

    const activeOperatorOutRef = h32("b8") + "#0";
    const disprovedOutputHex = settlementOutput();
    const activeOperatorOutputHex = cardanoOutput(
      enterpriseScriptAddress(h28("46")),
      5_000_000n,
      [
        {
          policyId: activeOperatorPolicyId,
          assetName: operator,
          quantity: 1n,
        },
      ],
      null,
    );
    const disproveMint = [
      {
        policyId: activeOperatorPolicyId,
        assetName: operator,
        quantity: -1n,
      },
    ] as const;
    const disproveBodyHex = transactionBody(
      [claimedOutRef, activeOperatorOutRef],
      [disprovedOutputHex, activeOperatorOutputHex],
      disproveMint,
    );
    const disproveTransactionHash = computeHash32(
      Buffer.from(disproveBodyHex, "hex"),
    ).toString("hex");
    const disprovedOutRef = `${disproveTransactionHash}#0`;
    const disprovedDurable = protocolUtxo(disprovedOutRef, disprovedOutputHex);
    const disprovedResource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      disprovedDurable,
    )!;
    const claimSubjectId = "resolution-claim-aa";
    const claimSubject = makeWatcherSettlementSubject({
      subjectId: claimSubjectId,
      subjectKind: "resolution_claim",
      status: "disproved",
      resourceOutRef: null,
      relatedSubjectId: settlementAsset,
      resolutionTime: "2000",
      operatorVkey: operator,
      attempt: "0",
      terminalTransactionHash: disproveTransactionHash,
      failureCode: null,
    })!;
    const disprovedSnapshot = makeWatcherSettlementSnapshot({
      resources: [disprovedResource],
      subjects: [settlementSubject(disprovedOutRef, "open"), claimSubject],
    })!;
    const disproveRedeemer = Data.to(
      {
        DisproveResolutionClaim: {
          settlement_input_index: 0n,
          settlement_output_index: 0n,
          hub_ref_input_index: 0n,
          operators_redeemer_index: 2n,
          operator,
          operator_is_active: true,
          unresolved_event_ref_input_index: 1n,
          unresolved_event_asset_name: "aa",
          event_type: "Deposit",
          membership_proof: {
            DepositMembership: {
              witness: {
                domain: "DepositsRootDomain",
                root: ROOT,
                phas_root: h32("b9"),
                count: 1n,
                key: "aa",
                value: "bb",
                proof: [],
              },
            },
          },
          inclusion_proof_script_withdraw_redeemer_index: 3n,
        },
      },
      SettlementSpendRedeemer,
    );
    const listTransitionRedeemer = Data.to(
      "ListStateTransition",
      ActiveOperatorSpendRedeemer,
    );
    const slashRedeemer = Data.to(
      {
        SlashOperator: {
          slashing_arguments: {
            slashed_operator: operator,
            hub_oracle_ref_input_index: 0n,
            slashed_operator_anchor_element_input_outref: {
              transactionId: h32("b8"),
              outputIndex: 0n,
            },
            slashed_operator_anchor_element_output_index: 1n,
            slashing_reason: {
              SlashOperatorForBadSettlement: {
                settlement_input_index: 0n,
                settlement_redeemer_index: 0n,
              },
            },
          },
          operator_removal_scheduler_sync: {
            ShowOperatorIsInactive: {
              scheduler_ref_input_index: 1n,
            },
          },
        },
      },
      ActiveOperatorMintRedeemer,
    );
    const membershipRedeemer = Data.to(
      [h32("b9"), "aa", "bb", []] as never,
      Data.Tuple([MerkleRootSchema, Data.Bytes(), Data.Bytes(), ProofSchema]),
    );
    const disproved = bundle({
      kind: "disprove_resolution_claim",
      previousState: claimedState,
      snapshot: disprovedSnapshot,
      inputs: [claimedOutRef, activeOperatorOutRef],
      outputHexes: [disprovedOutputHex, activeOperatorOutputHex],
      mint: disproveMint,
      redeemers: [
        { purpose: "spend", index: "0", cborHex: disproveRedeemer },
        {
          purpose: "spend",
          index: "1",
          cborHex: listTransitionRedeemer,
        },
        { purpose: "mint", index: "0", cborHex: slashRedeemer },
        {
          purpose: "withdrawal",
          index: "0",
          cborHex: membershipRedeemer,
        },
      ],
      transition: {
        subjectId: settlementAsset,
        relatedSubjectId: claimSubjectId,
        consumedOutRefs: [claimedOutRef],
        producedOutRefs: [disprovedOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "settlement_spend",
            constructor: "DisproveResolutionClaim",
          },
          {
            purpose: "spend",
            index: "1",
            schema: "active_operator_spend",
            constructor: "ListStateTransition",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "active_operator_mint",
            constructor: "SlashOperator",
          },
          {
            purpose: "withdrawal",
            index: "0",
            schema: "membership_withdrawal",
            constructor: "MembershipProof",
          },
        ],
        exactMint: [
          {
            policyId: activeOperatorPolicyId,
            assetName: operator,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
        attachEvidence.restartBinding,
      ],
      protocolUtxos: [disprovedDurable],
    });
    expect(accepted(claimedState, disproved).snapshot.subjects).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ subjectId: settlementAsset, status: "open" }),
        expect.objectContaining({
          subjectId: claimSubjectId,
          status: "disproved",
        }),
      ]),
    );

    const resolveInputs = [claimedOutRef];
    const resolveMint = [
      {
        policyId: settlementPolicyId,
        assetName: settlementAsset,
        quantity: -1n,
      },
    ] as const;
    const resolveBodyHex = transactionBody(resolveInputs, [], resolveMint);
    const resolveTransactionHash = computeHash32(
      Buffer.from(resolveBodyHex, "hex"),
    ).toString("hex");
    const resolvedSnapshot = makeWatcherSettlementSnapshot({
      resources: [],
      subjects: [
        settlementSubject(null, "resolved", "0", null, resolveTransactionHash),
      ],
    })!;
    const resolveSpendRedeemer = Data.to(
      { Resolve: { settlement_id: settlementAsset } },
      SettlementSpendRedeemer,
    );
    const resolveMintRedeemer = Data.to(
      {
        Remove: {
          settlement_id: settlementAsset,
          input_index: 0n,
          spend_redeemer_index: 0n,
        },
      },
      SettlementMintRedeemer,
    );
    const resolved = bundle({
      kind: "resolve_settlement",
      previousState: claimedState,
      snapshot: resolvedSnapshot,
      inputs: resolveInputs,
      mint: resolveMint,
      redeemers: [
        {
          purpose: "spend",
          index: "0",
          cborHex: resolveSpendRedeemer,
        },
        { purpose: "mint", index: "0", cborHex: resolveMintRedeemer },
      ],
      transition: {
        subjectId: settlementAsset,
        consumedOutRefs: [claimedOutRef],
        requiredRedeemers: [
          {
            purpose: "spend",
            index: "0",
            schema: "settlement_spend",
            constructor: "Resolve",
          },
          {
            purpose: "mint",
            index: "0",
            schema: "settlement_mint",
            constructor: "Remove",
          },
        ],
        exactMint: [
          {
            policyId: settlementPolicyId,
            assetName: settlementAsset,
            quantity: "-1",
          },
        ],
      },
      restartBindings: [
        spawned.bootstrapEvidence.restartBinding,
        spawned.spawnEvidence.restartBinding,
        attachEvidence.restartBinding,
      ],
      protocolUtxos: [],
    });
    expect(accepted(claimedState, resolved).snapshot.subjects[0]).toMatchObject(
      {
        status: "resolved",
        terminalTransactionHash: resolveTransactionHash,
      },
    );
  }, 30_000);

  it("rejects exact transaction identity collisions", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, initial);
    const collision = makeWatcherSettlementObservation({
      policyDigest: initial.observation.policyDigest,
      network: initial.observation.network,
      releaseEvidenceDigest: initial.observation.releaseEvidenceDigest,
      deploymentMarker: initial.observation.deploymentMarker,
      pointDigest: initial.observation.pointDigest,
      chainPointId: initial.observation.chainPointId,
      blockHash: initial.observation.blockHash,
      slot: initial.observation.slot,
      blockNo: initial.observation.blockNo,
      transactionHash: initial.observation.transactionHash,
      sourceObservationDigest: initial.observation.sourceObservationDigest,
      durableStoreDigest: initial.observation.durableStoreDigest,
      predecessorStateDigest: state.stateDigest,
      transition: initial.observation.transition,
      snapshot: initial.observation.snapshot,
    })!;
    const result = evaluateWatcherSettlementIndexer(policy, state, collision, {
      ...initial.context,
      sourceDurableStore: initial.store,
      restartContexts: [initial.restartBinding],
    });
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["identity_collision"],
    });
  });

  it("consumes an exact external-provider W13 post-finality recovery, prunes W16 orphan state, restarts, and resumes", () => {
    const sparseSequence = spawnSequence({
      emptyBlockGap: true,
    });
    const recovery = postFinalitySettlementRecoveryBundle(sparseSequence);
    expect(recovery.recovery.removedRecords.protocolUtxoOutRefs).toContain(
      sparseSequence.outRef,
    );
    const result = evaluateWatcherSettlementIndexer(
      policy,
      sparseSequence.spawnState,
      recovery.recoveryEvidence.observation,
      recovery.recoveryEvidence.context,
    );
    expect(result).toMatchObject({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: ["rollback_authenticated"],
      state: {
        snapshot: { resources: [], subjects: [] },
      },
    });
    expect(result.state?.activeHistory).toEqual(
      sparseSequence.bootstrapState.activeHistory,
    );
    expect(result.state?.transitionHistory).toHaveLength(
      sparseSequence.spawnState.transitionHistory.length + 1,
    );
    expect(result.state?.transitionHistory).toContainEqual(
      sparseSequence.spawnState.transitionHistory.at(-1),
    );
    expect(result.state?.rollbackHistory).toHaveLength(1);
    expect(result.state?.durableStoreDigest).toBe(
      watcherDurableStoreBytesSha256(
        encodeWatcherDurableStore(recovery.recovery.nextStore!),
      ),
    );
    const settlementRoles = new Set([
      "settlement",
      "reserve",
      "payout",
      "withdrawal",
    ]);
    for (const collection of ["protocolUtxos", "spentProtocolUtxos"] as const) {
      expect(
        recovery.recovery.nextStore![collection].filter(
          ({ role }) => !settlementRoles.has(role),
        ),
      ).toEqual(
        recovery.incident.nextStore![collection].filter(
          ({ role }) => !settlementRoles.has(role),
        ),
      );
    }
    const recoveryBinding = {
      resultDigest: recovery.recovery.resultDigest,
      context: recovery.recoveryInput,
    };
    const restartBindings = [
      sparseSequence.bootstrapEvidence.restartBinding,
      sparseSequence.spawnEvidence.restartBinding,
      recovery.recoveryEvidence.restartBinding,
    ];
    const restarted = parseWatcherSettlementIndexerState(
      JSON.parse(JSON.stringify(result.state)),
      policy,
      restartBindings,
      [recoveryBinding],
    );
    expect(restarted).toEqual(result.state);

    const duplicateContext: WatcherSettlementPublicContext = {
      ...recovery.recoveryEvidence.context,
      restartContexts: restartBindings,
      restartRollbackContexts: [recoveryBinding],
    };
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        restarted,
        recovery.recoveryEvidence.observation,
        duplicateContext,
      ),
    ).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_observation"],
      state: result.state,
    });

    const reIncludedRaw = structuredClone(recovery.replacementRaw) as Mutable;
    reIncludedRaw.providerId = recovery.replacementProvider.providerId;
    reIncludedRaw.chainPoint.depth = "1";
    const resumed = bundle({
      kind: "spawn_settlement",
      previousState: restarted,
      snapshot: sparseSequence.spawnEvidence.observation.snapshot,
      transition: sparseSequence.spawnEvidence.observation.transition,
      restartBindings,
      restartRollbackBindings: [recoveryBinding],
      authenticatedProvider: recovery.replacementProvider,
      l1Observation: reIncludedRaw,
      sourceDurableStore: recovery.recovery.nextStore!,
      protocolUtxos: [sparseSequence.durable],
    });
    expect(accepted(restarted, resumed).snapshot).toEqual(
      sparseSequence.spawnEvidence.observation.snapshot,
    );
  }, 30_000);

  it("records an authenticated W16 recovery when W13 removed no indexed settlement change", () => {
    const sequence = spawnSequence({
      emptyBlockGap: true,
    });
    const recovery = postFinalitySettlementRecoveryBundle(sequence);
    const evidence = bundle({
      kind: "rollback",
      previousState: sequence.bootstrapState,
      predecessorStateDigest: sequence.bootstrapState.stateDigest,
      snapshot: emptySnapshot(),
      restartBindings: [sequence.bootstrapEvidence.restartBinding],
      authenticatedProvider: recovery.replacementProvider,
      l1Observation: recovery.recoveryEvidence.context.l1Observation as Mutable,
      sourceDurableStore: recovery.incident.nextStore!,
      durableStore: recovery.recovery.nextStore!,
      journalSpentAtChainPointId: latestChainPoint(recovery.incident.nextStore!)
        .chainPointId,
      rollbackAuthority: {
        result: recovery.recovery,
        context: recovery.recoveryInput,
      },
    });
    const result = evaluateWatcherSettlementIndexer(
      policy,
      sequence.bootstrapState,
      evidence.observation,
      evidence.context,
    );
    expect(result).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: {
        pointDigest: recovery.common.observations[0]!.chainPoint.pointDigest,
        activeHistory: sequence.bootstrapState.activeHistory,
        rollbackHistory: [expect.any(Object)],
      },
    });
    expect(
      parseWatcherSettlementIndexerState(
        structuredClone(result.state),
        policy,
        [sequence.bootstrapEvidence.restartBinding, evidence.restartBinding],
        [
          {
            resultDigest: recovery.recovery.resultDigest,
            context: recovery.recoveryInput,
          },
        ],
      ),
    ).toEqual(result.state);
  }, 30_000);

  it("rejects forged, mismatched, wrong-target, wrong-mode, and duplicate-only W13 recovery evidence", () => {
    const sequence = spawnSequence();
    const recovery = postFinalitySettlementRecoveryBundle(sequence);
    const evaluate = (
      observation: WatcherSettlementObservation,
      context: WatcherSettlementPublicContext,
    ) =>
      evaluateWatcherSettlementIndexer(
        policy,
        sequence.spawnState,
        observation,
        context,
      );

    const forgedContext = structuredClone(
      recovery.recoveryEvidence.context,
    ) as WatcherSettlementPublicContext;
    const forgedResult = forgedContext.rollbackAuthority!.result as Mutable;
    forgedResult.nextStoreDigest = h32("ff");
    delete forgedResult.resultDigest;
    forgedResult.resultDigest = sha256CanonicalForTest(forgedResult);
    expect(
      evaluate(recovery.recoveryEvidence.observation, forgedContext),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const mismatchedContext = structuredClone(
      recovery.recoveryEvidence.context,
    ) as WatcherSettlementPublicContext;
    (
      mismatchedContext.rollbackAuthority!.context as unknown as Mutable
    ).replacementCanonicalPath = recovery.recoveryInput.previousCanonicalPath;
    expect(
      evaluate(recovery.recoveryEvidence.observation, mismatchedContext),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const wrongModeContext = structuredClone(
      recovery.recoveryEvidence.context,
    ) as WatcherSettlementPublicContext;
    (wrongModeContext.rollbackAuthority!.context as unknown as Mutable).policy =
      {
        ...finalityPolicy,
        sourceMode: "local_node",
      };
    expect(
      evaluate(recovery.recoveryEvidence.observation, wrongModeContext),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const wrongTarget = makeWatcherSettlementObservation({
      ...recovery.recoveryEvidence.observation,
      predecessorStateDigest: sequence.bootstrapState.stateDigest,
    })!;
    expect(
      evaluate(wrongTarget, recovery.recoveryEvidence.context),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["stale_state"],
    });

    const duplicateRecovery = evaluateWatcherPostFinalityRecovery({
      ...recovery.recoveryInput,
      currentStore: recovery.recovery.nextStore,
      previousRecoveryState: recovery.recovery.recoveryState,
    });
    expect(duplicateRecovery.action).toBe("duplicate_recovery");
    const duplicateOnlyContext: WatcherSettlementPublicContext = {
      ...recovery.recoveryEvidence.context,
      sourceDurableStore: recovery.recovery.nextStore,
      rollbackAuthority: {
        result: duplicateRecovery,
        context: {
          ...recovery.recoveryInput,
          currentStore: recovery.recovery.nextStore,
          previousRecoveryState: recovery.recovery.recoveryState,
        },
      },
    };
    expect(
      evaluate(recovery.recoveryEvidence.observation, duplicateOnlyContext),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const acceptedRecovery = evaluate(
      recovery.recoveryEvidence.observation,
      recovery.recoveryEvidence.context,
    );
    expect(acceptedRecovery.action).toBe("accept");
    expect(
      parseWatcherSettlementIndexerState(
        acceptedRecovery.state,
        policy,
        [
          sequence.bootstrapEvidence.restartBinding,
          recovery.recoveryEvidence.restartBinding,
        ],
        [],
      ),
    ).toBeNull();
  }, 30_000);

  it("applies an authoritative W13 rewind, replays it after restart, and safely re-includes the orphaned transaction", () => {
    const bootstrapEvidence = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const bootstrapState = accepted(null, bootstrapEvidence);
    const outputHex = settlementOutput();
    const queueOutputHex = stateQueueOutput();
    const inputs = [h32("d1") + "#0"];
    const mint = [
      {
        policyId: settlementPolicyId,
        assetName: settlementAsset,
        quantity: 1n,
      },
      {
        policyId: stateQueuePolicyId,
        assetName: `4d424c43${settlementAsset}`,
        quantity: -1n,
      },
    ] as const;
    const bodyHex = transactionBody(inputs, [outputHex, queueOutputHex], mint);
    const transactionHash = computeHash32(Buffer.from(bodyHex, "hex")).toString(
      "hex",
    );
    const outRef = `${transactionHash}#0`;
    const datumHex = CML.TransactionOutput.from_cbor_hex(outputHex)
      .datum()
      ?.as_datum()
      ?.to_canonical_cbor_hex();
    expect(datumHex).toBeDefined();
    const spawnRedeemer = Data.to(
      {
        Spawn: {
          settlement_id: settlementAsset,
          output_index: 0n,
          state_queue_merge_redeemer_index: 1n,
          hub_ref_input_index: 0n,
        },
      },
      SettlementMintRedeemer,
    );
    const canonicalSpawnRedeemer =
      CML.PlutusData.from_cbor_hex(spawnRedeemer).to_canonical_cbor_hex();
    const canonicalMergeRedeemer = CML.PlutusData.from_cbor_hex(
      stateQueueMergeRedeemer(inputs[0]!),
    ).to_canonical_cbor_hex();
    const witnessSet = CML.TransactionWitnessSet.new();
    const legacyRedeemers = CML.LegacyRedeemerList.new();
    legacyRedeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Mint,
        0n,
        CML.PlutusData.from_cbor_hex(canonicalSpawnRedeemer),
        CML.ExUnits.new(0n, 0n),
      ),
    );
    legacyRedeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Mint,
        1n,
        CML.PlutusData.from_cbor_hex(canonicalMergeRedeemer),
        CML.ExUnits.new(0n, 0n),
      ),
    );
    witnessSet.set_redeemers(
      CML.Redeemers.new_arr_legacy_redeemer(legacyRedeemers),
    );
    const fullTransaction = CML.Transaction.new(
      CML.TransactionBody.from_cbor_hex(bodyHex),
      witnessSet,
      true,
      undefined,
    );
    const publicTransaction = () => ({
      txHash: transactionHash,
      transactionIndex: "0",
      fullTransaction: makeWatcherL1PublicBytes(
        fullTransaction.to_canonical_cbor_hex(),
      ),
      body: makeWatcherL1PublicBytes(bodyHex),
      witnessSet: makeWatcherL1PublicBytes(witnessSet.to_canonical_cbor_hex()),
      utxos: [
        {
          outRef,
          outputIndex: "0",
          output: makeWatcherL1PublicBytes(
            CML.TransactionOutput.from_cbor_hex(
              outputHex,
            ).to_canonical_cbor_hex(),
          ),
          datum: {
            datumHash: computeHash32(Buffer.from(datumHex!, "hex")).toString(
              "hex",
            ),
            bytes: makeWatcherL1PublicBytes(datumHex!),
          },
          referenceScript: null,
        },
        {
          outRef: `${transactionHash}#1`,
          outputIndex: "1",
          output: makeWatcherL1PublicBytes(
            CML.TransactionOutput.from_cbor_hex(
              queueOutputHex,
            ).to_canonical_cbor_hex(),
          ),
          datum: null,
          referenceScript: null,
        },
      ],
      scripts: [],
      datums: [],
      redeemers: [
        {
          purpose: "mint",
          index: "0",
          bytes: makeWatcherL1PublicBytes(canonicalSpawnRedeemer),
        },
        {
          purpose: "mint",
          index: "1",
          bytes: makeWatcherL1PublicBytes(canonicalMergeRedeemer),
        },
      ],
    });
    const bootstrapHead = bootstrapState.activeHistory.at(-1)!.observation;
    const oldPoint = {
      blockHash: h32("d2"),
      parentBlockHash: bootstrapHead.blockHash,
      slot: (BigInt(bootstrapHead.slot) + 1n).toString(),
      blockNo: (BigInt(bootstrapHead.blockNo) + 1n).toString(),
      depth: "1",
    } as const;
    const replacementPoint = {
      blockHash: h32("d3"),
      parentBlockHash: bootstrapHead.blockHash,
      slot: (BigInt(bootstrapHead.slot) + 2n).toString(),
      blockNo: (BigInt(bootstrapHead.blockNo) + 1n).toString(),
      depth: "2",
    } as const;
    const providerA = rollbackProvider("provider-a", "77");
    const providerB = rollbackProvider("provider-b", "78");
    const oldTransaction = publicTransaction();
    const replacementTransaction = publicTransaction();
    const oldRawA = rawBlock(providerA, oldPoint, oldTransaction);
    const oldRawB = rawBlock(providerB, oldPoint, oldTransaction);
    const replacementRawA = rawBlock(
      providerA,
      replacementPoint,
      replacementTransaction,
    );
    const replacementRawB = rawBlock(
      providerB,
      replacementPoint,
      replacementTransaction,
    );
    const oldA = normalizeWatcherL1Block(providerA, oldRawA);
    const oldB = normalizeWatcherL1Block(providerB, oldRawB);
    const replacementA = normalizeWatcherL1Block(providerA, replacementRawA);
    const replacementB = normalizeWatcherL1Block(providerB, replacementRawB);
    const persisted = [oldA, oldB, replacementA, replacementB];
    const oldDurable: WatcherProtocolUtxo = {
      outRef,
      role: "settlement",
      chainPointId: oldA.chainPoint.chainPointId,
      output: makeWatcherDurablePayload(outputHex),
    };
    const sourceStore = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: (BigInt(bootstrapEvidence.store.revision) + 1n).toString(),
      records: {
        l1Observations: [
          ...bootstrapEvidence.store.l1Observations,
          ...persisted.map(persistedObservation),
        ],
        chainPoints: [
          ...bootstrapEvidence.store.chainPoints,
          ...persistedChainPoints(persisted),
        ],
        protocolUtxos: [...bootstrapEvidence.store.protocolUtxos, oldDurable],
        spentProtocolUtxos: bootstrapEvidence.store.spentProtocolUtxos,
        daProofInputs: bootstrapEvidence.store.daProofInputs,
        reconstructedStates: bootstrapEvidence.store.reconstructedStates,
        decisions: bootstrapEvidence.store.decisions,
        faults: bootstrapEvidence.store.faults,
        submissions: bootstrapEvidence.store.submissions,
        confirmations: bootstrapEvidence.store.confirmations,
        retries: bootstrapEvidence.store.retries,
        deadlines: bootstrapEvidence.store.deadlines,
        correctionResults: bootstrapEvidence.store.correctionResults,
      },
    });
    const resource = makeWatcherSettlementResourceFromProtocolUtxo(
      policy,
      oldDurable,
    )!;
    const spawnedSnapshot = makeWatcherSettlementSnapshot({
      resources: [resource],
      subjects: [settlementSubject(outRef, "open")],
    })!;
    const spawnTransition = {
      subjectId: settlementAsset,
      producedOutRefs: [outRef],
      requiredRedeemers: [
        {
          purpose: "mint" as const,
          index: "0",
          schema: "settlement_mint" as const,
          constructor: "Spawn" as const,
        },
        {
          purpose: "mint" as const,
          index: "1",
          schema: "state_queue_mint" as const,
          constructor: "MergeToConfirmedStateV1" as const,
        },
      ],
      exactMint: [
        {
          policyId: settlementPolicyId,
          assetName: settlementAsset,
          quantity: "1",
        },
        {
          policyId: stateQueuePolicyId,
          assetName: `4d424c43${settlementAsset}`,
          quantity: "-1",
        },
      ],
    };
    const spawnEvidence = bundle({
      kind: "spawn_settlement",
      previousState: bootstrapState,
      snapshot: spawnedSnapshot,
      inputs,
      outputHexes: [outputHex, queueOutputHex],
      mint,
      redeemers: [
        { purpose: "mint", index: "0", cborHex: spawnRedeemer },
        {
          purpose: "mint",
          index: "1",
          cborHex: stateQueueMergeRedeemer(inputs[0]!),
        },
      ],
      transition: spawnTransition,
      restartBindings: [bootstrapEvidence.restartBinding],
      authenticatedProvider: providerA,
      l1Observation: oldRawA,
      durableStore: sourceStore,
    });
    const spawnedState = accepted(bootstrapState, spawnEvidence);

    const rollbackFinalityPolicy = makeWatcherFinalityPolicy(
      {
        schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
        mode: "development",
        targetNetwork: "Preprod",
        l1: {
          source: {
            sourceMode: "external_providers",
            providers: [
              {
                identity: "provider-a",
                operatorIdentitySha256: h32("97"),
                endpoint: transportEndpointByProviderId.get("provider-a")!,
              },
              {
                identity: "provider-b",
                operatorIdentitySha256: h32("98"),
                endpoint: transportEndpointByProviderId.get("provider-b")!,
              },
            ],
          },
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
          finality: {
            depth: 5,
            rollback: {
              beforeFinality: "rewind",
              afterFinality: "quarantine",
              maxDepth: 5,
            },
          },
        },
        da: {
          peers: [
            {
              identity: "da-peer-a",
              multiaddr:
                "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
            },
          ],
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
        },
        storage: {
          driver: "sqlite",
          path: "/var/lib/midgard-watcher/watcher.sqlite",
          rollbackAuthorityKeySource: {
            kind: "environment",
            variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
          },
        },
        proverWallet: {
          keySource: {
            kind: "environment",
            variable: "MIDGARD_WATCHER_PROVER_KEY",
          },
        },
        deadlines: {
          daFetchMs: 60_000,
          daPublishMs: 60_000,
          proofConstructMs: 300_000,
          proofSubmitMs: 120_000,
        },
      },
      {
        manifestId: policy.deploymentMarker.manifestId,
        network: "Preprod",
        trustRootId: h32("23"),
        releaseEvidenceDigest: policy.releaseEvidenceDigest,
        ruleBundleCommitment: h32("24"),
        programCommitments: { validation: h32("25") },
        durableMarker: policy.deploymentMarker,
      },
    )!;
    const oldConsistency = evaluateWatcherMultiProviderConsistency(
      externalSource,
      [oldA, oldB],
    );
    const priorFinalityResult = evaluateWatcherFinality(
      rollbackFinalityPolicy,
      null,
      oldConsistency,
    );
    expect(priorFinalityResult.action).toBe("observe_pending");
    const priorFinalityState = priorFinalityResult.state!;
    const replacementConsistency = evaluateWatcherMultiProviderConsistency(
      externalSource,
      [replacementA, replacementB],
    );
    const finalityResult = evaluateWatcherFinality(
      rollbackFinalityPolicy,
      priorFinalityState,
      replacementConsistency,
    );
    expect(finalityResult.action).toBe("rewind_pending");
    const rollbackBootstrapState = makeWatcherRollbackBootstrapState(
      rollbackFinalityPolicy,
      sourceStore,
      priorFinalityState,
    )!;
    const rollbackVerificationContext = {
      policy: rollbackFinalityPolicy,
      sourceStore,
      previousFinalityState: priorFinalityState,
      consistency: replacementConsistency,
      finalityResult,
      previousRollbackState: rollbackBootstrapState,
      rollbackBootstrapState,
    };
    const authoritative = evaluateWatcherRollback(
      rollbackFinalityPolicy,
      sourceStore,
      priorFinalityState,
      replacementConsistency,
      finalityResult,
      rollbackBootstrapState,
      rollbackBootstrapState,
    );
    expect(authoritative).toMatchObject({
      action: "apply_rewind",
      protocolDecision: "resume_pending",
      removedRecords: {
        protocolUtxoOutRefs: [outRef],
      },
    });
    expect(authoritative.nextStore).not.toBeNull();
    const rollbackEvidence = bundle({
      kind: "rollback",
      previousState: spawnedState,
      snapshot: emptySnapshot(),
      restartBindings: [
        bootstrapEvidence.restartBinding,
        spawnEvidence.restartBinding,
      ],
      authenticatedProvider: providerA,
      l1Observation: replacementRawA,
      durableStore: authoritative.nextStore!,
      rollbackAuthority: {
        result: authoritative,
        context: rollbackVerificationContext,
      },
    });
    const rollbackState = accepted(spawnedState, rollbackEvidence);
    expect(rollbackState.activeHistory).toHaveLength(1);
    expect(rollbackState.orphanAuditDigests).toContain(
      spawnedState.activeHistory.at(-1)!.stateDigest,
    );
    const retainedAnchorProvider = bootstrapEvidence.context
      .authenticatedProvider as WatcherAuthenticatedL1Provider;
    const retainedAnchorRaw = structuredClone(
      bootstrapEvidence.context.l1Observation,
    ) as Mutable;
    const retainedAnchor = normalizeWatcherL1Block(
      retainedAnchorProvider,
      retainedAnchorRaw,
    );
    expect(authoritative.nextStore!.l1Observations).toContainEqual(
      persistedObservation(retainedAnchor),
    );
    expect(authoritative.nextStore!.chainPoints).toContainEqual({
      chainPointId: retainedAnchor.chainPoint.chainPointId,
      providerId: retainedAnchor.provider.providerId,
      blockHash: retainedAnchor.chainPoint.blockHash,
      slot: retainedAnchor.chainPoint.slot,
      blockNo: retainedAnchor.chainPoint.blockNo,
      depth: retainedAnchor.chainPoint.depth,
    });
    expect(retainedAnchor.observationDigest).not.toBe(
      replacementA.observationDigest,
    );
    const retainedAnchorForgery = bundle({
      kind: "rollback",
      previousState: spawnedState,
      snapshot: emptySnapshot(),
      restartBindings: [
        bootstrapEvidence.restartBinding,
        spawnEvidence.restartBinding,
      ],
      authenticatedProvider: retainedAnchorProvider,
      l1Observation: retainedAnchorRaw,
      durableStore: authoritative.nextStore!,
      journalSpentAtChainPointId: replacementA.chainPoint.chainPointId,
      rollbackAuthority: {
        result: authoritative,
        context: rollbackVerificationContext,
      },
    });
    expect(retainedAnchorForgery.observation.sourceObservationDigest).toBe(
      retainedAnchor.observationDigest,
    );
    expect(retainedAnchorForgery.observation.observationDigest).not.toBe(
      rollbackEvidence.observation.observationDigest,
    );
    expect(retainedAnchorForgery.restartBinding.publicContextDigest).not.toBe(
      rollbackEvidence.restartBinding.publicContextDigest,
    );
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        spawnedState,
        retainedAnchorForgery.observation,
        retainedAnchorForgery.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });
    const restartRollbackBinding = {
      resultDigest: authoritative.resultDigest,
      context: rollbackVerificationContext,
    };
    expect(
      parseWatcherSettlementIndexerState(
        JSON.parse(JSON.stringify(rollbackState)),
        policy,
        [
          bootstrapEvidence.restartBinding,
          spawnEvidence.restartBinding,
          rollbackEvidence.restartBinding,
        ],
        [restartRollbackBinding],
      ),
    ).toEqual(rollbackState);
    expect(
      parseWatcherSettlementIndexerState(
        rollbackState,
        policy,
        [bootstrapEvidence.restartBinding],
        [restartRollbackBinding],
      ),
    ).toBeNull();
    expect(
      parseWatcherSettlementIndexerState(
        rollbackState,
        policy,
        [
          bootstrapEvidence.restartBinding,
          spawnEvidence.restartBinding,
          rollbackEvidence.restartBinding,
        ],
        [],
      ),
    ).toBeNull();
    const forgedState: Mutable = JSON.parse(JSON.stringify(rollbackState));
    forgedState.durableStoreDigest = h32("fe");
    delete forgedState.stateDigest;
    forgedState.stateDigest = sha256CanonicalForTest(forgedState);
    expect(
      parseWatcherSettlementIndexerState(
        forgedState,
        policy,
        [bootstrapEvidence.restartBinding, rollbackEvidence.restartBinding],
        [restartRollbackBinding],
      ),
    ).toBeNull();
    const authenticRollbackResult = evaluateWatcherSettlementIndexer(
      policy,
      spawnedState,
      rollbackEvidence.observation,
      rollbackEvidence.context,
    );
    const forgedResult: Mutable = JSON.parse(
      JSON.stringify(authenticRollbackResult),
    );
    forgedResult.protocolDecision = "hold";
    delete forgedResult.resultDigest;
    forgedResult.resultDigest = sha256CanonicalForTest(forgedResult);
    expect(
      parseWatcherSettlementIndexerResult(forgedResult, {
        policy,
        previousState: spawnedState,
        observation: rollbackEvidence.observation,
        publicContext: rollbackEvidence.context,
      }),
    ).toBeNull();

    const nextStore = authoritative.nextStore!;
    const unrelatedSentinel = {
      inputId: h32("ed"),
      kind: "proof_input" as const,
      payload: makeWatcherDurablePayload("81"),
    };
    const extendedSourceStore = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: (BigInt(nextStore.revision) + 1n).toString(),
      records: {
        ...nextStore,
        daProofInputs: [...nextStore.daProofInputs, unrelatedSentinel],
      },
    });
    const deletedPriorRecordSource = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: extendedSourceStore.revision,
      records: {
        ...extendedSourceStore,
        l1Observations: extendedSourceStore.l1Observations.slice(1),
      },
    });
    const mutatedPriorRecordSource = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: extendedSourceStore.revision,
      records: {
        ...extendedSourceStore,
        chainPoints: extendedSourceStore.chainPoints.map((entry, index) =>
          index === 0
            ? {
                ...entry,
                depth: (BigInt(entry.depth) + 1n).toString(),
              }
            : entry,
        ),
      },
    });
    const reIncludedDurable: WatcherProtocolUtxo = {
      ...oldDurable,
      chainPointId: replacementA.chainPoint.chainPointId,
    };
    const reIncludedStore = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: (BigInt(extendedSourceStore.revision) + 1n).toString(),
      records: {
        ...extendedSourceStore,
        protocolUtxos: [
          ...extendedSourceStore.protocolUtxos,
          reIncludedDurable,
        ],
      },
    });
    const reIncluded = bundle({
      kind: "spawn_settlement",
      previousState: rollbackState,
      snapshot: spawnedSnapshot,
      inputs,
      outputHexes: [outputHex],
      mint,
      redeemers: [{ purpose: "mint", index: "0", cborHex: spawnRedeemer }],
      transition: spawnTransition,
      restartBindings: [
        bootstrapEvidence.restartBinding,
        spawnEvidence.restartBinding,
        rollbackEvidence.restartBinding,
      ],
      restartRollbackBindings: [restartRollbackBinding],
      authenticatedProvider: providerA,
      l1Observation: replacementRawA,
      sourceDurableStore: extendedSourceStore,
      durableStore: reIncludedStore,
    });
    expect(reIncluded.observation.transactionHash).toBe(
      spawnEvidence.observation.transactionHash,
    );
    const reIncludedState = accepted(rollbackState, reIncluded);
    expect(reIncludedState.snapshot).toEqual(spawnedSnapshot);
    expect(reIncluded.store.daProofInputs).toContainEqual(unrelatedSentinel);
    for (const divergentSource of [
      deletedPriorRecordSource,
      mutatedPriorRecordSource,
    ]) {
      const divergent = bundle({
        kind: "spawn_settlement",
        previousState: rollbackState,
        snapshot: spawnedSnapshot,
        inputs,
        outputHexes: [outputHex],
        mint,
        redeemers: [{ purpose: "mint", index: "0", cborHex: spawnRedeemer }],
        transition: spawnTransition,
        restartBindings: [
          bootstrapEvidence.restartBinding,
          spawnEvidence.restartBinding,
          rollbackEvidence.restartBinding,
        ],
        restartRollbackBindings: [restartRollbackBinding],
        authenticatedProvider: providerA,
        l1Observation: replacementRawA,
        sourceDurableStore: divergentSource,
      });
      expect(
        evaluateWatcherSettlementIndexer(
          policy,
          rollbackState,
          divergent.observation,
          divergent.context,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["stale_state"],
      });
    }

    const secondReplacementPoint = {
      blockHash: h32("d4"),
      parentBlockHash: bootstrapHead.blockHash,
      slot: (BigInt(replacementPoint.slot) + 1n).toString(),
      blockNo: replacementPoint.blockNo,
      depth: "3",
    } as const;
    const secondReplacementRawA = rawBlock(
      providerA,
      secondReplacementPoint,
      publicTransaction(),
    );
    const secondReplacementRawB = rawBlock(
      providerB,
      secondReplacementPoint,
      publicTransaction(),
    );
    const secondReplacementA = normalizeWatcherL1Block(
      providerA,
      secondReplacementRawA,
    );
    const secondReplacementB = normalizeWatcherL1Block(
      providerB,
      secondReplacementRawB,
    );
    const secondSourceStore = makeWatcherDurableStore({
      deploymentMarker: policy.deploymentMarker,
      revision: (BigInt(reIncludedStore.revision) + 1n).toString(),
      records: {
        ...reIncludedStore,
        l1Observations: [
          ...reIncludedStore.l1Observations,
          persistedObservation(secondReplacementA),
          persistedObservation(secondReplacementB),
        ],
        chainPoints: [
          ...reIncludedStore.chainPoints,
          ...persistedChainPoints([secondReplacementA, secondReplacementB]),
        ],
      },
    });
    const secondConsistency = evaluateWatcherMultiProviderConsistency(
      externalSource,
      [secondReplacementA, secondReplacementB],
    );
    const secondFinalityResult = evaluateWatcherFinality(
      rollbackFinalityPolicy,
      finalityResult.state,
      secondConsistency,
    );
    expect(secondFinalityResult.action).toBe("rewind_pending");
    const secondRollbackBootstrap = makeWatcherRollbackBootstrapState(
      rollbackFinalityPolicy,
      secondSourceStore,
      finalityResult.state,
    )!;
    const secondRollbackContext = {
      policy: rollbackFinalityPolicy,
      sourceStore: secondSourceStore,
      previousFinalityState: finalityResult.state,
      consistency: secondConsistency,
      finalityResult: secondFinalityResult,
      previousRollbackState: secondRollbackBootstrap,
      rollbackBootstrapState: secondRollbackBootstrap,
    };
    const secondAuthoritative = evaluateWatcherRollback(
      rollbackFinalityPolicy,
      secondSourceStore,
      finalityResult.state,
      secondConsistency,
      secondFinalityResult,
      secondRollbackBootstrap,
      secondRollbackBootstrap,
    );
    expect(secondAuthoritative).toMatchObject({
      action: "apply_rewind",
      protocolDecision: "resume_pending",
      removedRecords: {
        protocolUtxoOutRefs: [outRef],
      },
    });
    const secondRollbackEvidence = bundle({
      kind: "rollback",
      previousState: reIncludedState,
      snapshot: emptySnapshot(),
      restartBindings: [
        bootstrapEvidence.restartBinding,
        spawnEvidence.restartBinding,
        rollbackEvidence.restartBinding,
        reIncluded.restartBinding,
      ],
      restartRollbackBindings: [restartRollbackBinding],
      authenticatedProvider: providerA,
      l1Observation: secondReplacementRawA,
      sourceDurableStore: secondSourceStore,
      durableStore: secondAuthoritative.nextStore!,
      rollbackAuthority: {
        result: secondAuthoritative,
        context: secondRollbackContext,
      },
    });
    const twiceRolledBack = accepted(reIncludedState, secondRollbackEvidence);
    const secondRollbackBinding = {
      resultDigest: secondAuthoritative.resultDigest,
      context: secondRollbackContext,
    };
    const allRestartBindings = [
      bootstrapEvidence.restartBinding,
      spawnEvidence.restartBinding,
      rollbackEvidence.restartBinding,
      reIncluded.restartBinding,
      secondRollbackEvidence.restartBinding,
    ];
    const allRollbackBindings = [restartRollbackBinding, secondRollbackBinding];
    expect(
      parseWatcherSettlementIndexerState(
        JSON.parse(JSON.stringify(twiceRolledBack)),
        policy,
        allRestartBindings,
        allRollbackBindings,
      ),
    ).toEqual(twiceRolledBack);
    expect(secondAuthoritative.nextStore!.daProofInputs).toContainEqual(
      unrelatedSentinel,
    );

    const rehashState = (state: Mutable): Mutable => {
      delete state.stateDigest;
      state.stateDigest = sha256CanonicalForTest(state);
      return state;
    };
    const forgedOrphanLineage = JSON.parse(
      JSON.stringify(twiceRolledBack),
    ) as Mutable;
    forgedOrphanLineage.orphanAuditDigests = [h32("f1")];
    forgedOrphanLineage.orphanLineageDigest = h32("f2");
    expect(
      parseWatcherSettlementIndexerState(
        rehashState(forgedOrphanLineage),
        policy,
        allRestartBindings,
        allRollbackBindings,
      ),
    ).toBeNull();

    const swappedRollbackOrder = JSON.parse(
      JSON.stringify(twiceRolledBack),
    ) as Mutable;
    const rollbackIndexes = (
      swappedRollbackOrder.transitionHistory as Mutable[]
    )
      .map((entry, index) => (entry.kind === "rollback" ? index : -1))
      .filter((index) => index >= 0);
    expect(rollbackIndexes).toHaveLength(2);
    const firstRollback = rollbackIndexes[0]!;
    const secondRollback = rollbackIndexes[1]!;
    [
      swappedRollbackOrder.transitionHistory[firstRollback],
      swappedRollbackOrder.transitionHistory[secondRollback],
    ] = [
      swappedRollbackOrder.transitionHistory[secondRollback],
      swappedRollbackOrder.transitionHistory[firstRollback],
    ];
    expect(
      parseWatcherSettlementIndexerState(
        rehashState(swappedRollbackOrder),
        policy,
        allRestartBindings,
        allRollbackBindings,
      ),
    ).toBeNull();

    const truncatedRecombined = JSON.parse(
      JSON.stringify(twiceRolledBack),
    ) as Mutable;
    truncatedRecombined.transitionHistory =
      truncatedRecombined.transitionHistory.filter(
        (_entry: unknown, index: number) => index !== 1,
      );
    expect(
      parseWatcherSettlementIndexerState(
        rehashState(truncatedRecombined),
        policy,
        allRestartBindings,
        allRollbackBindings,
      ),
    ).toBeNull();
  }, 30_000);

  it("fails closed on a fabricated rollback without an exact W13 apply_rewind result", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, initial);
    const rollback = bundle({
      kind: "rollback",
      previousState: state,
      snapshot: emptySnapshot(),
      restartBindings: [initial.restartBinding],
    });
    expect(
      evaluateWatcherSettlementIndexer(
        policy,
        state,
        rollback.observation,
        rollback.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_mismatch"],
    });
  });

  it("replays every retained W10/W03 context when parsing state after restart", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, initial);
    expect(
      parseWatcherSettlementIndexerState(
        state,
        policy,
        [initial.restartBinding],
        [],
      ),
    ).toEqual(state);
    expect(
      parseWatcherSettlementIndexerState(state, policy, [], []),
    ).toBeNull();
  });

  it("rejects aggregate oversized and cyclic restart evidence at the direct parser boundary", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, initial);
    expect(
      parseWatcherSettlementIndexerState(
        state,
        policy,
        [initial.restartBinding],
        [],
      ),
    ).toEqual(state);

    const individuallyBoundedButAggregateOversized = Array.from(
      {
        length: WATCHER_SETTLEMENT_INDEXER_BOUNDS.transitionHistoryEntries,
      },
      () => initial.restartBinding,
    );
    expect(
      parseWatcherSettlementIndexerState(
        state,
        policy,
        individuallyBoundedButAggregateOversized,
        [],
      ),
    ).toBeNull();

    const cyclicRestartEvidence: unknown[] = [initial.restartBinding];
    cyclicRestartEvidence.push(cyclicRestartEvidence);
    expect(
      parseWatcherSettlementIndexerState(
        state,
        policy,
        cyclicRestartEvidence as unknown as WatcherSettlementPublicContext["restartContexts"],
        [],
      ),
    ).toBeNull();
  });

  it("fails closed when W15 receives missing, detached, mismatched, or closed transport capabilities", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const providerATransport = transportFor(provider);
    const providerBTransport = transportFor(
      rollbackProvider("provider-b", "78"),
    );
    const evaluateRaw = (
      attestations: readonly WatcherL1TransportAttestationContext[],
    ) =>
      evaluateWatcherSettlementIndexerRaw(
        policy,
        null,
        initial.observation,
        initial.context,
        attestations,
      );

    expect(evaluateRaw([])).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    expect(
      evaluateRaw(
        structuredClone(
          transportContexts,
        ) as WatcherL1TransportAttestationContext[],
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    expect(evaluateRaw([providerBTransport])).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    closeWatcherL1TransportAttestationContext(providerATransport);
    expect(evaluateRaw(transportContexts)).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });
});

import { execFile } from "node:child_process";
import { createHash, X509Certificate } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { isDeepStrictEqual, promisify } from "node:util";

import {
  DepositSpendRedeemer,
  MerkleRootSchema,
  PayoutDatum,
  PayoutMintRedeemer,
  ProofSchema,
  SettlementDatum,
  SettlementMintRedeemer,
  StateQueueRedeemer,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  WithdrawalOrderDatum,
  WithdrawalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { computeHash32 } from "../../../midgard-core/src/codec/hash.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/config.js";
import {
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  makeWatcherDurablePayloadV1,
  makeWatcherDurableStoreV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
  type WatcherProtocolUtxoV1,
} from "../../src/durable-store.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
} from "../../src/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  encodeWatcherNormalizedL1BlockV1,
  establishWatcherExternalProviderTransportV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1 as normalizeWatcherL1BlockV1Raw,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherAuthenticatedL1ProviderV1,
  type WatcherL1TransportAttestationContextV1,
  watcherL1TransportAttestationDetailsV1,
  type WatcherNormalizedL1BlockV1,
} from "../../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw } from "../../src/multi-provider-consistency.js";
import {
  evaluateWatcherPostFinalityRecoveryV1 as evaluateWatcherPostFinalityRecoveryV1Raw,
  evaluateWatcherRollbackV1 as evaluateWatcherRollbackV1Raw,
  type WatcherPostFinalityRecoveryInputV1,
} from "../../src/rollback-engine.js";
import {
  evaluateWatcherSettlementIndexerV1 as evaluateWatcherSettlementIndexerV1Raw,
  makeWatcherSettlementIndexerPolicyV1,
  makeWatcherSettlementObservationV1,
  makeWatcherSettlementResourceFromProtocolUtxoV1,
  makeWatcherSettlementSnapshotV1,
  makeWatcherSettlementSubjectV1,
  parseWatcherSettlementIndexerResultV1 as parseWatcherSettlementIndexerResultV1Raw,
  parseWatcherSettlementIndexerStateV1 as parseWatcherSettlementIndexerStateV1Raw,
  WATCHER_SETTLEMENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  type WatcherSettlementIndexerPolicyV1,
  type WatcherSettlementIndexerStateV1,
  type WatcherSettlementObservationV1,
  type WatcherSettlementPublicContextV1,
  type WatcherSettlementResultVerificationContextV1,
  type WatcherSettlementSnapshotV1,
  type WatcherSettlementSubjectV1,
  type WatcherSettlementTransitionKindV1,
  type WatcherSettlementTransitionV1,
} from "../../src/settlement-indexer.js";
import { h28, h32 } from "./deployment-authority-fixture.js";
import { makeWatcherAuthorityDeploymentFixtureV1 } from "./watcher-opaque-authority-harness.js";

type Mutable = Record<string, unknown>;
const execFileAsync = promisify(execFile);
const transportContexts: WatcherL1TransportAttestationContextV1[] = [];
const tlsServers: Server[] = [];
const tlsIdentityByProviderId = new Map<string, string>();
const transportEndpointByProviderId = new Map<string, string>();
let transportFixtureDirectory = "";
let opaqueSettlementTransportLease:
  | "idle"
  | "initializing"
  | "active"
  | "disposing" = "idle";
let opaqueSettlementTransportLeaseOwner: symbol | null = null;
let opaqueSettlementTransportCleanupToken: symbol | null = null;
let opaqueSettlementTransportCleanupPromise: Promise<void> | null = null;

const listen = async (server: Server, target: string | number): Promise<void> =>
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    const onListen = () => {
      server.off("error", reject);
      resolve();
    };
    if (typeof target === "string") {
      server.listen(target, onListen);
    } else {
      server.listen(target, "127.0.0.1", onListen);
    }
  });

const makeTlsTransportFixture = async (providerId: string) => {
  const keyPath = join(transportFixtureDirectory, `${providerId}.key`);
  const certificatePath = join(transportFixtureDirectory, `${providerId}.crt`);
  await execFileAsync("openssl", [
    "req",
    "-x509",
    "-newkey",
    "rsa:2048",
    "-nodes",
    "-keyout",
    keyPath,
    "-out",
    certificatePath,
    "-days",
    "1",
    "-subj",
    "/CN=localhost",
    "-addext",
    "subjectAltName=DNS:localhost",
  ]);
  const [key, certificate] = await Promise.all([
    readFile(keyPath, "utf8"),
    readFile(certificatePath, "utf8"),
  ]);
  const server = createTlsServer({ key, cert: certificate });
  await listen(server, 0);
  tlsServers.push(server);
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("TLS fixture did not bind a TCP port");
  }
  return {
    certificate,
    identitySha256: createHash("sha256")
      .update(new X509Certificate(certificate).raw)
      .digest("hex"),
    port: address.port,
  };
};

const transportFor = (
  authenticatedProvider: unknown,
): WatcherL1TransportAttestationContextV1 => {
  const matching = transportContexts.filter((context) => {
    const details = watcherL1TransportAttestationDetailsV1(context);
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

const normalizeWatcherL1BlockV1 = (
  authenticatedProvider: unknown,
  observation: unknown,
  session?: Parameters<typeof normalizeWatcherL1BlockV1Raw>[2],
) =>
  normalizeWatcherL1BlockV1Raw(
    transportFor(authenticatedProvider),
    observation,
    session,
  );

const evaluateWatcherMultiProviderConsistencyV1 = (
  configuredSource: unknown,
  observations: unknown,
) =>
  evaluateWatcherMultiProviderConsistencyV1Raw(
    configuredSource,
    observations,
    transportContexts,
  );

export const parseWatcherSettlementIndexerStateV1 = (
  value: unknown,
  policyValue: unknown,
  restartContexts: readonly WatcherSettlementPublicContextV1["restartContexts"][number][] = [],
  restartRollbackContexts: Parameters<
    typeof parseWatcherSettlementIndexerStateV1Raw
  >[4] = [],
): WatcherSettlementIndexerStateV1 | null =>
  parseWatcherSettlementIndexerStateV1Raw(
    value,
    policyValue,
    transportContexts,
    restartContexts,
    restartRollbackContexts,
  );

const evaluateWatcherSettlementIndexerV1 = (
  policyValue: unknown,
  previousStateValue: unknown,
  observationValue: unknown,
  publicContextValue: unknown,
) =>
  evaluateWatcherSettlementIndexerV1Raw(
    policyValue,
    previousStateValue,
    observationValue,
    publicContextValue,
    transportContexts,
  );

export const evaluateWatcherRollbackV1 = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointAuthorityInput?: unknown,
) =>
  evaluateWatcherRollbackV1Raw(
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

export const evaluateWatcherPostFinalityRecoveryV1 = (
  input: WatcherPostFinalityRecoveryInputV1,
) =>
  evaluateWatcherPostFinalityRecoveryV1Raw({
    ...input,
    transportAttestations: transportContexts,
  });

const parseWatcherSettlementIndexerResultV1 = (
  value: unknown,
  context: Omit<
    Parameters<typeof parseWatcherSettlementIndexerResultV1Raw>[1],
    "transportAttestations"
  >,
) =>
  parseWatcherSettlementIndexerResultV1Raw(value, {
    ...context,
    transportAttestations: transportContexts,
  });

const ROOT = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const settlementAsset = "aa";
const operator = h28("91");
const HUB_REFERENCE_OUT_REF = `${h32("7a")}#0`;
const SETTLEMENT_REFERENCE_OUT_REF = `${h32("7b")}#0`;
const RELEASE_DIGEST = h32("22");
const RULE_BUNDLE_COMMITMENT = h32("44");

const deploymentAuthorityFixture = makeWatcherAuthorityDeploymentFixtureV1();
const applied = deploymentAuthorityFixture.policy.appliedScriptHashes;
let deploymentAuthority = {
  signedIdentity: deploymentAuthorityFixture.signedIdentity,
  policy: deploymentAuthorityFixture.policy,
  trustRoots: deploymentAuthorityFixture.trustRoots,
  result: deploymentAuthorityFixture.result,
};

const enterpriseScriptAddress = (scriptHash: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.from(scriptHash, "hex")]),
  ).to_hex();

export const enterprisePublicKeyAddress = (keyHash: string): string =>
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
const hubReferenceUtxo: WatcherProtocolUtxoV1 = {
  outRef: HUB_REFERENCE_OUT_REF,
  role: "hub_oracle",
  chainPointId: h32("00"),
  output: makeWatcherDurablePayloadV1(hubReferenceOutputHex),
};

const bootstrapStore = makeWatcherDurableStoreV1({
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

const policy = makeWatcherSettlementIndexerPolicyV1({
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
    encodeWatcherDurableStoreV1(bootstrapStore),
  ),
  deploymentTrustRootId: deploymentAuthorityFixture.result.trustRootId,
  requiredFinalityDepth: "2",
  maximumHistoryEntries: "32",
  maximumRollbackEntries: "16",
  maximumRetryAttempts: "2",
}) as WatcherSettlementIndexerPolicyV1;

const makeExternalFinalityPolicy = () =>
  makeWatcherFinalityPolicyV1(
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

let finalityPolicy: NonNullable<ReturnType<typeof makeWatcherFinalityPolicyV1>>;

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
  status: WatcherSettlementSubjectV1["status"],
  attempt = "0",
  failureCode: string | null = null,
  terminalTransactionHash: string | null = null,
): WatcherSettlementSubjectV1 =>
  makeWatcherSettlementSubjectV1({
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

const emptySnapshot = (): WatcherSettlementSnapshotV1 =>
  makeWatcherSettlementSnapshotV1({ resources: [], subjects: [] })!;

const makePrimaryProvider = (
  publicIdentitySha256: string,
): WatcherAuthenticatedL1ProviderV1 => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: "provider-a",
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: h32("97"),
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256,
  },
});

let provider = makePrimaryProvider(h32("77"));

const externalSource: {
  sourceMode: "external_providers";
  network: "Preprod";
  providers: {
    providerId: string;
    operatorIdentitySha256: string;
    endpoint: string;
  }[];
} = {
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
};

const rollbackProvider = (
  providerId: string,
  identityByte: string,
): WatcherAuthenticatedL1ProviderV1 => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
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

const initializeOpaqueSettlementTransports = async (
  fixtureRoot = "/dev/shm",
): Promise<symbol> => {
  if (opaqueSettlementTransportLease !== "idle") {
    throw new Error("W16 opaque authority fixture lease is not idle");
  }
  const leaseOwner = Symbol("W16 opaque authority fixture lease");
  opaqueSettlementTransportLeaseOwner = leaseOwner;
  opaqueSettlementTransportLease = "initializing";
  try {
    if (
      transportContexts.length !== 0 ||
      tlsServers.length !== 0 ||
      transportFixtureDirectory !== ""
    ) {
      throw new Error("W16 opaque authority fixture state is not clean");
    }
    const freshDeployment = makeWatcherAuthorityDeploymentFixtureV1();
    deploymentAuthority = {
      signedIdentity: freshDeployment.signedIdentity,
      policy: freshDeployment.policy,
      trustRoots: freshDeployment.trustRoots,
      result: freshDeployment.result,
    };
    transportFixtureDirectory = await mkdtemp(
      join(fixtureRoot, "midgard-w15-settlement-"),
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
      configuredProvider.endpoint = endpoint;
      if (providerId === provider.providerId) {
        provider = makePrimaryProvider(fixture.identitySha256);
      }
      transportContexts.push(
        await establishWatcherExternalProviderTransportV1({
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
    opaqueSettlementTransportLease = "active";
    return leaseOwner;
  } catch (error) {
    await disposeOpaqueSettlementTransports(leaseOwner);
    throw error;
  }
};

const disposeOpaqueSettlementTransports = (
  leaseOwner: symbol,
): Promise<void> => {
  if (opaqueSettlementTransportLeaseOwner !== leaseOwner) {
    return Promise.resolve();
  }
  if (opaqueSettlementTransportLease === "disposing") {
    return opaqueSettlementTransportCleanupPromise ?? Promise.resolve();
  }
  opaqueSettlementTransportLease = "disposing";
  const cleanupToken = Symbol("W16 opaque authority fixture cleanup");
  opaqueSettlementTransportCleanupToken = cleanupToken;
  const cleanupPromise = Promise.resolve().then(async () => {
    try {
      for (const context of transportContexts) {
        closeWatcherL1TransportAttestationContextV1(context);
      }
      await Promise.all(
        tlsServers.map(
          (server) =>
            new Promise<void>((resolve) => server.close(() => resolve())),
        ),
      );
      if (transportFixtureDirectory !== "") {
        await rm(transportFixtureDirectory, { recursive: true, force: true });
      }
    } finally {
      if (
        opaqueSettlementTransportLeaseOwner === leaseOwner &&
        opaqueSettlementTransportCleanupToken === cleanupToken
      ) {
        transportContexts.length = 0;
        tlsServers.length = 0;
        tlsIdentityByProviderId.clear();
        transportEndpointByProviderId.clear();
        settlementFinalityLineageByStateDigest.clear();
        serial = 0;
        transportFixtureDirectory = "";
        externalSource.providers[0]!.endpoint = "https://cardano-a.example";
        externalSource.providers[1]!.endpoint = "https://cardano-b.example";
        provider = makePrimaryProvider(h32("77"));
        opaqueSettlementTransportCleanupPromise = null;
        opaqueSettlementTransportCleanupToken = null;
        opaqueSettlementTransportLeaseOwner = null;
        opaqueSettlementTransportLease = "idle";
      }
    }
  });
  opaqueSettlementTransportCleanupPromise = cleanupPromise;
  return cleanupPromise;
};

type RollbackPoint = Readonly<{
  blockHash: string;
  parentBlockHash: string | null;
  slot: string;
  blockNo: string;
  depth: string;
}>;

export const rawBlock = (
  authenticatedProvider: WatcherAuthenticatedL1ProviderV1,
  point: RollbackPoint,
  transaction: Mutable,
): Mutable => ({
  schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: authenticatedProvider.providerId,
  chainPoint: point,
  transactions: [transaction],
});

export const persistedObservation = (value: WatcherNormalizedL1BlockV1) => ({
  observationId: value.observationDigest,
  providerId: value.provider.providerId,
  chainPointId: value.chainPoint.chainPointId,
  payload: makeWatcherDurablePayloadV1(
    encodeWatcherNormalizedL1BlockV1(value).toString("hex"),
  ),
});

export const persistedChainPoints = (
  values: readonly WatcherNormalizedL1BlockV1[],
  preferredProviderId = "provider-a",
) => {
  const points = new Map<
    string,
    WatcherDurableStoreV1["chainPoints"][number]
  >();
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
export const latestChainPoint = (store: WatcherDurableStoreV1) =>
  store.chainPoints.reduce((latest, candidate) =>
    BigInt(candidate.blockNo) > BigInt(latest.blockNo) ||
    (BigInt(candidate.blockNo) === BigInt(latest.blockNo) &&
      BigInt(candidate.slot) > BigInt(latest.slot))
      ? candidate
      : latest,
  );

let serial = 0;

type Bundle = Readonly<{
  observation: WatcherSettlementObservationV1;
  context: WatcherSettlementPublicContextV1;
  restartBinding: WatcherSettlementPublicContextV1["restartContexts"][number];
  store: ReturnType<typeof makeWatcherDurableStoreV1>;
  finalityState: ReturnType<typeof evaluateWatcherFinalityV1>["state"];
  finalityResult: ReturnType<typeof evaluateWatcherFinalityV1>;
}>;

type SettlementFinalityLineage = NonNullable<
  WatcherSettlementPublicContextV1["finalityAuthority"]
>["lineage"];

const settlementFinalityLineageByStateDigest = new Map<
  string,
  SettlementFinalityLineage
>();

const bundle = (input: {
  policyOverride?: WatcherSettlementIndexerPolicyV1;
  kind: WatcherSettlementTransitionKindV1;
  previousState: WatcherSettlementIndexerStateV1 | null;
  snapshot: WatcherSettlementSnapshotV1;
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
  transition?: Partial<WatcherSettlementTransitionV1>;
  restartBindings?: readonly WatcherSettlementPublicContextV1["restartContexts"][number][];
  restartRollbackBindings?: readonly WatcherSettlementPublicContextV1["restartRollbackContexts"][number][];
  protocolUtxos?: readonly WatcherProtocolUtxoV1[];
  authenticatedProvider?: WatcherAuthenticatedL1ProviderV1;
  l1Observation?: Mutable;
  sourceDurableStore?: WatcherDurableStoreV1;
  durableStore?: WatcherDurableStoreV1;
  journalSpentAtChainPointId?: string;
  previousFinalityState?: unknown;
  rollbackAuthority?: WatcherSettlementPublicContextV1["rollbackAuthority"];
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
      output: makeWatcherL1PublicBytesV1(canonicalOutputHex),
      datum:
        datumHex === undefined
          ? null
          : {
              datumHash: computeHash32(Buffer.from(datumHex, "hex")).toString(
                "hex",
              ),
              bytes: makeWatcherL1PublicBytesV1(datumHex),
            },
      referenceScript: null,
    };
  });
  const authenticatedProvider = input.authenticatedProvider ?? provider;
  const previousStore = input.restartBindings?.at(-1)?.durableStore as
    | WatcherDurableStoreV1
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
    bytes: makeWatcherL1PublicBytesV1(
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
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
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
              fullTransaction: makeWatcherL1PublicBytesV1(
                fullTransaction.to_canonical_cbor_hex(),
              ),
              body: makeWatcherL1PublicBytesV1(bodyHex),
              witnessSet: makeWatcherL1PublicBytesV1(
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
  const normalized = normalizeWatcherL1BlockV1(
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
    payload: makeWatcherDurablePayloadV1(
      encodeWatcherNormalizedL1BlockV1(normalized).toString("hex"),
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
  const protocolJournal = journalWatcherProtocolUtxoTransitionV1({
    sourceStore,
    nextChainPoints,
    nextProtocolUtxos: protocolUtxos,
    spentAtChainPointId:
      input.journalSpentAtChainPointId ?? normalized.chainPoint.chainPointId,
  });
  const store =
    input.durableStore ??
    makeWatcherDurableStoreV1({
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
  const transition: WatcherSettlementTransitionV1 = {
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
  const observation = makeWatcherSettlementObservationV1({
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
      encodeWatcherDurableStoreV1(store),
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
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    configuredSource,
    finalityObservations.map(({ authenticatedProvider, l1Observation }) =>
      normalizeWatcherL1BlockV1(authenticatedProvider, l1Observation),
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
  const finalityResult = evaluateWatcherFinalityV1(
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
  const context: WatcherSettlementPublicContextV1 = {
    schemaVersion: WATCHER_SETTLEMENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
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
      encodeWatcherDurableStoreV1(sourceStore),
    ),
    storeDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
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
  const canonical = (candidate: unknown): string => {
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
    if (typeof candidate !== "object") {
      return "{}";
    }
    const record = candidate as Readonly<Record<string, unknown>>;
    return `{${Object.keys(record)
      .sort()
      .map((key) => `${JSON.stringify(key)}:${canonical(record[key])}`)
      .join(",")}}`;
  };
  return createHash("sha256").update(canonical(value), "utf8").digest("hex");
};

const accepted = (
  state: WatcherSettlementIndexerStateV1 | null,
  evidence: Bundle,
  activePolicy: WatcherSettlementIndexerPolicyV1 = policy,
): WatcherSettlementIndexerStateV1 => {
  const result = evaluateWatcherSettlementIndexerV1(
    activePolicy,
    state,
    evidence.observation,
    evidence.context,
  );
  expect(result.action, JSON.stringify(result)).toBe("accept");
  expect(result.state).not.toBeNull();
  expect(
    parseWatcherSettlementIndexerResultV1(result, {
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
): WatcherProtocolUtxoV1 => ({
  outRef,
  role: "settlement",
  chainPointId: h32("00"),
  output: makeWatcherDurablePayloadV1(outputHex),
});

export const scenarioBootstrap = (
  protocolUtxos: readonly WatcherProtocolUtxoV1[],
  spentProtocolUtxos: WatcherDurableStoreV1["spentProtocolUtxos"] = [],
): Readonly<{
  policy: WatcherSettlementIndexerPolicyV1;
  store: WatcherDurableStoreV1;
}> => {
  const store = makeWatcherDurableStoreV1({
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
  const scenarioPolicy = makeWatcherSettlementIndexerPolicyV1({
    ...policy,
    bootstrapStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
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
  const resource = makeWatcherSettlementResourceFromProtocolUtxoV1(
    policy,
    durable,
  )!;
  const snapshot = makeWatcherSettlementSnapshotV1({
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

export type GenuineW16SpawnAuthorityFixtureV1 = Readonly<{
  spawn: Readonly<{
    result: ReturnType<typeof evaluateWatcherSettlementIndexerV1Raw>;
    context: WatcherSettlementResultVerificationContextV1;
    parsed: ReturnType<typeof evaluateWatcherSettlementIndexerV1Raw>;
    observation: WatcherSettlementObservationV1;
    historyEntryDigests: readonly string[];
  }>;
  dispose: () => Promise<void>;
}>;

/** Builds a real spawn transaction, finality evidence, and parser-accepted W16 authority. */
export const createGenuineW16SpawnAuthorityV1 =
  async (): Promise<GenuineW16SpawnAuthorityFixtureV1> => {
    const leaseOwner = await initializeOpaqueSettlementTransports();
    try {
      const scenario = spawnSequence();
      const result = evaluateWatcherSettlementIndexerV1(
        policy,
        scenario.bootstrapState,
        scenario.spawnEvidence.observation,
        scenario.spawnEvidence.context,
      );
      const context: WatcherSettlementResultVerificationContextV1 = {
        policy,
        previousState: scenario.bootstrapState,
        observation: scenario.spawnEvidence.observation,
        publicContext: scenario.spawnEvidence.context,
        transportAttestations: Object.freeze([...transportContexts]),
      };
      const parsed = parseWatcherSettlementIndexerResultV1(result, context);
      if (
        parsed === null ||
        parsed.action !== "accept" ||
        parsed.protocolDecision !== "indexed" ||
        parsed.state === null
      )
        throw new Error("genuine W16 spawn authority was not accepted");
      const historyEntryDigests = parsed.state.activeHistory
        .filter(
          (entry) =>
            entry.observation.observationDigest ===
            scenario.spawnEvidence.observation.observationDigest,
        )
        .map(({ stateDigest }) => stateDigest);
      if (historyEntryDigests.length === 0)
        throw new Error("genuine W16 spawn authority has no history");
      return Object.freeze({
        spawn: Object.freeze({
          result,
          context,
          parsed,
          observation: scenario.spawnEvidence.observation,
          historyEntryDigests: Object.freeze(historyEntryDigests),
        }),
        dispose: () => disposeOpaqueSettlementTransports(leaseOwner),
      });
    } catch (error) {
      await disposeOpaqueSettlementTransports(leaseOwner);
      throw error;
    }
  };

export type GenuineW15SettlementEventRecordV1 = Readonly<{
  outRef: string;
  outputCborHex: string;
  datumCborHex: string;
  assetNameHex: string;
  policyId: string;
}>;

export type GenuineW16SettlementAuthorityV1 = Readonly<{
  result: ReturnType<typeof evaluateWatcherSettlementIndexerV1Raw>;
  context: WatcherSettlementResultVerificationContextV1;
  parsed: ReturnType<typeof evaluateWatcherSettlementIndexerV1Raw>;
  observation: WatcherSettlementObservationV1;
  historyEntryDigests: readonly string[];
}>;

export type GenuineW16SettlementAuthorityFixtureSetV1 = Readonly<{
  spawn: GenuineW16SettlementAuthorityV1;
  absorbToReserve: GenuineW16SettlementAuthorityV1;
  initializePayout: GenuineW16SettlementAuthorityV1;
  refundWithdrawal: GenuineW16SettlementAuthorityV1;
  dispose: () => Promise<void>;
}>;

export type GenuineW16SettlementAuthorityFixtureInputV1 = Readonly<{
  /** Test-only root override used to prove setup-failure lease cleanup. */
  transportFixtureRoot?: string;
  deposit: GenuineW15SettlementEventRecordV1;
  withdrawal: GenuineW15SettlementEventRecordV1;
}>;

type WithdrawalDatumView = Readonly<{
  event: Readonly<{
    info: Readonly<{
      body: Readonly<{
        l2_value: Map<string, Map<string, bigint>>;
        l1_address: unknown;
        l1_datum: unknown;
      }>;
    }>;
  }>;
  refund_address: unknown;
  refund_datum: unknown;
}>;

const eventRecordOutRef = (
  event: GenuineW15SettlementEventRecordV1,
): Readonly<{ transactionId: string; outputIndex: bigint }> => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/.exec(event.outRef);
  if (match === null)
    throw new Error(`genuine W15 event has malformed out-ref: ${event.outRef}`);
  return { transactionId: match[1]!, outputIndex: BigInt(match[2]!) };
};

const eventRecordOutput = (
  event: GenuineW15SettlementEventRecordV1,
  expectedPolicyId: string,
): CML.TransactionOutput => {
  if (
    event.policyId !== expectedPolicyId ||
    !/^(?:[0-9a-f]{2}){1,32}$/.test(event.assetNameHex)
  )
    throw new Error("genuine W15 event is not bound to the expected policy");
  eventRecordOutRef(event);
  const output = CML.TransactionOutput.from_cbor_hex(event.outputCborHex);
  const datum = CML.PlutusData.from_cbor_hex(event.datumCborHex);
  if (
    output.to_canonical_cbor_hex() !== event.outputCborHex ||
    datum.to_canonical_cbor_hex() !== event.datumCborHex ||
    output.datum()?.as_datum()?.to_canonical_cbor_hex() !==
      event.datumCborHex ||
    output
      .amount()
      .multi_asset()
      .get(
        CML.ScriptHash.from_hex(event.policyId),
        CML.AssetName.from_hex(event.assetNameHex),
      ) !== 1n
  )
    throw new Error("genuine W15 event record is detached from its output");
  return output;
};

const eventValueAssets = (
  output: CML.TransactionOutput,
  omittedUnit: Readonly<{ policyId: string; assetName: string }> | null,
): readonly { policyId: string; assetName: string; quantity: bigint }[] => {
  const result: { policyId: string; assetName: string; quantity: bigint }[] =
    [];
  const multiAsset = output.amount().multi_asset();
  const policies = multiAsset.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policyHash = policies.get(policyIndex);
    const policyId = policyHash.to_hex();
    const assets = multiAsset.get_assets(policyHash);
    if (assets === undefined) throw new Error("W15 event policy disappeared");
    const names = assets.keys();
    for (let assetIndex = 0; assetIndex < names.len(); assetIndex += 1) {
      const name = names.get(assetIndex);
      const assetName = name.to_hex();
      const quantity = assets.get(name);
      if (quantity === undefined || quantity <= 0n)
        throw new Error("W15 event contains a non-positive asset");
      if (
        omittedUnit === null ||
        omittedUnit.policyId !== policyId ||
        omittedUnit.assetName !== assetName
      )
        result.push({ policyId, assetName, quantity });
    }
  }
  return Object.freeze(result);
};

const protocolEventUtxo = (
  event: GenuineW15SettlementEventRecordV1,
  role: "deposit" | "withdrawal",
): WatcherProtocolUtxoV1 => ({
  outRef: event.outRef,
  role,
  chainPointId: h32("00"),
  output: makeWatcherDurablePayloadV1(event.outputCborHex),
});

const settlementAuthority = (
  previousState: WatcherSettlementIndexerStateV1 | null,
  evidence: Bundle,
  activePolicy: WatcherSettlementIndexerPolicyV1,
  expectedKind: WatcherSettlementTransitionKindV1,
): GenuineW16SettlementAuthorityV1 => {
  const result = evaluateWatcherSettlementIndexerV1(
    activePolicy,
    previousState,
    evidence.observation,
    evidence.context,
  );
  const context: WatcherSettlementResultVerificationContextV1 = {
    policy: activePolicy,
    previousState,
    observation: evidence.observation,
    publicContext: evidence.context,
    transportAttestations: Object.freeze([...transportContexts]),
  };
  const parsed = parseWatcherSettlementIndexerResultV1(result, context);
  if (
    parsed === null ||
    parsed.action !== "accept" ||
    parsed.protocolDecision !== "indexed" ||
    parsed.state === null ||
    evidence.observation.transition.kind !== expectedKind
  )
    throw new Error(`genuine W16 ${expectedKind} authority was not accepted`);
  const historyEntryDigests = parsed.state.activeHistory
    .filter(
      (entry) =>
        entry.observation.observationDigest ===
          evidence.observation.observationDigest &&
        entry.observation.transition.kind === expectedKind,
    )
    .map(({ stateDigest }) => stateDigest);
  if (historyEntryDigests.length === 0)
    throw new Error(`genuine W16 ${expectedKind} authority has no history`);
  return Object.freeze({
    result,
    context,
    parsed,
    observation: evidence.observation,
    historyEntryDigests: Object.freeze(historyEntryDigests),
  });
};

const pendingWithdrawalSnapshot = (
  activePolicy: WatcherSettlementIndexerPolicyV1,
  event: GenuineW15SettlementEventRecordV1,
  durable: WatcherProtocolUtxoV1,
): WatcherSettlementSnapshotV1 => {
  const resource = makeWatcherSettlementResourceFromProtocolUtxoV1(
    activePolicy,
    durable,
  );
  const subject = makeWatcherSettlementSubjectV1({
    subjectId: event.assetNameHex,
    subjectKind: "withdrawal",
    status: "withdrawal_pending",
    resourceOutRef: event.outRef,
    relatedSubjectId: null,
    resolutionTime: null,
    operatorVkey: null,
    attempt: "0",
    terminalTransactionHash: null,
    failureCode: null,
  });
  const snapshot = makeWatcherSettlementSnapshotV1({
    resources: resource === null ? [] : [resource],
    subjects: subject === null ? [] : [subject],
  });
  if (resource === null || subject === null || snapshot === null)
    throw new Error("genuine W15 withdrawal did not create a W16 snapshot");
  return snapshot;
};

const bootstrapEventScenario = (
  event: GenuineW15SettlementEventRecordV1,
  durable: WatcherProtocolUtxoV1,
  snapshot: WatcherSettlementSnapshotV1,
) => {
  const scenario = scenarioBootstrap([durable]);
  const evidence = bundle({
    policyOverride: scenario.policy,
    kind: "bootstrap",
    previousState: null,
    snapshot,
    protocolUtxos: [durable],
    sourceDurableStore: scenario.store,
  });
  const state = accepted(null, evidence, scenario.policy);
  if (
    durable.outRef !== event.outRef ||
    durable.output.cborHex !== event.outputCborHex
  )
    throw new Error("W16 bootstrap detached the genuine W15 event");
  return { scenario, evidence, state };
};

const absorbToReserveAuthority = (
  event: GenuineW15SettlementEventRecordV1,
): GenuineW16SettlementAuthorityV1 => {
  const inputOutput = eventRecordOutput(event, depositPolicyId);
  if (
    inputOutput.address().to_hex() !==
    enterpriseScriptAddress(applied.depositSpend!)
  )
    throw new Error("genuine W15 deposit has the wrong address");
  const durable = protocolEventUtxo(event, "deposit");
  const bootstrap = bootstrapEventScenario(event, durable, emptySnapshot());
  const reserveOutput = cardanoOutput(
    bootstrap.scenario.policy.reserveAddressHex,
    inputOutput.amount().coin(),
    eventValueAssets(inputOutput, {
      policyId: event.policyId,
      assetName: event.assetNameHex,
    }),
    null,
  );
  const mint = [
    {
      policyId: event.policyId,
      assetName: event.assetNameHex,
      quantity: -1n,
    },
  ] as const;
  const transactionHash = computeHash32(
    Buffer.from(transactionBody([event.outRef], [reserveOutput], mint), "hex"),
  ).toString("hex");
  const reserveOutRef = `${transactionHash}#0`;
  const reserveDurable: WatcherProtocolUtxoV1 = {
    outRef: reserveOutRef,
    role: "reserve",
    chainPointId: h32("00"),
    output: makeWatcherDurablePayloadV1(reserveOutput),
  };
  const reserveResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
    bootstrap.scenario.policy,
    reserveDurable,
  );
  const reserveSubject = makeWatcherSettlementSubjectV1({
    subjectId: `reserve-deposit-${event.assetNameHex}`,
    subjectKind: "reserve",
    status: "reserve_active",
    resourceOutRef: reserveOutRef,
    relatedSubjectId: event.assetNameHex,
    resolutionTime: null,
    operatorVkey: null,
    attempt: "0",
    terminalTransactionHash: null,
    failureCode: null,
  });
  const snapshot = makeWatcherSettlementSnapshotV1({
    resources: reserveResource === null ? [] : [reserveResource],
    subjects: reserveSubject === null ? [] : [reserveSubject],
  });
  if (reserveResource === null || reserveSubject === null || snapshot === null)
    throw new Error("genuine W16 reserve output was not recognized");
  const membershipProof = {
    domain: "DepositsRootDomain" as const,
    root: ROOT,
    phas_root: h32("cc"),
    count: 1n,
    key: "aa",
    value: "bb",
    proof: [],
  };
  const evidence = bundle({
    policyOverride: bootstrap.scenario.policy,
    kind: "absorb_to_reserve",
    previousState: bootstrap.state,
    snapshot,
    inputs: [event.outRef],
    outputHexes: [reserveOutput],
    mint,
    redeemers: [
      {
        purpose: "spend",
        index: "0",
        cborHex: Data.to(
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
        ),
      },
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            BurnEventNFT: {
              nonce_asset_name: event.assetNameHex,
              witness_unregistration_redeemer_index: 2n,
            },
          },
          UserEventMintRedeemer,
        ),
      },
      {
        purpose: "certificate",
        index: "0",
        cborHex: Data.to(
          { MintOrBurn: { targetPolicy: event.policyId } },
          UserEventWitnessPublishRedeemer,
        ),
      },
      {
        purpose: "withdrawal",
        index: "0",
        cborHex: Data.to(
          [
            membershipProof.phas_root,
            membershipProof.key,
            membershipProof.value,
            membershipProof.proof,
          ] as never,
          Data.Tuple([
            MerkleRootSchema,
            Data.Bytes(),
            Data.Bytes(),
            ProofSchema,
          ]),
        ),
      },
    ],
    transition: {
      subjectId: event.assetNameHex,
      consumedOutRefs: [event.outRef],
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
      exactMint: mint.map((asset) => ({
        ...asset,
        quantity: asset.quantity.toString(),
      })),
    },
    restartBindings: [bootstrap.evidence.restartBinding],
    protocolUtxos: [durable, reserveDurable],
  });
  return settlementAuthority(
    bootstrap.state,
    evidence,
    bootstrap.scenario.policy,
    "absorb_to_reserve",
  );
};

const addressCredential = (
  value: unknown,
): Readonly<{ kind: "key" | "script"; hash: string }> | null => {
  if (typeof value !== "object" || value === null) return null;
  const record = value as Record<string, unknown>;
  for (const [field, kind] of [
    ["PublicKeyCredential", "key"],
    ["ScriptCredential", "script"],
  ] as const) {
    const candidate = record[field];
    if (
      Array.isArray(candidate) &&
      candidate.length === 1 &&
      typeof candidate[0] === "string" &&
      /^[0-9a-f]{56}$/.test(candidate[0])
    )
      return { kind, hash: candidate[0] };
  }
  return null;
};

const addressDataToHex = (value: unknown): string => {
  if (typeof value !== "object" || value === null)
    throw new Error("W15 withdrawal contains an invalid address");
  const record = value as {
    paymentCredential?: unknown;
    stakeCredential?: unknown;
  };
  const payment = addressCredential(record.paymentCredential);
  if (payment === null)
    throw new Error("W15 withdrawal contains an invalid payment credential");
  if (record.stakeCredential === null)
    return `${payment.kind === "key" ? "60" : "70"}${payment.hash}`;
  const stakeRecord = record.stakeCredential as { Inline?: unknown };
  const inline = stakeRecord?.Inline;
  const stake =
    Array.isArray(inline) && inline.length === 1
      ? addressCredential(inline[0])
      : null;
  if (stake === null)
    throw new Error("W15 withdrawal contains an invalid stake credential");
  const addressType =
    payment.kind === "key"
      ? stake.kind === "key"
        ? 0
        : 2
      : stake.kind === "key"
        ? 1
        : 3;
  return `${addressType.toString(16)}0${payment.hash}${stake.hash}`;
};

const withdrawalDatumView = (
  event: GenuineW15SettlementEventRecordV1,
): WithdrawalDatumView =>
  Data.from(event.datumCborHex, WithdrawalOrderDatum) as WithdrawalDatumView;

const payoutTargetValue = (
  value: Map<string, Map<string, bigint>>,
): Readonly<{
  lovelace: bigint;
  assets: readonly { policyId: string; assetName: string; quantity: bigint }[];
}> => {
  let lovelace = 0n;
  const assets: { policyId: string; assetName: string; quantity: bigint }[] =
    [];
  for (const [policyId, policyAssets] of value) {
    for (const [assetName, quantity] of policyAssets) {
      if (quantity < 0n) throw new Error("W15 withdrawal target is negative");
      if (policyId === "" && assetName === "") lovelace = quantity;
      else if (policyId !== "" && quantity > 0n)
        assets.push({ policyId, assetName, quantity });
      else if (assetName !== "")
        throw new Error("W15 withdrawal has a malformed native asset");
    }
  }
  return { lovelace, assets: Object.freeze(assets) };
};

const initializePayoutAuthority = (
  event: GenuineW15SettlementEventRecordV1,
): GenuineW16SettlementAuthorityV1 => {
  const inputOutput = eventRecordOutput(event, withdrawalPolicyId);
  if (inputOutput.address().to_hex() !== policy.withdrawalAddressHex)
    throw new Error("genuine W15 withdrawal has the wrong address");
  const durable = protocolEventUtxo(event, "withdrawal");
  const sourceSnapshot = pendingWithdrawalSnapshot(policy, event, durable);
  const bootstrap = bootstrapEventScenario(event, durable, sourceSnapshot);
  const datum = withdrawalDatumView(event);
  const payoutDatum = Data.to(
    {
      l2_value: datum.event.info.body.l2_value,
      l1_address: datum.event.info.body.l1_address,
      l1_datum: datum.event.info.body.l1_datum,
    } as never,
    PayoutDatum,
  );
  const target = payoutTargetValue(datum.event.info.body.l2_value);
  const payoutOutput = cardanoOutput(
    bootstrap.scenario.policy.payoutAddressHex,
    target.lovelace,
    [
      ...target.assets,
      {
        policyId: payoutPolicyId,
        assetName: event.assetNameHex,
        quantity: 1n,
      },
    ],
    payoutDatum,
  );
  const mint = [
    {
      policyId: payoutPolicyId,
      assetName: event.assetNameHex,
      quantity: 1n,
    },
    {
      policyId: event.policyId,
      assetName: event.assetNameHex,
      quantity: -1n,
    },
  ] as const;
  const transactionHash = computeHash32(
    Buffer.from(transactionBody([event.outRef], [payoutOutput], mint), "hex"),
  ).toString("hex");
  const payoutOutRef = `${transactionHash}#0`;
  const payoutDurable: WatcherProtocolUtxoV1 = {
    outRef: payoutOutRef,
    role: "payout",
    chainPointId: h32("00"),
    output: makeWatcherDurablePayloadV1(payoutOutput),
  };
  const payoutResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
    bootstrap.scenario.policy,
    payoutDurable,
  );
  const payoutSubject = makeWatcherSettlementSubjectV1({
    subjectId: event.assetNameHex,
    subjectKind: "payout",
    status: "payout_funded",
    resourceOutRef: payoutOutRef,
    relatedSubjectId: event.assetNameHex,
    resolutionTime: null,
    operatorVkey: null,
    attempt: "0",
    terminalTransactionHash: null,
    failureCode: null,
  });
  const resolvedWithdrawal = makeWatcherSettlementSubjectV1({
    subjectId: event.assetNameHex,
    subjectKind: "withdrawal",
    status: "resolved",
    resourceOutRef: null,
    relatedSubjectId: null,
    resolutionTime: null,
    operatorVkey: null,
    attempt: "0",
    terminalTransactionHash: transactionHash,
    failureCode: null,
  });
  const snapshot = makeWatcherSettlementSnapshotV1({
    resources: payoutResource === null ? [] : [payoutResource],
    subjects: [payoutSubject, resolvedWithdrawal].filter(
      (subject): subject is WatcherSettlementSubjectV1 => subject !== null,
    ),
  });
  if (
    payoutResource === null ||
    payoutSubject === null ||
    resolvedWithdrawal === null ||
    snapshot === null
  )
    throw new Error("genuine W16 payout output was not recognized");
  const membershipProof = {
    domain: "WithdrawalsRootDomain" as const,
    root: ROOT,
    phas_root: h32("ca"),
    count: 1n,
    key: "aa",
    value: "bb",
    proof: [],
  };
  const withdrawalOutRef = eventRecordOutRef(event);
  const evidence = bundle({
    policyOverride: bootstrap.scenario.policy,
    kind: "initialize_payout",
    previousState: bootstrap.state,
    snapshot,
    inputs: [event.outRef],
    outputHexes: [payoutOutput],
    mint,
    redeemers: [
      {
        purpose: "spend",
        index: "0",
        cborHex: Data.to(
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
        ),
      },
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            MintPayout: {
              withdrawal_utxo_out_ref: withdrawalOutRef,
              withdrawal_input_index: 0n,
              withdrawal_spend_redeemer_index: 0n,
              hub_ref_input_index: 0n,
            },
          },
          PayoutMintRedeemer,
        ),
      },
      {
        purpose: "mint",
        index: "1",
        cborHex: Data.to(
          {
            BurnEventNFT: {
              nonce_asset_name: event.assetNameHex,
              witness_unregistration_redeemer_index: 3n,
            },
          },
          UserEventMintRedeemer,
        ),
      },
      {
        purpose: "certificate",
        index: "0",
        cborHex: Data.to(
          { MintOrBurn: { targetPolicy: event.policyId } },
          UserEventWitnessPublishRedeemer,
        ),
      },
      {
        purpose: "withdrawal",
        index: "0",
        cborHex: Data.to(
          [
            membershipProof.phas_root,
            membershipProof.key,
            membershipProof.value,
            membershipProof.proof,
          ] as never,
          Data.Tuple([
            MerkleRootSchema,
            Data.Bytes(),
            Data.Bytes(),
            ProofSchema,
          ]),
        ),
      },
    ],
    transition: {
      subjectId: event.assetNameHex,
      relatedSubjectId: event.assetNameHex,
      consumedOutRefs: [event.outRef],
      producedOutRefs: [payoutOutRef],
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
      exactMint: mint.map((asset) => ({
        ...asset,
        quantity: asset.quantity.toString(),
      })),
    },
    restartBindings: [bootstrap.evidence.restartBinding],
    protocolUtxos: [payoutDurable],
  });
  return settlementAuthority(
    bootstrap.state,
    evidence,
    bootstrap.scenario.policy,
    "initialize_payout",
  );
};

const refundDatumOption = (value: unknown): CML.DatumOption | null => {
  if (value === "NoDatum") return null;
  if (typeof value !== "object" || value === null)
    throw new Error("W15 withdrawal has an invalid refund datum");
  if ("DatumHash" in value) {
    const hash = (value as { DatumHash: { hash?: unknown } }).DatumHash.hash;
    if (typeof hash !== "string" || !/^[0-9a-f]{64}$/.test(hash))
      throw new Error("W15 withdrawal has an invalid refund datum hash");
    return CML.DatumOption.new_hash(CML.DatumHash.from_hex(hash));
  }
  if ("InlineDatum" in value) {
    const data = (value as { InlineDatum: { data?: unknown } }).InlineDatum
      .data;
    return CML.DatumOption.new_datum(
      CML.PlutusData.from_cbor_hex(Data.to(data as never)),
    );
  }
  throw new Error("W15 withdrawal has an unsupported refund datum");
};

const refundWithdrawalAuthority = (
  event: GenuineW15SettlementEventRecordV1,
): GenuineW16SettlementAuthorityV1 => {
  const inputOutput = eventRecordOutput(event, withdrawalPolicyId);
  if (inputOutput.address().to_hex() !== policy.withdrawalAddressHex)
    throw new Error("genuine W15 withdrawal has the wrong address");
  const durable = protocolEventUtxo(event, "withdrawal");
  const sourceSnapshot = pendingWithdrawalSnapshot(policy, event, durable);
  const bootstrap = bootstrapEventScenario(event, durable, sourceSnapshot);
  const datum = withdrawalDatumView(event);
  const refundOutput = CML.TransactionOutput.new(
    CML.Address.from_hex(addressDataToHex(datum.refund_address)),
    cardanoValue(
      inputOutput.amount().coin(),
      eventValueAssets(inputOutput, {
        policyId: event.policyId,
        assetName: event.assetNameHex,
      }),
    ),
    refundDatumOption(datum.refund_datum),
    null,
  ).to_canonical_cbor_hex();
  const mint = [
    {
      policyId: event.policyId,
      assetName: event.assetNameHex,
      quantity: -1n,
    },
  ] as const;
  const transactionHash = computeHash32(
    Buffer.from(transactionBody([event.outRef], [refundOutput], mint), "hex"),
  ).toString("hex");
  const refunded = makeWatcherSettlementSubjectV1({
    subjectId: event.assetNameHex,
    subjectKind: "withdrawal",
    status: "refunded",
    resourceOutRef: null,
    relatedSubjectId: null,
    resolutionTime: null,
    operatorVkey: null,
    attempt: "0",
    terminalTransactionHash: transactionHash,
    failureCode: null,
  });
  const snapshot = makeWatcherSettlementSnapshotV1({
    resources: [],
    subjects: refunded === null ? [] : [refunded],
  });
  if (refunded === null || snapshot === null)
    throw new Error("genuine W16 refund subject was not recognized");
  const membershipProof = {
    domain: "WithdrawalsRootDomain" as const,
    root: ROOT,
    phas_root: h32("db"),
    count: 1n,
    key: "aa",
    value: "bb",
    proof: [],
  };
  const evidence = bundle({
    policyOverride: bootstrap.scenario.policy,
    kind: "refund_withdrawal",
    previousState: bootstrap.state,
    snapshot,
    inputs: [event.outRef],
    outputHexes: [refundOutput],
    mint,
    redeemers: [
      {
        purpose: "spend",
        index: "0",
        cborHex: Data.to(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            settlement_ref_input_index: 1n,
            burn_redeemer_index: 1n,
            payout_mint_redeemer_index: 0n,
            membership_proof: membershipProof,
            inclusion_proof_script_withdraw_redeemer_index: 3n,
            purpose: {
              Refund: { validity_override: "IncorrectWithdrawalSignature" },
            },
          },
          WithdrawalSpendRedeemer,
        ),
      },
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            BurnEventNFT: {
              nonce_asset_name: event.assetNameHex,
              witness_unregistration_redeemer_index: 2n,
            },
          },
          UserEventMintRedeemer,
        ),
      },
      {
        purpose: "certificate",
        index: "0",
        cborHex: Data.to(
          { MintOrBurn: { targetPolicy: event.policyId } },
          UserEventWitnessPublishRedeemer,
        ),
      },
      {
        purpose: "withdrawal",
        index: "0",
        cborHex: Data.to(
          [
            membershipProof.phas_root,
            membershipProof.key,
            membershipProof.value,
            membershipProof.proof,
          ] as never,
          Data.Tuple([
            MerkleRootSchema,
            Data.Bytes(),
            Data.Bytes(),
            ProofSchema,
          ]),
        ),
      },
    ],
    transition: {
      subjectId: event.assetNameHex,
      consumedOutRefs: [event.outRef],
      requiredRedeemers: [
        {
          purpose: "spend",
          index: "0",
          schema: "withdrawal_spend",
          constructor: "Refund",
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
      exactMint: mint.map((asset) => ({
        ...asset,
        quantity: asset.quantity.toString(),
      })),
    },
    restartBindings: [bootstrap.evidence.restartBinding],
    protocolUtxos: [],
  });
  return settlementAuthority(
    bootstrap.state,
    evidence,
    bootstrap.scenario.policy,
    "refund_withdrawal",
  );
};

/**
 * Builds parser-accepted W16 authorities that consume the caller's exact
 * genuine W15 deposit and withdrawal records.
 */
export const createGenuineW16SettlementAuthoritiesV1 = async (
  input: GenuineW16SettlementAuthorityFixtureInputV1,
): Promise<GenuineW16SettlementAuthorityFixtureSetV1> => {
  const leaseOwner = await initializeOpaqueSettlementTransports(
    input.transportFixtureRoot,
  );
  try {
    const spawnScenario = spawnSequence();
    return Object.freeze({
      spawn: settlementAuthority(
        spawnScenario.bootstrapState,
        spawnScenario.spawnEvidence,
        policy,
        "spawn_settlement",
      ),
      absorbToReserve: absorbToReserveAuthority(input.deposit),
      initializePayout: initializePayoutAuthority(input.withdrawal),
      refundWithdrawal: refundWithdrawalAuthority(input.withdrawal),
      dispose: () => disposeOpaqueSettlementTransports(leaseOwner),
    });
  } catch (error) {
    await disposeOpaqueSettlementTransports(leaseOwner);
    throw error;
  }
};

export type W16AuthorityScenarioInputV1 = Readonly<{
  policy: WatcherSettlementIndexerPolicyV1;
  previousState: WatcherSettlementIndexerStateV1 | null;
  observationInput: Parameters<typeof makeWatcherSettlementObservationV1>[0];
  publicContext: WatcherSettlementPublicContextV1;
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
}>;
export type W16AcceptedAuthorityScenarioV1 = Readonly<{
  result: ReturnType<typeof evaluateWatcherSettlementIndexerV1Raw>;
  context: WatcherSettlementResultVerificationContextV1;
  parsed: ReturnType<typeof evaluateWatcherSettlementIndexerV1Raw>;
  observation: WatcherSettlementObservationV1;
  observationInput: Parameters<typeof makeWatcherSettlementObservationV1>[0];
  historyEntryDigests: readonly string[];
}>;
export const replayAcceptedW16AuthorityScenarioV1 = (
  input: W16AuthorityScenarioInputV1,
  expectedKind: WatcherSettlementTransitionKindV1,
): W16AcceptedAuthorityScenarioV1 => {
  const observation = makeWatcherSettlementObservationV1(
    input.observationInput,
  );
  if (observation === null || observation.transition.kind !== expectedKind)
    throw new Error(`W16 authority scenario did not produce ${expectedKind}`);
  const result = evaluateWatcherSettlementIndexerV1Raw(
    input.policy,
    input.previousState,
    observation,
    input.publicContext,
    input.transportAttestations,
  );
  const context: WatcherSettlementResultVerificationContextV1 = {
    policy: input.policy,
    previousState: input.previousState,
    observation,
    publicContext: input.publicContext,
    transportAttestations: input.transportAttestations,
  };
  const parsed = parseWatcherSettlementIndexerResultV1Raw(result, context);
  if (
    parsed === null ||
    parsed.action !== "accept" ||
    parsed.protocolDecision !== "indexed" ||
    parsed.state === null
  )
    throw new Error(
      `W16 authority scenario was not parser-accepted: ${expectedKind}`,
    );
  const historyEntryDigests = parsed.state.activeHistory
    .filter(
      (entry) =>
        entry.observation.observationDigest === observation.observationDigest &&
        entry.observation.transition.kind === expectedKind,
    )
    .map(({ stateDigest }) => stateDigest);
  if (historyEntryDigests.length === 0)
    throw new Error(`W16 authority scenario has no ${expectedKind} history`);
  return Object.freeze({
    result,
    context,
    parsed,
    observation,
    observationInput: input.observationInput,
    historyEntryDigests: Object.freeze(historyEntryDigests),
  });
};
export const replayGenuineSpawnSettlementAuthorityScenarioV1 = (
  input: W16AuthorityScenarioInputV1,
): W16AcceptedAuthorityScenarioV1 =>
  replayAcceptedW16AuthorityScenarioV1(input, "spawn_settlement");
export const replayGenuineRefundWithdrawalAuthorityScenarioV1 = (
  input: W16AuthorityScenarioInputV1,
): W16AcceptedAuthorityScenarioV1 =>
  replayAcceptedW16AuthorityScenarioV1(input, "refund_withdrawal");
export const replayGenuineAbsorbToReserveAuthorityScenarioV1 = (
  input: W16AuthorityScenarioInputV1,
): W16AcceptedAuthorityScenarioV1 =>
  replayAcceptedW16AuthorityScenarioV1(input, "absorb_to_reserve");

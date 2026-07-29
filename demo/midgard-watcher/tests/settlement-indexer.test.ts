import { createHash, generateKeyPairSync, sign } from "node:crypto";

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
import {
  CML,
  Constr,
  Data,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "../../midgard-core/src/consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "../../midgard-core/src/da-transport.js";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
  makeDeploymentMarkerV1,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../src/config.js";
import {
  makeWatcherDeploymentIdentitySignaturePayloadV1,
  verifyWatcherDeploymentIdentityV1,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicyV1,
} from "../src/deployment-identity.js";
import {
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  makeWatcherDurablePayloadV1,
  makeWatcherDurableStoreV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
  type WatcherProtocolUtxoV1,
} from "../src/durable-store.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
} from "../src/finality-engine.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherAuthenticatedL1ProviderV1,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "../src/multi-provider-consistency.js";
import {
  evaluateWatcherRollbackV1,
  makeWatcherRollbackBootstrapStateV1,
} from "../src/rollback-engine.js";
import {
  evaluateWatcherSettlementIndexerV1,
  makeWatcherSettlementIndexerPolicyV1,
  makeWatcherSettlementObservationV1,
  makeWatcherSettlementResourceFromProtocolUtxoV1,
  makeWatcherSettlementSnapshotV1,
  makeWatcherSettlementSubjectV1,
  parseWatcherSettlementIndexerResultV1,
  parseWatcherSettlementIndexerStateV1,
  WATCHER_SETTLEMENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  type WatcherSettlementIndexerPolicyV1,
  type WatcherSettlementIndexerStateV1,
  type WatcherSettlementObservationV1,
  type WatcherSettlementPublicContextV1,
  type WatcherSettlementSnapshotV1,
  type WatcherSettlementSubjectV1,
  type WatcherSettlementTransitionKindV1,
  type WatcherSettlementTransitionV1,
} from "../src/settlement-indexer.js";
import { canonicalFraudProofCatalogueFixture } from "./canonical-catalogue-fixture.js";

type Mutable = Record<string, any>;

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const ROOT = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const settlementAsset = "aa";
const operator = h28("91");
const HUB_REFERENCE_OUT_REF = `${h32("7a")}#0`;
const SETTLEMENT_REFERENCE_OUT_REF = `${h32("7b")}#0`;
const RELEASE_DIGEST = h32("22");
const BLUEPRINT_HASH = h32("55");
const RULE_BUNDLE_COMMITMENT = h32("44");
const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";
const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const makeDeploymentAuthority = () => {
  const referenceOutRefByContract = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: h32("12"), outputIndex },
      ],
    ),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName, index) => {
      const native = contractName === "referenceScriptAuthMint";
      const script = native
        ? NATIVE_SCRIPT_CBOR
        : (index + 1).toString(16).padStart(2, "0");
      return [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          contract: { type: native ? "Native" : "PlutusV3", cborHex: script },
          scriptHash: native
            ? NATIVE_SCRIPT_HASH
            : validatorToScriptHash({ type: "PlutusV3", script }),
        },
      ];
    }),
  ) as Mutable;
  contracts.fraudProofCatalogueMint.fraudProofCatalogue =
    canonicalFraudProofCatalogueFixture(contracts);
  const referenceScripts = Object.fromEntries(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contractName]) => {
      const outRef = referenceOutRefByContract.get(contractName)!;
      const tokenName =
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
          role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
        ];
      return [
        role,
        {
          status: "confirmed",
          roleUnit:
            NATIVE_SCRIPT_HASH + Buffer.from(tokenName, "utf8").toString("hex"),
          scriptHash: contracts[contractName].scriptHash,
          outRef: `${outRef.txHash}#${outRef.outputIndex.toString()}`,
        },
      ];
    }),
  );
  const parameters = {
    maxTxSize: 16_384,
    maxValueSize: 5_000,
    maxTxExUnits: { memory: "16500000", steps: "10000000000" },
  };
  const identity: Mutable = {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: computeDeploymentManifestV1JsonDigest(parameters),
    },
    genesis: {
      headerHash: h28("00"),
      utxoSetDigest: computeDeploymentManifestV1JsonDigest([]),
    },
    createdAt: "2026-07-28T00:00:00.000Z",
    updatedAt: "2026-07-28T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1vcanonical",
    hubOracleOneShot: {
      txHash: h32("11"),
      outputIndex: 0,
      outRef: `${h32("11")}#0`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId: NATIVE_SCRIPT_HASH,
      nativeScript: {
        type: "Native",
        cborHex: NATIVE_SCRIPT_CBOR,
        expiresAtSlot: 1,
        expiresAtUnixTime: 1,
        timelockDurationMs: 1,
      },
      tokenNames: DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts,
    referenceScripts,
    da: {
      committeeVkeys: [h32("44")],
      committeeSignersHash: DA_SIGNERS_HASH,
      threshold: 1,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS_V1,
        retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
      },
    },
    proofEvidence: {
      digest: RELEASE_DIGEST,
      blueprintHash: BLUEPRINT_HASH,
    },
    steps: Object.fromEntries(
      DEPLOYMENT_MANIFEST_V1_STEP_NAMES.map((stepName) => [
        stepName,
        {
          status:
            stepName === "prepareHubOracleNonce" ||
            stepName === "deployNodeRuntimeReferenceScripts" ||
            stepName === "initProtocol"
              ? "complete"
              : "pending",
        },
      ]),
    ),
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
    },
  };
  const manifest: Mutable = {
    ...identity,
    manifestId: computeDeploymentManifestV1Id(identity),
  };
  const programCommitments = {
    "validation-machine-v1": h32("88"),
    "transition-order-v1": h32("99"),
  };
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestV1JsonDigest(manifest.da),
    },
    releaseEvidence: {
      digest: RELEASE_DIGEST,
      blueprintHash: BLUEPRINT_HASH,
    },
  };
  const { privateKey, publicKey } = generateKeyPairSync("ed25519");
  const publicKeySpkiDerHex = publicKey
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = sha256(Buffer.from(publicKeySpkiDerHex, "hex"));
  const signedIdentity: Mutable = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: "",
    },
  };
  signedIdentity.attestation.signature = sign(
    null,
    makeWatcherDeploymentIdentitySignaturePayloadV1(
      manifest.manifestId,
      releaseBindings,
    ),
    privateKey,
  ).toString("hex");
  const deploymentPolicy: WatcherDeploymentIdentityPolicyV1 = {
    network: "Preprod",
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: Object.fromEntries(
      DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((name) => [
        name,
        contracts[name].scriptHash,
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        (role) => [
          role,
          {
            scriptHash: manifest.referenceScripts[role].scriptHash,
            outRef: manifest.referenceScripts[role].outRef,
          },
        ],
      ),
    ),
    fraudProofCatalogue: {
      root: contracts.fraudProofCatalogueMint.fraudProofCatalogue.root,
      categories: Object.fromEntries(
        Object.entries(
          contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories,
        ).map(([name, value]: [string, any]) => [
          name,
          { categoryId: value.categoryId, scriptHash: value.scriptHash },
        ]),
      ),
    } as WatcherDeploymentIdentityPolicyV1["fraudProofCatalogue"],
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    releaseEvidenceDigest: RELEASE_DIGEST,
    blueprintHash: BLUEPRINT_HASH,
  };
  const trustRoots = [{ trustRootId, publicKeySpkiDerHex }];
  const marker = makeDeploymentMarkerV1(manifest.manifestId);
  const result = verifyWatcherDeploymentIdentityV1({
    signedIdentity,
    policy: deploymentPolicy,
    trustRoots,
    durableMarker: marker,
  });
  return {
    signedIdentity,
    policy: deploymentPolicy,
    trustRoots,
    result,
    marker,
    contracts,
  };
};

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

const finalityPolicy = makeWatcherFinalityPolicyV1(
  {
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "external_providers",
        providers: [
          {
            identity: "provider-a",
            operatorIdentitySha256: h32("97"),
            endpoint: "https://cardano-a.example",
          },
          {
            identity: "provider-b",
            operatorIdentitySha256: h32("98"),
            endpoint: "https://cardano-b.example",
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
            "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ],
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
    },
    storage: {
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
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

const LOCAL_NODE_ID = "watcher-node-a";
const LOCAL_GENESIS_IDENTITY = h32("76");
const makeLocalFinalityPolicy = (
  queryServices: readonly Readonly<{
    kind: "ogmios";
    identity: string;
    endpoint: string;
  }>[],
) =>
  makeWatcherFinalityPolicyV1(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        source: {
          sourceMode: "local_node",
          authorityNodeId: LOCAL_NODE_ID,
          chainSync: {
            kind: "cardano_node_socket",
            socketPath: "/ipc/node.socket",
            genesisIdentitySha256: LOCAL_GENESIS_IDENTITY,
          },
          queryServices,
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
              "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
          },
        ],
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
      },
      storage: {
        driver: "sqlite",
        path: "/var/lib/midgard-watcher/watcher.sqlite",
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
const localFinalityPolicy = makeLocalFinalityPolicy([]);
const localOgmiosFinalityPolicy = makeLocalFinalityPolicy([
  {
    kind: "ogmios",
    identity: "local-ogmios",
    endpoint: "http://127.0.0.1:1337",
  },
]);

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
  return body.to_canonical_cbor_hex();
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

const provider = {
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
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
    },
    {
      providerId: "provider-b",
      operatorIdentitySha256: h32("98"),
    },
  ],
} as const;

const localSource = {
  sourceMode: "local_node",
  network: "Preprod",
  authorityNodeId: LOCAL_NODE_ID,
  genesisIdentitySha256: LOCAL_GENESIS_IDENTITY,
  queryServices: [],
} as const;
const localChainSyncProvider = {
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: "local-chain-sync",
  source: {
    sourceMode: "local_node",
    authorityNodeId: LOCAL_NODE_ID,
    surface: "chain_sync",
  },
  authentication: {
    kind: "cardano_node_genesis_v1",
    publicIdentitySha256: LOCAL_GENESIS_IDENTITY,
  },
} as const;
const localOgmiosProvider = {
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: "local-ogmios",
  source: {
    sourceMode: "local_node",
    authorityNodeId: LOCAL_NODE_ID,
    surface: "ogmios",
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256: h32("75"),
  },
} as const;
const localSourceWithOgmios = {
  ...localSource,
  queryServices: [
    {
      kind: "ogmios",
      providerId: localOgmiosProvider.providerId,
    },
  ],
} as const;

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
    publicIdentitySha256: h32(identityByte),
  },
});

type RollbackPoint = Readonly<{
  blockHash: string;
  slot: string;
  blockNo: string;
  depth: string;
}>;

const rawBlock = (
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

const persistedObservation = (value: WatcherNormalizedL1BlockV1) => ({
  observationId: value.observationDigest,
  providerId: value.provider.providerId,
  chainPointId: value.chainPoint.chainPointId,
  payload: makeWatcherDurablePayloadV1(
    encodeWatcherNormalizedL1BlockV1(value).toString("hex"),
  ),
});

const persistedChainPoints = (
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
  previousFinalityState?: unknown;
  rollbackAuthority?: WatcherSettlementPublicContextV1["rollbackAuthority"];
}): Bundle => {
  const activePolicy = input.policyOverride ?? policy;
  serial += 1;
  const isLocal = ["retry", "mark_stuck", "mark_invalid", "rollback"].includes(
    input.kind,
  );
  const bodyHex = transactionBody(
    input.inputs ?? [h32("81") + "#0"],
    input.outputHexes ?? [],
    input.mint ?? [],
  );
  const transactionHash = computeHash32(Buffer.from(bodyHex, "hex")).toString(
    "hex",
  );
  const utxos = (input.outputHexes ?? []).map((outputHex, outputIndex) => {
    const output = CML.TransactionOutput.from_cbor_hex(outputHex);
    const datumHex = output.datum()?.as_datum()?.to_canonical_cbor_hex();
    return {
      outRef: `${transactionHash}#${outputIndex.toString()}`,
      outputIndex: outputIndex.toString(),
      output: makeWatcherL1PublicBytesV1(outputHex),
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
  const l1Observation: Mutable =
    input.l1Observation ??
    ({
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: "Preprod",
      providerId: authenticatedProvider.providerId,
      chainPoint: {
        blockHash: h32(((serial + 50) % 240).toString(16).padStart(2, "0")),
        slot: (1_000 + serial).toString(),
        blockNo: (100 + serial).toString(),
        depth: "1",
      },
      transactions: isLocal
        ? []
        : [
            {
              txHash: transactionHash,
              body: makeWatcherL1PublicBytesV1(bodyHex),
              utxos,
              scripts: [],
              datums: [],
              redeemers: (input.redeemers ?? []).map((redeemer) => ({
                purpose: redeemer.purpose,
                index: redeemer.index,
                bytes: makeWatcherL1PublicBytesV1(redeemer.cborHex),
              })),
            },
          ],
    } satisfies Mutable);
  const normalized = normalizeWatcherL1BlockV1(
    authenticatedProvider,
    l1Observation,
  );
  const previousStore = input.restartBindings?.at(-1)?.durableStore as
    | WatcherDurableStoreV1
    | undefined;
  const sourceStore =
    input.sourceDurableStore ?? previousStore ?? bootstrapStore;
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
    spentAtChainPointId: normalized.chainPoint.chainPointId,
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
    transactionHash: isLocal
      ? null
      : (normalized.transactions[0]?.txHash ?? null),
    sourceObservationDigest: normalized.observationDigest,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
    ),
    predecessorStateDigest: input.previousState?.stateDigest ?? null,
    transition,
    snapshot: input.snapshot,
  });
  expect(observation).not.toBeNull();
  const source = normalized.provider.source;
  const finalityObservations =
    source.sourceMode === "local_node"
      ? [{ authenticatedProvider, l1Observation }]
      : (() => {
          const otherProvider = rollbackProvider("provider-b", "78");
          const otherL1Observation = structuredClone(l1Observation);
          otherL1Observation.providerId = otherProvider.providerId;
          return [
            { authenticatedProvider, l1Observation },
            {
              authenticatedProvider: otherProvider,
              l1Observation: otherL1Observation,
            },
          ];
        })();
  const configuredSource =
    source.sourceMode === "local_node" ? localSource : externalSource;
  const activeFinalityPolicy =
    source.sourceMode === "local_node" ? localFinalityPolicy : finalityPolicy;
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

const scenarioBootstrap = (
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

const spawnSequence = () => {
  const bootstrapEvidence = bundle({
    kind: "bootstrap",
    previousState: null,
    snapshot: emptySnapshot(),
  });
  const bootstrapState = accepted(null, bootstrapEvidence);
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
  });
  const spawnState = accepted(bootstrapState, spawnEvidence);
  return {
    bootstrapEvidence,
    spawnEvidence,
    spawnState,
    outRef,
    durable,
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

  it("accepts one local-node authority with aligned queries and rejects source or query substitution", () => {
    const local = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      authenticatedProvider: localChainSyncProvider,
    });
    expect(local.context.finalityAuthority?.consistency).toMatchObject({
      status: "agreed",
      sourceMode: "local_node",
      authorityNodeId: LOCAL_NODE_ID,
      independentProviderCount: 1,
      queryObservationCount: 0,
    });
    expect(accepted(null, local).snapshot).toEqual(emptySnapshot());

    const contextWithQuery = (
      mutateQuery?: (query: Mutable) => void,
    ): Mutable => {
      const context = structuredClone(local.context) as Mutable;
      const query = structuredClone(context.l1Observation) as Mutable;
      query.providerId = localOgmiosProvider.providerId;
      mutateQuery?.(query);
      const observations = [
        {
          authenticatedProvider: localChainSyncProvider,
          l1Observation: context.l1Observation,
        },
        {
          authenticatedProvider: localOgmiosProvider,
          l1Observation: query,
        },
      ];
      const consistency = evaluateWatcherMultiProviderConsistencyV1(
        localSourceWithOgmios,
        observations.map(({ authenticatedProvider, l1Observation }) =>
          normalizeWatcherL1BlockV1(authenticatedProvider, l1Observation),
        ),
      );
      context.finalityAuthority = {
        policy: localOgmiosFinalityPolicy,
        lineage: [],
        previousState: null,
        observations,
        consistency,
        result: evaluateWatcherFinalityV1(
          localOgmiosFinalityPolicy,
          null,
          consistency,
        ),
      };
      return context;
    };
    const aligned = contextWithQuery();
    expect(aligned.finalityAuthority.consistency).toMatchObject({
      status: "agreed",
      sourceMode: "local_node",
      independentProviderCount: 1,
      queryObservationCount: 1,
      reasonCodes: ["local_node_consistent"],
    });
    expect(
      accepted(null, {
        ...local,
        context: aligned as WatcherSettlementPublicContextV1,
      }).snapshot,
    ).toEqual(emptySnapshot());

    const queryOnly = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
      authenticatedProvider: localOgmiosProvider,
    });
    expect(queryOnly.context.finalityAuthority?.consistency).toMatchObject({
      status: "quarantined",
      reasonCodes: expect.arrayContaining(["missing_chain_sync_authority"]),
    });
    expect(
      evaluateWatcherSettlementIndexerV1(
        policy,
        null,
        queryOnly.observation,
        queryOnly.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const mismatchedQuery = contextWithQuery((query) => {
      query.transactions = [];
    });
    expect(mismatchedQuery.finalityAuthority.consistency).toMatchObject({
      status: "quarantined",
      reasonCodes: ["block_content_mismatch"],
    });
    expect(
      evaluateWatcherSettlementIndexerV1(
        policy,
        null,
        local.observation,
        mismatchedQuery,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const substitutedMode = structuredClone(aligned) as Mutable;
    substitutedMode.finalityAuthority.policy = finalityPolicy;
    expect(
      evaluateWatcherSettlementIndexerV1(
        policy,
        null,
        local.observation,
        substitutedMode,
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

    const forgedPolicy = makeWatcherSettlementIndexerPolicyV1({
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
      evaluateWatcherSettlementIndexerV1(
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
        normalizeWatcherL1BlockV1(
          candidate.authenticatedProvider,
          candidate.l1Observation,
        ),
    );
    disagreement.finalityAuthority.consistency =
      evaluateWatcherMultiProviderConsistencyV1(
        externalSource,
        disagreementBlocks,
      );
    disagreement.finalityAuthority.result = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      disagreement.finalityAuthority.consistency,
    );
    expect(
      evaluateWatcherSettlementIndexerV1(
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
      evaluateWatcherSettlementIndexerV1(
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
    const resource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      durable,
    );
    expect(resource).not.toBeNull();
    const snapshot = makeWatcherSettlementSnapshotV1({
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
      output: makeWatcherDurablePayloadV1(unauthenticatedHubOutput),
    };
    const unauthenticatedHubStore = makeWatcherDurableStoreV1({
      deploymentMarker: policy.deploymentMarker,
      revision: bootstrapStore.revision,
      records: {
        ...bootstrapStore,
        protocolUtxos: [unauthenticatedHub],
      },
    });
    const unauthenticatedHubPolicy = makeWatcherSettlementIndexerPolicyV1({
      ...policy,
      bootstrapStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(unauthenticatedHubStore),
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
      evaluateWatcherSettlementIndexerV1(
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
      makeWatcherSettlementResourceFromProtocolUtxoV1(policy, fake),
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
      makeWatcherSettlementResourceFromProtocolUtxoV1(policy, malformed),
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
      output: makeWatcherDurablePayloadV1(
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
      output: makeWatcherDurablePayloadV1(
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
      output: makeWatcherDurablePayloadV1(
        cardanoOutput(
          policy.reserveAddressHex,
          20_000_000n,
          [{ policyId: h28("68"), assetName: "02", quantity: 5n }],
          null,
        ),
      ),
    };
    expect(
      makeWatcherSettlementResourceFromProtocolUtxoV1(policy, payout),
    ).toMatchObject({ datumKind: "payout", identityAssetName: payoutAsset });
    expect(
      makeWatcherSettlementResourceFromProtocolUtxoV1(policy, withdrawal),
    ).toMatchObject({ datumKind: "withdrawal", identityAssetName: "ac" });
    expect(
      makeWatcherSettlementResourceFromProtocolUtxoV1(policy, reserve),
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
    const depositDurable: WatcherProtocolUtxoV1 = {
      outRef: depositOutRef,
      role: "deposit",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(depositOutput),
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
    const reserveDurable: WatcherProtocolUtxoV1 = {
      outRef: reserveOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(reserveOutput),
    };
    const reserveResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      reserveDurable,
    )!;
    const reserveSubject = makeWatcherSettlementSubjectV1({
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
    const snapshot = makeWatcherSettlementSnapshotV1({
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
      protocolUtxos: [reserveDurable],
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
    const withdrawalDurable: WatcherProtocolUtxoV1 = {
      outRef: withdrawalOutRef,
      role: "withdrawal",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(
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
    const withdrawalResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      withdrawalDurable,
    )!;
    const pendingSubject = makeWatcherSettlementSubjectV1({
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
    const initialSnapshot = makeWatcherSettlementSnapshotV1({
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
    const refundedSubject = makeWatcherSettlementSubjectV1({
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
    const terminalSnapshot = makeWatcherSettlementSnapshotV1({
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
    const underpaidSubject = makeWatcherSettlementSubjectV1({
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
      snapshot: makeWatcherSettlementSnapshotV1({
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
      evaluateWatcherSettlementIndexerV1(
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
    const withdrawalDurable: WatcherProtocolUtxoV1 = {
      outRef: withdrawalOutRef,
      role: "withdrawal",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(
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
    const reserveDurable: WatcherProtocolUtxoV1 = {
      outRef: reserveOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(
        cardanoOutput(policy.reserveAddressHex, 20_000_000n, [], null),
      ),
    };
    const withdrawalResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      withdrawalDurable,
    )!;
    const reserveResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      reserveDurable,
    )!;
    const withdrawalPending = makeWatcherSettlementSubjectV1({
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
    const reserveSubject = (outRef: string): WatcherSettlementSubjectV1 =>
      makeWatcherSettlementSubjectV1({
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
    const sourceSnapshot = makeWatcherSettlementSnapshotV1({
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
    const initialPayoutDurable: WatcherProtocolUtxoV1 = {
      outRef: initialPayoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(initialPayoutOutput),
    };
    const initialPayoutResource =
      makeWatcherSettlementResourceFromProtocolUtxoV1(
        policy,
        initialPayoutDurable,
      )!;
    const resolvedWithdrawal = makeWatcherSettlementSubjectV1({
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
    const payoutInitializing = makeWatcherSettlementSubjectV1({
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
    const initializedSnapshot = makeWatcherSettlementSnapshotV1({
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
    const fundedPayoutDurable: WatcherProtocolUtxoV1 = {
      outRef: fundedPayoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(fundedPayoutOutput),
    };
    const reserveChangeDurable: WatcherProtocolUtxoV1 = {
      outRef: reserveChangeOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(reserveChangeOutput),
    };
    const fundedPayoutResource =
      makeWatcherSettlementResourceFromProtocolUtxoV1(
        policy,
        fundedPayoutDurable,
      )!;
    const reserveChangeResource =
      makeWatcherSettlementResourceFromProtocolUtxoV1(
        policy,
        reserveChangeDurable,
      )!;
    const fundedPayoutSubject = makeWatcherSettlementSubjectV1({
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
    const fundedSnapshot = makeWatcherSettlementSnapshotV1({
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
      evaluateWatcherSettlementIndexerV1(
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
    const unbalancedPayoutDurable: WatcherProtocolUtxoV1 = {
      outRef: unbalancedPayoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(fundedPayoutOutput),
    };
    const unbalancedReserveDurable: WatcherProtocolUtxoV1 = {
      outRef: unbalancedReserveOutRef,
      role: "reserve",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(unbalancedReserveOutput),
    };
    const unbalancedSnapshot = makeWatcherSettlementSnapshotV1({
      resources: [
        makeWatcherSettlementResourceFromProtocolUtxoV1(
          policy,
          unbalancedPayoutDurable,
        )!,
        makeWatcherSettlementResourceFromProtocolUtxoV1(
          policy,
          unbalancedReserveDurable,
        )!,
      ],
      subjects: [
        makeWatcherSettlementSubjectV1({
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
      evaluateWatcherSettlementIndexerV1(
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
    const paidPayoutSubject = makeWatcherSettlementSubjectV1({
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
    const terminalSnapshot = makeWatcherSettlementSnapshotV1({
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
    const payoutDurable: WatcherProtocolUtxoV1 = {
      outRef: payoutOutRef,
      role: "payout",
      chainPointId: h32("00"),
      output: makeWatcherDurablePayloadV1(payoutOutputHex),
    };
    const payoutResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      payoutDurable,
    )!;
    const payoutSubject = makeWatcherSettlementSubjectV1({
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
    const fundedSnapshot = makeWatcherSettlementSnapshotV1({
      resources: [payoutResource],
      subjects: [payoutSubject],
    })!;
    const unrelatedSpentProtocolUtxo = {
      ...payoutDurable,
      outRef: `${h32("b4")}#1`,
      role: "reserve" as const,
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
    const paidSubject = makeWatcherSettlementSubjectV1({
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
    const terminalSnapshot = makeWatcherSettlementSnapshotV1({
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
    const underpaidSubject = makeWatcherSettlementSubjectV1({
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
    const underpaidSnapshot = makeWatcherSettlementSnapshotV1({
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
      evaluateWatcherSettlementIndexerV1(
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
    const oldRawA = structuredClone(terminal.context.l1Observation) as Mutable;
    oldRawA.providerId = rollbackProviderA.providerId;
    oldRawA.chainPoint = {
      blockHash: h32("e1"),
      slot: "7000",
      blockNo: "700",
      depth: "1",
    };
    const oldRawB = structuredClone(oldRawA) as Mutable;
    oldRawB.providerId = rollbackProviderB.providerId;
    const replacementRawA = structuredClone(oldRawA) as Mutable;
    replacementRawA.chainPoint = {
      blockHash: h32("e2"),
      slot: "7001",
      blockNo: "701",
      depth: "1",
    };
    const replacementRawB = structuredClone(replacementRawA) as Mutable;
    replacementRawB.providerId = rollbackProviderB.providerId;
    const oldA = normalizeWatcherL1BlockV1(rollbackProviderA, oldRawA);
    const oldB = normalizeWatcherL1BlockV1(rollbackProviderB, oldRawB);
    const replacementA = normalizeWatcherL1BlockV1(
      rollbackProviderA,
      replacementRawA,
    );
    const replacementB = normalizeWatcherL1BlockV1(
      rollbackProviderB,
      replacementRawB,
    );
    const rollbackBlocks = [oldA, oldB, replacementA, replacementB];
    const rollbackChainPoints = [
      ...initial.store.chainPoints,
      ...persistedChainPoints(rollbackBlocks),
    ];
    const terminalJournal = journalWatcherProtocolUtxoTransitionV1({
      sourceStore: initial.store,
      nextChainPoints: rollbackChainPoints,
      nextProtocolUtxos: initial.store.protocolUtxos.filter(
        ({ role }) => role === "hub_oracle",
      ),
      spentAtChainPointId: oldA.chainPoint.chainPointId,
    });
    const rollbackSourceStore = makeWatcherDurableStoreV1({
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
    const omittedArchiveStore = makeWatcherDurableStoreV1({
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
      evaluateWatcherSettlementIndexerV1(
        scenario.policy,
        fundedState,
        omittedArchive.observation,
        omittedArchive.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["durable_evidence_mismatch"],
    });
    const substitutedArchiveEntry = {
      ...terminalJournal.spentProtocolUtxos[0]!,
      output: makeWatcherDurablePayloadV1(
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
    const substitutedArchiveStore = makeWatcherDurableStoreV1({
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
      evaluateWatcherSettlementIndexerV1(
        scenario.policy,
        fundedState,
        substitutedArchive.observation,
        substitutedArchive.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["durable_evidence_mismatch"],
    });
    const priorFinalityState = terminalForRollback.finalityState!;
    const replacementConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [replacementA, replacementB],
    );
    const replacementFinality = evaluateWatcherFinalityV1(
      finalityPolicy,
      priorFinalityState,
      replacementConsistency,
    );
    expect(replacementFinality.action).toBe("rewind_pending");
    const rollbackBootstrap = makeWatcherRollbackBootstrapStateV1(
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
    const authoritativeRollback = evaluateWatcherRollbackV1(
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
    const restoredStore = makeWatcherDurableStoreV1({
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
      parseWatcherSettlementIndexerStateV1(
        JSON.parse(JSON.stringify(restoredState)),
        scenario.policy,
        [initial.restartBinding, rollbackEvidence.restartBinding],
        [rollbackBinding],
      ),
    ).toEqual(restoredState);

    const omittedRestoredStore = makeWatcherDurableStoreV1({
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
      evaluateWatcherSettlementIndexerV1(
        scenario.policy,
        terminalRollbackState,
        omittedRestoration.observation,
        omittedRestoration.context,
      ),
    ).toMatchObject({ action: "reject" });

    const substitutedPayout: WatcherProtocolUtxoV1 = {
      ...retainedPayout,
      output: makeWatcherDurablePayloadV1(
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
    const substitutedStore = makeWatcherDurableStoreV1({
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
      evaluateWatcherSettlementIndexerV1(
        scenario.policy,
        terminalRollbackState,
        substitutedRestoration.observation,
        substitutedRestoration.context,
      ),
    ).toMatchObject({ action: "reject" });

    const reIncludedJournal = journalWatcherProtocolUtxoTransitionV1({
      sourceStore: restoredStore,
      nextChainPoints: restoredStore.chainPoints,
      nextProtocolUtxos: restoredStore.protocolUtxos.filter(
        ({ role }) => role === "hub_oracle",
      ),
      spentAtChainPointId: replacementA.chainPoint.chainPointId,
    });
    const reIncludedStore = makeWatcherDurableStoreV1({
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
      evaluateWatcherSettlementIndexerV1(
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
      .sourceDurableStore as WatcherDurableStoreV1;
    const currentObservationId =
      spawned.spawnEvidence.observation.sourceObservationDigest;
    const subsetStore = makeWatcherDurableStoreV1({
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
    const forgedObservation = makeWatcherSettlementObservationV1({
      ...observationFields,
      durableStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(subsetStore),
      ),
    })!;
    const forgedContext = {
      ...spawned.spawnEvidence.context,
      durableStore: subsetStore,
    };
    expect(
      evaluateWatcherSettlementIndexerV1(
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
    const claimedResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      claimedDurable,
    )!;
    const claimedSnapshot = makeWatcherSettlementSnapshotV1({
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
    const retryOneSnapshot = makeWatcherSettlementSnapshotV1({
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
    const retryTwoSnapshot = makeWatcherSettlementSnapshotV1({
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
    const stuckSnapshot = makeWatcherSettlementSnapshotV1({
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
    const invalidSnapshot = makeWatcherSettlementSnapshotV1({
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
    const disprovedResource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      disprovedDurable,
    )!;
    const claimSubjectId = "resolution-claim-aa";
    const claimSubject = makeWatcherSettlementSubjectV1({
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
    const disprovedSnapshot = makeWatcherSettlementSnapshotV1({
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
    const resolvedSnapshot = makeWatcherSettlementSnapshotV1({
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
  });

  it("rejects exact transaction identity collisions", () => {
    const initial = bundle({
      kind: "bootstrap",
      previousState: null,
      snapshot: emptySnapshot(),
    });
    const state = accepted(null, initial);
    const collision = makeWatcherSettlementObservationV1({
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
    const result = evaluateWatcherSettlementIndexerV1(
      policy,
      state,
      collision,
      {
        ...initial.context,
        restartContexts: [initial.restartBinding],
      },
    );
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["identity_collision"],
    });
  });

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
    const publicTransaction = {
      txHash: transactionHash,
      body: makeWatcherL1PublicBytesV1(bodyHex),
      utxos: [
        {
          outRef,
          outputIndex: "0",
          output: makeWatcherL1PublicBytesV1(outputHex),
          datum: {
            datumHash: computeHash32(Buffer.from(datumHex!, "hex")).toString(
              "hex",
            ),
            bytes: makeWatcherL1PublicBytesV1(datumHex!),
          },
          referenceScript: null,
        },
        {
          outRef: `${transactionHash}#1`,
          outputIndex: "1",
          output: makeWatcherL1PublicBytesV1(queueOutputHex),
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
          bytes: makeWatcherL1PublicBytesV1(spawnRedeemer),
        },
        {
          purpose: "mint",
          index: "1",
          bytes: makeWatcherL1PublicBytesV1(
            stateQueueMergeRedeemer(inputs[0]!),
          ),
        },
      ],
    };
    const oldPoint = {
      blockHash: h32("d2"),
      slot: "5000",
      blockNo: "500",
      depth: "1",
    } as const;
    const replacementPoint = {
      blockHash: h32("d3"),
      slot: "5001",
      blockNo: "501",
      depth: "2",
    } as const;
    const providerA = rollbackProvider("provider-a", "77");
    const providerB = rollbackProvider("provider-b", "78");
    const oldRawA = rawBlock(providerA, oldPoint, publicTransaction);
    const oldRawB = rawBlock(providerB, oldPoint, publicTransaction);
    const replacementRawA = rawBlock(
      providerA,
      replacementPoint,
      publicTransaction,
    );
    const replacementRawB = rawBlock(
      providerB,
      replacementPoint,
      publicTransaction,
    );
    const oldA = normalizeWatcherL1BlockV1(providerA, oldRawA);
    const oldB = normalizeWatcherL1BlockV1(providerB, oldRawB);
    const replacementA = normalizeWatcherL1BlockV1(providerA, replacementRawA);
    const replacementB = normalizeWatcherL1BlockV1(providerB, replacementRawB);
    const persisted = [oldA, oldB, replacementA, replacementB];
    const oldDurable: WatcherProtocolUtxoV1 = {
      outRef,
      role: "settlement",
      chainPointId: oldA.chainPoint.chainPointId,
      output: makeWatcherDurablePayloadV1(outputHex),
    };
    const sourceStore = makeWatcherDurableStoreV1({
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
    const resource = makeWatcherSettlementResourceFromProtocolUtxoV1(
      policy,
      oldDurable,
    )!;
    const spawnedSnapshot = makeWatcherSettlementSnapshotV1({
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

    const rollbackFinalityPolicy = makeWatcherFinalityPolicyV1(
      {
        schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
        mode: "acceptance",
        targetNetwork: "Preprod",
        l1: {
          source: {
            sourceMode: "external_providers",
            providers: [
              {
                identity: "provider-a",
                operatorIdentitySha256: h32("97"),
                endpoint: "https://cardano-a.example",
              },
              {
                identity: "provider-b",
                operatorIdentitySha256: h32("98"),
                endpoint: "https://cardano-b.example",
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
                "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
            },
          ],
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
        },
        storage: {
          driver: "sqlite",
          path: "/var/lib/midgard-watcher/watcher.sqlite",
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
    const oldConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [oldA, oldB],
    );
    const priorFinalityResult = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      null,
      oldConsistency,
    );
    expect(priorFinalityResult.action).toBe("observe_pending");
    const priorFinalityState = priorFinalityResult.state!;
    const replacementConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      [replacementA, replacementB],
    );
    const finalityResult = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      priorFinalityState,
      replacementConsistency,
    );
    expect(finalityResult.action).toBe("rewind_pending");
    const rollbackBootstrapState = makeWatcherRollbackBootstrapStateV1(
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
    const authoritative = evaluateWatcherRollbackV1(
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
      .authenticatedProvider as WatcherAuthenticatedL1ProviderV1;
    const retainedAnchorRaw = structuredClone(
      bootstrapEvidence.context.l1Observation,
    ) as Mutable;
    const retainedAnchor = normalizeWatcherL1BlockV1(
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
      evaluateWatcherSettlementIndexerV1(
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
      parseWatcherSettlementIndexerStateV1(
        JSON.parse(JSON.stringify(rollbackState)),
        policy,
        [bootstrapEvidence.restartBinding, rollbackEvidence.restartBinding],
        [restartRollbackBinding],
      ),
    ).toEqual(rollbackState);
    expect(
      parseWatcherSettlementIndexerStateV1(
        rollbackState,
        policy,
        [bootstrapEvidence.restartBinding],
        [restartRollbackBinding],
      ),
    ).toBeNull();
    expect(
      parseWatcherSettlementIndexerStateV1(
        rollbackState,
        policy,
        [bootstrapEvidence.restartBinding, rollbackEvidence.restartBinding],
        [],
      ),
    ).toBeNull();
    const forgedState: Mutable = JSON.parse(JSON.stringify(rollbackState));
    forgedState.durableStoreDigest = h32("fe");
    delete forgedState.stateDigest;
    forgedState.stateDigest = sha256CanonicalForTest(forgedState);
    expect(
      parseWatcherSettlementIndexerStateV1(
        forgedState,
        policy,
        [bootstrapEvidence.restartBinding, rollbackEvidence.restartBinding],
        [restartRollbackBinding],
      ),
    ).toBeNull();
    const authenticRollbackResult = evaluateWatcherSettlementIndexerV1(
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
      parseWatcherSettlementIndexerResultV1(forgedResult, {
        policy,
        previousState: spawnedState,
        observation: rollbackEvidence.observation,
        publicContext: rollbackEvidence.context,
      }),
    ).toBeNull();

    const nextStore = authoritative.nextStore!;
    const reIncludedDurable: WatcherProtocolUtxoV1 = {
      ...oldDurable,
      chainPointId: replacementA.chainPoint.chainPointId,
    };
    const reIncludedStore = makeWatcherDurableStoreV1({
      deploymentMarker: policy.deploymentMarker,
      revision: (BigInt(nextStore.revision) + 1n).toString(),
      records: {
        l1Observations: nextStore.l1Observations,
        chainPoints: nextStore.chainPoints,
        protocolUtxos: [...nextStore.protocolUtxos, reIncludedDurable],
        daProofInputs: nextStore.daProofInputs,
        reconstructedStates: nextStore.reconstructedStates,
        decisions: nextStore.decisions,
        faults: nextStore.faults,
        submissions: nextStore.submissions,
        confirmations: nextStore.confirmations,
        retries: nextStore.retries,
        deadlines: nextStore.deadlines,
        correctionResults: nextStore.correctionResults,
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
        rollbackEvidence.restartBinding,
      ],
      restartRollbackBindings: [restartRollbackBinding],
      authenticatedProvider: providerA,
      l1Observation: replacementRawA,
      durableStore: reIncludedStore,
    });
    expect(reIncluded.observation.transactionHash).toBe(
      spawnEvidence.observation.transactionHash,
    );
    expect(accepted(rollbackState, reIncluded).snapshot).toEqual(
      spawnedSnapshot,
    );
  });

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
      evaluateWatcherSettlementIndexerV1(
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
      parseWatcherSettlementIndexerStateV1(
        state,
        policy,
        [initial.restartBinding],
        [],
      ),
    ).toEqual(state);
    expect(
      parseWatcherSettlementIndexerStateV1(state, policy, [], []),
    ).toBeNull();
  });
});

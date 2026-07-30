import { createHash, generateKeyPairSync, sign } from "node:crypto";

import {
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  AddressSchema,
  ConfirmedState,
  DaAttestationDatum,
  DaAttestationMintRedeemer,
  DaAttestationSpendRedeemer,
  FraudProofTokenDatum,
  HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  LinkedListDatum,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SettlementDatum,
  SettlementMintRedeemer,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueNodeV1,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data, validatorToScriptHash } from "@lucid-evolution/lucid";
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
  type WatcherDurableRecordsV1,
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
  type WatcherL1RedeemerV1,
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "../src/multi-provider-consistency.js";
import {
  evaluateWatcherRollbackV1,
  makeWatcherRollbackBootstrapStateV1,
} from "../src/rollback-engine.js";
import {
  evaluateWatcherStateQueueIndexerV1,
  makeWatcherStateQueueHeaderV1,
  makeWatcherStateQueueIndexerPolicyV1,
  makeWatcherStateQueueObservationV1,
  makeWatcherStateQueueSnapshotV1,
  parseWatcherStateQueueIndexerResultV1,
  parseWatcherStateQueueIndexerStateV1,
  WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  type WatcherStateQueueIndexerPolicyV1,
  type WatcherStateQueueObservationV1,
  type WatcherStateQueuePublicContextV1,
  type WatcherStateQueueSnapshotV1,
} from "../src/state-queue-indexer.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const asWireValue = <T>(value: T): T => JSON.parse(JSON.stringify(value)) as T;
const EMPTY_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const operator = h28("aa");
const maturity = 604_800_000n;

const canonicalJson = (value: unknown): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJson).join(",")}]`;
  }
  const record = value as Record<string, unknown>;
  return `{${Object.keys(record)
    .sort()
    .map((key) => `${JSON.stringify(key)}:${canonicalJson(record[key])}`)
    .join(",")}}`;
};

const canonicalDigest = (value: unknown): string =>
  createHash("sha256").update(canonicalJson(value), "utf8").digest("hex");

const scriptAddress = (hash: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.from(hash, "hex")]),
  ).to_hex();

type Mutable = Record<string, any>;
const RELEASE_DIGEST = h32("22");
const BLUEPRINT_HASH = h32("55");
const RULE_BUNDLE_COMMITMENT = h32("44");
const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

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
  contracts.fraudProofCatalogueMint.fraudProofCatalogue = {
    root: h32("13"),
    categories: Object.fromEntries(
      [
        "doubleSpend",
        "nonExistentInput",
        "nonExistentInputNoIndex",
        "invalidRange",
        "transitionTrace",
        "validationTraceDispute",
      ].map((category, index) => {
        const contractName = {
          doubleSpend: "fraudProofDoubleSpend",
          nonExistentInput: "fraudProofNonExistentInput",
          nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
          invalidRange: "fraudProofInvalidRange",
          transitionTrace: "fraudProofTransitionTrace",
          validationTraceDispute: "validationTraceDispute",
        }[category]!;
        return [
          category,
          {
            categoryId: index.toString(16).padStart(8, "0"),
            scriptHash: contracts[contractName].scriptHash,
            membershipProofCbor: "80",
          },
        ];
      }),
    ),
  };
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
  const trustRootId = createHash("sha256")
    .update(Buffer.from(publicKeySpkiDerHex, "hex"))
    .digest("hex");
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
        ).map(([name, entry]: [string, any]) => [
          name,
          { categoryId: entry.categoryId, scriptHash: entry.scriptHash },
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
const categoryIds = Object.values(
  deploymentAuthorityFixture.policy.fraudProofCatalogue.categories,
)
  .map(({ categoryId }) => categoryId)
  .sort();

const policy = makeWatcherStateQueueIndexerPolicyV1({
  network: "Preprod",
  releaseEvidenceDigest: RELEASE_DIGEST,
  deploymentMarker: deploymentAuthorityFixture.marker,
  deploymentTrustRootId: deploymentAuthorityFixture.result.trustRootId,
  requiredFinalityDepth: "2",
  stateQueuePolicyId: applied.stateQueueMint!,
  stateQueueSpendScriptHash: applied.stateQueueSpend!,
  schedulerPolicyId: applied.schedulerMint!,
  schedulerSpendScriptHash: applied.schedulerSpend!,
  activeOperatorsPolicyId: applied.activeOperatorsMint!,
  activeOperatorsSpendScriptHash: applied.activeOperatorsSpend!,
  retiredOperatorsPolicyId: applied.retiredOperatorsMint!,
  retiredOperatorsSpendScriptHash: applied.retiredOperatorsSpend!,
  fraudProofPolicyId: applied.fraudProofMint!,
  fraudProofSpendScriptHash: applied.fraudProofSpend!,
  daAttestationPolicyId: applied.daAttestationMint!,
  daAttestationSpendScriptHash: applied.daAttestationSpend!,
  hubOraclePolicyId: applied.hubOracleMint!,
  stateQueueAddressHex: scriptAddress(applied.stateQueueSpend!),
  schedulerAddressHex: scriptAddress(applied.schedulerSpend!),
  activeOperatorsAddressHex: scriptAddress(applied.activeOperatorsSpend!),
  retiredOperatorsAddressHex: scriptAddress(applied.retiredOperatorsSpend!),
  fraudProofAddressHex: scriptAddress(applied.fraudProofSpend!),
  daAttestationAddressHex: scriptAddress(applied.daAttestationSpend!),
  hubOracleAddressHex: scriptAddress(applied.hubOracleMint!),
  stateQueueRootAssetNameHex: STATE_QUEUE_ROOT_ASSET_NAME,
  stateQueueNodeAssetPrefixHex: STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  schedulerAssetNameHex: SCHEDULER_ASSET_NAME,
  activeOperatorAssetPrefixHex: "4d414354",
  retiredOperatorAssetPrefixHex: "4d524554",
  fraudProofCategoryIdsHex: categoryIds,
  daAttestationAssetPrefixHex: "44414154",
  hubOracleAssetNameHex: HUB_ORACLE_ASSET_NAME,
  maturityDurationMs: maturity.toString(),
  maximumHistoryEntries: "32",
}) as WatcherStateQueueIndexerPolicyV1;

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

const providerB = {
  ...provider,
  providerId: "provider-b",
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: h32("98"),
  },
  authentication: {
    ...provider.authentication,
    publicIdentitySha256: h32("88"),
  },
} as const;

const externalSource = {
  sourceMode: "external_providers",
  network: "Preprod",
} as const;

const LOCAL_NODE_ID = "watcher-node-a";
const LOCAL_GENESIS_IDENTITY = h32("76");
const localNodeProvider = {
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
const localSource = {
  sourceMode: "local_node",
  network: "Preprod",
  authorityNodeId: LOCAL_NODE_ID,
  genesisIdentitySha256: LOCAL_GENESIS_IDENTITY,
} as const;

const finalityPolicyAtDepth = (
  depth: number,
  sourceMode: "external_providers" | "local_node" = "external_providers",
) =>
  makeWatcherFinalityPolicyV1(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        source:
          sourceMode === "local_node"
            ? {
                sourceMode: "local_node",
                authorityNodeId: LOCAL_NODE_ID,
                chainSync: {
                  kind: "cardano_node_socket",
                  socketPath: "/ipc/node.socket",
                  genesisIdentitySha256: LOCAL_GENESIS_IDENTITY,
                },
                queryServices: [],
              }
            : {
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
          depth,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: depth,
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
      trustRootId: h32("33"),
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      ruleBundleCommitment: h32("44"),
      programCommitments: { validation: h32("55") },
      durableMarker: policy.deploymentMarker,
    },
  )!;

const finalityPolicy = finalityPolicyAtDepth(2);

const confirmed: ConfirmedState = {
  headerHash: h28("00"),
  prevHeaderHash: h28("00"),
  utxoRoot: EMPTY_ROOT,
  startTime: 1_000n,
  endTime: 1_000n,
  protocolVersion: 0n,
};

const canonicalDatum = (cborHex: string): string =>
  CML.PlutusData.from_cbor_hex(cborHex).to_canonical_cbor_hex();

const linkedRoot = (
  data: unknown,
  schema: unknown,
  link: string | null,
): string =>
  canonicalDatum(
    Data.to(
      {
        data: { Root: { data: Data.castTo(data as never, schema as never) } },
        link,
      },
      LinkedListDatum,
    ),
  );

const linkedNode = (
  data: unknown,
  schema: unknown,
  link: string | null,
): string =>
  canonicalDatum(
    Data.to(
      {
        data: { Node: { data: Data.castTo(data as never, schema as never) } },
        link,
      },
      LinkedListDatum,
    ),
  );

const value = (
  policyId: string,
  assetName: string,
  lovelace = 2_000_000n,
): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(policyId),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  return CML.Value.new(lovelace, multiasset);
};

const output = (
  addressHex: string,
  policyId: string,
  assetName: string,
  datumHex: string,
  lovelace = 2_000_000n,
): string =>
  CML.TransactionOutput.new(
    CML.Address.from_hex(addressHex),
    value(policyId, assetName, lovelace),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumHex)),
    null,
  ).to_canonical_cbor_hex();

type OutputFixture = Readonly<{
  role: WatcherProtocolUtxoV1["role"];
  outputHex: string;
  datumHex: string;
}>;

const bodyFrom = (
  inputs: readonly string[],
  references: readonly string[],
  outputs: readonly OutputFixture[],
  mint: readonly Readonly<{
    policyId: string;
    assetName: string;
    quantity: bigint;
  }>[],
  lower: bigint | null,
  upper: bigint | null,
): string => {
  const bodyInputs = CML.TransactionInputList.new();
  for (const outRef of inputs) {
    const [txHash, index] = outRef.split("#");
    bodyInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(index!),
      ),
    );
  }
  const bodyOutputs = CML.TransactionOutputList.new();
  for (const fixture of outputs) {
    bodyOutputs.add(CML.TransactionOutput.from_cbor_hex(fixture.outputHex));
  }
  const body = CML.TransactionBody.new(bodyInputs, bodyOutputs, 170_000n);
  if (references.length > 0) {
    const referenceInputs = CML.TransactionInputList.new();
    for (const outRef of references) {
      const [txHash, index] = outRef.split("#");
      referenceInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
    }
    body.set_reference_inputs(referenceInputs);
  }
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
  if (lower !== null) {
    body.set_validity_interval_start(lower);
  }
  if (upper !== null) {
    body.set_ttl(upper);
  }
  const requiredSigners = CML.Ed25519KeyHashList.new();
  requiredSigners.add(CML.Ed25519KeyHash.from_hex(operator));
  body.set_required_signers(requiredSigners);
  return body.to_canonical_cbor_hex();
};

let blockSerial = 0;
const l1Block = (
  bodyHex: string,
  outputs: readonly OutputFixture[],
  redeemers: readonly Readonly<{
    purpose: WatcherL1RedeemerV1["purpose"];
    index: string;
    bytesHex: string;
  }>[],
) => {
  blockSerial += 1;
  const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
  const utxos = outputs.map((fixture, index) => {
    const datum = CML.PlutusData.from_cbor_hex(fixture.datumHex);
    return {
      outRef: `${txHash}#${index.toString()}`,
      outputIndex: index.toString(),
      output: makeWatcherL1PublicBytesV1(fixture.outputHex),
      datum: {
        datumHash: CML.hash_plutus_data(datum).to_hex(),
        bytes: makeWatcherL1PublicBytesV1(fixture.datumHex),
      },
      referenceScript: null,
    };
  });
  const observation = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "provider-a",
    chainPoint: {
      blockHash: blockSerial.toString(16).padStart(2, "0").repeat(32),
      slot: (1_000 + blockSerial).toString(),
      blockNo: (100 + blockSerial).toString(),
      depth: "2",
    },
    transactions: [
      {
        txHash,
        body: makeWatcherL1PublicBytesV1(bodyHex),
        utxos,
        scripts: [],
        datums: [],
        redeemers: redeemers.map(({ purpose, index, bytesHex }) => ({
          purpose,
          index,
          bytes: makeWatcherL1PublicBytesV1(bytesHex),
        })),
      },
    ],
  };
  return {
    txHash,
    raw: observation,
    normalized: normalizeWatcherL1BlockV1(provider, observation),
  };
};

const protocolRecords = (
  block: ReturnType<typeof l1Block>,
  outputs: readonly OutputFixture[],
): readonly WatcherProtocolUtxoV1[] =>
  outputs.map((fixture, index) => ({
    outRef: `${block.txHash}#${index.toString()}`,
    role: fixture.role,
    chainPointId: block.normalized.chainPoint.chainPointId,
    output: makeWatcherDurablePayloadV1(fixture.outputHex),
  }));

const currentObservationRecord = (block: ReturnType<typeof l1Block>) => ({
  observationId: block.normalized.observationDigest,
  providerId: block.normalized.provider.providerId,
  chainPointId: block.normalized.chainPoint.chainPointId,
  payload: makeWatcherDurablePayloadV1(
    encodeWatcherNormalizedL1BlockV1(block.normalized).toString("hex"),
  ),
});

const currentPointRecord = (block: ReturnType<typeof l1Block>) => ({
  chainPointId: block.normalized.chainPoint.chainPointId,
  providerId: block.normalized.provider.providerId,
  blockHash: block.normalized.chainPoint.blockHash,
  slot: block.normalized.chainPoint.slot,
  blockNo: block.normalized.chainPoint.blockNo,
  depth: block.normalized.chainPoint.depth,
});

const storeSources = new WeakMap<
  WatcherDurableStoreV1,
  WatcherDurableStoreV1
>();

const storeFor = (
  block: ReturnType<typeof l1Block>,
  protocols: readonly WatcherProtocolUtxoV1[],
  previous: WatcherDurableStoreV1 | null,
): WatcherDurableStoreV1 => {
  const source = previous ?? emptyStore();
  const l1Observations = [
    ...(previous?.l1Observations ?? []),
    currentObservationRecord(block),
  ];
  const chainPoints = [
    ...(previous?.chainPoints ?? []),
    currentPointRecord(block),
  ];
  const journal = journalWatcherProtocolUtxoTransitionV1({
    sourceStore: source,
    nextChainPoints: chainPoints,
    nextProtocolUtxos: protocols,
    spentAtChainPointId: block.normalized.chainPoint.chainPointId,
  });
  const next = makeWatcherDurableStoreV1({
    deploymentMarker: policy.deploymentMarker,
    revision: (BigInt(previous?.revision ?? "0") + 1n).toString(),
    records: {
      l1Observations,
      chainPoints,
      ...journal,
      daProofInputs: previous?.daProofInputs ?? [],
      reconstructedStates: previous?.reconstructedStates ?? [],
      decisions: previous?.decisions ?? [],
      faults: previous?.faults ?? [],
      submissions: previous?.submissions ?? [],
      confirmations: previous?.confirmations ?? [],
      retries: previous?.retries ?? [],
      deadlines: previous?.deadlines ?? [],
      correctionResults: previous?.correctionResults ?? [],
    },
  });
  storeSources.set(next, source);
  return next;
};

const emptyStore = (): WatcherDurableStoreV1 =>
  makeWatcherDurableStoreV1({
    deploymentMarker: policy.deploymentMarker,
    revision: "0",
    records: {
      l1Observations: [],
      chainPoints: [],
      protocolUtxos: [],
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

const remakeStore = (
  source: WatcherDurableStoreV1,
  overrides: Partial<WatcherDurableRecordsV1>,
  revision = source.revision,
): WatcherDurableStoreV1 =>
  makeWatcherDurableStoreV1({
    deploymentMarker: source.deploymentMarker,
    revision,
    records: {
      l1Observations: source.l1Observations,
      chainPoints: source.chainPoints,
      protocolUtxos: source.protocolUtxos,
      spentProtocolUtxos: source.spentProtocolUtxos,
      daProofInputs: source.daProofInputs,
      reconstructedStates: source.reconstructedStates,
      decisions: source.decisions,
      faults: source.faults,
      submissions: source.submissions,
      confirmations: source.confirmations,
      retries: source.retries,
      deadlines: source.deadlines,
      correctionResults: source.correctionResults,
      ...overrides,
    },
  });

const contextFor = (
  block: ReturnType<typeof l1Block>,
  store: WatcherDurableStoreV1,
  sourceStore: WatcherDurableStoreV1 = storeSources.get(store) ?? emptyStore(),
): WatcherStateQueuePublicContextV1 =>
  asWireValue({
    ...(() => {
      const otherObservation = structuredClone(block.raw);
      otherObservation.providerId = providerB.providerId;
      const observations = [
        { authenticatedProvider: provider, l1Observation: block.raw },
        {
          authenticatedProvider: providerB,
          l1Observation: otherObservation,
        },
      ];
      const consistency = evaluateWatcherMultiProviderConsistencyV1(
        externalSource,
        observations.map(({ authenticatedProvider, l1Observation }) =>
          normalizeWatcherL1BlockV1(authenticatedProvider, l1Observation),
        ),
      );
      const priorObservations = observations.map(
        ({ authenticatedProvider, l1Observation }) => {
          const prior = structuredClone(l1Observation);
          prior.chainPoint.depth = "1";
          return { authenticatedProvider, l1Observation: prior };
        },
      );
      const priorConsistency = evaluateWatcherMultiProviderConsistencyV1(
        externalSource,
        priorObservations.map(({ authenticatedProvider, l1Observation }) =>
          normalizeWatcherL1BlockV1(authenticatedProvider, l1Observation),
        ),
      );
      const previousState = evaluateWatcherFinalityV1(
        finalityPolicy,
        null,
        priorConsistency,
      ).state;
      const result = evaluateWatcherFinalityV1(
        finalityPolicy,
        previousState,
        consistency,
      );
      return {
        schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
        authenticatedProvider: provider,
        l1Observation: block.raw,
        sourceDurableStore: sourceStore,
        durableStore: store,
        deploymentAuthority,
        finalityAuthority: {
          policy: finalityPolicy,
          previousState,
          observations,
          consistency,
          result,
        },
        rollbackAuthority: null,
      };
    })(),
  });

const asLocalNodeBlock = (
  block: ReturnType<typeof l1Block>,
): ReturnType<typeof l1Block> => {
  const raw = structuredClone(block.raw);
  raw.providerId = localNodeProvider.providerId;
  return {
    ...block,
    raw,
    normalized: normalizeWatcherL1BlockV1(localNodeProvider, raw),
  };
};

const localNodeContextFor = (
  block: ReturnType<typeof l1Block>,
  store: WatcherDurableStoreV1,
  sourceStore: WatcherDurableStoreV1 = storeSources.get(store) ?? emptyStore(),
): WatcherStateQueuePublicContextV1 => {
  const consistency = evaluateWatcherMultiProviderConsistencyV1(localSource, [
    block.normalized,
  ]);
  const priorRaw = structuredClone(block.raw);
  priorRaw.chainPoint.depth = "1";
  const priorConsistency = evaluateWatcherMultiProviderConsistencyV1(
    localSource,
    [normalizeWatcherL1BlockV1(localNodeProvider, priorRaw)],
  );
  const policyAtDepth = finalityPolicyAtDepth(2, "local_node");
  const previousState = evaluateWatcherFinalityV1(
    policyAtDepth,
    null,
    priorConsistency,
  ).state;
  const result = evaluateWatcherFinalityV1(
    policyAtDepth,
    previousState,
    consistency,
  );
  return asWireValue({
    schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: localNodeProvider,
    l1Observation: block.raw,
    sourceDurableStore: sourceStore,
    durableStore: store,
    deploymentAuthority,
    finalityAuthority: {
      policy: policyAtDepth,
      previousState,
      observations: [
        {
          authenticatedProvider: localNodeProvider,
          l1Observation: block.raw,
        },
      ],
      consistency,
      result,
    },
    rollbackAuthority: null,
  });
};

const observationFor = (
  block: ReturnType<typeof l1Block>,
  store: WatcherDurableStoreV1,
  _snapshot: WatcherStateQueueSnapshotV1,
  kind:
    | "bootstrap"
    | "append"
    | "attach_da"
    | "merge"
    | "remove_fraudulent"
    | "rollback",
  predecessor: string | null,
  _lower: string | null,
  _upper: string | null,
  overrides: Partial<WatcherStateQueueObservationV1> = {},
): WatcherStateQueueObservationV1 => {
  const encoded = encodeWatcherNormalizedL1BlockV1(block.normalized);
  const sourceStore = storeSources.get(store) ?? emptyStore();
  const observation = makeWatcherStateQueueObservationV1({
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: kind,
    pointDigest: block.normalized.chainPoint.pointDigest,
    chainPointId: block.normalized.chainPoint.chainPointId,
    blockHash: block.normalized.chainPoint.blockHash,
    slot: block.normalized.chainPoint.slot,
    blockNo: block.normalized.chainPoint.blockNo,
    transactionHash: block.txHash,
    publicInputDigest: createHash("sha256").update(encoded).digest("hex"),
    sourceObservationDigest: block.normalized.observationDigest,
    sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(sourceStore),
    ),
    sourceDurableStoreRevision: sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
    ),
    durableStoreRevision: store.revision,
    predecessorStateDigest: predecessor,
    ...overrides,
  });
  expect(observation).not.toBeNull();
  return observation!;
};

const remakeObservation = (
  original: WatcherStateQueueObservationV1,
  overrides: Partial<
    Omit<WatcherStateQueueObservationV1, "schemaVersion" | "observationDigest">
  >,
): WatcherStateQueueObservationV1 => {
  const {
    schemaVersion: _schemaVersion,
    observationDigest: _observationDigest,
    ...fields
  } = original;
  const observation = makeWatcherStateQueueObservationV1({
    ...fields,
    ...overrides,
  });
  expect(observation).not.toBeNull();
  return observation!;
};

const shaDatum = (datumHex: string): string =>
  createHash("sha256").update(Buffer.from(datumHex, "hex")).digest("hex");

const addressData = (scriptHash: string) =>
  ({
    paymentCredential: { ScriptCredential: [scriptHash] },
    stakeCredential: null,
  }) as Data.Static<typeof AddressSchema>;

const rootFixtures = (activeBond: bigint | null = null) => {
  const queueDatum = linkedRoot(confirmed, ConfirmedState, null);
  const activeRootDatum = linkedRoot("", Data.Bytes(), operator);
  const activeNodeDatum = linkedNode(
    {
      bond_unlock_time: activeBond,
      inactivity_strikes: 0n,
    },
    ActiveOperatorDatum,
    null,
  );
  const retiredRootDatum = linkedRoot("", Data.Bytes(), null);
  const schedulerDatum = canonicalDatum(
    Data.to({ ActiveOperator: { operator, start_time: 900n } }, SchedulerDatum),
  );
  const hubDatum = canonicalDatum(
    Data.to(
      {
        registered_operators: applied.registeredOperatorsMint!,
        active_operators: policy.activeOperatorsPolicyId,
        retired_operators: policy.retiredOperatorsPolicyId,
        scheduler: policy.schedulerPolicyId,
        state_queue: policy.stateQueuePolicyId,
        fraud_proof_catalogue: applied.fraudProofCatalogueMint!,
        fraud_proof: policy.fraudProofPolicyId,
        deposit: applied.depositMint!,
        withdrawal: applied.withdrawalMint!,
        tx_order: applied.txOrderMint!,
        settlement: applied.settlementMint!,
        payout: applied.payoutMint!,
        registered_operators_addr: addressData(
          applied.registeredOperatorsSpend!,
        ),
        active_operators_addr: addressData(
          policy.activeOperatorsSpendScriptHash,
        ),
        retired_operators_addr: addressData(
          policy.retiredOperatorsSpendScriptHash,
        ),
        scheduler_addr: addressData(policy.schedulerSpendScriptHash),
        state_queue_addr: addressData(policy.stateQueueSpendScriptHash),
        fraud_proof_catalogue_addr: addressData(
          applied.fraudProofCatalogueSpend!,
        ),
        fraud_proof_addr: addressData(policy.fraudProofSpendScriptHash),
        deposit_addr: addressData(applied.depositSpend!),
        withdrawal_addr: addressData(applied.withdrawalSpend!),
        tx_order_addr: addressData(applied.txOrderSpend!),
        settlement_addr: addressData(applied.settlementSpend!),
        reserve_addr: addressData(applied.reserveSpend!),
        payout_addr: addressData(applied.payoutSpend!),
        reserve_observer: applied.reserveWithdraw!,
      },
      HubOracleDatum,
    ),
  );
  const outputs: readonly OutputFixture[] = [
    {
      role: "state_queue",
      datumHex: queueDatum,
      outputHex: output(
        policy.stateQueueAddressHex,
        policy.stateQueuePolicyId,
        policy.stateQueueRootAssetNameHex,
        queueDatum,
      ),
    },
    {
      role: "operator_directory",
      datumHex: activeRootDatum,
      outputHex: output(
        policy.activeOperatorsAddressHex,
        policy.activeOperatorsPolicyId,
        ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        activeRootDatum,
      ),
    },
    {
      role: "operator_directory",
      datumHex: activeNodeDatum,
      outputHex: output(
        policy.activeOperatorsAddressHex,
        policy.activeOperatorsPolicyId,
        `${policy.activeOperatorAssetPrefixHex}${operator}`,
        activeNodeDatum,
      ),
    },
    {
      role: "operator_directory",
      datumHex: retiredRootDatum,
      outputHex: output(
        policy.retiredOperatorsAddressHex,
        policy.retiredOperatorsPolicyId,
        RETIRED_OPERATORS_ROOT_ASSET_NAME,
        retiredRootDatum,
      ),
    },
    {
      role: "operator_directory",
      datumHex: schedulerDatum,
      outputHex: output(
        policy.schedulerAddressHex,
        policy.schedulerPolicyId,
        policy.schedulerAssetNameHex,
        schedulerDatum,
      ),
    },
    {
      role: "hub_oracle",
      datumHex: hubDatum,
      outputHex: output(
        policy.hubOracleAddressHex,
        policy.hubOraclePolicyId,
        policy.hubOracleAssetNameHex,
        hubDatum,
      ),
    },
  ];
  return {
    outputs,
    queueDatum,
    activeRootDatum,
    activeNodeDatum,
    retiredRootDatum,
    schedulerDatum,
    hubDatum,
  };
};

const snapshotFor = (
  fixture: ReturnType<typeof rootFixtures>,
  queue: WatcherStateQueueSnapshotV1["queue"] = [],
) => {
  const snapshot = makeWatcherStateQueueSnapshotV1({
    confirmedState: {
      headerHash: confirmed.headerHash,
      prevHeaderHash: confirmed.prevHeaderHash,
      utxosRoot: confirmed.utxoRoot,
      startTime: confirmed.startTime.toString(),
      endTime: confirmed.endTime.toString(),
      protocolVersion: confirmed.protocolVersion.toString(),
      datumSha256: shaDatum(fixture.queueDatum),
    },
    queue,
    scheduler: {
      operatorVkey: operator,
      shiftStartTime: "900",
      datumSha256: shaDatum(fixture.schedulerDatum),
    },
    activeOperators: [
      {
        operatorVkey: operator,
        nextOperatorVkey: null,
        bondUnlockTime: null,
        inactivityStrikes: "0",
        datumSha256: shaDatum(fixture.activeNodeDatum),
      },
    ],
    retiredOperators: [],
    quarantinedFromHeaderHash: null,
  });
  expect(snapshot).not.toBeNull();
  return snapshot!;
};

const bootstrapBundle = () => {
  const fixture = rootFixtures();
  const body = bodyFrom(
    [],
    [],
    fixture.outputs,
    [
      {
        policyId: policy.stateQueuePolicyId,
        assetName: policy.stateQueueRootAssetNameHex,
        quantity: 1n,
      },
    ],
    null,
    null,
  );
  const block = l1Block(body, fixture.outputs, [
    {
      purpose: "mint",
      index: "0",
      bytesHex: Data.to({ InitV1: { output_index: 0n } }, StateQueueRedeemer),
    },
  ]);
  const store = storeFor(block, protocolRecords(block, fixture.outputs), null);
  const snapshot = snapshotFor(fixture);
  const observation = observationFor(
    block,
    store,
    snapshot,
    "bootstrap",
    null,
    null,
    null,
  );
  return {
    fixture,
    block,
    store,
    snapshot,
    observation,
    context: contextFor(block, store),
  };
};

const attachDaBundle = (input: {
  header: NonNullable<ReturnType<typeof makeWatcherStateQueueHeaderV1>>;
  appendBlock: ReturnType<typeof l1Block>;
  appendStore: WatcherDurableStoreV1;
  appendSnapshot: WatcherStateQueueSnapshotV1;
  predecessorStateDigest: string;
  applyOutputIndex?: bigint;
  validFrom?: bigint;
}) => {
  const attestationDatum = canonicalDatum(
    Data.to(
      {
        header_hash: input.header.headerHash,
        da_threshold: 1n,
        committee_signers_hash: DA_SIGNERS_HASH,
        attested_signers: `${"01"}${"00".repeat(31)}`,
        attestation_count: 1n,
      },
      DaAttestationDatum,
    ),
  );
  const attestationOutputs: readonly OutputFixture[] = [
    {
      role: "proof_thread",
      datumHex: attestationDatum,
      outputHex: output(
        policy.daAttestationAddressHex,
        policy.daAttestationPolicyId,
        `${policy.daAttestationAssetPrefixHex}${input.header.headerHash}`,
        attestationDatum,
      ),
    },
  ];
  const producerBody = bodyFrom(
    [],
    [],
    attestationOutputs,
    [
      {
        policyId: policy.daAttestationPolicyId,
        assetName: `${policy.daAttestationAssetPrefixHex}${input.header.headerHash}`,
        quantity: 1n,
      },
    ],
    input.validFrom ?? null,
    null,
  );
  const producer = l1Block(producerBody, attestationOutputs, []);
  const attestationOutRef = `${producer.txHash}#0`;
  const oldNode = `${input.appendBlock.txHash}#1`;
  const attachedNodeDatum = linkedNode(
    {
      header: Data.from(input.header.headerCborHex, HeaderV1),
      da_attestation: policy.daAttestationPolicyId,
    },
    StateQueueNodeV1,
    null,
  );
  const outputs: readonly OutputFixture[] = [
    {
      role: "state_queue",
      datumHex: attachedNodeDatum,
      outputHex: output(
        policy.stateQueueAddressHex,
        policy.stateQueuePolicyId,
        `${policy.stateQueueNodeAssetPrefixHex}${input.header.headerHash}`,
        attachedNodeDatum,
      ),
    },
  ];
  const bodyHex = bodyFrom(
    [oldNode, attestationOutRef],
    [
      deploymentAuthorityFixture.policy.referenceScripts["state-queue minting"]!
        .outRef,
    ],
    outputs,
    [
      {
        policyId: policy.daAttestationPolicyId,
        assetName: `${policy.daAttestationAssetPrefixHex}${input.header.headerHash}`,
        quantity: -1n,
      },
    ],
    input.validFrom ?? null,
    null,
  );
  const body = CML.TransactionBody.from_cbor_hex(bodyHex);
  const inputs: string[] = [];
  for (let index = 0; index < body.inputs().len(); index += 1) {
    const txInput = body.inputs().get(index);
    inputs.push(
      `${txInput.transaction_id().to_hex()}#${txInput.index().toString()}`,
    );
  }
  const stateQueueInputIndex = inputs.indexOf(oldNode);
  const attestationInputIndex = inputs.indexOf(attestationOutRef);
  const selected = l1Block(bodyHex, outputs, [
    {
      purpose: "spend",
      index: attestationInputIndex.toString(),
      bytesHex: Data.to(
        { BurnForStateQueue: { mint_redeemer_index: 2n } },
        DaAttestationSpendRedeemer,
      ),
    },
    {
      purpose: "spend",
      index: stateQueueInputIndex.toString(),
      bytesHex: Data.to(
        {
          AttachDaAttestation: {
            state_queue_input_index: BigInt(stateQueueInputIndex),
            da_attestation_mint_redeemer_index: 2n,
          },
        },
        StateQueueSpendRedeemer,
      ),
    },
    {
      purpose: "mint",
      index: "0",
      bytesHex: Data.to(
        {
          ApplyToStateQueue: {
            da_attestation_input_index: BigInt(attestationInputIndex),
            state_queue_input_index: BigInt(stateQueueInputIndex),
            state_queue_output_index: input.applyOutputIndex ?? 0n,
            state_queue_mint_ref_script_input_index: 0n,
          },
        },
        DaAttestationMintRedeemer,
      ),
    },
  ]);
  const raw = structuredClone(selected.raw);
  raw.transactions.unshift(producer.raw.transactions[0]!);
  const block = {
    ...selected,
    raw,
    normalized: normalizeWatcherL1BlockV1(provider, raw),
  };
  const store = storeFor(
    block,
    [
      ...input.appendStore.protocolUtxos.filter(
        ({ outRef }) => outRef !== oldNode,
      ),
      ...protocolRecords(block, outputs),
    ],
    input.appendStore,
  );
  const priorHeader = input.appendSnapshot.queue[0]!;
  const attachedHeader = makeWatcherStateQueueHeaderV1({
    ...priorHeader,
    nextHeaderHash: priorHeader.nextHeaderHash,
    datumSha256: shaDatum(attachedNodeDatum),
    daAttestationPolicyId: policy.daAttestationPolicyId,
  });
  expect(attachedHeader).not.toBeNull();
  const snapshot = makeWatcherStateQueueSnapshotV1({
    confirmedState: input.appendSnapshot.confirmedState,
    queue: [attachedHeader!],
    scheduler: input.appendSnapshot.scheduler,
    activeOperators: input.appendSnapshot.activeOperators,
    retiredOperators: input.appendSnapshot.retiredOperators,
    quarantinedFromHeaderHash: input.appendSnapshot.quarantinedFromHeaderHash,
  });
  expect(snapshot).not.toBeNull();
  const observation = observationFor(
    block,
    store,
    snapshot!,
    "attach_da",
    input.predecessorStateDigest,
    null,
    null,
  );
  const context = contextFor(block, store);
  return {
    block,
    store,
    snapshot: snapshot!,
    observation,
    context,
    attestationInputIndex,
    stateQueueInputIndex,
  };
};

describe("authenticated state-queue indexer", () => {
  it("derives canonical HeaderV1 identity from real Plutus data", () => {
    const header = makeWatcherStateQueueHeaderV1({
      nextHeaderHash: null,
      datumSha256: h32("99"),
      prevUtxosRoot: EMPTY_ROOT,
      utxosRoot: h32("81"),
      withdrawalsRoot: EMPTY_ROOT,
      forcedTransactionsRoot: EMPTY_ROOT,
      transactionsRoot: h32("82"),
      depositsRoot: EMPTY_ROOT,
      transitionTraceRoot: h32("83"),
      eventToStepRoot: h32("84"),
      validationTracesRoot: h32("85"),
      withdrawalCount: "0",
      forcedTransactionCount: "0",
      l2TransactionCount: "1",
      depositCount: "0",
      totalEventCount: "1",
      transitionStepCount: "1",
      validationTraceCount: "1",
      startTime: "1000",
      endTime: "2000",
      blockSlot: "42",
      expectedNetworkId: "0",
      minFeeA: "44",
      minFeeB: "155381",
      prevHeaderHash: confirmed.headerHash,
      operatorVkey: operator,
      protocolVersion: "1",
      daAttestationPolicyId: null,
    });
    expect(header).not.toBeNull();
    expect(Data.to(Data.from(header!.headerCborHex, HeaderV1), HeaderV1)).toBe(
      header!.headerCborHex,
    );
    expect(
      makeWatcherStateQueueHeaderV1({
        ...header!,
        datumSha256: h32("99"),
        nextHeaderHash: null,
        totalEventCount: "2",
      }),
    ).toBeNull();
    for (const mutation of [
      { startTime: header!.endTime },
      { validationTraceCount: "0" },
      { transactionsRoot: EMPTY_ROOT },
      { transitionTraceRoot: EMPTY_ROOT },
      { eventToStepRoot: EMPTY_ROOT },
      { validationTracesRoot: EMPTY_ROOT },
    ]) {
      expect(
        makeWatcherStateQueueHeaderV1({
          ...header!,
          nextHeaderHash: null,
          datumSha256: h32("99"),
          ...mutation,
        }),
      ).toBeNull();
    }
  });

  it("bootstraps only from canonical Cardano outputs and Aiken datums", () => {
    const bundle = bootstrapBundle();
    // This assertion makes the upstream W11/W12 authority failure explicit.
    expect(bundle.context.finalityAuthority?.result).toMatchObject({
      protocolDecision: "finality_granted",
    });
    const result = evaluateWatcherStateQueueIndexerV1(
      policy,
      null,
      bundle.observation,
      bundle.context,
    );
    expect(result.action, JSON.stringify(result)).toBe("accept");
    expect(result.reasonCodes).toEqual(["bootstrap_authenticated"]);
    expect(result.state).not.toBeNull();
    expect(parseWatcherStateQueueIndexerStateV1(result.state, policy)).toEqual(
      result.state,
    );
    expect(
      parseWatcherStateQueueIndexerResultV1(result, {
        policy,
        previousState: null,
        observation: bundle.observation,
        publicContext: bundle.context,
      }),
    ).toEqual(result);
  });

  it("rejects cyclic, aliased, and cumulatively oversized evidence before recursive parsing", () => {
    const bundle = bootstrapBundle();
    const cyclicContext = asWireValue(bundle.context) as Record<string, any>;
    cyclicContext.sourceDurableStore = cyclicContext;
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        bundle.observation,
        cyclicContext,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const aliasedContext = asWireValue(bundle.context) as Record<string, any>;
    aliasedContext.durableStore = aliasedContext.sourceDurableStore;
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        bundle.observation,
        aliasedContext,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const oversizedObservation = asWireValue(bundle.observation) as Record<
      string,
      any
    >;
    oversizedObservation.blockHash = "a".repeat(5 * 1_024 * 1_024);
    oversizedObservation.publicInputDigest = "b".repeat(5 * 1_024 * 1_024);
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        oversizedObservation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_observation"],
    });

    const boot = evaluateWatcherStateQueueIndexerV1(
      policy,
      null,
      bundle.observation,
      bundle.context,
    ).state!;
    const cyclicState = JSON.parse(JSON.stringify(boot)) as Record<string, any>;
    cyclicState.history[0].publicContext.rollbackResult = cyclicState;
    expect(
      parseWatcherStateQueueIndexerStateV1(cyclicState, policy),
    ).toBeNull();
  });

  it("accepts one authoritative local-node chain-sync source without a provider quorum", () => {
    const external = bootstrapBundle();
    const block = asLocalNodeBlock(external.block);
    const store = storeFor(
      block,
      protocolRecords(block, external.fixture.outputs),
      null,
    );
    const observation = observationFor(
      block,
      store,
      external.snapshot,
      "bootstrap",
      null,
      null,
      null,
    );
    const context = localNodeContextFor(block, store);
    expect(context.finalityAuthority?.consistency).toMatchObject({
      status: "agreed",
      sourceMode: "local_node",
      authorityNodeId: LOCAL_NODE_ID,
      independentProviderCount: 1,
      queryObservationCount: 0,
    });
    expect(
      evaluateWatcherStateQueueIndexerV1(policy, null, observation, context),
    ).toMatchObject({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: ["bootstrap_authenticated"],
    });
    expect(
      evaluateWatcherStateQueueIndexerV1(policy, null, observation, {
        ...context,
        finalityAuthority: external.context.finalityAuthority,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("rejects a caller-supplied snapshot even when the extra object is self-rehashed", () => {
    const bundle = bootstrapBundle();
    const { observationDigest: _digest, ...fields } = bundle.observation;
    const withSnapshot = {
      ...fields,
      snapshot: bundle.snapshot,
    };
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        {
          ...withSnapshot,
          observationDigest: canonicalDigest(withSnapshot),
        },
        bundle.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_observation"],
    });
  });

  it("rejects altered durable output bytes instead of trusting JSON labels", () => {
    const bundle = bootstrapBundle();
    const altered = structuredClone(bundle.context) as {
      durableStore: {
        protocolUtxos: Array<{ output: { cborHex: string } }>;
      };
    };
    altered.durableStore.protocolUtxos[0]!.output.cborHex = "80";
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        bundle.observation,
        altered,
      ),
    ).toMatchObject({ action: "reject" });
  });

  it("rejects self-rehashed W02, W03, W11 and W12 authority substitutions", () => {
    const bundle = bootstrapBundle();
    const forgedDeployment = structuredClone(bundle.context) as Mutable;
    forgedDeployment.deploymentAuthority.policy.appliedScriptHashes.stateQueueMint =
      policy.schedulerPolicyId;
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        bundle.observation,
        forgedDeployment,
      ),
    ).toMatchObject({ action: "reject" });

    const decoy = bootstrapBundle();
    const relabelledStore = makeWatcherDurableStoreV1({
      deploymentMarker: policy.deploymentMarker,
      revision: bundle.store.revision,
      records: {
        l1Observations: [
          ...bundle.store.l1Observations,
          ...decoy.store.l1Observations,
        ],
        chainPoints: [...bundle.store.chainPoints, ...decoy.store.chainPoints],
        protocolUtxos: bundle.store.protocolUtxos.map((record, index) =>
          index === 0
            ? {
                ...record,
                chainPointId: decoy.block.normalized.chainPoint.chainPointId,
              }
            : record,
        ),
        daProofInputs: bundle.store.daProofInputs,
        reconstructedStates: bundle.store.reconstructedStates,
        decisions: bundle.store.decisions,
        faults: bundle.store.faults,
        submissions: bundle.store.submissions,
        confirmations: bundle.store.confirmations,
        retries: bundle.store.retries,
        deadlines: bundle.store.deadlines,
        correctionResults: bundle.store.correctionResults,
      },
    });
    const relabelledObservation = remakeObservation(bundle.observation, {
      durableStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(relabelledStore),
      ),
    });
    expect(
      evaluateWatcherStateQueueIndexerV1(policy, null, relabelledObservation, {
        ...bundle.context,
        durableStore: relabelledStore,
      }),
    ).toMatchObject({ action: "reject" });

    const divergent = structuredClone(bundle.context) as Mutable;
    const divergentRaw = structuredClone(bundle.block.raw);
    divergentRaw.providerId = providerB.providerId;
    divergentRaw.chainPoint.blockHash = h32("ed");
    const normalized = [
      bundle.block.normalized,
      normalizeWatcherL1BlockV1(providerB, divergentRaw),
    ];
    divergent.finalityAuthority.consistency =
      evaluateWatcherMultiProviderConsistencyV1(externalSource, normalized);
    divergent.finalityAuthority.result = evaluateWatcherFinalityV1(
      finalityPolicy,
      divergent.finalityAuthority.previousState,
      divergent.finalityAuthority.consistency,
    );
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        bundle.observation,
        divergent,
      ),
    ).toMatchObject({ action: "reject" });
  });

  it("rejects an independently rehashed fake second provider behind forged W11/W12 summaries", () => {
    const bundle = bootstrapBundle();
    const hostile = structuredClone(bundle.context) as Mutable;
    const fakeProvider = {
      ...provider,
      providerId: providerB.providerId,
    };
    const fakeRaw = structuredClone(bundle.block.raw);
    fakeRaw.providerId = fakeProvider.providerId;
    const fakeNormalized = normalizeWatcherL1BlockV1(fakeProvider, fakeRaw);
    expect(fakeNormalized.observationDigest).not.toBe(
      bundle.block.normalized.observationDigest,
    );
    hostile.finalityAuthority.observations[1] = {
      authenticatedProvider: fakeProvider,
      l1Observation: fakeRaw,
    };

    const { consistencyDigest: _consistencyDigest, ...summaryWithoutDigest } =
      hostile.finalityAuthority.consistency;
    const forgedSummaryWithoutDigest = {
      ...summaryWithoutDigest,
      observationEvidenceDigests: [
        bundle.block.normalized.observationDigest,
        fakeNormalized.observationDigest,
      ].sort(),
    };
    const forgedConsistency = {
      ...forgedSummaryWithoutDigest,
      consistencyDigest: createHash("sha256")
        .update(JSON.stringify(forgedSummaryWithoutDigest), "utf8")
        .digest("hex"),
    };
    hostile.finalityAuthority.consistency = forgedConsistency;
    hostile.finalityAuthority.result = evaluateWatcherFinalityV1(
      hostile.finalityAuthority.policy,
      hostile.finalityAuthority.previousState,
      forgedConsistency,
    );
    expect(hostile.finalityAuthority.result).toMatchObject({
      protocolDecision: "finality_granted",
    });

    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        bundle.observation,
        hostile,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("rejects a fully recomputed external-to-local source-mode downgrade", () => {
    const bundle = bootstrapBundle();
    const localBlock = asLocalNodeBlock(bundle.block);
    const localAuthority = localNodeContextFor(
      localBlock,
      bundle.store,
    ).finalityAuthority;
    expect(localAuthority?.policy).toMatchObject({
      sourceMode: "local_node",
    });
    expect(localAuthority?.result).toMatchObject({
      protocolDecision: "finality_granted",
    });
    expect(
      evaluateWatcherStateQueueIndexerV1(policy, null, bundle.observation, {
        ...bundle.context,
        finalityAuthority: localAuthority,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("indexes node-accepted append, DA, merge, and rollback output bytes without replaying validators", () => {
    const bootBundle = bootstrapBundle();
    const bootResult = evaluateWatcherStateQueueIndexerV1(
      policy,
      null,
      bootBundle.observation,
      bootBundle.context,
    );
    const boot = bootResult.state!;
    const headerBase = makeWatcherStateQueueHeaderV1({
      nextHeaderHash: null,
      datumSha256: h32("00"),
      prevUtxosRoot: confirmed.utxoRoot,
      utxosRoot: h32("81"),
      withdrawalsRoot: EMPTY_ROOT,
      forcedTransactionsRoot: EMPTY_ROOT,
      transactionsRoot: h32("82"),
      depositsRoot: EMPTY_ROOT,
      transitionTraceRoot: h32("83"),
      eventToStepRoot: h32("84"),
      validationTracesRoot: h32("85"),
      withdrawalCount: "0",
      forcedTransactionCount: "0",
      l2TransactionCount: "1",
      depositCount: "0",
      totalEventCount: "1",
      transitionStepCount: "1",
      validationTraceCount: "1",
      startTime: "1000",
      endTime: "2000",
      blockSlot: "42",
      expectedNetworkId: "0",
      minFeeA: "44",
      minFeeB: "155381",
      prevHeaderHash: confirmed.headerHash,
      operatorVkey: operator,
      protocolVersion: "1",
      daAttestationPolicyId: null,
    })!;
    const upper = 5_000n;
    const rootDatum = linkedRoot(
      confirmed,
      ConfirmedState,
      headerBase.headerHash,
    );
    const nodeData: StateQueueNodeV1 = {
      header: Data.from(headerBase.headerCborHex, HeaderV1),
      da_attestation: "",
    };
    const nodeDatum = linkedNode(nodeData, StateQueueNodeV1, null);
    const activeDatum = linkedNode(
      {
        bond_unlock_time: upper - 1n + maturity,
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
      null,
    );
    const appendOutputs: readonly OutputFixture[] = [
      {
        role: "state_queue",
        datumHex: rootDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          policy.stateQueueRootAssetNameHex,
          rootDatum,
        ),
      },
      {
        role: "state_queue",
        datumHex: nodeDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          `${policy.stateQueueNodeAssetPrefixHex}${headerBase.headerHash}`,
          nodeDatum,
        ),
      },
      {
        role: "operator_directory",
        datumHex: activeDatum,
        outputHex: output(
          policy.activeOperatorsAddressHex,
          policy.activeOperatorsPolicyId,
          `${policy.activeOperatorAssetPrefixHex}${operator}`,
          activeDatum,
        ),
      },
    ];
    const oldRoot = `${bootBundle.block.txHash}#0`;
    const oldActive = `${bootBundle.block.txHash}#2`;
    const scheduler = `${bootBundle.block.txHash}#4`;
    const hubOracle = `${bootBundle.block.txHash}#5`;
    const body = bodyFrom(
      [oldRoot, oldActive],
      [scheduler, hubOracle],
      appendOutputs,
      [
        {
          policyId: policy.stateQueuePolicyId,
          assetName: `${policy.stateQueueNodeAssetPrefixHex}${headerBase.headerHash}`,
          quantity: 1n,
        },
      ],
      4_000n,
      upper,
    );
    const appendRedeemers = [
      {
        purpose: "spend" as const,
        index: "0",
        bytesHex: Data.to("LinkedListMutation", StateQueueSpendRedeemer),
      },
      {
        purpose: "spend" as const,
        index: "1",
        bytesHex: Data.to(
          {
            UpdateBondHoldNewState: {
              active_operator: operator,
              active_node_input_index: 1n,
              active_node_output_index: 2n,
              hub_oracle_ref_input_index: 1n,
              state_queue_redeemer_index: 2n,
            },
          },
          ActiveOperatorSpendRedeemer,
        ),
      },
      {
        purpose: "mint" as const,
        index: "0",
        bytesHex: Data.to(
          {
            CommitBlockHeader: {
              new_block_output_index: 1n,
              continued_latest_block_output_index: 0n,
              operator,
              scheduler_ref_input_index: 0n,
              active_operators_input_index: 1n,
              active_operators_redeemer_index: 1n,
            },
          },
          StateQueueRedeemer,
        ),
      },
    ];
    const block = l1Block(body, appendOutputs, appendRedeemers);
    const previousProtocols = bootBundle.store.protocolUtxos.filter(
      ({ outRef }) => outRef !== oldRoot && outRef !== oldActive,
    );
    const protocols = [
      ...previousProtocols,
      ...protocolRecords(block, appendOutputs),
    ];
    const store = storeFor(block, protocols, bootBundle.store);
    const queueHeader = {
      ...headerBase,
      datumSha256: shaDatum(nodeDatum),
      daAttestationPolicyId: null,
    };
    const snapshot = makeWatcherStateQueueSnapshotV1({
      confirmedState: {
        ...boot.snapshot.confirmedState,
        datumSha256: shaDatum(rootDatum),
      },
      queue: [queueHeader],
      scheduler: boot.snapshot.scheduler,
      activeOperators: [
        {
          operatorVkey: operator,
          nextOperatorVkey: null,
          bondUnlockTime: (upper - 1n + maturity).toString(),
          inactivityStrikes: "0",
          datumSha256: shaDatum(activeDatum),
        },
      ],
      retiredOperators: [],
      quarantinedFromHeaderHash: null,
    })!;
    const observation = observationFor(
      block,
      store,
      snapshot,
      "append",
      boot.stateDigest,
      "4000",
      upper.toString(),
    );
    const context = contextFor(block, store);
    const result = evaluateWatcherStateQueueIndexerV1(
      policy,
      boot,
      observation,
      context,
    );
    expect(result.action, JSON.stringify(result)).toBe("accept");
    expect(result.reasonCodes).toEqual(["append_authenticated"]);
    expect(result.state?.snapshot.queue).toHaveLength(1);
    expect(parseWatcherStateQueueIndexerStateV1(result.state, policy)).toEqual(
      result.state,
    );
    expect(store.spentProtocolUtxos.length).toBeGreaterThan(0);
    for (const spentProtocolUtxos of [
      [],
      store.spentProtocolUtxos.map((entry, index) =>
        index === 0
          ? {
              ...entry,
              spentAtChainPointId:
                bootBundle.block.normalized.chainPoint.chainPointId,
            }
          : entry,
      ),
    ]) {
      const hostileStore = remakeStore(store, { spentProtocolUtxos });
      const hostileObservation = remakeObservation(observation, {
        durableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(hostileStore),
        ),
      });
      expect(
        evaluateWatcherStateQueueIndexerV1(policy, boot, hostileObservation, {
          ...context,
          durableStore: hostileStore,
        }),
      ).toMatchObject({ action: "reject" });
    }
    const attach = attachDaBundle({
      header: headerBase,
      appendBlock: block,
      appendStore: store,
      appendSnapshot: snapshot,
      predecessorStateDigest: result.state!.stateDigest,
    });
    const attached = evaluateWatcherStateQueueIndexerV1(
      policy,
      result.state,
      attach.observation,
      attach.context,
    );
    expect(attached.action, JSON.stringify(attached)).toBe("accept");
    expect(attached.reasonCodes).toEqual(["da_attestation_authenticated"]);

    const mergedConfirmed: ConfirmedState = {
      headerHash: headerBase.headerHash,
      prevHeaderHash: confirmed.headerHash,
      utxoRoot: headerBase.utxosRoot,
      startTime: confirmed.startTime,
      endTime: BigInt(headerBase.endTime),
      protocolVersion: 1n,
    };
    const mergedRootDatum = linkedRoot(mergedConfirmed, ConfirmedState, null);
    const settlementDatum = canonicalDatum(
      Data.to(
        {
          deposits_root: headerBase.depositsRoot,
          withdrawals_root: headerBase.withdrawalsRoot,
          forced_transactions_root: headerBase.forcedTransactionsRoot,
          transactions_root: headerBase.transactionsRoot,
          resolution_claim: null,
        },
        SettlementDatum,
      ),
    );
    const mergeOutputs: readonly OutputFixture[] = [
      {
        role: "state_queue",
        datumHex: mergedRootDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          policy.stateQueueRootAssetNameHex,
          mergedRootDatum,
        ),
      },
      {
        role: "settlement",
        datumHex: settlementDatum,
        outputHex: output(
          scriptAddress(applied.settlementSpend!),
          applied.settlementMint!,
          headerBase.headerHash,
          settlementDatum,
        ),
      },
    ];
    const mergeOldRoot = `${block.txHash}#0`;
    const attachedNode = `${attach.block.txHash}#0`;
    const mergeHubOracle = `${bootBundle.block.txHash}#5`;
    const mergeLower = BigInt(headerBase.endTime) + maturity;
    const mergeBodyHex = bodyFrom(
      [mergeOldRoot, attachedNode],
      [mergeHubOracle],
      mergeOutputs,
      [
        {
          policyId: policy.stateQueuePolicyId,
          assetName: `${policy.stateQueueNodeAssetPrefixHex}${headerBase.headerHash}`,
          quantity: -1n,
        },
        {
          policyId: applied.settlementMint!,
          assetName: headerBase.headerHash,
          quantity: 1n,
        },
      ],
      mergeLower,
      null,
    );
    const mergeBody = CML.TransactionBody.from_cbor_hex(mergeBodyHex);
    const mergeInputs: string[] = [];
    for (let index = 0; index < mergeBody.inputs().len(); index += 1) {
      const txInput = mergeBody.inputs().get(index);
      mergeInputs.push(
        `${txInput.transaction_id().to_hex()}#${txInput.index().toString()}`,
      );
    }
    const mintPolicyIndex = (policyId: string): number => {
      const keys = mergeBody.mint()!.keys();
      for (let index = 0; index < keys.len(); index += 1) {
        if (keys.get(index).to_hex() === policyId) {
          return index;
        }
      }
      return -1;
    };
    const statePolicyIndex = mintPolicyIndex(policy.stateQueuePolicyId);
    const settlementPolicyIndex = mintPolicyIndex(applied.settlementMint!);
    const stateRedeemerOrdinal =
      2n + (statePolicyIndex < settlementPolicyIndex ? 0n : 1n);
    const settlementRedeemerOrdinal =
      2n + (settlementPolicyIndex < statePolicyIndex ? 0n : 1n);
    const mergeBlock = l1Block(mergeBodyHex, mergeOutputs, [
      {
        purpose: "spend",
        index: mergeInputs.indexOf(mergeOldRoot).toString(),
        bytesHex: Data.to("LinkedListMutation", StateQueueSpendRedeemer),
      },
      {
        purpose: "spend",
        index: mergeInputs.indexOf(attachedNode).toString(),
        bytesHex: Data.to("LinkedListMutation", StateQueueSpendRedeemer),
      },
      {
        purpose: "mint",
        index: statePolicyIndex.toString(),
        bytesHex: Data.to(
          {
            MergeToConfirmedStateV1: {
              header_node_key: headerBase.headerHash,
              confirmed_state_input_outref: {
                transactionId: block.txHash,
                outputIndex: 0n,
              },
              confirmed_state_output_index: 0n,
              m_settlement_redeemer_index: settlementRedeemerOrdinal,
              merged_block_withdrawals_root: headerBase.withdrawalsRoot,
              merged_block_forced_transactions_root:
                headerBase.forcedTransactionsRoot,
              merged_block_transactions_root: headerBase.transactionsRoot,
              merged_block_deposits_root: headerBase.depositsRoot,
              merged_block_transition_trace_root:
                headerBase.transitionTraceRoot,
              merged_block_event_to_step_root: headerBase.eventToStepRoot,
              merged_block_validation_traces_root:
                headerBase.validationTracesRoot,
              merged_block_withdrawal_count: 0n,
              merged_block_forced_transaction_count: 0n,
              merged_block_l2_transaction_count: 1n,
              merged_block_deposit_count: 0n,
              merged_block_total_event_count: 1n,
              merged_block_transition_step_count: 1n,
              merged_block_validation_trace_count: 1n,
            },
          },
          StateQueueRedeemer,
        ),
      },
      {
        purpose: "mint",
        index: settlementPolicyIndex.toString(),
        bytesHex: Data.to(
          {
            Spawn: {
              settlement_id: headerBase.headerHash,
              output_index: 1n,
              state_queue_merge_redeemer_index: stateRedeemerOrdinal,
              hub_ref_input_index: 0n,
            },
          },
          SettlementMintRedeemer,
        ),
      },
    ]);
    const mergeStore = storeFor(
      mergeBlock,
      [
        ...attach.store.protocolUtxos.filter(
          ({ outRef }) => outRef !== mergeOldRoot && outRef !== attachedNode,
        ),
        ...protocolRecords(mergeBlock, mergeOutputs),
      ],
      attach.store,
    );
    const mergeSnapshot = makeWatcherStateQueueSnapshotV1({
      confirmedState: {
        headerHash: mergedConfirmed.headerHash,
        prevHeaderHash: mergedConfirmed.prevHeaderHash,
        utxosRoot: mergedConfirmed.utxoRoot,
        startTime: mergedConfirmed.startTime.toString(),
        endTime: mergedConfirmed.endTime.toString(),
        protocolVersion: mergedConfirmed.protocolVersion.toString(),
        datumSha256: shaDatum(mergedRootDatum),
      },
      queue: [],
      scheduler: attach.snapshot.scheduler,
      activeOperators: attach.snapshot.activeOperators,
      retiredOperators: attach.snapshot.retiredOperators,
      quarantinedFromHeaderHash: null,
    })!;
    const mergeObservation = observationFor(
      mergeBlock,
      mergeStore,
      mergeSnapshot,
      "merge",
      attached.state!.stateDigest,
      mergeLower.toString(),
      null,
    );
    const merged = evaluateWatcherStateQueueIndexerV1(
      policy,
      attached.state,
      mergeObservation,
      contextFor(mergeBlock, mergeStore),
    );
    expect(merged.action, JSON.stringify(merged)).toBe("accept");
    expect(merged.reasonCodes).toEqual(["merge_authenticated"]);

    const rollbackFinalityPolicy = finalityPolicyAtDepth(5);
    const pairedObservations = (selectedBlock: ReturnType<typeof l1Block>) => {
      const providerBObservation = structuredClone(selectedBlock.raw);
      providerBObservation.providerId = providerB.providerId;
      return [
        {
          authenticatedProvider: provider,
          l1Observation: selectedBlock.raw,
        },
        {
          authenticatedProvider: providerB,
          l1Observation: providerBObservation,
        },
      ] as const;
    };
    const oldObservations = pairedObservations(mergeBlock);
    const replacementObservations = pairedObservations(attach.block);
    const normalizeAuthorityObservations = (
      observations: ReturnType<typeof pairedObservations>,
    ) =>
      observations.map(({ authenticatedProvider, l1Observation }) =>
        normalizeWatcherL1BlockV1(authenticatedProvider, l1Observation),
      );
    const oldNormalized = normalizeAuthorityObservations(oldObservations);
    const replacementNormalized = normalizeAuthorityObservations(
      replacementObservations,
    );
    const secondReplacementObservations = pairedObservations(block);
    const secondReplacementNormalized = normalizeAuthorityObservations(
      secondReplacementObservations,
    );
    const oldConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      oldNormalized,
    );
    const previousFinalityState = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      null,
      oldConsistency,
    ).state!;
    const replacementConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      replacementNormalized,
    );
    const rollbackFinalityResult = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      previousFinalityState,
      replacementConsistency,
    );
    expect(rollbackFinalityResult.action).toBe("rewind_pending");
    const additionalNormalized = [
      oldNormalized[1]!,
      replacementNormalized[1]!,
      secondReplacementNormalized[1]!,
    ];
    const rollbackSourceStore = remakeStore(
      mergeStore,
      {
        l1Observations: [
          ...mergeStore.l1Observations,
          ...additionalNormalized.map((normalized) => ({
            observationId: normalized.observationDigest,
            providerId: normalized.provider.providerId,
            chainPointId: normalized.chainPoint.chainPointId,
            payload: makeWatcherDurablePayloadV1(
              encodeWatcherNormalizedL1BlockV1(normalized).toString("hex"),
            ),
          })),
        ],
        chainPoints: [
          ...mergeStore.chainPoints,
          ...additionalNormalized.map((normalized) => ({
            chainPointId: normalized.chainPoint.chainPointId,
            providerId: normalized.provider.providerId,
            blockHash: normalized.chainPoint.blockHash,
            slot: normalized.chainPoint.slot,
            blockNo: normalized.chainPoint.blockNo,
            depth: normalized.chainPoint.depth,
          })),
        ],
      },
      (BigInt(mergeStore.revision) + 1n).toString(),
    );
    const rollbackBootstrapState = makeWatcherRollbackBootstrapStateV1(
      rollbackFinalityPolicy,
      rollbackSourceStore,
      previousFinalityState,
    )!;
    const appliedRollback = evaluateWatcherRollbackV1(
      rollbackFinalityPolicy,
      rollbackSourceStore,
      previousFinalityState,
      replacementConsistency,
      rollbackFinalityResult,
      rollbackBootstrapState,
      rollbackBootstrapState,
    );
    expect(appliedRollback.action, JSON.stringify(appliedRollback)).toBe(
      "apply_rewind",
    );
    expect(appliedRollback.nextStore?.protocolUtxos).toEqual(
      attach.store.protocolUtxos,
    );
    expect(appliedRollback.nextStore?.spentProtocolUtxos).toEqual(
      attach.store.spentProtocolUtxos,
    );
    const rollbackVerificationContext = {
      policy: rollbackFinalityPolicy,
      sourceStore: rollbackSourceStore,
      previousFinalityState,
      consistency: replacementConsistency,
      finalityResult: rollbackFinalityResult,
      previousRollbackState: rollbackBootstrapState,
      rollbackBootstrapState,
    };
    const appliedStore = appliedRollback.nextStore!;
    storeSources.set(appliedStore, rollbackSourceStore);
    const rollbackObservation = observationFor(
      attach.block,
      appliedStore,
      attach.snapshot,
      "rollback",
      merged.state!.stateDigest,
      null,
      null,
      { transactionHash: null },
    );
    const rollbackContext: WatcherStateQueuePublicContextV1 = asWireValue({
      schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
      authenticatedProvider: provider,
      l1Observation: attach.block.raw,
      sourceDurableStore: rollbackSourceStore,
      durableStore: appliedStore,
      deploymentAuthority,
      finalityAuthority: null,
      rollbackAuthority: {
        result: appliedRollback,
        context: rollbackVerificationContext,
      },
    });
    const cyclicRollbackContext = asWireValue(rollbackContext) as Record<
      string,
      any
    >;
    cyclicRollbackContext.rollbackAuthority.result.nextStore =
      cyclicRollbackContext.rollbackAuthority.result;
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        merged.state,
        rollbackObservation,
        cyclicRollbackContext,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
    const rolledBack = evaluateWatcherStateQueueIndexerV1(
      policy,
      merged.state,
      rollbackObservation,
      rollbackContext,
    );
    expect(rolledBack.action, JSON.stringify(rolledBack)).toBe("accept");
    expect(rolledBack.reasonCodes).toEqual(["rollback_authenticated"]);
    const restarted = parseWatcherStateQueueIndexerStateV1(
      JSON.parse(JSON.stringify(rolledBack.state)),
      policy,
    );
    expect(restarted).toEqual(rolledBack.state);
    const forgedPriorLink = JSON.parse(
      JSON.stringify(rolledBack.state),
    ) as Record<string, any>;
    const forgedRollbackAudit = forgedPriorLink.auditHistory.at(-1)!;
    forgedRollbackAudit.entry.priorActiveEntryDigest = h32("fe");
    const { entryDigest: _forgedEntryDigest, ...forgedRollbackEntryFields } =
      forgedRollbackAudit.entry;
    forgedRollbackAudit.entry.entryDigest = canonicalDigest(
      forgedRollbackEntryFields,
    );
    const { auditDigest: _forgedAuditDigest, ...forgedRollbackAuditFields } =
      forgedRollbackAudit;
    forgedRollbackAudit.auditDigest = canonicalDigest(
      forgedRollbackAuditFields,
    );
    const { stateDigest: _forgedStateDigest, ...forgedStateFields } =
      forgedPriorLink;
    forgedPriorLink.stateDigest = canonicalDigest(forgedStateFields);
    expect(
      parseWatcherStateQueueIndexerStateV1(forgedPriorLink, policy),
    ).toBeNull();

    const noChangeBody = bodyFrom([], [], [], [], null, null);
    const noChangeBlock = l1Block(noChangeBody, [], []);
    const noChangeObservations = pairedObservations(noChangeBlock);
    const noChangeNormalized =
      normalizeAuthorityObservations(noChangeObservations);
    const noChangeConsistency = evaluateWatcherMultiProviderConsistencyV1(
      externalSource,
      noChangeNormalized,
    );
    const noChangePreviousFinalityState = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      null,
      noChangeConsistency,
    ).state!;
    const noChangeReplacementConsistency =
      evaluateWatcherMultiProviderConsistencyV1(
        externalSource,
        secondReplacementNormalized,
      );
    const noChangeFinalityResult = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      noChangePreviousFinalityState,
      noChangeReplacementConsistency,
    );
    expect(noChangeFinalityResult.action).toBe("rewind_pending");
    const noChangeStore = storeFor(noChangeBlock, store.protocolUtxos, store);
    const noChangeProviderB = noChangeNormalized[1]!;
    const noChangeReplacementProviderB = secondReplacementNormalized[1]!;
    const noChangeAdditionalEvidence = [
      noChangeProviderB,
      noChangeReplacementProviderB,
    ];
    const noChangeSourceStore = remakeStore(
      noChangeStore,
      {
        l1Observations: [
          ...noChangeStore.l1Observations,
          ...noChangeAdditionalEvidence.map((candidate) => ({
            observationId: candidate.observationDigest,
            providerId: candidate.provider.providerId,
            chainPointId: candidate.chainPoint.chainPointId,
            payload: makeWatcherDurablePayloadV1(
              encodeWatcherNormalizedL1BlockV1(candidate).toString("hex"),
            ),
          })),
        ],
        chainPoints: [
          ...noChangeStore.chainPoints,
          ...noChangeAdditionalEvidence.map((candidate) => ({
            chainPointId: candidate.chainPoint.chainPointId,
            providerId: candidate.provider.providerId,
            blockHash: candidate.chainPoint.blockHash,
            slot: candidate.chainPoint.slot,
            blockNo: candidate.chainPoint.blockNo,
            depth: candidate.chainPoint.depth,
          })),
        ],
      },
      (BigInt(noChangeStore.revision) + 1n).toString(),
    );
    const noChangeRollbackBootstrap = makeWatcherRollbackBootstrapStateV1(
      rollbackFinalityPolicy,
      noChangeSourceStore,
      noChangePreviousFinalityState,
    )!;
    const noChangeAppliedRollback = evaluateWatcherRollbackV1(
      rollbackFinalityPolicy,
      noChangeSourceStore,
      noChangePreviousFinalityState,
      noChangeReplacementConsistency,
      noChangeFinalityResult,
      noChangeRollbackBootstrap,
      noChangeRollbackBootstrap,
    );
    expect(
      noChangeAppliedRollback.action,
      JSON.stringify(noChangeAppliedRollback),
    ).toBe("apply_rewind");
    const noChangeAppliedStore = noChangeAppliedRollback.nextStore!;
    storeSources.set(noChangeAppliedStore, noChangeSourceStore);
    const noChangeObservation = observationFor(
      block,
      noChangeAppliedStore,
      snapshot,
      "rollback",
      result.state!.stateDigest,
      null,
      null,
      { transactionHash: null },
    );
    const noChangeRollbackState = evaluateWatcherStateQueueIndexerV1(
      policy,
      result.state,
      noChangeObservation,
      asWireValue({
        schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
        authenticatedProvider: provider,
        l1Observation: block.raw,
        sourceDurableStore: noChangeSourceStore,
        durableStore: noChangeAppliedStore,
        deploymentAuthority,
        finalityAuthority: null,
        rollbackAuthority: {
          result: noChangeAppliedRollback,
          context: {
            policy: rollbackFinalityPolicy,
            sourceStore: noChangeSourceStore,
            previousFinalityState: noChangePreviousFinalityState,
            consistency: noChangeReplacementConsistency,
            finalityResult: noChangeFinalityResult,
            previousRollbackState: noChangeRollbackBootstrap,
            rollbackBootstrapState: noChangeRollbackBootstrap,
          },
        },
      }),
    );
    expect(
      noChangeRollbackState.action,
      JSON.stringify(noChangeRollbackState),
    ).toBe("accept");
    expect(noChangeRollbackState.reasonCodes).toEqual([
      "rollback_authenticated",
    ]);
    expect(noChangeRollbackState.state?.history).toEqual(result.state!.history);
    expect(noChangeRollbackState.state?.auditHistory).toMatchObject([
      { status: "rollback" },
    ]);
    const afterNoChangeRestart = parseWatcherStateQueueIndexerStateV1(
      JSON.parse(JSON.stringify(noChangeRollbackState.state)),
      policy,
    );
    expect(afterNoChangeRestart).toEqual(noChangeRollbackState.state);
    const afterNoChangeAttach = attachDaBundle({
      header: headerBase,
      appendBlock: block,
      appendStore: noChangeAppliedStore,
      appendSnapshot: snapshot,
      predecessorStateDigest: afterNoChangeRestart!.stateDigest,
      validFrom: 2n,
    });
    const afterNoChangeNormal = evaluateWatcherStateQueueIndexerV1(
      policy,
      afterNoChangeRestart,
      afterNoChangeAttach.observation,
      afterNoChangeAttach.context,
    );
    expect(afterNoChangeNormal).toMatchObject({
      action: "accept",
      reasonCodes: ["da_attestation_authenticated"],
    });
    expect(
      parseWatcherStateQueueIndexerStateV1(
        JSON.parse(JSON.stringify(afterNoChangeNormal.state)),
        policy,
      ),
    ).toEqual(afterNoChangeNormal.state);

    const secondReplacementConsistency =
      evaluateWatcherMultiProviderConsistencyV1(
        externalSource,
        secondReplacementNormalized,
      );
    const secondPreviousFinalityState = rollbackFinalityResult.state!;
    const secondFinalityResult = evaluateWatcherFinalityV1(
      rollbackFinalityPolicy,
      secondPreviousFinalityState,
      secondReplacementConsistency,
    );
    expect(secondFinalityResult.action).toBe("rewind_pending");
    const secondAppliedRollback = evaluateWatcherRollbackV1(
      rollbackFinalityPolicy,
      appliedStore,
      secondPreviousFinalityState,
      secondReplacementConsistency,
      secondFinalityResult,
      appliedRollback.rollbackState,
      rollbackBootstrapState,
    );
    expect(
      secondAppliedRollback.action,
      JSON.stringify(secondAppliedRollback),
    ).toBe("apply_rewind");
    expect(secondAppliedRollback.nextStore?.protocolUtxos).toEqual(
      store.protocolUtxos,
    );
    expect(secondAppliedRollback.nextStore?.spentProtocolUtxos).toEqual(
      store.spentProtocolUtxos,
    );
    const secondAppliedStore = secondAppliedRollback.nextStore!;
    storeSources.set(secondAppliedStore, appliedStore);
    const repeatedRollbackObservation = observationFor(
      block,
      secondAppliedStore,
      snapshot,
      "rollback",
      restarted!.stateDigest,
      null,
      null,
      { transactionHash: null },
    );
    const secondRollbackVerificationContext = {
      policy: rollbackFinalityPolicy,
      sourceStore: appliedStore,
      previousFinalityState: secondPreviousFinalityState,
      consistency: secondReplacementConsistency,
      finalityResult: secondFinalityResult,
      previousRollbackState: appliedRollback.rollbackState,
      rollbackBootstrapState,
    };
    const repeatedRollback = evaluateWatcherStateQueueIndexerV1(
      policy,
      restarted,
      repeatedRollbackObservation,
      asWireValue({
        schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
        authenticatedProvider: provider,
        l1Observation: block.raw,
        sourceDurableStore: appliedStore,
        durableStore: secondAppliedStore,
        deploymentAuthority,
        finalityAuthority: null,
        rollbackAuthority: {
          result: secondAppliedRollback,
          context: secondRollbackVerificationContext,
        },
      }),
    );
    expect(repeatedRollback.action, JSON.stringify(repeatedRollback)).toBe(
      "accept",
    );
    expect(
      parseWatcherStateQueueIndexerStateV1(
        JSON.parse(JSON.stringify(repeatedRollback.state)),
        policy,
      ),
    ).toEqual(repeatedRollback.state);
    const postRollbackAttach = attachDaBundle({
      header: headerBase,
      appendBlock: block,
      appendStore: secondAppliedStore,
      appendSnapshot: snapshot,
      predecessorStateDigest: repeatedRollback.state!.stateDigest,
      validFrom: 1n,
    });
    const postRollbackAccepted = evaluateWatcherStateQueueIndexerV1(
      policy,
      repeatedRollback.state,
      postRollbackAttach.observation,
      postRollbackAttach.context,
    );
    expect(postRollbackAccepted).toMatchObject({
      action: "accept",
      reasonCodes: ["da_attestation_authenticated"],
    });
    expect(
      parseWatcherStateQueueIndexerStateV1(
        JSON.parse(JSON.stringify(postRollbackAccepted.state)),
        policy,
      ),
    ).toEqual(postRollbackAccepted.state);
    const duplicateRollback = evaluateWatcherRollbackV1(
      rollbackFinalityPolicy,
      secondAppliedStore,
      secondPreviousFinalityState,
      secondReplacementConsistency,
      secondFinalityResult,
      secondAppliedRollback.rollbackState,
      rollbackBootstrapState,
    );
    expect(duplicateRollback.action).toBe("duplicate_rewind");
    storeSources.set(secondAppliedStore, secondAppliedStore);
    const duplicateObservation = remakeObservation(
      repeatedRollbackObservation,
      {
        sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(secondAppliedStore),
        ),
        sourceDurableStoreRevision: secondAppliedStore.revision,
        predecessorStateDigest: repeatedRollback.state!.stateDigest,
      },
    );
    const duplicate = evaluateWatcherStateQueueIndexerV1(
      policy,
      repeatedRollback.state,
      duplicateObservation,
      asWireValue({
        schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
        authenticatedProvider: provider,
        l1Observation: block.raw,
        sourceDurableStore: secondAppliedStore,
        durableStore: secondAppliedStore,
        deploymentAuthority,
        finalityAuthority: null,
        rollbackAuthority: {
          result: duplicateRollback,
          context: {
            ...secondRollbackVerificationContext,
            sourceStore: secondAppliedStore,
            previousRollbackState: secondAppliedRollback.rollbackState,
          },
        },
      }),
    );
    expect(duplicate).toMatchObject({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_observation"],
      state: repeatedRollback.state,
    });

    const hostileAttach = attachDaBundle({
      header: headerBase,
      appendBlock: block,
      appendStore: store,
      appendSnapshot: snapshot,
      predecessorStateDigest: result.state!.stateDigest,
      applyOutputIndex: 1n,
    });
    expect(
      evaluateWatcherStateQueueIndexerV1(
        policy,
        result.state,
        hostileAttach.observation,
        hostileAttach.context,
      ),
    ).toMatchObject({
      action: "accept",
      reasonCodes: ["da_attestation_authenticated"],
    });
  });

  it("indexes the node-accepted removal from canonical output and datum bytes", () => {
    const makeHeader = (
      prevHeaderHash: string,
      prevUtxosRoot: string,
      utxosRoot: string,
      startTime: string,
      endTime: string,
      headerOperator: string,
    ) =>
      makeWatcherStateQueueHeaderV1({
        nextHeaderHash: null,
        datumSha256: h32("00"),
        prevUtxosRoot,
        utxosRoot,
        withdrawalsRoot: EMPTY_ROOT,
        forcedTransactionsRoot: EMPTY_ROOT,
        transactionsRoot: EMPTY_ROOT,
        depositsRoot: EMPTY_ROOT,
        transitionTraceRoot: EMPTY_ROOT,
        eventToStepRoot: EMPTY_ROOT,
        validationTracesRoot: EMPTY_ROOT,
        withdrawalCount: "0",
        forcedTransactionCount: "0",
        l2TransactionCount: "0",
        depositCount: "0",
        totalEventCount: "0",
        transitionStepCount: "0",
        validationTraceCount: "0",
        startTime,
        endTime,
        blockSlot: startTime,
        expectedNetworkId: "0",
        minFeeA: "44",
        minFeeB: "155381",
        prevHeaderHash,
        operatorVkey: headerOperator,
        protocolVersion: "1",
        daAttestationPolicyId: null,
      })!;
    const firstBase = makeHeader(
      confirmed.headerHash,
      confirmed.utxoRoot,
      h32("81"),
      "1000",
      "2000",
      operator,
    );
    const second = makeHeader(
      firstBase.headerHash,
      firstBase.utxosRoot,
      h32("82"),
      "2000",
      "3000",
      h28("bb"),
    );
    const first = {
      ...firstBase,
      nextHeaderHash: second.headerHash,
    };
    const queueRootDatum = linkedRoot(
      confirmed,
      ConfirmedState,
      first.headerHash,
    );
    const firstDatum = linkedNode(
      {
        header: Data.from(first.headerCborHex, HeaderV1),
        da_attestation: "",
      },
      StateQueueNodeV1,
      second.headerHash,
    );
    const secondDatum = linkedNode(
      {
        header: Data.from(second.headerCborHex, HeaderV1),
        da_attestation: "",
      },
      StateQueueNodeV1,
      null,
    );
    const base = rootFixtures();
    const proofDatum = canonicalDatum(
      Data.to({ fraud_prover: operator }, FraudProofTokenDatum),
    );
    const outputs: readonly OutputFixture[] = [
      {
        role: "state_queue",
        datumHex: queueRootDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          policy.stateQueueRootAssetNameHex,
          queueRootDatum,
        ),
      },
      {
        role: "state_queue",
        datumHex: firstDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          `${policy.stateQueueNodeAssetPrefixHex}${first.headerHash}`,
          firstDatum,
        ),
      },
      {
        role: "state_queue",
        datumHex: secondDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          `${policy.stateQueueNodeAssetPrefixHex}${second.headerHash}`,
          secondDatum,
        ),
      },
      ...base.outputs.slice(1),
      {
        role: "proof_thread",
        datumHex: proofDatum,
        outputHex: output(
          policy.fraudProofAddressHex,
          policy.fraudProofPolicyId,
          `${categoryIds[0]!}${first.headerHash}`,
          proofDatum,
        ),
      },
    ];
    const bootstrapBody = bodyFrom(
      [],
      [],
      outputs,
      [
        {
          policyId: policy.stateQueuePolicyId,
          assetName: policy.stateQueueRootAssetNameHex,
          quantity: 1n,
        },
      ],
      null,
      null,
    );
    const bootstrapBlock = l1Block(bootstrapBody, outputs, [
      {
        purpose: "mint",
        index: "0",
        bytesHex: Data.to({ InitV1: { output_index: 0n } }, StateQueueRedeemer),
      },
    ]);
    const bootstrapStore = storeFor(
      bootstrapBlock,
      protocolRecords(bootstrapBlock, outputs),
      null,
    );
    const bootstrapSnapshot = makeWatcherStateQueueSnapshotV1({
      confirmedState: {
        headerHash: confirmed.headerHash,
        prevHeaderHash: confirmed.prevHeaderHash,
        utxosRoot: confirmed.utxoRoot,
        startTime: confirmed.startTime.toString(),
        endTime: confirmed.endTime.toString(),
        protocolVersion: confirmed.protocolVersion.toString(),
        datumSha256: shaDatum(queueRootDatum),
      },
      queue: [
        {
          ...first,
          datumSha256: shaDatum(firstDatum),
        },
        {
          ...second,
          datumSha256: shaDatum(secondDatum),
        },
      ],
      scheduler: {
        operatorVkey: operator,
        shiftStartTime: "900",
        datumSha256: shaDatum(base.schedulerDatum),
      },
      activeOperators: [
        {
          operatorVkey: operator,
          nextOperatorVkey: null,
          bondUnlockTime: null,
          inactivityStrikes: "0",
          datumSha256: shaDatum(base.activeNodeDatum),
        },
      ],
      retiredOperators: [],
      quarantinedFromHeaderHash: null,
    })!;
    const bootstrapObservation = observationFor(
      bootstrapBlock,
      bootstrapStore,
      bootstrapSnapshot,
      "bootstrap",
      null,
      null,
      null,
    );
    const boot = evaluateWatcherStateQueueIndexerV1(
      policy,
      null,
      bootstrapObservation,
      contextFor(bootstrapBlock, bootstrapStore),
    ).state!;
    expect(boot).not.toBeNull();

    const rejectedProofBootstrap = (
      hostileOutputs: readonly OutputFixture[],
    ) => {
      const hostileBody = bodyFrom(
        [],
        [],
        hostileOutputs,
        [
          {
            policyId: policy.stateQueuePolicyId,
            assetName: policy.stateQueueRootAssetNameHex,
            quantity: 1n,
          },
        ],
        null,
        null,
      );
      const hostileBlock = l1Block(hostileBody, hostileOutputs, [
        {
          purpose: "mint",
          index: "0",
          bytesHex: Data.to(
            { InitV1: { output_index: 0n } },
            StateQueueRedeemer,
          ),
        },
      ]);
      const hostileStore = storeFor(
        hostileBlock,
        protocolRecords(hostileBlock, hostileOutputs),
        null,
      );
      return evaluateWatcherStateQueueIndexerV1(
        policy,
        null,
        observationFor(
          hostileBlock,
          hostileStore,
          bootstrapSnapshot,
          "bootstrap",
          null,
          null,
          null,
        ),
        contextFor(hostileBlock, hostileStore),
      );
    };
    expect(rejectedProofBootstrap([...outputs, outputs.at(-1)!])).toMatchObject(
      { action: "reject" },
    );
    const malformedProofDatum = canonicalDatum(
      Data.to("" as never, Data.Bytes()),
    );
    expect(
      rejectedProofBootstrap([
        ...outputs.slice(0, -1),
        {
          ...outputs.at(-1)!,
          datumHex: malformedProofDatum,
          outputHex: output(
            policy.fraudProofAddressHex,
            policy.fraudProofPolicyId,
            `${categoryIds[0]!}${first.headerHash}`,
            malformedProofDatum,
          ),
        },
      ]),
    ).toMatchObject({ action: "reject" });

    const continuedFirstDatum = linkedNode(
      {
        header: Data.from(first.headerCborHex, HeaderV1),
        da_attestation: "",
      },
      StateQueueNodeV1,
      null,
    );
    const removalOutputs: readonly OutputFixture[] = [
      {
        role: "state_queue",
        datumHex: continuedFirstDatum,
        outputHex: output(
          policy.stateQueueAddressHex,
          policy.stateQueuePolicyId,
          `${policy.stateQueueNodeAssetPrefixHex}${first.headerHash}`,
          continuedFirstDatum,
          4_000_000n,
        ),
      },
    ];
    const firstOutRef = `${bootstrapBlock.txHash}#1`;
    const secondOutRef = `${bootstrapBlock.txHash}#2`;
    const activeOutRef = `${bootstrapBlock.txHash}#4`;
    const retiredRootOutRef = `${bootstrapBlock.txHash}#5`;
    const proofOutRef = `${bootstrapBlock.txHash}#8`;
    const removalBody = bodyFrom(
      [firstOutRef, secondOutRef],
      [proofOutRef, activeOutRef, retiredRootOutRef],
      removalOutputs,
      [
        {
          policyId: policy.stateQueuePolicyId,
          assetName: `${policy.stateQueueNodeAssetPrefixHex}${second.headerHash}`,
          quantity: -1n,
        },
      ],
      null,
      null,
    );
    const decodedBody = CML.TransactionBody.from_cbor_hex(removalBody);
    const referenceOutRefs: string[] = [];
    const refs = decodedBody.reference_inputs()!;
    for (let index = 0; index < refs.len(); index += 1) {
      const input = refs.get(index);
      referenceOutRefs.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
    const removalBlock = l1Block(removalBody, removalOutputs, [
      {
        purpose: "spend",
        index: "0",
        bytesHex: Data.to("LinkedListMutation", StateQueueSpendRedeemer),
      },
      {
        purpose: "spend",
        index: "1",
        bytesHex: Data.to("LinkedListMutation", StateQueueSpendRedeemer),
      },
      {
        purpose: "mint",
        index: "0",
        bytesHex: Data.to(
          {
            RemoveFraudulentBlockHeader: {
              fraudulent_operator: second.operatorVkey,
              fraudulent_blocks_header_hash: first.headerHash,
              slashing_approach: {
                OperatorAlreadySlashed: {
                  active_operators_element_ref_input_index: BigInt(
                    referenceOutRefs.indexOf(activeOutRef),
                  ),
                  retired_operators_element_ref_input_index: BigInt(
                    referenceOutRefs.indexOf(retiredRootOutRef),
                  ),
                },
              },
              fraud_proof_ref_input_index: BigInt(
                referenceOutRefs.indexOf(proofOutRef),
              ),
              block_removal_approach: {
                RemoveFraudulentBlocksLink: {
                  fraudulent_node_input_outref: {
                    transactionId: bootstrapBlock.txHash,
                    outputIndex: 1n,
                  },
                  fraudulent_node_output_index: 0n,
                },
              },
            },
          },
          StateQueueRedeemer,
        ),
      },
    ]);
    const removalStore = storeFor(
      removalBlock,
      [
        ...bootstrapStore.protocolUtxos.filter(
          ({ outRef }) => outRef !== firstOutRef && outRef !== secondOutRef,
        ),
        ...protocolRecords(removalBlock, removalOutputs),
      ],
      bootstrapStore,
    );
    const removalSnapshot = makeWatcherStateQueueSnapshotV1({
      confirmedState: bootstrapSnapshot.confirmedState,
      queue: [
        {
          ...first,
          nextHeaderHash: null,
          datumSha256: shaDatum(continuedFirstDatum),
        },
      ],
      scheduler: bootstrapSnapshot.scheduler,
      activeOperators: bootstrapSnapshot.activeOperators,
      retiredOperators: bootstrapSnapshot.retiredOperators,
      quarantinedFromHeaderHash: null,
    })!;
    const removalObservation = observationFor(
      removalBlock,
      removalStore,
      removalSnapshot,
      "remove_fraudulent",
      boot.stateDigest,
      null,
      null,
    );
    const removed = evaluateWatcherStateQueueIndexerV1(
      policy,
      boot,
      removalObservation,
      contextFor(removalBlock, removalStore),
    );
    expect(removed.action, JSON.stringify(removed)).toBe("accept");
    expect(removed.reasonCodes).toEqual(["removal_authenticated"]);
    expect(parseWatcherStateQueueIndexerStateV1(removed.state, policy)).toEqual(
      removed.state,
    );
  });

  it("rejects a fully self-rehashed history mutation because W03/W10 context is replayed", () => {
    const bundle = bootstrapBundle();
    const result = evaluateWatcherStateQueueIndexerV1(
      policy,
      null,
      bundle.observation,
      bundle.context,
    );
    const forged = structuredClone(result.state) as unknown as {
      history: Array<Record<string, any>>;
      stateDigest: string;
    };
    const entry = forged.history[0]!;
    entry.observation.blockNo = "999";
    const { observationDigest: _oldObservationDigest, ...observationFields } =
      entry.observation;
    entry.observation.observationDigest = canonicalDigest(observationFields);
    const { entryDigest: _oldEntryDigest, ...entryFields } = entry;
    entry.entryDigest = canonicalDigest(entryFields);
    const { stateDigest: _oldStateDigest, ...stateFields } = forged;
    forged.stateDigest = canonicalDigest(stateFields);
    expect(parseWatcherStateQueueIndexerStateV1(forged, policy)).toBeNull();
  });
});

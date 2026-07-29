import { createHash, generateKeyPairSync, sign } from "node:crypto";

import {
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  Proof,
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
  type WatcherFinalityPolicyV1,
  type WatcherFinalityStateV1,
} from "../src/finality-engine.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherAuthenticatedL1ProviderV1,
  type WatcherL1RedeemerV1,
  type WatcherNormalizedL1BlockV1,
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "../src/multi-provider-consistency.js";
import {
  evaluateWatcherProofThreadIndexerV1,
  makeWatcherProofThreadJournalV1,
  makeWatcherProofThreadLayoutV1,
  makeWatcherProofThreadObservationV1,
  makeWatcherProofThreadPolicyV1,
  parseWatcherProofThreadResultV1,
  parseWatcherProofThreadStateV1,
  WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  type WatcherProofThreadFamilyV1,
  type WatcherProofThreadJournalV1,
  type WatcherProofThreadObservationV1,
  type WatcherProofThreadPolicyV1,
  type WatcherProofThreadPublicContextV1,
  type WatcherProofThreadStateV1,
} from "../src/proof-thread-indexer.js";
import {
  evaluateWatcherRollbackV1,
  makeWatcherRollbackBootstrapStateV1,
} from "../src/rollback-engine.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const RELEASE_DIGEST = h32("66");
const BLUEPRINT_HASH = h32("55");
const RULE_BUNDLE_COMMITMENT = h32("77");
const CT_POLICY = h28("c1");
const PROVER = h28("ab");
const FRAUD_HEADER = h28("cd");
const FRAUD_BLOCK = h32("ef");
const FAULT_ID = h32("f1");
const SUBMISSION_ID = h32("f2");
const CONFIRMATION_ID = h32("f3");
const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

type Mutable = Record<string, any>;

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

const digest = (value: unknown): string =>
  createHash("sha256").update(canonicalJson(value), "utf8").digest("hex");

const same = (left: unknown, right: unknown): boolean =>
  canonicalJson(left) === canonicalJson(right);

const scriptAddress = (hash: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.from(hash, "hex")]),
  ).to_hex();

const publicBytes = (hex: string) => makeWatcherL1PublicBytesV1(hex);
const canonicalData = (hex: string): string =>
  CML.PlutusData.from_cbor_hex(hex).to_canonical_cbor_hex();

const familyNames = [
  ["double-spend", "doubleSpend"],
  ["invalid-range", "invalidRange"],
  ["non-existent-input", "nonExistentInput"],
  ["non-existent-input-no-index", "nonExistentInputNoIndex"],
  ["transition-trace", "transitionTrace"],
  ["validation-trace-dispute", "validationTraceDispute"],
  ["zero-input", "zeroInput"],
] as const;

const makeDeploymentAuthority = () => {
  const referenceOutRefs = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contract, index) => [
        contract,
        { txHash: h32("12"), outputIndex: index },
      ],
    ),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((name, index) => {
      const native = name === "referenceScriptAuthMint";
      const script = native
        ? NATIVE_SCRIPT_CBOR
        : (index + 1).toString(16).padStart(2, "0");
      return [
        name,
        {
          refScriptUTxO: referenceOutRefs.get(name) ?? null,
          contract: { type: native ? "Native" : "PlutusV3", cborHex: script },
          scriptHash: native
            ? NATIVE_SCRIPT_HASH
            : validatorToScriptHash({ type: "PlutusV3", script }),
        },
      ];
    }),
  ) as Mutable;
  const catalogueNames = [
    "doubleSpend",
    "nonExistentInput",
    "nonExistentInputNoIndex",
    "invalidRange",
    "transitionTrace",
    "zeroInput",
    "validationTraceDispute",
  ] as const;
  const contractByCategory = {
    doubleSpend: "fraudProofDoubleSpend",
    nonExistentInput: "fraudProofNonExistentInput",
    nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
    invalidRange: "fraudProofInvalidRange",
    transitionTrace: "fraudProofTransitionTrace",
    zeroInput: "fraudProofZeroInput",
    validationTraceDispute: "validationTraceDispute",
  } as const;
  contracts.fraudProofCatalogueMint.fraudProofCatalogue = {
    root: h32("13"),
    categories: Object.fromEntries(
      catalogueNames.map((name, index) => [
        name,
        {
          categoryId: index.toString(16).padStart(8, "0"),
          scriptHash: contracts[contractByCategory[name]].scriptHash,
          membershipProofCbor: "80",
        },
      ]),
    ),
  };
  const referenceScripts = Object.fromEntries(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contract]) => {
      const outRef = referenceOutRefs.get(contract)!;
      const token =
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
          role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
        ];
      return [
        role,
        {
          status: "confirmed",
          roleUnit:
            NATIVE_SCRIPT_HASH + Buffer.from(token, "utf8").toString("hex"),
          scriptHash: contracts[contract].scriptHash,
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
      DEPLOYMENT_MANIFEST_V1_STEP_NAMES.map((name) => [
        name,
        {
          status:
            name === "prepareHubOracleNonce" ||
            name === "deployNodeRuntimeReferenceScripts" ||
            name === "initProtocol"
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
  const families: readonly WatcherProofThreadFamilyV1[] = Object.freeze(
    familyNames.map(([familyId, catalogueCategory]) => {
      const category =
        contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories[
          catalogueCategory
        ];
      return Object.freeze({
        familyId,
        catalogueCategory,
        categoryId: category.categoryId,
        firstStepScriptHash: category.scriptHash,
        stepScriptHashes:
          familyId === "double-spend"
            ? Object.freeze([category.scriptHash, h28("d2")])
            : Object.freeze([category.scriptHash]),
      });
    }),
  );
  const programCommitments = {
    "computation-thread-policy-v1": digest({
      computationThreadPolicyId: CT_POLICY,
    }),
    "proof-thread-catalogue-v1": digest(families),
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
    deploymentAuthority: {
      signedIdentity,
      policy: deploymentPolicy,
      trustRoots,
      result,
    },
    marker,
    result,
    families,
    contracts,
  };
};

const authority = makeDeploymentAuthority();
const applied = authority.deploymentAuthority.policy.appliedScriptHashes;
const policy = makeWatcherProofThreadPolicyV1({
  network: "Preprod",
  releaseEvidenceDigest: RELEASE_DIGEST,
  deploymentMarker: authority.marker,
  deploymentTrustRootId: authority.result.trustRootId,
  requiredFinalityDepth: "2",
  computationThreadPolicyId: CT_POLICY,
  fraudProofPolicyId: applied.fraudProofMint!,
  fraudProofSpendScriptHash: applied.fraudProofSpend!,
  fraudProofAddressHex: scriptAddress(applied.fraudProofSpend!),
  families: authority.families,
  maximumHistoryEntries: "32",
}) as WatcherProofThreadPolicyV1;

const makeFinalityPolicy = (source: unknown): WatcherFinalityPolicyV1 =>
  makeWatcherFinalityPolicyV1(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        source,
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
        finality: {
          depth: 2,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: 2,
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
    authority.result,
  ) as WatcherFinalityPolicyV1;

const externalSource = {
  sourceMode: "external_providers",
  providers: [
    {
      identity: "provider-a",
      operatorIdentitySha256: h32("a1"),
      endpoint: "https://a.example",
    },
    {
      identity: "provider-b",
      operatorIdentitySha256: h32("b2"),
      endpoint: "https://b.example",
    },
  ],
} as const;

const localSource = {
  sourceMode: "local_node",
  authorityNodeId: "watcher-node-a",
  chainSync: {
    kind: "cardano_node_socket",
    socketPath: "/var/lib/cardano/node.socket",
    genesisIdentitySha256: h32("c3"),
  },
  queryServices: [
    {
      kind: "ogmios",
      identity: "watcher-node-a-ogmios",
      endpoint: "http://127.0.0.1:1337",
    },
  ],
} as const;

const finalityPolicy = makeFinalityPolicy(externalSource);
const localFinalityPolicy = makeFinalityPolicy(localSource);

const providers = [
  {
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "provider-a",
    source: {
      sourceMode: "external_providers",
      operatorIdentitySha256: h32("a1"),
    },
    authentication: {
      kind: "https_tls_identity_v1",
      publicIdentitySha256: h32("a1"),
    },
  },
  {
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "provider-b",
    source: {
      sourceMode: "external_providers",
      operatorIdentitySha256: h32("b2"),
    },
    authentication: {
      kind: "https_tls_identity_v1",
      publicIdentitySha256: h32("b2"),
    },
  },
] as const;

const localProviders = [
  {
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "watcher-node-a",
    source: {
      sourceMode: "local_node",
      authorityNodeId: "watcher-node-a",
      surface: "chain_sync",
    },
    authentication: {
      kind: "cardano_node_genesis_v1",
      publicIdentitySha256: h32("c3"),
    },
  },
  {
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "watcher-node-a-ogmios",
    source: {
      sourceMode: "local_node",
      authorityNodeId: "watcher-node-a",
      surface: "ogmios",
    },
    authentication: {
      kind: "https_tls_identity_v1",
      publicIdentitySha256: h32("d4"),
    },
  },
] as const;

type FixtureSourceMode = "local_node" | "external_providers";

const sourceFixture = (
  sourceMode: FixtureSourceMode,
): Readonly<{
  policy: WatcherFinalityPolicyV1;
  providers: readonly WatcherAuthenticatedL1ProviderV1[];
  consistencyConfig: unknown;
}> =>
  sourceMode === "local_node"
    ? {
        policy: localFinalityPolicy,
        providers: localProviders,
        consistencyConfig: {
          sourceMode: "local_node",
          network: "Preprod",
          authorityNodeId: localSource.authorityNodeId,
          genesisIdentitySha256: localSource.chainSync.genesisIdentitySha256,
        },
      }
    : {
        policy: finalityPolicy,
        providers,
        consistencyConfig: {
          sourceMode: "external_providers",
          network: "Preprod",
        },
      };

const input = (outRef: string): CML.TransactionInput => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};

const outputWithToken = (
  address: string,
  policyId: string,
  assetName: string,
  datumHex: string,
): CML.TransactionOutput => {
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(policyId),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  return CML.TransactionOutput.new(
    CML.Address.from_hex(address),
    CML.Value.new(3_000_000n, assets),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumHex)),
    null,
  );
};

const bareOutputHex = (scriptHash: string): string =>
  CML.TransactionOutput.new(
    CML.Address.from_hex(scriptAddress(scriptHash)),
    CML.Value.from_coin(2_000_000n),
    null,
    null,
  ).to_canonical_cbor_hex();

type TxFixture = Readonly<{
  bodyHex: string;
  txHash: string;
  outputs: readonly CML.TransactionOutput[];
  redeemers: readonly Readonly<{
    purpose: WatcherL1RedeemerV1["purpose"];
    index: string;
    bytesHex: string;
  }>[];
}>;

const initTransaction = (): TxFixture => {
  const family = policy.families.find(
    ({ familyId }) => familyId === "double-spend",
  )!;
  const assetName = `${family.categoryId}${FRAUD_HEADER}`;
  const datumHex = Data.to(
    { fraud_prover: PROVER, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const output = outputWithToken(
    scriptAddress(family.firstStepScriptHash),
    CT_POLICY,
    assetName,
    datumHex,
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(`${h32("20")}#0`));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const references = CML.TransactionInputList.new();
  references.add(input(`${h32("30")}#0`));
  references.add(input(`${h32("31")}#0`));
  references.add(input(`${h32("32")}#0`));
  body.set_reference_inputs(references);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(CT_POLICY),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  body.set_mint(mint);
  const bodyHex = body.to_canonical_cbor_hex();
  const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
  const mintRedeemer = CML.PlutusData.from_cbor_hex(
    Data.to(
      {
        Init: {
          first_step_output_index: 0n,
          fraud_category_id: family.categoryId,
          fraud_category: family.firstStepScriptHash,
          fraud_category_membership_proof: Data.from("80", Proof),
          fraud_proof_catalogue_ref_input_index: 0n,
          inclusion_proof_script_redeemer_index: 1n,
          hub_oracle_ref_input_index: 1n,
          fraudulent_block_ref_input_index: 2n,
        },
      },
      FraudProofComputationThreadRedeemer,
    ),
  ).to_canonical_cbor_hex();
  return {
    bodyHex,
    txHash,
    outputs: [output],
    redeemers: [
      { purpose: "mint", index: "0", bytesHex: mintRedeemer },
      { purpose: "withdrawal", index: "0", bytesHex: "d87980" },
    ],
  };
};

const policyIndex = (body: CML.TransactionBody, policyId: string): string => {
  const policies = body.mint()!.keys();
  for (let index = 0; index < policies.len(); index += 1) {
    if (policies.get(index).to_hex() === policyId) {
      return index.toString();
    }
  }
  throw new Error(`missing fixture mint policy ${policyId}`);
};

const stepTransaction = (source: WatcherProofThreadJournalV1): TxFixture => {
  const family = policy.families.find(
    ({ familyId }) => familyId === source.familyId,
  )!;
  const datumHex = canonicalData(
    Data.to(
      { fraud_prover: source.fraudProver, data: Data.from("01") },
      FraudProofComputationThreadStepDatum,
    ),
  );
  const output = outputWithToken(
    scriptAddress(family.stepScriptHashes[1]!),
    CT_POLICY,
    source.computationThreadAssetName,
    datumHex,
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(source.threadOutRef!));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const bodyHex = body.to_canonical_cbor_hex();
  return {
    bodyHex,
    txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
    outputs: [output],
    redeemers: [
      {
        purpose: "spend",
        index: "0",
        bytesHex: "d87a8101",
      },
    ],
  };
};

const successTransaction = (source: WatcherProofThreadJournalV1): TxFixture => {
  const proofOutput = outputWithToken(
    policy.fraudProofAddressHex,
    policy.fraudProofPolicyId,
    source.computationThreadAssetName,
    canonicalData(
      Data.to({ fraud_prover: source.fraudProver }, FraudProofTokenDatum),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(source.threadOutRef!));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(proofOutput);
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(policy.computationThreadPolicyId),
    CML.AssetName.from_hex(source.computationThreadAssetName),
    -1n,
  );
  mint.set(
    CML.ScriptHash.from_hex(policy.fraudProofPolicyId),
    CML.AssetName.from_hex(source.computationThreadAssetName),
    1n,
  );
  body.set_mint(mint);
  const bodyHex = body.to_canonical_cbor_hex();
  const canonicalBody = CML.TransactionBody.from_cbor_hex(bodyHex);
  const ctGlobalIndex = 2n;
  return {
    bodyHex,
    txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
    outputs: [proofOutput],
    redeemers: [
      { purpose: "spend", index: "0", bytesHex: "d87a8101" },
      {
        purpose: "mint",
        index: policyIndex(canonicalBody, policy.computationThreadPolicyId),
        bytesHex: canonicalData(
          Data.to(
            {
              Success: {
                burning_token_asset_name: source.computationThreadAssetName,
              },
            },
            FraudProofComputationThreadRedeemer,
          ),
        ),
      },
      {
        purpose: "mint",
        index: policyIndex(canonicalBody, policy.fraudProofPolicyId),
        bytesHex: canonicalData(
          Data.to(
            {
              computation_thread_token_asset_name:
                source.computationThreadAssetName,
              computation_thread_mint_redeemer_index: ctGlobalIndex,
            },
            FraudProofTokenMintRedeemer,
          ),
        ),
      },
    ],
  };
};

const cancelTransaction = (source: WatcherProofThreadJournalV1): TxFixture => {
  const change = CML.TransactionOutput.new(
    CML.Address.from_hex(scriptAddress(h28("73"))),
    CML.Value.from_coin(2_000_000n),
    null,
    null,
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(source.threadOutRef!));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(change);
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(policy.computationThreadPolicyId),
    CML.AssetName.from_hex(source.computationThreadAssetName),
    -1n,
  );
  body.set_mint(mint);
  const bodyHex = body.to_canonical_cbor_hex();
  return {
    bodyHex,
    txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
    outputs: [change],
    redeemers: [
      { purpose: "spend", index: "0", bytesHex: "d87981d879820001" },
      {
        purpose: "mint",
        index: policyIndex(body, policy.computationThreadPolicyId),
        bytesHex: canonicalData(
          Data.to(
            {
              BurnForCancellation: {
                burning_token_asset_name: source.computationThreadAssetName,
              },
            },
            FraudProofComputationThreadRedeemer,
          ),
        ),
      },
    ],
  };
};

const removalTransaction = (source: WatcherProofThreadJournalV1): TxFixture => {
  const change = CML.TransactionOutput.new(
    CML.Address.from_hex(scriptAddress(h28("74"))),
    CML.Value.from_coin(2_000_000n),
    null,
    null,
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(`${h32("75")}#0`));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(change);
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const references = CML.TransactionInputList.new();
  references.add(input(source.proofTokenOutRef!));
  body.set_reference_inputs(references);
  const bodyHex = body.to_canonical_cbor_hex();
  return {
    bodyHex,
    txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
    outputs: [change],
    redeemers: [],
  };
};

const rawTransaction = (fixture: TxFixture) => ({
  txHash: fixture.txHash,
  body: publicBytes(fixture.bodyHex),
  utxos: fixture.outputs.map((output, index) => {
    const datum = output.datum()?.as_datum();
    const datumHex = datum?.to_canonical_cbor_hex() ?? null;
    return {
      outRef: `${fixture.txHash}#${index.toString()}`,
      outputIndex: index.toString(),
      output: publicBytes(output.to_canonical_cbor_hex()),
      datum:
        datumHex === null
          ? null
          : {
              datumHash: computeHash32(Buffer.from(datumHex, "hex")).toString(
                "hex",
              ),
              bytes: publicBytes(datumHex),
            },
      referenceScript: null,
    };
  }),
  scripts: [],
  datums: [],
  redeemers: fixture.redeemers.map(({ purpose, index, bytesHex }) => ({
    purpose,
    index,
    bytes: publicBytes(bytesHex),
  })),
});

const rawObservation = (
  provider: WatcherAuthenticatedL1ProviderV1,
  fixture: TxFixture,
  depth: string,
  ordinal = 0,
) => ({
  schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: provider.providerId,
  chainPoint: {
    blockHash: h32((0x40 + ordinal).toString(16).padStart(2, "0")),
    slot: (1000 + ordinal).toString(),
    blockNo: (100 + ordinal).toString(),
    depth,
  },
  transactions: [rawTransaction(fixture)],
});

const normalize = (
  provider: WatcherAuthenticatedL1ProviderV1,
  fixture: TxFixture,
  depth: string,
  ordinal = 0,
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(
    provider,
    rawObservation(provider, fixture, depth, ordinal),
  );

const protocolUtxo = (
  outRef: string,
  role: WatcherProtocolUtxoV1["role"],
  chainPointId: string,
  outputHex: string,
): WatcherProtocolUtxoV1 => ({
  outRef,
  role,
  chainPointId,
  output: makeWatcherDurablePayloadV1(outputHex),
});

const emptyRecords = (): WatcherDurableRecordsV1 => ({
  l1Observations: [],
  chainPoints: [],
  protocolUtxos: [],
  spentProtocolUtxos: [],
  daProofInputs: [],
  reconstructedStates: [],
  decisions: [],
  faults: [],
  submissions: [],
  confirmations: [],
  retries: [],
  deadlines: [],
  correctionResults: [],
});

const baseStore = (fixture: TxFixture): WatcherDurableStoreV1 => {
  const records = emptyRecords();
  const priorPoint = {
    chainPointId: h32("50"),
    providerId: "provider-a",
    blockHash: FRAUD_BLOCK,
    slot: "900",
    blockNo: "90",
    depth: "20",
  };
  return makeWatcherDurableStoreV1({
    deploymentMarker: authority.marker,
    revision: "0",
    records: {
      ...records,
      chainPoints: [priorPoint],
      protocolUtxos: [
        protocolUtxo(
          `${h32("31")}#0`,
          "hub_oracle",
          priorPoint.chainPointId,
          bareOutputHex(h28("61")),
        ),
        protocolUtxo(
          `${h32("32")}#0`,
          "state_queue",
          priorPoint.chainPointId,
          bareOutputHex(h28("62")),
        ),
      ],
      daProofInputs: [
        {
          inputId: h32("52"),
          kind: "da_payload",
          payload: makeWatcherDurablePayloadV1("80"),
        },
      ],
      reconstructedStates: [
        {
          blockHash: FRAUD_BLOCK,
          priorStateRoot: h32("53"),
          postStateRoot: h32("54"),
          inputIds: [h32("52")],
          state: makeWatcherDurablePayloadV1("80"),
        },
      ],
      decisions: [
        {
          blockHash: FRAUD_BLOCK,
          decision: "fault_detected",
          reconstructionDigest: h32("55"),
          evidenceDigest: h32("56"),
        },
      ],
      faults: [
        {
          faultId: FAULT_ID,
          blockHash: FRAUD_BLOCK,
          familyId: "double-spend",
          evidence: makeWatcherDurablePayloadV1("80"),
        },
      ],
      submissions: [
        {
          submissionId: SUBMISSION_ID,
          faultId: FAULT_ID,
          txBodyHash: fixture.txHash,
          status: "submitted",
        },
      ],
    },
  });
};

const appendPublicStore = ({
  source,
  block,
  fixture,
  phase,
  applyEffects,
}: {
  source: WatcherDurableStoreV1;
  block: WatcherNormalizedL1BlockV1;
  fixture: TxFixture;
  phase: "pending" | "final";
  applyEffects: boolean;
}): WatcherDurableStoreV1 => {
  const encoded = encodeWatcherNormalizedL1BlockV1(block).toString("hex");
  const observation = {
    observationId: block.observationDigest,
    providerId: block.provider.providerId,
    chainPointId: block.chainPoint.chainPointId,
    payload: makeWatcherDurablePayloadV1(encoded),
  };
  const point = {
    chainPointId: block.chainPoint.chainPointId,
    providerId: block.provider.providerId,
    blockHash: block.chainPoint.blockHash,
    slot: block.chainPoint.slot,
    blockNo: block.chainPoint.blockNo,
    depth: block.chainPoint.depth,
  };
  const l1Observations = [
    ...source.l1Observations.filter(
      ({ observationId }) => observationId !== observation.observationId,
    ),
    observation,
  ];
  const chainPoints = [
    ...source.chainPoints.filter(
      ({ chainPointId }) => chainPointId !== point.chainPointId,
    ),
    point,
  ];
  const createdProtocolUtxos = fixture.outputs.map((output, index) =>
    protocolUtxo(
      `${fixture.txHash}#${index.toString()}`,
      "computation_thread",
      block.chainPoint.chainPointId,
      output.to_canonical_cbor_hex(),
    ),
  );
  const missingProtocolUtxos = createdProtocolUtxos.filter((created) => {
    const existing = source.protocolUtxos.find(
      ({ outRef }) => outRef === created.outRef,
    );
    return existing === undefined || !same(existing, created);
  });
  const transactionEffects = applyEffects
    ? journalWatcherProtocolUtxoTransitionV1({
        sourceStore: source,
        nextChainPoints: chainPoints,
        nextProtocolUtxos: [...source.protocolUtxos, ...missingProtocolUtxos],
        spentAtChainPointId: block.chainPoint.chainPointId,
      })
    : {
        protocolUtxos: source.protocolUtxos,
        spentProtocolUtxos: source.spentProtocolUtxos,
      };
  const confirmation = {
    confirmationId: CONFIRMATION_ID,
    submissionId: SUBMISSION_ID,
    txHash: fixture.txHash,
    chainPointId: block.chainPoint.chainPointId,
    depth: block.chainPoint.depth,
    status:
      phase === "pending" ? ("observed" as const) : ("confirmed" as const),
  };
  return makeWatcherDurableStoreV1({
    deploymentMarker: authority.marker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      ...source,
      l1Observations,
      chainPoints,
      protocolUtxos: transactionEffects.protocolUtxos,
      spentProtocolUtxos: transactionEffects.spentProtocolUtxos,
      confirmations: [
        ...source.confirmations.filter(
          ({ confirmationId }) =>
            confirmationId !== confirmation.confirmationId,
        ),
        confirmation,
      ],
    },
  });
};

const journalIdentity = digest({
  domain: "midgard-watcher-proof-thread-journal-identity-v1",
  manifestId: authority.marker.manifestId,
  faultId: FAULT_ID,
});

const initJournal = (fixture: TxFixture): WatcherProofThreadJournalV1 =>
  makeWatcherProofThreadJournalV1({
    journalId: journalIdentity,
    faultId: FAULT_ID,
    familyId: "double-spend",
    fraudulentBlockHash: FRAUD_BLOCK,
    fraudulentHeaderHash: FRAUD_HEADER,
    fraudProver: PROVER,
    computationThreadAssetName: `${
      policy.families.find(({ familyId }) => familyId === "double-spend")!
        .categoryId
    }${FRAUD_HEADER}`,
    phase: "active",
    stepIndex: "0",
    threadOutRef: `${fixture.txHash}#0`,
    proofTokenOutRef: null,
    lastSubmissionId: SUBMISSION_ID,
    lastConfirmationId: CONFIRMATION_ID,
    correctionId: null,
    confirmedTransactionHashes: [fixture.txHash],
  })!;

type InitStage = Readonly<{
  fixture: TxFixture;
  block: WatcherNormalizedL1BlockV1;
  sourceStore: WatcherDurableStoreV1;
  store: WatcherDurableStoreV1;
  observation: WatcherProofThreadObservationV1;
  context: WatcherProofThreadPublicContextV1;
  finalityState: WatcherFinalityStateV1;
  journal: WatcherProofThreadJournalV1;
}>;

const initStage = ({
  phase,
  previousState,
  previousFinalityState,
  sourceStore: suppliedSource,
  sourceMode = "external_providers",
}: {
  phase: "pending" | "final";
  previousState: WatcherProofThreadStateV1 | null;
  previousFinalityState: WatcherFinalityStateV1 | null;
  sourceStore?: WatcherDurableStoreV1;
  sourceMode?: FixtureSourceMode;
}): InitStage => {
  const fixture = initTransaction();
  const depth = phase === "pending" ? "1" : "2";
  const l1 = sourceFixture(sourceMode);
  const normalized = l1.providers.map((provider) =>
    normalize(provider, fixture, depth),
  );
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    normalized,
  );
  const finalityResult = evaluateWatcherFinalityV1(
    l1.policy,
    previousFinalityState,
    consistency,
  );
  const sourceStore = suppliedSource ?? baseStore(fixture);
  const store = appendPublicStore({
    source: sourceStore,
    block: normalized[0]!,
    fixture,
    phase,
    applyEffects: previousState?.pending === null || previousState === null,
  });
  const journal = initJournal(fixture);
  const observation = makeWatcherProofThreadObservationV1({
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: "init",
    confirmationPhase: phase,
    pointDigest: normalized[0]!.chainPoint.pointDigest,
    blockHash: normalized[0]!.chainPoint.blockHash,
    slot: normalized[0]!.chainPoint.slot,
    blockNo: normalized[0]!.chainPoint.blockNo,
    transactionHash: fixture.txHash,
    publicInputDigest: createHash("sha256")
      .update(encodeWatcherNormalizedL1BlockV1(normalized[0]!))
      .digest("hex"),
    sourceObservationDigest: normalized[0]!.observationDigest,
    chainPointId: normalized[0]!.chainPoint.chainPointId,
    sourceDurableStoreDigest: createHash("sha256")
      .update(encodeWatcherDurableStoreV1(sourceStore))
      .digest("hex"),
    sourceDurableStoreRevision: sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
    ),
    durableStoreRevision: store.revision,
    predecessorStateDigest: previousState?.stateDigest ?? null,
    submissionId: SUBMISSION_ID,
    confirmationId: CONFIRMATION_ID,
    rollbackTargetStateDigest: null,
    layout: makeWatcherProofThreadLayoutV1({
      threadInputIndex: null,
      threadOutputIndex: "0",
      proofTokenOutputIndex: null,
      proofTokenReferenceInputIndex: null,
      stepSpendRedeemerGlobalIndex: null,
      computationThreadMintRedeemerGlobalIndex: "0",
      fraudProofMintRedeemerGlobalIndex: null,
    })!,
    journal,
  })!;
  const context: WatcherProofThreadPublicContextV1 = {
    schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: l1.providers[0],
    l1Observation: rawObservation(l1.providers[0]!, fixture, depth),
    sourceDurableStore: sourceStore,
    durableStore: store,
    deploymentAuthority: authority.deploymentAuthority,
    finalityAuthority: {
      policy: l1.policy,
      previousState: previousFinalityState,
      observations: l1.providers.map((provider) => ({
        authenticatedProvider: provider,
        l1Observation: rawObservation(provider, fixture, depth),
      })),
      consistency,
      result: finalityResult,
    },
    rollbackAuthority: null,
    sourceJournal: null,
    durableJournal: journal,
  };
  return {
    fixture,
    block: normalized[0]!,
    sourceStore,
    store,
    observation,
    context,
    finalityState: finalityResult.state!,
    journal,
  };
};

type OrdinaryTransition = "step" | "success" | "cancel" | "removal";

const transitionIds = (ordinal: number) => ({
  submissionId: h32((0x80 + ordinal).toString(16).padStart(2, "0")),
  confirmationId: h32((0x90 + ordinal).toString(16).padStart(2, "0")),
  correctionId: h32((0xa0 + ordinal).toString(16).padStart(2, "0")),
});

const addSubmittedTransaction = (
  source: WatcherDurableStoreV1,
  fixture: TxFixture,
  submissionId: string,
): WatcherDurableStoreV1 =>
  makeWatcherDurableStoreV1({
    deploymentMarker: authority.marker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      ...source,
      submissions: [
        ...source.submissions,
        {
          submissionId,
          faultId: FAULT_ID,
          txBodyHash: fixture.txHash,
          status: "submitted",
        },
      ],
    },
  });

const fixtureInputs = (fixture: TxFixture): readonly string[] => {
  const body = CML.TransactionBody.from_cbor_hex(fixture.bodyHex);
  const inputs: string[] = [];
  for (let index = 0; index < body.inputs().len(); index += 1) {
    const value = body.inputs().get(index);
    inputs.push(
      `${value.transaction_id().to_hex()}#${value.index().toString()}`,
    );
  }
  return inputs;
};

const appendTransitionStore = ({
  source,
  block,
  fixture,
  transitionKind,
  phase,
  submissionId,
  confirmationId,
  correctionId,
  applyEffects,
}: {
  source: WatcherDurableStoreV1;
  block: WatcherNormalizedL1BlockV1;
  fixture: TxFixture;
  transitionKind: OrdinaryTransition;
  phase: "pending" | "final";
  submissionId: string;
  confirmationId: string;
  correctionId: string;
  applyEffects: boolean;
}): WatcherDurableStoreV1 => {
  const encoded = encodeWatcherNormalizedL1BlockV1(block).toString("hex");
  const observation = {
    observationId: block.observationDigest,
    providerId: block.provider.providerId,
    chainPointId: block.chainPoint.chainPointId,
    payload: makeWatcherDurablePayloadV1(encoded),
  };
  const point = {
    chainPointId: block.chainPoint.chainPointId,
    providerId: block.provider.providerId,
    blockHash: block.chainPoint.blockHash,
    slot: block.chainPoint.slot,
    blockNo: block.chainPoint.blockNo,
    depth: block.chainPoint.depth,
  };
  const l1Observations = [
    ...source.l1Observations.filter(
      ({ observationId }) => observationId !== observation.observationId,
    ),
    observation,
  ];
  const chainPoints = [
    ...source.chainPoints.filter(
      ({ chainPointId }) => chainPointId !== point.chainPointId,
    ),
    point,
  ];
  const consumed = new Set(fixtureInputs(fixture));
  const created =
    transitionKind === "step" || transitionKind === "success"
      ? [
          protocolUtxo(
            `${fixture.txHash}#0`,
            transitionKind === "step" ? "computation_thread" : "proof_thread",
            block.chainPoint.chainPointId,
            fixture.outputs[0]!.to_canonical_cbor_hex(),
          ),
        ]
      : [];
  const nextProtocol = applyEffects
    ? [
        ...source.protocolUtxos.filter(({ outRef }) => !consumed.has(outRef)),
        ...created,
      ]
    : source.protocolUtxos;
  const transactionEffects = applyEffects
    ? journalWatcherProtocolUtxoTransitionV1({
        sourceStore: source,
        nextChainPoints: chainPoints,
        nextProtocolUtxos: nextProtocol,
        spentAtChainPointId: block.chainPoint.chainPointId,
      })
    : {
        protocolUtxos: source.protocolUtxos,
        spentProtocolUtxos: source.spentProtocolUtxos,
      };
  const confirmation = {
    confirmationId,
    submissionId,
    txHash: fixture.txHash,
    chainPointId: block.chainPoint.chainPointId,
    depth: block.chainPoint.depth,
    status:
      phase === "pending" ? ("observed" as const) : ("confirmed" as const),
  };
  const correctionResults =
    transitionKind === "removal" && phase === "final"
      ? [
          ...source.correctionResults,
          {
            correctionId,
            faultId: FAULT_ID,
            confirmationId,
            outcome: "removed" as const,
            finalStateRoot: h32("b0"),
            slashLovelace: "0",
            rewardLovelace: "0",
          },
        ]
      : source.correctionResults;
  return makeWatcherDurableStoreV1({
    deploymentMarker: authority.marker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      ...source,
      l1Observations,
      chainPoints,
      protocolUtxos: transactionEffects.protocolUtxos,
      spentProtocolUtxos: transactionEffects.spentProtocolUtxos,
      confirmations: [
        ...source.confirmations.filter(
          ({ confirmationId: priorId }) => priorId !== confirmationId,
        ),
        confirmation,
      ],
      correctionResults,
    },
  });
};

const transitionJournal = ({
  source,
  fixture,
  transitionKind,
  submissionId,
  confirmationId,
  correctionId,
}: {
  source: WatcherProofThreadJournalV1;
  fixture: TxFixture;
  transitionKind: OrdinaryTransition;
  submissionId: string;
  confirmationId: string;
  correctionId: string;
}): WatcherProofThreadJournalV1 =>
  makeWatcherProofThreadJournalV1({
    journalId: source.journalId,
    faultId: source.faultId,
    familyId: source.familyId,
    fraudulentBlockHash: source.fraudulentBlockHash,
    fraudulentHeaderHash: source.fraudulentHeaderHash,
    fraudProver: source.fraudProver,
    computationThreadAssetName: source.computationThreadAssetName,
    phase:
      transitionKind === "step"
        ? "active"
        : transitionKind === "success"
          ? "proven"
          : transitionKind === "cancel"
            ? "cancelled"
            : "removed",
    stepIndex: transitionKind === "step" ? "1" : null,
    threadOutRef: transitionKind === "step" ? `${fixture.txHash}#0` : null,
    proofTokenOutRef:
      transitionKind === "success"
        ? `${fixture.txHash}#0`
        : transitionKind === "removal"
          ? source.proofTokenOutRef
          : null,
    lastSubmissionId: submissionId,
    lastConfirmationId: confirmationId,
    correctionId: transitionKind === "removal" ? correctionId : null,
    confirmedTransactionHashes: [
      ...source.confirmedTransactionHashes,
      fixture.txHash,
    ],
  })!;

const transitionLayout = (transitionKind: OrdinaryTransition) =>
  makeWatcherProofThreadLayoutV1({
    threadInputIndex:
      transitionKind === "step" ||
      transitionKind === "success" ||
      transitionKind === "cancel"
        ? "0"
        : null,
    threadOutputIndex: transitionKind === "step" ? "0" : null,
    proofTokenOutputIndex: transitionKind === "success" ? "0" : null,
    proofTokenReferenceInputIndex: transitionKind === "removal" ? "0" : null,
    stepSpendRedeemerGlobalIndex:
      transitionKind === "step" ||
      transitionKind === "success" ||
      transitionKind === "cancel"
        ? "0"
        : null,
    computationThreadMintRedeemerGlobalIndex:
      transitionKind === "success"
        ? "2"
        : transitionKind === "cancel"
          ? "1"
          : null,
    fraudProofMintRedeemerGlobalIndex:
      transitionKind === "success" ? "1" : null,
  })!;

type TransitionStage = Readonly<{
  fixture: TxFixture;
  block: WatcherNormalizedL1BlockV1;
  sourceStore: WatcherDurableStoreV1;
  store: WatcherDurableStoreV1;
  observation: WatcherProofThreadObservationV1;
  context: WatcherProofThreadPublicContextV1;
  finalityState: WatcherFinalityStateV1;
  journal: WatcherProofThreadJournalV1;
}>;

const transitionStage = ({
  transitionKind,
  fixture,
  phase,
  previousState,
  previousFinalityState,
  sourceStore,
  sourceJournal,
  ordinal,
  sourceMode = "external_providers",
}: {
  transitionKind: OrdinaryTransition;
  fixture: TxFixture;
  phase: "pending" | "final";
  previousState: WatcherProofThreadStateV1;
  previousFinalityState: WatcherFinalityStateV1 | null;
  sourceStore: WatcherDurableStoreV1;
  sourceJournal: WatcherProofThreadJournalV1;
  ordinal: number;
  sourceMode?: FixtureSourceMode;
}): TransitionStage => {
  const ids = transitionIds(ordinal);
  const depth = phase === "pending" ? "1" : "2";
  const l1 = sourceFixture(sourceMode);
  const normalized = l1.providers.map((provider) =>
    normalize(provider, fixture, depth, ordinal),
  );
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    normalized,
  );
  const finalityResult = evaluateWatcherFinalityV1(
    l1.policy,
    previousFinalityState,
    consistency,
  );
  const store = appendTransitionStore({
    source: sourceStore,
    block: normalized[0]!,
    fixture,
    transitionKind,
    phase,
    ...ids,
    applyEffects: phase === "pending",
  });
  const journal = transitionJournal({
    source: sourceJournal,
    fixture,
    transitionKind,
    ...ids,
  });
  const observation = makeWatcherProofThreadObservationV1({
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind,
    confirmationPhase: phase,
    pointDigest: normalized[0]!.chainPoint.pointDigest,
    blockHash: normalized[0]!.chainPoint.blockHash,
    slot: normalized[0]!.chainPoint.slot,
    blockNo: normalized[0]!.chainPoint.blockNo,
    transactionHash: fixture.txHash,
    publicInputDigest: createHash("sha256")
      .update(encodeWatcherNormalizedL1BlockV1(normalized[0]!))
      .digest("hex"),
    sourceObservationDigest: normalized[0]!.observationDigest,
    chainPointId: normalized[0]!.chainPoint.chainPointId,
    sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(sourceStore),
    ),
    sourceDurableStoreRevision: sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
    ),
    durableStoreRevision: store.revision,
    predecessorStateDigest: previousState.stateDigest,
    submissionId: ids.submissionId,
    confirmationId: ids.confirmationId,
    rollbackTargetStateDigest: null,
    layout: transitionLayout(transitionKind),
    journal,
  })!;
  const context: WatcherProofThreadPublicContextV1 = {
    schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: l1.providers[0],
    l1Observation: rawObservation(l1.providers[0]!, fixture, depth, ordinal),
    sourceDurableStore: sourceStore,
    durableStore: store,
    deploymentAuthority: authority.deploymentAuthority,
    finalityAuthority: {
      policy: l1.policy,
      previousState: previousFinalityState,
      observations: l1.providers.map((provider) => ({
        authenticatedProvider: provider,
        l1Observation: rawObservation(provider, fixture, depth, ordinal),
      })),
      consistency,
      result: finalityResult,
    },
    rollbackAuthority: null,
    sourceJournal,
    durableJournal: journal,
  };
  return {
    fixture,
    block: normalized[0]!,
    sourceStore,
    store,
    observation,
    context,
    finalityState: finalityResult.state!,
    journal,
  };
};

const runFinalTransition = ({
  transitionKind,
  fixture,
  previousState,
  previousFinalityState,
  sourceStore,
  sourceJournal,
  ordinal,
}: {
  transitionKind: OrdinaryTransition;
  fixture: TxFixture;
  previousState: WatcherProofThreadStateV1;
  previousFinalityState: WatcherFinalityStateV1 | null;
  sourceStore: WatcherDurableStoreV1;
  sourceJournal: WatcherProofThreadJournalV1;
  ordinal: number;
}): Readonly<{
  pending: TransitionStage;
  final: TransitionStage;
  state: WatcherProofThreadStateV1;
}> => {
  const pending = transitionStage({
    transitionKind,
    fixture,
    phase: "pending",
    previousState,
    previousFinalityState,
    sourceStore,
    sourceJournal,
    ordinal,
  });
  const pendingResult = evaluateWatcherProofThreadIndexerV1(
    policy,
    previousState,
    pending.observation,
    pending.context,
  );
  expect(pending.context.finalityAuthority?.result).toMatchObject({
    protocolDecision: "hold",
    state: { phase: "pending" },
  });
  expect(pendingResult).toMatchObject({
    action: "accept",
    protocolDecision: "hold",
    reasonCodes: [`${transitionKind}_pending`],
  });
  const final = transitionStage({
    transitionKind,
    fixture,
    phase: "final",
    previousState: pendingResult.state!,
    previousFinalityState: pending.finalityState,
    sourceStore: pending.store,
    sourceJournal,
    ordinal,
  });
  const finalResult = evaluateWatcherProofThreadIndexerV1(
    policy,
    pendingResult.state,
    final.observation,
    final.context,
  );
  expect(finalResult).toMatchObject({
    action: "accept",
    protocolDecision: "indexed",
    reasonCodes: [`${transitionKind}_confirmed`],
  });
  return {
    pending,
    final,
    state: finalResult.state!,
  };
};

const persistNormalizedObservations = (
  source: WatcherDurableStoreV1,
  observations: readonly WatcherNormalizedL1BlockV1[],
): WatcherDurableStoreV1 =>
  makeWatcherDurableStoreV1({
    deploymentMarker: authority.marker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      ...source,
      l1Observations: [
        ...source.l1Observations,
        ...observations.map((block) => {
          const bytesHex =
            encodeWatcherNormalizedL1BlockV1(block).toString("hex");
          return {
            observationId: block.observationDigest,
            providerId: block.provider.providerId,
            chainPointId: block.chainPoint.chainPointId,
            payload: makeWatcherDurablePayloadV1(bytesHex),
          };
        }),
      ],
      chainPoints: [
        ...source.chainPoints,
        ...observations.map((block) => ({
          chainPointId: block.chainPoint.chainPointId,
          providerId: block.provider.providerId,
          blockHash: block.chainPoint.blockHash,
          slot: block.chainPoint.slot,
          blockNo: block.chainPoint.blockNo,
          depth: block.chainPoint.depth,
        })),
      ],
    },
  });

describe("W17 public proof/computation-thread indexer", () => {
  it("binds the proof lifecycle catalogue and computation policy to signed W02 commitments", () => {
    expect(authority.families.map(({ familyId }) => familyId)).toEqual(
      [...authority.families.map(({ familyId }) => familyId)].sort(),
    );
    const proofAddress = CML.Address.from_hex(
      scriptAddress(applied.fraudProofSpend!),
    );
    expect(proofAddress.network_id()).toBe(0);
    expect(proofAddress.payment_cred()?.as_script()?.to_hex()).toBe(
      applied.fraudProofSpend,
    );
    expect(
      makeWatcherProofThreadPolicyV1({
        network: "Preprod",
        releaseEvidenceDigest: RELEASE_DIGEST,
        deploymentMarker: authority.marker,
        deploymentTrustRootId: authority.result.trustRootId,
        requiredFinalityDepth: "2",
        computationThreadPolicyId: CT_POLICY,
        fraudProofPolicyId: applied.fraudProofMint!,
        fraudProofSpendScriptHash: applied.fraudProofSpend!,
        fraudProofAddressHex: scriptAddress(applied.fraudProofSpend!),
        families: [authority.families[0]!],
        maximumHistoryEntries: "32",
      }),
    ).not.toBeNull();
    expect(policy).not.toBeNull();
    expect(policy.families).toHaveLength(7);
    expect(policy.families[0]!.familyId).toBe("double-spend");

    const hostile = structuredClone(authority.deploymentAuthority);
    (hostile.policy.programCommitments as Mutable)[
      "proof-thread-catalogue-v1"
    ] = h32("00");
    const stage = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
    });
    const result = evaluateWatcherProofThreadIndexerV1(
      policy,
      null,
      stage.observation,
      { ...stage.context, deploymentAuthority: hostile },
    );
    expect(result).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("holds first visibility, then promotes the exact W12-final transaction and replays after restart", () => {
    const pending = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
    });
    const pendingResult = evaluateWatcherProofThreadIndexerV1(
      policy,
      null,
      pending.observation,
      pending.context,
    );
    expect(pending.context.finalityAuthority?.consistency).toMatchObject({
      status: "agreed",
      sourceMode: "external_providers",
      independentProviderCount: 2,
      reasonCodes: ["providers_consistent"],
    });
    expect(pendingResult).toMatchObject({
      action: "accept",
      protocolDecision: "hold",
      reasonCodes: ["init_pending"],
    });
    expect(pendingResult.state?.journal).toBeNull();
    expect(pendingResult.state?.pending?.journal).toEqual(pending.journal);

    const final = initStage({
      phase: "final",
      previousState: pendingResult.state!,
      previousFinalityState: pending.finalityState,
      sourceStore: pending.store,
    });
    const finalResult = evaluateWatcherProofThreadIndexerV1(
      policy,
      pendingResult.state,
      final.observation,
      final.context,
    );
    expect(finalResult).toMatchObject({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: ["init_confirmed"],
    });
    expect(finalResult.state?.journal).toEqual(final.journal);
    expect(finalResult.state?.pending).toBeNull();
    expect(parseWatcherProofThreadStateV1(finalResult.state, policy)).toEqual(
      finalResult.state,
    );
    expect(
      parseWatcherProofThreadResultV1(finalResult, {
        policy,
        previousState: pendingResult.state,
        observation: final.observation,
        publicContext: final.context,
      }),
    ).toEqual(finalResult);
  });

  it("indexes through one local chain-sync authority with an aligned query surface and no provider quorum", () => {
    const pending = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
      sourceMode: "local_node",
    });
    expect(pending.context.finalityAuthority?.consistency).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      sourceMode: "local_node",
      authorityNodeId: localSource.authorityNodeId,
      authorityGenesisIdentitySha256:
        localSource.chainSync.genesisIdentitySha256,
      observationCount: 2,
      independentProviderCount: 1,
      queryObservationCount: 1,
      reasonCodes: ["local_node_consistent"],
    });
    expect(
      (
        pending.context.finalityAuthority?.consistency as {
          reasonCodes: readonly string[];
        }
      ).reasonCodes,
    ).not.toContain("insufficient_independent_providers");
    expect(
      evaluateWatcherProofThreadIndexerV1(policy, null, pending.observation, {
        ...pending.context,
        finalityAuthority: {
          ...pending.context.finalityAuthority!,
          policy: finalityPolicy,
        },
      }),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "hold",
      reasonCodes: ["malformed_public_context"],
    });
    const pendingResult = evaluateWatcherProofThreadIndexerV1(
      policy,
      null,
      pending.observation,
      pending.context,
    );
    expect(pendingResult).toMatchObject({
      action: "accept",
      protocolDecision: "hold",
      reasonCodes: ["init_pending"],
    });

    const final = initStage({
      phase: "final",
      previousState: pendingResult.state!,
      previousFinalityState: pending.finalityState,
      sourceStore: pending.store,
      sourceMode: "local_node",
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        pendingResult.state,
        final.observation,
        final.context,
      ),
    ).toMatchObject({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: ["init_confirmed"],
    });

    const modeSubstitution: WatcherProofThreadPublicContextV1 = {
      ...pending.context,
      finalityAuthority: {
        ...pending.context.finalityAuthority!,
        policy: finalityPolicy,
      },
    };
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        pending.observation,
        modeSubstitution,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const observations = structuredClone(
      pending.context.finalityAuthority!.observations,
    ) as Mutable[];
    (
      (observations[1]!.l1Observation as Mutable).chainPoint as Mutable
    ).blockHash = h32("de");
    const consistency = evaluateWatcherMultiProviderConsistencyV1(
      sourceFixture("local_node").consistencyConfig,
      observations.map((candidate) =>
        normalizeWatcherL1BlockV1(
          candidate.authenticatedProvider,
          candidate.l1Observation,
        ),
      ),
    );
    const finalityResult = evaluateWatcherFinalityV1(
      localFinalityPolicy,
      null,
      consistency,
    );
    expect(consistency).toMatchObject({
      status: "quarantined",
      reasonCodes: ["fork_disagreement"],
    });
    const misalignedQuery: WatcherProofThreadPublicContextV1 = {
      ...pending.context,
      finalityAuthority: {
        ...pending.context.finalityAuthority!,
        observations,
        consistency,
        result: finalityResult,
      },
    };
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        pending.observation,
        misalignedQuery,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("indexes deterministic step, success, proof-token removal, and cancellation lifecycles", () => {
    const initPending = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
      sourceMode: "local_node",
    });
    const initPendingResult = evaluateWatcherProofThreadIndexerV1(
      policy,
      null,
      initPending.observation,
      initPending.context,
    );
    expect(initPendingResult.action).toBe("accept");
    const initFinal = initStage({
      phase: "final",
      previousState: initPendingResult.state!,
      previousFinalityState: initPending.finalityState,
      sourceStore: initPending.store,
      sourceMode: "local_node",
    });
    const initFinalResult = evaluateWatcherProofThreadIndexerV1(
      policy,
      initPendingResult.state,
      initFinal.observation,
      initFinal.context,
    );
    expect(initFinalResult.action).toBe("accept");

    const stepFixture = stepTransaction(initFinal.journal);
    const stepSource = addSubmittedTransaction(
      initFinal.store,
      stepFixture,
      transitionIds(1).submissionId,
    );
    const step = runFinalTransition({
      transitionKind: "step",
      fixture: stepFixture,
      previousState: initFinalResult.state!,
      previousFinalityState: null,
      sourceStore: stepSource,
      sourceJournal: initFinal.journal,
      ordinal: 1,
    });
    expect(step.state.journal).toMatchObject({
      phase: "active",
      stepIndex: "1",
      threadOutRef: `${stepFixture.txHash}#0`,
    });

    const successFixture = successTransaction(step.final.journal);
    const successSource = addSubmittedTransaction(
      step.final.store,
      successFixture,
      transitionIds(2).submissionId,
    );
    const success = runFinalTransition({
      transitionKind: "success",
      fixture: successFixture,
      previousState: step.state,
      previousFinalityState: null,
      sourceStore: successSource,
      sourceJournal: step.final.journal,
      ordinal: 2,
    });
    expect(success.state.journal).toMatchObject({
      phase: "proven",
      threadOutRef: null,
      proofTokenOutRef: `${successFixture.txHash}#0`,
    });
    expect(
      success.final.store.protocolUtxos.find(
        ({ outRef }) => outRef === `${successFixture.txHash}#0`,
      )?.role,
    ).toBe("proof_thread");

    const removalFixture = removalTransaction(success.final.journal);
    const removalSource = addSubmittedTransaction(
      success.final.store,
      removalFixture,
      transitionIds(3).submissionId,
    );
    const removal = runFinalTransition({
      transitionKind: "removal",
      fixture: removalFixture,
      previousState: success.state,
      previousFinalityState: null,
      sourceStore: removalSource,
      sourceJournal: success.final.journal,
      ordinal: 3,
    });
    expect(removal.state.journal).toMatchObject({
      phase: "removed",
      proofTokenOutRef: `${successFixture.txHash}#0`,
      correctionId: transitionIds(3).correctionId,
    });
    expect(removal.final.store.correctionResults).toContainEqual(
      expect.objectContaining({
        correctionId: transitionIds(3).correctionId,
        outcome: "removed",
      }),
    );

    const cancelFixture = cancelTransaction(initFinal.journal);
    const cancelSource = addSubmittedTransaction(
      initFinal.store,
      cancelFixture,
      transitionIds(4).submissionId,
    );
    const cancel = runFinalTransition({
      transitionKind: "cancel",
      fixture: cancelFixture,
      previousState: initFinalResult.state!,
      previousFinalityState: null,
      sourceStore: cancelSource,
      sourceJournal: initFinal.journal,
      ordinal: 4,
    });
    expect(cancel.state.journal).toMatchObject({
      phase: "cancelled",
      threadOutRef: null,
      proofTokenOutRef: null,
    });
    expect(
      cancel.final.store.protocolUtxos.some(
        ({ role }) => role === "computation_thread",
      ),
    ).toBe(false);
  });

  it.each<FixtureSourceMode>(["local_node", "external_providers"])(
    "accepts only an exact %s W13 rewind, binds every replacement anchor, and restores the prior local journal",
    (sourceMode) => {
      const initPending = initStage({
        phase: "pending",
        previousState: null,
        previousFinalityState: null,
      });
      const initPendingResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        initPending.observation,
        initPending.context,
      );
      const initFinal = initStage({
        phase: "final",
        previousState: initPendingResult.state!,
        previousFinalityState: initPending.finalityState,
        sourceStore: initPending.store,
      });
      const initFinalResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        initPendingResult.state,
        initFinal.observation,
        initFinal.context,
      );
      const stepFixture = stepTransaction(initFinal.journal);
      const stepSource = addSubmittedTransaction(
        initFinal.store,
        stepFixture,
        transitionIds(5).submissionId,
      );
      const stepPending = transitionStage({
        transitionKind: "step",
        fixture: stepFixture,
        phase: "pending",
        previousState: initFinalResult.state!,
        previousFinalityState: null,
        sourceStore: stepSource,
        sourceJournal: initFinal.journal,
        ordinal: 5,
        sourceMode,
      });
      const stepPendingResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        initFinalResult.state,
        stepPending.observation,
        stepPending.context,
      );
      expect(stepPendingResult.action).toBe("accept");

      const replacementL1 = sourceFixture(sourceMode);
      const replacement = replacementL1.providers.map((provider) =>
        normalize(provider, stepFixture, "1", 6),
      );
      const consistency = evaluateWatcherMultiProviderConsistencyV1(
        replacementL1.consistencyConfig,
        replacement,
      );
      expect(consistency).toMatchObject({
        status: "agreed",
        sourceMode,
        protocolDecision: "allowed",
      });
      expect(consistency.independentProviderCount).toBe(
        sourceMode === "local_node" ? 1 : 2,
      );
      expect(consistency.queryObservationCount).toBe(
        sourceMode === "local_node" ? 1 : 0,
      );
      const finalityResult = evaluateWatcherFinalityV1(
        replacementL1.policy,
        stepPending.finalityState,
        consistency,
      );
      expect(finalityResult.action).toBe("rewind_pending");
      const rollbackSource = persistNormalizedObservations(
        stepPending.store,
        replacement,
      );
      const bootstrap = makeWatcherRollbackBootstrapStateV1(
        replacementL1.policy,
        rollbackSource,
        stepPending.finalityState,
      )!;
      expect(bootstrap).not.toBeNull();
      const rollbackContext = {
        policy: replacementL1.policy,
        sourceStore: rollbackSource,
        previousFinalityState: stepPending.finalityState,
        consistency,
        finalityResult,
        previousRollbackState: bootstrap,
        rollbackBootstrapState: bootstrap,
      };
      const rollbackResult = evaluateWatcherRollbackV1(
        replacementL1.policy,
        rollbackSource,
        stepPending.finalityState,
        consistency,
        finalityResult,
        bootstrap,
        bootstrap,
      );
      expect(rollbackResult).toMatchObject({
        action: "apply_rewind",
        reasonCodes: ["rewind_applied"],
      });
      expect(rollbackResult.removedRecords.confirmationIds).toContain(
        transitionIds(5).confirmationId,
      );
      expect(rollbackResult.nextStore).not.toBeNull();
      const replacementBlock = replacement[0]!;
      const rollbackObservation = makeWatcherProofThreadObservationV1({
        policyDigest: policy.policyDigest,
        network: policy.network,
        releaseEvidenceDigest: policy.releaseEvidenceDigest,
        deploymentMarker: policy.deploymentMarker,
        transitionKind: "rollback",
        confirmationPhase: null,
        pointDigest: replacementBlock.chainPoint.pointDigest,
        blockHash: replacementBlock.chainPoint.blockHash,
        slot: replacementBlock.chainPoint.slot,
        blockNo: replacementBlock.chainPoint.blockNo,
        transactionHash: null,
        publicInputDigest: createHash("sha256")
          .update(encodeWatcherNormalizedL1BlockV1(replacementBlock))
          .digest("hex"),
        sourceObservationDigest: replacementBlock.observationDigest,
        chainPointId: replacementBlock.chainPoint.chainPointId,
        sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(rollbackSource),
        ),
        sourceDurableStoreRevision: rollbackSource.revision,
        durableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(rollbackResult.nextStore!),
        ),
        durableStoreRevision: rollbackResult.nextStore!.revision,
        predecessorStateDigest: stepPendingResult.state!.stateDigest,
        submissionId: null,
        confirmationId: null,
        rollbackTargetStateDigest: initFinalResult.state!.stateDigest,
        layout: null,
        journal: null,
      })!;
      const rollbackPublicContext: WatcherProofThreadPublicContextV1 = {
        schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
        authenticatedProvider: null,
        l1Observation: null,
        sourceDurableStore: rollbackSource,
        durableStore: rollbackResult.nextStore!,
        deploymentAuthority: authority.deploymentAuthority,
        finalityAuthority: null,
        rollbackAuthority: {
          result: rollbackResult,
          context: rollbackContext,
        },
        sourceJournal: initFinal.journal,
        durableJournal: initFinal.journal,
      };
      const indexed = evaluateWatcherProofThreadIndexerV1(
        policy,
        stepPendingResult.state,
        rollbackObservation,
        rollbackPublicContext,
      );
      expect(indexed).toMatchObject({
        action: "accept",
        protocolDecision: "indexed",
        reasonCodes: ["rollback_confirmed"],
        state: {
          journal: initFinal.journal,
          pending: null,
        },
      });
      expect(indexed.state?.history).toHaveLength(2);
      expect(indexed.state?.auditHistory.at(-1)?.status).toBe("rollback");
      const {
        schemaVersion: _rollbackObservationSchema,
        observationDigest: _rollbackObservationDigest,
        ...rollbackObservationWithoutDigest
      } = rollbackObservation;
      const anchorMutations = [
        ["pointDigest", h32("b1")],
        ["blockHash", h32("b2")],
        ["slot", (BigInt(rollbackObservation.slot) + 1n).toString()],
        ["blockNo", (BigInt(rollbackObservation.blockNo) + 1n).toString()],
        ["chainPointId", h32("b4")],
        ["sourceObservationDigest", h32("b5")],
        ["publicInputDigest", h32("b6")],
      ] as const;
      for (const [field, hostileValue] of anchorMutations) {
        const hostileObservation = makeWatcherProofThreadObservationV1({
          ...rollbackObservationWithoutDigest,
          [field]: hostileValue,
        })!;
        expect(
          evaluateWatcherProofThreadIndexerV1(
            policy,
            stepPendingResult.state,
            hostileObservation,
            rollbackPublicContext,
          ),
          field,
        ).toMatchObject({
          action: "reject",
          protocolDecision: "hold",
          reasonCodes: ["rollback_authority_mismatch"],
        });
      }
      const hostile = structuredClone(rollbackResult);
      (hostile.removedRecords as Mutable).confirmationIds = [];
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          stepPendingResult.state,
          rollbackObservation,
          {
            ...rollbackPublicContext,
            rollbackAuthority: {
              result: hostile,
              context: rollbackContext,
            },
          },
        ).action,
      ).toBe("reject");
    },
  );

  it("rejects independently rehashed one-field mutations of tx roles, journal lineage, finality, and W03 confirmation", () => {
    const pending = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
    });
    const {
      schemaVersion: _observationSchema,
      observationDigest: _observationDigest,
      ...observationWithoutDigest
    } = pending.observation;
    const wrongLayout = makeWatcherProofThreadObservationV1({
      ...observationWithoutDigest,
      layout: makeWatcherProofThreadLayoutV1({
        ...pending.observation.layout!,
        computationThreadMintRedeemerGlobalIndex: "1",
      })!,
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        wrongLayout,
        pending.context,
      ).action,
    ).toBe("reject");

    const {
      schemaVersion: _journalSchema,
      journalDigest: _journalDigest,
      ...journalWithoutDigest
    } = pending.journal;
    const wrongJournal = makeWatcherProofThreadJournalV1({
      ...journalWithoutDigest,
      threadOutRef: `${pending.fixture.txHash}#1`,
      confirmedTransactionHashes: pending.journal.confirmedTransactionHashes,
    });
    const wrongJournalObservation = makeWatcherProofThreadObservationV1({
      ...observationWithoutDigest,
      journal: wrongJournal,
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        wrongJournalObservation,
        {
          ...pending.context,
          durableJournal: wrongJournal,
        },
      ).action,
    ).toBe("reject");

    const wrongFinality = structuredClone(pending.context);
    (wrongFinality.finalityAuthority as Mutable).result.protocolDecision =
      "finality_granted";
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        pending.observation,
        wrongFinality,
      ).action,
    ).toBe("reject");

    const wrongStore = makeWatcherDurableStoreV1({
      deploymentMarker: authority.marker,
      revision: pending.store.revision,
      records: {
        ...pending.store,
        confirmations: pending.store.confirmations.map((confirmation) => ({
          ...confirmation,
          txHash: h32("00"),
        })),
      },
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(policy, null, pending.observation, {
        ...pending.context,
        durableStore: wrongStore,
      }).action,
    ).toBe("reject");
  });
});

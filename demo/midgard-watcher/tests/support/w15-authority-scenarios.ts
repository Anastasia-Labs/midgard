import { execFile } from "node:child_process";
import {
  createHash,
  generateKeyPairSync,
  sign,
  X509Certificate,
} from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";
import { createServer as createTlsServer } from "node:tls";
import { promisify } from "node:util";

import {
  type AddressData,
  DepositDatum,
  ForcedInclusionTxV1,
  HubOracleDatum,
  MerkleRoot,
  outputReferenceToPlutusDataCbor,
  PayoutDatum,
  PayoutMintRedeemer,
  Proof,
  resolveEventInclusionTime,
  RootDomain,
  SettlementDatum,
  TxFieldReceiptV1,
  TxOrderDatumV1,
  TxOrderSpendRedeemerV1,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  WithdrawalOrderDatum,
  WithdrawalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { blake2b } from "../../../midgard-core/node_modules/@noble/hashes/blake2.js";
import { computeHash32 } from "../../../midgard-core/src/codec/hash.js";
import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1,
  materializeMidgardNativeTxFromCanonicalV1,
  type MidgardNativeTxCanonicalV1,
} from "../../../midgard-core/src/codec/native.js";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "../../../midgard-core/src/codec/native-constants.js";
import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "../../../midgard-core/src/consensus-profile-v1.js";
import {
  deriveMidgardTxFieldReceiptAssetNameV1,
  deriveMidgardV1TxFieldChunks,
} from "../../../midgard-core/src/consensus-validation-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "../../../midgard-core/src/da-transport.js";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
  makeDeploymentMarkerV1,
} from "../../../midgard-core/src/deployment-manifest-identity-v1.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/config.js";
import {
  makeWatcherDeploymentIdentitySignaturePayloadV1,
  verifyWatcherDeploymentIdentityV1,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicyV1,
} from "../../src/deployment-identity.js";
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
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherAuthenticatedL1ProviderV1,
  type WatcherL1TransportAttestationContextV1,
  watcherL1TransportAttestationDetailsV1,
} from "../../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw } from "../../src/multi-provider-consistency.js";
import {
  deriveWatcherUserEventObservationV1 as deriveWatcherUserEventObservationV1Raw,
  evaluateWatcherUserEventIndexerV1 as evaluateWatcherUserEventIndexerV1Raw,
  makeWatcherUserEventIndexerPolicyV1,
  parseWatcherUserEventIndexerResultV1 as parseWatcherUserEventIndexerResultV1Raw,
  WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  type WatcherIndexedUserEventV1,
  type WatcherTerminalUserEventV1,
  type WatcherUserEventIndexerPolicyV1,
  type WatcherUserEventIndexerStateV1,
  type WatcherUserEventKindV1,
  type WatcherUserEventPublicContextV1,
} from "../../src/user-event-indexer.js";
import { canonicalFraudProofCatalogueFixture } from "../canonical-fraud-proof-catalogue.js";
import { makeWatcherAuthorityDeploymentFixtureV1 } from "./watcher-opaque-authority-harness.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const asWireValue = <T>(value: T): T => JSON.parse(JSON.stringify(value)) as T;
const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const reorderWireKeys = (value: unknown): unknown => {
  if (Array.isArray(value)) {
    return value.map(reorderWireKeys);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value as Record<string, unknown>)
        .reverse()
        .map(([key, member]) => [key, reorderWireKeys(member)]),
    );
  }
  return value;
};
const encodeData = Data.to as unknown as (
  value: unknown,
  schema: unknown,
) => string;
const scriptAddress = (scriptHash: string): string => `70${scriptHash}`;

type MutableRecord = Record<string, unknown>;
type AuthorityContractFixtureV1 = Readonly<{
  refScriptUTxO: Readonly<{ txHash: string; outputIndex: number }> | null;
  contract: Readonly<{ type: string; cborHex: string }>;
  scriptHash: string;
}> & {
  fraudProofCatalogue?: ReturnType<typeof canonicalFraudProofCatalogueFixture>;
};
type AuthorityReferenceScriptFixtureV1 = Readonly<{
  status: string;
  roleUnit: string;
  scriptHash: string;
  outRef: string;
}>;
const transportEndpointByProviderId = new Map<string, string>();
const RELEASE_DIGEST = h32("22");
const BLUEPRINT_HASH = h32("55");
const RULE_BUNDLE_COMMITMENT = h32("44");
const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

export const makeDeploymentAuthority = () => {
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
  ) as Record<string, AuthorityContractFixtureV1>;
  const fraudProofCatalogue = canonicalFraudProofCatalogueFixture(contracts);
  const catalogueContract = contracts.fraudProofCatalogueMint;
  if (catalogueContract === undefined) {
    throw new Error("authority catalogue contract is missing");
  }
  catalogueContract.fraudProofCatalogue = fraudProofCatalogue;
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
  ) as Record<string, AuthorityReferenceScriptFixtureV1>;
  const parameters = {
    maxTxSize: 16_384,
    maxValueSize: 5_000,
    maxTxExUnits: { memory: "16500000", steps: "10000000000" },
  };
  const hubOracleOneShot = {
    txHash: h32("11"),
    outputIndex: 0,
    outRef: `${h32("11")}#0`,
    status: "consumed_by_init",
  };
  const daIdentity = {
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
  };
  const identity: MutableRecord = {
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
    hubOracleOneShot,
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
    da: daIdentity,
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
  const manifestId = computeDeploymentManifestV1Id(identity);
  const manifest = {
    ...identity,
    manifestId,
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
      identityDigest: computeDeploymentManifestV1JsonDigest(daIdentity),
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
  const signedIdentity = {
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
      manifestId,
      releaseBindings,
    ),
    privateKey,
  ).toString("hex");
  const deploymentPolicy: WatcherDeploymentIdentityPolicyV1 = {
    network: "Preprod",
    hubOracleOneShotOutRef: hubOracleOneShot.outRef,
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
            scriptHash: referenceScripts[role]!.scriptHash,
            outRef: referenceScripts[role]!.outRef,
          },
        ],
      ),
    ),
    fraudProofCatalogue: {
      root: fraudProofCatalogue.root,
      categories: Object.fromEntries(
        Object.entries(fraudProofCatalogue.categories).map(([name, value]) => {
          const category = value as {
            readonly categoryId?: unknown;
            readonly scriptHash?: unknown;
          };
          if (
            typeof category.categoryId !== "string" ||
            typeof category.scriptHash !== "string"
          ) {
            throw new Error("authority catalogue category is malformed");
          }
          return [
            name,
            {
              categoryId: category.categoryId,
              scriptHash: category.scriptHash,
            },
          ];
        }),
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
  const marker = makeDeploymentMarkerV1(manifestId);
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

const deploymentAuthorityFixture = makeWatcherAuthorityDeploymentFixtureV1();
const applied = deploymentAuthorityFixture.policy.appliedScriptHashes;

const eventFields = {
  deposit: {
    policyId: applied.depositMint!,
    spendScriptHash: applied.depositSpend!,
    addressHex: scriptAddress(applied.depositSpend!),
  },
  withdrawal: {
    policyId: applied.withdrawalMint!,
    spendScriptHash: applied.withdrawalSpend!,
    addressHex: scriptAddress(applied.withdrawalSpend!),
  },
  forcedOrder: {
    policyId: applied.txOrderMint!,
    spendScriptHash: applied.txOrderSpend!,
    addressHex: scriptAddress(applied.txOrderSpend!),
  },
} as const;
const asAddressData = (addressHex: string) => ({
  paymentCredential: {
    ScriptCredential: [addressHex.slice(2)],
  } as { ScriptCredential: [string] },
  stakeCredential: null,
});
const hubDatum = {
  registered_operators: applied.registeredOperatorsMint!,
  active_operators: applied.activeOperatorsMint!,
  retired_operators: applied.retiredOperatorsMint!,
  scheduler: applied.schedulerMint!,
  state_queue: applied.stateQueueMint!,
  fraud_proof_catalogue: applied.fraudProofCatalogueMint!,
  fraud_proof: applied.fraudProofMint!,
  deposit: eventFields.deposit.policyId,
  withdrawal: eventFields.withdrawal.policyId,
  tx_order: eventFields.forcedOrder.policyId,
  settlement: applied.settlementMint!,
  payout: applied.payoutMint!,
  registered_operators_addr: asAddressData(
    scriptAddress(applied.registeredOperatorsSpend!),
  ),
  active_operators_addr: asAddressData(
    scriptAddress(applied.activeOperatorsSpend!),
  ),
  retired_operators_addr: asAddressData(
    scriptAddress(applied.retiredOperatorsSpend!),
  ),
  scheduler_addr: asAddressData(scriptAddress(applied.schedulerSpend!)),
  state_queue_addr: asAddressData(scriptAddress(applied.stateQueueSpend!)),
  fraud_proof_catalogue_addr: asAddressData(
    scriptAddress(applied.fraudProofCatalogueSpend!),
  ),
  fraud_proof_addr: asAddressData(scriptAddress(applied.fraudProofSpend!)),
  deposit_addr: asAddressData(eventFields.deposit.addressHex),
  withdrawal_addr: asAddressData(eventFields.withdrawal.addressHex),
  tx_order_addr: asAddressData(eventFields.forcedOrder.addressHex),
  settlement_addr: asAddressData(scriptAddress(applied.settlementSpend!)),
  reserve_addr: asAddressData(scriptAddress(applied.reserveSpend!)),
  payout_addr: asAddressData(scriptAddress(applied.payoutSpend!)),
  reserve_observer: applied.reserveWithdraw!,
};
const hubDatumHex = Data.to(hubDatum, HubOracleDatum);
const hubAssets = CML.MultiAsset.new();
hubAssets.set(
  CML.ScriptHash.from_hex(applied.hubOracleMint!),
  CML.AssetName.from_hex(""),
  1n,
);
const hubOutput = CML.TransactionOutput.new(
  CML.Address.from_hex(scriptAddress(applied.hubOracleMint!)),
  CML.Value.new(5_000_000n, hubAssets),
  CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(hubDatumHex)),
);
const HUB_OUT_REF = `${h32("a0")}#0`;
const SETTLEMENT_OUT_REF = `${h32("a3")}#0`;
const MEMBERSHIP_PHAS_ROOT = h32("a4");
const countedRoot = (
  domain:
    | "DepositsRootDomain"
    | "WithdrawalsRootDomain"
    | "ForcedTransactionsV1RootDomain",
): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardRootCountV1", "utf8"),
        Buffer.from(Data.to(domain as never, RootDomain as never), "hex"),
        Buffer.from(MEMBERSHIP_PHAS_ROOT, "hex"),
        Buffer.from(Data.to(1n as never, Data.Integer() as never), "hex"),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");
const settlementDatum = {
  deposits_root: countedRoot("DepositsRootDomain"),
  withdrawals_root: countedRoot("WithdrawalsRootDomain"),
  forced_transactions_root: countedRoot("ForcedTransactionsV1RootDomain"),
  transactions_root: h32("a5"),
  resolution_claim: null,
};
const settlementAssets = CML.MultiAsset.new();
settlementAssets.set(
  CML.ScriptHash.from_hex(applied.settlementMint!),
  CML.AssetName.from_hex(""),
  1n,
);
const settlementOutput = CML.TransactionOutput.new(
  CML.Address.from_hex(scriptAddress(applied.settlementSpend!)),
  CML.Value.new(5_000_000n, settlementAssets),
  CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(Data.to(settlementDatum, SettlementDatum)),
  ),
);
const BOOTSTRAP_CHAIN_POINT_ID = h32("a1");
const bootstrapStore = makeWatcherDurableStoreV1({
  deploymentMarker: deploymentAuthorityFixture.marker,
  revision: "0",
  records: {
    l1Observations: [],
    chainPoints: [
      {
        chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
        providerId: "provider-a",
        blockHash: h32("a2"),
        slot: "1",
        blockNo: "1",
        depth: "10",
      },
    ],
    protocolUtxos: [
      {
        outRef: HUB_OUT_REF,
        role: "hub_oracle",
        chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
        output: makeWatcherDurablePayloadV1(hubOutput.to_cbor_hex()),
      },
      {
        outRef: SETTLEMENT_OUT_REF,
        role: "settlement",
        chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
        output: makeWatcherDurablePayloadV1(settlementOutput.to_cbor_hex()),
      },
    ],
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
let deploymentAuthority = {
  signedIdentity: deploymentAuthorityFixture.signedIdentity,
  policy: deploymentAuthorityFixture.policy,
  trustRoots: deploymentAuthorityFixture.trustRoots,
  result: deploymentAuthorityFixture.result,
};

const emptyNativeTxCanonical: MidgardNativeTxCanonicalV1 = {
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: Buffer.alloc(32),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
};
const emptyNativeTx = materializeMidgardNativeTxFromCanonicalV1(
  emptyNativeTxCanonical,
);
const emptyNativeSource = deriveMidgardNativeTxProofSourceV1(emptyNativeTx);
const emptyNativePayload = {
  tx_id: computeMidgardNativeTxIdV1(emptyNativeTx).toString("hex"),
  transaction_commitment:
    computeMidgardNativeTxProofCommitmentV1(emptyNativeSource).toString("hex"),
  source: {
    compact_cbor: emptyNativeSource.compactCbor.toString("hex"),
    witness_set_compact_cbor:
      emptyNativeSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      emptyNativeSource.fieldPreimageLengthsCbor.toString("hex"),
  },
  terminal_receipt_reference: null,
};
const policy = makeWatcherUserEventIndexerPolicyV1({
  network: "Preprod",
  releaseEvidenceDigest: RELEASE_DIGEST,
  deploymentMarker: deploymentAuthorityFixture.marker,
  deposit: eventFields.deposit,
  withdrawal: eventFields.withdrawal,
  forcedOrder: eventFields.forcedOrder,
  bootstrapStoreDigest: watcherDurableStoreBytesSha256(
    encodeWatcherDurableStoreV1(bootstrapStore),
  ),
  deploymentTrustRootId: deploymentAuthorityFixture.result.trustRootId,
  requiredFinalityDepth: "2",
  maximumActiveHistoryEntries: "32",
  maximumAuditHistoryEntries: "128",
}) as WatcherUserEventIndexerPolicyV1;

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
      trustRootId: h32("33"),
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      ruleBundleCommitment: h32("44"),
      programCommitments: { validation: h32("55") },
      durableMarker: policy.deploymentMarker,
    },
  )!;
let finalityPolicy: NonNullable<ReturnType<typeof makeWatcherFinalityPolicyV1>>;

let provider: WatcherAuthenticatedL1ProviderV1;
let providerB: WatcherAuthenticatedL1ProviderV1;
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
const execFileAsync = promisify(execFile);
const watcherTransportContexts: WatcherL1TransportAttestationContextV1[] = [];
let normalizedTransportContexts = new WeakMap<
  object,
  WatcherL1TransportAttestationContextV1
>();
const watcherTransportServers: Server[] = [];
let watcherTransportFixtureDirectory = "";
let opaqueAuthorityTransportLease:
  | "idle"
  | "initializing"
  | "active"
  | "disposing" = "idle";
let opaqueAuthorityTransportLeaseOwner: symbol | null = null;
let opaqueAuthorityTransportCleanupToken: symbol | null = null;
let opaqueAuthorityTransportCleanupPromise: Promise<void> | null = null;

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

const makeTlsTransportFixture = async (name: string) => {
  const keyPath = join(watcherTransportFixtureDirectory, `${name}.key`);
  const certificatePath = join(watcherTransportFixtureDirectory, `${name}.crt`);
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
  watcherTransportServers.push(server);
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("watcher TLS fixture did not bind a port");
  }
  return {
    certificate,
    identitySha256: createHash("sha256")
      .update(new X509Certificate(certificate).raw)
      .digest("hex"),
    port: address.port,
  };
};

const initializeOpaqueAuthorityTransports = async (
  fixtureRoot = "/dev/shm",
): Promise<symbol> => {
  if (opaqueAuthorityTransportLease !== "idle") {
    throw new Error("W15 opaque authority fixture lease is not idle");
  }
  const leaseOwner = Symbol("W15 opaque authority fixture lease");
  opaqueAuthorityTransportLeaseOwner = leaseOwner;
  opaqueAuthorityTransportLease = "initializing";
  try {
    if (
      watcherTransportContexts.length !== 0 ||
      watcherTransportServers.length !== 0 ||
      watcherTransportFixtureDirectory !== ""
    ) {
      throw new Error("W15 opaque authority fixture state is not clean");
    }
    const freshDeployment = makeWatcherAuthorityDeploymentFixtureV1();
    deploymentAuthority = {
      signedIdentity: freshDeployment.signedIdentity,
      policy: freshDeployment.policy,
      trustRoots: freshDeployment.trustRoots,
      result: freshDeployment.result,
    };
    watcherTransportFixtureDirectory = await mkdtemp(
      join(fixtureRoot, "midgard-w17-transports-"),
    );
    const externalTransports: WatcherL1TransportAttestationContextV1[] = [];
    for (const [providerId, operatorIdentitySha256] of [
      ["provider-a", h32("97")],
      ["provider-b", h32("98")],
    ] as const) {
      const fixture = await makeTlsTransportFixture(providerId);
      const endpoint = `https://localhost:${fixture.port}`;
      transportEndpointByProviderId.set(providerId, endpoint);
      const configuredProvider = externalSource.providers.find(
        ({ providerId: configuredProviderId }) =>
          configuredProviderId === providerId,
      );
      if (configuredProvider === undefined) {
        throw new Error("missing external-provider fixture policy");
      }
      configuredProvider.endpoint = endpoint;
      const transport = await establishWatcherExternalProviderTransportV1({
        network: "Preprod",
        providerId,
        operatorIdentitySha256,
        endpoint,
        caPem: fixture.certificate,
        expectedTlsPublicIdentitySha256: fixture.identitySha256,
        connectTimeoutMs: 2_000,
      });
      externalTransports.push(transport);
      watcherTransportContexts.push(transport);
    }
    finalityPolicy = makeExternalFinalityPolicy();
    provider = watcherL1TransportAttestationDetailsV1(
      externalTransports[0],
    )!.provider;
    providerB = watcherL1TransportAttestationDetailsV1(
      externalTransports[1],
    )!.provider;
    opaqueAuthorityTransportLease = "active";
    return leaseOwner;
  } catch (error) {
    await disposeOpaqueAuthorityTransports(leaseOwner);
    throw error;
  }
};

const disposeOpaqueAuthorityTransports = (
  leaseOwner: symbol,
): Promise<void> => {
  if (opaqueAuthorityTransportLeaseOwner !== leaseOwner) {
    return Promise.resolve();
  }
  if (opaqueAuthorityTransportLease === "disposing") {
    return opaqueAuthorityTransportCleanupPromise ?? Promise.resolve();
  }
  opaqueAuthorityTransportLease = "disposing";
  const cleanupToken = Symbol("W15 opaque authority fixture cleanup");
  opaqueAuthorityTransportCleanupToken = cleanupToken;
  const cleanupPromise = Promise.resolve().then(async () => {
    try {
      for (const context of watcherTransportContexts) {
        closeWatcherL1TransportAttestationContextV1(context);
      }
      await Promise.all(
        watcherTransportServers.map(
          (server) =>
            new Promise<void>((resolve) => server.close(() => resolve())),
        ),
      );
      if (watcherTransportFixtureDirectory !== "") {
        await rm(watcherTransportFixtureDirectory, {
          recursive: true,
          force: true,
        });
      }
    } finally {
      if (
        opaqueAuthorityTransportLeaseOwner === leaseOwner &&
        opaqueAuthorityTransportCleanupToken === cleanupToken
      ) {
        watcherTransportContexts.length = 0;
        watcherTransportServers.length = 0;
        transportEndpointByProviderId.clear();
        normalizedTransportContexts = new WeakMap();
        userEventFinalityLineageByStateDigest.clear();
        serial = 0;
        watcherTransportFixtureDirectory = "";
        externalSource.providers[0]!.endpoint = "https://cardano-a.example";
        externalSource.providers[1]!.endpoint = "https://cardano-b.example";
        opaqueAuthorityTransportCleanupPromise = null;
        opaqueAuthorityTransportCleanupToken = null;
        opaqueAuthorityTransportLeaseOwner = null;
        opaqueAuthorityTransportLease = "idle";
      }
    }
  });
  opaqueAuthorityTransportCleanupPromise = cleanupPromise;
  return cleanupPromise;
};

const transportForProvider = (
  authenticatedProvider: unknown,
): WatcherL1TransportAttestationContextV1 => {
  const matches = watcherTransportContexts.filter((context) => {
    const details = watcherL1TransportAttestationDetailsV1(context);
    return (
      details !== null &&
      JSON.stringify(details.provider) === JSON.stringify(authenticatedProvider)
    );
  });
  if (matches.length !== 1) {
    throw new Error("test provider lacks one live transport attestation");
  }
  return matches[0]!;
};

const normalizeWatcherL1BlockV1 = (
  authenticatedProvider: unknown,
  observation: unknown,
) => {
  const transport = transportForProvider(authenticatedProvider);
  const normalized = normalizeWatcherL1BlockV1Raw(transport, observation);
  normalizedTransportContexts.set(normalized, transport);
  return normalized;
};

const evaluateWatcherMultiProviderConsistencyV1 = (
  configuredSource: unknown,
  observations: readonly unknown[],
) =>
  evaluateWatcherMultiProviderConsistencyV1Raw(
    configuredSource,
    observations,
    observations.map((observation) => {
      const transport =
        typeof observation === "object" && observation !== null
          ? normalizedTransportContexts.get(observation)
          : undefined;
      if (transport === undefined) {
        throw new Error("test observation lacks live transport provenance");
      }
      return transport;
    }),
  );

const deriveWatcherUserEventObservationV1 = (
  policyInput: unknown,
  previousStateInput: unknown,
  publicContextInput: unknown,
  rollbackTargetEntryDigest: string | null = null,
) =>
  deriveWatcherUserEventObservationV1Raw(
    policyInput,
    previousStateInput,
    publicContextInput,
    watcherTransportContexts,
    rollbackTargetEntryDigest,
  );

const evaluateWatcherUserEventIndexerV1 = (
  policyInput: unknown,
  previousStateInput: unknown,
  observationInput: unknown,
  publicContextInput: unknown,
) =>
  evaluateWatcherUserEventIndexerV1Raw(
    policyInput,
    previousStateInput,
    observationInput,
    publicContextInput,
    watcherTransportContexts,
  );

const parseWatcherUserEventIndexerResultV1 = (
  value: unknown,
  context: Omit<
    Parameters<typeof parseWatcherUserEventIndexerResultV1Raw>[1],
    "transportAttestations"
  >,
) =>
  parseWatcherUserEventIndexerResultV1Raw(value, {
    ...context,
    transportAttestations: watcherTransportContexts,
  });

const addressData = {
  paymentCredential: {
    PublicKeyCredential: [h28("88")],
  },
  stakeCredential: null,
} as const;

const nonceAssetName = (txHash: string, outputIndex: number): string => {
  const eventId = outputReferenceToPlutusDataCbor({ txHash, outputIndex });
  return Buffer.from(
    blake2b(Buffer.from(eventId, "hex"), { dkLen: 32 }),
  ).toString("hex");
};

type EventFixture = Readonly<{
  kind: WatcherUserEventKindV1;
  output: CML.TransactionOutput;
  outputCborHex: string;
  datumCborHex: string;
  assetNameHex: string;
  nonceInput: CML.TransactionInput;
  mintRedeemerHex: string;
  certificateRedeemerHex: string;
  certificate: CML.Certificate;
  fields: WatcherUserEventIndexerPolicyV1[
    | "deposit"
    | "withdrawal"
    | "forcedOrder"];
  extraReferenceOutRefs: readonly string[];
}>;

export type GenuineW15ForcedPayloadV1 = Readonly<{
  tx_id: string;
  transaction_commitment: string;
  source: Readonly<{
    compact_cbor: string;
    witness_set_compact_cbor: string;
    field_preimage_lengths_cbor: string;
  }>;
  terminal_receipt_reference: Readonly<{
    transactionId: string;
    outputIndex: bigint;
  }> | null;
}>;

const makeEventFixture = (
  kind: WatcherUserEventKindV1,
  nonceByte: string,
  nonceIndex: number,
  ttl: bigint,
  inclusionTimeDelta = 0n,
  addressOverride?: string,
  witnessOverride?: string,
  forcedPayloadOverride?: GenuineW15ForcedPayloadV1,
  extraReferenceOutRefs: readonly string[] = [],
  eventOverrides?: Readonly<{
    depositL2Address?: AddressData;
    withdrawalL2OutRef?: Readonly<{
      transactionId: string;
      outputIndex: bigint;
    }>;
  }>,
): EventFixture => {
  const fields =
    kind === "deposit"
      ? policy.deposit
      : kind === "withdrawal"
        ? policy.withdrawal
        : policy.forcedOrder;
  const nonceHash = h32(nonceByte);
  const nonceInput = CML.TransactionInput.new(
    CML.TransactionHash.from_hex(nonceHash),
    BigInt(nonceIndex),
  );
  const assetNameHex = nonceAssetName(nonceHash, nonceIndex);
  const witness = witnessOverride ?? userEventWitnessScriptHash(assetNameHex);
  const eventId = {
    transactionId: nonceHash,
    outputIndex: BigInt(nonceIndex),
  };
  const inclusion_time =
    BigInt(resolveEventInclusionTime(Number(ttl), "Preprod")) +
    inclusionTimeDelta;
  const datum =
    kind === "deposit"
      ? {
          event: {
            id: eventId,
            info: {
              l2_address: eventOverrides?.depositL2Address ?? addressData,
              l2_network_id: 0n,
              l2_datum: null,
            },
          },
          inclusion_time,
          witness,
        }
      : kind === "withdrawal"
        ? {
            event: {
              id: eventId,
              info: {
                body: {
                  l2_outref: eventOverrides?.withdrawalL2OutRef ?? eventId,
                  l2_owner: h28("89"),
                  l2_value: new Map<string, Map<string, bigint>>(),
                  l1_address: addressData,
                  l1_datum: "NoDatum" as const,
                },
                signature: ["aa", "bb"] as [string, string],
                validity: "WithdrawalIsValid" as const,
              },
            },
            inclusion_time,
            witness,
            refund_address: addressData,
            refund_datum: "NoDatum" as const,
          }
        : {
            event: {
              id: eventId,
              tx: forcedPayloadOverride ?? emptyNativePayload,
            },
            inclusion_time,
            witness,
            refund_address: addressData,
            refund_datum: "NoDatum" as const,
          };
  const schema =
    kind === "deposit"
      ? DepositDatum
      : kind === "withdrawal"
        ? WithdrawalOrderDatum
        : TxOrderDatumV1;
  const datumCborHex = CML.PlutusData.from_cbor_hex(
    Data.to(datum as never, schema as never),
  ).to_canonical_cbor_hex();
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(fields.policyId),
    CML.AssetName.from_hex(assetNameHex),
    1n,
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_hex(addressOverride ?? fields.addressHex),
    CML.Value.new(3_000_000n, multiasset),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCborHex)),
  );
  const certificate = CML.Certificate.new_reg_cert(
    CML.Credential.new_script(CML.ScriptHash.from_hex(witness)),
    0n,
  );
  return {
    kind,
    output,
    outputCborHex: output.to_canonical_cbor_hex(),
    datumCborHex,
    assetNameHex,
    nonceInput,
    mintRedeemerHex: encodeData(
      {
        AuthenticateEvent: {
          nonce_input_index: 0n,
          event_output_index: 0n,
          hub_ref_input_index: 0n,
          witness_registration_redeemer_index: 0n,
        },
      },
      UserEventMintRedeemer,
    ),
    certificateRedeemerHex: encodeData(
      { MintOrBurn: { targetPolicy: fields.policyId } },
      UserEventWitnessPublishRedeemer,
    ),
    certificate,
    fields,
    extraReferenceOutRefs,
  };
};

type BlockBundle = Readonly<{
  context: WatcherUserEventPublicContextV1;
  store: ReturnType<typeof makeWatcherDurableStoreV1>;
  transactionHash: string | null;
  finalityState: unknown;
}>;

type UserEventFinalityLineage = NonNullable<
  WatcherUserEventPublicContextV1["finalityAuthority"]
>["lineage"];

const userEventFinalityLineageByStateDigest = new Map<
  string,
  UserEventFinalityLineage
>();

const USER_EVENT_SCRIPT_DATA_HASH = CML.ScriptDataHash.from_raw_bytes(
  Buffer.alloc(32, 0x6a),
);

const userEventRedeemerTag = (purpose: string): CML.RedeemerTag => {
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

const contextFromTransaction = (
  transaction: {
    readonly txHash: string;
    readonly body: ReturnType<typeof makeWatcherL1PublicBytesV1>;
    readonly utxos: readonly unknown[];
    readonly scripts: readonly never[];
    readonly datums: readonly never[];
    readonly redeemers: readonly unknown[];
  } | null,
  priorStore: ReturnType<typeof makeWatcherDurableStoreV1> | null,
  nextProtocolUtxos: readonly WatcherProtocolUtxoV1[],
  blockNo: number,
  depth: number,
  previousFinalityState: unknown = null,
  pointOverride?: Readonly<{
    blockHash: string;
    parentBlockHash: string | null;
    slot: string;
    blockNo: string;
    depth: string;
  }>,
  transactionIsValid = true,
): BlockBundle => {
  serial += 1;
  const sourceStore = priorStore ?? bootstrapStore;
  const authenticatedProvider = provider;
  const selectedFinalityPolicy = finalityPolicy;
  const currentBlockNo = BigInt(pointOverride?.blockNo ?? blockNo.toString());
  const parentHash =
    [...sourceStore.chainPoints]
      .filter(
        ({ blockNo: priorBlockNo }) => BigInt(priorBlockNo) < currentBlockNo,
      )
      .sort((left, right) =>
        BigInt(left.blockNo) < BigInt(right.blockNo) ? 1 : -1,
      )
      .at(0)?.blockHash ?? null;
  const canonicalTransaction =
    transaction === null
      ? null
      : (() => {
          const body = CML.TransactionBody.from_cbor_hex(
            transaction.body.bytesHex,
          );
          const canonicalRedeemers = transaction.redeemers.map((candidate) => {
            const redeemer = candidate as MutableRecord;
            const bytes = redeemer.bytes as ReturnType<
              typeof makeWatcherL1PublicBytesV1
            >;
            return {
              ...redeemer,
              bytes: makeWatcherL1PublicBytesV1(
                CML.PlutusData.from_cbor_hex(
                  bytes.bytesHex,
                ).to_canonical_cbor_hex(),
              ),
            };
          });
          const witnessSet = CML.TransactionWitnessSet.new();
          if (canonicalRedeemers.length > 0) {
            const redeemers = CML.LegacyRedeemerList.new();
            for (const candidate of canonicalRedeemers) {
              const redeemer = candidate as MutableRecord;
              const bytes = redeemer.bytes as ReturnType<
                typeof makeWatcherL1PublicBytesV1
              >;
              redeemers.add(
                CML.LegacyRedeemer.new(
                  userEventRedeemerTag(String(redeemer.purpose)),
                  BigInt(String(redeemer.index)),
                  CML.PlutusData.from_cbor_hex(bytes.bytesHex),
                  CML.ExUnits.new(0n, 0n),
                ),
              );
            }
            witnessSet.set_redeemers(
              CML.Redeemers.new_arr_legacy_redeemer(redeemers),
            );
          }
          const fullTransaction = CML.Transaction.new(
            body,
            witnessSet,
            transactionIsValid,
            undefined,
          );
          const appliedUtxos = Array.from(
            { length: body.outputs().len() },
            (_, index) => {
              const output = body.outputs().get(index);
              const datum = output.datum()?.as_datum();
              return {
                outRef: `${transaction.txHash}#${index.toString()}`,
                outputIndex: index.toString(),
                output: makeWatcherL1PublicBytesV1(
                  output.to_canonical_cbor_hex(),
                ),
                datum:
                  datum === undefined
                    ? null
                    : {
                        datumHash: CML.hash_plutus_data(datum).to_hex(),
                        bytes: makeWatcherL1PublicBytesV1(
                          datum.to_canonical_cbor_hex(),
                        ),
                      },
                referenceScript:
                  (
                    transaction.utxos.find(
                      (candidate) =>
                        (candidate as MutableRecord).outputIndex ===
                        index.toString(),
                    ) as MutableRecord | undefined
                  )?.referenceScript ?? null,
              };
            },
          );
          return {
            ...transaction,
            transactionIndex: "0",
            fullTransaction: makeWatcherL1PublicBytesV1(
              fullTransaction.to_canonical_cbor_hex(),
            ),
            witnessSet: makeWatcherL1PublicBytesV1(
              witnessSet.to_canonical_cbor_hex(),
            ),
            utxos: transactionIsValid ? appliedUtxos : [],
            redeemers: canonicalRedeemers,
          };
        })();
  const l1Observation = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: authenticatedProvider.providerId,
    chainPoint:
      pointOverride ??
      ({
        blockHash: h32((40 + serial).toString(16).padStart(2, "0")),
        parentBlockHash: parentHash,
        slot: (1_000 + serial).toString(),
        blockNo: blockNo.toString(),
        depth: depth.toString(),
      } as const),
    transactions: canonicalTransaction === null ? [] : [canonicalTransaction],
  };
  const normalized = normalizeWatcherL1BlockV1(
    authenticatedProvider,
    l1Observation,
  );
  const finalityObservations = [
    { authenticatedProvider, l1Observation },
    {
      authenticatedProvider: providerB,
      l1Observation: {
        ...structuredClone(l1Observation),
        providerId: providerB.providerId,
      },
    },
  ];
  const normalizedEvidence = finalityObservations.map(
    ({
      authenticatedProvider: authorityProvider,
      l1Observation: observation,
    }) => normalizeWatcherL1BlockV1(authorityProvider, observation),
  );
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    normalizedEvidence,
  );
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
      : (userEventFinalityLineageByStateDigest.get(previousStateDigest) ?? []);
  const finalityResult = evaluateWatcherFinalityV1(
    selectedFinalityPolicy,
    previousFinalityState,
    consistency,
  );
  expect([
    "observe_pending",
    "advance_pending",
    "finalize",
    "duplicate",
    "rewind_pending",
  ]).toContain(finalityResult.action);
  if (finalityResult.state !== null) {
    userEventFinalityLineageByStateDigest.set(
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
  const chainPoints = [
    ...sourceStore.chainPoints.filter(
      ({ chainPointId }) => chainPointId !== normalized.chainPoint.chainPointId,
    ),
    {
      chainPointId: normalized.chainPoint.chainPointId,
      providerId: normalized.provider.providerId,
      blockHash: normalized.chainPoint.blockHash,
      slot: normalized.chainPoint.slot,
      blockNo: normalized.chainPoint.blockNo,
      depth: normalized.chainPoint.depth,
    },
  ];
  const protocolUtxos = [
    ...new Map(
      [
        ...sourceStore.protocolUtxos.filter(
          ({ role }) =>
            !["deposit", "withdrawal", "forced_transaction"].includes(role),
        ),
        ...nextProtocolUtxos.map((utxo) => ({
          ...utxo,
          chainPointId:
            utxo.chainPointId === ""
              ? normalized.chainPoint.chainPointId
              : utxo.chainPointId,
        })),
      ].map((utxo) => [utxo.outRef, utxo]),
    ).values(),
  ];
  const protocolJournal = journalWatcherProtocolUtxoTransitionV1({
    sourceStore,
    nextChainPoints: chainPoints,
    nextProtocolUtxos: protocolUtxos,
    spentAtChainPointId: normalized.chainPoint.chainPointId,
  });
  const store = makeWatcherDurableStoreV1({
    deploymentMarker: policy.deploymentMarker,
    revision: (BigInt(sourceStore.revision) + 1n).toString(),
    records: {
      l1Observations: [
        ...sourceStore.l1Observations.filter(
          ({ observationId }) => observationId !== normalized.observationDigest,
        ),
        {
          observationId: normalized.observationDigest,
          providerId: normalized.provider.providerId,
          chainPointId: normalized.chainPoint.chainPointId,
          payload: makeWatcherDurablePayloadV1(
            encodeWatcherNormalizedL1BlockV1(normalized).toString("hex"),
          ),
        },
      ],
      chainPoints,
      ...protocolJournal,
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
  return {
    context: asWireValue({
      schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
      authenticatedProvider,
      l1Observation,
      sourceDurableStore: sourceStore,
      durableStore: store,
      deploymentAuthority,
      rollbackRestoredEventUtxos: [],
      finalityAuthority: {
        policy: selectedFinalityPolicy,
        lineage,
        previousState: previousFinalityState,
        observations: finalityObservations,
        consistency,
        result: finalityResult,
      },
      rollbackAuthority: null,
    }),
    store,
    transactionHash: transaction?.txHash ?? null,
    finalityState: finalityResult.state,
  };
};

let serial = 0;
const blockBundle = (
  eventFixtures: readonly EventFixture[],
  priorStore: ReturnType<typeof makeWatcherDurableStoreV1> | null = null,
  blockNo = 100,
  depth = 1,
  mintPurpose: "mint" | "spend" = "mint",
  mutateRedeemers?: (redeemers: MutableRecord[]) => void,
  transactionIsValid = true,
): BlockBundle => {
  let transaction:
    | {
        txHash: string;
        body: ReturnType<typeof makeWatcherL1PublicBytesV1>;
        utxos: readonly unknown[];
        scripts: readonly never[];
        datums: readonly never[];
        redeemers: readonly unknown[];
      }
    | undefined;
  if (eventFixtures.length > 0) {
    const inputs = CML.TransactionInputList.new();
    const outputs = CML.TransactionOutputList.new();
    const mint = CML.Mint.new();
    const certificates = CML.CertificateList.new();
    const utxos: unknown[] = [];
    const mintRedeemers: MutableRecord[] = [];
    const certificateRedeemers: MutableRecord[] = [];
    eventFixtures.forEach((fixture, index) => {
      inputs.add(fixture.nonceInput);
      outputs.add(fixture.output);
      mint.set(
        CML.ScriptHash.from_hex(fixture.fields.policyId),
        CML.AssetName.from_hex(fixture.assetNameHex),
        1n,
      );
      certificates.add(fixture.certificate);
      mintRedeemers.push({
        purpose: mintPurpose,
        index: index.toString(),
        bytes: makeWatcherL1PublicBytesV1(
          encodeData(
            {
              AuthenticateEvent: {
                nonce_input_index: BigInt(index),
                event_output_index: BigInt(index),
                hub_ref_input_index: 0n,
                witness_registration_redeemer_index: BigInt(
                  eventFixtures.length + index,
                ),
              },
            },
            UserEventMintRedeemer,
          ),
        ),
      });
      certificateRedeemers.push({
        purpose: "certificate",
        index: index.toString(),
        bytes: makeWatcherL1PublicBytesV1(fixture.certificateRedeemerHex),
      });
    });
    const redeemers = [...mintRedeemers, ...certificateRedeemers];
    mutateRedeemers?.(redeemers);
    const body = CML.TransactionBody.new(
      inputs,
      outputs,
      200_000n + BigInt(blockNo),
    );
    body.set_mint(mint);
    body.set_certs(certificates);
    const referenceInputs = CML.TransactionInputList.new();
    referenceInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(HUB_OUT_REF.split("#")[0]!),
        0n,
      ),
    );
    for (const outRef of new Set(
      eventFixtures.flatMap(
        ({ extraReferenceOutRefs }) => extraReferenceOutRefs,
      ),
    )) {
      const [transactionId, outputIndex] = outRef.split("#");
      referenceInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(transactionId!),
          BigInt(outputIndex!),
        ),
      );
    }
    body.set_reference_inputs(referenceInputs);
    body.set_ttl(1_000n);
    body.set_script_data_hash(USER_EVENT_SCRIPT_DATA_HASH);
    const bodyHex = body.to_canonical_cbor_hex();
    const mintPolicies = CML.TransactionBody.from_cbor_hex(bodyHex)
      .mint()!
      .keys();
    eventFixtures.forEach((fixture, eventIndex) => {
      let index = -1;
      for (
        let policyIndex = 0;
        policyIndex < mintPolicies.len();
        policyIndex += 1
      ) {
        if (
          mintPolicies.get(policyIndex).to_hex() === fixture.fields.policyId
        ) {
          index = policyIndex;
          break;
        }
      }
      if (index < 0) {
        throw new Error("missing event mint policy");
      }
      mintRedeemers[eventIndex]!.index = index.toString();
    });
    const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
    eventFixtures.forEach((fixture, index) => {
      const datum = CML.PlutusData.from_cbor_hex(fixture.datumCborHex);
      utxos.push({
        outRef: `${txHash}#${index.toString()}`,
        outputIndex: index.toString(),
        output: makeWatcherL1PublicBytesV1(fixture.outputCborHex),
        datum: {
          datumHash: CML.hash_plutus_data(datum).to_hex(),
          bytes: makeWatcherL1PublicBytesV1(fixture.datumCborHex),
        },
        referenceScript: null,
      });
    });
    transaction = {
      txHash,
      body: makeWatcherL1PublicBytesV1(bodyHex),
      utxos,
      scripts: [],
      datums: [],
      redeemers,
    };
  }
  if (transaction === undefined) {
    return contextFromTransaction(
      null,
      priorStore,
      priorStore?.protocolUtxos ?? [],
      blockNo,
      depth,
      null,
      undefined,
    );
  }
  const createdProtocolUtxos: WatcherProtocolUtxoV1[] = eventFixtures.map(
    (fixture, index) => ({
      outRef: `${transaction.txHash}#${index.toString()}`,
      role:
        fixture.kind === "forced_order" ? "forced_transaction" : fixture.kind,
      chainPointId: "",
      output: makeWatcherDurablePayloadV1(fixture.outputCborHex),
    }),
  );
  return contextFromTransaction(
    transaction,
    priorStore,
    transactionIsValid
      ? [...(priorStore?.protocolUtxos ?? []), ...createdProtocolUtxos]
      : (priorStore?.protocolUtxos ?? []),
    blockNo,
    depth,
    null,
    undefined,
    transactionIsValid,
  );
};

export type GenuineW15AuthorityFixtureSetV1 = Readonly<{
  deposit: W15AcceptedAuthorityScenarioV1;
  withdrawal: W15AcceptedAuthorityScenarioV1;
  forced: W15AcceptedAuthorityScenarioV1;
  dispose: () => Promise<void>;
}>;

export type GenuineW15AuthorityFixtureInputV1 = Readonly<{
  /** Test-only root override used to prove setup-failure lease cleanup. */
  transportFixtureRoot?: string;
  forcedPayloadOverride?: GenuineW15ForcedPayloadV1;
  /** Required when the forced proof source is non-empty. */
  forcedCanonicalNativeTxCbor?: Buffer;
  depositL2Address?: AddressData;
  withdrawalL2OutRef?: Readonly<{
    transactionId: string;
    outputIndex: bigint;
  }>;
}>;

const acceptedAuthority = (
  previousState: WatcherUserEventIndexerStateV1 | null,
  bundle: BlockBundle,
  expectedKind: WatcherUserEventKindV1,
  authorityPolicy: WatcherUserEventIndexerPolicyV1 = policy,
) => {
  const observation = deriveWatcherUserEventObservationV1(
    authorityPolicy,
    previousState,
    bundle.context,
  );
  if (observation === null)
    throw new Error("genuine W15 authority did not derive an observation");
  const result = evaluateWatcherUserEventIndexerV1(
    authorityPolicy,
    previousState,
    observation,
    bundle.context,
  );
  const context = Object.freeze({
    policy: authorityPolicy,
    previousState,
    observation,
    publicContext: bundle.context,
    transportAttestations: Object.freeze([...watcherTransportContexts]),
  });
  const parsed = parseWatcherUserEventIndexerResultV1(result, context);
  if (
    parsed === null ||
    parsed.action !== "accept" ||
    parsed.protocolDecision !== "indexed" ||
    parsed.state === null
  )
    throw new Error("genuine W15 authority was not accepted");
  const event = parsed.state.snapshot.activeEvents.find(
    (candidate) => candidate.kind === expectedKind,
  );
  if (event === undefined)
    throw new Error("genuine W15 authority has no expected event");
  const historyEntryDigests = parsed.state.history
    .filter(({ observation: entry }) =>
      [...entry.snapshot.activeEvents, ...entry.snapshot.terminalEvents].some(
        (candidate) =>
          candidate.eventId === event.eventId &&
          candidate.eventContentDigest === event.eventContentDigest &&
          candidate.datumDigest === event.datumDigest &&
          candidate.outputDigest === event.outputDigest,
      ),
    )
    .map(({ entryDigest }) => entryDigest);
  if (historyEntryDigests.length === 0)
    throw new Error("genuine W15 authority has no event history");
  return Object.freeze({
    result,
    context,
    parsed,
    observation,
    event,
    historyEntryDigests: Object.freeze(historyEntryDigests),
  });
};

const forcedReceiptFixture = (input: {
  readonly payload: GenuineW15ForcedPayloadV1;
  readonly canonicalNativeTxCbor: Buffer;
}): Readonly<{
  payload: GenuineW15ForcedPayloadV1;
  store: WatcherDurableStoreV1;
  policy: WatcherUserEventIndexerPolicyV1;
  receiptOutRef: string;
}> => {
  const receiptTransactionId = h32("f0");
  const receiptOutRef = `${receiptTransactionId}#0`;
  const txOrderId = { transactionId: h32("9c"), outputIndex: 0n };
  const terminal = deriveMidgardV1TxFieldChunks(input.canonicalNativeTxCbor).at(
    -1,
  );
  if (terminal === undefined) {
    throw new Error("non-empty forced order has no terminal field chunk");
  }
  const receiptDatum = {
    field_receipt_policy_id: applied.txOrderFieldReceiptMint!,
    tx_order_policy_id: eventFields.forcedOrder.policyId,
    tx_order_id: txOrderId,
    transaction_commitment: input.payload.transaction_commitment,
    collection_proof: {
      version: BigInt(terminal.collectionProof.version),
      field_index: BigInt(terminal.collectionProof.fieldIndex),
      item_count: BigInt(terminal.collectionProof.itemCount),
      item_index: BigInt(terminal.collectionProof.itemIndex),
      item_length: BigInt(terminal.collectionProof.itemLength),
      item_commitment: terminal.collectionProof.itemCommitment.toString("hex"),
      frontier: terminal.collectionProof.frontier.peaks.map((peak) => ({
        height: BigInt(peak.height),
        hash: peak.hash.toString("hex"),
      })),
      siblings: terminal.collectionProof.siblings.map((hash) =>
        hash.toString("hex"),
      ),
    },
    chunk_index: BigInt(terminal.proof.chunkIndex),
    field_reference: { transactionId: h32("f1"), outputIndex: 0n },
    predecessor_receipt_reference: null,
    field_encoded_size: BigInt(terminal.fieldEncodedSize),
  };
  const receiptAssetName = deriveMidgardTxFieldReceiptAssetNameV1({
    txOrderPolicyId: Buffer.from(eventFields.forcedOrder.policyId, "hex"),
    txOrderTransactionId: Buffer.from(txOrderId.transactionId, "hex"),
    txOrderOutputIndex: txOrderId.outputIndex,
    transactionCommitment: Buffer.from(
      input.payload.transaction_commitment,
      "hex",
    ),
    fieldIndex: terminal.proof.fieldIndex,
    itemIndex: terminal.proof.itemIndex,
    chunkIndex: terminal.proof.chunkIndex,
  }).toString("hex");
  const receiptAssets = CML.MultiAsset.new();
  receiptAssets.set(
    CML.ScriptHash.from_hex(applied.txOrderFieldReceiptMint!),
    CML.AssetName.from_hex(receiptAssetName),
    1n,
  );
  const receiptOutput = CML.TransactionOutput.new(
    CML.Address.from_hex(scriptAddress(applied.txOrderFieldReceiptSpend!)),
    CML.Value.new(3_000_000n, receiptAssets),
    CML.DatumOption.new_datum(
      CML.PlutusData.from_cbor_hex(Data.to(receiptDatum, TxFieldReceiptV1)),
    ),
  );
  const store = makeWatcherDurableStoreV1({
    deploymentMarker: bootstrapStore.deploymentMarker,
    revision: "0",
    records: {
      l1Observations: bootstrapStore.l1Observations,
      chainPoints: bootstrapStore.chainPoints,
      protocolUtxos: [
        ...bootstrapStore.protocolUtxos,
        {
          outRef: receiptOutRef,
          role: "proof_thread",
          chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
          output: makeWatcherDurablePayloadV1(receiptOutput.to_cbor_hex()),
        },
      ],
      daProofInputs: bootstrapStore.daProofInputs,
      reconstructedStates: bootstrapStore.reconstructedStates,
      decisions: bootstrapStore.decisions,
      faults: bootstrapStore.faults,
      submissions: bootstrapStore.submissions,
      confirmations: bootstrapStore.confirmations,
      retries: bootstrapStore.retries,
      deadlines: bootstrapStore.deadlines,
      correctionResults: bootstrapStore.correctionResults,
    },
  });
  const authorityPolicy = makeWatcherUserEventIndexerPolicyV1({
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    deposit: policy.deposit,
    withdrawal: policy.withdrawal,
    forcedOrder: policy.forcedOrder,
    bootstrapStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(store),
    ),
    deploymentTrustRootId: policy.deploymentTrustRootId,
    requiredFinalityDepth: policy.requiredFinalityDepth,
    maximumActiveHistoryEntries: policy.maximumActiveHistoryEntries,
    maximumAuditHistoryEntries: policy.maximumAuditHistoryEntries,
  });
  if (authorityPolicy === null) {
    throw new Error("forced receipt policy did not parse");
  }
  return Object.freeze({
    payload: Object.freeze({
      ...input.payload,
      terminal_receipt_reference: Object.freeze({
        transactionId: receiptTransactionId,
        outputIndex: 0n,
      }),
    }),
    store,
    policy: authorityPolicy,
    receiptOutRef,
  });
};

/** Creates accepted W15 authorities from real L1 blocks and live opaque contexts. */
export const createGenuineW15DepositWithdrawalAuthoritiesV1 = async (
  input: GenuineW15AuthorityFixtureInputV1 = {},
): Promise<GenuineW15AuthorityFixtureSetV1> => {
  const leaseOwner = await initializeOpaqueAuthorityTransports(
    input.transportFixtureRoot,
  );
  try {
    const deposit = acceptedAuthority(
      null,
      blockBundle(
        [
          makeEventFixture(
            "deposit",
            "9a",
            0,
            1_000n,
            0n,
            undefined,
            undefined,
            undefined,
            [],
            { depositL2Address: input.depositL2Address },
          ),
        ],
        null,
        100,
        1,
      ),
      "deposit",
    );
    const withdrawal = acceptedAuthority(
      null,
      blockBundle(
        [
          makeEventFixture(
            "withdrawal",
            "9b",
            0,
            1_000n,
            0n,
            undefined,
            undefined,
            undefined,
            [],
            { withdrawalL2OutRef: input.withdrawalL2OutRef },
          ),
        ],
        null,
        101,
        1,
      ),
      "withdrawal",
    );
    const forcedReceipt =
      input.forcedPayloadOverride === undefined ||
      input.forcedCanonicalNativeTxCbor === undefined
        ? null
        : forcedReceiptFixture({
            payload: input.forcedPayloadOverride,
            canonicalNativeTxCbor: input.forcedCanonicalNativeTxCbor,
          });
    const forcedPolicy = forcedReceipt?.policy ?? policy;
    const forcedCreation = blockBundle(
      [
        makeEventFixture(
          "forced_order",
          "9c",
          0,
          1_000n,
          0n,
          undefined,
          undefined,
          forcedReceipt?.payload ?? input.forcedPayloadOverride,
          forcedReceipt === null ? [] : [forcedReceipt.receiptOutRef],
        ),
      ],
      forcedReceipt?.store ?? null,
      100,
      1,
    );
    const forcedActive = acceptedAuthority(
      null,
      forcedCreation,
      "forced_order",
      forcedPolicy,
    );
    const forcedTerminalBundle = nonDepositSpendBundle(
      forcedActive.parsed.state!,
      forcedCreation.store,
    );
    const forced = replayGenuineForcedTerminalAuthorityScenarioV1({
      policy: forcedPolicy,
      previousState: forcedActive.parsed.state!,
      publicContext: forcedTerminalBundle.context,
      transportAttestations: Object.freeze([...watcherTransportContexts]),
    });
    return Object.freeze({
      deposit,
      withdrawal,
      forced,
      dispose: () => disposeOpaqueAuthorityTransports(leaseOwner),
    });
  } catch (error) {
    await disposeOpaqueAuthorityTransports(leaseOwner);
    throw error;
  }
};

export type W15AuthorityScenarioInputV1 = Readonly<{
  policy: WatcherUserEventIndexerPolicyV1;
  previousState: WatcherUserEventIndexerStateV1 | null;
  publicContext: WatcherUserEventPublicContextV1;
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
}>;

export type W15AcceptedAuthorityScenarioV1 = Readonly<{
  result: ReturnType<typeof evaluateWatcherUserEventIndexerV1Raw>;
  context: Readonly<{
    policy: WatcherUserEventIndexerPolicyV1;
    previousState: WatcherUserEventIndexerStateV1 | null;
    observation: NonNullable<
      ReturnType<typeof deriveWatcherUserEventObservationV1Raw>
    >;
    publicContext: WatcherUserEventPublicContextV1;
    transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }>;
  parsed: ReturnType<typeof evaluateWatcherUserEventIndexerV1Raw>;
  observation: NonNullable<
    ReturnType<typeof deriveWatcherUserEventObservationV1Raw>
  >;
  event: WatcherIndexedUserEventV1 | WatcherTerminalUserEventV1;
  historyEntryDigests: readonly string[];
}>;

export const replayAcceptedW15AuthorityScenarioV1 = (
  input: W15AuthorityScenarioInputV1,
  expectedKind: WatcherUserEventKindV1,
  expectedLocation: "active" | "processed",
): W15AcceptedAuthorityScenarioV1 => {
  const observation = deriveWatcherUserEventObservationV1Raw(
    input.policy,
    input.previousState,
    input.publicContext,
    input.transportAttestations,
  );
  if (observation === null)
    throw new Error("W15 authority scenario did not derive an observation");
  const result = evaluateWatcherUserEventIndexerV1Raw(
    input.policy,
    input.previousState,
    observation,
    input.publicContext,
    input.transportAttestations,
  );
  const context = Object.freeze({ ...input, observation });
  const parsed = parseWatcherUserEventIndexerResultV1Raw(result, context);
  if (
    parsed === null ||
    parsed.action !== "accept" ||
    parsed.protocolDecision !== "indexed" ||
    parsed.state === null
  )
    throw new Error("W15 authority scenario was not parser-accepted");
  const candidates =
    expectedLocation === "active"
      ? parsed.state.snapshot.activeEvents
      : parsed.state.snapshot.terminalEvents.filter(
          (event) => event.terminalStatus === "processed",
        );
  const event = candidates.find((candidate) => candidate.kind === expectedKind);
  if (event === undefined)
    throw new Error("W15 authority scenario has no expected event");
  const historyEntryDigests = parsed.state.history
    .filter(({ observation: entry }) =>
      [...entry.snapshot.activeEvents, ...entry.snapshot.terminalEvents].some(
        (candidate) =>
          candidate.eventId === event.eventId &&
          candidate.eventContentDigest === event.eventContentDigest &&
          candidate.datumDigest === event.datumDigest &&
          candidate.outputDigest === event.outputDigest,
      ),
    )
    .map(({ entryDigest }) => entryDigest);
  if (historyEntryDigests.length === 0)
    throw new Error("W15 authority scenario has no digest-bound event history");
  return Object.freeze({
    result,
    context,
    parsed,
    observation,
    event,
    historyEntryDigests: Object.freeze(historyEntryDigests),
  });
};

export const replayGenuineDepositAuthorityScenarioV1 = (
  input: W15AuthorityScenarioInputV1,
): W15AcceptedAuthorityScenarioV1 =>
  replayAcceptedW15AuthorityScenarioV1(input, "deposit", "active");
export const replayGenuineWithdrawalAuthorityScenarioV1 = (
  input: W15AuthorityScenarioInputV1,
): W15AcceptedAuthorityScenarioV1 =>
  replayAcceptedW15AuthorityScenarioV1(input, "withdrawal", "active");
export const replayGenuineForcedTerminalAuthorityScenarioV1 = (
  input: W15AuthorityScenarioInputV1,
): W15AcceptedAuthorityScenarioV1 =>
  replayAcceptedW15AuthorityScenarioV1(input, "forced_order", "processed");

const nonDepositSpendBundle = (
  state: WatcherUserEventIndexerStateV1,
  priorStore: ReturnType<typeof makeWatcherDurableStoreV1>,
  mutateRedeemers?: (redeemers: MutableRecord[]) => void,
): BlockBundle => {
  const event = state.snapshot.activeEvents[0]!;
  if (event.kind === "deposit") {
    throw new Error(
      "non-deposit terminal fixture requires a non-deposit event",
    );
  }
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(event.transactionHash),
      BigInt(event.outputIndex),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(event.policyId),
    CML.AssetName.from_hex(event.assetNameHex),
    -1n,
  );
  if (event.kind === "withdrawal") {
    const payoutAssets = CML.MultiAsset.new();
    payoutAssets.set(
      CML.ScriptHash.from_hex(applied.payoutMint!),
      CML.AssetName.from_hex(event.assetNameHex),
      1n,
    );
    mint.set(
      CML.ScriptHash.from_hex(applied.payoutMint!),
      CML.AssetName.from_hex(event.assetNameHex),
      1n,
    );
    const withdrawalDatum = Data.from(
      event.datumCborHex,
      WithdrawalOrderDatum,
    ) as {
      event: {
        info: {
          body: {
            l2_value: unknown;
            l1_address: unknown;
            l1_datum: unknown;
          };
        };
      };
    };
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(scriptAddress(applied.payoutSpend!)),
        CML.Value.new(3_000_000n, payoutAssets),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            Data.to(
              {
                l2_value: withdrawalDatum.event.info.body.l2_value,
                l1_address: withdrawalDatum.event.info.body.l1_address,
                l1_datum: withdrawalDatum.event.info.body.l1_datum,
              } as never,
              PayoutDatum,
            ),
          ),
        ),
      ),
    );
  } else {
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(`60${h28("88")}`),
        CML.Value.from_coin(3_000_000n),
      ),
    );
  }
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_unreg_cert(
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(event.witnessScriptHash),
      ),
      0n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_mint(mint);
  body.set_certs(certificates);
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32("a0")), 0n),
  );
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(h32("a3")), 0n),
  );
  if (event.kind === "forced_order") {
    const forcedDatum = Data.from(event.datumCborHex, TxOrderDatumV1) as {
      readonly event: {
        readonly tx: {
          readonly terminal_receipt_reference: Readonly<{
            transactionId: string;
            outputIndex: bigint;
          }> | null;
        };
      };
    };
    const terminalReceipt = forcedDatum.event.tx.terminal_receipt_reference;
    if (terminalReceipt !== null) {
      referenceInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(terminalReceipt.transactionId),
          terminalReceipt.outputIndex,
        ),
      );
    }
  }
  body.set_reference_inputs(referenceInputs);
  body.set_script_data_hash(USER_EVENT_SCRIPT_DATA_HASH);
  const bodyHex = body.to_canonical_cbor_hex();
  const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
  const eventFieldsData = CML.PlutusData.from_cbor_hex(event.eventCborHex)
    .as_constr_plutus_data()!
    .fields();
  const datum = Data.from(
    event.datumCborHex,
    event.kind === "withdrawal" ? WithdrawalOrderDatum : TxOrderDatumV1,
  ) as {
    event: {
      tx?: {
        tx_id: string;
        transaction_commitment: string;
        source: typeof emptyNativePayload.source;
      };
    };
  };
  const validity = "TxIsValid" as const;
  const value =
    event.kind === "withdrawal"
      ? eventFieldsData.get(1).to_cbor_hex()
      : Data.to(
          {
            tx_id: datum.event.tx!.tx_id,
            transaction_commitment: datum.event.tx!.transaction_commitment,
            source: datum.event.tx!.source,
            operator_validity: validity,
          },
          ForcedInclusionTxV1,
        );
  const domain =
    event.kind === "withdrawal"
      ? "WithdrawalsRootDomain"
      : "ForcedTransactionsV1RootDomain";
  const rawProof = {
    domain,
    root:
      event.kind === "withdrawal"
        ? settlementDatum.withdrawals_root
        : settlementDatum.forced_transactions_root,
    phas_root: MEMBERSHIP_PHAS_ROOT,
    count: 1n,
    key: eventFieldsData.get(0).to_cbor_hex(),
    value,
    proof: [],
  };
  const canonicalMint = CML.TransactionBody.from_cbor_hex(bodyHex).mint()!;
  const eventMintPolicyIndex = [
    ...Array(canonicalMint.keys().len()).keys(),
  ].find(
    (index) => canonicalMint.keys().get(index).to_hex() === event.policyId,
  )!;
  const payoutMintPolicyIndex =
    event.kind === "withdrawal"
      ? [...Array(canonicalMint.keys().len()).keys()].find(
          (index) =>
            canonicalMint.keys().get(index).to_hex() === applied.payoutMint,
        )!
      : -1;
  const mintIndices = [eventMintPolicyIndex, payoutMintPolicyIndex]
    .filter((index) => index >= 0)
    .sort((left, right) => left - right);
  const eventBurnGlobalIndex = 1 + mintIndices.indexOf(eventMintPolicyIndex);
  const payoutMintGlobalIndex =
    event.kind === "withdrawal"
      ? 1 + mintIndices.indexOf(payoutMintPolicyIndex)
      : -1;
  const certificateGlobalIndex = 1 + mintIndices.length;
  const membershipGlobalIndex = certificateGlobalIndex + 1;
  const spendRedeemer =
    event.kind === "withdrawal"
      ? encodeData(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            settlement_ref_input_index: 1n,
            burn_redeemer_index: BigInt(eventBurnGlobalIndex),
            payout_mint_redeemer_index: BigInt(payoutMintGlobalIndex),
            membership_proof: rawProof,
            inclusion_proof_script_withdraw_redeemer_index: BigInt(
              membershipGlobalIndex,
            ),
            purpose: "InitializePayout",
          },
          WithdrawalSpendRedeemer,
        )
      : encodeData(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            settlement_ref_input_index: 1n,
            burn_redeemer_index: BigInt(eventBurnGlobalIndex),
            membership_proof: rawProof,
            inclusion_proof_script_withdraw_redeemer_index: BigInt(
              membershipGlobalIndex,
            ),
            validity_override: validity,
          },
          TxOrderSpendRedeemerV1,
        );
  const burnRedeemer = encodeData(
    {
      BurnEventNFT: {
        nonce_asset_name: event.assetNameHex,
        witness_unregistration_redeemer_index: BigInt(certificateGlobalIndex),
      },
    },
    UserEventMintRedeemer,
  );
  const certificateRedeemer = encodeData(
    { MintOrBurn: { targetPolicy: event.policyId } },
    UserEventWitnessPublishRedeemer,
  );
  const membershipItems = CML.PlutusDataList.new();
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(
      Data.to(MEMBERSHIP_PHAS_ROOT as never, MerkleRoot as never),
    ),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(rawProof.key, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(rawProof.value, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(Data.to([] as never, Proof as never)),
  );
  const redeemers: MutableRecord[] = [
    {
      purpose: "spend",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(spendRedeemer),
    },
    {
      purpose: "mint",
      index: eventMintPolicyIndex.toString(),
      bytes: makeWatcherL1PublicBytesV1(burnRedeemer),
    },
  ];
  if (event.kind === "withdrawal") {
    redeemers.push({
      purpose: "mint",
      index: payoutMintPolicyIndex.toString(),
      bytes: makeWatcherL1PublicBytesV1(
        encodeData(
          {
            MintPayout: {
              withdrawal_utxo_out_ref: {
                transactionId: event.transactionHash,
                outputIndex: BigInt(event.outputIndex),
              },
              withdrawal_input_index: 0n,
              withdrawal_spend_redeemer_index: 0n,
              hub_ref_input_index: 0n,
            },
          },
          PayoutMintRedeemer,
        ),
      ),
    });
  }
  redeemers.push(
    {
      purpose: "certificate",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(certificateRedeemer),
    },
    {
      purpose: "withdrawal",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(
        CML.PlutusData.new_list(membershipItems).to_cbor_hex(),
      ),
    },
  );
  mutateRedeemers?.(redeemers);
  return contextFromTransaction(
    {
      txHash,
      body: makeWatcherL1PublicBytesV1(bodyHex),
      utxos: [],
      scripts: [],
      datums: [],
      redeemers,
    },
    priorStore,
    [],
    101,
    1,
  );
};

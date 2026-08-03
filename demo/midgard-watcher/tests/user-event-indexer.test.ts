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
  DepositDatum,
  DepositSpendRedeemer,
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
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { blake2b } from "../../midgard-core/node_modules/@noble/hashes/blake2.js";
import { encodeCbor } from "../../midgard-core/src/codec/cbor.js";
import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  type MidgardNativeTxCanonicalV1,
} from "../../midgard-core/src/codec/native.js";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "../../midgard-core/src/codec/native-constants.js";
import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "../../midgard-core/src/consensus-profile-v1.js";
import {
  deriveMidgardTxFieldReceiptAssetNameV1,
  deriveMidgardV1TxFieldChunks,
} from "../../midgard-core/src/consensus-validation-v1.js";
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
  parseWatcherDurableStoreV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
  type WatcherProtocolUtxoV1,
} from "../src/durable-store.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
} from "../src/finality-engine.js";
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
} from "../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw } from "../src/multi-provider-consistency.js";
import {
  evaluateWatcherPostFinalityRecoveryV1 as evaluateWatcherPostFinalityRecoveryV1Raw,
  evaluateWatcherRollbackV1 as evaluateWatcherRollbackV1Raw,
  makeWatcherRollbackBootstrapStateV1,
  type WatcherPostFinalityRecoveryInputV1,
} from "../src/rollback-engine.js";
import {
  deriveWatcherUserEventObservationV1 as deriveWatcherUserEventObservationV1Raw,
  evaluateWatcherUserEventIndexerV1 as evaluateWatcherUserEventIndexerV1Raw,
  makeWatcherUserEventIndexerPolicyV1,
  parseWatcherUserEventIndexerResultV1 as parseWatcherUserEventIndexerResultV1Raw,
  parseWatcherUserEventIndexerStateV1 as parseWatcherUserEventIndexerStateV1Raw,
  WATCHER_USER_EVENT_INDEXER_V1_BOUNDS,
  WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  type WatcherUserEventIndexerPolicyV1,
  type WatcherUserEventIndexerStateV1,
  type WatcherUserEventKindV1,
  type WatcherUserEventPublicContextV1,
} from "../src/user-event-indexer.js";
import { canonicalFraudProofCatalogueFixture } from "./canonical-fraud-proof-catalogue.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const asWireValue = <T>(value: T): T => JSON.parse(JSON.stringify(value)) as T;
const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const canonicalJsonForTest = (value: unknown): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value) as string;
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error("unsupported test number");
    }
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJsonForTest).join(",")}]`;
  }
  if (typeof value === "object" && value !== null) {
    const record = value as Record<string, unknown>;
    return `{${Object.keys(record)
      .sort()
      .map(
        (key) => `${JSON.stringify(key)}:${canonicalJsonForTest(record[key])}`,
      )
      .join(",")}}`;
  }
  throw new Error("unsupported test value");
};
const sha256Canonical = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJsonForTest(value), "utf8")
    .digest("hex");
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
const adjacentConstructor = (cborHex: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cborHex).as_constr_plutus_data()!;
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(
      constructor.alternative() + 1n,
      constructor.fields(),
    ),
  ).to_cbor_hex();
};
const truncatedConstructor = (cborHex: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cborHex).as_constr_plutus_data()!;
  const fields = constructor.fields();
  const truncated = CML.PlutusDataList.new();
  for (let index = 0; index + 1 < fields.len(); index += 1) {
    truncated.add(fields.get(index));
  }
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(constructor.alternative(), truncated),
  ).to_cbor_hex();
};
const truncatedList = (cborHex: string): string => {
  const fields = CML.PlutusData.from_cbor_hex(cborHex).as_list()!;
  const truncated = CML.PlutusDataList.new();
  for (let index = 0; index + 1 < fields.len(); index += 1) {
    truncated.add(fields.get(index));
  }
  return CML.PlutusData.new_list(truncated).to_cbor_hex();
};
const scriptAddress = (scriptHash: string): string => `70${scriptHash}`;

type MutableRecord = Record<string, any>;
const transportEndpointByProviderId = new Map<string, string>();
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
  ) as MutableRecord;
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
  const manifest: MutableRecord = {
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
  const signedIdentity: MutableRecord = {
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
const deploymentAuthority = {
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
const nonEmptyNativeCanonical: MidgardNativeTxCanonicalV1 = {
  ...emptyNativeTxCanonical,
  body: {
    ...emptyNativeTxCanonical.body,
    requiredSignersPreimageCbor: encodeCbor([Buffer.alloc(28, 0x77)]),
  },
};
const nonEmptyNativeTx = materializeMidgardNativeTxFromCanonicalV1(
  nonEmptyNativeCanonical,
);
const nonEmptyNativeSource =
  deriveMidgardNativeTxProofSourceV1(nonEmptyNativeTx);
const NON_EMPTY_NATIVE_TX_CBOR =
  encodeMidgardNativeTxCanonicalV1(nonEmptyNativeTx);
const nonEmptyNativePayload = {
  tx_id: computeMidgardNativeTxIdV1(nonEmptyNativeTx).toString("hex"),
  transaction_commitment:
    computeMidgardNativeTxProofCommitmentV1(nonEmptyNativeSource).toString(
      "hex",
    ),
  source: {
    compact_cbor: nonEmptyNativeSource.compactCbor.toString("hex"),
    witness_set_compact_cbor:
      nonEmptyNativeSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      nonEmptyNativeSource.fieldPreimageLengthsCbor.toString("hex"),
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
              "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
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
const execFileAsync = promisify(execFile);
const watcherTransportContexts: WatcherL1TransportAttestationContextV1[] = [];
const normalizedTransportContexts = new WeakMap<
  object,
  WatcherL1TransportAttestationContextV1
>();
const watcherTransportServers: Server[] = [];
let watcherTransportFixtureDirectory = "";

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

beforeAll(async () => {
  watcherTransportFixtureDirectory = await mkdtemp(
    join("/dev/shm", "midgard-w17-transports-"),
  );
  const externalTransports = await Promise.all(
    [
      ["provider-a", h32("97")],
      ["provider-b", h32("98")],
    ].map(async ([providerId, operatorIdentitySha256]) => {
      const fixture = await makeTlsTransportFixture(providerId!);
      const endpoint = `https://localhost:${fixture.port}`;
      transportEndpointByProviderId.set(providerId!, endpoint);
      const configuredProvider = externalSource.providers.find(
        ({ providerId: configuredProviderId }) =>
          configuredProviderId === providerId,
      );
      if (configuredProvider === undefined) {
        throw new Error("missing external-provider fixture policy");
      }
      (configuredProvider as MutableRecord).endpoint = endpoint;
      return await establishWatcherExternalProviderTransportV1({
        network: "Preprod",
        providerId: providerId!,
        operatorIdentitySha256: operatorIdentitySha256!,
        endpoint,
        caPem: fixture.certificate,
        expectedTlsPublicIdentitySha256: fixture.identitySha256,
        connectTimeoutMs: 2_000,
      });
    }),
  );
  watcherTransportContexts.push(...externalTransports);
  finalityPolicy = makeExternalFinalityPolicy();
  provider = watcherL1TransportAttestationDetailsV1(
    externalTransports[0],
  )!.provider;
  providerB = watcherL1TransportAttestationDetailsV1(
    externalTransports[1],
  )!.provider;
});

afterAll(async () => {
  for (const context of watcherTransportContexts) {
    closeWatcherL1TransportAttestationContextV1(context);
  }
  for (const server of watcherTransportServers) {
    server.close();
  }
  await rm(watcherTransportFixtureDirectory, {
    recursive: true,
    force: true,
  });
});

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

const parseWatcherUserEventIndexerStateV1 = (
  value: unknown,
  policyInput: unknown,
) =>
  parseWatcherUserEventIndexerStateV1Raw(
    value,
    policyInput,
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

const evaluateWatcherRollbackV1 = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointAuthorityInput: unknown = undefined,
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
    watcherTransportContexts,
  );

const evaluateWatcherPostFinalityRecoveryV1 = (
  input: WatcherPostFinalityRecoveryInputV1,
) =>
  evaluateWatcherPostFinalityRecoveryV1Raw({
    ...input,
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

const makeEventFixture = (
  kind: WatcherUserEventKindV1,
  nonceByte: string,
  nonceIndex: number,
  ttl: bigint,
  inclusionTimeDelta = 0n,
  addressOverride?: string,
  witnessOverride?: string,
  forcedPayloadOverride?: Readonly<{
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
  }>,
  extraReferenceOutRefs: readonly string[] = [],
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
              l2_address: addressData,
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
                  l2_outref: eventId,
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

const depositSpendBundle = (
  state: WatcherUserEventIndexerStateV1,
  priorStore: ReturnType<typeof makeWatcherDurableStoreV1>,
  mutateBurn = false,
  mutateRedeemers?: (redeemers: MutableRecord[]) => void,
): BlockBundle => {
  const event = state.snapshot.activeEvents[0]!;
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(event.transactionHash),
      BigInt(event.outputIndex),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(scriptAddress(applied.reserveSpend!)),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(event.policyId),
    CML.AssetName.from_hex(event.assetNameHex),
    mutateBurn ? -2n : -1n,
  );
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
  body.set_reference_inputs(referenceInputs);
  body.set_script_data_hash(USER_EVENT_SCRIPT_DATA_HASH);
  const bodyHex = body.to_canonical_cbor_hex();
  const txHash = computeHash32(Buffer.from(bodyHex, "hex")).toString("hex");
  const eventData = CML.PlutusData.from_cbor_hex(event.eventCborHex)
    .as_constr_plutus_data()!
    .fields();
  const rawProof = {
    domain: "DepositsRootDomain",
    root: settlementDatum.deposits_root,
    phas_root: MEMBERSHIP_PHAS_ROOT,
    count: 1n,
    key: eventData.get(0).to_cbor_hex(),
    value: eventData.get(1).to_cbor_hex(),
    proof: [],
  };
  const spendRedeemer = encodeData(
    {
      input_index: 0n,
      output_index: 0n,
      hub_ref_input_index: 0n,
      settlement_ref_input_index: 1n,
      mint_redeemer_index: 1n,
      membership_proof: rawProof,
      inclusion_proof_script_withdraw_redeemer_index: 3n,
    },
    DepositSpendRedeemer,
  );
  const burnRedeemer = encodeData(
    {
      BurnEventNFT: {
        nonce_asset_name: event.assetNameHex,
        witness_unregistration_redeemer_index: 2n,
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
  const membershipRedeemer =
    CML.PlutusData.new_list(membershipItems).to_cbor_hex();
  const redeemers: MutableRecord[] = [
    {
      purpose: "spend",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(spendRedeemer),
    },
    {
      purpose: "mint",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(burnRedeemer),
    },
    {
      purpose: "certificate",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(certificateRedeemer),
    },
    {
      purpose: "withdrawal",
      index: "0",
      bytes: makeWatcherL1PublicBytesV1(membershipRedeemer),
    },
  ];
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

type RollbackSourceRecordOverrides = Partial<
  Pick<
    WatcherDurableStoreV1,
    "protocolUtxos" | "spentProtocolUtxos" | "daProofInputs"
  >
>;

const rebuildRollbackSourceStore = (
  sourceStore: WatcherDurableStoreV1,
  overrides: RollbackSourceRecordOverrides,
  revisionOverride?: string,
): WatcherDurableStoreV1 =>
  makeWatcherDurableStoreV1({
    deploymentMarker: sourceStore.deploymentMarker,
    revision:
      revisionOverride ?? (BigInt(sourceStore.revision) + 1n).toString(),
    records: {
      l1Observations: sourceStore.l1Observations,
      chainPoints: sourceStore.chainPoints,
      protocolUtxos: overrides.protocolUtxos ?? sourceStore.protocolUtxos,
      spentProtocolUtxos:
        overrides.spentProtocolUtxos ?? sourceStore.spentProtocolUtxos,
      daProofInputs: overrides.daProofInputs ?? sourceStore.daProofInputs,
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

const rollbackBundle = (
  created: BlockBundle,
  restoredEventUtxos: readonly WatcherProtocolUtxoV1[] = [],
  sourceStoreTransform: (
    sourceStore: WatcherDurableStoreV1,
  ) => WatcherDurableStoreV1 = (sourceStore) => sourceStore,
  replacementPointOverride?: Readonly<{
    blockHash: string;
    parentBlockHash: string | null;
    slot: string;
    blockNo: string;
    depth: string;
  }>,
): Readonly<{
  context: WatcherUserEventPublicContextV1;
  applied: ReturnType<typeof evaluateWatcherRollbackV1>;
}> => {
  const oldRaw = created.context.l1Observation as Record<string, any>;
  const oldA = normalizeWatcherL1BlockV1(provider, oldRaw);
  const oldRawB = {
    ...structuredClone(oldRaw),
    providerId: "provider-b",
  };
  const oldB = normalizeWatcherL1BlockV1(providerB, oldRawB);
  const oldConsistency = evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    [oldA, oldB],
  );
  const previousFinality = evaluateWatcherFinalityV1(
    finalityPolicy,
    null,
    oldConsistency,
  ).state!;
  const replacementPoint = replacementPointOverride ?? {
    blockHash: h32("ee"),
    parentBlockHash: oldA.chainPoint.blockHash,
    slot: (BigInt(oldA.chainPoint.slot) + 1n).toString(),
    blockNo: (BigInt(oldA.chainPoint.blockNo) + 1n).toString(),
    depth: "1",
  };
  const replacementRawA = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: "provider-a",
    chainPoint: replacementPoint,
    transactions: [],
  } as const;
  const replacementRawB = {
    ...replacementRawA,
    providerId: "provider-b",
  } as const;
  const replacementA = normalizeWatcherL1BlockV1(provider, replacementRawA);
  const replacementB = normalizeWatcherL1BlockV1(providerB, replacementRawB);
  const replacementConsistency = evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    [replacementA, replacementB],
  );
  const finalityResult = evaluateWatcherFinalityV1(
    finalityPolicy,
    previousFinality,
    replacementConsistency,
  );
  expect(finalityResult.action).toBe("rewind_pending");

  const priorStore = created.store;
  const extraObservations = [oldB, replacementA, replacementB];
  const l1Observations = [
    ...priorStore.l1Observations,
    ...extraObservations.map((block) => ({
      observationId: block.observationDigest,
      providerId: block.provider.providerId,
      chainPointId: block.chainPoint.chainPointId,
      payload: makeWatcherDurablePayloadV1(
        encodeWatcherNormalizedL1BlockV1(block).toString("hex"),
      ),
    })),
  ];
  const chainPoints = [
    ...new Map(
      [
        ...priorStore.chainPoints,
        ...extraObservations.map((block) => ({
          chainPointId: block.chainPoint.chainPointId,
          providerId: block.provider.providerId,
          blockHash: block.chainPoint.blockHash,
          slot: block.chainPoint.slot,
          blockNo: block.chainPoint.blockNo,
          depth: block.chainPoint.depth,
        })),
      ].map((point) => [point.chainPointId, point]),
    ).values(),
  ];
  const sourceStore = sourceStoreTransform(
    makeWatcherDurableStoreV1({
      deploymentMarker: policy.deploymentMarker,
      revision: "20",
      records: {
        l1Observations,
        chainPoints,
        protocolUtxos: priorStore.protocolUtxos,
        spentProtocolUtxos: priorStore.spentProtocolUtxos,
        daProofInputs: priorStore.daProofInputs,
        reconstructedStates: priorStore.reconstructedStates,
        decisions: priorStore.decisions,
        faults: priorStore.faults,
        submissions: priorStore.submissions,
        confirmations: priorStore.confirmations,
        retries: priorStore.retries,
        deadlines: priorStore.deadlines,
        correctionResults: priorStore.correctionResults,
      },
    }),
  );
  const rollbackBootstrap = makeWatcherRollbackBootstrapStateV1(
    finalityPolicy,
    sourceStore,
    previousFinality,
  )!;
  const applied = evaluateWatcherRollbackV1(
    finalityPolicy,
    sourceStore,
    previousFinality,
    replacementConsistency,
    finalityResult,
    rollbackBootstrap,
    rollbackBootstrap,
  );
  expect(applied.action).toBe("apply_rewind");
  expect(applied.nextStore).not.toBeNull();
  const verificationContext = {
    policy: finalityPolicy,
    sourceStore,
    previousFinalityState: previousFinality,
    consistency: replacementConsistency,
    finalityResult,
    previousRollbackState: rollbackBootstrap,
    rollbackBootstrapState: rollbackBootstrap,
  };
  return {
    context: asWireValue({
      schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
      authenticatedProvider: null,
      l1Observation: null,
      sourceDurableStore: sourceStore,
      durableStore: applied.nextStore,
      deploymentAuthority,
      rollbackRestoredEventUtxos: restoredEventUtxos,
      finalityAuthority: null,
      rollbackAuthority: {
        result: applied,
        context: verificationContext,
      },
    }),
    applied,
  };
};

const postFinalityRecoveryEvidence = (
  rawObservation: Record<string, any>,
  depth: string,
) => {
  const chainPoint = {
    ...(structuredClone(rawObservation.chainPoint) as Record<string, unknown>),
    depth,
  };
  const primaryRaw = {
    ...structuredClone(rawObservation),
    providerId: provider.providerId,
    chainPoint,
  };
  const observations = [
    normalizeWatcherL1BlockV1(provider, primaryRaw),
    normalizeWatcherL1BlockV1(providerB, {
      ...structuredClone(primaryRaw),
      providerId: providerB.providerId,
    }),
  ];
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    externalSource,
    observations,
  );
  expect(consistency).toMatchObject({
    status: "agreed",
    sourceMode: "external_providers",
    independentProviderCount: 2,
  });
  return { observations, consistency };
};

const postFinalityUserEventRecoveryBundle = (
  commonBundle: BlockBundle,
  orphanBundle: BlockBundle,
) => {
  const selectedFinalityPolicy = finalityPolicy;
  const commonRaw = commonBundle.context.l1Observation as Record<string, any>;
  const orphanRaw = orphanBundle.context.l1Observation as Record<string, any>;
  const common = postFinalityRecoveryEvidence(commonRaw, "0");
  const orphanPending = postFinalityRecoveryEvidence(orphanRaw, "1");
  const orphanFinalized = postFinalityRecoveryEvidence(orphanRaw, "2");
  const replacementRaw = {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: provider.providerId,
    chainPoint: {
      blockHash: h32("e9"),
      parentBlockHash: common.observations[0]!.chainPoint.blockHash,
      slot: (BigInt(common.observations[0]!.chainPoint.slot) + 1n).toString(),
      blockNo: (
        BigInt(common.observations[0]!.chainPoint.blockNo) + 1n
      ).toString(),
      depth: "0",
    },
    transactions: [],
  };
  const replacement = postFinalityRecoveryEvidence(replacementRaw, "0");
  const pending = evaluateWatcherFinalityV1(
    selectedFinalityPolicy,
    null,
    orphanPending.consistency,
  );
  expect(pending.action).toBe("observe_pending");
  const finalized = evaluateWatcherFinalityV1(
    selectedFinalityPolicy,
    pending.state,
    orphanFinalized.consistency,
  );
  expect(finalized.action).toBe("finalize");
  const contradiction = evaluateWatcherFinalityV1(
    selectedFinalityPolicy,
    finalized.state,
    replacement.consistency,
  );
  expect(contradiction.action).toBe("quarantine_incident");

  const baseStore = orphanBundle.store;
  const persistedObservations = [
    ...common.observations,
    ...orphanPending.observations,
    ...orphanFinalized.observations,
    ...replacement.observations,
  ];
  const sourceStore = makeWatcherDurableStoreV1({
    deploymentMarker: baseStore.deploymentMarker,
    revision: (BigInt(baseStore.revision) + 1n).toString(),
    records: {
      l1Observations: [
        ...new Map(
          [
            ...baseStore.l1Observations,
            ...persistedObservations.map((observation) => ({
              observationId: observation.observationDigest,
              providerId: observation.provider.providerId,
              chainPointId: observation.chainPoint.chainPointId,
              payload: makeWatcherDurablePayloadV1(
                encodeWatcherNormalizedL1BlockV1(observation).toString("hex"),
              ),
            })),
          ].map((entry) => [entry.observationId, entry]),
        ).values(),
      ],
      chainPoints: [
        ...new Map(
          [
            ...baseStore.chainPoints,
            ...persistedObservations.map((observation) => ({
              chainPointId: observation.chainPoint.chainPointId,
              providerId: observation.provider.providerId,
              blockHash: observation.chainPoint.blockHash,
              slot: observation.chainPoint.slot,
              blockNo: observation.chainPoint.blockNo,
              depth: observation.chainPoint.depth,
            })),
          ].map((entry) => [entry.chainPointId, entry]),
        ).values(),
      ],
      protocolUtxos: baseStore.protocolUtxos,
      spentProtocolUtxos: baseStore.spentProtocolUtxos,
      daProofInputs: baseStore.daProofInputs,
      reconstructedStates: baseStore.reconstructedStates,
      decisions: baseStore.decisions,
      faults: baseStore.faults,
      submissions: baseStore.submissions,
      confirmations: baseStore.confirmations,
      retries: baseStore.retries,
      deadlines: baseStore.deadlines,
      correctionResults: baseStore.correctionResults,
    },
  });
  const rollbackBootstrapState = makeWatcherRollbackBootstrapStateV1(
    selectedFinalityPolicy,
    sourceStore,
    finalized.state,
  )!;
  const incident = evaluateWatcherRollbackV1(
    selectedFinalityPolicy,
    sourceStore,
    finalized.state,
    replacement.consistency,
    contradiction,
    rollbackBootstrapState,
    rollbackBootstrapState,
  );
  expect(incident.action).toBe("quarantine_incident");
  expect(incident.nextStore).not.toBeNull();
  expect(incident.rollbackState?.incident).not.toBeNull();
  const recoveryInput: WatcherPostFinalityRecoveryInputV1 = {
    policy: selectedFinalityPolicy,
    sourceStore: incident.nextStore,
    currentStore: incident.nextStore,
    quarantinedRollbackState: incident.rollbackState,
    rollbackBootstrapState,
    previousCanonicalPath: [common.consistency, orphanFinalized.consistency],
    replacementCanonicalPath: [common.consistency, replacement.consistency],
    previousRecoveryState: null,
  };
  const recovery = evaluateWatcherPostFinalityRecoveryV1(recoveryInput);
  expect(recovery).toMatchObject({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    recoveryState: {
      path: {
        commonAncestorPointDigest:
          common.observations[0]!.chainPoint.pointDigest,
        rollbackDepth: "1",
      },
      incidentLifecycle: { status: "recovered" },
    },
    resumableFinalityState: {
      phase: "unobserved",
      incident: null,
    },
  });
  const context: WatcherUserEventPublicContextV1 = {
    schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: null,
    l1Observation: null,
    sourceDurableStore: incident.nextStore,
    durableStore: recovery.nextStore,
    deploymentAuthority,
    rollbackRestoredEventUtxos: [],
    finalityAuthority: null,
    rollbackAuthority: {
      result: recovery,
      context: recoveryInput,
    },
  };
  return {
    context,
    recovery,
    recoveryInput,
    replacementObservationIds: replacement.observations.map(
      ({ observationDigest }) => observationDigest,
    ),
  };
};

const accepted = (
  previous: WatcherUserEventIndexerStateV1 | null,
  bundle: BlockBundle,
): WatcherUserEventIndexerStateV1 => {
  const observation = deriveWatcherUserEventObservationV1(
    policy,
    previous,
    bundle.context,
  );
  expect(observation).not.toBeNull();
  const indexed = evaluateWatcherUserEventIndexerV1(
    policy,
    previous,
    observation,
    bundle.context,
  );
  expect(indexed.action, JSON.stringify(indexed)).toBe("accept");
  expect(indexed.protocolDecision).toBe("indexed");
  expect(
    parseWatcherUserEventIndexerResultV1(JSON.parse(JSON.stringify(indexed)), {
      policy,
      previousState: previous,
      observation,
      publicContext: bundle.context,
    }),
  ).toEqual(indexed);
  expect(
    parseWatcherUserEventIndexerStateV1(
      JSON.parse(JSON.stringify(indexed.state)),
      policy,
    ),
  ).toEqual(indexed.state);
  return indexed.state!;
};

describe("canonical authenticated user-event indexer", () => {
  it("accepts reordered wire keys while rejecting array, mutation, unknown, and unsupported changes", () => {
    const bundle = blockBundle(
      [makeEventFixture("deposit", "9a", 0, 1_000n)],
      null,
      100,
      1,
    );
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      null,
      bundle.context,
    );
    expect(observation).not.toBeNull();
    const indexed = evaluateWatcherUserEventIndexerV1(
      policy,
      null,
      observation,
      bundle.context,
    );
    expect(indexed.action).toBe("accept");

    const context = {
      policy,
      previousState: null,
      observation,
      publicContext: bundle.context,
    };
    const reordered = reorderWireKeys(asWireValue(indexed));
    expect(parseWatcherUserEventIndexerResultV1(reordered, context)).toEqual(
      indexed,
    );
    expect(
      parseWatcherUserEventIndexerStateV1(
        (reordered as Record<string, unknown>).state,
        policy,
      ),
    ).toEqual(indexed.state);

    expect(
      parseWatcherUserEventIndexerResultV1(
        new Proxy(asWireValue(indexed), {}),
        context,
      ),
    ).toBeNull();
    const nestedProxy = asWireValue(indexed) as Record<string, unknown>;
    const nestedState = nestedProxy.state as Record<string, unknown>;
    nestedState.snapshot = new Proxy(nestedState.snapshot as object, {});
    expect(
      parseWatcherUserEventIndexerResultV1(nestedProxy, context),
    ).toBeNull();

    const arrayTampered = asWireValue(indexed) as Record<string, unknown>;
    arrayTampered.reasonCodes = [
      "duplicate_observation",
      ...(arrayTampered.reasonCodes as readonly string[]),
    ];
    expect(parseWatcherUserEventIndexerResultV1(arrayTampered, context)).toBe(
      null,
    );

    const mutated = asWireValue(indexed) as Record<string, unknown>;
    mutated.resultDigest = h32("ff");
    expect(parseWatcherUserEventIndexerResultV1(mutated, context)).toBe(null);

    const unknown = asWireValue(indexed) as Record<string, unknown>;
    unknown.unexpected = true;
    expect(parseWatcherUserEventIndexerResultV1(unknown, context)).toBe(null);

    for (const unsupportedValue of [1n, new Date(0)]) {
      const unsupported = asWireValue(indexed) as Record<string, unknown>;
      const state = unsupported.state as Record<string, unknown>;
      const history = state.history as Array<Record<string, unknown>>;
      const publicContext = history[0]!.publicContext as Record<
        string,
        unknown
      >;
      publicContext.authenticatedProvider = unsupportedValue;
      expect(parseWatcherUserEventIndexerResultV1(unsupported, context)).toBe(
        null,
      );
    }
  });

  it("rejects a proxied user-event result before canonical integrity comparison", () => {
    const bundle = blockBundle(
      [makeEventFixture("deposit", "9a", 0, 1_000n)],
      null,
      100,
      1,
    );
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      null,
      bundle.context,
    );
    expect(observation).not.toBeNull();
    const indexed = evaluateWatcherUserEventIndexerV1(
      policy,
      null,
      observation,
      bundle.context,
    );
    expect(indexed.action).toBe("accept");

    expect(
      parseWatcherUserEventIndexerResultV1(
        new Proxy(asWireValue(indexed), {}),
        {
          policy,
          previousState: null,
          observation,
          publicContext: bundle.context,
        },
      ),
    ).toBeNull();
  });

  it("requires one live, uniquely matching transport capability for every W10/W11 replay", async () => {
    const external = blockBundle(
      [makeEventFixture("deposit", "9a", 0, 1_000n)],
      null,
      100,
      1,
    );
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      null,
      external.context,
    )!;
    const providerATransport = transportForProvider(provider);
    const providerBTransport = transportForProvider(providerB);

    expect(
      deriveWatcherUserEventObservationV1Raw(
        policy,
        null,
        external.context,
        [],
      ),
    ).toBeNull();
    expect(
      evaluateWatcherUserEventIndexerV1Raw(
        policy,
        null,
        observation,
        external.context,
        [],
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["public_evidence_mismatch"],
    });
    expect(
      deriveWatcherUserEventObservationV1Raw(
        policy,
        null,
        external.context,
        structuredClone(watcherTransportContexts),
      ),
    ).toBeNull();
    expect(
      deriveWatcherUserEventObservationV1Raw(policy, null, external.context, [
        providerBTransport,
      ]),
    ).toBeNull();
    expect(
      deriveWatcherUserEventObservationV1Raw(policy, null, external.context, [
        ...watcherTransportContexts,
        providerATransport,
      ]),
    ).toBeNull();
  });

  it("indexes exact deposit, withdrawal, and forced-order bytes with NFT, datum, witness, provenance, and finality", () => {
    expect(policy).not.toBeNull();
    const fixtures: EventFixture[] = [];
    for (const [index, kind] of (
      ["deposit", "withdrawal", "forced_order"] as const
    ).entries()) {
      const fixture = makeEventFixture(
        kind,
        (0xa1 + index).toString(16),
        index,
        1_000n,
      );
      fixtures.push(fixture);
      const state = accepted(null, blockBundle([fixture], null, 100, 1));
      expect(state.snapshot.activeEvents).toHaveLength(1);
      expect(state.snapshot.activeEvents[0]).toMatchObject({
        kind,
        policyId: fixture.fields.policyId,
        assetNameHex: fixture.assetNameHex,
        witnessScriptHash: userEventWitnessScriptHash(fixture.assetNameHex),
        inclusionTime: resolveEventInclusionTime(1_000, "Preprod").toString(),
        finalityStatus: "pending",
      });
      expect(state.snapshot.activeEvents[0]!.datumCborHex).toBe(
        fixture.datumCborHex,
      );
      expect(state.snapshot.activeEvents[0]!.outputCborHex).toBe(
        fixture.outputCborHex,
      );
    }
    const globalIndexState = accepted(
      null,
      blockBundle(fixtures, null, 101, 1),
    );
    expect(
      globalIndexState.snapshot.activeEvents.map(({ kind }) => kind),
    ).toEqual(["deposit", "withdrawal", "forced_order"]);
  });

  it.each(["external_providers"] as const)(
    "rejects %s two-step competing forks and oversized W12 evidence",
    () => {
      const initial = blockBundle(
        [makeEventFixture("deposit", "d3", 0, 1_000n)],
        null,
        100,
        1,
        "mint",
        undefined,
      );
      const state = accepted(null, initial);
      const oversized = structuredClone(initial.context) as MutableRecord;
      oversized.finalityAuthority.observations = Array.from(
        { length: 17 },
        () => structuredClone(oversized.finalityAuthority.observations[0]),
      );
      expect(
        deriveWatcherUserEventObservationV1(policy, null, oversized),
      ).toBeNull();
      const cyclic = structuredClone(initial.context) as MutableRecord;
      cyclic.finalityAuthority.lineage = [cyclic.finalityAuthority];
      expect(
        deriveWatcherUserEventObservationV1(policy, null, cyclic),
      ).toBeNull();
      const oversizedSparse = structuredClone(initial.context) as MutableRecord;
      const sparseLineage: unknown[] = [];
      sparseLineage.length =
        WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceContainerEntries + 1;
      oversizedSparse.finalityAuthority.lineage = sparseLineage;
      expect(
        deriveWatcherUserEventObservationV1(policy, null, oversizedSparse),
      ).toBeNull();
      const veryWide = structuredClone(initial.context) as MutableRecord;
      veryWide.untrusted = Object.fromEntries(
        Array.from(
          {
            length:
              WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceContainerEntries + 1,
          },
          (_, index) => [`field_${index.toString()}`, "x"],
        ),
      );
      expect(
        deriveWatcherUserEventObservationV1(policy, null, veryWide),
      ).toBeNull();

      const invalid = blockBundle(
        [makeEventFixture("deposit", "d7", 0, 1_000n)],
        null,
        100,
        1,
        "mint",
        undefined,
        false,
      );
      const invalidObservation = deriveWatcherUserEventObservationV1(
        policy,
        null,
        invalid.context,
      );
      expect(invalidObservation).not.toBeNull();
      expect(
        evaluateWatcherUserEventIndexerV1(
          policy,
          null,
          invalidObservation,
          invalid.context,
        ),
      ).toMatchObject({
        action: "accept",
        state: {
          snapshot: {
            activeEvents: [],
            terminalEvents: [],
          },
        },
      });

      const fork = blockBundle(
        [makeEventFixture("deposit", "d5", 0, 1_000n)],
        initial.store,
        102,
        1,
        "mint",
        undefined,
      );
      expect(
        deriveWatcherUserEventObservationV1(policy, state, fork.context),
      ).toBeNull();
    },
  );

  it.each(["external_providers"] as const)(
    "authenticates %s ancestry across an empty intermediate block",
    () => {
      const initial = blockBundle([], null, 100, 1, "mint", undefined);
      const state = accepted(null, initial);
      const gap = contextFromTransaction(
        null,
        initial.store,
        initial.store.protocolUtxos,
        101,
        1,
        null,
        undefined,
      );
      const gapPoint = (gap.context.l1Observation as MutableRecord)
        .chainPoint as MutableRecord;
      const current = contextFromTransaction(
        null,
        initial.store,
        initial.store.protocolUtxos,
        102,
        1,
        gap.finalityState,
        {
          blockHash: h32("e7"),
          parentBlockHash: String(gapPoint.blockHash),
          slot: (BigInt(String(gapPoint.slot)) + 1n).toString(),
          blockNo: (BigInt(String(gapPoint.blockNo)) + 1n).toString(),
          depth: "1",
        },
      );
      const observation = deriveWatcherUserEventObservationV1(
        policy,
        state,
        current.context,
      );
      expect(observation).not.toBeNull();
      expect(
        evaluateWatcherUserEventIndexerV1(
          policy,
          state,
          observation,
          current.context,
        ),
      ).toMatchObject({
        action: "accept",
        reasonCodes: ["block_authenticated"],
      });
    },
  );

  it("promotes pending status only at the release-bound depth and rejects omitted topology", () => {
    const fixture = makeEventFixture("deposit", "b1", 0, 1_000n);
    const firstBundle = blockBundle([fixture], null, 100, 1);
    const pendingState = accepted(null, firstBundle);
    const firstRaw = firstBundle.context.l1Observation as {
      transactions: readonly [
        {
          txHash: string;
          body: ReturnType<typeof makeWatcherL1PublicBytesV1>;
          utxos: readonly unknown[];
          scripts: readonly never[];
          datums: readonly never[];
          redeemers: readonly unknown[];
        },
      ];
      chainPoint: {
        blockHash: string;
        parentBlockHash: string | null;
        slot: string;
        blockNo: string;
        depth: string;
      };
    };
    const finalityBundle = contextFromTransaction(
      firstRaw.transactions[0],
      firstBundle.store,
      firstBundle.store.protocolUtxos,
      100,
      2,
      firstBundle.finalityState,
      { ...firstRaw.chainPoint, depth: "2" },
    );
    const finalState = accepted(pendingState, finalityBundle);
    expect(finalState.snapshot.activeEvents[0]?.finalityStatus).toBe("final");

    const omitted = blockBundle([], null, 103, 1);
    expect(
      deriveWatcherUserEventObservationV1(policy, finalState, omitted.context),
    ).toBeNull();
  });

  it("authenticates an exact spend/burn/witness-unregistration lifecycle and rejects adjacent burn quantity", () => {
    const fixture = makeEventFixture("deposit", "b2", 0, 1_000n);
    const created = blockBundle([fixture], null, 100, 1);
    const active = accepted(null, created);
    const terminal = accepted(
      active,
      depositSpendBundle(active, created.store),
    );
    expect(terminal.snapshot.activeEvents).toEqual([]);
    expect(terminal.snapshot.terminalEvents).toMatchObject([
      {
        kind: "deposit",
        terminalStatus: "absorbed",
      },
    ]);

    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        active,
        depositSpendBundle(active, created.store, true).context,
      ),
    ).toBeNull();
  });

  it("authenticates exact withdrawal-payout and forced-order terminal semantics", () => {
    for (const [kind, terminalStatus] of [
      ["withdrawal", "payout_initialized"],
      ["forced_order", "processed"],
    ] as const) {
      const fixture = makeEventFixture(
        kind,
        kind === "withdrawal" ? "b4" : "b5",
        0,
        1_000n,
      );
      const created = blockBundle([fixture], null, 100, 1);
      const active = accepted(null, created);
      const terminal = accepted(
        active,
        nonDepositSpendBundle(active, created.store),
      );
      expect(terminal.snapshot.activeEvents).toEqual([]);
      expect(terminal.snapshot.terminalEvents).toMatchObject([
        {
          kind,
          terminalStatus,
          terminalFinalityStatus: "pending",
        },
      ]);
    }
  });

  it("rejects cyclic, aliased, and cumulatively oversized raw evidence before hashing or replay", () => {
    const fixture = makeEventFixture("deposit", "ae", 0, 1_000n);
    const bundle = blockBundle([fixture]);
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      null,
      bundle.context,
    )!;

    const cyclicObservation = asWireValue(observation) as Record<string, any>;
    cyclicObservation.snapshot.activeEvents = cyclicObservation;
    expect(
      evaluateWatcherUserEventIndexerV1(
        policy,
        null,
        cyclicObservation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_observation"],
    });

    const aliasedContext = asWireValue(bundle.context) as Record<string, any>;
    aliasedContext.durableStore = aliasedContext.sourceDurableStore;
    expect(
      deriveWatcherUserEventObservationV1(policy, null, aliasedContext),
    ).toBeNull();

    const oversizedObservation = asWireValue(observation) as Record<
      string,
      any
    >;
    oversizedObservation.blockHash = "a".repeat(5 * 1_024 * 1_024);
    oversizedObservation.pointDigest = "b".repeat(5 * 1_024 * 1_024);
    expect(
      evaluateWatcherUserEventIndexerV1(
        policy,
        null,
        oversizedObservation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_observation"],
    });

    const state = accepted(null, bundle);
    const cyclicState = JSON.parse(JSON.stringify(state)) as Record<
      string,
      any
    >;
    cyclicState.history[0].publicContext.sourceDurableStore = cyclicState;
    expect(parseWatcherUserEventIndexerStateV1(cyclicState, policy)).toBeNull();
  });

  it("requires non-empty forced-order material to close with an authenticated terminal receipt", () => {
    const missingReceipt = makeEventFixture(
      "forced_order",
      "b6",
      0,
      1_000n,
      0n,
      undefined,
      undefined,
      nonEmptyNativePayload,
    );
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        blockBundle([missingReceipt]).context,
      ),
    ).toBeNull();

    const receiptOutRef = `${h32("f0")}#0`;
    const txOrderId = {
      transactionId: h32("b7"),
      outputIndex: 0n,
    };
    const terminal = deriveMidgardV1TxFieldChunks(NON_EMPTY_NATIVE_TX_CBOR).at(
      -1,
    )!;
    const receiptDatum = {
      field_receipt_policy_id: applied.txOrderFieldReceiptMint!,
      tx_order_policy_id: eventFields.forcedOrder.policyId,
      tx_order_id: txOrderId,
      transaction_commitment: nonEmptyNativePayload.transaction_commitment,
      collection_proof: {
        version: BigInt(terminal.collectionProof.version),
        field_index: BigInt(terminal.collectionProof.fieldIndex),
        item_count: BigInt(terminal.collectionProof.itemCount),
        item_index: BigInt(terminal.collectionProof.itemIndex),
        item_length: BigInt(terminal.collectionProof.itemLength),
        item_commitment:
          terminal.collectionProof.itemCommitment.toString("hex"),
        frontier: terminal.collectionProof.frontier.peaks.map((peak) => ({
          height: BigInt(peak.height),
          hash: peak.hash.toString("hex"),
        })),
        siblings: terminal.collectionProof.siblings.map((hash) =>
          hash.toString("hex"),
        ),
      },
      chunk_index: BigInt(terminal.proof.chunkIndex),
      field_reference: {
        transactionId: h32("f1"),
        outputIndex: 0n,
      },
      predecessor_receipt_reference: null,
      field_encoded_size: BigInt(terminal.fieldEncodedSize),
    };
    const receiptAssetName = deriveMidgardTxFieldReceiptAssetNameV1({
      txOrderPolicyId: Buffer.from(eventFields.forcedOrder.policyId, "hex"),
      txOrderTransactionId: Buffer.from(txOrderId.transactionId, "hex"),
      txOrderOutputIndex: txOrderId.outputIndex,
      transactionCommitment: Buffer.from(
        nonEmptyNativePayload.transaction_commitment,
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
    const receiptBootstrap = makeWatcherDurableStoreV1({
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
    const receiptPolicy = makeWatcherUserEventIndexerPolicyV1({
      network: policy.network,
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      deploymentMarker: policy.deploymentMarker,
      deposit: policy.deposit,
      withdrawal: policy.withdrawal,
      forcedOrder: policy.forcedOrder,
      bootstrapStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(receiptBootstrap),
      ),
      deploymentTrustRootId: policy.deploymentTrustRootId,
      requiredFinalityDepth: policy.requiredFinalityDepth,
      maximumActiveHistoryEntries: policy.maximumActiveHistoryEntries,
      maximumAuditHistoryEntries: policy.maximumAuditHistoryEntries,
    })!;
    const payload = {
      ...nonEmptyNativePayload,
      terminal_receipt_reference: {
        transactionId: h32("f0"),
        outputIndex: 0n,
      },
    };
    const fixture = makeEventFixture(
      "forced_order",
      "b7",
      0,
      1_000n,
      0n,
      undefined,
      undefined,
      payload,
      [receiptOutRef],
    );
    const bundle = blockBundle([fixture], receiptBootstrap, 100, 1);
    const observation = deriveWatcherUserEventObservationV1(
      receiptPolicy,
      null,
      bundle.context,
    );
    expect(observation).not.toBeNull();
    expect(
      evaluateWatcherUserEventIndexerV1(
        receiptPolicy,
        null,
        observation,
        bundle.context,
      ),
    ).toMatchObject({
      action: "accept",
      state: {
        snapshot: {
          activeEvents: [{ kind: "forced_order" }],
        },
      },
    });
  });

  it("promotes terminal finality independently and restores terminal consumption through W13 restart/reprocessing", () => {
    const finalityFixture = makeEventFixture("deposit", "b8", 0, 1_000n);
    const finalityCreated = blockBundle([finalityFixture], null, 100, 1);
    const finalityActive = accepted(null, finalityCreated);
    const pendingBundle = depositSpendBundle(
      finalityActive,
      finalityCreated.store,
    );
    const pending = accepted(finalityActive, pendingBundle);
    expect(pending.snapshot.terminalEvents[0]?.terminalFinalityStatus).toBe(
      "pending",
    );
    const rawPending = pendingBundle.context.l1Observation as MutableRecord;
    const pendingPoint = rawPending.chainPoint as {
      blockHash: string;
      parentBlockHash: string | null;
      slot: string;
      blockNo: string;
      depth: string;
    };
    const finalityBundle = contextFromTransaction(
      rawPending.transactions[0],
      pendingBundle.store,
      pendingBundle.store.protocolUtxos,
      Number(pendingPoint.blockNo),
      2,
      pendingBundle.finalityState,
      { ...pendingPoint, depth: "2" },
    );
    const finalized = accepted(pending, finalityBundle);
    expect(finalized.snapshot.terminalEvents[0]?.terminalFinalityStatus).toBe(
      "final",
    );

    const rollbackFixture = makeEventFixture("deposit", "b9", 0, 1_000n);
    const rollbackCreated = blockBundle([rollbackFixture], null, 100, 1);
    const rollbackActive = accepted(null, rollbackCreated);
    const terminalBundle = depositSpendBundle(
      rollbackActive,
      rollbackCreated.store,
    );
    const terminalState = accepted(rollbackActive, terminalBundle);
    const restoredUtxos = rollbackCreated.store.protocolUtxos.filter(
      ({ role }) => role === "deposit",
    );
    const rollback = rollbackBundle(terminalBundle, restoredUtxos);
    const targetDigest = rollbackActive.activeEntryDigests.at(-1)!;
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      terminalState,
      rollback.context,
      targetDigest,
    );
    expect(observation).not.toBeNull();
    const rewound = evaluateWatcherUserEventIndexerV1(
      policy,
      terminalState,
      observation,
      rollback.context,
    );
    expect(rewound).toMatchObject({
      action: "accept",
      state: {
        snapshot: {
          activeEvents: [{ kind: "deposit" }],
          terminalEvents: [],
        },
      },
    });
    const restarted = parseWatcherUserEventIndexerStateV1(
      JSON.parse(JSON.stringify(rewound.state)),
      policy,
    );
    expect(restarted).toEqual(rewound.state);
    const reprocessed = accepted(
      restarted,
      depositSpendBundle(restarted!, rollback.applied.nextStore!),
    );
    expect(reprocessed.snapshot.activeEvents).toEqual([]);
    expect(reprocessed.snapshot.terminalEvents).toMatchObject([
      { terminalStatus: "absorbed" },
    ]);
  });

  it("routes same-point content replacement through authenticated rollback before replay", () => {
    const olderBlock = blockBundle([], null, 89, 1);
    const olderState = accepted(null, olderBlock);
    const created = blockBundle(
      [makeEventFixture("deposit", "b2", 0, 1_000n)],
      olderBlock.store,
      90,
      1,
    );
    const active = accepted(olderState, created);
    const oldPoint = (created.context.l1Observation as MutableRecord)
      .chainPoint as {
      blockHash: string;
      parentBlockHash: string | null;
      slot: string;
      blockNo: string;
      depth: string;
    };
    const replacement = contextFromTransaction(
      null,
      created.store,
      created.store.protocolUtxos,
      Number(oldPoint.blockNo),
      Number(oldPoint.depth),
      created.finalityState,
      oldPoint,
    );
    expect(replacement.context.finalityAuthority?.result).toMatchObject({
      action: "rewind_pending",
      protocolDecision: "rewind_required",
      reasonCodes: ["pending_content_changed"],
    });
    expect(
      deriveWatcherUserEventObservationV1(policy, active, replacement.context),
    ).toBeNull();

    const rollback = rollbackBundle(
      created,
      [],
      (sourceStore) => sourceStore,
      oldPoint,
    );
    expect(rollback.applied.action).toBe("apply_rewind");
    const rollbackObservation = deriveWatcherUserEventObservationV1(
      policy,
      active,
      rollback.context,
      olderState.activeEntryDigests.at(-1)!,
    );
    expect(rollbackObservation?.transitionKind).toBe("rollback");
    const rewound = evaluateWatcherUserEventIndexerV1(
      policy,
      active,
      rollbackObservation,
      rollback.context,
    );
    expect(rewound).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: { snapshot: { activeEvents: [] } },
    });

    const replay = contextFromTransaction(
      null,
      rollback.applied.nextStore,
      rollback.applied.nextStore!.protocolUtxos,
      Number(oldPoint.blockNo),
      Number(oldPoint.depth),
      replacement.finalityState,
      oldPoint,
    );
    const replayObservation = deriveWatcherUserEventObservationV1(
      policy,
      rewound.state,
      replay.context,
    );
    expect(replayObservation?.transitionKind).toBe("apply_block");
    const replayed = evaluateWatcherUserEventIndexerV1(
      policy,
      rewound.state,
      replayObservation,
      replay.context,
    );
    expect(replayed).toMatchObject({
      action: "accept",
      reasonCodes: ["block_authenticated"],
      state: { snapshot: { activeEvents: [] } },
    });
  });

  it("applies an exact W13 rewind, survives serialized restart, deactivates the orphan, and permits re-inclusion", () => {
    const olderIdenticalBlock = blockBundle([], null, 89, 1);
    const olderIdenticalState = accepted(null, olderIdenticalBlock);
    const bootstrapBlock = blockBundle([], olderIdenticalBlock.store, 90, 1);
    const bootstrapState = accepted(olderIdenticalState, bootstrapBlock);
    const fixture = makeEventFixture("deposit", "b3", 0, 1_000n);
    const created = blockBundle([fixture], bootstrapBlock.store, 91, 1);
    const active = accepted(bootstrapState, created);
    expect(active.snapshot.activeEvents).toHaveLength(1);

    const rollback = rollbackBundle(created);
    expect(rollback.applied.removedRecords.protocolUtxoOutRefs).toContain(
      active.snapshot.activeEvents[0]!.outRef,
    );
    const targetDigest = bootstrapState.activeEntryDigests.at(-1)!;
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        active,
        rollback.context,
        olderIdenticalState.activeEntryDigests.at(-1)!,
      ),
    ).toBeNull();
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      active,
      rollback.context,
      targetDigest,
    );
    expect(observation).not.toBeNull();
    const rewound = evaluateWatcherUserEventIndexerV1(
      policy,
      active,
      observation,
      rollback.context,
    );
    expect(rewound).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: {
        snapshot: { activeEvents: [] },
      },
    });
    const restarted = parseWatcherUserEventIndexerStateV1(
      JSON.parse(JSON.stringify(rewound.state)),
      policy,
    );
    expect(restarted).toEqual(rewound.state);
    expect(restarted!.activeEntryDigests).not.toContain(
      active.activeEntryDigests.at(-1),
    );

    const reinclusion = blockBundle(
      [fixture],
      rollback.applied.nextStore,
      91,
      1,
    );
    const reincluded = accepted(restarted, reinclusion);
    expect(reincluded.snapshot.activeEvents).toHaveLength(1);
    expect(reincluded.history.length).toBe(restarted!.history.length + 1);
  });

  it.each(["external_providers"] as const)(
    "consumes an exact %s W13 post-finality recovery, prunes the orphan lineage, and resumes idempotently",
    () => {
      const commonBundle = blockBundle([], null, 1_100, 0, "mint", undefined);
      const commonState = accepted(null, commonBundle);
      const fixture = makeEventFixture("deposit", "d9", 0, 1_000n);
      const orphanBundle = blockBundle(
        [fixture],
        commonBundle.store,
        1_101,
        1,
        "mint",
        undefined,
      );
      const orphanState = accepted(commonState, orphanBundle);
      const recovery = postFinalityUserEventRecoveryBundle(
        commonBundle,
        orphanBundle,
      );
      const orphan = orphanState.snapshot.activeEvents[0]!;
      expect(recovery.recovery.removedRecords.protocolUtxoOutRefs).toContain(
        orphan.outRef,
      );
      const targetEntryDigest = commonState.activeEntryDigests.at(-1)!;
      const observation = deriveWatcherUserEventObservationV1(
        policy,
        orphanState,
        recovery.context,
        targetEntryDigest,
      );
      expect(observation).not.toBeNull();
      const applied = evaluateWatcherUserEventIndexerV1(
        policy,
        orphanState,
        observation,
        recovery.context,
      );
      expect(applied).toMatchObject({
        action: "accept",
        protocolDecision: "indexed",
        reasonCodes: ["rollback_authenticated"],
        state: {
          snapshot: {
            activeEvents: [],
            terminalEvents: [],
            quarantined: false,
          },
        },
      });
      expect(applied.state?.history).toHaveLength(
        orphanState.history.length + 1,
      );
      expect(applied.state?.history).toContainEqual(orphanState.history.at(-1));
      expect(applied.state?.activeEntryDigests).not.toContain(
        orphanState.activeEntryDigests.at(-1),
      );
      const persistedStore = parseWatcherDurableStoreV1(
        applied.state?.history.at(-1)?.publicContext.durableStore,
      );
      expect(persistedStore.protocolUtxos).not.toContainEqual(
        expect.objectContaining({ outRef: orphan.outRef }),
      );
      for (const observationId of recovery.replacementObservationIds) {
        expect(persistedStore.l1Observations).toContainEqual(
          expect.objectContaining({ observationId }),
        );
      }

      const serializedState = JSON.parse(JSON.stringify(applied.state));
      const serializedContext =
        serializedState.history.at(-1)?.publicContext ?? null;
      expect(
        deriveWatcherUserEventObservationV1(
          policy,
          orphanState,
          serializedContext,
          targetEntryDigest,
        ),
      ).toEqual(observation);
      expect(
        evaluateWatcherUserEventIndexerV1(
          policy,
          orphanState,
          observation,
          serializedContext,
        ).state,
      ).toEqual(applied.state);
      const restarted = parseWatcherUserEventIndexerStateV1(
        serializedState,
        policy,
      );
      expect(restarted).toEqual(applied.state);
      expect(
        evaluateWatcherUserEventIndexerV1(
          policy,
          restarted,
          observation,
          recovery.context,
        ),
      ).toMatchObject({
        action: "duplicate",
        protocolDecision: "indexed",
        state: applied.state,
      });

      const resumedBundle = blockBundle(
        [],
        recovery.recovery.nextStore,
        1_102,
        1,
        "mint",
        undefined,
      );
      const resumed = accepted(restarted, resumedBundle);
      expect(resumed.snapshot.quarantined).toBe(false);
      expect(resumed.history).toHaveLength(restarted!.history.length + 1);
    },
    30_000,
  );

  it("derives a post-finality target internally and records a no-owned-change recovery", () => {
    const commonBundle = blockBundle([], null, 1_150, 0);
    const commonState = accepted(null, commonBundle);
    const orphanBundle = blockBundle(
      [makeEventFixture("deposit", "df", 0, 1_000n)],
      commonBundle.store,
      1_151,
      1,
    );
    const recovery = postFinalityUserEventRecoveryBundle(
      commonBundle,
      orphanBundle,
    );
    const derivedWithoutCallerTarget = deriveWatcherUserEventObservationV1(
      policy,
      commonState,
      recovery.context,
    );
    expect(derivedWithoutCallerTarget).not.toBeNull();
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        commonState,
        recovery.context,
        h32("ff"),
      ),
    ).toEqual(derivedWithoutCallerTarget);
    const applied = evaluateWatcherUserEventIndexerV1(
      policy,
      commonState,
      derivedWithoutCallerTarget,
      recovery.context,
    );
    expect(applied).toMatchObject({
      action: "accept",
      reasonCodes: ["rollback_authenticated"],
      state: {
        snapshot: commonState.snapshot,
        activeEntryDigests: [
          ...commonState.activeEntryDigests,
          expect.any(String),
        ],
      },
    });
    expect(applied.state?.history).toHaveLength(commonState.history.length + 1);
    expect(
      parseWatcherUserEventIndexerStateV1(
        structuredClone(applied.state),
        policy,
      ),
    ).toEqual(applied.state);
  }, 30_000);

  it("rejects forged, mismatched, wrong-target, mode-invalid, and duplicate-only post-finality recovery authorities", () => {
    const commonBundle = blockBundle([], null, 1_200, 0);
    const commonState = accepted(null, commonBundle);
    const orphanBundle = blockBundle(
      [makeEventFixture("deposit", "da", 0, 1_000n)],
      commonBundle.store,
      1_201,
      1,
    );
    const orphanState = accepted(commonState, orphanBundle);
    const recovery = postFinalityUserEventRecoveryBundle(
      commonBundle,
      orphanBundle,
    );
    const targetEntryDigest = commonState.activeEntryDigests.at(-1)!;
    const derive = (context: WatcherUserEventPublicContextV1) =>
      deriveWatcherUserEventObservationV1(
        policy,
        orphanState,
        context,
        targetEntryDigest,
      );

    const forgedContext = structuredClone(
      recovery.context,
    ) as WatcherUserEventPublicContextV1;
    const forgedResult = forgedContext.rollbackAuthority!
      .result as MutableRecord;
    forgedResult.nextStoreDigest = h32("ff");
    const { resultDigest: _resultDigest, ...forgedCanonical } = forgedResult;
    forgedResult.resultDigest = sha256Canonical(forgedCanonical);
    expect(derive(forgedContext)).toBeNull();

    const mismatchedContext = structuredClone(
      recovery.context,
    ) as WatcherUserEventPublicContextV1;
    (
      mismatchedContext.rollbackAuthority!.context as unknown as MutableRecord
    ).replacementCanonicalPath = recovery.recoveryInput.previousCanonicalPath;
    expect(derive(mismatchedContext)).toBeNull();

    const modeInvalidContext = structuredClone(
      recovery.context,
    ) as WatcherUserEventPublicContextV1;
    (
      modeInvalidContext.rollbackAuthority!.context as unknown as MutableRecord
    ).policy = {
      ...finalityPolicy,
      sourceMode: "local_node",
    };
    expect(derive(modeInvalidContext)).toBeNull();

    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        orphanState,
        recovery.context,
        orphanState.activeEntryDigests.at(-1)!,
      ),
    ).toEqual(derive(recovery.context));

    const duplicateRecovery = evaluateWatcherPostFinalityRecoveryV1({
      ...recovery.recoveryInput,
      currentStore: recovery.recovery.nextStore,
      previousRecoveryState: recovery.recovery.recoveryState,
    });
    expect(duplicateRecovery.action).toBe("duplicate_recovery");
    const duplicateOnlyContext = {
      ...recovery.context,
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
    expect(derive(duplicateOnlyContext)).toBeNull();
  }, 30_000);

  it("retains append-only foreign durable records through an authenticated rewind", () => {
    const olderBlock = blockBundle([], null, 92, 1);
    const olderState = accepted(null, olderBlock);
    const targetBlock = blockBundle([], olderBlock.store, 93, 1);
    const targetState = accepted(olderState, targetBlock);
    const fixture = makeEventFixture("deposit", "ba", 0, 1_000n);
    const created = blockBundle([fixture], targetBlock.store, 94, 1);
    const active = accepted(targetState, created);
    const sentinelDaInput = {
      inputId: h32("d4"),
      kind: "da_payload" as const,
      payload: makeWatcherDurablePayloadV1("01"),
    };
    const sentinelUnrelatedUtxo = {
      outRef: `${h32("d5")}#0`,
      role: "payout" as const,
      chainPointId: BOOTSTRAP_CHAIN_POINT_ID,
      output: makeWatcherDurablePayloadV1("02"),
    };
    const rollback = rollbackBundle(created, [], (sourceStore) =>
      rebuildRollbackSourceStore(sourceStore, {
        protocolUtxos: [...sourceStore.protocolUtxos, sentinelUnrelatedUtxo],
        daProofInputs: [...sourceStore.daProofInputs, sentinelDaInput],
      }),
    );
    expect(rollback.applied.nextStore?.daProofInputs).toContainEqual(
      sentinelDaInput,
    );
    expect(rollback.applied.nextStore?.protocolUtxos).toContainEqual(
      sentinelUnrelatedUtxo,
    );

    const observation = deriveWatcherUserEventObservationV1(
      policy,
      active,
      rollback.context,
      targetState.activeEntryDigests.at(-1)!,
    );
    expect(observation).not.toBeNull();
    const rewound = evaluateWatcherUserEventIndexerV1(
      policy,
      active,
      observation,
      rollback.context,
    );
    expect(rewound.action).toBe("accept");
    const persistedStore = parseWatcherDurableStoreV1(
      rewound.state?.history.at(-1)?.publicContext.durableStore,
    );
    expect(persistedStore.daProofInputs).toContainEqual(sentinelDaInput);
    expect(persistedStore.protocolUtxos).toContainEqual(sentinelUnrelatedUtxo);
  });

  it("rejects rollback sources that delete, mutate, or collide with anchored durable records", () => {
    const targetBlock = blockBundle([], null, 95, 1);
    const targetState = accepted(null, targetBlock);
    const fixture = makeEventFixture("deposit", "bb", 0, 1_000n);
    const created = blockBundle([fixture], targetBlock.store, 96, 1);
    const active = accepted(targetState, created);
    const targetDigest = targetState.activeEntryDigests.at(-1)!;
    const deriveHostile = (
      transform: (sourceStore: WatcherDurableStoreV1) => WatcherDurableStoreV1,
    ) => {
      const rollback = rollbackBundle(created, [], transform);
      return deriveWatcherUserEventObservationV1(
        policy,
        active,
        rollback.context,
        targetDigest,
      );
    };

    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          protocolUtxos: sourceStore.protocolUtxos.filter(
            ({ outRef }) => outRef !== HUB_OUT_REF,
          ),
        }),
      ),
    ).toBeNull();
    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          protocolUtxos: sourceStore.protocolUtxos.map((utxo) =>
            utxo.outRef === SETTLEMENT_OUT_REF
              ? {
                  ...utxo,
                  output: makeWatcherDurablePayloadV1("03"),
                }
              : utxo,
          ),
        }),
      ),
    ).toBeNull();
    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          protocolUtxos: sourceStore.protocolUtxos.map((utxo) =>
            utxo.role === "deposit"
              ? { ...utxo, role: "payout" as const }
              : utxo,
          ),
        }),
      ),
    ).toBeNull();
    expect(
      deriveHostile((sourceStore) =>
        rebuildRollbackSourceStore(
          sourceStore,
          {
            daProofInputs: [
              ...sourceStore.daProofInputs,
              {
                inputId: h32("d6"),
                kind: "da_payload" as const,
                payload: makeWatcherDurablePayloadV1("04"),
              },
            ],
          },
          created.store.revision,
        ),
      ),
    ).toBeNull();

    const spentCreated = blockBundle([
      makeEventFixture("deposit", "bc", 0, 1_000n),
    ]);
    const spentActive = accepted(null, spentCreated);
    const terminalBundle = depositSpendBundle(spentActive, spentCreated.store);
    const terminal = accepted(spentActive, terminalBundle);
    const restoredEventUtxos = spentCreated.store.protocolUtxos.filter(
      ({ role }) => role === "deposit",
    );
    const removedSpentEvent = rollbackBundle(
      terminalBundle,
      restoredEventUtxos,
      (sourceStore) =>
        rebuildRollbackSourceStore(sourceStore, {
          spentProtocolUtxos: sourceStore.spentProtocolUtxos.filter(
            ({ role }) => role !== "deposit",
          ),
        }),
    );
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        terminal,
        removedSpentEvent.context,
        spentActive.activeEntryDigests.at(-1)!,
      ),
    ).toBeNull();
  });

  it("rejects adjacent inclusion time, malformed canonical datum, wrong network/address, policy, witness, and duplicate evidence", () => {
    const wrongTime = makeEventFixture("deposit", "c1", 0, 1_000n, 1n);
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        blockBundle([wrongTime]).context,
      ),
    ).toBeNull();
    const wrongAddress = makeEventFixture(
      "deposit",
      "c3",
      0,
      1_000n,
      0n,
      scriptAddress(h28("99")),
    );
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        blockBundle([wrongAddress]).context,
      ),
    ).toBeNull();
    const wrongWitness = makeEventFixture(
      "deposit",
      "c4",
      0,
      1_000n,
      0n,
      undefined,
      h28("9a"),
    );
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        blockBundle([wrongWitness]).context,
      ),
    ).toBeNull();

    const valid = makeEventFixture("deposit", "c2", 0, 1_000n);
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        blockBundle([valid], null, 100, 1, "spend").context,
      ),
    ).toBeNull();
    const wrongPolicy = {
      ...valid,
      fields: {
        ...valid.fields,
        policyId: h28("9b"),
      },
    };
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        blockBundle([wrongPolicy]).context,
      ),
    ).toBeNull();
    const validBundle = blockBundle([valid]);
    const state = accepted(null, validBundle);
    const observation = state.history[0]!.observation;
    expect(
      evaluateWatcherUserEventIndexerV1(
        policy,
        state,
        observation,
        validBundle.context,
      ),
    ).toMatchObject({
      action: "duplicate",
      reasonCodes: ["duplicate_observation"],
    });
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        state,
        blockBundle([valid], validBundle.store, 101, 1).context,
      ),
    ).toBeNull();

    const wrongNetworkPolicy = {
      ...policy,
      network: "Mainnet",
    };
    expect(
      deriveWatcherUserEventObservationV1(
        wrongNetworkPolicy,
        null,
        validBundle.context,
      ),
    ).toBeNull();

    const malformed = structuredClone(validBundle.context) as Record<
      string,
      any
    >;
    malformed.l1Observation.transactions[0].utxos[0].datum.bytes.bytesHex +=
      "00";
    malformed.l1Observation.transactions[0].utxos[0].datum.bytes.sha256 =
      sha256(
        Buffer.from(
          malformed.l1Observation.transactions[0].utxos[0].datum.bytes.bytesHex,
          "hex",
        ),
      );
    expect(
      deriveWatcherUserEventObservationV1(policy, null, malformed),
    ).toBeNull();

    const forgedFinality = structuredClone(validBundle.context) as Record<
      string,
      any
    >;
    forgedFinality.finalityAuthority.result.resultDigest = h32("fd");
    expect(
      deriveWatcherUserEventObservationV1(policy, null, forgedFinality),
    ).toBeNull();

    const providerDisagreement = structuredClone(validBundle.context) as Record<
      string,
      any
    >;
    const disagreementRaw = structuredClone(providerDisagreement.l1Observation);
    disagreementRaw.providerId = providerB.providerId;
    disagreementRaw.transactions = [];
    providerDisagreement.finalityAuthority.consistency =
      evaluateWatcherMultiProviderConsistencyV1(externalSource, [
        normalizeWatcherL1BlockV1(provider, providerDisagreement.l1Observation),
        normalizeWatcherL1BlockV1(providerB, disagreementRaw),
      ]);
    providerDisagreement.finalityAuthority.result = evaluateWatcherFinalityV1(
      finalityPolicy,
      providerDisagreement.finalityAuthority.previousState,
      providerDisagreement.finalityAuthority.consistency,
    );
    expect(
      deriveWatcherUserEventObservationV1(policy, null, providerDisagreement),
    ).toBeNull();
  });

  it("rejects adjacent constructor tags, arities, and global-list index substitutions across every event redeemer", () => {
    const fixture = makeEventFixture("deposit", "c5", 0, 1_000n);
    for (const redeemerIndex of [0, 1]) {
      for (const mutate of [adjacentConstructor, truncatedConstructor]) {
        const hostile = blockBundle(
          [fixture],
          null,
          110,
          1,
          "mint",
          (redeemers) => {
            const entry = redeemers[redeemerIndex]!;
            entry.bytes = makeWatcherL1PublicBytesV1(
              mutate(entry.bytes.bytesHex),
            );
          },
        );
        expect(
          deriveWatcherUserEventObservationV1(policy, null, hostile.context),
        ).toBeNull();
      }
    }
    const wrongGlobalIndex = blockBundle(
      [fixture],
      null,
      111,
      1,
      "mint",
      (redeemers) => {
        redeemers[0]!.bytes = makeWatcherL1PublicBytesV1(
          encodeData(
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
        );
      },
    );
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        null,
        wrongGlobalIndex.context,
      ),
    ).toBeNull();

    const created = blockBundle([fixture], null, 100, 1);
    const active = accepted(null, created);
    for (const redeemerIndex of [0, 1, 2]) {
      for (const mutate of [adjacentConstructor, truncatedConstructor]) {
        const hostile = depositSpendBundle(
          active,
          created.store,
          false,
          (redeemers) => {
            const entry = redeemers[redeemerIndex]!;
            entry.bytes = makeWatcherL1PublicBytesV1(
              mutate(entry.bytes.bytesHex),
            );
          },
        );
        expect(
          deriveWatcherUserEventObservationV1(policy, active, hostile.context),
        ).toBeNull();
      }
    }
    const malformedMembership = depositSpendBundle(
      active,
      created.store,
      false,
      (redeemers) => {
        redeemers[3]!.bytes = makeWatcherL1PublicBytesV1(
          truncatedList(redeemers[3]!.bytes.bytesHex),
        );
      },
    );
    expect(
      deriveWatcherUserEventObservationV1(
        policy,
        active,
        malformedMembership.context,
      ),
    ).toBeNull();

    for (const [kind, nonce] of [
      ["withdrawal", "c7"],
      ["forced_order", "c8"],
    ] as const) {
      const nonDepositFixture = makeEventFixture(kind, nonce, 0, 1_000n);
      const nonDepositCreated = blockBundle([nonDepositFixture], null, 100, 1);
      const nonDepositActive = accepted(null, nonDepositCreated);
      const redeemerIndices = kind === "withdrawal" ? [0, 2] : [0];
      for (const redeemerIndex of redeemerIndices) {
        for (const mutate of [adjacentConstructor, truncatedConstructor]) {
          const hostile = nonDepositSpendBundle(
            nonDepositActive,
            nonDepositCreated.store,
            (redeemers) => {
              const entry = redeemers[redeemerIndex]!;
              entry.bytes = makeWatcherL1PublicBytesV1(
                mutate(entry.bytes.bytesHex),
              );
            },
          );
          expect(
            deriveWatcherUserEventObservationV1(
              policy,
              nonDepositActive,
              hostile.context,
            ),
          ).toBeNull();
        }
      }
    }
  });

  it("rejects unsigned deployment substitutions and rehashed store subset, revision, and chain-point attacks", () => {
    const fixture = makeEventFixture("deposit", "c6", 0, 1_000n);
    const initial = blockBundle([fixture], null, 100, 1);
    const forgedDeployment = structuredClone(initial.context) as MutableRecord;
    forgedDeployment.deploymentAuthority.policy.appliedScriptHashes.depositMint =
      h28("ee");
    expect(
      deriveWatcherUserEventObservationV1(policy, null, forgedDeployment),
    ).toBeNull();

    const state = accepted(null, initial);
    const successor = blockBundle([], initial.store, 121, 1);
    const source = successor.context.sourceDurableStore as ReturnType<
      typeof makeWatcherDurableStoreV1
    >;
    const next = successor.context.durableStore as ReturnType<
      typeof makeWatcherDurableStoreV1
    >;
    const subset = structuredClone(successor.context) as MutableRecord;
    subset.durableStore = makeWatcherDurableStoreV1({
      deploymentMarker: next.deploymentMarker,
      revision: next.revision,
      records: {
        ...next,
        l1Observations: next.l1Observations.slice(-1),
      },
    });
    expect(
      deriveWatcherUserEventObservationV1(policy, state, subset),
    ).toBeNull();

    const jumpedRevision = structuredClone(successor.context) as MutableRecord;
    jumpedRevision.durableStore = makeWatcherDurableStoreV1({
      deploymentMarker: next.deploymentMarker,
      revision: (BigInt(source.revision) + 2n).toString(),
      records: next,
    });
    expect(
      deriveWatcherUserEventObservationV1(policy, state, jumpedRevision),
    ).toBeNull();

    const reassignedPoint = structuredClone(successor.context) as MutableRecord;
    reassignedPoint.durableStore = makeWatcherDurableStoreV1({
      deploymentMarker: next.deploymentMarker,
      revision: next.revision,
      records: {
        ...next,
        protocolUtxos: next.protocolUtxos.map((utxo) =>
          utxo.role === "deposit"
            ? { ...utxo, chainPointId: BOOTSTRAP_CHAIN_POINT_ID }
            : utxo,
        ),
      },
    });
    expect(
      deriveWatcherUserEventObservationV1(policy, state, reassignedPoint),
    ).toBeNull();

    const terminal = depositSpendBundle(state, initial.store);
    const terminalStore = terminal.context.durableStore as ReturnType<
      typeof makeWatcherDurableStoreV1
    >;
    const omittedArchive = structuredClone(terminal.context) as MutableRecord;
    omittedArchive.durableStore = makeWatcherDurableStoreV1({
      deploymentMarker: terminalStore.deploymentMarker,
      revision: terminalStore.revision,
      records: {
        ...terminalStore,
        spentProtocolUtxos: [],
      },
    });
    expect(
      deriveWatcherUserEventObservationV1(policy, state, omittedArchive),
    ).toBeNull();

    const substitutedArchive = structuredClone(
      terminal.context,
    ) as MutableRecord;
    substitutedArchive.durableStore = makeWatcherDurableStoreV1({
      deploymentMarker: terminalStore.deploymentMarker,
      revision: terminalStore.revision,
      records: {
        ...terminalStore,
        spentProtocolUtxos: terminalStore.spentProtocolUtxos.map((entry) => ({
          ...entry,
          spentAtChainPointId: entry.chainPointId,
        })),
      },
    });
    expect(
      deriveWatcherUserEventObservationV1(policy, state, substitutedArchive),
    ).toBeNull();
  });

  it("rejects forged state/history/public results even when attackers recompute outer digests", () => {
    const fixture = makeEventFixture("deposit", "d1", 0, 1_000n);
    const bundle = blockBundle([fixture]);
    const state = accepted(null, bundle);
    const forged = structuredClone(state) as Record<string, any>;
    forged.snapshot.activeEvents[0].eventCborHex = "d87980";
    forged.snapshot.snapshotDigest = h32("ef");
    forged.stateDigest = h32("fe");
    expect(parseWatcherUserEventIndexerStateV1(forged, policy)).toBeNull();

    const successor = blockBundle([], bundle.store, 101, 1);
    const observation = deriveWatcherUserEventObservationV1(
      policy,
      state,
      successor.context,
    );
    expect(observation).not.toBeNull();
    const result = evaluateWatcherUserEventIndexerV1(
      policy,
      state,
      observation,
      blockBundle([], bundle.store, 104, 1).context,
    );
    expect(result.action).toBe("reject");
  });
});

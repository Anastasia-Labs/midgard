import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { type Server } from "node:net";
import { join } from "node:path";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  availabilityResponseGeometryV1,
  buildDaAvailabilityCommitmentV1,
  DA_ATTESTATION_ASSET_NAME_PREFIX,
  DaAttestationDatum,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  Proof,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  evaluateWatcherProofThreadIndexerV1 as evaluateWatcherProofThreadIndexerV1Raw,
  makeWatcherProofThreadJournalV1,
  makeWatcherProofThreadLayoutV1,
  makeWatcherProofThreadObservationV1,
  makeWatcherProofThreadPolicyV1,
  parseWatcherProofThreadResultV1 as parseWatcherProofThreadResultV1Raw,
  parseWatcherProofThreadStateV1 as parseWatcherProofThreadStateV1Raw,
  WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1,
  WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
  WATCHER_PROOF_THREAD_V1_BOUNDS,
  type WatcherProofThreadFamilyV1,
  type WatcherProofThreadJournalV1,
  type WatcherProofThreadObservationV1,
  type WatcherProofThreadPolicyV1,
  type WatcherProofThreadPublicContextV1,
  type WatcherProofThreadStateV1,
} from "../../src/indexers/proof-thread-indexer.js";
import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityStateV1,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  encodeWatcherNormalizedL1BlockV1,
  establishWatcherExternalProviderTransportV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1 as normalizeWatcherL1BlockV1Raw,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherAuthenticatedL1ProviderV1,
  type WatcherL1RedeemerV1,
  type WatcherL1TransportAttestationContextV1,
  watcherL1TransportAttestationDetailsV1,
  type WatcherNormalizedL1BlockV1,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 as evaluateWatcherMultiProviderConsistencyV1Raw } from "../../src/l1/multi-provider-consistency.js";
import {
  evaluateWatcherPostFinalityRecoveryV1 as evaluateWatcherPostFinalityRecoveryV1Raw,
  evaluateWatcherRollbackV1 as evaluateWatcherRollbackV1Raw,
  makeWatcherRollbackBootstrapStateV1,
  type WatcherPostFinalityRecoveryInputV1,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  makeWatcherDurablePayloadV1,
  makeWatcherDurableStoreV1,
  type WatcherDurableRecordsV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
  type WatcherProtocolUtxoV1,
} from "../../src/storage/durable-store.js";
import {
  canonicalDigest as digest,
  canonicalJson,
} from "../support/canonical-json.js";
import {
  h28,
  h32,
  makeWatcherAuthorityContractsV1,
  makeWatcherDeploymentAuthorityFixtureV1,
} from "../support/deployment-authority-fixture.js";
import { makeWatcherTlsTransportFixtureV1 } from "../support/tls-transport-fixture.js";

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
type Mutable = Record<string, any>;

const rehashProofThreadEntry = (value: Mutable): Mutable => {
  const { entryDigest: _entryDigest, ...canonical } = value;
  return { ...canonical, entryDigest: digest(canonical) };
};

const rehashProofThreadAudit = (value: Mutable): Mutable => {
  const { auditDigest: _auditDigest, ...canonical } = value;
  return { ...canonical, auditDigest: digest(canonical) };
};

const rehashProofThreadState = (value: Mutable): Mutable => {
  const { stateDigest: _stateDigest, ...canonical } = value;
  return { ...canonical, stateDigest: digest(canonical) };
};

const same = (left: unknown, right: unknown): boolean =>
  canonicalJson(left) === canonicalJson(right);

const scriptAddress = (hash: string): string =>
  CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.from(hash, "hex")]),
  ).to_hex();

const publicBytes = (hex: string) => makeWatcherL1PublicBytesV1(hex);
const canonicalData = (hex: string): string =>
  CML.PlutusData.from_cbor_hex(hex).to_canonical_cbor_hex();

const proofThreadFixtureStepHash = (
  familyId: string,
  stepIndex: number,
): string =>
  createHash("sha256")
    .update(`midgard-watcher-proof-thread-step-v1:${familyId}:${stepIndex}`)
    .digest("hex")
    .slice(0, 56);

const linearNextStepIndexes = (
  stepCount: number,
): readonly (readonly string[])[] =>
  Object.freeze(
    Array.from({ length: stepCount }, (_, index) =>
      Object.freeze(index + 1 < stepCount ? [(index + 1).toString()] : []),
    ),
  );

const makeDeploymentAuthority = () => {
  const contractSet = makeWatcherAuthorityContractsV1();
  const families: readonly WatcherProofThreadFamilyV1[] = Object.freeze(
    Object.entries(WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1)
      .map(([catalogueCategory, familyAuthority]) => {
        const category =
          contractSet.fraudProofCatalogue.categories[
            catalogueCategory as keyof typeof contractSet.fraudProofCatalogue.categories
          ];
        const stepScriptHashes = Object.freeze(
          Array.from({ length: familyAuthority.stepCount }, (_, index) => {
            const deployedContractName =
              familyAuthority.deployedStepContractNames[index];
            const deployedScriptHash =
              deployedContractName === undefined
                ? undefined
                : contractSet.contracts[deployedContractName]?.scriptHash;
            if (
              deployedContractName !== undefined &&
              deployedScriptHash === undefined
            ) {
              throw new Error(
                `proof-thread authority contract ${deployedContractName} is missing`,
              );
            }
            return (
              deployedScriptHash ??
              proofThreadFixtureStepHash(familyAuthority.familyId, index)
            );
          }),
        );
        return Object.freeze({
          familyId: familyAuthority.familyId,
          catalogueCategory,
          categoryId: category.categoryId,
          firstStepScriptHash: category.scriptHash,
          stepScriptHashes,
          nextStepIndexes:
            catalogueCategory === "transitionTrace"
              ? Object.freeze([
                  Object.freeze(
                    Array.from(
                      { length: familyAuthority.stepCount - 1 },
                      (_, index) => (index + 1).toString(),
                    ),
                  ),
                  ...Array.from({ length: familyAuthority.stepCount - 1 }, () =>
                    Object.freeze([] as string[]),
                  ),
                ])
              : linearNextStepIndexes(familyAuthority.stepCount),
        });
      })
      .sort((left, right) => left.familyId.localeCompare(right.familyId)),
  );
  const fixture = makeWatcherDeploymentAuthorityFixtureV1({
    contractSet,
    releaseDigest: RELEASE_DIGEST,
    blueprintHash: BLUEPRINT_HASH,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments: {
      "computation-thread-policy-v1": digest({
        computationThreadPolicyId: CT_POLICY,
      }),
      "proof-thread-catalogue-v1": digest(families),
      "transition-order-v1": h32("99"),
    },
  });
  return {
    deploymentAuthority: {
      signedIdentity: fixture.signedIdentity,
      policy: fixture.policy,
      trustRoots: fixture.trustRoots,
      result: fixture.result,
    },
    marker: fixture.marker,
    result: fixture.result,
    families,
    contracts: fixture.contracts,
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
      mode: "development",
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

let finalityPolicy: WatcherFinalityPolicyV1;

let providers: readonly WatcherAuthenticatedL1ProviderV1[];

const watcherTransportContexts: WatcherL1TransportAttestationContextV1[] = [];
const normalizedTransportContexts = new WeakMap<
  object,
  WatcherL1TransportAttestationContextV1
>();
const watcherTransportServers: Server[] = [];
let watcherTransportFixtureDirectory = "";

const makeTlsTransportFixture = async (name: string) =>
  await makeWatcherTlsTransportFixtureV1(
    watcherTransportFixtureDirectory,
    watcherTransportServers,
    name,
  );

beforeAll(async () => {
  watcherTransportFixtureDirectory = await mkdtemp(
    join("/dev/shm", "midgard-w16-transports-"),
  );
  const externalTransports = await Promise.all(
    [
      ["provider-a", h32("a1")],
      ["provider-b", h32("b2")],
    ].map(async ([providerId, operatorIdentitySha256]) => {
      const fixture = await makeTlsTransportFixture(providerId!);
      const endpoint = `https://localhost:${fixture.port}`;
      const configuredProvider = externalSource.providers.find(
        ({ identity }) => identity === providerId,
      );
      if (configuredProvider === undefined) {
        throw new Error("missing external-provider fixture policy");
      }
      (configuredProvider as Mutable).endpoint = endpoint;
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
  finalityPolicy = makeFinalityPolicy(externalSource);
  providers = externalTransports.map(
    (context) => watcherL1TransportAttestationDetailsV1(context)!.provider,
  );
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

const evaluateWatcherProofThreadIndexerV1 = (
  policyInput: unknown,
  previousStateInput: unknown,
  observationInput: unknown,
  publicContextInput: unknown,
) =>
  evaluateWatcherProofThreadIndexerV1Raw(
    policyInput,
    previousStateInput,
    observationInput,
    publicContextInput,
    watcherTransportContexts,
  );

const parseWatcherProofThreadStateV1 = (value: unknown, policyInput: unknown) =>
  parseWatcherProofThreadStateV1Raw(
    value,
    policyInput,
    watcherTransportContexts,
  );

const parseWatcherProofThreadResultV1 = (
  value: unknown,
  context: Omit<
    Parameters<typeof parseWatcherProofThreadResultV1Raw>[1],
    "transportAttestations"
  >,
) =>
  parseWatcherProofThreadResultV1Raw(value, {
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

type FixtureSourceMode = "external_providers";

const sourceFixture = (
  _sourceMode: FixtureSourceMode,
): Readonly<{
  policy: WatcherFinalityPolicyV1;
  providers: readonly WatcherAuthenticatedL1ProviderV1[];
  consistencyConfig: unknown;
}> => ({
  policy: finalityPolicy,
  providers,
  consistencyConfig: {
    sourceMode: "external_providers",
    network: "Preprod",
    providers: providers.map((provider) => ({
      providerId: provider.providerId,
      operatorIdentitySha256:
        provider.source.sourceMode === "external_providers"
          ? provider.source.operatorIdentitySha256
          : "",
      endpoint: finalityPolicy.externalProviders!.find(
        ({ providerId }) => providerId === provider.providerId,
      )!.endpoint,
    })),
  },
});

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

const SCRIPT_DATA_HASH = CML.ScriptDataHash.from_raw_bytes(
  Buffer.alloc(32, 0x5a),
);

const redeemerTag = (
  purpose: WatcherL1RedeemerV1["purpose"],
): CML.RedeemerTag => {
  switch (purpose) {
    case "spend":
      return CML.RedeemerTag.Spend;
    case "mint":
      return CML.RedeemerTag.Mint;
    case "certificate":
      return CML.RedeemerTag.Cert;
    case "withdrawal":
      return CML.RedeemerTag.Reward;
    case "vote":
      return CML.RedeemerTag.Voting;
    case "propose":
      return CML.RedeemerTag.Proposing;
  }
};

const witnessSetFor = (fixture: TxFixture): CML.TransactionWitnessSet => {
  const witnessSet = CML.TransactionWitnessSet.new();
  if (fixture.redeemers.length > 0) {
    const redeemers = CML.LegacyRedeemerList.new();
    for (const redeemer of fixture.redeemers) {
      redeemers.add(
        CML.LegacyRedeemer.new(
          redeemerTag(redeemer.purpose),
          BigInt(redeemer.index),
          CML.PlutusData.from_cbor_hex(redeemer.bytesHex),
          CML.ExUnits.new(0n, 0n),
        ),
      );
    }
    witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  }
  return witnessSet;
};

const initTransaction = (): TxFixture => {
  const family = policy.families.find(
    ({ familyId }) => familyId === "invalid-range",
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
  body.set_script_data_hash(SCRIPT_DATA_HASH);
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
  body.set_script_data_hash(SCRIPT_DATA_HASH);
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
  body.set_script_data_hash(SCRIPT_DATA_HASH);
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
  body.set_script_data_hash(SCRIPT_DATA_HASH);
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

const rawTransaction = (
  fixture: TxFixture,
  isValid = true,
  transactionIndex = "0",
) => {
  const body = CML.TransactionBody.from_cbor_hex(fixture.bodyHex);
  const witnessSet = witnessSetFor(fixture);
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    isValid,
    undefined,
  );
  return {
    txHash: fixture.txHash,
    transactionIndex,
    fullTransaction: publicBytes(fullTransaction.to_canonical_cbor_hex()),
    body: publicBytes(fixture.bodyHex),
    witnessSet: publicBytes(witnessSet.to_canonical_cbor_hex()),
    utxos: isValid
      ? fixture.outputs.map((output, index) => {
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
                    datumHash: computeHash32(
                      Buffer.from(datumHex, "hex"),
                    ).toString("hex"),
                    bytes: publicBytes(datumHex),
                  },
            referenceScript: null,
          };
        })
      : [],
    scripts: [],
    datums: [],
    redeemers: fixture.redeemers.map(({ purpose, index, bytesHex }) => ({
      purpose,
      index,
      bytes: publicBytes(bytesHex),
    })),
  };
};

type RawObservationOptions = Readonly<{
  empty?: boolean;
  transactionIsValid?: boolean;
  transactions?: readonly Readonly<{
    fixture: TxFixture;
    isValid?: boolean;
    transactionIndex?: string;
  }>[];
}>;

const rawObservation = (
  provider: WatcherAuthenticatedL1ProviderV1,
  fixture: TxFixture,
  depth: string,
  ordinal = 0,
  options: RawObservationOptions = {},
) => ({
  schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: provider.providerId,
  chainPoint: {
    blockHash: h32((0x40 + ordinal).toString(16).padStart(2, "0")),
    parentBlockHash:
      ordinal === 0
        ? null
        : h32((0x40 + ordinal - 1).toString(16).padStart(2, "0")),
    slot: (1000 + ordinal).toString(),
    blockNo: (100 + ordinal).toString(),
    depth,
  },
  transactions:
    options.transactions !== undefined
      ? options.transactions.map((candidate, index) =>
          rawTransaction(
            candidate.fixture,
            candidate.isValid,
            candidate.transactionIndex ?? index.toString(),
          ),
        )
      : options.empty
        ? []
        : [rawTransaction(fixture, options.transactionIsValid)],
});

const normalize = (
  provider: WatcherAuthenticatedL1ProviderV1,
  fixture: TxFixture,
  depth: string,
  ordinal = 0,
  options: RawObservationOptions = {},
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(
    provider,
    rawObservation(provider, fixture, depth, ordinal, options),
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
          chainPointId: priorPoint.chainPointId,
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
          familyId: "invalid-range",
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
    familyId: "invalid-range",
    fraudulentBlockHash: FRAUD_BLOCK,
    fraudulentHeaderHash: FRAUD_HEADER,
    fraudProver: PROVER,
    computationThreadAssetName: `${
      policy.families.find(({ familyId }) => familyId === "invalid-range")!
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

type ProofThreadFinalityLineage = NonNullable<
  WatcherProofThreadPublicContextV1["finalityAuthority"]
>["lineage"];

const proofThreadFinalityLineageByStateDigest = new Map<
  string,
  ProofThreadFinalityLineage
>();

const initStage = ({
  phase,
  previousState,
  previousFinalityState,
  sourceStore: suppliedSource,
  sourceMode = "external_providers",
  transactionIsValid = true,
  ordinal = 0,
}: {
  phase: "pending" | "final";
  previousState: WatcherProofThreadStateV1 | null;
  previousFinalityState: WatcherFinalityStateV1 | null;
  sourceStore?: WatcherDurableStoreV1;
  sourceMode?: FixtureSourceMode;
  transactionIsValid?: boolean;
  ordinal?: number;
}): InitStage => {
  const fixture = initTransaction();
  const depth = phase === "pending" ? "1" : "2";
  const l1 = sourceFixture(sourceMode);
  const normalized = l1.providers.map((provider) =>
    normalize(provider, fixture, depth, ordinal, { transactionIsValid }),
  );
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    normalized,
  );
  const lineage =
    previousFinalityState === null
      ? []
      : (proofThreadFinalityLineageByStateDigest.get(
          previousFinalityState.stateDigest,
        ) ?? []);
  const finalityResult = evaluateWatcherFinalityV1(
    l1.policy,
    previousFinalityState,
    consistency,
  );
  const finalityObservations = l1.providers.map((provider) => ({
    authenticatedProvider: provider,
    l1Observation: rawObservation(provider, fixture, depth, ordinal, {
      transactionIsValid,
    }),
  }));
  if (finalityResult.state !== null) {
    proofThreadFinalityLineageByStateDigest.set(
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
    l1Observation: rawObservation(l1.providers[0]!, fixture, depth, ordinal, {
      transactionIsValid,
    }),
    sourceDurableStore: sourceStore,
    durableStore: store,
    deploymentAuthority: authority.deploymentAuthority,
    finalityAuthority: {
      policy: l1.policy,
      lineage,
      previousState: previousFinalityState,
      observations: finalityObservations,
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
  blockTransactions,
  blockOrdinal = ordinal,
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
  blockTransactions?: RawObservationOptions["transactions"];
  blockOrdinal?: number;
}): TransitionStage => {
  const ids = transitionIds(ordinal);
  const depth = phase === "pending" ? "1" : "2";
  const l1 = sourceFixture(sourceMode);
  const observationOptions: RawObservationOptions =
    blockTransactions === undefined ? {} : { transactions: blockTransactions };
  const normalized = l1.providers.map((provider) =>
    normalize(provider, fixture, depth, blockOrdinal, observationOptions),
  );
  const consistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    normalized,
  );
  const lineage =
    previousFinalityState === null
      ? []
      : (proofThreadFinalityLineageByStateDigest.get(
          previousFinalityState.stateDigest,
        ) ?? []);
  const finalityResult = evaluateWatcherFinalityV1(
    l1.policy,
    previousFinalityState,
    consistency,
  );
  const finalityObservations = l1.providers.map((provider) => ({
    authenticatedProvider: provider,
    l1Observation: rawObservation(
      provider,
      fixture,
      depth,
      blockOrdinal,
      observationOptions,
    ),
  }));
  if (finalityResult.state !== null) {
    proofThreadFinalityLineageByStateDigest.set(
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
    l1Observation: rawObservation(
      l1.providers[0]!,
      fixture,
      depth,
      blockOrdinal,
      observationOptions,
    ),
    sourceDurableStore: sourceStore,
    durableStore: store,
    deploymentAuthority: authority.deploymentAuthority,
    finalityAuthority: {
      policy: l1.policy,
      lineage,
      previousState: previousFinalityState,
      observations: finalityObservations,
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

const persistRecoveryObservations = (
  source: WatcherDurableStoreV1,
  observations: readonly WatcherNormalizedL1BlockV1[],
): WatcherDurableStoreV1 => {
  const l1Observations = new Map(
    source.l1Observations.map((entry) => [entry.observationId, entry]),
  );
  const chainPoints = new Map(
    source.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  for (const block of observations) {
    l1Observations.set(block.observationDigest, {
      observationId: block.observationDigest,
      providerId: block.provider.providerId,
      chainPointId: block.chainPoint.chainPointId,
      payload: makeWatcherDurablePayloadV1(
        encodeWatcherNormalizedL1BlockV1(block).toString("hex"),
      ),
    });
    chainPoints.set(block.chainPoint.chainPointId, {
      chainPointId: block.chainPoint.chainPointId,
      providerId: block.provider.providerId,
      blockHash: block.chainPoint.blockHash,
      slot: block.chainPoint.slot,
      blockNo: block.chainPoint.blockNo,
      depth: block.chainPoint.depth,
    });
  }
  return makeWatcherDurableStoreV1({
    deploymentMarker: authority.marker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      ...source,
      l1Observations: [...l1Observations.values()],
      chainPoints: [...chainPoints.values()],
    },
  });
};

type PostFinalityProofRecoveryStage = Readonly<{
  fixture: TxFixture;
  store: WatcherDurableStoreV1;
}>;

const postFinalityProofRecoveryBundle = (
  sourceMode: FixtureSourceMode,
  common: PostFinalityProofRecoveryStage,
  orphan: PostFinalityProofRecoveryStage,
  replacementFixture: TxFixture,
) => {
  const l1 = sourceFixture(sourceMode);
  const commonBlocks = l1.providers.map((provider) =>
    normalize(provider, common.fixture, "0", 0),
  );
  const commonRawObservations = l1.providers.map((provider) =>
    rawObservation(provider, common.fixture, "0", 0),
  );
  const commonConsistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    commonBlocks,
  );
  const orphanPendingBlocks = l1.providers.map((provider) =>
    normalize(provider, orphan.fixture, "1", 1),
  );
  const orphanPendingConsistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    orphanPendingBlocks,
  );
  const orphanPending = evaluateWatcherFinalityV1(
    l1.policy,
    null,
    orphanPendingConsistency,
  );
  expect(orphanPending.action).toBe("observe_pending");
  const orphanFinalBlocks = l1.providers.map((provider) =>
    normalize(provider, orphan.fixture, "2", 1),
  );
  const orphanFinalConsistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    orphanFinalBlocks,
  );
  const orphanFinalized = evaluateWatcherFinalityV1(
    l1.policy,
    orphanPending.state,
    orphanFinalConsistency,
  );
  expect(orphanFinalized.action).toBe("finalize");
  const replacementRaw = (provider: WatcherAuthenticatedL1ProviderV1) => ({
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: "Preprod",
    providerId: provider.providerId,
    chainPoint: {
      blockHash: h32("e9"),
      parentBlockHash: commonBlocks[0]!.chainPoint.blockHash,
      slot: (BigInt(commonBlocks[0]!.chainPoint.slot) + 1n).toString(),
      blockNo: (BigInt(commonBlocks[0]!.chainPoint.blockNo) + 1n).toString(),
      depth: "1",
    },
    transactions: [rawTransaction(replacementFixture)],
  });
  const replacementRawObservations = l1.providers.map((provider) =>
    replacementRaw(provider),
  );
  const replacementBlocks = l1.providers.map((provider, index) =>
    normalizeWatcherL1BlockV1(provider, replacementRawObservations[index]!),
  );
  const replacementConsistency = evaluateWatcherMultiProviderConsistencyV1(
    l1.consistencyConfig,
    replacementBlocks,
  );
  const replacementTail = [2, 3].map((ordinal) => {
    const parent = ordinal === 2 ? replacementBlocks[0]! : undefined;
    const rawObservations = l1.providers.map((provider) =>
      rawObservation(provider, replacementFixture, "1", ordinal, {
        empty: true,
      }),
    );
    for (const raw of rawObservations) {
      raw.chainPoint.parentBlockHash =
        parent?.chainPoint.blockHash ??
        h32((0x40 + ordinal - 1).toString(16).padStart(2, "0"));
    }
    const blocks = l1.providers.map((provider, index) =>
      normalizeWatcherL1BlockV1(provider, rawObservations[index]!),
    );
    return {
      rawObservations,
      blocks,
      consistency: evaluateWatcherMultiProviderConsistencyV1(
        l1.consistencyConfig,
        blocks,
      ),
    };
  });
  const contradiction = evaluateWatcherFinalityV1(
    l1.policy,
    orphanFinalized.state,
    replacementTail.at(-1)!.consistency,
  );
  expect(contradiction.action).toBe("quarantine_incident");
  const sourceStore = persistRecoveryObservations(orphan.store, [
    ...commonBlocks,
    ...orphanPendingBlocks,
    ...orphanFinalBlocks,
    ...replacementBlocks,
    ...replacementTail.flatMap(({ blocks }) => blocks),
  ]);
  const rollbackBootstrapState = makeWatcherRollbackBootstrapStateV1(
    l1.policy,
    sourceStore,
    orphanFinalized.state,
  )!;
  const incident = evaluateWatcherRollbackV1(
    l1.policy,
    sourceStore,
    orphanFinalized.state,
    replacementTail.at(-1)!.consistency,
    contradiction,
    rollbackBootstrapState,
    rollbackBootstrapState,
  );
  expect(incident).toMatchObject({
    action: "quarantine_incident",
    protocolDecision: "quarantined",
  });
  expect(incident.nextStore).not.toBeNull();
  expect(incident.rollbackState?.incident).not.toBeNull();
  const recoveryInput: WatcherPostFinalityRecoveryInputV1 = {
    policy: l1.policy,
    sourceStore: incident.nextStore,
    currentStore: incident.nextStore,
    quarantinedRollbackState: incident.rollbackState,
    rollbackBootstrapState,
    previousCanonicalPath: [commonConsistency, orphanFinalConsistency],
    replacementCanonicalPath: [
      commonConsistency,
      replacementConsistency,
      ...replacementTail.map(({ consistency }) => consistency),
    ],
    previousRecoveryState: null,
  };
  const recovery = evaluateWatcherPostFinalityRecoveryV1(recoveryInput);
  expect(recovery).toMatchObject({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    recoveryState: {
      network: "Preprod",
      path: {
        commonAncestorPointDigest: commonBlocks[0]!.chainPoint.pointDigest,
        replacementTipPointDigest:
          replacementTail.at(-1)!.blocks[0]!.chainPoint.pointDigest,
        rollbackDepth: "1",
      },
      incidentLifecycle: { status: "recovered" },
    },
    resumableFinalityState: {
      phase: "unobserved",
      incident: null,
    },
  });
  return {
    recovery,
    recoveryInput,
    commonBlocks,
    commonRawObservations,
    replacementBlocks,
    replacementConsistency,
    replacementRawObservations,
    finalityPolicy: l1.policy,
    providers: l1.providers,
  };
};

const postFinalityProofThreadScenario = (sourceMode: FixtureSourceMode) => {
  const commonPending = initStage({
    phase: "pending",
    previousState: null,
    previousFinalityState: null,
    sourceMode,
  });
  const commonPendingResult = evaluateWatcherProofThreadIndexerV1(
    policy,
    null,
    commonPending.observation,
    commonPending.context,
  );
  expect(commonPendingResult.action).toBe("accept");
  const commonFinal = initStage({
    phase: "final",
    previousState: commonPendingResult.state!,
    previousFinalityState: commonPending.finalityState,
    sourceStore: commonPending.store,
    sourceMode,
  });
  const commonFinalResult = evaluateWatcherProofThreadIndexerV1(
    policy,
    commonPendingResult.state,
    commonFinal.observation,
    commonFinal.context,
  );
  expect(commonFinalResult).toMatchObject({
    action: "accept",
    protocolDecision: "indexed",
  });

  const orphanFixture = stepTransaction(commonFinal.journal);
  const orphanSource = addSubmittedTransaction(
    commonFinal.store,
    orphanFixture,
    transitionIds(1).submissionId,
  );
  const orphanPending = transitionStage({
    transitionKind: "step",
    fixture: orphanFixture,
    phase: "pending",
    previousState: commonFinalResult.state!,
    previousFinalityState: null,
    sourceStore: orphanSource,
    sourceJournal: commonFinal.journal,
    ordinal: 1,
    sourceMode,
  });
  const orphanPendingResult = evaluateWatcherProofThreadIndexerV1(
    policy,
    commonFinalResult.state,
    orphanPending.observation,
    orphanPending.context,
  );
  expect(orphanPendingResult.action).toBe("accept");
  const orphanFinal = transitionStage({
    transitionKind: "step",
    fixture: orphanFixture,
    phase: "final",
    previousState: orphanPendingResult.state!,
    previousFinalityState: orphanPending.finalityState,
    sourceStore: orphanPending.store,
    sourceJournal: commonFinal.journal,
    ordinal: 1,
    sourceMode,
  });
  const orphanFinalResult = evaluateWatcherProofThreadIndexerV1(
    policy,
    orphanPendingResult.state,
    orphanFinal.observation,
    orphanFinal.context,
  );
  expect(orphanFinalResult).toMatchObject({
    action: "accept",
    protocolDecision: "indexed",
  });
  const resumeFixture = cancelTransaction(commonFinal.journal);
  const recoverySource = addSubmittedTransaction(
    orphanFinal.store,
    resumeFixture,
    transitionIds(2).submissionId,
  );
  const bundle = postFinalityProofRecoveryBundle(
    sourceMode,
    commonFinal,
    { ...orphanFinal, store: recoverySource },
    resumeFixture,
  );
  const common = bundle.commonBlocks[0]!;
  const replacement = bundle.replacementBlocks[0]!;
  const sourceStore = bundle.recoveryInput.sourceStore as WatcherDurableStoreV1;
  const observation = makeWatcherProofThreadObservationV1({
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: "rollback",
    confirmationPhase: null,
    pointDigest: common.chainPoint.pointDigest,
    blockHash: common.chainPoint.blockHash,
    slot: common.chainPoint.slot,
    blockNo: common.chainPoint.blockNo,
    transactionHash: null,
    publicInputDigest: createHash("sha256")
      .update(encodeWatcherNormalizedL1BlockV1(common))
      .digest("hex"),
    sourceObservationDigest: common.observationDigest,
    chainPointId: common.chainPoint.chainPointId,
    sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(sourceStore),
    ),
    sourceDurableStoreRevision: sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(bundle.recovery.nextStore!),
    ),
    durableStoreRevision: bundle.recovery.nextStore!.revision,
    predecessorStateDigest: orphanFinalResult.state!.stateDigest,
    submissionId: null,
    confirmationId: null,
    rollbackTargetStateDigest: commonFinalResult.state!.stateDigest,
    layout: null,
    journal: null,
  })!;
  const context: WatcherProofThreadPublicContextV1 = {
    schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: null,
    l1Observation: null,
    sourceDurableStore: sourceStore,
    durableStore: bundle.recovery.nextStore!,
    deploymentAuthority: authority.deploymentAuthority,
    finalityAuthority: null,
    rollbackAuthority: {
      result: bundle.recovery,
      context: bundle.recoveryInput,
    },
    sourceJournal: orphanFinal.journal,
    durableJournal: commonFinal.journal,
  };
  const resumeIds = transitionIds(2);
  const resumeStore = appendTransitionStore({
    source: bundle.recovery.nextStore!,
    block: replacement,
    fixture: resumeFixture,
    transitionKind: "cancel",
    phase: "pending",
    ...resumeIds,
    applyEffects: true,
  });
  const resumeJournal = transitionJournal({
    source: commonFinal.journal,
    fixture: resumeFixture,
    transitionKind: "cancel",
    ...resumeIds,
  });
  const resumeFinality = evaluateWatcherFinalityV1(
    bundle.finalityPolicy,
    null,
    bundle.replacementConsistency,
  );
  expect(resumeFinality).toMatchObject({
    action: "observe_pending",
    protocolDecision: "hold",
  });
  const resumeObservation = makeWatcherProofThreadObservationV1({
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: "cancel",
    confirmationPhase: "pending",
    pointDigest: replacement.chainPoint.pointDigest,
    blockHash: replacement.chainPoint.blockHash,
    slot: replacement.chainPoint.slot,
    blockNo: replacement.chainPoint.blockNo,
    transactionHash: resumeFixture.txHash,
    publicInputDigest: createHash("sha256")
      .update(encodeWatcherNormalizedL1BlockV1(replacement))
      .digest("hex"),
    sourceObservationDigest: replacement.observationDigest,
    chainPointId: replacement.chainPoint.chainPointId,
    sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(bundle.recovery.nextStore!),
    ),
    sourceDurableStoreRevision: bundle.recovery.nextStore!.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(resumeStore),
    ),
    durableStoreRevision: resumeStore.revision,
    predecessorStateDigest: null,
    submissionId: resumeIds.submissionId,
    confirmationId: resumeIds.confirmationId,
    rollbackTargetStateDigest: null,
    layout: transitionLayout("cancel"),
    journal: resumeJournal,
  })!;
  const resumeContext: WatcherProofThreadPublicContextV1 = {
    schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: bundle.providers[0],
    l1Observation: bundle.replacementRawObservations[0],
    sourceDurableStore: bundle.recovery.nextStore!,
    durableStore: resumeStore,
    deploymentAuthority: authority.deploymentAuthority,
    finalityAuthority: {
      policy: bundle.finalityPolicy,
      lineage: [],
      previousState: null,
      observations: bundle.providers.map((authenticatedProvider, index) => ({
        authenticatedProvider,
        l1Observation: bundle.replacementRawObservations[index]!,
      })),
      consistency: bundle.replacementConsistency,
      result: resumeFinality,
    },
    rollbackAuthority: null,
    sourceJournal: commonFinal.journal,
    durableJournal: resumeJournal,
  };
  return {
    ...bundle,
    commonState: commonFinalResult.state!,
    orphanState: orphanFinalResult.state!,
    sourceJournal: orphanFinal.journal,
    targetJournal: commonFinal.journal,
    observation,
    context,
    resumeObservation,
    resumeContext,
  };
};

describe("W17 public proof/computation-thread indexer", () => {
  it("requires one live, uniquely matching transport capability for every W10/W11 replay", async () => {
    const external = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
    });
    const providerATransport = transportForProvider(providers[0]);
    const providerBTransport = transportForProvider(providers[1]);

    for (const transportAttestations of [
      [],
      structuredClone(watcherTransportContexts),
      [providerBTransport],
      [...watcherTransportContexts, providerATransport],
    ]) {
      expect(
        evaluateWatcherProofThreadIndexerV1Raw(
          policy,
          null,
          external.observation,
          external.context,
          transportAttestations,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["malformed_public_context"],
      });
    }
  });

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
    ).toBeNull();
    expect(policy).not.toBeNull();
    expect(policy.families).toHaveLength(
      DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    );
    expect(
      [...policy.families.map(({ catalogueCategory }) => catalogueCategory)]
        .sort()
        .join(","),
    ).toBe(
      [...DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER]
        .sort()
        .join(","),
    );
    expect(policy.families[0]!.familyId).toBe("canonical-decodability");
    const transitionTrace = policy.families.find(
      ({ catalogueCategory }) => catalogueCategory === "transitionTrace",
    )!;
    expect(transitionTrace.categoryId).toBe("00000004");
    expect(transitionTrace.stepScriptHashes).toHaveLength(9);
    expect(transitionTrace.nextStepIndexes).toEqual([
      ["1", "2", "3", "4", "5", "6", "7", "8"],
      [],
      [],
      [],
      [],
      [],
      [],
      [],
      [],
    ]);
    for (const family of policy.families) {
      const registered =
        WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1[
          family.catalogueCategory as keyof typeof WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1
        ];
      expect(family.familyId).toBe(registered.familyId);
      expect(family.stepScriptHashes).toHaveLength(registered.stepCount);
      expect(family.nextStepIndexes).toHaveLength(registered.stepCount);
      registered.deployedStepContractNames.forEach(
        (contractName, stepIndex) => {
          expect(family.stepScriptHashes[stepIndex]).toBe(
            authority.deploymentAuthority.policy.appliedScriptHashes[
              contractName
            ],
          );
        },
      );
    }

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

  it("rejects incomplete, ambiguous, and unreachable signed family graphs", () => {
    const base = {
      network: "Preprod" as const,
      releaseEvidenceDigest: RELEASE_DIGEST,
      deploymentMarker: authority.marker,
      deploymentTrustRootId: authority.result.trustRootId,
      requiredFinalityDepth: "2",
      computationThreadPolicyId: CT_POLICY,
      fraudProofPolicyId: applied.fraudProofMint!,
      fraudProofSpendScriptHash: applied.fraudProofSpend!,
      fraudProofAddressHex: scriptAddress(applied.fraudProofSpend!),
      maximumHistoryEntries: "32",
    };

    expect(
      makeWatcherProofThreadPolicyV1({
        ...base,
        families: authority.families.slice(1),
      }),
    ).toBeNull();

    const ambiguous = structuredClone(
      authority.families,
    ) as unknown as Mutable[];
    const routed = ambiguous.find(
      ({ catalogueCategory }) => catalogueCategory === "transitionTrace",
    )!;
    routed.nextStepIndexes[0] = ["1", "1", "2", "3", "4", "5", "6", "7"];
    expect(
      makeWatcherProofThreadPolicyV1({
        ...base,
        families: ambiguous as unknown as WatcherProofThreadFamilyV1[],
      }),
    ).toBeNull();

    const unreachable = structuredClone(
      authority.families,
    ) as unknown as Mutable[];
    const linear = unreachable.find(
      ({ catalogueCategory }) => catalogueCategory === "invalidRange",
    )!;
    linear.nextStepIndexes = [[], []];
    expect(
      makeWatcherProofThreadPolicyV1({
        ...base,
        families: unreachable as unknown as WatcherProofThreadFamilyV1[],
      }),
    ).toBeNull();
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

  it("rejects a same-revision durable-store addition between accepted transitions", () => {
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
    expect(pendingResult.action).toBe("accept");

    const sameRevisionSource = makeWatcherDurableStoreV1({
      deploymentMarker: authority.marker,
      revision: pending.store.revision,
      records: {
        ...pending.store,
        daProofInputs: [
          ...pending.store.daProofInputs,
          {
            inputId: h32("d1"),
            kind: "da_payload",
            payload: makeWatcherDurablePayloadV1("81"),
          },
        ],
      },
    });
    expect(sameRevisionSource.revision).toBe(pending.store.revision);
    expect(
      watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(sameRevisionSource),
      ),
    ).not.toBe(
      watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(pending.store),
      ),
    );

    const final = initStage({
      phase: "final",
      previousState: pendingResult.state!,
      previousFinalityState: pending.finalityState,
      sourceStore: sameRevisionSource,
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        pendingResult.state,
        final.observation,
        final.context,
      ),
    ).toMatchObject({
      action: "reject",
      protocolDecision: "hold",
      reasonCodes: ["stale_state"],
    });
  });

  it("rejects aggregate restart evidence spread across retained transitions and cyclic state evidence", () => {
    const aggregateFragmentHex = "ab".repeat(8_500_000);
    const sourceFixtureValue = initTransaction();
    const aggregateBaseStore = baseStore(sourceFixtureValue);
    const bulkySource = makeWatcherDurableStoreV1({
      deploymentMarker: authority.marker,
      revision: (BigInt(aggregateBaseStore.revision) + 1n).toString(),
      records: {
        ...aggregateBaseStore,
        daProofInputs: [
          ...aggregateBaseStore.daProofInputs,
          {
            inputId: h32("d2"),
            kind: "da_payload",
            payload: makeWatcherDurablePayloadV1(aggregateFragmentHex),
          },
        ],
      },
    });
    const pending = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
      sourceStore: bulkySource,
    });
    const pendingResult = evaluateWatcherProofThreadIndexerV1(
      policy,
      null,
      pending.observation,
      pending.context,
    );
    expect(pendingResult.action).toBe("accept");
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
    expect(finalResult.action).toBe("accept");
    expect(finalResult.state?.transitionHistory).toHaveLength(2);
    expect(
      parseWatcherProofThreadStateV1(
        structuredClone(finalResult.state),
        policy,
      ),
    ).toBeNull();

    const cyclic = structuredClone(pendingResult.state) as Mutable;
    cyclic.transitionHistory.push(cyclic);
    expect(parseWatcherProofThreadStateV1(cyclic, policy)).toBeNull();
  }, 60_000);

  it("orders distinct same-block proof transactions and rejects reversed or forged ordinals", () => {
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
    expect(initFinalResult.action).toBe("accept");

    const stepFixture = stepTransaction(initFinal.journal);
    const stepSource = addSubmittedTransaction(
      initFinal.store,
      stepFixture,
      transitionIds(1).submissionId,
    );
    const orderedTransactions = [
      { fixture: initFinal.fixture, transactionIndex: "0" },
      { fixture: stepFixture, transactionIndex: "1" },
    ] as const;
    const ordered = transitionStage({
      transitionKind: "step",
      fixture: stepFixture,
      phase: "pending",
      previousState: initFinalResult.state!,
      previousFinalityState: null,
      sourceStore: stepSource,
      sourceJournal: initFinal.journal,
      ordinal: 1,
      blockOrdinal: 0,
      blockTransactions: orderedTransactions,
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        initFinalResult.state,
        ordered.observation,
        ordered.context,
      ),
    ).toMatchObject({
      action: "accept",
      reasonCodes: ["step_pending"],
    });

    const reversed = transitionStage({
      transitionKind: "step",
      fixture: stepFixture,
      phase: "pending",
      previousState: initFinalResult.state!,
      previousFinalityState: null,
      sourceStore: stepSource,
      sourceJournal: initFinal.journal,
      ordinal: 1,
      blockOrdinal: 0,
      blockTransactions: [
        { fixture: stepFixture, transactionIndex: "0" },
        { fixture: initFinal.fixture, transactionIndex: "1" },
      ],
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        initFinalResult.state,
        reversed.observation,
        reversed.context,
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["stale_state"],
    });

    expect(() =>
      transitionStage({
        transitionKind: "step",
        fixture: stepFixture,
        phase: "pending",
        previousState: initFinalResult.state!,
        previousFinalityState: null,
        sourceStore: stepSource,
        sourceJournal: initFinal.journal,
        ordinal: 1,
        blockOrdinal: 0,
        blockTransactions: [
          { fixture: initFinal.fixture, transactionIndex: "0" },
          { fixture: stepFixture, transactionIndex: "7" },
        ],
      }),
    ).toThrow(/transactionIndex/u);
  });

  it.each<FixtureSourceMode>(["external_providers"])(
    "traverses authenticated empty-block ancestry in %s mode",
    (sourceMode) => {
      const initPending = initStage({
        phase: "pending",
        previousState: null,
        previousFinalityState: null,
        sourceMode,
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
        sourceMode,
      });
      const initFinalResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        initPendingResult.state,
        initFinal.observation,
        initFinal.context,
      );
      expect(initFinalResult.action).toBe("accept");

      const fixture = stepTransaction(initFinal.journal);
      const l1 = sourceFixture(sourceMode);
      const finalityStep = (
        previousState: WatcherFinalityStateV1 | null,
        depth: string,
        ordinal: number,
        empty: boolean,
      ) => {
        const blocks = l1.providers.map((provider) =>
          normalize(provider, fixture, depth, ordinal, { empty }),
        );
        const observations = l1.providers.map((provider) => ({
          authenticatedProvider: provider,
          l1Observation: rawObservation(provider, fixture, depth, ordinal, {
            empty,
          }),
        }));
        const consistency = evaluateWatcherMultiProviderConsistencyV1(
          l1.consistencyConfig,
          blocks,
        );
        const result = evaluateWatcherFinalityV1(
          l1.policy,
          previousState,
          consistency,
        );
        return { blocks, observations, consistency, result };
      };
      const empty = finalityStep(null, "1", 1, true);
      expect(empty.blocks[0]?.transactions).toEqual([]);
      const target = finalityStep(empty.result.state, "0", 2, false);
      expect(target.result).toMatchObject({
        action: "rewind_pending",
        protocolDecision: "rewind_required",
      });
      proofThreadFinalityLineageByStateDigest.set(
        target.result.state!.stateDigest,
        [
          {
            observations: empty.observations,
            consistency: empty.consistency,
            result: empty.result,
          },
          {
            observations: target.observations,
            consistency: target.consistency,
            result: target.result,
          },
        ],
      );

      const sourceStore = addSubmittedTransaction(
        initFinal.store,
        fixture,
        transitionIds(2).submissionId,
      );
      const step = transitionStage({
        transitionKind: "step",
        fixture,
        phase: "pending",
        previousState: initFinalResult.state!,
        previousFinalityState: target.result.state,
        sourceStore,
        sourceJournal: initFinal.journal,
        ordinal: 2,
        sourceMode,
      });
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          initFinalResult.state,
          step.observation,
          step.context,
        ),
      ).toMatchObject({
        action: "accept",
        protocolDecision: "hold",
        reasonCodes: ["step_pending"],
      });
    },
  );

  it.each<FixtureSourceMode>(["external_providers"])(
    "ignores a phase-2-invalid phantom proof transaction in %s mode",
    (sourceMode) => {
      const phantom = initStage({
        phase: "pending",
        previousState: null,
        previousFinalityState: null,
        sourceMode,
        transactionIsValid: false,
      });
      expect(phantom.block.transactions[0]).toMatchObject({
        txHash: phantom.fixture.txHash,
        isValid: false,
        utxos: [],
      });
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          null,
          phantom.observation,
          phantom.context,
        ),
      ).toMatchObject({
        action: "reject",
        protocolDecision: "hold",
        reasonCodes: ["malformed_public_context"],
      });
    },
  );

  it("indexes deterministic step, success, proof-token removal, and cancellation lifecycles", () => {
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
    expect(initPendingResult.action).toBe("accept");
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
      transitionIds(1).submissionId,
    );
    const cancel = runFinalTransition({
      transitionKind: "cancel",
      fixture: cancelFixture,
      previousState: initFinalResult.state!,
      previousFinalityState: null,
      sourceStore: cancelSource,
      sourceJournal: initFinal.journal,
      ordinal: 1,
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

  it.each<FixtureSourceMode>(["external_providers"])(
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
      const submittedStepSource = addSubmittedTransaction(
        initFinal.store,
        stepFixture,
        transitionIds(1).submissionId,
      );
      const continuitySentinel = {
        inputId: h32("de"),
        kind: "da_payload" as const,
        payload: makeWatcherDurablePayloadV1("81"),
      };
      const stepSource = makeWatcherDurableStoreV1({
        deploymentMarker: authority.marker,
        revision: (BigInt(submittedStepSource.revision) + 1n).toString(),
        records: {
          ...submittedStepSource,
          daProofInputs: [
            ...submittedStepSource.daProofInputs,
            continuitySentinel,
          ],
        },
      });
      const stepPending = transitionStage({
        transitionKind: "step",
        fixture: stepFixture,
        phase: "pending",
        previousState: initFinalResult.state!,
        previousFinalityState: null,
        sourceStore: stepSource,
        sourceJournal: initFinal.journal,
        ordinal: 1,
        sourceMode,
      });
      const stepPendingResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        initFinalResult.state,
        stepPending.observation,
        stepPending.context,
      );
      expect(stepPendingResult.action).toBe("accept");
      expect(stepPending.store.daProofInputs).toContainEqual(
        continuitySentinel,
      );

      const replacementL1 = sourceFixture(sourceMode);
      const replacement = replacementL1.providers.map((provider) =>
        normalize(provider, stepFixture, "1", 2),
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
      expect(consistency.independentProviderCount).toBe(2);
      expect(consistency.queryObservationCount).toBe(0);
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
        transitionIds(1).confirmationId,
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
      expect(indexed.state?.transitionHistory).toHaveLength(4);
      expect(
        parseWatcherProofThreadStateV1(structuredClone(indexed.state), policy),
      ).toEqual(indexed.state);

      const {
        schemaVersion: _divergentObservationSchema,
        observationDigest: _divergentObservationDigest,
        ...rollbackObservationInput
      } = rollbackObservation;
      const duplicateRollbackContext = {
        ...rollbackContext,
        sourceStore: rollbackResult.nextStore!,
        previousRollbackState: rollbackResult.rollbackState,
      };
      const duplicateRollbackResult = evaluateWatcherRollbackV1(
        replacementL1.policy,
        rollbackResult.nextStore,
        stepPending.finalityState,
        consistency,
        finalityResult,
        rollbackResult.rollbackState,
        bootstrap,
      );
      expect(duplicateRollbackResult.action).toBe("duplicate_rewind");
      const duplicateRollbackObservation = makeWatcherProofThreadObservationV1({
        ...rollbackObservationInput,
        sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(rollbackResult.nextStore!),
        ),
        sourceDurableStoreRevision: rollbackResult.nextStore!.revision,
        durableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(rollbackResult.nextStore!),
        ),
        durableStoreRevision: rollbackResult.nextStore!.revision,
        predecessorStateDigest: indexed.state!.stateDigest,
      })!;
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          indexed.state,
          duplicateRollbackObservation,
          {
            ...rollbackPublicContext,
            sourceDurableStore: rollbackResult.nextStore!,
            durableStore: rollbackResult.nextStore!,
            rollbackAuthority: {
              result: duplicateRollbackResult,
              context: duplicateRollbackContext,
            },
          },
        ),
      ).toMatchObject({
        action: "duplicate",
        reasonCodes: ["duplicate_observation"],
        state: indexed.state,
      });
      const divergentRollbackSource = makeWatcherDurableStoreV1({
        deploymentMarker: authority.marker,
        revision: rollbackSource.revision,
        records: {
          ...rollbackSource,
          daProofInputs: rollbackSource.daProofInputs.filter(
            ({ inputId }) => inputId !== continuitySentinel.inputId,
          ),
        },
      });
      const divergentBootstrap = makeWatcherRollbackBootstrapStateV1(
        replacementL1.policy,
        divergentRollbackSource,
        stepPending.finalityState,
      )!;
      const divergentRollbackContext = {
        policy: replacementL1.policy,
        sourceStore: divergentRollbackSource,
        previousFinalityState: stepPending.finalityState,
        consistency,
        finalityResult,
        previousRollbackState: divergentBootstrap,
        rollbackBootstrapState: divergentBootstrap,
      };
      const divergentRollbackResult = evaluateWatcherRollbackV1(
        replacementL1.policy,
        divergentRollbackSource,
        stepPending.finalityState,
        consistency,
        finalityResult,
        divergentBootstrap,
        divergentBootstrap,
      );
      expect(divergentRollbackResult.action).toBe("apply_rewind");
      const divergentRollbackObservation = makeWatcherProofThreadObservationV1({
        ...rollbackObservationInput,
        sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(divergentRollbackSource),
        ),
        sourceDurableStoreRevision: divergentRollbackSource.revision,
        durableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(divergentRollbackResult.nextStore!),
        ),
        durableStoreRevision: divergentRollbackResult.nextStore!.revision,
      })!;
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          stepPendingResult.state,
          divergentRollbackObservation,
          {
            ...rollbackPublicContext,
            sourceDurableStore: divergentRollbackSource,
            durableStore: divergentRollbackResult.nextStore!,
            rollbackAuthority: {
              result: divergentRollbackResult,
              context: divergentRollbackContext,
            },
          },
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["rollback_authority_mismatch"],
      });

      const rehashedPredecessor = structuredClone(indexed.state) as Mutable;
      const predecessorTransition = rehashedPredecessor.transitionHistory.at(
        -1,
      ) as Mutable;
      const {
        schemaVersion: _predecessorSchema,
        observationDigest: _predecessorDigest,
        ...predecessorObservation
      } = predecessorTransition.observation;
      predecessorTransition.observation = makeWatcherProofThreadObservationV1({
        ...predecessorObservation,
        predecessorStateDigest: h32("bd"),
      })!;
      predecessorTransition.predecessorStateDigest = h32("bd");
      const rehashedPredecessorEntry = rehashProofThreadEntry(
        predecessorTransition,
      );
      rehashedPredecessor.transitionHistory[
        rehashedPredecessor.transitionHistory.length - 1
      ] = rehashedPredecessorEntry;
      const rollbackAudit = rehashedPredecessor.auditHistory.at(-1) as Mutable;
      rollbackAudit.entry = structuredClone(rehashedPredecessorEntry);
      rehashedPredecessor.auditHistory[
        rehashedPredecessor.auditHistory.length - 1
      ] = rehashProofThreadAudit(rollbackAudit);
      expect(
        parseWatcherProofThreadStateV1(
          rehashProofThreadState(rehashedPredecessor),
          policy,
        ),
      ).toBeNull();

      const swappedRollbackResult = structuredClone(indexed.state) as Mutable;
      const swappedTransition = swappedRollbackResult.transitionHistory.at(
        -1,
      ) as Mutable;
      (
        swappedTransition.rollbackResult.removedRecords as Mutable
      ).confirmationIds = [];
      const swappedEntry = rehashProofThreadEntry(swappedTransition);
      swappedRollbackResult.transitionHistory[
        swappedRollbackResult.transitionHistory.length - 1
      ] = swappedEntry;
      const swappedAudit = swappedRollbackResult.auditHistory.at(-1) as Mutable;
      swappedAudit.entry = structuredClone(swappedEntry);
      swappedRollbackResult.auditHistory[
        swappedRollbackResult.auditHistory.length - 1
      ] = rehashProofThreadAudit(swappedAudit);
      expect(
        parseWatcherProofThreadStateV1(
          rehashProofThreadState(swappedRollbackResult),
          policy,
        ),
      ).toBeNull();

      const swappedRollbackContext = structuredClone(indexed.state) as Mutable;
      const contextTransition = swappedRollbackContext.transitionHistory.at(
        -1,
      ) as Mutable;
      contextTransition.publicContext.rollbackAuthority = {
        result: structuredClone(duplicateRollbackResult),
        context: structuredClone(duplicateRollbackContext),
      };
      const contextEntry = rehashProofThreadEntry(contextTransition);
      swappedRollbackContext.transitionHistory[
        swappedRollbackContext.transitionHistory.length - 1
      ] = contextEntry;
      const contextAudit = swappedRollbackContext.auditHistory.at(
        -1,
      ) as Mutable;
      contextAudit.entry = structuredClone(contextEntry);
      swappedRollbackContext.auditHistory[
        swappedRollbackContext.auditHistory.length - 1
      ] = rehashProofThreadAudit(contextAudit);
      expect(
        parseWatcherProofThreadStateV1(
          rehashProofThreadState(swappedRollbackContext),
          policy,
        ),
      ).toBeNull();

      const reordered = structuredClone(indexed.state) as Mutable;
      [reordered.transitionHistory[0], reordered.transitionHistory[1]] = [
        reordered.transitionHistory[1],
        reordered.transitionHistory[0],
      ];
      expect(
        parseWatcherProofThreadStateV1(
          rehashProofThreadState(reordered),
          policy,
        ),
      ).toBeNull();
      const truncated = structuredClone(indexed.state) as Mutable;
      truncated.transitionHistory = truncated.transitionHistory.slice(1);
      expect(
        parseWatcherProofThreadStateV1(
          rehashProofThreadState(truncated),
          policy,
        ),
      ).toBeNull();
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
    30_000,
  );

  it.each<FixtureSourceMode>(["external_providers"])(
    "fully rewinds %s history, restarts from the rollback anchor, and retains unrelated durable updates",
    (sourceMode) => {
      const pending = initStage({
        phase: "pending",
        previousState: null,
        previousFinalityState: null,
        sourceMode,
      });
      const pendingResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        pending.observation,
        pending.context,
      );
      expect(pendingResult.action).toBe("accept");

      const unrelatedInput = {
        inputId: h32("e1"),
        kind: "da_payload" as const,
        payload: makeWatcherDurablePayloadV1("81"),
      };
      const sourceWithUnrelatedUpdate = makeWatcherDurableStoreV1({
        deploymentMarker: authority.marker,
        revision: (BigInt(pending.store.revision) + 1n).toString(),
        records: {
          ...pending.store,
          daProofInputs: [...pending.store.daProofInputs, unrelatedInput],
        },
      });
      const l1 = sourceFixture(sourceMode);
      const replacementBlocks = l1.providers.map((provider) =>
        normalize(provider, pending.fixture, "1", 1),
      );
      const replacementConsistency = evaluateWatcherMultiProviderConsistencyV1(
        l1.consistencyConfig,
        replacementBlocks,
      );
      const replacementFinality = evaluateWatcherFinalityV1(
        l1.policy,
        pending.finalityState,
        replacementConsistency,
      );
      expect(replacementFinality).toMatchObject({
        action: "rewind_pending",
        protocolDecision: "rewind_required",
      });
      const rollbackSource = persistNormalizedObservations(
        sourceWithUnrelatedUpdate,
        replacementBlocks,
      );
      const bootstrap = makeWatcherRollbackBootstrapStateV1(
        l1.policy,
        rollbackSource,
        pending.finalityState,
      )!;
      const rollbackContext = {
        policy: l1.policy,
        sourceStore: rollbackSource,
        previousFinalityState: pending.finalityState,
        consistency: replacementConsistency,
        finalityResult: replacementFinality,
        previousRollbackState: bootstrap,
        rollbackBootstrapState: bootstrap,
      };
      const rollbackResult = evaluateWatcherRollbackV1(
        l1.policy,
        rollbackSource,
        pending.finalityState,
        replacementConsistency,
        replacementFinality,
        bootstrap,
        bootstrap,
      );
      expect(rollbackResult).toMatchObject({
        action: "apply_rewind",
        reasonCodes: ["rewind_applied"],
      });
      expect(rollbackResult.nextStore?.daProofInputs).toContainEqual(
        unrelatedInput,
      );

      const replacement = replacementBlocks[0]!;
      const rollbackObservation = makeWatcherProofThreadObservationV1({
        policyDigest: policy.policyDigest,
        network: policy.network,
        releaseEvidenceDigest: policy.releaseEvidenceDigest,
        deploymentMarker: policy.deploymentMarker,
        transitionKind: "rollback",
        confirmationPhase: null,
        pointDigest: replacement.chainPoint.pointDigest,
        blockHash: replacement.chainPoint.blockHash,
        slot: replacement.chainPoint.slot,
        blockNo: replacement.chainPoint.blockNo,
        transactionHash: null,
        publicInputDigest: createHash("sha256")
          .update(encodeWatcherNormalizedL1BlockV1(replacement))
          .digest("hex"),
        sourceObservationDigest: replacement.observationDigest,
        chainPointId: replacement.chainPoint.chainPointId,
        sourceDurableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(rollbackSource),
        ),
        sourceDurableStoreRevision: rollbackSource.revision,
        durableStoreDigest: watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(rollbackResult.nextStore!),
        ),
        durableStoreRevision: rollbackResult.nextStore!.revision,
        predecessorStateDigest: pendingResult.state!.stateDigest,
        submissionId: null,
        confirmationId: null,
        rollbackTargetStateDigest: null,
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
        sourceJournal: null,
        durableJournal: null,
      };
      const rewound = evaluateWatcherProofThreadIndexerV1(
        policy,
        pendingResult.state,
        rollbackObservation,
        rollbackPublicContext,
      );
      expect(rewound).toMatchObject({
        action: "accept",
        protocolDecision: "indexed",
        reasonCodes: ["rollback_confirmed"],
        state: {
          journal: null,
          pending: null,
          history: [],
          transitionHistory: expect.arrayContaining([
            expect.objectContaining({ transitionKind: "rollback" }),
          ]),
        },
      });
      const restartedState = parseWatcherProofThreadStateV1(
        structuredClone(rewound.state),
        policy,
      );
      expect(restartedState).toEqual(rewound.state);

      const restartFixture = initTransaction();
      const restartSource = makeWatcherDurableStoreV1({
        deploymentMarker: authority.marker,
        revision: (BigInt(rollbackResult.nextStore!.revision) + 1n).toString(),
        records: {
          ...rollbackResult.nextStore!,
          submissions: [
            ...rollbackResult.nextStore!.submissions.filter(
              ({ submissionId }) => submissionId !== SUBMISSION_ID,
            ),
            {
              submissionId: SUBMISSION_ID,
              faultId: FAULT_ID,
              txBodyHash: restartFixture.txHash,
              status: "submitted",
            },
          ],
        },
      });
      expect(restartSource.daProofInputs).toContainEqual(unrelatedInput);
      const restarted = initStage({
        phase: "pending",
        previousState: restartedState,
        previousFinalityState: null,
        sourceStore: restartSource,
        sourceMode,
        ordinal: 2,
      });
      const restartedResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        restartedState,
        restarted.observation,
        restarted.context,
      );
      expect(restartedResult).toMatchObject({
        action: "accept",
        protocolDecision: "hold",
        reasonCodes: ["init_pending"],
      });
      expect((restartedResult.state?.history.length ?? 0) > 0).toBe(true);
      expect(restarted.store.daProofInputs).toContainEqual(unrelatedInput);
    },
  );

  it.each<FixtureSourceMode>(["external_providers"])(
    "rejects %s two-step competing forks and oversized W12 evidence before indexing",
    (sourceMode) => {
      const initial = initStage({
        phase: "pending",
        previousState: null,
        previousFinalityState: null,
        sourceMode,
      });
      const oversized = structuredClone(initial.context);
      const authority = oversized.finalityAuthority as Mutable;
      authority.observations = Array.from({ length: 17 }, () =>
        structuredClone(authority.observations[0]),
      );
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          null,
          initial.observation,
          oversized,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["malformed_public_context"],
      });
      const excessiveWidth = structuredClone(initial.context);
      (excessiveWidth.finalityAuthority as Mutable).lineage = Array.from(
        {
          length: WATCHER_PROOF_THREAD_V1_BOUNDS.evidenceContainerEntries + 1,
        },
        () => null,
      );
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          null,
          initial.observation,
          excessiveWidth,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["malformed_public_context"],
      });
      const cyclic = structuredClone(initial.context);
      (cyclic.finalityAuthority as Mutable).lineage = [
        cyclic.finalityAuthority,
      ];
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          null,
          initial.observation,
          cyclic,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["malformed_public_context"],
      });

      const pendingResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        initial.observation,
        initial.context,
      );
      expect(pendingResult.action).toBe("accept");
      const finalized = initStage({
        phase: "final",
        previousState: pendingResult.state!,
        previousFinalityState: initial.finalityState,
        sourceStore: initial.store,
        sourceMode,
      });
      const finalizedResult = evaluateWatcherProofThreadIndexerV1(
        policy,
        pendingResult.state,
        finalized.observation,
        finalized.context,
      );
      expect(finalizedResult.action).toBe("accept");

      const stepFixture = stepTransaction(finalized.journal);
      const forkSource = addSubmittedTransaction(
        finalized.store,
        stepFixture,
        transitionIds(2).submissionId,
      );
      const twoStepFork = transitionStage({
        transitionKind: "step",
        fixture: stepFixture,
        phase: "pending",
        previousState: finalizedResult.state!,
        previousFinalityState: null,
        sourceStore: forkSource,
        sourceJournal: finalized.journal,
        ordinal: 2,
        sourceMode,
      });
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          finalizedResult.state,
          twoStepFork.observation,
          twoStepFork.context,
        ),
      ).toMatchObject({
        action: "reject",
        reasonCodes: ["stale_state"],
      });
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

  it.each<FixtureSourceMode>(["external_providers"])(
    "consumes exact %s W13 recovery, prunes only the orphan W17 lineage, and restarts idempotently",
    (sourceMode) => {
      const scenario = postFinalityProofThreadScenario(sourceMode);
      const indexed = evaluateWatcherProofThreadIndexerV1(
        policy,
        scenario.orphanState,
        scenario.observation,
        scenario.context,
      );
      expect(indexed).toMatchObject({
        action: "accept",
        protocolDecision: "indexed",
        reasonCodes: ["rollback_confirmed"],
        state: {
          journal: scenario.targetJournal,
          pending: null,
        },
      });
      expect(indexed.state?.history).toEqual(scenario.commonState.history);
      expect(indexed.state?.auditHistory).toEqual(
        expect.arrayContaining([
          expect.objectContaining({ status: "orphaned" }),
          expect.objectContaining({ status: "rollback" }),
        ]),
      );
      expect(indexed.state?.transitionHistory.at(-1)).toMatchObject({
        transitionKind: "rollback",
        rollbackResult: {
          action: "rewind_and_replay",
          protocolDecision: "resume_replay",
        },
      });
      const foreignRoles = new Set(["hub_oracle", "state_queue"]);
      const foreign = (store: WatcherDurableStoreV1) => ({
        protocolUtxos: store.protocolUtxos.filter(({ role }) =>
          foreignRoles.has(role),
        ),
        spentProtocolUtxos: store.spentProtocolUtxos.filter(({ role }) =>
          foreignRoles.has(role),
        ),
      });
      expect(foreign(scenario.recovery.nextStore!)).toEqual(
        foreign(scenario.recoveryInput.sourceStore as WatcherDurableStoreV1),
      );

      const restarted = parseWatcherProofThreadStateV1(
        structuredClone(indexed.state),
        policy,
      );
      expect(restarted).toEqual(indexed.state);
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          scenario.orphanState,
          scenario.observation,
          structuredClone(scenario.context),
        ),
      ).toEqual(indexed);
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          restarted,
          scenario.observation,
          scenario.context,
        ),
      ).toMatchObject({
        action: "reject",
        protocolDecision: "hold",
        reasonCodes: ["stale_state"],
        state: null,
      });
      const {
        schemaVersion: _resumeSchema,
        observationDigest: _resumeDigest,
        ...resumeFields
      } = scenario.resumeObservation;
      const resumeObservation = makeWatcherProofThreadObservationV1({
        ...resumeFields,
        predecessorStateDigest: restarted!.stateDigest,
      })!;
      expect(
        evaluateWatcherProofThreadIndexerV1(
          policy,
          restarted,
          resumeObservation,
          scenario.resumeContext,
        ),
      ).toMatchObject({
        action: "accept",
        protocolDecision: "hold",
        reasonCodes: ["cancel_pending"],
        state: {
          pending: {
            transitionKind: "cancel",
          },
        },
      });
    },
    30_000,
  );

  it("rejects forged, mismatched-path, wrong-target, wrong-mode, replacement-mismatched, and duplicate-only W13 recovery", () => {
    const scenario = postFinalityProofThreadScenario("external_providers");
    const evaluate = (
      observation: WatcherProofThreadObservationV1,
      context: WatcherProofThreadPublicContextV1,
    ) =>
      evaluateWatcherProofThreadIndexerV1(
        policy,
        scenario.orphanState,
        observation,
        context,
      );

    const forged = structuredClone(
      scenario.context,
    ) as WatcherProofThreadPublicContextV1;
    const forgedResult = forged.rollbackAuthority!.result as Mutable;
    forgedResult.nextStoreDigest = h32("ff");
    const { resultDigest: _forgedDigest, ...forgedCanonical } = forgedResult;
    forgedResult.resultDigest = digest(forgedCanonical);
    expect(evaluate(scenario.observation, forged)).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const mismatchedPath = structuredClone(
      scenario.context,
    ) as WatcherProofThreadPublicContextV1;
    const mismatchedInput = mismatchedPath.rollbackAuthority!
      .context as Mutable;
    mismatchedInput.replacementCanonicalPath =
      mismatchedInput.previousCanonicalPath;
    expect(evaluate(scenario.observation, mismatchedPath)).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const wrongMode = structuredClone(
      scenario.context,
    ) as WatcherProofThreadPublicContextV1;
    (wrongMode.rollbackAuthority!.context as Mutable).policy = {
      ...finalityPolicy,
      sourceMode: "local_node",
    };
    expect(evaluate(scenario.observation, wrongMode)).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const {
      schemaVersion: _observationSchema,
      observationDigest: _observationDigest,
      ...observationFields
    } = scenario.observation;
    const wrongTarget = makeWatcherProofThreadObservationV1({
      ...observationFields,
      rollbackTargetStateDigest: scenario.orphanState.stateDigest,
    })!;
    expect(evaluate(wrongTarget, scenario.context)).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_mismatch"],
    });
    const commonObservation = scenario.commonState.history.at(-1)!.observation;
    const wrongReplacement = makeWatcherProofThreadObservationV1({
      ...observationFields,
      pointDigest: commonObservation.pointDigest,
      blockHash: commonObservation.blockHash,
      slot: commonObservation.slot,
      blockNo: commonObservation.blockNo,
      publicInputDigest: commonObservation.publicInputDigest,
      sourceObservationDigest: commonObservation.sourceObservationDigest,
      chainPointId: commonObservation.chainPointId,
    })!;
    expect(evaluate(wrongReplacement, scenario.context)).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });

    const duplicateInput: WatcherPostFinalityRecoveryInputV1 = {
      ...scenario.recoveryInput,
      currentStore: scenario.recovery.nextStore,
      previousRecoveryState: scenario.recovery.recoveryState,
    };
    const duplicateRecovery =
      evaluateWatcherPostFinalityRecoveryV1(duplicateInput);
    expect(duplicateRecovery.action).toBe("duplicate_recovery");
    const duplicateOnly = {
      ...scenario.context,
      sourceDurableStore: scenario.recovery.nextStore,
      durableStore: scenario.recovery.nextStore,
      rollbackAuthority: {
        result: duplicateRecovery,
        context: duplicateInput,
      },
    };
    expect(evaluate(scenario.observation, duplicateOnly)).toMatchObject({
      action: "reject",
      reasonCodes: ["rollback_authority_mismatch"],
    });
  }, 30_000);

  it("rejects a self-rehashed legitimate computation output assigned another indexer's durable role", () => {
    const pending = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
    });
    const wrongRoleStore = makeWatcherDurableStoreV1({
      deploymentMarker: authority.marker,
      revision: pending.store.revision,
      records: {
        ...pending.store,
        protocolUtxos: pending.store.protocolUtxos.map((durable) =>
          durable.outRef === `${pending.fixture.txHash}#0`
            ? { ...durable, role: "settlement" as const }
            : durable,
        ),
      },
    });
    const {
      schemaVersion: _observationSchema,
      observationDigest: _observationDigest,
      ...observationFields
    } = pending.observation;
    const wrongRoleObservation = makeWatcherProofThreadObservationV1({
      ...observationFields,
      durableStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(wrongRoleStore),
      ),
    })!;
    expect(
      evaluateWatcherProofThreadIndexerV1(policy, null, wrongRoleObservation, {
        ...pending.context,
        durableStore: wrongRoleStore,
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });
  });

  it("rejects inserting an ordinary settlement role while retaining a signed W14 DA attestation", () => {
    const fixture = initTransaction();
    const seed = baseStore(fixture);
    const priorChainPointId = seed.chainPoints[0]!.chainPointId;
    const withProtocolUtxo = (
      durable: WatcherProtocolUtxoV1,
    ): WatcherDurableStoreV1 =>
      makeWatcherDurableStoreV1({
        deploymentMarker: authority.marker,
        revision: seed.revision,
        records: {
          ...seed,
          protocolUtxos: [...seed.protocolUtxos, durable],
        },
      });

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
    expect(initPendingResult.action).toBe("accept");
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
    expect(initFinalResult.action).toBe("accept");
    const cancelFixture = cancelTransaction(initFinal.journal);
    const cancelSource = addSubmittedTransaction(
      initFinal.store,
      cancelFixture,
      transitionIds(7).submissionId,
    );
    const cancel = transitionStage({
      transitionKind: "cancel",
      fixture: cancelFixture,
      phase: "pending",
      previousState: initFinalResult.state!,
      previousFinalityState: null,
      sourceStore: cancelSource,
      sourceJournal: initFinal.journal,
      ordinal: 7,
    });
    const hostileStore = makeWatcherDurableStoreV1({
      deploymentMarker: authority.marker,
      revision: cancel.store.revision,
      records: {
        ...cancel.store,
        protocolUtxos: [
          ...cancel.store.protocolUtxos,
          protocolUtxo(
            `${cancelFixture.txHash}#0`,
            "settlement",
            cancel.block.chainPoint.chainPointId,
            cancelFixture.outputs[0]!.to_canonical_cbor_hex(),
          ),
        ],
      },
    });
    const {
      schemaVersion: _hostileSchema,
      observationDigest: _hostileDigest,
      ...hostileFields
    } = cancel.observation;
    const hostileObservation = makeWatcherProofThreadObservationV1({
      ...hostileFields,
      durableStoreDigest: watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(hostileStore),
      ),
    })!;
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        initFinalResult.state,
        hostileObservation,
        {
          ...cancel.context,
          durableStore: hostileStore,
        },
      ),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["malformed_public_context"],
    });

    const headerHash = h28("da");
    const daDatum = Data.to(
      {
        header_hash: headerHash,
        availability_commitment: buildDaAvailabilityCommitmentV1({
          deploymentIdentity: h28("de"),
          headerHash,
          payload: Uint8Array.of(1),
          bondOwner: h28("df"),
          responseGeometry: availabilityResponseGeometryV1({
            chunkByteLength: 14_020,
            trancheByteLength: 4_194_304,
            maxTrancheCount: 16,
          }),
        }),
        da_threshold: 1n,
        committee_signers_hash: h32("db"),
        rescue_beneficiary: {
          paymentCredential: { PublicKeyCredential: [h28("dc")] },
          stakeCredential: null,
        },
        attested_signers: `${"01"}${"00".repeat(31)}`,
        attestation_count: 1n,
      },
      DaAttestationDatum,
    );
    const daOutput = outputWithToken(
      scriptAddress(applied.daAttestationSpend!),
      applied.daAttestationMint!,
      `${DA_ATTESTATION_ASSET_NAME_PREFIX}${headerHash}`,
      daDatum,
    );
    const legitimateSource = withProtocolUtxo(
      protocolUtxo(
        `${h32("3a")}#0`,
        "proof_thread",
        priorChainPointId,
        daOutput.to_canonical_cbor_hex(),
      ),
    );
    const legitimate = initStage({
      phase: "pending",
      previousState: null,
      previousFinalityState: null,
      sourceStore: legitimateSource,
    });
    expect(
      evaluateWatcherProofThreadIndexerV1(
        policy,
        null,
        legitimate.observation,
        legitimate.context,
      ),
    ).toMatchObject({
      action: "accept",
      reasonCodes: ["init_pending"],
    });
  });
});

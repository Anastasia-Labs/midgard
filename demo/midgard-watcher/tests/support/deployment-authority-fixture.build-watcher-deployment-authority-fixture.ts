import { generateKeyPairSync, sign } from "node:crypto";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_STEP_NAMES,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  daBondManifestAmounts,
  DEPLOYMENT_PROFILE_DIGESTS,
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";

import {
  makeWatcherDeploymentIdentitySignaturePayload,
  verifyWatcherDeploymentIdentity,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicy,
} from "../../src/runtime/deployment-identity.js";
import {
  addWatcherHistoryFixtureMetadata,
  DA_SIGNERS_HASH,
  deepFreezeFixture,
  h28,
  h32,
  makeWatcherAuthorityContracts,
  type MutableRecord,
  NATIVE_SCRIPT_CBOR,
  NATIVE_SCRIPT_HASH,
  sha256,
  WATCHER_AUTHORITY_BLUEPRINT_HASH,
  WATCHER_AUTHORITY_PROGRAM_COMMITMENTS,
  WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  type WatcherDeploymentAuthorityFixtureOptions,
} from "./deployment-authority-fixture.build-watcher-authority-contracts.js";

const buildWatcherDeploymentAuthorityFixture = (
  options: WatcherDeploymentAuthorityFixtureOptions = {},
) => {
  const blueprintHash =
    options.blueprintHash ?? WATCHER_AUTHORITY_BLUEPRINT_HASH;
  const ruleBundleCommitment =
    options.ruleBundleCommitment ?? WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT;
  const programCommitments =
    options.programCommitments ?? WATCHER_AUTHORITY_PROGRAM_COMMITMENTS;
  const { contracts, fraudProofCatalogue, referenceScripts } =
    options.contractSet ?? makeWatcherAuthorityContracts();
  const parameters = WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS;
  const oneShot = parseOutRefLabel(
    options.hubOracleOneShotOutRef ?? `${h32("11")}#0`,
  );
  const hubOracleOneShot = {
    ...oneShot,
    outRef: `${oneShot.txHash}#${oneShot.outputIndex.toString()}`,
    status: "consumed_by_init",
  };
  addWatcherHistoryFixtureMetadata(
    contracts,
    {
      txHash: hubOracleOneShot.txHash,
      outputIndex: hubOracleOneShot.outputIndex,
    },
    options.eventHistoryRecipe,
  );
  const daIdentity = {
    committeeVkeys: [h32("44")],
    committeeSignersHash: DA_SIGNERS_HASH,
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
      envelopeEncoding: "identity",
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS,
      retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
    },
  };
  const identity: MutableRecord = {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    deploymentProfile:
      options.network === "Custom"
        ? DEPLOYMENT_PROFILES["local-devnet-testing"]
        : SELECTED_DEPLOYMENT_PROFILE,
    deploymentProfileDigest:
      options.network === "Custom"
        ? DEPLOYMENT_PROFILE_DIGESTS["local-devnet-testing"]
        : SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    network: options.network ?? "Preprod",
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: computeDeploymentManifestJsonDigest(parameters),
    },
    genesis: {
      headerHash: h28("00"),
      utxoSetDigest: computeDeploymentManifestJsonDigest([]),
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
      tokenNames: DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts,
    referenceScripts,
    da: daIdentity,
    artifacts: {
      blueprintHash,
    },
    steps: Object.fromEntries(
      DEPLOYMENT_MANIFEST_STEP_NAMES.map((stepName) => [
        stepName,
        {
          status:
            stepName === "prepareHubOracleNonce" ||
            stepName === "deployNodeRuntimeReferenceScripts" ||
            stepName === "availabilityRegistration" ||
            stepName === "initProtocol"
              ? "complete"
              : "pending",
        },
      ]),
    ),
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    availabilityChallenge: {
      responseClasses: {
        smallPayloadMaxBytes: 65_536,
        smallResponseWindowMs:
          SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms,
        fullPayloadMaxBytes: 67_108_864,
        fullResponseWindowMs:
          SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms,
      },
      responseGeometry: {
        chunkByteLength: 14_020,
        trancheByteLength: 4_194_304,
        maxTrancheCount: 16,
      },
      ...daBondManifestAmounts(),
      challengerBondLovelace: 10_000_000_000,
      maxOpenFeeLovelace: 500_000,
      maxPublicationFeeLovelace: 500_000,
      maxSettlementFeeLovelace: 500_000,
      maxCloseFeeLovelace: 1_000_000,
      maxTimeoutFeeLovelace: 1_200_000,
    },
  };
  const manifestId = computeDeploymentManifestId(identity);
  const manifest: MutableRecord = {
    ...identity,
    manifestId,
  };
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    fundingProfileBundleDigest:
      options.fundingProfileBundleDigest ?? "ab".repeat(32),
    ruleBundleCommitment,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(daIdentity),
    },
    artifacts: {
      blueprintHash,
    },
  };
  const { privateKey, publicKey } = generateKeyPairSync("ed25519");
  const publicKeySpkiDerHex = publicKey
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = sha256(Buffer.from(publicKeySpkiDerHex, "hex"));
  const signedIdentity: MutableRecord = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
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
    makeWatcherDeploymentIdentitySignaturePayload(manifestId, releaseBindings),
    privateKey,
  ).toString("hex");
  const deploymentPolicy: WatcherDeploymentIdentityPolicy = {
    network: options.network ?? "Preprod",
    hubOracleOneShotOutRef: hubOracleOneShot.outRef,
    appliedScriptHashes: Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((name) => [
        name,
        contracts[name].scriptHash,
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
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
    } as WatcherDeploymentIdentityPolicy["fraudProofCatalogue"],
    ruleBundleCommitment,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    fundingProfileBundleDigest: releaseBindings.fundingProfileBundleDigest,

    blueprintHash,
  };
  const trustRoots = [{ trustRootId, publicKeySpkiDerHex }];
  const marker = makeDeploymentMarker(manifestId);
  const result = verifyWatcherDeploymentIdentity({
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

type WatcherDeploymentAuthorityFixture = ReturnType<
  typeof buildWatcherDeploymentAuthorityFixture
>;

let cachedDefaultWatcherDeploymentAuthorityFixture: WatcherDeploymentAuthorityFixture | null =
  null;

const cloneDefaultWatcherDeploymentAuthorityFixture = (
  fixture: WatcherDeploymentAuthorityFixture,
): WatcherDeploymentAuthorityFixture => {
  const mutable = structuredClone({
    signedIdentity: fixture.signedIdentity,
    policy: fixture.policy,
    trustRoots: fixture.trustRoots,
    marker: fixture.marker,
    contracts: fixture.contracts,
  });
  return {
    ...mutable,
    // The verifier result is deliberately shared: it is frozen and its live
    // authority is module-admitted, so a structural clone would be invalid.
    result: fixture.result,
  };
};

export const makeWatcherDeploymentAuthorityFixture = (
  options: WatcherDeploymentAuthorityFixtureOptions = {},
): WatcherDeploymentAuthorityFixture => {
  if (Object.keys(options).length !== 0) {
    return buildWatcherDeploymentAuthorityFixture(options);
  }
  cachedDefaultWatcherDeploymentAuthorityFixture ??= deepFreezeFixture(
    buildWatcherDeploymentAuthorityFixture(),
  );
  return cloneDefaultWatcherDeploymentAuthorityFixture(
    cachedDefaultWatcherDeploymentAuthorityFixture,
  );
};

/** The default-parameter authority. */
export const makeDeploymentAuthority = () =>
  makeWatcherDeploymentAuthorityFixture();

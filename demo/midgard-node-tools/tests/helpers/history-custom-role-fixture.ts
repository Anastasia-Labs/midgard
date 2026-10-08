import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { h32ForOrdinal } from "@al-ft/midgard-test-support/hex";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
} from "midgard-node/commands/contract-deployment-info";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestValue,
} from "midgard-node/deployment-manifest";
import { TEST_AVAILABILITY_CHALLENGE } from "midgard-node/tests/helpers/availability-challenge";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "midgard-node/tests/helpers/cardano-protocol-parameters";
import { loadRealMidgardContractsForTest } from "midgard-node/tests/helpers/real-midgard-contracts";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "midgard-node/transactions/initialization";
import * as watcher from "midgard-watcher";

import { makeLayout } from "../../src/devnet-stack/layout.js";
import { ensureHistoryProviders } from "../../src/devnet-stack/watcher-history.js";
import {
  ensureWatcherReleaseBundle,
  releasePaths,
} from "../../src/devnet-stack/watcher-release.js";
const makeDevnetManifest = async (
  blueprintHash: string,
  genesisHeaderHash = "00".repeat(28),
): Promise<DeploymentManifest> => {
  const oneShot = { txHash: "ab".repeat(32), outputIndex: 0 };
  const contracts = await loadRealMidgardContractsForTest(oneShot);
  const nativeScriptCbor = "820500";
  const referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo = {
    policyId: validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    }),
    nativeScript: {
      type: "Native",
      cborHex: nativeScriptCbor,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 1,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: { required: true, rule: "test fixture" },
  };
  const referenceScriptOutRefs = new Map(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, index) => [
        contractName,
        { txHash: h32ForOrdinal(index + 1), outputIndex: 0 },
      ],
    ),
  );
  const catalogue = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    ),
  );
  const daVkey = "44".repeat(32);
  return parseDeploymentManifestValue(
    buildDeploymentManifest(
      buildContractDeploymentInfoFromContracts(
        contracts,
        referenceScriptAuthPolicy,
        referenceScriptOutRefs,
        catalogue,
      ),
      {
        network: SELECTED_DEPLOYMENT_PROFILE.network,
        availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
        economics: SELECTED_DEPLOYMENT_PROFILE.economics,
        cardanoProtocolParameters: {
          snapshot: TEST_CARDANO_PROTOCOL_PARAMETERS,
          digest: computeDeploymentManifestJsonDigest(
            TEST_CARDANO_PROTOCOL_PARAMETERS,
          ),
        },
        genesis: {
          headerHash: genesisHeaderHash,
          utxoSetDigest: computeDeploymentManifestJsonDigest(
            normalizeDeploymentManifestJsonValue([]),
          ),
        },
        da: {
          committeeVkeys: [daVkey],
          committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash(
            [daVkey],
          ),
          threshold: 1,
          transportProfile: {
            protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
            runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
            envelopeEncoding: "identity",
            zstdLevel: 3,
            limits: DA_TRANSPORT_LIMITS,
            retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
          },
        },
        artifacts: { blueprintHash },
        referenceScriptDeployAddress: "addr_test1reference",
        hubOracleOneShotTxHash: oneShot.txHash,
        hubOracleOneShotOutputIndex: oneShot.outputIndex,
        hubOracleOneShotStatus: "consumed_by_init",
        steps: {
          initProtocol: { status: "complete" },
          availabilityRegistration: { status: "complete" },
        },
      },
    ),
  );
};

const write = (path: string, value: unknown) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(value));
};
const run = async () => {
  const root = process.argv[2];
  if (root === undefined) throw Error("owned synthetic root absent");
  const layout = makeLayout(root);
  const blueprint = readFileSync(
    join(layout.repoRoot, "onchain/aiken/plutus.json"),
  );
  mkdirSync(dirname(layout.blueprint), { recursive: true });
  writeFileSync(layout.blueprint, blueprint);
  writeFileSync(
    `${layout.blueprint}.deployment.json`,
    readFileSync(
      join(layout.repoRoot, "onchain/aiken/plutus.json.deployment.json"),
    ),
  );
  const manifest = await makeDevnetManifest(
    createHash("sha256").update(blueprint).digest("hex"),
  );
  if (
    manifest.network !== "Custom" ||
    !((name: string) => name === "local-devnet-testing")(
      SELECTED_DEPLOYMENT_PROFILE.name,
    )
  )
    throw Error("declared Custom fixture profile required");
  write(layout.contractManifest, manifest);
  await ensureWatcherReleaseBundle(layout, watcher, manifest);
  const paths = releasePaths(layout);
  const verified = await watcher.loadWatcherVerifiedDeploymentAuthority({
    path: paths.authority,
    ruleBundlePath: paths.rules,
  });
  const release = await watcher
    .watcherDeploymentReleaseFinalityAuthority(verified.deploymentIdentity)
    .verifyForWorkflow({ deploymentFingerprint: manifest.manifestId });
  const roster = ensureHistoryProviders(
    layout,
    {
      runId: "synthetic-history-binding",
      portOffset: 0,
      composeProject: "synthetic-history",
      networkMagic: 1,
      ogmiosPort: 2337,
      kupoPort: 2442,
      postgresPort: 1,
      postgresUser: "unused",
      postgresPassword: "unused-synthetic",
      postgresDatabase: "unused",
      cardanoImage: "unused",
      postgresImage: "unused",
    },
    release,
    manifest.manifestId,
  );
  const config = JSON.parse(
    readFileSync(
      join(layout.watcherRoot, "watcher-process.example.json"),
      "utf8",
    ),
  );
  Object.assign(config, {
    watcherRuntimeConfigPath: layout.watcherRuntimeConfig,
    deploymentAuthorityPath: paths.authority,
    ruleBundlePath: paths.rules,
    fundingProfileBundlePath: paths.fundingProfiles,
    l1NodeTransportBinaryPath: join(layout.bin, "midgard-l1-node-transport"),
    workflowJournalDirectory: join(root, "workflows"),
  });
  config.availability.journalPath = join(root, "availability.sqlite");
  Object.assign(config.faultProofInfrastructure, {
    manifestPath: paths.manifest,
    blueprintPath: paths.blueprint,
    deploymentInfoPath: paths.deploymentInfo,
    historicalNativeScriptHistory: roster,
  });
  write(layout.watcherProcessConfig, config);
  console.log("PASS actual production Custom manifest/release signed fixture");
};
void run().catch((error) => {
  console.error(
    error instanceof Error ? error.message : "synthetic fixture failed",
  );
  process.exitCode = 1;
});

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as watcher from "midgard-watcher";
import { watcherConfigValue } from "midgard-watcher/tests/runtime/process-config.watcher-config-value";
import {
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
} from "midgard-watcher/tests/support/deployment-authority-fixture";

import { makeLayout, type RunEnv } from "../../src/devnet-stack/layout.js";
import { ensureHistoryProviders } from "../../src/devnet-stack/watcher-history.js";
import { releasePaths } from "../../src/devnet-stack/watcher-release.js";

const write = (path: string, value: unknown) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(value));
};
const generate = async (root: string, portOffset: number) => {
  const layout = makeLayout(root);
  const run: RunEnv = {
    runId: "synthetic-history-binding",
    composeProject: "synthetic-history",
    networkMagic: 1,
    portOffset,
    ogmiosPort: 2337 + portOffset,
    kupoPort: 2442 + portOffset,
    postgresPort: 5433,
    postgresUser: "unused",
    postgresPassword: "unused-synthetic",
    postgresDatabase: "unused",
    cardanoImage: "unused",
    postgresImage: "unused",
  };
  const construction = makeWatcherDeploymentAuthorityFixture();
  const ruleBundle = watcher.makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: construction.result.manifestId,
      network: construction.result.network,
      blueprintHash: construction.result.blueprintHash,
      programCommitments: construction.result.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  const fixture = makeWatcherDeploymentAuthorityFixture({
    ruleBundleCommitment:
      watcher.computeWatcherRuleBundleCommitment(ruleBundle),
  });
  const manifest = verifyFinalizedDeploymentManifest(
    fixture.signedIdentity.manifest,
  );
  const paths = releasePaths(layout);
  write(layout.contractManifest, manifest);
  write(paths.manifest, manifest);
  write(paths.authority, {
    signedIdentity: fixture.signedIdentity,
    policy: fixture.policy,
    trustRoots: fixture.trustRoots,
    durableMarker: fixture.marker,
  });
  write(paths.rules, ruleBundle);
  // Fixture builder and compiled runtime have independent identity brands.
  // Obtain actual runtime authority by its real signed-file loader.
  const verified = await watcher.loadWatcherVerifiedDeploymentAuthority({
    path: paths.authority,
    ruleBundlePath: paths.rules,
  });
  const release = await watcher
    .watcherDeploymentReleaseFinalityAuthority(verified.deploymentIdentity)
    .verifyForWorkflow({ deploymentFingerprint: manifest.manifestId });
  const roster = ensureHistoryProviders(
    layout,
    run,
    release,
    manifest.manifestId,
  );
  const raw = watcherConfigValue();
  const config = {
    schemaVersion: watcher.WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: raw,
    watcherRuntimeConfigPath: layout.watcherRuntimeConfig,
    deploymentAuthorityPath: paths.authority,
    ruleBundlePath: paths.rules,
    fundingProfileBundlePath: paths.fundingProfiles,
    nativeChainSyncBinaryPath: join(layout.bin, "midgard-chain-sync"),
    trustedHeadAuthorityEndpoint: "http://127.0.0.1:43123",
    operationsEndpoint: "http://127.0.0.1:43124",
    httpBearerSecretSource: {
      kind: "environment",
      variable: "UNUSED_SYNTHETIC_HISTORY_BEARER",
    },
    workflowJournalDirectory: join(root, "workflows"),
    availability: {
      keySource: {
        kind: "environment",
        variable: "UNUSED_SYNTHETIC_AVAILABILITY",
      },
      journalPath: join(root, "availability.sqlite"),
      minimumFundingLovelace: "100000000",
    },
    faultProofInfrastructure: {
      manifestPath: paths.manifest,
      blueprintPath: paths.blueprint,
      deploymentInfoPath: paths.deploymentInfo,
      historicalNativeScriptHistory: roster,
    },
  };
  watcher.parseWatcherProcessConfig(config);
  write(layout.watcherRuntimeConfig, config.watcherConfig);
  write(layout.watcherProcessConfig, config);
};
// `<root> [portOffset]` writes one run directory. `--roots <root>...` writes
// several independent run directories (offset 0) in one process, so a test
// file pays the module load once per batch rather than once per directory.
const run = async () => {
  if (process.argv[2] === "--roots") {
    const roots = process.argv.slice(3);
    if (roots.length === 0) throw Error("synthetic fixture root absent");
    for (const root of roots) {
      await generate(root, 0);
      console.log(`PASS synthetic signed history public evidence ${root}`);
    }
    return;
  }
  const root = process.argv[2];
  if (root === undefined) throw Error("synthetic fixture root absent");
  const portOffset = Number(process.argv[3] ?? 0);
  if (!Number.isSafeInteger(portOffset) || portOffset < 0)
    throw Error("synthetic fixture offset invalid");
  await generate(root, portOffset);
  console.log("PASS synthetic signed history public evidence");
};
void run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});

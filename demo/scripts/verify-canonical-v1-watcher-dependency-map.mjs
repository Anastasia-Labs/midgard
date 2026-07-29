import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { readdir, readFile, readlink } from "node:fs/promises";
import { relative, resolve } from "node:path";

const repositoryRoot = resolve(import.meta.dirname, "../..");
const mapPath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
);
const dependencyMap = JSON.parse(await readFile(mapPath, "utf8"));

const fail = (message) => {
  throw new Error(`Watcher dependency map verification failed: ${message}`);
};

if (
  dependencyMap.schemaVersion !==
  "midgard-canonical-v1-watcher-dependency-map-v1"
) {
  fail("unknown schemaVersion");
}
if (dependencyMap.trustPolicy?.unknownBehavior !== "fail_closed") {
  fail("unknown behavior must fail closed");
}
if (
  dependencyMap.authority?.sourceRevision !==
    "4acf68215c76bbac72c5a7f35962c611ce3b92da" ||
  dependencyMap.authority?.baseRevision !==
    "8bae9403a13124f647f215999848ff5c82784e37"
) {
  fail("authority must bind the checkpoint and tx-validation base revisions");
}
const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const trackedContentTreeSha256 = async () => {
  const excludedPaths = [
    "GOAL_PROGRESS.md",
    "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
  ];
  if (
    JSON.stringify(dependencyMap.authority?.contentTreeExclusions) !==
    JSON.stringify(excludedPaths)
  ) {
    fail("authority content-tree exclusions must remain exact");
  }
  let trackedIndexBytes;
  try {
    trackedIndexBytes = execFileSync("git", ["ls-files", "-s", "-z"], {
      cwd: repositoryRoot,
    });
  } catch (error) {
    // Some read-only sandboxes report EPERM after returning a complete,
    // successful child-process result. Preserve the exact successful bytes.
    if (error.status !== 0 || error.stdout === undefined) {
      throw error;
    }
    trackedIndexBytes = error.stdout;
  }
  const trackedPaths = trackedIndexBytes
    .toString("utf8")
    .split("\0")
    .filter((entry) => entry !== "")
    .map((entry) => {
      const match = /^(100644|100755|120000|160000) ([0-9a-f]{40}) 0\t(.+)$/u.exec(
        entry,
      );
      if (match === null) {
        fail("tracked index contains a non-stage-zero or unsupported entry");
      }
      return {
        mode: match[1],
        objectId: match[2],
        path: match[3],
      };
    })
    .filter(({ path }) => !excludedPaths.includes(path))
    .sort((left, right) => left.path.localeCompare(right.path));
  const entries = await Promise.all(
    trackedPaths.map(async ({ mode, objectId, path }) => ({
      path,
      mode,
      sha256:
        mode === "160000"
          ? sha256(Buffer.from(`gitlink:${objectId}`, "utf8"))
          : mode === "120000"
            ? sha256(Buffer.from(await readlink(resolve(repositoryRoot, path))))
            : sha256(await readFile(resolve(repositoryRoot, path))),
    })),
  );
  return sha256(
    JSON.stringify({
      domain: "midgard-reviewed-integration-content-tree-v1",
      entries,
    }),
  );
};
if (
  dependencyMap.authority?.resultContentTreeSha256 !==
  (await trackedContentTreeSha256())
) {
  fail("authority result content tree is stale");
}

const requiredIds = [
  "public_da",
  "proof_bundle",
  "validation",
  "proof_tooling",
  "deployment_manifest",
  "l1_provider",
  "state_queue",
  "correction_removal",
];
const dependencies = dependencyMap.dependencies;
if (!Array.isArray(dependencies)) {
  fail("dependencies must be an array");
}
const byId = new Map(dependencies.map((entry) => [entry.id, entry]));
if (byId.size !== dependencies.length) {
  fail("dependency ids must be unique");
}
for (const id of requiredIds) {
  const entry = byId.get(id);
  if (entry === undefined) {
    fail(`missing dependency ${id}`);
  }
  if (!Array.isArray(entry.sourcePaths) || entry.sourcePaths.length === 0) {
    fail(`${id} must name source paths`);
  }
  if (
    entry.sourceSha256 === null ||
    typeof entry.sourceSha256 !== "object" ||
    Array.isArray(entry.sourceSha256) ||
    JSON.stringify(Object.keys(entry.sourceSha256).sort()) !==
      JSON.stringify([...entry.sourcePaths].sort())
  ) {
    fail(`${id} must hash-bind every source path exactly`);
  }
  if (!Array.isArray(entry.sourceSymbols) || entry.sourceSymbols.length === 0) {
    fail(`${id} must name source symbols`);
  }
  if (
    !Array.isArray(entry.remainingTasks) ||
    entry.remainingTasks.length === 0
  ) {
    fail(`${id} must map remaining tasks`);
  }
  if (
    typeof entry.watcherBoundary !== "string" ||
    entry.watcherBoundary === ""
  ) {
    fail(`${id} must define the watcher boundary`);
  }
  const sourceTexts = [];
  for (const sourcePath of entry.sourcePaths) {
    const absoluteSourcePath = resolve(repositoryRoot, sourcePath);
    if (
      relative(repositoryRoot, absoluteSourcePath).startsWith("..") ||
      entry.sourceSha256[sourcePath] !==
        sha256(await readFile(absoluteSourcePath))
    ) {
      fail(`${id} source hash is stale for ${sourcePath}`);
    }
    sourceTexts.push(await readFile(absoluteSourcePath, "utf8"));
  }
  const combinedSource = sourceTexts.join("\n");
  for (const sourceSymbol of entry.sourceSymbols) {
    const searchableSymbol = sourceSymbol.includes(".")
      ? sourceSymbol.slice(sourceSymbol.lastIndexOf(".") + 1)
      : sourceSymbol;
    if (!combinedSource.includes(searchableSymbol)) {
      fail(
        `${id} source symbol ${sourceSymbol} is absent from its source paths`,
      );
    }
  }
}

const removal = byId.get("correction_removal");
if (
  removal.prohibitedSurface?.symbol !==
    "createHttpStateQueueMutationLeaseCoordinator" ||
  !removal.prohibitedSurface.inputs?.includes("midgardNodeAdminKey")
) {
  fail("operator-admin removal dependency must be explicitly prohibited");
}

const rejected = dependencyMap.explicitlyRejectedDependencies;
if (
  !Array.isArray(rejected) ||
  !rejected.some((entry) => entry.path === "demo/midgard-node/src/database") ||
  !rejected.some(
    (entry) => entry.symbol === "createHttpStateQueueMutationLeaseCoordinator",
  )
) {
  fail("operator-private database and mutation lease must be rejected");
}

if (
  dependencyMap.requiredWatcherPackage?.path !== "demo/midgard-watcher" ||
  dependencyMap.requiredWatcherPackage.workspacePackage !== true ||
  dependencyMap.requiredWatcherPackage.packageManifestPresent !== true ||
  dependencyMap.requiredWatcherPackage.productionSourcePresent !== true
) {
  fail("W00-W02 watcher production sources must exist");
}
const watcherManifest = JSON.parse(
  await readFile(
    resolve(repositoryRoot, "demo/midgard-watcher/package.json"),
    "utf8",
  ),
);
const workspaceManifest = JSON.parse(
  await readFile(resolve(repositoryRoot, "demo/package.json"), "utf8"),
);
const committeeManifest = JSON.parse(
  await readFile(
    resolve(repositoryRoot, "demo/da-committee-node/package.json"),
    "utf8",
  ),
);
if (
  watcherManifest.name !== "midgard-watcher" ||
  watcherManifest.bin?.["midgard-watcher"] !== "./dist/cli.js" ||
  watcherManifest.packageManager !== undefined ||
  workspaceManifest.packageManager !==
    "pnpm@9.15.4+sha512.b2dc20e2fc72b3e18848459b37359a32064663e5627a51e4c74b2c29dd8e8e0491483c3abb40789cfd578bf362fb6ba8261b05f0387d76792ed6e23ea3b1b6a0" ||
  committeeManifest.name !== "da-committee-node" ||
  committeeManifest.bin?.["da-committee-node"] !== "./dist/index.js"
) {
  fail(
    "watcher/committee identities and the workspace package-manager authority must remain exact",
  );
}
const listPackageFiles = async (relativeDirectory) => {
  const discovered = [];
  const walk = async (absoluteDirectory, relativePrefix) => {
    const entries = await readdir(absoluteDirectory, { withFileTypes: true });
    for (const entry of entries) {
      if (
        entry.isDirectory() &&
        (entry.name === "dist" || entry.name === "node_modules")
      ) {
        continue;
      }
      const relativePath =
        relativePrefix === "" ? entry.name : `${relativePrefix}/${entry.name}`;
      if (entry.isDirectory()) {
        await walk(resolve(absoluteDirectory, entry.name), relativePath);
      } else if (entry.isFile()) {
        discovered.push(`${relativeDirectory}/${relativePath}`);
      }
    }
  };
  await walk(resolve(repositoryRoot, relativeDirectory), "");
  return discovered.sort();
};
const declaredWatcherContents = [
  ...(dependencyMap.requiredWatcherPackage?.currentContents ?? []),
].sort();
const actualWatcherContents = await listPackageFiles("demo/midgard-watcher");
if (
  JSON.stringify(declaredWatcherContents) !==
  JSON.stringify(actualWatcherContents)
) {
  fail("requiredWatcherPackage.currentContents must exactly cover the package");
}
if (
  workspaceManifest.scripts?.["watcher:dependency-map:verify"] !==
  "node scripts/verify-canonical-v1-watcher-dependency-map.mjs"
) {
  fail("workspace must expose the canonical watcher dependency-map verifier");
}
const nodeCi = await readFile(
  resolve(repositoryRoot, ".github/workflows/midgard-node-ci.yml"),
  "utf8",
);
for (const requiredCiText of [
  "demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs",
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
  "pnpm --dir demo run watcher:dependency-map:verify",
]) {
  if (!nodeCi.includes(requiredCiText)) {
    fail(`Midgard node CI is missing ${requiredCiText}`);
  }
}
const watcherArchitecture = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/midgard-watcher-architecture.md",
  ),
  "utf8",
);
const watcherAdversarialReview = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/watcher-plan-adversarial-review.md",
  ),
  "utf8",
);
for (const sourceModeDocument of [
  watcherArchitecture,
  watcherAdversarialReview,
]) {
  if (
    !sourceModeDocument.includes("`local_node`") ||
    !sourceModeDocument.includes("`external_providers`") ||
    !sourceModeDocument.includes("watcher-operated")
  ) {
    fail("shipped watcher documents must define both L1-source modes");
  }
}
const watcherSource = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/scaffold.ts"),
  "utf8",
);
if (
  !watcherSource.includes('state: "foundation_incomplete"') ||
  !watcherSource.includes("productionReady: false")
) {
  fail("W00 watcher commands must remain explicitly fail closed");
}
const strictConfiguration =
  dependencyMap.requiredWatcherPackage?.strictConfiguration;
const configBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/config.ts"),
);
const configTestBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/tests/config.test.ts"),
);
const deploymentIdentityBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/deployment-identity.ts"),
);
const deploymentIdentityTestBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/tests/deployment-identity.test.ts",
  ),
);
const durableStoreBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/durable-store.ts"),
);
const durableStoreTestBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/tests/durable-store.test.ts"),
);
const l1AdapterBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/l1-adapter.ts"),
);
const l1AdapterTestBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/tests/l1-adapter.test.ts"),
);
const multiProviderConsistencyBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/src/multi-provider-consistency.ts",
  ),
);
const multiProviderConsistencyTestBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/tests/multi-provider-consistency.test.ts",
  ),
);
const finalityEngineBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/finality-engine.ts"),
);
const finalityEngineTestBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/tests/finality-engine.test.ts"),
);
const rollbackEngineBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/rollback-engine.ts"),
);
const rollbackEngineTestBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/tests/rollback-engine.test.ts"),
);
const ruleBundleBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/rule-bundle-v1.ts"),
);
const ruleBundleTestBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/tests/rule-bundle-v1.test.ts"),
);
const stateQueueIndexerBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/state-queue-indexer.ts"),
);
const stateQueueIndexerTestBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/tests/state-queue-indexer.test.ts",
  ),
);
const userEventIndexerBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/user-event-indexer.ts"),
);
const userEventIndexerTestBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/tests/user-event-indexer.test.ts",
  ),
);
const settlementIndexerBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/settlement-indexer.ts"),
);
const settlementIndexerTestBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/tests/settlement-indexer.test.ts",
  ),
);
const proofThreadIndexerBytes = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/proof-thread-indexer.ts"),
);
const proofThreadIndexerTestBytes = await readFile(
  resolve(
    repositoryRoot,
    "demo/midgard-watcher/tests/proof-thread-indexer.test.ts",
  ),
);
const watcherIndexSource = await readFile(
  resolve(repositoryRoot, "demo/midgard-watcher/src/index.ts"),
  "utf8",
);
if (
  strictConfiguration?.schemaVersion !== "midgard-watcher-config-v1" ||
  strictConfiguration.sourceSha256 !== sha256(configBytes) ||
  strictConfiguration.testSha256 !== sha256(configTestBytes) ||
  JSON.stringify(strictConfiguration.l1SourceModes) !==
    JSON.stringify(["local_node", "external_providers"]) ||
  strictConfiguration.discriminatorPolicy !==
    "explicit_source_mode_required_without_compatibility_inference" ||
  strictConfiguration.localNodePolicy !==
    "one_chain_sync_authority_zero_to_eight_aligned_query_surfaces_no_provider_quorum" ||
  strictConfiguration.externalProviderPolicy !==
    "two_to_four_operationally_independent_provider_operator_endpoint_identities" ||
  strictConfiguration.finalityPolicy?.beforeFinality !== "rewind" ||
  strictConfiguration.finalityPolicy?.afterFinality !== "quarantine" ||
  strictConfiguration.unknownBehavior !== "fail_closed" ||
  strictConfiguration.diagnostics !== "code_and_schema_path_only"
) {
  fail("W01 strict watcher configuration evidence is incomplete or stale");
}
const configSource = configBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_CONFIG_SCHEMA_VERSION",
  "WATCHER_CONFIG_BOUNDS",
  "WatcherL1SourceMode",
  '"local_node"',
  '"external_providers"',
  "parseWatcherConfig",
  "parseWatcherConfigJson",
  "watcherConfigDiagnostic",
]) {
  if (!configSource.includes(requiredSymbol)) {
    fail(`W01 configuration symbol ${requiredSymbol} is absent`);
  }
}
const deploymentIdentity =
  dependencyMap.requiredWatcherPackage?.deploymentIdentity;
if (
  deploymentIdentity?.schemaVersion !==
    "midgard-watcher-signed-deployment-identity-v1" ||
  deploymentIdentity.sourceSha256 !== sha256(deploymentIdentityBytes) ||
  deploymentIdentity.testSha256 !== sha256(deploymentIdentityTestBytes) ||
  deploymentIdentity.signatureAlgorithm !== "ed25519" ||
  deploymentIdentity.signatureDomain !==
    "midgard-watcher-deployment-identity-signature-v1" ||
  deploymentIdentity.trustRootIdentity !== "sha256_spki_der" ||
  JSON.stringify(deploymentIdentity.catalogueCategoryOrder) !==
    JSON.stringify([
      "doubleSpend",
      "nonExistentInput",
      "nonExistentInputNoIndex",
      "invalidRange",
      "transitionTrace",
      "zeroInput",
      "validationTraceDispute",
    ]) ||
  deploymentIdentity.catalogueContractBinding !==
    "exact_category_id_order_and_deployed_script_hash" ||
  deploymentIdentity.unknownBehavior !== "fail_closed" ||
  deploymentIdentity.diagnostics !== "code_and_schema_path_only" ||
  deploymentIdentity.node22FocusedTestsPassed !== 18
) {
  fail("W02 deployment-identity evidence is incomplete or stale");
}
const requiredBindings = [
  "network",
  "hub_oracle_one_shot",
  "consensus_profile",
  "applied_script_hashes",
  "reference_scripts",
  "fraud_proof_catalogue",
  "rule_bundle_commitment",
  "program_commitments",
  "da_mode_and_identity",
  "release_evidence_digest",
  "blueprint_hash",
  "durable_marker",
];
if (
  !Array.isArray(deploymentIdentity.bindings) ||
  deploymentIdentity.bindings.length !== requiredBindings.length ||
  requiredBindings.some(
    (binding, index) => deploymentIdentity.bindings[index] !== binding,
  )
) {
  fail("W02 deployment-identity bindings must be exact and ordered");
}
const deploymentIdentitySource = deploymentIdentityBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION",
  "WATCHER_DEPLOYMENT_IDENTITY_SIGNATURE_DOMAIN",
  "makeWatcherDeploymentIdentitySignaturePayloadV1",
  "verifyWatcherDeploymentIdentityV1",
  "watcherDeploymentIdentityDiagnostic",
]) {
  if (
    !deploymentIdentitySource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W02 deployment-identity symbol ${requiredSymbol} is not public`);
  }
}
if (!deploymentIdentitySource.includes('zeroInput: "fraudProofZeroInput"')) {
  fail("W02 deployment identity must bind the zeroInput catalogue family");
}
const durableStore = dependencyMap.requiredWatcherPackage?.durableStore;
const requiredRecordClasses = [
  "deployment_marker",
  "l1_observations",
  "chain_points",
  "protocol_utxos",
  "spent_protocol_utxos",
  "da_and_proof_inputs",
  "reconstructed_states",
  "decisions",
  "faults",
  "submissions",
  "confirmations",
  "retries",
  "deadlines",
  "correction_results",
];
if (
  durableStore?.schemaVersion !== "midgard-watcher-durable-store-v1" ||
  durableStore.migrationVersion !== 1 ||
  durableStore.sourceSha256 !== sha256(durableStoreBytes) ||
  durableStore.testSha256 !== sha256(durableStoreTestBytes) ||
  durableStore.cachePolicy !== "deterministic_rebuild_from_canonical_records" ||
  durableStore.migrationPolicy !== "fresh_install_atomic_compare_and_swap" ||
  durableStore.unknownBehavior !== "fail_closed" ||
  durableStore.node22FocusedTestsPassed !== 11 ||
  !Array.isArray(durableStore.recordClasses) ||
  durableStore.recordClasses.length !== requiredRecordClasses.length ||
  requiredRecordClasses.some(
    (recordClass, index) => durableStore.recordClasses[index] !== recordClass,
  )
) {
  fail("W03 durable-store evidence is incomplete or stale");
}
const durableStoreSource = durableStoreBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_DURABLE_STORE_V1_SCHEMA_VERSION",
  "parseWatcherDurableStoreV1",
  "journalWatcherProtocolUtxoTransitionV1",
  "rebuildWatcherDurableCachesV1",
  "migrateWatcherDurableStoreV1",
  "WatcherDurableAtomicBackend",
]) {
  if (
    !durableStoreSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W03 durable-store symbol ${requiredSymbol} is not public`);
  }
}
const l1Adapter = dependencyMap.requiredWatcherPackage?.l1Adapter;
if (
  l1Adapter?.providerSchemaVersion !==
    "midgard-watcher-authenticated-l1-provider-v1" ||
  l1Adapter.observationSchemaVersion !==
    "midgard-watcher-l1-block-observation-v1" ||
  l1Adapter.normalizedSchemaVersion !==
    "midgard-watcher-normalized-l1-block-v1" ||
  l1Adapter.sourceSha256 !== sha256(l1AdapterBytes) ||
  l1Adapter.testSha256 !== sha256(l1AdapterTestBytes) ||
  l1Adapter.canonicalFixtureSha256 !==
    "aeecff9e4492846016727cf2d62193f3c9acf9b09246d01d6255e436059d3d94" ||
  JSON.stringify(l1Adapter.sourceModes) !==
    JSON.stringify(["local_node", "external_providers"]) ||
  l1Adapter.identityPolicy !==
    "local_chain_authority_or_external_provider_bound_observation_with_provider_neutral_block_content" ||
  l1Adapter.inputPolicy !== "authenticated_node_derived_l1_only" ||
  l1Adapter.unknownBehavior !== "fail_closed" ||
  l1Adapter.diagnostics !== "code_and_schema_path_only" ||
  l1Adapter.node22FocusedTestsPassed !== 11
) {
  fail("W10 L1-adapter evidence is incomplete or stale");
}
const l1AdapterSource = l1AdapterBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION",
  "WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION",
  "WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION",
  "WATCHER_L1_SOURCE_MODES_V1",
  "makeWatcherL1PublicBytesV1",
  "normalizeWatcherL1BlockV1",
  "encodeWatcherNormalizedL1BlockV1",
  "watcherL1AdapterDiagnostic",
]) {
  if (
    !l1AdapterSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W10 L1-adapter symbol ${requiredSymbol} is not public`);
  }
}
const multiProviderConsistency =
  dependencyMap.requiredWatcherPackage?.multiProviderConsistency;
if (
  multiProviderConsistency?.schemaVersion !==
    "midgard-watcher-multi-provider-consistency-v1" ||
  multiProviderConsistency.sourceSha256 !==
    sha256(multiProviderConsistencyBytes) ||
  multiProviderConsistency.testSha256 !==
    sha256(multiProviderConsistencyTestBytes) ||
  JSON.stringify(multiProviderConsistency.sourceModes) !==
    JSON.stringify(["local_node", "external_providers"]) ||
  multiProviderConsistency.minimumIndependentProvidersByMode?.local_node !==
    1 ||
  multiProviderConsistency.minimumIndependentProvidersByMode
    ?.external_providers !== 2 ||
  multiProviderConsistency.compatibleBlockLag !== 64 ||
  multiProviderConsistency.agreementPolicy !==
    "local_node_chain_sync_authority_with_aligned_query_surfaces_or_two_independent_external_providers_at_compatible_points" ||
  multiProviderConsistency.externalProviderBindingPolicy !==
    "exact_W01_provider_operator_endpoint_https_transport_allowlist" ||
  multiProviderConsistency.lagPolicy !==
    "bounded_lag_pending_protocol_quarantined" ||
  multiProviderConsistency.disagreementPolicy !==
    "fork_content_identity_network_or_shape_quarantined" ||
  multiProviderConsistency.unknownBehavior !== "fail_closed" ||
  multiProviderConsistency.diagnostics !== "deterministic_value_free_codes" ||
  multiProviderConsistency.node22FocusedTestsPassed !== 15
) {
  fail("W11 multi-provider consistency evidence is incomplete or stale");
}
const multiProviderConsistencySource =
  multiProviderConsistencyBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION",
  "WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_BOUNDS",
  "WATCHER_MULTI_PROVIDER_REASON_CODES_V1",
  "WATCHER_MULTI_PROVIDER_ALERT_CODES_V1",
  "evaluateWatcherMultiProviderConsistencyV1",
]) {
  if (
    !multiProviderConsistencySource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W11 multi-provider symbol ${requiredSymbol} is not public`);
  }
}
if (!multiProviderConsistencySource.includes("externalProviderBindings")) {
  fail("W11 consistency evidence must bind configured external providers");
}
const finalityEngine = dependencyMap.requiredWatcherPackage?.finalityEngine;
if (
  finalityEngine?.policySchemaVersion !==
    "midgard-watcher-finality-policy-v1" ||
  finalityEngine.stateSchemaVersion !== "midgard-watcher-finality-state-v1" ||
  finalityEngine.resultSchemaVersion !== "midgard-watcher-finality-result-v1" ||
  finalityEngine.sourceSha256 !== sha256(finalityEngineBytes) ||
  finalityEngine.testSha256 !== sha256(finalityEngineTestBytes) ||
  finalityEngine.confirmationDepthPolicy !== "release_and_deployment_bound" ||
  finalityEngine.firstVisibilityPolicy !== "always_pending" ||
  finalityEngine.preFinalityRollbackPolicy !==
    "deterministic_release_bound_rewind" ||
  finalityEngine.postFinalityRollbackPolicy !==
    "durable_quarantine_incident_preserving_finalized_binding" ||
  finalityEngine.consistencyPolicy !==
    "exact_source_mode_bound_W11_agreement_required" ||
  finalityEngine.externalProviderPolicy !==
    "exact_W01_policy_match_for_every_W11_external_provider_binding" ||
  finalityEngine.unknownBehavior !== "fail_closed" ||
  finalityEngine.diagnostics !== "deterministic_value_free_codes" ||
  finalityEngine.node22FocusedTestsPassed !== 22
) {
  fail("W12 finality-engine evidence is incomplete or stale");
}
const finalityEngineSource = finalityEngineBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_FINALITY_POLICY_V1_SCHEMA_VERSION",
  "WATCHER_FINALITY_STATE_V1_SCHEMA_VERSION",
  "WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_FINALITY_V1_BOUNDS",
  "parseWatcherFinalityPolicyV1",
  "parseWatcherFinalityStateV1",
  "makeWatcherFinalityPolicyV1",
  "evaluateWatcherFinalityV1",
]) {
  if (
    !finalityEngineSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W12 finality-engine symbol ${requiredSymbol} is not public`);
  }
}
if (!finalityEngineSource.includes("externalProviderBindingsMatchPolicy")) {
  fail("W12 finality must match W11 provider bindings to W01 policy");
}
const rollbackEngine = dependencyMap.requiredWatcherPackage?.rollbackEngine;
if (
  rollbackEngine?.stateSchemaVersion !== "midgard-watcher-rollback-state-v1" ||
  rollbackEngine.incidentSchemaVersion !==
    "midgard-watcher-rollback-incident-v1" ||
  rollbackEngine.resultSchemaVersion !== "midgard-watcher-rollback-result-v1" ||
  rollbackEngine.sourceSha256 !== sha256(rollbackEngineBytes) ||
  rollbackEngine.testSha256 !== sha256(rollbackEngineTestBytes) ||
  rollbackEngine.status !== "PASS" ||
  rollbackEngine.independentAudit !==
    "PASS_all_original_and_residual_hostile_probes" ||
  rollbackEngine.rewindPolicy !==
    "deterministic_dependency_cascade_with_shared_input_retention_and_orphan_consumption_restoration" ||
  rollbackEngine.restartPolicy !==
    "externally_bootstrapped_bounded_exact_transition_replay" ||
  rollbackEngine.postFinalityPolicy !==
    "durable_quarantine_incident_preserving_finalized_binding_and_store" ||
  rollbackEngine.inputPolicy !==
    "exact_source_mode_bound_W10_evidence_W11_result_W12_transition_W03_store_and_external_bootstrap" ||
  rollbackEngine.unknownBehavior !== "fail_closed" ||
  rollbackEngine.diagnostics !== "deterministic_value_free_codes" ||
  rollbackEngine.node22FocusedTestsPassed !== 19
) {
  fail("W13 rollback-engine evidence is incomplete or stale");
}
const rollbackEngineSource = rollbackEngineBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_V1_BOUNDS",
  "WATCHER_ROLLBACK_REASON_CODES_V1",
  "WATCHER_ROLLBACK_ALERT_CODES_V1",
  "makeWatcherRollbackBootstrapStateV1",
  "parseWatcherRollbackStateV1",
  "parseWatcherRollbackResultV1",
  "evaluateWatcherRollbackV1",
]) {
  if (
    !rollbackEngineSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W13 rollback-engine symbol ${requiredSymbol} is not public`);
  }
}
if (!rollbackEngineSource.includes("externalProviderBindings")) {
  fail("W13 rollback provenance must retain W11 external provider bindings");
}
const stateQueueIndexer =
  dependencyMap.requiredWatcherPackage?.stateQueueIndexer;
if (
  stateQueueIndexer?.status !== "PASS" ||
  stateQueueIndexer.independentAudit !==
    "PASS_remediated_original_and_spent_journal_hostile_probes" ||
  stateQueueIndexer.policySchemaVersion !==
    "midgard-watcher-state-queue-indexer-policy-v1" ||
  stateQueueIndexer.snapshotSchemaVersion !==
    "midgard-watcher-state-queue-snapshot-v1" ||
  stateQueueIndexer.observationSchemaVersion !==
    "midgard-watcher-state-queue-observation-v1" ||
  stateQueueIndexer.stateSchemaVersion !==
    "midgard-watcher-state-queue-indexer-state-v1" ||
  stateQueueIndexer.resultSchemaVersion !==
    "midgard-watcher-state-queue-indexer-result-v1" ||
  stateQueueIndexer.sourceSha256 !== sha256(stateQueueIndexerBytes) ||
  stateQueueIndexer.testSha256 !== sha256(stateQueueIndexerTestBytes) ||
  stateQueueIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W02_deployment_W03_store_W10_observations_with_recomputed_W11_consistency_and_W12_finality_plus_W13_rollback" ||
  stateQueueIndexer.queuePolicy !==
    "decode_and_index_node_accepted_canonical_transaction_output_and_datum_bytes_without_replaying_Cardano_validator_semantics" ||
  stateQueueIndexer.rollbackPolicy !==
    "exact_W13_active_and_spent_journal_rewind_restart_replay_and_duplicate_hold" ||
  stateQueueIndexer.unknownBehavior !== "fail_closed" ||
  stateQueueIndexer.diagnostics !== "deterministic_value_free_codes" ||
  stateQueueIndexer.node22FocusedTestsPassed !== 15
) {
  fail("W14 state-queue-indexer evidence is incomplete or stale");
}
const stateQueueIndexerSource = stateQueueIndexerBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_STATE_QUEUE_INDEXER_POLICY_V1_SCHEMA_VERSION",
  "WATCHER_STATE_QUEUE_SNAPSHOT_V1_SCHEMA_VERSION",
  "WATCHER_STATE_QUEUE_OBSERVATION_V1_SCHEMA_VERSION",
  "WATCHER_STATE_QUEUE_INDEXER_STATE_V1_SCHEMA_VERSION",
  "WATCHER_STATE_QUEUE_INDEXER_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_STATE_QUEUE_INDEXER_V1_BOUNDS",
  "makeWatcherStateQueueIndexerPolicyV1",
  "parseWatcherStateQueueIndexerPolicyV1",
  "makeWatcherStateQueueObservationV1",
  "parseWatcherStateQueueIndexerStateV1",
  "evaluateWatcherStateQueueIndexerV1",
  "parseWatcherStateQueueIndexerResultV1",
]) {
  if (
    !stateQueueIndexerSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W14 state-queue-indexer symbol ${requiredSymbol} is not public`);
  }
}
const userEventIndexer = dependencyMap.requiredWatcherPackage?.userEventIndexer;
if (
  userEventIndexer?.status !== "PASS" ||
  userEventIndexer.independentAudit !==
    "PASS_source_replay_and_residual_boundary_review" ||
  userEventIndexer.policySchemaVersion !==
    "midgard-watcher-user-event-indexer-policy-v1" ||
  userEventIndexer.snapshotSchemaVersion !==
    "midgard-watcher-user-event-snapshot-v1" ||
  userEventIndexer.observationSchemaVersion !==
    "midgard-watcher-user-event-observation-v1" ||
  userEventIndexer.stateSchemaVersion !==
    "midgard-watcher-user-event-indexer-state-v1" ||
  userEventIndexer.resultSchemaVersion !==
    "midgard-watcher-user-event-indexer-result-v1" ||
  userEventIndexer.sourceSha256 !== sha256(userEventIndexerBytes) ||
  userEventIndexer.testSha256 !== sha256(userEventIndexerTestBytes) ||
  userEventIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W10_observations_with_parent_recomputed_W11_consistency_and_W12_finality_plus_W02_W03_W13" ||
  userEventIndexer.eventPolicy !==
    "exact_deposit_withdrawal_forced_order_NFT_datum_witness_time_content_and_terminal_semantics" ||
  userEventIndexer.finalityPolicy !==
    "independent_active_and_terminal_pending_to_final_transitions" ||
  userEventIndexer.rollbackPolicy !==
    "exact_W13_journal_restoration_suffix_rewind_restart_and_reinclusion" ||
  userEventIndexer.unknownBehavior !== "fail_closed" ||
  userEventIndexer.diagnostics !== "deterministic_value_free_codes" ||
  userEventIndexer.node22FocusedTestsPassed !== 12
) {
  fail("W15 user-event-indexer evidence is incomplete or stale");
}
const userEventIndexerSource = userEventIndexerBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION",
  "WATCHER_USER_EVENT_SNAPSHOT_V1_SCHEMA_VERSION",
  "WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION",
  "WATCHER_USER_EVENT_INDEXER_STATE_V1_SCHEMA_VERSION",
  "WATCHER_USER_EVENT_INDEXER_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_USER_EVENT_INDEXER_V1_BOUNDS",
  "makeWatcherUserEventIndexerPolicyV1",
  "parseWatcherUserEventIndexerPolicyV1",
  "deriveWatcherUserEventObservationV1",
  "parseWatcherUserEventIndexerStateV1",
  "evaluateWatcherUserEventIndexerV1",
  "parseWatcherUserEventIndexerResultV1",
]) {
  if (
    !userEventIndexerSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W15 user-event-indexer symbol ${requiredSymbol} is not public`);
  }
}
const settlementIndexer =
  dependencyMap.requiredWatcherPackage?.settlementIndexer;
if (
  settlementIndexer?.status !== "PASS" ||
  settlementIndexer.independentAudit !==
    "PASS_source_replay_residual_journal_preservation_fix_and_hostile_probes" ||
  settlementIndexer.policySchemaVersion !==
    "midgard-watcher-settlement-indexer-policy-v1" ||
  settlementIndexer.snapshotSchemaVersion !==
    "midgard-watcher-settlement-snapshot-v1" ||
  settlementIndexer.observationSchemaVersion !==
    "midgard-watcher-settlement-observation-v1" ||
  settlementIndexer.stateSchemaVersion !==
    "midgard-watcher-settlement-indexer-state-v1" ||
  settlementIndexer.resultSchemaVersion !==
    "midgard-watcher-settlement-indexer-result-v1" ||
  settlementIndexer.sourceSha256 !== sha256(settlementIndexerBytes) ||
  settlementIndexer.testSha256 !== sha256(settlementIndexerTestBytes) ||
  settlementIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W10_observations_with_recomputed_W11_consistency_W12_finality_and_W13_replacement_anchor_plus_W02_W03" ||
  settlementIndexer.settlementPolicy !==
    "exact_claim_disproof_resolution_reserve_payout_refund_value_and_terminal_semantics" ||
  settlementIndexer.retryPolicy !==
    "bounded_retry_stuck_invalid_identity_and_restart_replay" ||
  settlementIndexer.rollbackPolicy !==
    "exact_W13_journal_restoration_unrelated_archive_preservation_restart_and_reinclusion" ||
  settlementIndexer.unknownBehavior !== "fail_closed" ||
  settlementIndexer.diagnostics !== "deterministic_value_free_codes" ||
  settlementIndexer.node22FocusedTestsPassed !== 17
) {
  fail("W16 settlement-indexer evidence is incomplete or stale");
}
const settlementIndexerSource = settlementIndexerBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_SETTLEMENT_INDEXER_POLICY_V1_SCHEMA_VERSION",
  "WATCHER_SETTLEMENT_SNAPSHOT_V1_SCHEMA_VERSION",
  "WATCHER_SETTLEMENT_OBSERVATION_V1_SCHEMA_VERSION",
  "WATCHER_SETTLEMENT_INDEXER_STATE_V1_SCHEMA_VERSION",
  "WATCHER_SETTLEMENT_INDEXER_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS",
  "makeWatcherSettlementIndexerPolicyV1",
  "parseWatcherSettlementIndexerPolicyV1",
  "makeWatcherSettlementObservationV1",
  "parseWatcherSettlementIndexerStateV1",
  "evaluateWatcherSettlementIndexerV1",
  "parseWatcherSettlementIndexerResultV1",
]) {
  if (
    !settlementIndexerSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W16 settlement-indexer symbol ${requiredSymbol} is not public`);
  }
}
const proofThreadIndexer =
  dependencyMap.requiredWatcherPackage?.proofThreadIndexer;
if (
  proofThreadIndexer?.status !== "PASS" ||
  proofThreadIndexer.independentAudit !==
    "PASS_source_mode_recomputation_and_lifecycle_replay" ||
  proofThreadIndexer.policySchemaVersion !==
    "midgard-watcher-proof-thread-policy-v1" ||
  proofThreadIndexer.journalSchemaVersion !==
    "midgard-watcher-proof-thread-journal-v1" ||
  proofThreadIndexer.layoutSchemaVersion !==
    "midgard-watcher-proof-thread-layout-v1" ||
  proofThreadIndexer.observationSchemaVersion !==
    "midgard-watcher-proof-thread-observation-v1" ||
  proofThreadIndexer.stateSchemaVersion !==
    "midgard-watcher-proof-thread-state-v1" ||
  proofThreadIndexer.resultSchemaVersion !==
    "midgard-watcher-proof-thread-result-v1" ||
  proofThreadIndexer.sourceSha256 !== sha256(proofThreadIndexerBytes) ||
  proofThreadIndexer.testSha256 !== sha256(proofThreadIndexerTestBytes) ||
  proofThreadIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W10_observations_with_recomputed_W11_consistency_W12_finality_and_canonical_W13_replacement_anchor_plus_W02_W03" ||
  proofThreadIndexer.threadPolicy !==
    "deterministic_step_success_proof_token_removal_and_cancellation_lifecycles" ||
  proofThreadIndexer.rollbackPolicy !==
    "exact_W13_journal_rewind_restart_replay_and_reinclusion" ||
  proofThreadIndexer.unknownBehavior !== "fail_closed" ||
  proofThreadIndexer.diagnostics !== "deterministic_value_free_codes" ||
  proofThreadIndexer.node22FocusedTestsPassed !== 7
) {
  fail("W17 proof-thread-indexer evidence is incomplete or stale");
}
const proofThreadIndexerSource = proofThreadIndexerBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_PROOF_THREAD_POLICY_V1_SCHEMA_VERSION",
  "WATCHER_PROOF_THREAD_JOURNAL_V1_SCHEMA_VERSION",
  "WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION",
  "WATCHER_PROOF_THREAD_OBSERVATION_V1_SCHEMA_VERSION",
  "WATCHER_PROOF_THREAD_STATE_V1_SCHEMA_VERSION",
  "WATCHER_PROOF_THREAD_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_PROOF_THREAD_V1_BOUNDS",
  "makeWatcherProofThreadPolicyV1",
  "parseWatcherProofThreadPolicyV1",
  "makeWatcherProofThreadObservationV1",
  "parseWatcherProofThreadStateV1",
  "evaluateWatcherProofThreadIndexerV1",
  "parseWatcherProofThreadResultV1",
]) {
  if (
    !proofThreadIndexerSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W17 proof-thread-indexer symbol ${requiredSymbol} is not public`);
  }
}
const ruleBundle = dependencyMap.requiredWatcherPackage?.ruleBundle;
if (
  ruleBundle?.status !== "PASS" ||
  ruleBundle.independentAudit !==
    "PASS_parent_found_and_closed_forgeable_verified_summary_boundary" ||
  ruleBundle.schemaVersion !== "midgard-watcher-rule-bundle-v1" ||
  ruleBundle.bundleVersion !== 1 ||
  ruleBundle.sourceSha256 !== sha256(ruleBundleBytes) ||
  ruleBundle.testSha256 !== sha256(ruleBundleTestBytes) ||
  ruleBundle.authorityPolicy !==
    "raw_signed_W02_authority_reverified_on_every_security_load" ||
  ruleBundle.featurePolicy !==
    "exact_enabled_canonical_V1_feature_order_no_unknown_or_disabled_features" ||
  ruleBundle.parameterPolicy !==
    "exact_commitment_bound_target_snapshot_and_consensus_limits" ||
  ruleBundle.priorityPolicy !==
    "canonical_transition_and_validation_phase_order_with_stable_first_rejection" ||
  ruleBundle.programPolicy !== "exact_W02_program_commitment_map" ||
  ruleBundle.unknownBehavior !== "fail_closed" ||
  ruleBundle.diagnostics !== "deterministic_code_and_path_only" ||
  ruleBundle.node22FocusedTestsPassed !== 9
) {
  fail("W23 rule-bundle evidence is incomplete or stale");
}
const ruleBundleSource = ruleBundleBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_RULE_BUNDLE_V1_SCHEMA_VERSION",
  "WATCHER_RULE_BUNDLE_V1_VERSION",
  "WATCHER_RULE_BUNDLE_V1_TRANSITION_PRIORITY",
  "WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY",
  "WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION",
  "WatcherRuleBundleV1Error",
  "parseWatcherRuleBundleV1",
  "encodeWatcherRuleBundleV1",
  "computeWatcherRuleBundleV1Commitment",
  "makeWatcherCanonicalRuleBundleV1",
  "loadWatcherRuleBundleV1",
  "verifyWatcherDeploymentIdentityV1",
]) {
  if (
    !ruleBundleSource.includes(requiredSymbol) ||
    (requiredSymbol !== "verifyWatcherDeploymentIdentityV1" &&
      !watcherIndexSource.includes(requiredSymbol))
  ) {
    fail(`W23 rule-bundle symbol ${requiredSymbol} is not public or invoked`);
  }
}
if (
  dependencyMap.requiredWatcherPackage.foundationStatus !==
    "W13_through_W17_and_W23_pass_source_modes_exact" ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W01") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W02") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W03") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W10") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W11") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W12") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W13") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W14") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W15") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W16") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W17") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W23")
) {
  fail(
    "W01-W03, W10-W17, and W23 must be complete without promoting watcher readiness",
  );
}
if (dependencyMap.f30Conclusion?.status !== "pass") {
  fail("F30 conclusion must pass");
}

console.log(
  `Canonical V1 watcher dependency map verified: ${dependencies.length} dependency classes.`,
);

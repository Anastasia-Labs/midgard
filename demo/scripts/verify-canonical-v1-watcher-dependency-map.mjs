import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { resolve } from "node:path";

const repositoryRoot = resolve(import.meta.dirname, "../..");
const execGit = (args, encoding = "buffer") => {
  let output;
  try {
    output = execFileSync("git", args, {
      cwd: repositoryRoot,
      encoding,
      maxBuffer: 128 * 1024 * 1024,
    });
  } catch (error) {
    if (error.status !== 0 || error.stdout === undefined) {
      throw error;
    }
    output = error.stdout;
  }
  if (encoding === "buffer") {
    return Buffer.isBuffer(output) ? output : Buffer.from(output);
  }
  return typeof output === "string"
    ? output
    : Buffer.from(output).toString(encoding);
};
const readIndexedFile = (path, encoding = "buffer") =>
  execGit(["show", `:${path}`], encoding);
const dependencyMap = JSON.parse(
  readIndexedFile(
    "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
    "utf8",
  ),
);

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
const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

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
const requiredTrustById = new Map([
  ["public_da", "public_protocol"],
  ["proof_bundle", "public_protocol"],
  ["validation", "deterministic_local"],
  ["proof_tooling", "mixed_public_and_file_inputs"],
  ["deployment_manifest", "signed_deployment_identity_required"],
  ["l1_provider", "authenticated_cardano_l1"],
  ["state_queue", "authenticated_cardano_l1"],
  ["correction_removal", "public_l1_with_prohibited_optional_dependency"],
]);
const requiredSourcesById = new Map([
  [
    "public_da",
    {
      paths: [
        "demo/midgard-core/src/da-transport.ts",
        "demo/midgard-core/src/da-payload-envelope.ts",
        "demo/da-committee-node/src/da/libp2p/payload-protocols.ts",
        "demo/da-committee-node/src/da/payload.ts",
        "demo/da-committee-node/src/da/libp2p/DaLibp2pNode.ts",
      ],
      symbols: [
        "DaRequestResponseProtocol",
        "DaLibp2pPayloadProtocolHandlers",
        "decodeDaPayloadByHeaderResponseV1Cbor",
        "decodeDaPayloadV1Strict",
        "DaLibp2pNode.request",
      ],
    },
  ],
  [
    "proof_bundle",
    {
      paths: [
        "demo/midgard-core/src/da-transport.ts",
        "demo/da-committee-node/src/da/proof-artifacts.ts",
        "demo/da-committee-node/src/da/libp2p/proof-protocols.ts",
        "demo/midgard-fault-proofs/src/transition-trace/fetch.ts",
        "demo/midgard-fault-proofs/src/transition-trace/reconstruct.ts",
      ],
      symbols: [
        "DaProofArtifactDeriver",
        "DaLibp2pProofProtocolHandlers",
        "DaLibp2pRetainedDaSource",
        "reconstructDaPayloadV1",
      ],
    },
  ],
  [
    "validation",
    {
      paths: [
        "demo/midgard-validation/src/index.ts",
        "demo/midgard-validation/src/phase-a.ts",
        "demo/midgard-validation/src/phase-b.ts",
        "demo/midgard-validation/src/validation-machine.ts",
        "demo/midgard-validation/src/validation-dispute-evidence.ts",
        "demo/midgard-core/src/consensus-validation-v1.ts",
      ],
      symbols: [
        "runPhaseAValidation",
        "runPhaseBValidationWithPatch",
        "buildDeterministicValidationMachineTrace",
        "validateMidgardConsensusV1Tx",
      ],
    },
  ],
  [
    "proof_tooling",
    {
      paths: [
        "demo/midgard-fault-proofs/src/index.ts",
        "demo/midgard-fault-proofs/src/bin.ts",
        "demo/midgard-fault-proofs/src/prepare-double-spend.ts",
        "demo/midgard-fault-proofs/src/prepare-invalid-range.ts",
        "demo/midgard-fault-proofs/src/prepare-non-existent-input.ts",
        "demo/midgard-fault-proofs/src/transition-trace/fetch.ts",
        "demo/midgard-fault-proofs/src/transition-trace/submit.ts",
        "demo/midgard-fault-proofs/src/validation-dispute/index.ts",
        "demo/midgard-fault-proofs/src/validation-dispute/submit.ts",
      ],
      symbols: [
        "prepareDoubleSpendFromFile",
        "prepareInvalidRangeFromFile",
        "prepareNonExistentInputFromFile",
        "DaLibp2pRetainedDaSource",
        "submitTransitionTraceProof",
        "submitValidationDisputeOpen",
      ],
    },
  ],
  [
    "deployment_manifest",
    {
      paths: [
        "demo/midgard-core/src/deployment-manifest-identity-v1.ts",
        "demo/midgard-node/src/deployment-manifest-v1.ts",
        "demo/midgard-node/src/commands/contract-deployment-info.ts",
        "demo/midgard-watcher/src/deployment-identity.ts",
      ],
      symbols: [
        "verifyDeploymentManifestV1Identity",
        "parseDeploymentManifestV1Value",
        "verifyDeploymentManifestAgainstConfig",
        "verifyWatcherDeploymentIdentityV1",
      ],
    },
  ],
  [
    "l1_provider",
    {
      paths: [
        "demo/da-committee-node/src/l1/provider.ts",
        "demo/da-committee-node/src/l1/state-queue-scanner.ts",
        "demo/midgard-sdk/src/state-queue.ts",
        "demo/midgard-watcher/src/l1-adapter.ts",
        "demo/midgard-watcher/src/multi-provider-consistency.ts",
        "demo/midgard-watcher/src/finality-engine.ts",
        "demo/midgard-watcher/src/rollback-engine.ts",
        "demo/midgard-watcher/src/state-queue-indexer.ts",
      ],
      symbols: [
        "StateQueueProvider",
        "LucidStateQueueProvider",
        "MultiStateQueueProvider",
        "scanStateQueue",
        "normalizeWatcherL1BlockV1",
        "evaluateWatcherMultiProviderConsistencyV1",
        "evaluateWatcherFinalityV1",
        "evaluateWatcherRollbackV1",
        "evaluateWatcherStateQueueIndexerV1",
      ],
    },
  ],
  [
    "state_queue",
    {
      paths: [
        "demo/midgard-sdk/src/ledger-state.ts",
        "demo/midgard-sdk/src/state-queue.ts",
        "demo/da-committee-node/src/l1/state-queue-scanner.ts",
        "demo/midgard-node/src/services/state-queue-topology.ts",
        "demo/midgard-watcher/src/durable-store.ts",
        "demo/midgard-watcher/src/finality-engine.ts",
        "demo/midgard-watcher/src/state-queue-indexer.ts",
      ],
      symbols: [
        "fetchSortedStateQueueUTxOs",
        "getStateQueueNodeV1FromStateQueueDatum",
        "scanStateQueue",
        "fetchStateQueueTopologyProgram",
        "migrateWatcherDurableStoreV1",
        "evaluateWatcherFinalityV1",
        "evaluateWatcherStateQueueIndexerV1",
      ],
    },
  ],
  [
    "correction_removal",
    {
      paths: [
        "demo/midgard-fault-proofs/src/remove-fraudulent-block.ts",
        "demo/midgard-sdk/src/state-queue.ts",
        "onchain/aiken/validators/state-queue.ak",
        "onchain/aiken/lib/midgard/operator-directory.ak",
      ],
      symbols: [
        "submitRemoveFraudulentBlock",
        "submitRemoveFraudulentBlockFromFiles",
        "createHttpStateQueueMutationLeaseCoordinator",
      ],
    },
  ],
]);
const requiredSymbolBindingsById = new Map([
  [
    "public_da",
    [
      {
        symbol: "DaRequestResponseProtocol",
        path: "demo/midgard-core/src/da-transport.ts",
      },
      {
        symbol: "DaLibp2pPayloadProtocolHandlers",
        path: "demo/da-committee-node/src/da/libp2p/payload-protocols.ts",
      },
      {
        symbol: "decodeDaPayloadByHeaderResponseV1Cbor",
        path: "demo/midgard-core/src/da-transport.ts",
      },
      {
        symbol: "decodeDaPayloadV1Strict",
        path: "demo/da-committee-node/src/da/payload.ts",
      },
      {
        symbol: "DaLibp2pNode.request",
        path: "demo/da-committee-node/src/da/libp2p/DaLibp2pNode.ts",
        owner: "DaLibp2pNode",
        member: "request",
      },
    ],
  ],
  [
    "proof_bundle",
    [
      {
        symbol: "DaProofArtifactDeriver",
        path: "demo/da-committee-node/src/da/proof-artifacts.ts",
      },
      {
        symbol: "DaLibp2pProofProtocolHandlers",
        path: "demo/da-committee-node/src/da/libp2p/proof-protocols.ts",
      },
      {
        symbol: "DaLibp2pRetainedDaSource",
        path: "demo/midgard-fault-proofs/src/transition-trace/fetch.ts",
      },
      {
        symbol: "reconstructDaPayloadV1",
        path: "demo/midgard-fault-proofs/src/transition-trace/reconstruct.ts",
      },
    ],
  ],
  [
    "validation",
    [
      {
        symbol: "runPhaseAValidation",
        path: "demo/midgard-validation/src/phase-a.ts",
      },
      {
        symbol: "runPhaseBValidationWithPatch",
        path: "demo/midgard-validation/src/phase-b.ts",
      },
      {
        symbol: "buildDeterministicValidationMachineTrace",
        path: "demo/midgard-validation/src/validation-machine.ts",
      },
      {
        symbol: "validateMidgardConsensusV1Tx",
        path: "demo/midgard-core/src/consensus-validation-v1.ts",
      },
    ],
  ],
  [
    "proof_tooling",
    [
      {
        symbol: "prepareDoubleSpendFromFile",
        path: "demo/midgard-fault-proofs/src/prepare-double-spend.ts",
      },
      {
        symbol: "prepareInvalidRangeFromFile",
        path: "demo/midgard-fault-proofs/src/prepare-invalid-range.ts",
      },
      {
        symbol: "prepareNonExistentInputFromFile",
        path: "demo/midgard-fault-proofs/src/prepare-non-existent-input.ts",
      },
      {
        symbol: "DaLibp2pRetainedDaSource",
        path: "demo/midgard-fault-proofs/src/transition-trace/fetch.ts",
      },
      {
        symbol: "submitTransitionTraceProof",
        path: "demo/midgard-fault-proofs/src/transition-trace/submit.ts",
      },
      {
        symbol: "submitValidationDisputeOpen",
        path: "demo/midgard-fault-proofs/src/validation-dispute/submit.ts",
      },
    ],
  ],
  [
    "deployment_manifest",
    [
      {
        symbol: "verifyDeploymentManifestV1Identity",
        path: "demo/midgard-core/src/deployment-manifest-identity-v1.ts",
      },
      {
        symbol: "parseDeploymentManifestV1Value",
        path: "demo/midgard-node/src/deployment-manifest-v1.ts",
      },
      {
        symbol: "verifyDeploymentManifestAgainstConfig",
        path: "demo/midgard-node/src/commands/contract-deployment-info.ts",
      },
      {
        symbol: "verifyWatcherDeploymentIdentityV1",
        path: "demo/midgard-watcher/src/deployment-identity.ts",
      },
    ],
  ],
  [
    "l1_provider",
    [
      {
        symbol: "StateQueueProvider",
        path: "demo/da-committee-node/src/l1/state-queue-scanner.ts",
      },
      {
        symbol: "LucidStateQueueProvider",
        path: "demo/da-committee-node/src/l1/provider.ts",
      },
      {
        symbol: "MultiStateQueueProvider",
        path: "demo/da-committee-node/src/l1/provider.ts",
      },
      {
        symbol: "scanStateQueue",
        path: "demo/da-committee-node/src/l1/state-queue-scanner.ts",
      },
      {
        symbol: "normalizeWatcherL1BlockV1",
        path: "demo/midgard-watcher/src/l1-adapter.ts",
      },
      {
        symbol: "evaluateWatcherMultiProviderConsistencyV1",
        path: "demo/midgard-watcher/src/multi-provider-consistency.ts",
      },
      {
        symbol: "evaluateWatcherFinalityV1",
        path: "demo/midgard-watcher/src/finality-engine.ts",
      },
      {
        symbol: "evaluateWatcherRollbackV1",
        path: "demo/midgard-watcher/src/rollback-engine.ts",
      },
      {
        symbol: "evaluateWatcherStateQueueIndexerV1",
        path: "demo/midgard-watcher/src/state-queue-indexer.ts",
      },
    ],
  ],
  [
    "state_queue",
    [
      {
        symbol: "fetchSortedStateQueueUTxOs",
        path: "demo/midgard-sdk/src/state-queue.ts",
      },
      {
        symbol: "getStateQueueNodeV1FromStateQueueDatum",
        path: "demo/midgard-sdk/src/ledger-state.ts",
      },
      {
        symbol: "scanStateQueue",
        path: "demo/da-committee-node/src/l1/state-queue-scanner.ts",
      },
      {
        symbol: "fetchStateQueueTopologyProgram",
        path: "demo/midgard-node/src/services/state-queue-topology.ts",
      },
      {
        symbol: "migrateWatcherDurableStoreV1",
        path: "demo/midgard-watcher/src/durable-store.ts",
      },
      {
        symbol: "evaluateWatcherFinalityV1",
        path: "demo/midgard-watcher/src/finality-engine.ts",
      },
      {
        symbol: "evaluateWatcherStateQueueIndexerV1",
        path: "demo/midgard-watcher/src/state-queue-indexer.ts",
      },
    ],
  ],
  [
    "correction_removal",
    [
      {
        symbol: "submitRemoveFraudulentBlock",
        path: "demo/midgard-fault-proofs/src/remove-fraudulent-block.ts",
      },
      {
        symbol: "submitRemoveFraudulentBlockFromFiles",
        path: "demo/midgard-fault-proofs/src/remove-fraudulent-block.ts",
      },
      {
        symbol: "createHttpStateQueueMutationLeaseCoordinator",
        path: "demo/midgard-fault-proofs/src/remove-fraudulent-block.ts",
      },
    ],
  ],
]);

const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");

const codeWithoutCommentsOrLiterals = (source) => {
  let result = "";
  let state = "code";
  let quote = "";
  for (let index = 0; index < source.length; index += 1) {
    const character = source[index];
    const next = source[index + 1];
    if (state === "code") {
      if (character === "/" && next === "/") {
        result += "  ";
        index += 1;
        state = "line_comment";
      } else if (character === "/" && next === "*") {
        result += "  ";
        index += 1;
        state = "block_comment";
      } else if (character === "'" || character === '"' || character === "`") {
        result += " ";
        quote = character;
        state = "literal";
      } else {
        result += character;
      }
    } else if (state === "line_comment") {
      result += character === "\n" ? "\n" : " ";
      if (character === "\n") {
        state = "code";
      }
    } else if (state === "block_comment") {
      if (character === "*" && next === "/") {
        result += "  ";
        index += 1;
        state = "code";
      } else {
        result += character === "\n" ? "\n" : " ";
      }
    } else if (character === "\\") {
      result += " ";
      if (next !== undefined) {
        result += next === "\n" ? "\n" : " ";
        index += 1;
      }
    } else if (character === quote) {
      result += " ";
      state = "code";
    } else {
      result += character === "\n" ? "\n" : " ";
    }
  }
  return result;
};

const exportedDeclarationPresent = (source, symbol) =>
  new RegExp(
    `\\bexport\\s+(?:(?:declare|async)\\s+)*(?:const|let|function|class|interface|type|enum)\\s+${escapeRegex(symbol)}\\b`,
    "u",
  ).test(source);

const exportedClassBody = (source, owner) => {
  const declaration = new RegExp(
    `\\bexport\\s+(?:(?:declare|abstract)\\s+)*class\\s+${escapeRegex(owner)}\\b`,
    "u",
  ).exec(source);
  if (declaration === null) {
    return null;
  }
  const open = source.indexOf("{", declaration.index + declaration[0].length);
  if (open < 0) {
    return null;
  }
  let depth = 0;
  for (let index = open; index < source.length; index += 1) {
    if (source[index] === "{") {
      depth += 1;
    } else if (source[index] === "}") {
      depth -= 1;
      if (depth === 0) {
        return source.slice(open + 1, index);
      }
    }
  }
  return null;
};

const sourceDeclaresBinding = (source, binding) => {
  const code = codeWithoutCommentsOrLiterals(source);
  if (binding.owner === undefined || binding.member === undefined) {
    return exportedDeclarationPresent(code, binding.symbol);
  }
  const body = exportedClassBody(code, binding.owner);
  return (
    body !== null &&
    new RegExp(
      `\\b(?:(?:public|private|protected|static|readonly|async)\\s+)*${escapeRegex(binding.member)}\\s*\\(`,
      "u",
    ).test(body)
  );
};
const dependencies = dependencyMap.dependencies;
if (!Array.isArray(dependencies)) {
  fail("dependencies must be an array");
}
const byId = new Map(dependencies.map((entry) => [entry.id, entry]));
if (byId.size !== dependencies.length) {
  fail("dependency ids must be unique");
}
if (
  dependencies.length !== requiredIds.length ||
  requiredIds.some((id, index) => dependencies[index]?.id !== id)
) {
  fail("dependency ids and order must match the exact required set");
}
for (const id of requiredIds) {
  const entry = byId.get(id);
  if (entry === undefined) {
    fail(`missing dependency ${id}`);
  }
  if (entry.trust !== requiredTrustById.get(id)) {
    fail(`${id} has an invalid trust classification`);
  }
  const requiredSources = requiredSourcesById.get(id);
  if (
    requiredSources === undefined ||
    JSON.stringify(entry.sourcePaths) !==
      JSON.stringify(requiredSources.paths) ||
    JSON.stringify(entry.sourceSymbols) !==
      JSON.stringify(requiredSources.symbols)
  ) {
    fail(`${id} source paths and symbols must match the exact required set`);
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
  const requiredBindings = requiredSymbolBindingsById.get(id);
  if (
    requiredBindings === undefined ||
    JSON.stringify(requiredBindings.map(({ symbol }) => symbol)) !==
      JSON.stringify(entry.sourceSymbols) ||
    requiredBindings.some(({ path }) => !entry.sourcePaths.includes(path))
  ) {
    fail(`${id} symbol bindings must match exact owning source paths`);
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
  const sourceTexts = new Map();
  for (const sourcePath of entry.sourcePaths) {
    if (
      sourcePath.startsWith("/") ||
      sourcePath.split("/").includes("..") ||
      entry.sourceSha256[sourcePath] !== sha256(readIndexedFile(sourcePath))
    ) {
      fail(`${id} source hash is stale for ${sourcePath}`);
    }
    sourceTexts.set(sourcePath, readIndexedFile(sourcePath, "utf8"));
  }
  for (const binding of requiredBindings) {
    const source = sourceTexts.get(binding.path);
    if (source === undefined || !sourceDeclaresBinding(source, binding)) {
      fail(
        `${id} source symbol ${binding.symbol} is not declared by ${binding.path}`,
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
  JSON.stringify(rejected) !==
    JSON.stringify([
      {
        path: "demo/midgard-node/src/database",
        reason: "operator-private database",
      },
      {
        path: "demo/midgard-node/src/commands/e2e-service.ts",
        reason: "operator administration and test orchestration",
      },
      {
        symbol: "createHttpStateQueueMutationLeaseCoordinator",
        reason: "operator-admin mutation lease",
      },
      {
        mode: "--midgard-node-url evidence preparation",
        reason:
          "operator endpoint is diagnostic only; watcher evidence must originate from authenticated public L1 and DA",
      },
    ])
) {
  fail("operator-private and diagnostic dependencies must be exactly rejected");
}
if (
  JSON.stringify(dependencyMap.trustPolicy?.allowedSecurityInputs) !==
    JSON.stringify([
      "authenticated_cardano_l1",
      "public_or_permissionless_da",
      "signed_deployment_identity",
      "deterministic_local_computation",
    ]) ||
  JSON.stringify(dependencyMap.trustPolicy?.prohibitedSecurityInputs) !==
    JSON.stringify([
      "operator_private_database",
      "operator_admin_api",
      "operator_private_file",
      "operator_only_diagnostic_endpoint",
    ])
) {
  fail("allowed and prohibited security inputs must match the exact policy");
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
  readIndexedFile("demo/midgard-watcher/package.json", "utf8"),
);
const workspaceManifest = JSON.parse(
  readIndexedFile("demo/package.json", "utf8"),
);
const committeeManifest = JSON.parse(
  readIndexedFile("demo/da-committee-node/package.json", "utf8"),
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
const listIndexedPackageFiles = (relativeDirectory) =>
  execGit(["ls-files", "-z", "--", relativeDirectory], "utf8")
    .split("\0")
    .filter((path) => path !== "")
    .sort((left, right) =>
      Buffer.compare(Buffer.from(left, "utf8"), Buffer.from(right, "utf8")),
    );
const declaredWatcherContents = [
  ...(dependencyMap.requiredWatcherPackage?.currentContents ?? []),
].sort((left, right) =>
  Buffer.compare(Buffer.from(left, "utf8"), Buffer.from(right, "utf8")),
);
const actualWatcherContents = listIndexedPackageFiles("demo/midgard-watcher");
if (
  JSON.stringify(declaredWatcherContents) !==
  JSON.stringify(actualWatcherContents)
) {
  fail("requiredWatcherPackage.currentContents must exactly cover the package");
}
if (
  workspaceManifest.scripts?.["watcher:dependency-map:verify"] !==
    "node scripts/verify-canonical-v1-watcher-dependency-map.mjs" ||
  workspaceManifest.scripts?.["watcher:dependency-map:test"] !==
    "node --test scripts/verify-canonical-v1-watcher-dependency-map.test.mjs"
) {
  fail(
    "workspace must expose the canonical watcher dependency-map verifier and mutation tests",
  );
}
const nodeCi = readIndexedFile(".github/workflows/midgard-node-ci.yml", "utf8");
for (const requiredCiText of [
  "demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs",
  "demo/scripts/verify-canonical-v1-watcher-dependency-map.test.mjs",
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
  "pnpm --dir demo run watcher:dependency-map:test && pnpm --dir demo run watcher:dependency-map:verify",
]) {
  if (!nodeCi.includes(requiredCiText)) {
    fail(`Midgard node CI is missing ${requiredCiText}`);
  }
}
const watcherArchitecture = readIndexedFile(
  "demo/midgard-watcher/midgard-watcher-architecture.md",
  "utf8",
);
const watcherAdversarialReview = readIndexedFile(
  "demo/midgard-watcher/watcher-plan-adversarial-review.md",
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
const watcherSource = readIndexedFile(
  "demo/midgard-watcher/src/scaffold.ts",
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
const configBytes = readIndexedFile("demo/midgard-watcher/src/config.ts");
const configTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/config.test.ts",
);
const deploymentIdentityBytes = readIndexedFile(
  "demo/midgard-watcher/src/deployment-identity.ts",
);
const deploymentIdentityTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/deployment-identity.test.ts",
);
const durableStoreBytes = readIndexedFile(
  "demo/midgard-watcher/src/durable-store.ts",
);
const durableStoreTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/durable-store.test.ts",
);
const l1AdapterBytes = readIndexedFile(
  "demo/midgard-watcher/src/l1-adapter.ts",
);
const l1AdapterTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/l1-adapter.test.ts",
);
const multiProviderConsistencyBytes = readIndexedFile(
  "demo/midgard-watcher/src/multi-provider-consistency.ts",
);
const multiProviderConsistencyTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/multi-provider-consistency.test.ts",
);
const finalityEngineBytes = readIndexedFile(
  "demo/midgard-watcher/src/finality-engine.ts",
);
const finalityEngineTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/finality-engine.test.ts",
);
const rollbackEngineBytes = readIndexedFile(
  "demo/midgard-watcher/src/rollback-engine.ts",
);
const rollbackEngineTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/rollback-engine.test.ts",
);
const ruleBundleBytes = readIndexedFile(
  "demo/midgard-watcher/src/rule-bundle-v1.ts",
);
const ruleBundleTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/rule-bundle-v1.test.ts",
);
const stateQueueIndexerBytes = readIndexedFile(
  "demo/midgard-watcher/src/state-queue-indexer.ts",
);
const stateQueueIndexerTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/state-queue-indexer.test.ts",
);
const userEventIndexerBytes = readIndexedFile(
  "demo/midgard-watcher/src/user-event-indexer.ts",
);
const userEventIndexerTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/user-event-indexer.test.ts",
);
const settlementIndexerBytes = readIndexedFile(
  "demo/midgard-watcher/src/settlement-indexer.ts",
);
const settlementIndexerTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/settlement-indexer.test.ts",
);
const proofThreadIndexerBytes = readIndexedFile(
  "demo/midgard-watcher/src/proof-thread-indexer.ts",
);
const proofThreadIndexerTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/proof-thread-indexer.test.ts",
);
const watcherIndexSource = readIndexedFile(
  "demo/midgard-watcher/src/index.ts",
  "utf8",
);
const readTrackedBytes = ({ mode, objectId }) => {
  if (mode === "160000") {
    return Buffer.from(`gitlink:${objectId}`);
  }
  return execGit(["cat-file", "blob", objectId]);
};
const contentTreeExclusions = [
  "GOAL_PROGRESS.md",
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
];
if (
  JSON.stringify(dependencyMap.authority?.publishedParentRevisions) !==
    JSON.stringify([
      "2b755a776d6af4a57d877633485ca0701e4cc51d",
      "d8fcc0f74f659fe614dc215dcef0f3d2c9b16590",
    ]) ||
  dependencyMap.authority?.sourceRevision !==
    "4acf68215c76bbac72c5a7f35962c611ce3b92da" ||
  dependencyMap.authority?.baseRevision !==
    "8bae9403a13124f647f215999848ff5c82784e37" ||
  dependencyMap.authority?.treeState !==
    "reviewed merge content tree bound by resultContentTreeSha256" ||
  JSON.stringify(dependencyMap.authority?.contentTreeExclusions) !==
    JSON.stringify(contentTreeExclusions)
) {
  fail("authority must bind both reviewed merge parents and exact exclusions");
}
const trackedEntriesFromIndex = execGit(["ls-files", "--stage", "-z"], "utf8")
  .split("\0")
  .filter((record) => record !== "")
  .map((record) => {
    const separatorIndex = record.indexOf("\t");
    if (separatorIndex <= 0) {
      fail("tracked index contains a malformed entry");
    }
    const metadata = record.slice(0, separatorIndex);
    const match = metadata.match(
      /^(100644|100755|120000|160000) ([0-9a-f]{40}|[0-9a-f]{64}) 0$/,
    );
    if (match === null) {
      fail("tracked index must contain only supported stage-zero entries");
    }
    const [, mode, objectId] = match;
    return {
      mode,
      objectId,
      path: record.slice(separatorIndex + 1),
    };
  })
  .filter(({ path }) => !contentTreeExclusions.includes(path))
  .sort((left, right) =>
    Buffer.compare(
      Buffer.from(left.path, "utf8"),
      Buffer.from(right.path, "utf8"),
    ),
  );
const trackedEntries = trackedEntriesFromIndex.map((entry) => ({
  path: entry.path,
  mode: entry.mode,
  sha256: sha256(readTrackedBytes(entry)),
}));
const resultContentTreeSha256 = sha256(
  JSON.stringify({
    domain: "midgard-reviewed-integration-content-tree-v1",
    entries: trackedEntries,
  }),
);
if (process.argv.includes("--print-result-content-tree-sha256")) {
  process.stdout.write(`${resultContentTreeSha256}\n`);
  process.exit(0);
}
if (
  dependencyMap.authority?.resultContentTreeSha256 !== resultContentTreeSha256
) {
  fail("authority result content tree is stale");
}
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
  l1Adapter.totalCollectionMembers !== 65_536 ||
  l1Adapter.unknownBehavior !== "fail_closed" ||
  l1Adapter.diagnostics !== "code_and_schema_path_only" ||
  l1Adapter.node22FocusedTestsPassed !== 21
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
  multiProviderConsistency.minimumChainAuthoritiesByMode?.local_node !== 1 ||
  multiProviderConsistency.minimumChainAuthoritiesByMode?.external_providers !==
    2 ||
  multiProviderConsistency.localQuerySurfacesCountAsIndependentProviders !==
    false ||
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
  multiProviderConsistency.node22FocusedTestsPassed !== 21
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
  userEventIndexer.node22FocusedTestsPassed !== 18
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
    "PASS_full_restart_replay_canonical_transaction_order_aggregate_bounds_and_hostile_probes" ||
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
    "bounded_retry_stuck_invalid_identity_and_full_transition_history_restart_replay" ||
  settlementIndexer.rollbackPolicy !==
    "exact_W13_journal_restoration_unrelated_archive_preservation_restart_reinclusion_and_same_point_transaction_order" ||
  settlementIndexer.unknownBehavior !== "fail_closed" ||
  settlementIndexer.diagnostics !== "deterministic_value_free_codes" ||
  settlementIndexer.node22FocusedTestsPassed !== 23
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
    "PASS_full_restart_replay_revision_monotonicity_canonical_transaction_order_and_aggregate_bounds" ||
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
    "exact_W13_journal_rewind_full_transition_history_restart_replay_revision_monotonicity_and_reinclusion" ||
  proofThreadIndexer.unknownBehavior !== "fail_closed" ||
  proofThreadIndexer.diagnostics !== "deterministic_value_free_codes" ||
  proofThreadIndexer.node22FocusedTestsPassed !== 18
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

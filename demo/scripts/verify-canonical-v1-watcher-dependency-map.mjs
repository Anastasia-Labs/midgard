import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { parse } from "@typescript-eslint/parser";
import { parseDocument } from "yaml";

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
const dependencyMapPath =
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json";
// `--map-under-test=` lets the behavioural self-test
// (verify-canonical-v1-watcher-dependency-map-self-test.mjs) drive this exact
// gate against a mutated copy of the map. Only the map is redirected: git
// history, the staged watcher sources, the CI workflows and every other input
// stay the real ones, so a seeded provenance claim is judged against reality
// rather than against a fixture of its own choosing.
const mapUnderTestArgument = process.argv
  .slice(2)
  .find((argument) => argument.startsWith("--map-under-test="));
const dependencyMap = JSON.parse(
  mapUnderTestArgument === undefined
    ? readIndexedFile(dependencyMapPath, "utf8")
    : readFileSync(
        resolve(mapUnderTestArgument.slice("--map-under-test=".length)),
        "utf8",
      ),
);

const fail = (message) => {
  throw new Error(`Watcher dependency map verification failed: ${message}`);
};

// Git-authority primitives (#537). Every provenance claim this file makes is a
// claim about Git history, so it is asked of Git instead of being compared to a
// second hardcoded copy of itself. The exemplar is
// demo/scripts/verify-canonical-v1-format-registry.mjs: 40-hex shape check,
// `git merge-base --is-ancestor <rev> HEAD`, and a byte-exact `git show
// <rev>:<path>`. Only immutable historical bytes are bound, so no binding here
// can go stale on a legitimate commit — the failure mode that retired the
// staged-tree identity this block replaces.
const isFullCommitSha = (value) =>
  typeof value === "string" && /^[0-9a-f]{40}$/u.test(value);
const isSha256Hex = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);
const isRepositoryRelativePath = (value) =>
  typeof value === "string" &&
  value.length > 0 &&
  !value.startsWith("/") &&
  !value.split("/").includes("..");
const gitCommandSucceeds = (args) => {
  try {
    execFileSync("git", args, {
      cwd: repositoryRoot,
      stdio: ["ignore", "ignore", "ignore"],
    });
    return true;
  } catch {
    return false;
  }
};
const isAncestorOf = (ancestor, descendant) =>
  gitCommandSucceeds(["merge-base", "--is-ancestor", ancestor, descendant]);
const isAncestorOfHead = (revision) => isAncestorOf(revision, "HEAD");
/** Historical bytes at a revision, or null when the path is absent there. */
const showAtRevision = (revision, path) => {
  try {
    return execFileSync("git", ["show", `${revision}:${path}`], {
      cwd: repositoryRoot,
      stdio: ["ignore", "pipe", "ignore"],
      maxBuffer: 128 * 1024 * 1024,
    });
  } catch {
    return null;
  }
};
const gitOutput = (args) => {
  try {
    return execFileSync("git", args, {
      cwd: repositoryRoot,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
  } catch {
    return null;
  }
};
const sha256Hex = (bytes) => createHash("sha256").update(bytes).digest("hex");

// #537: the W13-W17/W23 rows used to carry an `independentAudit` prose string
// compared only to a hardcoded copy of itself in this file, so editing both
// literals passed CI, and "independent" named no second party that could have
// been independent of anything. Each is now a `reviewRecord` whose falsifiable
// content is Git: `reviewedAtRev` must be a real 40-hex ancestor of HEAD that
// actually changed the reviewed paths, and every reviewed path must resolve
// byte-exactly at that revision. `summary` is prose for humans and is NEVER
// evidence — it is checked only for being a non-empty string, so no assertion
// in this file can be satisfied by a sentence. A record therefore states what
// content was reviewed and when; if the reviewed files have moved since, that
// is visible rather than hidden, which is the whole point.
const REVIEW_RECORD_FIELDS = new Set([
  "reviewedAtRev",
  "reviewedPaths",
  "summary",
  "secondPartyAudit",
]);
const SECOND_PARTY_AUDIT_FIELDS = new Set(["auditor", "rev", "reportDigest"]);
// The optional slot a genuine external audit would fill. It is absent
// everywhere today — the honest state — and is validated structurally whenever
// it appears, so the schema exists before the first real second-party report
// rather than being invented under deadline.
const verifySecondPartyAudit = (audit, label) => {
  if (audit === null || typeof audit !== "object" || Array.isArray(audit)) {
    fail(`${label} secondPartyAudit must be an object when present`);
  }
  for (const field of Object.keys(audit)) {
    if (!SECOND_PARTY_AUDIT_FIELDS.has(field)) {
      fail(`${label} secondPartyAudit carries the unknown field ${field}`);
    }
  }
  if (typeof audit.auditor !== "string" || audit.auditor.trim().length === 0) {
    fail(`${label} secondPartyAudit.auditor must be a non-empty string`);
  }
  if (!isFullCommitSha(audit.rev)) {
    fail(
      `${label} secondPartyAudit.rev must be a full 40-character Git commit`,
    );
  }
  if (!isAncestorOfHead(audit.rev)) {
    fail(
      `${label} secondPartyAudit.rev ${audit.rev} is not an ancestor of HEAD`,
    );
  }
  if (!isSha256Hex(audit.reportDigest)) {
    fail(
      `${label} secondPartyAudit.reportDigest must be a 64-character SHA-256 hex digest`,
    );
  }
};
const verifyReviewRecord = (record, label, requiredReviewedPaths) => {
  if (record === null || typeof record !== "object" || Array.isArray(record)) {
    fail(`${label} reviewRecord must be an object`);
  }
  for (const field of Object.keys(record)) {
    if (!REVIEW_RECORD_FIELDS.has(field)) {
      fail(`${label} reviewRecord carries the unknown field ${field}`);
    }
  }
  if (!isFullCommitSha(record.reviewedAtRev)) {
    fail(
      `${label} reviewRecord.reviewedAtRev must be a full 40-character Git commit`,
    );
  }
  if (!isAncestorOfHead(record.reviewedAtRev)) {
    fail(
      `${label} reviewRecord.reviewedAtRev ${record.reviewedAtRev} is not an ancestor of HEAD`,
    );
  }
  const reviewedPaths = record.reviewedPaths;
  if (
    !Array.isArray(reviewedPaths) ||
    reviewedPaths.length === 0 ||
    new Set(reviewedPaths).size !== reviewedPaths.length ||
    !reviewedPaths.every(isRepositoryRelativePath)
  ) {
    fail(
      `${label} reviewRecord.reviewedPaths must be a non-empty array of unique repository-relative paths without '..'`,
    );
  }
  const missingReviewedPaths = requiredReviewedPaths.filter(
    (requiredPath) => !reviewedPaths.includes(requiredPath),
  );
  if (missingReviewedPaths.length !== 0) {
    fail(
      `${label} reviewRecord.reviewedPaths does not cover ${missingReviewedPaths.join(", ")}`,
    );
  }
  for (const reviewedPath of reviewedPaths) {
    if (showAtRevision(record.reviewedAtRev, reviewedPath) === null) {
      fail(
        `${label} reviewRecord.reviewedAtRev ${record.reviewedAtRev} does not contain ${reviewedPath}`,
      );
    }
  }
  // A revision that never touched the reviewed content cannot be the revision
  // that content was reviewed at, so an arbitrary ancestor cannot be pasted in.
  const lastChangingRevision = gitOutput([
    "rev-list",
    "-1",
    record.reviewedAtRev,
    "--",
    ...reviewedPaths,
  ]);
  if (lastChangingRevision !== record.reviewedAtRev) {
    fail(
      `${label} reviewRecord.reviewedAtRev ${record.reviewedAtRev} did not change the reviewed paths (git reports ${lastChangingRevision ?? "no such commit"})`,
    );
  }
  if (
    typeof record.summary !== "string" ||
    record.summary.trim().length === 0
  ) {
    fail(`${label} reviewRecord.summary must be non-empty prose`);
  }
  if (record.secondPartyAudit !== undefined) {
    verifySecondPartyAudit(record.secondPartyAudit, label);
  }
};
// The retired field cannot come back anywhere in the map, including on rows
// that never carried it (W25, W26 and the F30 conclusion), whose evidence is
// the runner-measured focused-test counts pinned below and in
// demo/scripts/verify-canonical-v1-watcher-focused-tests.mjs.
const retiredMapFields = (value, path = "$") => {
  if (Array.isArray(value)) {
    return value.flatMap((entry, index) =>
      retiredMapFields(entry, `${path}[${index}]`),
    );
  }
  if (value === null || typeof value !== "object") {
    return [];
  }
  return Object.entries(value).flatMap(([key, entry]) =>
    key === "independentAudit"
      ? [`${path}.${key}`]
      : retiredMapFields(entry, `${path}.${key}`),
  );
};
const retiredAuditFields = retiredMapFields(dependencyMap);
if (retiredAuditFields.length !== 0) {
  fail(
    `the retired independentAudit field reappeared at ${retiredAuditFields.join(", ")}; a review claim must be a git-bound reviewRecord`,
  );
}

if (
  dependencyMap.schemaVersion !==
  "midgard-canonical-v1-watcher-dependency-map-v1"
) {
  fail("unknown schemaVersion");
}
if (dependencyMap.trustPolicy?.unknownBehavior !== "fail_closed") {
  fail("unknown behavior must fail closed");
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
const requiredMetadataById = new Map([
  [
    "public_da",
    {
      capability:
        "Deployment-bound payload retrieval with exact V1 framing, limits, hashes, and strict decoding",
      state: "public_server_and_strict_identity_bound_watcher_client_ready",
      remainingTasks: ["Q58", "W27"],
      watcherBoundary:
        "Use the strict public watcher client and libp2p transport to verify deployment fingerprint, header and payload hashes, bounded framing, and strict body bytes; persist through W21 before W27 verification.",
    },
  ],
  [
    "proof_bundle",
    {
      capability:
        "Deployment-bound proof-bundle, indexed trace-step, and event-to-step retrieval",
      state: "public_protocol_and_existing_challenger_client_ready",
      remainingTasks: ["Q58", "W27"],
      watcherBoundary:
        "Reuse the exact transport codecs and public challenger request path; never read the committee store directly. Verify returned deployment/header/index/event identities before persistence.",
    },
  ],
  [
    "validation",
    {
      capability:
        "Canonical native transaction decoding, Phase A/B validation, CEK/script execution, evidence, and one-step semantics",
      state:
        "shared_library_rule_bundle_total_replay_and_event_classification_ready_CG3_open",
      remainingTasks: ["CG3"],
      watcherBoundary:
        "Import shared production semantics and the deployment-bound rule/profile identity; a watcher-specific folklore validator is forbidden.",
    },
  ],
  [
    "proof_tooling",
    {
      capability:
        "Prepare, inspect, submit, reconcile, and remove fault proofs",
      state:
        "selected_families_and_public_transition_trace_ready_total_resumable_api_missing",
      remainingTasks: ["Q10", "Q11", "Q12", "Q21", "Q22", "Q51", "W30", "W32"],
      watcherBoundary:
        "Call typed programmatic APIs with persisted authenticated public evidence. Node URL and arbitrary local file modes are diagnostic/import boundaries, not watcher security inputs.",
    },
  ],
  [
    "deployment_manifest",
    {
      capability:
        "Strict V1 manifest identity, script/reference identities, consensus profile, and DA transport binding",
      state: "strict_signed_release_identity_and_durable_marker_verifier_ready",
      remainingTasks: ["C70", "C75", "F41"],
      watcherBoundary:
        "Use the strict signed-identity verifier and persist its exact durable marker through W03 before accepting observations; C70/C75/F41 must replace placeholder release and deployment inputs with final-tree/live evidence.",
    },
  ],
  [
    "l1_provider",
    {
      capability:
        "Current external-provider observations with canonical chain-point agreement; local-node vocabulary deferred until native peer authentication",
      state:
        "strict_external_source_library_normalizer_consistency_finality_automated_rollback_recovery_and_indexing_ready_local_node_deferred_operational_wire_open",
      remainingTasks: ["C70", "W10-OPERATIONAL-WIRE", "W14-LIVE-PROVENANCE"],
      watcherBoundary:
        "The current wire path selects external_providers and requires at least two operationally independent provider/operator/endpoint identities; disagreement is quarantined. local_node remains pure state vocabulary but is deferred until a native adapter binds peer identity to the connected socket, and the wire parser rejects it before socket-path processing. W10 proves exact configured TLS transport capability and normalizes supplied observations, but the operational adapter that obtains every observation from those live transports remains open and start/replay remain fail closed. W12 preserves the finalized binding during transient source non-agreement; only a mode-valid agreed canonical replacement opens an incident. W13 automatically rewinds and resumes replay from persisted W10 bytes and W11 agreement within Cardano k=2160. W14-W17 library state machines consume that authorized recovery without duplicating Cardano validator semantics; node-accepted byte provenance is not claimed until the operational W10 adapter is complete.",
    },
  ],
  [
    "state_queue",
    {
      capability:
        "Decode, sort, authenticate, and traverse canonical state-queue nodes and headers",
      state:
        "read_helpers_strict_durable_schema_finality_automated_rollback_recovery_and_protocol_index_library_ready_live_provenance_open",
      remainingTasks: ["W10-OPERATIONAL-WIRE", "W14-LIVE-PROVENANCE"],
      watcherBoundary:
        "Reuse canonical SDK datum/header decoding, persist supplied W10 observations through the strict atomic W03 schema, and gate irreversible interpretation through source-mode-bound W11-W13. W14's library state machine consumes the canonical observation and automated rollback/replay pipeline, derives durable roles from deployed policy and actual output bytes, and does not reimplement the on-chain state-queue validator. Actual node-accepted transaction/output/datum provenance remains open until the operational W10 adapter supplies every observation directly from the configured live transport.",
    },
  ],
  [
    "correction_removal",
    {
      capability:
        "Construct and submit tail/non-tail fraudulent-header removal with operator slashing and refetch",
      state:
        "public_l1_builder_ready_non_tail_coordination_depends_on_prohibited_admin_api",
      remainingTasks: ["Q52", "W35"],
      watcherBoundary:
        "The watcher may reuse public L1 topology/refetch and transaction construction only after Q52/W35 replace operator-admin coordination with a protocol-safe public concurrency strategy.",
    },
  ],
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
        "demo/midgard-watcher/src/public-da-client.ts",
        "demo/midgard-watcher/src/public-da-libp2p-transport.ts",
      ],
      symbols: [
        "DaRequestResponseProtocol",
        "DaLibp2pPayloadProtocolHandlers",
        "decodeDaPayloadByHeaderResponseV1Cbor",
        "decodeDaPayloadV1Strict",
        "DaLibp2pNode.request",
        "WatcherPublicDaClientV1",
        "createWatcherPublicDaLibp2pTransportV1",
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
        "demo/midgard-validation/src/transition-effect-v1.ts",
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
        "buildCanonicalTransitionEffectV1",
        "deriveCanonicalDepositTransitionEffectV1",
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
      {
        symbol: "WatcherPublicDaClientV1",
        path: "demo/midgard-watcher/src/public-da-client.ts",
      },
      {
        symbol: "createWatcherPublicDaLibp2pTransportV1",
        path: "demo/midgard-watcher/src/public-da-libp2p-transport.ts",
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
        symbol: "buildCanonicalTransitionEffectV1",
        path: "demo/midgard-validation/src/transition-effect-v1.ts",
      },
      {
        symbol: "deriveCanonicalDepositTransitionEffectV1",
        path: "demo/midgard-validation/src/transition-effect-v1.ts",
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

const parseTypescriptModule = (source) => {
  try {
    return parse(source, {
      ecmaVersion: "latest",
      sourceType: "module",
    });
  } catch {
    return null;
  }
};

const classBindingKeys = new Set([
  "demo/da-committee-node/src/da/libp2p/payload-protocols.ts#DaLibp2pPayloadProtocolHandlers",
  "demo/da-committee-node/src/da/proof-artifacts.ts#DaProofArtifactDeriver",
  "demo/da-committee-node/src/da/libp2p/proof-protocols.ts#DaLibp2pProofProtocolHandlers",
  "demo/midgard-fault-proofs/src/transition-trace/fetch.ts#DaLibp2pRetainedDaSource",
  "demo/da-committee-node/src/l1/provider.ts#LucidStateQueueProvider",
  "demo/da-committee-node/src/l1/provider.ts#MultiStateQueueProvider",
  "demo/midgard-watcher/src/public-da-client.ts#WatcherPublicDaClientV1",
]);
const interfaceBindingKeys = new Set([
  "demo/da-committee-node/src/l1/state-queue-scanner.ts#StateQueueProvider",
]);

const declarationImplementsBinding = (declaration, binding) => {
  const bindingKey = `${binding.path}#${binding.symbol}`;
  if (interfaceBindingKeys.has(bindingKey)) {
    return (
      declaration.type === "TSInterfaceDeclaration" &&
      declaration.declare !== true &&
      declaration.id.name === binding.symbol
    );
  }
  if (classBindingKeys.has(bindingKey)) {
    return (
      declaration.type === "ClassDeclaration" &&
      declaration.declare !== true &&
      declaration.abstract !== true &&
      declaration.id?.name === binding.symbol &&
      declaration.body.type === "ClassBody"
    );
  }
  return (
    declaration.type === "VariableDeclaration" &&
    declaration.declare !== true &&
    declaration.declarations.some(
      ({ id, init }) =>
        id.type === "Identifier" && id.name === binding.symbol && init !== null,
    )
  );
};

const exportedDeclarationPresent = (moduleAst, binding) =>
  moduleAst.body.some(
    (statement) =>
      statement.type === "ExportNamedDeclaration" &&
      statement.declaration !== null &&
      declarationImplementsBinding(statement.declaration, binding),
  );

const directClassMemberPresent = (moduleAst, owner, member) => {
  const ownerDeclaration = moduleAst.body.find(
    (statement) =>
      statement.type === "ExportNamedDeclaration" &&
      statement.declaration?.type === "ClassDeclaration" &&
      statement.declaration.declare !== true &&
      statement.declaration.id?.name === owner,
  )?.declaration;
  if (
    ownerDeclaration?.type !== "ClassDeclaration" ||
    ownerDeclaration.abstract === true
  ) {
    return false;
  }
  return ownerDeclaration.body.body.some(
    (element) =>
      element.type === "MethodDefinition" &&
      element.computed === false &&
      element.kind === "method" &&
      element.static !== true &&
      element.optional !== true &&
      (element.accessibility === undefined ||
        element.accessibility === "public") &&
      element.key.type === "Identifier" &&
      element.key.name === member &&
      element.value.body?.type === "BlockStatement",
  );
};

const sourceDeclaresBinding = (moduleAst, binding) => {
  if (binding.owner === undefined || binding.member === undefined) {
    return exportedDeclarationPresent(moduleAst, binding);
  }
  return directClassMemberPresent(moduleAst, binding.owner, binding.member);
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
  const requiredMetadata = requiredMetadataById.get(id);
  if (
    requiredMetadata === undefined ||
    entry.capability !== requiredMetadata.capability ||
    entry.state !== requiredMetadata.state ||
    JSON.stringify(entry.remainingTasks) !==
      JSON.stringify(requiredMetadata.remainingTasks) ||
    entry.watcherBoundary !== requiredMetadata.watcherBoundary
  ) {
    fail(`${id} capability, state, tasks, and boundary must remain exact`);
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
  const sourceTexts = new Map();
  const sourceModules = new Map();
  for (const sourcePath of entry.sourcePaths) {
    // Path containment only. The per-file byte hashes that used to be compared
    // here went stale on every legitimate commit to a watcher dependency and
    // caught nothing Git does not already show (GOAL_SPEC §13.4, owner
    // amendment 2026-08-01). The symbol-level AST checks below are what
    // actually prove each dependency class resolves to real exported API.
    if (sourcePath.startsWith("/") || sourcePath.split("/").includes("..")) {
      fail(`${id} source path escapes the repository: ${sourcePath}`);
    }
    const sourceText = readIndexedFile(sourcePath, "utf8");
    sourceTexts.set(sourcePath, sourceText);
    sourceModules.set(sourcePath, parseTypescriptModule(sourceText));
  }
  for (const binding of requiredBindings) {
    const source = sourceTexts.get(binding.path);
    const sourceModule = sourceModules.get(binding.path);
    if (
      source === undefined ||
      sourceModule === undefined ||
      sourceModule === null ||
      !sourceDeclaresBinding(sourceModule, binding)
    ) {
      fail(
        `${id} source symbol ${binding.symbol} is not declared by ${binding.path}`,
      );
    }
  }
}

if (
  JSON.stringify(dependencyMap.f30Conclusion) !==
  JSON.stringify({
    status: "pass",
    reason:
      "Every required dependency class is resolved to current source, each permitted watcher boundary is explicit, and operator-private surfaces are rejected and mapped to concrete replacement tasks.",
    nextTasks: ["W04", "W27", "Q52"],
  })
) {
  fail("F30 conclusion and next tasks must remain exact");
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
  workspaceManifest.scripts?.["watcher:focused-tests:verify"] !==
    "node scripts/verify-canonical-v1-watcher-focused-tests.mjs"
) {
  fail(
    "workspace must expose the canonical watcher dependency-map verifier and focused-test verifier",
  );
}
const nodeCi = readIndexedFile(".github/workflows/midgard-node-ci.yml", "utf8");
const activeYamlLines = (source) => {
  const active = [];
  let blockScalarIndent = null;
  for (const line of source.split(/\r?\n/u)) {
    const indent = line.match(/^[ \t]*/u)?.[0].length ?? 0;
    const trimmed = line.trim();
    if (
      blockScalarIndent !== null &&
      (trimmed === "" || indent > blockScalarIndent)
    ) {
      continue;
    }
    blockScalarIndent = null;
    if (trimmed === "" || trimmed.startsWith("#")) {
      continue;
    }
    active.push({ indent, trimmed });
    if (/:\s*[>|][-+0-9]*\s*$/u.test(trimmed)) {
      blockScalarIndent = indent;
    }
  }
  return active;
};
const decodeYamlScalar = (value) => {
  if (value.startsWith('"') && value.endsWith('"')) {
    return JSON.parse(value);
  }
  if (value.startsWith("'") && value.endsWith("'")) {
    return value.slice(1, -1).replaceAll("''", "'");
  }
  return value;
};
const nodeCiActiveLines = activeYamlLines(nodeCi);
const activeRunCommands = nodeCiActiveLines.flatMap(({ trimmed }) => {
  const match = trimmed.match(/^run:\s*(.+)$/u);
  return match === null ? [] : [decodeYamlScalar(match[1].trim())];
});
const exactActiveStepCount = (lines, name, command) => {
  let count = 0;
  for (let index = 0; index < lines.length; index += 1) {
    if (
      lines[index]?.indent !== 6 ||
      lines[index]?.trimmed !== `- name: ${name}`
    ) {
      continue;
    }
    let end = index + 1;
    while (end < lines.length && lines[end].indent > 6) {
      end += 1;
    }
    const step = lines.slice(index, end);
    if (
      step.length === 2 &&
      step[1]?.indent === 8 &&
      step[1]?.trimmed === `run: ${command}`
    ) {
      count += 1;
    }
  }
  return count;
};
const isYamlRecord = (value) =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const parseWorkflow = (source, path) => {
  const document = parseDocument(source, {
    maxAliasCount: 0,
    uniqueKeys: true,
  });
  if (document.errors.length !== 0) {
    fail(`${path} must be valid unique-key YAML`);
  }
  const workflow = document.toJS({ maxAliasCount: 0 });
  if (!isYamlRecord(workflow) || !isYamlRecord(workflow.on)) {
    fail(`${path} must declare event triggers as a YAML mapping`);
  }
  return workflow;
};
const workflowEvent = (workflow, eventName, path) => {
  if (!Object.hasOwn(workflow.on, eventName)) {
    fail(`${path} must declare ${eventName}`);
  }
  const event = workflow.on[eventName];
  if (event === null) {
    return {};
  }
  if (!isYamlRecord(event)) {
    fail(`${path} ${eventName} configuration must be a YAML mapping or null`);
  }
  return event;
};
const nodeCiWorkflow = parseWorkflow(
  nodeCi,
  ".github/workflows/midgard-node-ci.yml",
);
const nodeCiEvents = new Map(
  ["push", "pull_request"].map((eventName) => [
    eventName,
    workflowEvent(
      nodeCiWorkflow,
      eventName,
      ".github/workflows/midgard-node-ci.yml",
    ),
  ]),
);
const pullRequestEvent = nodeCiEvents.get("pull_request");
if (
  Object.hasOwn(pullRequestEvent, "branches") ||
  Object.hasOwn(pullRequestEvent, "branches-ignore")
) {
  fail("Midgard node CI pull_request must not be restricted by branch filters");
}
for (const requiredCiPath of [
  ".github/workflows/evidence-integrity-ci.yml",
  "demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs",
  "demo/scripts/verify-canonical-v1-watcher-focused-tests.mjs",
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
]) {
  for (const triggerName of ["push", "pull_request"]) {
    const paths = nodeCiEvents.get(triggerName).paths;
    if (
      !Array.isArray(paths) ||
      paths.filter((entry) => entry === requiredCiPath).length !== 1
    ) {
      fail(
        `Midgard node CI must actively scope ${triggerName} to ${requiredCiPath}`,
      );
    }
  }
}
const requiredDependencyCommand =
  "pnpm --dir demo run watcher:dependency-map:verify";
const dependencyCommandIndex = nodeCiActiveLines.findIndex(
  ({ trimmed }) => trimmed === `run: ${requiredDependencyCommand}`,
);
let dependencyStepStart = dependencyCommandIndex;
while (
  dependencyStepStart >= 0 &&
  !(
    nodeCiActiveLines[dependencyStepStart].indent === 6 &&
    nodeCiActiveLines[dependencyStepStart].trimmed.startsWith("- ")
  )
) {
  dependencyStepStart -= 1;
}
let dependencyStepEnd = dependencyCommandIndex + 1;
while (
  dependencyStepEnd < nodeCiActiveLines.length &&
  !(
    nodeCiActiveLines[dependencyStepEnd].indent === 6 &&
    nodeCiActiveLines[dependencyStepEnd].trimmed.startsWith("- ")
  )
) {
  dependencyStepEnd += 1;
}
const dependencyStep =
  dependencyStepStart < 0
    ? []
    : nodeCiActiveLines.slice(dependencyStepStart, dependencyStepEnd);
if (
  activeRunCommands.filter((command) => command === requiredDependencyCommand)
    .length !== 1 ||
  dependencyStep.length !== 2 ||
  dependencyStep[0]?.trimmed !==
    "- name: Verify canonical watcher dependency map" ||
  dependencyStep[1]?.indent !== 8 ||
  dependencyStep[1]?.trimmed !== `run: ${requiredDependencyCommand}` ||
  nodeCiActiveLines.some(
    ({ indent, trimmed }) => indent === 4 && /^if\s*:/u.test(trimmed),
  )
) {
  fail(
    `Midgard node CI must actively run exactly one ${requiredDependencyCommand}`,
  );
}
const requiredFocusedTestCommand =
  "pnpm --dir demo run watcher:focused-tests:verify";
const requiredWatcherGateCommand =
  "pnpm --dir demo/midgard-watcher run build && pnpm --dir demo/midgard-watcher run typecheck && pnpm --dir demo/midgard-watcher run lint && pnpm --dir demo/midgard-watcher run format-check && pnpm --dir demo run watcher:focused-tests:verify";
if (
  activeRunCommands.filter((command) => command === requiredWatcherGateCommand)
    .length !== 1 ||
  exactActiveStepCount(
    nodeCiActiveLines,
    "Build, typecheck, lint, format-check, and test Midgard watcher scaffold",
    requiredWatcherGateCommand,
  ) !== 1
) {
  fail(
    `Midgard node CI must actively run exactly one ${requiredFocusedTestCommand}`,
  );
}
const evidenceCi = readIndexedFile(
  ".github/workflows/evidence-integrity-ci.yml",
  "utf8",
);
const evidenceCiWorkflow = parseWorkflow(
  evidenceCi,
  ".github/workflows/evidence-integrity-ci.yml",
);
const evidencePullRequestEvent = workflowEvent(
  evidenceCiWorkflow,
  "pull_request",
  ".github/workflows/evidence-integrity-ci.yml",
);
if (
  evidenceCiWorkflow.on.pull_request !== null ||
  Object.hasOwn(evidencePullRequestEvent, "branches") ||
  Object.hasOwn(evidencePullRequestEvent, "branches-ignore")
) {
  fail(
    "Evidence Integrity CI pull_request must be unrestricted by branch, path, or type filters",
  );
}
if (
  !isYamlRecord(evidenceCiWorkflow.jobs) ||
  !isYamlRecord(evidenceCiWorkflow.jobs.verify) ||
  Object.hasOwn(evidenceCiWorkflow.jobs.verify, "if") ||
  Object.hasOwn(evidenceCiWorkflow.jobs.verify, "continue-on-error")
) {
  fail("Evidence Integrity CI verify job must be unconditional and strict");
}
const evidenceCiActiveLines = activeYamlLines(evidenceCi);
for (const [name, requiredEvidenceCommand] of [
  [
    "Verify canonical watcher dependency map",
    "node demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs",
  ],
  [
    "Verify canonical watcher focused-test evidence",
    "node demo/scripts/verify-canonical-v1-watcher-focused-tests.mjs",
  ],
  // #537: a gate whose provenance bindings are only asserted by itself is the
  // defect this lane closed, so the behavioural self-test that seeds hostile
  // provenance into a copy of the map is itself CI-bound here.
  [
    "Self-test the watcher dependency-map git-authority bindings",
    "node demo/scripts/verify-canonical-v1-watcher-dependency-map-self-test.mjs",
  ],
]) {
  if (
    evidenceCiActiveLines.filter(
      ({ trimmed }) => trimmed === `run: ${requiredEvidenceCommand}`,
    ).length !== 1 ||
    exactActiveStepCount(
      evidenceCiActiveLines,
      name,
      requiredEvidenceCommand,
    ) !== 1
  ) {
    fail(
      `Evidence Integrity CI must actively run exactly one ${requiredEvidenceCommand}`,
    );
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
    !sourceModeDocument.includes("deferred") ||
    !sourceModeDocument.includes("peer-authenticated")
  ) {
    fail(
      "shipped watcher documents must define both L1-source vocabulary modes and defer local_node pending peer authentication",
    );
  }
}
const watcherSource = readIndexedFile(
  "demo/midgard-watcher/src/scaffold.ts",
  "utf8",
);
const watcherScaffoldTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/scaffold.test.ts",
);
if (
  !watcherSource.includes('state: "foundation_incomplete"') ||
  !watcherSource.includes("productionReady: false")
) {
  fail("W00 watcher commands must remain explicitly fail closed");
}
// Per-file byte hashes removed here and in every W-row block below
// (GOAL_SPEC §13.4, owner amendment 2026-08-01): they went stale on every
// legitimate edit to a watcher module and caught nothing Git does not show.
// The behavioural assertions — required exported symbols, schema-version
// strings, policy strings, fail-closed markers, and expected test counts —
// are what actually prove each module still does its job.
const scaffold = dependencyMap.requiredWatcherPackage?.scaffold;
if (scaffold?.expectedFocusedTestCount !== 5) {
  fail("W00 watcher scaffold evidence is incomplete or stale");
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
const blockReplayBytes = readIndexedFile(
  "demo/midgard-watcher/src/block-replay.ts",
);
const blockReplayTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/block-replay.test.ts",
);
const eventClassificationVerifierBytes = readIndexedFile(
  "demo/midgard-watcher/src/event-classification-verifier.ts",
);
const eventClassificationVerifierTestBytes = readIndexedFile(
  "demo/midgard-watcher/tests/event-classification-verifier.test.ts",
);
const w15AuthorityScenariosBytes = readIndexedFile(
  "demo/midgard-watcher/tests/support/w15-authority-scenarios.ts",
);
const w16AuthorityScenariosBytes = readIndexedFile(
  "demo/midgard-watcher/tests/support/w16-authority-scenarios.ts",
);
const w25AuthorityFixturesBytes = readIndexedFile(
  "demo/midgard-watcher/tests/support/w25-authority-fixtures.ts",
);
const watcherOpaqueAuthorityHarnessBytes = readIndexedFile(
  "demo/midgard-watcher/tests/support/watcher-opaque-authority-harness.ts",
);
// GOAL_SPEC §13.4 / §0 Integrity (owner amendment 2026-08-01; git-authority
// binding 2026-08-06, issue #537). The staged-tree identity that used to live
// here recomputed a SHA-256 over every tracked file in the repository — a
// hand-rolled duplicate of Git's own tree object, which `git rev-parse
// HEAD^{tree}` already provides. It caught no defect, went stale on every
// legitimate commit, cost two CI-red heads to converge, and formed a mutually
// unsatisfiable cycle with the closure manifest. What replaced it was three
// commit SHAs compared to hardcoded copies of themselves in this file: the
// revisions did not have to exist, be reachable, or contain anything. Each of
// the three is now asked of Git, and each binds exactly what it claims:
//
//   1. `publishedParentRevisions` claims two revisions were reviewed and
//      merged. Git is asked for the merge: `publishedMergeRevision` must be an
//      ancestor of HEAD whose parent list is exactly those two revisions in
//      that order, and the map artifact's bytes at each parent must hash to the
//      declared digest.
//   2. `sourceRevision` claims the revision this artifact was first published
//      at. It must be an ancestor of HEAD and of both reviewed parents, and the
//      artifact's bytes there must hash to the declared digest.
//   3. `baseRevision` claims the upstream base both reviewed parents descend
//      from, before this artifact existed. It must be an ancestor of HEAD and
//      of both parents, its tree must be exactly the declared tree object, and
//      the artifact must be absent there.
//
// Every bound byte is historical and therefore immutable, so a legitimate
// commit to the current tree can never falsify these bindings.
const authority = dependencyMap.authority;
if (
  authority === null ||
  typeof authority !== "object" ||
  Array.isArray(authority)
) {
  fail("authority must be an object");
}
if (authority.artifactPath !== dependencyMapPath) {
  fail(`authority.artifactPath must be ${dependencyMapPath}`);
}
const publishedParentRevisions = authority.publishedParentRevisions;
if (
  !Array.isArray(publishedParentRevisions) ||
  publishedParentRevisions.length !== 2 ||
  !publishedParentRevisions.every(isFullCommitSha)
) {
  fail(
    "authority.publishedParentRevisions must be exactly two full 40-character Git commits",
  );
}
// Reachability is checked per revision before the merge shape, so a fabricated
// revision is named by its own field rather than reported as a merge mismatch.
for (const [index, parentRevision] of publishedParentRevisions.entries()) {
  if (!isAncestorOfHead(parentRevision)) {
    fail(
      `authority.publishedParentRevisions[${index}] ${parentRevision} is not an ancestor of HEAD`,
    );
  }
}
if (!isFullCommitSha(authority.publishedMergeRevision)) {
  fail(
    "authority.publishedMergeRevision must be a full 40-character Git commit",
  );
}
if (!isAncestorOfHead(authority.publishedMergeRevision)) {
  fail(
    `authority.publishedMergeRevision ${authority.publishedMergeRevision} is not an ancestor of HEAD`,
  );
}
const publishedMergeParentLine = gitOutput([
  "rev-list",
  "--parents",
  "-1",
  authority.publishedMergeRevision,
]);
if (
  publishedMergeParentLine !==
  [authority.publishedMergeRevision, ...publishedParentRevisions].join(" ")
) {
  fail(
    `authority.publishedMergeRevision ${authority.publishedMergeRevision} does not merge exactly ${publishedParentRevisions.join(" and ")} in that order (git reports: ${publishedMergeParentLine ?? "no such commit"})`,
  );
}
const publishedParentArtifactSha256 = authority.publishedParentArtifactSha256;
if (
  !Array.isArray(publishedParentArtifactSha256) ||
  publishedParentArtifactSha256.length !== publishedParentRevisions.length ||
  !publishedParentArtifactSha256.every(isSha256Hex)
) {
  fail(
    "authority.publishedParentArtifactSha256 must be one 64-character SHA-256 hex digest per published parent",
  );
}
for (const [index, parentRevision] of publishedParentRevisions.entries()) {
  const parentArtifactBytes = showAtRevision(parentRevision, dependencyMapPath);
  if (parentArtifactBytes === null) {
    fail(
      `authority.publishedParentRevisions[${index}] ${parentRevision} does not contain ${dependencyMapPath}`,
    );
  }
  const parentArtifactDigest = sha256Hex(parentArtifactBytes);
  if (parentArtifactDigest !== publishedParentArtifactSha256[index]) {
    fail(
      `authority.publishedParentArtifactSha256[${index}] declares ${publishedParentArtifactSha256[index]}, but ${dependencyMapPath} at ${parentRevision} hashes to ${parentArtifactDigest}`,
    );
  }
}
if (!isFullCommitSha(authority.sourceRevision)) {
  fail("authority.sourceRevision must be a full 40-character Git commit");
}
if (!isAncestorOfHead(authority.sourceRevision)) {
  fail(
    `authority.sourceRevision ${authority.sourceRevision} is not an ancestor of HEAD`,
  );
}
for (const [index, parentRevision] of publishedParentRevisions.entries()) {
  if (!isAncestorOf(authority.sourceRevision, parentRevision)) {
    fail(
      `authority.sourceRevision ${authority.sourceRevision} is not an ancestor of publishedParentRevisions[${index}] ${parentRevision}`,
    );
  }
}
if (!isSha256Hex(authority.sourceArtifactSha256)) {
  fail(
    "authority.sourceArtifactSha256 must be a 64-character SHA-256 hex digest",
  );
}
const sourceArtifactBytes = showAtRevision(
  authority.sourceRevision,
  dependencyMapPath,
);
if (sourceArtifactBytes === null) {
  fail(
    `authority.sourceRevision ${authority.sourceRevision} does not contain ${dependencyMapPath}`,
  );
}
const sourceArtifactDigest = sha256Hex(sourceArtifactBytes);
if (sourceArtifactDigest !== authority.sourceArtifactSha256) {
  fail(
    `authority.sourceArtifactSha256 declares ${authority.sourceArtifactSha256}, but ${dependencyMapPath} at ${authority.sourceRevision} hashes to ${sourceArtifactDigest}`,
  );
}
if (!isFullCommitSha(authority.baseRevision)) {
  fail("authority.baseRevision must be a full 40-character Git commit");
}
if (!isAncestorOfHead(authority.baseRevision)) {
  fail(
    `authority.baseRevision ${authority.baseRevision} is not an ancestor of HEAD`,
  );
}
for (const [index, parentRevision] of publishedParentRevisions.entries()) {
  if (!isAncestorOf(authority.baseRevision, parentRevision)) {
    fail(
      `authority.baseRevision ${authority.baseRevision} is not an ancestor of publishedParentRevisions[${index}] ${parentRevision}`,
    );
  }
}
if (!isFullCommitSha(authority.baseTree)) {
  fail("authority.baseTree must be a full 40-character Git tree object");
}
const baseTree = gitOutput(["rev-parse", `${authority.baseRevision}^{tree}`]);
if (baseTree !== authority.baseTree) {
  fail(
    `authority.baseTree declares ${authority.baseTree}, but git resolves ${authority.baseRevision}^{tree} to ${baseTree ?? "no such tree"}`,
  );
}
if (authority.baseArtifactState !== "absent_before_first_publication") {
  fail("authority.baseArtifactState must be absent_before_first_publication");
}
if (showAtRevision(authority.baseRevision, dependencyMapPath) !== null) {
  fail(
    `authority.baseRevision ${authority.baseRevision} already contains ${dependencyMapPath}, so it is not the pre-publication base`,
  );
}
if (
  strictConfiguration?.schemaVersion !== "midgard-watcher-config-v1" ||
  JSON.stringify(strictConfiguration.l1SourceModes) !==
    JSON.stringify(["local_node", "external_providers"]) ||
  strictConfiguration.discriminatorPolicy !==
    "explicit_source_mode_required_without_compatibility_inference" ||
  strictConfiguration.wireSourceConfigPolicy !==
    "external_providers_only_local_node_pure_state_vocabulary_not_public_wire_config" ||
  strictConfiguration.selectableL1SourceModes?.length !== 1 ||
  strictConfiguration.selectableL1SourceModes[0] !== "external_providers" ||
  JSON.stringify(strictConfiguration.deferredL1SourceModes) !==
    JSON.stringify(["local_node"]) ||
  strictConfiguration.localNodePolicy !==
    "deferred_until_peer_authenticated_native_adapter_no_pathname_authority" ||
  strictConfiguration.externalProviderPolicy !==
    "two_to_four_operationally_independent_provider_operator_endpoint_identities" ||
  strictConfiguration.endpointBindingPolicy !==
    "exact_configured_https_provider_endpoints_only_local_node_wire_rejected_before_socket_processing" ||
  strictConfiguration.rollbackAuthorityKeyPolicy !==
    "separate_required_key_source_not_inline_and_not_reused_from_prover_credentials" ||
  strictConfiguration.finalityPolicy?.beforeFinality !== "rewind" ||
  strictConfiguration.finalityPolicy?.afterFinality !== "quarantine" ||
  strictConfiguration.finalityPolicy?.postFinalityRecoveryMaxDepth !== 2160 ||
  strictConfiguration.finalityPolicy?.postFinalityRecoveryPolicy !==
    "automatic_mode_valid_agreed_rewind_and_replay_at_fixed_cardano_k" ||
  strictConfiguration.finalityPolicy?.transientSourceEvidencePolicy !==
    "per_decision_quarantine_preserving_exact_finalized_state_without_incident" ||
  strictConfiguration.unknownBehavior !== "fail_closed" ||
  strictConfiguration.diagnostics !== "code_and_schema_path_only" ||
  strictConfiguration.expectedFocusedTestCount !== 42
) {
  fail("W01 strict watcher configuration evidence is incomplete or stale");
}
const configSource = configBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_CONFIG_SCHEMA_VERSION",
  "WATCHER_CONFIG_BOUNDS",
  "WATCHER_CARDANO_SECURITY_PARAMETER_K",
  "WatcherL1SourceMode",
  '"local_node"',
  '"external_providers"',
  "rollbackAuthorityKeySource",
  "parseWatcherConfig",
  "parseWatcherConfigJson",
  "watcherConfigDiagnostic",
]) {
  if (!configSource.includes(requiredSymbol)) {
    fail(`W01 configuration symbol ${requiredSymbol} is absent`);
  }
}
if (
  !/if \(preliminary\.sourceMode === "local_node"\) \{\s*fail\("invalid_value", "\$\.l1\.source\.sourceMode"\);\s*\}/u.test(
    configSource,
  ) ||
  configSource.includes("parseLocalQueryEndpoint") ||
  configSource.includes("parseLocalQueryServices") ||
  configSource.includes("WatcherLocalNodeQueryService") ||
  configSource.includes("queryServices") ||
  configSource.includes("socketPath") ||
  watcherIndexSource.includes("WatcherLocalNodeQueryService") ||
  !/export type WatcherL1SourceConfig = Readonly<\{\s*sourceMode: "external_providers";\s*providers:/u.test(
    configSource,
  )
) {
  fail(
    "W01 local_node must be rejected before socket-path processing and absent from the public wire source type",
  );
}
const deploymentIdentity =
  dependencyMap.requiredWatcherPackage?.deploymentIdentity;
if (
  deploymentIdentity?.schemaVersion !==
    "midgard-watcher-signed-deployment-identity-v1" ||
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
      "daHashPreimage",
    ]) ||
  deploymentIdentity.catalogueContractBinding !==
    "exact_category_id_order_and_deployed_script_hash" ||
  deploymentIdentity.unknownBehavior !== "fail_closed" ||
  deploymentIdentity.diagnostics !== "code_and_schema_path_only" ||
  deploymentIdentity.expectedFocusedTestCount !== 18
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
  durableStore.reconstructedStateOriginPolicy !==
    "required_exact_L1_chain_point_provenance_with_reference_integrity" ||
  durableStore.cachePolicy !== "deterministic_rebuild_from_canonical_records" ||
  durableStore.migrationPolicy !== "fresh_install_atomic_compare_and_swap" ||
  durableStore.unknownBehavior !== "fail_closed" ||
  durableStore.expectedFocusedTestCount !== 12 ||
  !Array.isArray(durableStore.recordClasses) ||
  durableStore.recordClasses.length !== requiredRecordClasses.length ||
  requiredRecordClasses.some(
    (recordClass, index) => durableStore.recordClasses[index] !== recordClass,
  )
) {
  fail("W03 durable-store evidence is incomplete or stale");
}
const durableStoreSource = durableStoreBytes.toString("utf8");
if (
  !durableStoreSource.includes(
    "$.reconstructedStates.${state.blockHash}.chainPointId",
  )
) {
  fail("W03 reconstructed state must bind an existing L1 chain point");
}
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
  l1Adapter?.status !==
    "LIBRARY_PASS_EXTERNAL_TLS_ONLY_LOCAL_NODE_DEFERRED_OPERATIONAL_WIRE_OPEN" ||
  l1Adapter.providerSchemaVersion !==
    "midgard-watcher-authenticated-l1-provider-v1" ||
  l1Adapter.observationSchemaVersion !==
    "midgard-watcher-l1-block-observation-v1" ||
  l1Adapter.normalizedSchemaVersion !==
    "midgard-watcher-normalized-l1-block-v1" ||
  JSON.stringify(l1Adapter.sourceModes) !==
    JSON.stringify(["external_providers"]) ||
  JSON.stringify(l1Adapter.deferredSourceModes) !==
    JSON.stringify(["local_node"]) ||
  l1Adapter.identityPolicy !==
    "external_provider_tls_bound_observation_with_provider_neutral_block_content_local_node_deferred_until_native_peer_binding" ||
  l1Adapter.inputPolicy !==
    "exact_configured_tls_transport_capability_with_in_process_observation_boundary_operational_wire_adapter_required_local_node_rejected_before_socket_processing" ||
  l1Adapter.totalCollectionMembers !== 65_536 ||
  l1Adapter.unknownBehavior !== "fail_closed" ||
  l1Adapter.diagnostics !== "code_and_schema_path_only" ||
  l1Adapter.expectedFocusedTestCount !== 23
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
for (const retiredSymbol of [
  "establishWatcherLocalNodeAuthorityTransportV1",
  "WatcherLocalNodeAuthorityTransportV1",
  "establishUnixSocket",
  "createNetConnection({ path",
]) {
  if (
    l1AdapterSource.includes(retiredSymbol) ||
    watcherIndexSource.includes(retiredSymbol)
  ) {
    fail(
      `retired pathname local-node authority symbol remains public: ${retiredSymbol}`,
    );
  }
}
const multiProviderConsistency =
  dependencyMap.requiredWatcherPackage?.multiProviderConsistency;
if (
  multiProviderConsistency?.schemaVersion !==
    "midgard-watcher-multi-provider-consistency-v1" ||
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
  multiProviderConsistency.expectedFocusedTestCount !== 18
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
  finalityEngine.confirmationDepthPolicy !== "release_and_deployment_bound" ||
  finalityEngine.firstVisibilityPolicy !== "always_pending" ||
  finalityEngine.preFinalityRollbackPolicy !==
    "deterministic_release_bound_rewind" ||
  finalityEngine.postFinalityRollbackPolicy !==
    "mode_valid_agreed_point_replacement_opens_recoverable_incident_while_transient_same_point_content_mismatch_or_depth_regression_preserves_finalized_binding" ||
  finalityEngine.consistencyPolicy !==
    "exact_source_mode_bound_W11_agreement_required" ||
  finalityEngine.externalProviderPolicy !==
    "exact_W01_policy_match_for_every_W11_external_provider_binding_and_an_agreed_record_must_bind_every_configured_provider" ||
  finalityEngine.unknownBehavior !== "fail_closed" ||
  finalityEngine.diagnostics !== "deterministic_value_free_codes" ||
  finalityEngine.expectedFocusedTestCount !== 25
) {
  fail("W12 finality-engine evidence is incomplete or stale");
}
const finalityEngineSource = finalityEngineBytes.toString("utf8");
const finalityEngineTestSource = finalityEngineTestBytes.toString("utf8");
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
// #539: the provider-binding predicate used to be a bare `.every` over a list
// the W11 parser bounds only from above, so a record binding a strict subset
// of the configured providers reached `finality_granted` with the unbound
// providers' operator/TLS/endpoint binding never evaluated. Both the lower
// bound and the reason code it reports are pinned here, with the adversarial
// selector and the fully bound control that separates them.
for (const requiredFinalitySymbol of [
  "source_provider_binding_unrun",
  "requireEveryConfiguredProvider",
]) {
  if (!finalityEngineSource.includes(requiredFinalitySymbol)) {
    fail(
      `W12 finality provider-coverage symbol ${requiredFinalitySymbol} is absent`,
    );
  }
}
for (const requiredFinalityTestSymbol of [
  "refuses finality while a configured external provider is unbound",
  "refuses finality when only part of the configured provider set ran",
  "grants finality when every configured external provider is bound",
  "source_provider_binding_unrun",
]) {
  if (!finalityEngineTestSource.includes(requiredFinalityTestSymbol)) {
    fail(
      `W12 finality provider-coverage case ${requiredFinalityTestSymbol} is not pinned`,
    );
  }
}
const rollbackEngine = dependencyMap.requiredWatcherPackage?.rollbackEngine;
if (
  rollbackEngine?.stateSchemaVersion !== "midgard-watcher-rollback-state-v1" ||
  rollbackEngine.incidentSchemaVersion !==
    "midgard-watcher-rollback-incident-v1" ||
  rollbackEngine.resultSchemaVersion !== "midgard-watcher-rollback-result-v1" ||
  rollbackEngine.postFinalityRecoveryStateSchemaVersion !==
    "midgard-watcher-post-finality-recovery-state-v1" ||
  rollbackEngine.postFinalityRecoveryResultSchemaVersion !==
    "midgard-watcher-post-finality-recovery-result-v1" ||
  rollbackEngine.epochCheckpointSchemaVersion !==
    "midgard-watcher-rollback-epoch-checkpoint-v1" ||
  rollbackEngine.durableTrustedHeadSchemaVersion !==
    "midgard-watcher-rollback-durable-trusted-head-v1" ||
  rollbackEngine.maximumPostFinalityRecoveryDepth !== 2160 ||
  rollbackEngine.transitionHistoryPerEpoch !== 128 ||
  rollbackEngine.status !== "PASS" ||
  rollbackEngine.rewindPolicy !==
    "deterministic_dependency_cascade_with_shared_input_retention_and_orphan_consumption_restoration" ||
  rollbackEngine.restartPolicy !==
    "externally_bootstrapped_bounded_exact_transition_and_recovery_replay_with_HMAC_bound_snapshot_and_monotonic_trusted_head" ||
  rollbackEngine.postFinalityPolicy !==
    "durable_incident_then_automatic_exact_common_ancestor_rewind_and_replay_within_fixed_cardano_k_and_recovery_lifecycle_bound_epoch_rotation" ||
  rollbackEngine.checkpointAuthorityPolicy !==
    "genesis_installation_anchor_then_HMAC_bound_monotonic_trusted_head_atomic_CAS_and_direct_successor_reconciliation" ||
  rollbackEngine.inputPolicy !==
    "exact_source_mode_bound_persisted_W10_paths_recomputed_W11_agreement_incident_endpoint_digests_W12_incident_W03_chain_point_origin_store_and_external_bootstrap" ||
  rollbackEngine.unknownBehavior !== "fail_closed" ||
  rollbackEngine.diagnostics !== "deterministic_value_free_codes" ||
  rollbackEngine.expectedFocusedTestCount !== 26
) {
  fail("W13 rollback-engine evidence is incomplete or stale");
}
verifyReviewRecord(rollbackEngine.reviewRecord, "W13 rollback-engine", [
  "demo/midgard-watcher/src/rollback-engine.ts",
  "demo/midgard-watcher/tests/rollback-engine.test.ts",
]);
const rollbackEngineSource = rollbackEngineBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_EPOCH_CHECKPOINT_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_V1_SCHEMA_VERSION",
  "WATCHER_POST_FINALITY_RECOVERY_STATE_V1_SCHEMA_VERSION",
  "WATCHER_POST_FINALITY_RECOVERY_RESULT_V1_SCHEMA_VERSION",
  "WATCHER_ROLLBACK_V1_BOUNDS",
  "WATCHER_ROLLBACK_REASON_CODES_V1",
  "WATCHER_ROLLBACK_ALERT_CODES_V1",
  "makeWatcherRollbackBootstrapStateV1",
  "parseWatcherRollbackStateV1",
  "parseWatcherRollbackResultV1",
  "parseWatcherPostFinalityRecoveryResultV1",
  "evaluateWatcherPostFinalityRecoveryV1",
  "evaluateWatcherRollbackV1",
  "prepareWatcherRollbackDurableTrustedHeadReconciliationV1",
]) {
  if (
    !rollbackEngineSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W13 rollback-engine symbol ${requiredSymbol} is not public`);
  }
}
if (
  !rollbackEngineSource.includes("trustedCheckpointStateDigest") ||
  !rollbackEngineSource.includes("resumableTrustedCheckpointStateDigest")
) {
  fail("W13 rollback epoch checkpoint authority is not implemented");
}
if (!rollbackEngineSource.includes("externalProviderBindings")) {
  fail("W13 rollback provenance must retain W11 external provider bindings");
}
const stateQueueIndexer =
  dependencyMap.requiredWatcherPackage?.stateQueueIndexer;
if (
  stateQueueIndexer?.status !== "LIBRARY_PASS_CANONICAL_WIRE_PROVENANCE_OPEN" ||
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
  stateQueueIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W02_deployment_W03_store_W10_observations_with_operational_W10_wire_provenance_open_and_recomputed_W11_consistency_W12_finality_and_verified_W13_recovery" ||
  stateQueueIndexer.queuePolicy !==
    "decode_and_index_canonical_transaction_output_and_datum_bytes_from_W10_without_replaying_Cardano_validator_semantics_operational_node_acceptance_provenance_open" ||
  stateQueueIndexer.durableRolePolicy !==
    "derive_owned_roles_from_signed_deployment_and_actual_output_bytes_while_preserving_foreign_roles_exactly" ||
  stateQueueIndexer.rollbackPolicy !==
    "exact_W13_pre_and_post_finality_sparse_block_cut_active_and_spent_journal_rewind_restart_replacement_path_replay_and_duplicate_hold" ||
  stateQueueIndexer.unknownBehavior !== "fail_closed" ||
  stateQueueIndexer.diagnostics !== "deterministic_value_free_codes" ||
  stateQueueIndexer.expectedFocusedTestCount !== 19
) {
  fail("W14 state-queue-indexer evidence is incomplete or stale");
}
verifyReviewRecord(stateQueueIndexer.reviewRecord, "W14 state-queue-indexer", [
  "demo/midgard-watcher/src/state-queue-indexer.ts",
  "demo/midgard-watcher/tests/state-queue-indexer.test.ts",
]);
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
  userEventIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W10_observations_with_parent_recomputed_W11_consistency_W12_finality_and_verified_W13_recovery_plus_W02_W03" ||
  userEventIndexer.eventPolicy !==
    "exact_deposit_withdrawal_forced_order_NFT_datum_witness_time_content_and_terminal_semantics" ||
  userEventIndexer.finalityPolicy !==
    "independent_active_and_terminal_pending_to_final_transitions" ||
  userEventIndexer.rollbackPolicy !==
    "exact_W13_pre_and_post_finality_internally_derived_sparse_block_cut_journal_restoration_suffix_rewind_restart_replacement_path_replay_and_reinclusion" ||
  userEventIndexer.unknownBehavior !== "fail_closed" ||
  userEventIndexer.diagnostics !== "deterministic_value_free_codes" ||
  userEventIndexer.expectedFocusedTestCount !== 23
) {
  fail("W15 user-event-indexer evidence is incomplete or stale");
}
verifyReviewRecord(userEventIndexer.reviewRecord, "W15 user-event-indexer", [
  "demo/midgard-watcher/src/user-event-indexer.ts",
  "demo/midgard-watcher/tests/user-event-indexer.test.ts",
]);
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
  settlementIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W10_observations_with_recomputed_W11_consistency_W12_finality_and_verified_W13_recovery_plus_W02_W03" ||
  settlementIndexer.settlementPolicy !==
    "exact_claim_disproof_resolution_reserve_payout_refund_value_and_terminal_semantics" ||
  settlementIndexer.retryPolicy !==
    "bounded_retry_stuck_invalid_identity_and_full_transition_history_restart_replay" ||
  settlementIndexer.durableRolePolicy !==
    "derive_owned_roles_from_signed_deployment_and_actual_output_bytes_while_preserving_foreign_roles_exactly" ||
  settlementIndexer.rollbackPolicy !==
    "exact_W13_pre_and_post_finality_sparse_block_cut_common_ancestor_cursor_replacement_path_replay_unrelated_archive_preservation_restart_reinclusion_and_same_point_transaction_order" ||
  settlementIndexer.unknownBehavior !== "fail_closed" ||
  settlementIndexer.diagnostics !== "deterministic_value_free_codes" ||
  settlementIndexer.expectedFocusedTestCount !== 25
) {
  fail("W16 settlement-indexer evidence is incomplete or stale");
}
verifyReviewRecord(settlementIndexer.reviewRecord, "W16 settlement-indexer", [
  "demo/midgard-watcher/src/settlement-indexer.ts",
  "demo/midgard-watcher/tests/settlement-indexer.test.ts",
]);
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
  proofThreadIndexer.inputPolicy !==
    "exact_raw_source_mode_bound_W10_observations_with_recomputed_W11_consistency_W12_finality_and_verified_W13_recovery_plus_W02_W03" ||
  proofThreadIndexer.threadPolicy !==
    "deterministic_step_success_proof_token_removal_and_cancellation_lifecycles" ||
  proofThreadIndexer.durableRolePolicy !==
    "derive_proof_computation_and_shared_DA_roles_from_signed_deployment_and_actual_output_bytes_while_preserving_other_roles_exactly" ||
  proofThreadIndexer.rollbackPolicy !==
    "exact_W13_pre_and_post_finality_sparse_block_cut_common_ancestor_cursor_journal_rewind_full_transition_history_restart_replacement_path_replay_revision_monotonicity_and_reinclusion" ||
  proofThreadIndexer.unknownBehavior !== "fail_closed" ||
  proofThreadIndexer.diagnostics !== "deterministic_value_free_codes" ||
  proofThreadIndexer.expectedFocusedTestCount !== 17
) {
  fail("W17 proof-thread-indexer evidence is incomplete or stale");
}
verifyReviewRecord(
  proofThreadIndexer.reviewRecord,
  "W17 proof-thread-indexer",
  [
    "demo/midgard-watcher/src/proof-thread-indexer.ts",
    "demo/midgard-watcher/tests/proof-thread-indexer.test.ts",
  ],
);
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
  ruleBundle.schemaVersion !== "midgard-watcher-rule-bundle-v1" ||
  ruleBundle.bundleVersion !== 1 ||
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
  ruleBundle.expectedFocusedTestCount !== 9
) {
  fail("W23 rule-bundle evidence is incomplete or stale");
}
verifyReviewRecord(ruleBundle.reviewRecord, "W23 rule-bundle", [
  "demo/midgard-watcher/src/rule-bundle-v1.ts",
  "demo/midgard-watcher/tests/rule-bundle-v1.test.ts",
]);
const blockReplay = dependencyMap.requiredWatcherPackage?.blockReplay;
if (
  blockReplay?.status !== "PASS" ||
  blockReplay.schemaVersion !== "midgard-watcher-block-replay-v1" ||
  blockReplay.authorityPolicy !==
    "parser_recomputed_W15_and_applicable_W16_authorities_plus_accepted_W21_W22_W23_W24_records_only" ||
  blockReplay.rootPolicy !==
    "prior_state_every_ledger_mutation_transition_event_and_post_state_are_recomputed_or_fail_closed_and_accept_requires_both_committed_bindings_to_have_run" ||
  blockReplay.rejectionPolicy !==
    "canonical_49_code_vocabulary_is_disjointly_partitioned_12_phase_b_27_phase_a_owned_10_unclaimed" ||
  blockReplay.eventPolicy !==
    "shared_canonical_raw_effects_are_derived_or_rebuilt_from_recomputed_authorities_and_applied_in_exact_dense_W22_source_order_W26_retains_classification" ||
  blockReplay.orderingPolicy !==
    "exact_W22_EventKey_phase_step_order_then_canonical_phase_priority_for_rejections_with_restart_replay_digest_determinism" ||
  blockReplay.downstreamPolicy !==
    "machine_prerequisite_requires_W26_accept_and_action_accept_alone_never_implies_W29_verified" ||
  blockReplay.unknownBehavior !== "fail_closed" ||
  blockReplay.expectedFocusedTestCount !== 21
) {
  fail("W25 block-replay evidence is incomplete or stale");
}
const blockReplaySource = blockReplayBytes.toString("utf8");
const blockReplayTestSource = blockReplayTestBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_BLOCK_REPLAY_V1_SCHEMA_VERSION",
  "WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT_V1",
  "WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION",
  "WATCHER_BLOCK_REPLAY_STAGES_V1",
  "WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1",
  "WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1",
  "WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES_V1",
  "watcherBlockReplayPriorStateV1",
  "evaluateWatcherBlockReplayCandidatesV1",
  "evaluateWatcherBlockReplayV1",
  "makeWatcherBlockReplayReconstructedStateV1",
  "WatcherBlockReplayEventAuthorityV1",
  "WatcherBlockReplayUserEventAuthorityV1",
  "WatcherBlockReplaySettlementAuthorityV1",
  "WatcherBlockReplayEventRootV1",
]) {
  if (
    !blockReplaySource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W25 block-replay symbol ${requiredSymbol} is not public`);
  }
}
for (const requiredTestSymbol of [
  "evaluateWatcherBlockReplayCandidatesV1",
  "watcherBlockReplayPriorStateV1",
  "watcherBlockReplayCommittedStepsV1",
  "WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1",
  "evaluateWatcherBlockReplayV1",
  "makeWatcherBlockReplayReconstructedStateV1",
  "FIXED_TWO_TX_ROOTS",
  "reconstruction_unsupported_schema",
  "phase_a_digest_mismatch",
  "canonical_reconstruction_failed",
  "unknown_reject_code",
  "replays a Deposit before an L2 spend",
  "replays a Withdrawal after an L2 transaction",
  "replays a ForcedTransaction before a later L2 spend",
  "trace substitution, omission, duplication/reorder, trailing steps",
  "refuses acceptance while either committed binding is unrun",
  "committed_trace_binding_unrun",
  "post_state_binding_unrun",
  "deterministic corpus for every evidenced Phase-B rejection code",
  "createGenuineW15DepositWithdrawalAuthoritiesV1",
  "createGenuineW16SettlementAuthoritiesV1",
  "genuineW16.spawn",
  "genuineW16.absorbToReserve",
  "genuineW16.initializePayout",
  "genuineW16.refundWithdrawal",
  "forcedCanonicalNativeTxCbor",
]) {
  if (!blockReplayTestSource.includes(requiredTestSymbol)) {
    fail(`W25 block-replay test does not cover ${requiredTestSymbol}`);
  }
}
const w15AuthorityScenariosSource = w15AuthorityScenariosBytes.toString("utf8");
const w16AuthorityScenariosSource = w16AuthorityScenariosBytes.toString("utf8");
const w25AuthorityFixturesSource = w25AuthorityFixturesBytes.toString("utf8");
const watcherOpaqueAuthorityHarnessSource =
  watcherOpaqueAuthorityHarnessBytes.toString("utf8");
for (const requiredAuthoritySymbol of [
  "deriveWatcherUserEventObservationV1",
  "evaluateWatcherUserEventIndexerV1",
  "parseWatcherUserEventIndexerResultV1",
  "replayGenuineForcedTerminalAuthorityScenarioV1",
  "createGenuineW15DepositWithdrawalAuthoritiesV1",
  "forcedReceiptFixture",
  "deriveMidgardV1TxFieldChunks",
  "deriveMidgardTxFieldReceiptAssetNameV1",
]) {
  if (!w15AuthorityScenariosSource.includes(requiredAuthoritySymbol)) {
    fail(`W25 W15 authority support is missing ${requiredAuthoritySymbol}`);
  }
}
for (const requiredAuthoritySymbol of [
  "makeWatcherSettlementObservationV1",
  "evaluateWatcherSettlementIndexerV1",
  "parseWatcherSettlementIndexerResultV1",
  "replayGenuineSpawnSettlementAuthorityScenarioV1",
  "replayGenuineAbsorbToReserveAuthorityScenarioV1",
  "replayGenuineRefundWithdrawalAuthorityScenarioV1",
  "createGenuineW16SettlementAuthoritiesV1",
  "absorbToReserveAuthority",
  "initializePayoutAuthority",
  "refundWithdrawalAuthority",
]) {
  if (!w16AuthorityScenariosSource.includes(requiredAuthoritySymbol)) {
    fail(`W25 W16 authority support is missing ${requiredAuthoritySymbol}`);
  }
}
for (const requiredAuthoritySymbol of [
  "makeAcceptedW25DepositAuthorityFixtureV1",
  "makeAcceptedW25WithdrawalAuthorityFixtureV1",
  "makeAcceptedW25ForcedAuthorityFixtureV1",
  "makeAcceptedW25SpawnSettlementAuthorityFixtureV1",
  "makeAcceptedW25AbsorbToReserveAuthorityFixtureV1",
  "makeAcceptedW25InitializePayoutAuthorityFixtureV1",
  "makeAcceptedW25RefundWithdrawalAuthorityFixtureV1",
]) {
  if (!w25AuthorityFixturesSource.includes(requiredAuthoritySymbol)) {
    fail(`W25 authority fixture facade is missing ${requiredAuthoritySymbol}`);
  }
}
for (const requiredAuthoritySymbol of [
  "createWatcherOpaqueAuthorityHarnessV1",
  "establishWatcherExternalProviderTransportV1",
  "closeWatcherL1TransportAttestationContextV1",
]) {
  if (!watcherOpaqueAuthorityHarnessSource.includes(requiredAuthoritySymbol)) {
    fail(`W25 opaque authority harness is missing ${requiredAuthoritySymbol}`);
  }
}
const eventClassificationVerifier =
  dependencyMap.requiredWatcherPackage?.eventClassificationVerifier;
if (
  eventClassificationVerifier?.status !== "PASS" ||
  eventClassificationVerifier.schemaVersion !==
    "midgard-watcher-event-classification-verifier-v1" ||
  eventClassificationVerifier.authorityPolicy !==
    "exact_W15_event_id_nonce_identity_and_terminal_classification_W16_settlement_and_genuine_W25_replay_outputs_are_reverified" ||
  eventClassificationVerifier.timingPolicy !==
    "timed_L1_events_are_due_in_open_start_closed_end_and_forced_intervals_must_intersect_the_block_window" ||
  eventClassificationVerifier.classificationPolicy !==
    "due_omitted_out_of_window_fabricated_duplicate_withdrawal_and_six_way_forced_classification_match_canonical_root_mutation_semantics" ||
  eventClassificationVerifier.identityPolicy !==
    "W25_event_key_fingerprint_equals_decoded_W15_event_id_and_exact_nonce_out_ref_never_the_created_event_output_ref" ||
  eventClassificationVerifier.downstreamPolicy !==
    "W26_accept_is_required_but_never_implies_W29_verified" ||
  eventClassificationVerifier.unknownBehavior !== "fail_closed" ||
  eventClassificationVerifier.diagnostics !==
    "deterministic_value_free_codes_and_schema_paths" ||
  eventClassificationVerifier.expectedFocusedTestCount !== 15
) {
  fail("W26 event-classification-verifier evidence is incomplete or stale");
}
const eventClassificationVerifierSource =
  eventClassificationVerifierBytes.toString("utf8");
const eventClassificationVerifierTestSource =
  eventClassificationVerifierTestBytes.toString("utf8");
for (const requiredSymbol of [
  "WATCHER_EVENT_CLASSIFICATION_VERIFIER_V1_SCHEMA_VERSION",
  "WATCHER_EVENT_CLASSIFICATION_REASON_CODES_V1",
  "EvaluateWatcherEventClassificationInputV1",
  "WatcherEventClassificationResultV1",
  "evaluateWatcherEventClassificationV1",
]) {
  if (
    !eventClassificationVerifierSource.includes(requiredSymbol) ||
    !watcherIndexSource.includes(requiredSymbol)
  ) {
    fail(`W26 event-classification symbol ${requiredSymbol} is not public`);
  }
}
for (const requiredSourceAnchor of [
  "authenticatedEventFingerprint",
  "Data.from(event.eventId, OutputReference)",
  "fingerprint(phase, event.nonceOutRef)",
  "parseWatcherUserEventIndexerResultV1",
  "parseWatcherSettlementIndexerResultV1",
  "forcedValidationFacts.filter",
  "evaluateWatcherEventClassificationRulesV1",
  "w26_accepted_not_w29_verified",
]) {
  if (!eventClassificationVerifierSource.includes(requiredSourceAnchor)) {
    fail(`W26 source lost ${requiredSourceAnchor}`);
  }
}
for (const requiredTestAnchor of [
  "makeGenuineW25PublicReplayFixtureV1",
  "evaluateWatcherBlockReplayV1",
  "valid mutation and all five invalid no-op categories",
  "decodeMidgardNativeTxFullV1FromCanonicalCbor",
  ".validity",
  'toBe("TxIsValid")',
  "genuine withdrawal initialize/refund authorities",
  "createdOutputFingerprint",
  "mismatchedNonce",
  "forcedValidationFacts",
]) {
  if (!eventClassificationVerifierTestSource.includes(requiredTestAnchor)) {
    fail(`W26 tests do not cover ${requiredTestAnchor}`);
  }
}
for (const forbiddenTestAnchor of [
  "acceptedW25ForAuthority",
  "REVIEWED_FORCED_OUTCOMES",
  "forcedFact",
]) {
  if (eventClassificationVerifierTestSource.includes(forbiddenTestAnchor)) {
    fail(`W26 genuine tests recreate W25 authority via ${forbiddenTestAnchor}`);
  }
}
for (const requiredFixtureAnchor of [
  "GENUINE_W25_DA_PROVENANCE_V1",
  "makeGenuineW25PublicReplayFixtureV1",
  "evaluateWatcherHeaderRootReconstructionV1",
  "makeWatcherCanonicalRuleBundleV1",
]) {
  if (!w25AuthorityFixturesSource.includes(requiredFixtureAnchor)) {
    fail(`W26 genuine W25 fixture support is missing ${requiredFixtureAnchor}`);
  }
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
// W44 crash/rollback matrix (#509). The matrix is a count contract, not a
// free-form suite: exactly 17 cases, of which exactly 14 are before/after
// crash points across the 7 W32 durable lifecycle transitions, 1 is an
// ordinary pre-finality L1 rollback, 1 is a rollback deeper than the
// configured finality depth and within the fixed Cardano k, and 1 is a
// configured-source inconsistency. The literals below pin that decomposition
// so a future edit cannot quietly drop a crash point and stay green.
const crashRollbackMatrix =
  dependencyMap.requiredWatcherPackage?.crashRollbackMatrix;
const crashRollbackMatrixSource = readIndexedFile(
  "demo/midgard-watcher/tests/crash-rollback-matrix.test.ts",
  "utf8",
);
if (
  crashRollbackMatrix?.expectedFocusedTestCount !== 17 ||
  crashRollbackMatrix?.status !== "PASS" ||
  crashRollbackMatrix?.unknownBehavior !== "fail_closed" ||
  !Array.isArray(crashRollbackMatrix?.goalIds) ||
  !crashRollbackMatrix.goalIds.includes("W44") ||
  typeof crashRollbackMatrix?.matrixPolicy !== "string" ||
  typeof crashRollbackMatrix?.crashSeam !== "string" ||
  typeof crashRollbackMatrix?.recoveryPolicy !== "string" ||
  typeof crashRollbackMatrix?.invariantPolicy !== "string" ||
  typeof crashRollbackMatrix?.securityConditionPolicy !== "string" ||
  typeof crashRollbackMatrix?.readinessPolicy !== "string"
) {
  fail("W44 crash/rollback matrix evidence is incomplete or stale");
}
for (const requiredTransition of [
  '"detect"',
  '"persist_evidence"',
  '"init"',
  '"steps"',
  '"proof_token"',
  '"removal_slashing"',
  '"terminal_verification"',
]) {
  if (!crashRollbackMatrixSource.includes(requiredTransition)) {
    fail(
      `W44 crash/rollback matrix must cover the W32 lifecycle transition ${requiredTransition}`,
    );
  }
}
for (const requiredMarker of [
  "compareAndSwapWatcherDurableAtomicSnapshotV1",
  "crashBeforeAttempt",
  "crashAfterAttempt",
  "evaluateAndPersistWatcherRollbackV1",
  "evaluateAndPersistWatcherPostFinalityRecoveryV1",
  "WATCHER_ROLLBACK_V1_BOUNDS",
  "doubleSubmits",
  "duplicateRewards",
  "lostEvidence",
  "falseVerifiedStates",
  "unrecoverableWorkflows",
  "publicDataViolations",
  "sourceConsistencyViolations",
  "maturityViolations",
  "disabledFamilyFaults",
]) {
  if (!crashRollbackMatrixSource.includes(requiredMarker)) {
    fail(`W44 crash/rollback matrix must exercise ${requiredMarker}`);
  }
}
// #519 finding V-4 (#527): publicDaClient, canonicalBlockStore,
// headerRootReconstruction, and phaseAVerifier — 301 of the published 616
// watcher tests — had no literal pin anywhere in this verifier, so the map
// could declare any number for them and both watcher gates stayed green. These
// pins were measured from a package-local Vitest 3.0.7 JSON report and are
// re-checked against a live run by
// demo/scripts/verify-canonical-v1-watcher-focused-tests.mjs, whose pin table
// must stay identical to these values.
for (const [evidenceKey, testFile, pinnedFocusedTestCount] of [
  ["publicDaClient", "public-da-client.test.ts", 102],
  ["canonicalBlockStore", "canonical-block-store.test.ts", 46],
  ["headerRootReconstruction", "header-root-reconstruction.test.ts", 59],
  ["phaseAVerifier", "phase-a-verifier.test.ts", 94],
]) {
  if (
    dependencyMap.requiredWatcherPackage?.[evidenceKey]
      ?.expectedFocusedTestCount !== pinnedFocusedTestCount
  ) {
    fail(
      `${evidenceKey} must declare the runner-measured pin of ${String(pinnedFocusedTestCount)} focused tests for ${testFile}`,
    );
  }
}
if (
  dependencyMap.requiredWatcherPackage.foundationStatus !==
    "W01_W03_and_W11_W17_library_surfaces_pass_operational_W10_wire_binding_open" ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W01") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W02") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W03") ||
  !dependencyMap.requiredWatcherPackage.remainingTasks.includes(
    "W10-OPERATIONAL-WIRE",
  ) ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W11") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W12") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W13") ||
  !dependencyMap.requiredWatcherPackage.remainingTasks.includes(
    "W14-LIVE-PROVENANCE",
  ) ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W15") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W16") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W17") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W20") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W21") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W22") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W23") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W24") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W25") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W26") ||
  dependencyMap.requiredWatcherPackage.remainingTasks.includes("W24-W46") ||
  !dependencyMap.requiredWatcherPackage.remainingTasks.includes("W27-W46")
) {
  fail(
    "W01-W03, W11-W17, and W20-W26 must be complete while W10 operational wire provenance and W27-W46 remain explicitly open",
  );
}
if (dependencyMap.f30Conclusion?.status !== "pass") {
  fail("F30 conclusion must pass");
}
if (
  JSON.stringify(dependencyMap.f30Conclusion.nextTasks) !==
  JSON.stringify(["W04", "W27", "Q52"])
) {
  fail("F30 next tasks must omit completed Q03/W20 and retain open work");
}

console.log(
  `Canonical V1 watcher dependency map verified: ${dependencies.length} dependency classes.`,
);

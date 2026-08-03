import { createHash } from "node:crypto";

export const CLOSURE_SCHEMA = "midgard.canonical-v1-goal-closure.v1";

export const REQUIRED_AC_IDS = Object.freeze([
  "AC-00",
  "AC-01",
  "AC-02",
  "AC-03",
  "AC-C10",
  "AC-C20",
  "AC-C21",
  "AC-C30",
  "AC-C31",
  "AC-C40",
  "AC-C50",
  "AC-C60",
  "AC-Q10",
  "AC-Q11",
  "AC-Q12",
  "AC-Q13",
  "AC-Q14",
  "AC-Q15",
  "AC-Q16",
  "AC-Q17",
  "AC-Q18",
  "AC-W10",
  "AC-W11",
  "AC-W12",
  "AC-W13",
  "AC-W14",
  "AC-W15",
  "AC-W16",
  "AC-W17",
  "AC-W18",
  "AC-W19",
  "AC-X10",
  "AC-X11",
  "AC-X12",
  "AC-X13",
]);

const SHA256_PATTERN = /^[0-9a-f]{64}$/;
const COMMIT_PATTERN = /^[0-9a-f]{40}$/;
const CRITERION_STATUSES = new Set(["OPEN", "PASS"]);
const BINDING_STATUSES = new Set(["OPEN", "BOUND"]);

const fail = (path, message) => {
  throw new Error(`${path}: ${message}`);
};

const assertObject = (value, path) => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    fail(path, "expected an object");
  }
  return value;
};

const assertExactKeys = (value, path, keys) => {
  const object = assertObject(value, path);
  const actual = Object.keys(object).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    fail(
      path,
      `expected keys [${expected.join(", ")}], received [${actual.join(", ")}]`,
    );
  }
  return object;
};

// Same exactness as assertExactKeys for required keys, plus a closed set of
// optional keys. Used only where a field is genuinely absent for most records
// (a command result carries transaction hashes only when it submitted
// transactions); every other shape stays exact.
const assertKeys = (value, path, required, optional) => {
  const object = assertObject(value, path);
  const allowed = new Set([...required, ...optional]);
  const unexpected = Object.keys(object)
    .filter((key) => !allowed.has(key))
    .sort();
  const missing = [...required].filter((key) => !(key in object)).sort();
  if (unexpected.length > 0 || missing.length > 0) {
    fail(
      path,
      `expected keys [${[...required].sort().join(", ")}] with optional [${[
        ...optional,
      ]
        .sort()
        .join(", ")}], received [${Object.keys(object).sort().join(", ")}]`,
    );
  }
  return object;
};

const assertString = (value, path, { nonEmpty = true } = {}) => {
  if (typeof value !== "string" || (nonEmpty && value.length === 0)) {
    fail(path, "expected a non-empty string");
  }
  return value;
};

const assertNullableString = (value, path) => {
  if (value !== null) {
    assertString(value, path);
  }
  return value;
};

const assertBoolean = (value, path) => {
  if (typeof value !== "boolean") {
    fail(path, "expected a boolean");
  }
  return value;
};

const assertInteger = (value, path, minimum = 0) => {
  if (!Number.isInteger(value) || value < minimum) {
    fail(path, `expected an integer >= ${minimum}`);
  }
  return value;
};

const assertArray = (value, path) => {
  if (!Array.isArray(value)) {
    fail(path, "expected an array");
  }
  return value;
};

const assertSha256 = (value, path, { nullable = false } = {}) => {
  if (nullable && value === null) {
    return value;
  }
  if (typeof value !== "string" || !SHA256_PATTERN.test(value)) {
    fail(path, "expected a lowercase SHA-256 digest");
  }
  return value;
};

const assertCommit = (value, path, { nullable = false } = {}) => {
  if (nullable && value === null) {
    return value;
  }
  if (typeof value !== "string" || !COMMIT_PATTERN.test(value)) {
    fail(path, "expected a lowercase 40-character Git commit");
  }
  return value;
};

// A Cardano transaction identity is the exact 32-byte hash, lowercase hex.
const assertTransactionHash = (value, path) => {
  if (typeof value !== "string" || !SHA256_PATTERN.test(value)) {
    fail(path, "expected a lowercase 32-byte transaction hash");
  }
  return value;
};

const assertEnum = (value, path, allowed) => {
  if (!allowed.has(value)) {
    fail(path, `expected one of ${[...allowed].join(", ")}`);
  }
  return value;
};

// GOAL_SPEC §13.4 (owner amendment 2026-08-01): repository evidence binds a
// path, not a byte hash — Git already content-addresses tracked files.
const decodeFileBinding = (value, path) => {
  const object = assertExactKeys(value, path, ["path", "status"]);
  assertString(object.path, `${path}.path`);
  assertEnum(object.status, `${path}.status`, BINDING_STATUSES);
  return object;
};

const decodeProtectedPath = (value, path) => {
  const object = assertExactKeys(value, path, ["disposition", "path"]);
  assertString(object.path, `${path}.path`);
  assertEnum(
    object.disposition,
    `${path}.disposition`,
    new Set([
      "PROTECTED_EXACT_BASELINE",
      "PROTECTED_ACCEPTED_EXTERNAL_DRIFT",
      "PROTECTED_EXTERNAL_UNTRACKED",
    ]),
  );
  return object;
};

const decodeTool = (value, path) => {
  const object = assertExactKeys(value, path, ["command", "version"]);
  assertString(object.command, `${path}.command`);
  assertString(object.version, `${path}.version`);
  return object;
};

const decodeBinding = (value, path) => {
  const object = assertExactKeys(value, path, [
    "evidence",
    "identity",
    "status",
  ]);
  assertEnum(object.status, `${path}.status`, BINDING_STATUSES);
  assertNullableString(object.identity, `${path}.identity`);
  assertArray(object.evidence, `${path}.evidence`).forEach((entry, index) =>
    decodeFileBinding(entry, `${path}.evidence[${index}]`),
  );
  if (
    (object.status === "BOUND" && object.identity === null) ||
    (object.status === "OPEN" && object.identity !== null)
  ) {
    fail(path, "BOUND requires identity and OPEN requires identity=null");
  }
  if (object.status === "BOUND" && !allBound(object.evidence)) {
    fail(path, "BOUND requires all evidence files to be bound");
  }
  return object;
};

// GOAL_SPEC §13.2 records command, exit code, test count, duration, revision
// and artifact identity; §13.3/F41 additionally require the transaction
// identities of live acceptance evidence. `transactionHashes` is optional
// because only state-changing target-testnet commands produce them — when it
// is present it must name at least one exact 32-byte hash.
const decodeCommandResult = (value, path) => {
  const object = assertKeys(
    value,
    path,
    [
      "artifactIdentity",
      "command",
      "durationMs",
      "exitCode",
      "finishedAt",
      "id",
      "revision",
      "testCount",
    ],
    ["transactionHashes"],
  );
  assertString(object.id, `${path}.id`);
  assertArray(object.command, `${path}.command`).forEach((entry, index) =>
    assertString(entry, `${path}.command[${index}]`),
  );
  assertInteger(object.exitCode, `${path}.exitCode`);
  assertInteger(object.durationMs, `${path}.durationMs`);
  if (object.testCount !== null) {
    assertInteger(object.testCount, `${path}.testCount`, 1);
  }
  assertCommit(object.revision, `${path}.revision`);
  assertNullableString(object.artifactIdentity, `${path}.artifactIdentity`);
  assertString(object.finishedAt, `${path}.finishedAt`);
  if ("transactionHashes" in object) {
    const hashes = assertArray(
      object.transactionHashes,
      `${path}.transactionHashes`,
    );
    if (hashes.length === 0) {
      fail(
        `${path}.transactionHashes`,
        "omit the key instead of recording an empty transaction list",
      );
    }
    hashes.forEach((entry, index) =>
      assertTransactionHash(entry, `${path}.transactionHashes[${index}]`),
    );
    if (new Set(hashes).size !== hashes.length) {
      fail(`${path}.transactionHashes`, "duplicate transaction hash");
    }
  }
  return object;
};

// GOAL_SPEC §9.5: a decision row may end in a named residual launch blocker,
// but only when it is recorded in root `public_testnet_readiness.md` and in
// this manifest with the owner's explicit acceptance — never silence.
// `ownerAccepted` therefore carries the same evidence burden as a PASS
// criterion: acceptance without a bound evidence file is not acceptance.
const decodeResidualBlocker = (value, path) => {
  const object = assertExactKeys(value, path, [
    "description",
    "evidence",
    "id",
    "ownerAccepted",
  ]);
  assertString(object.id, `${path}.id`);
  assertString(object.description, `${path}.description`);
  assertBoolean(object.ownerAccepted, `${path}.ownerAccepted`);
  assertArray(object.evidence, `${path}.evidence`).forEach((entry, index) =>
    decodeFileBinding(entry, `${path}.evidence[${index}]`),
  );
  if (object.ownerAccepted && object.evidence.length === 0) {
    fail(path, "ownerAccepted requires at least one bound evidence file");
  }
  if (object.ownerAccepted && !allBound(object.evidence)) {
    fail(path, "ownerAccepted requires all evidence files to be bound");
  }
  return object;
};

const decodeCriterion = (value, path) => {
  const object = assertExactKeys(value, path, ["evidence", "id", "status"]);
  assertString(object.id, `${path}.id`);
  assertEnum(object.status, `${path}.status`, CRITERION_STATUSES);
  assertArray(object.evidence, `${path}.evidence`).forEach((entry, index) =>
    decodeFileBinding(entry, `${path}.evidence[${index}]`),
  );
  if (object.status === "PASS" && object.evidence.length === 0) {
    fail(path, "PASS requires at least one bound evidence file");
  }
  if (object.status === "PASS" && !allBound(object.evidence)) {
    fail(path, "PASS requires all evidence files to be bound");
  }
  return object;
};

export const decodeCanonicalV1GoalClosure = (input) => {
  const root = assertExactKeys(input, "$", [
    "acceptanceCriteria",
    "blueprint",
    "closureArtifacts",
    "commandResults",
    "deployment",
    "dirtyBaseline",
    "fixtureSets",
    "generatedAt",
    "parameterSnapshot",
    "release",
    "residualBlockers",
    "revision",
    "schema",
    "secrets",
    "targetTestnetParameterSnapshot",
    "toolchain",
    "validatorSet",
    "version",
  ]);

  if (root.schema !== CLOSURE_SCHEMA || root.version !== 1) {
    fail("$", `expected ${CLOSURE_SCHEMA} version 1`);
  }
  assertString(root.generatedAt, "$.generatedAt");

  const revision = assertExactKeys(root.revision, "$.revision", [
    "branch",
    "headCommit",
    "releaseCommit",
    "worktree",
  ]);
  assertString(revision.branch, "$.revision.branch");
  assertCommit(revision.headCommit, "$.revision.headCommit");
  assertCommit(revision.releaseCommit, "$.revision.releaseCommit", {
    nullable: true,
  });
  assertEnum(
    revision.worktree,
    "$.revision.worktree",
    new Set(["IN_PROGRESS", "BASELINE_RELATIVE_CLEAN"]),
  );

  const dirtyBaseline = assertExactKeys(root.dirtyBaseline, "$.dirtyBaseline", [
    "ledgerPath",
    "protectedPaths",
    "startingRevision",
  ]);
  assertString(dirtyBaseline.ledgerPath, "$.dirtyBaseline.ledgerPath");
  assertCommit(
    dirtyBaseline.startingRevision,
    "$.dirtyBaseline.startingRevision",
  );
  const protectedPaths = assertArray(
    dirtyBaseline.protectedPaths,
    "$.dirtyBaseline.protectedPaths",
  );
  protectedPaths.forEach((entry, index) =>
    decodeProtectedPath(entry, `$.dirtyBaseline.protectedPaths[${index}]`),
  );
  const uniqueProtectedPaths = new Set(protectedPaths.map(({ path }) => path));
  if (uniqueProtectedPaths.size !== protectedPaths.length) {
    fail("$.dirtyBaseline.protectedPaths", "duplicate path");
  }

  const toolchain = assertExactKeys(root.toolchain, "$.toolchain", [
    "aiken",
    "docker",
    "git",
    "make",
    "nix",
    "node",
    "pnpm",
  ]);
  for (const key of Object.keys(toolchain)) {
    decodeTool(toolchain[key], `$.toolchain.${key}`);
  }

  // GOAL_SPEC §13.3 and C70 require BOTH parameter snapshots: the trusted
  // Cardano mainnet effective/pending parameters that fix the capability floor
  // (`parameterSnapshot`, unchanged key so existing bindings keep their
  // meaning) and the target-testnet effective/pending parameters the
  // deployment is validated against (`targetTestnetParameterSnapshot`).
  // Capability parity derives from the least restrictive applicable value
  // across both (§3.1.10); one snapshot cannot answer both questions.
  decodeFileBinding(root.parameterSnapshot, "$.parameterSnapshot");
  decodeFileBinding(
    root.targetTestnetParameterSnapshot,
    "$.targetTestnetParameterSnapshot",
  );
  if (
    root.parameterSnapshot.path === root.targetTestnetParameterSnapshot.path
  ) {
    fail(
      "$.targetTestnetParameterSnapshot",
      "the mainnet capability-floor and target-testnet snapshots must be distinct files",
    );
  }
  decodeFileBinding(root.blueprint, "$.blueprint");
  decodeBinding(root.validatorSet, "$.validatorSet");
  assertArray(root.closureArtifacts, "$.closureArtifacts").forEach(
    (entry, index) => decodeFileBinding(entry, `$.closureArtifacts[${index}]`),
  );

  const deployment = assertExactKeys(root.deployment, "$.deployment", [
    "evidence",
    "identity",
    "network",
    "status",
  ]);
  if (deployment.network !== "preprod") {
    fail("$.deployment.network", "must be preprod");
  }
  assertEnum(deployment.status, "$.deployment.status", BINDING_STATUSES);
  assertNullableString(deployment.identity, "$.deployment.identity");
  assertArray(deployment.evidence, "$.deployment.evidence").forEach(
    (entry, index) =>
      decodeFileBinding(entry, `$.deployment.evidence[${index}]`),
  );
  if (
    (deployment.status === "BOUND" && deployment.identity === null) ||
    (deployment.status === "OPEN" && deployment.identity !== null)
  ) {
    fail(
      "$.deployment",
      "BOUND requires identity and OPEN requires identity=null",
    );
  }
  if (deployment.status === "BOUND" && !allBound(deployment.evidence)) {
    fail("$.deployment", "BOUND requires all evidence files to be bound");
  }

  assertArray(root.fixtureSets, "$.fixtureSets").forEach((entry, index) =>
    decodeFileBinding(entry, `$.fixtureSets[${index}]`),
  );
  assertArray(root.commandResults, "$.commandResults").forEach((entry, index) =>
    decodeCommandResult(entry, `$.commandResults[${index}]`),
  );

  const residualBlockers = assertArray(
    root.residualBlockers,
    "$.residualBlockers",
  );
  residualBlockers.forEach((entry, index) =>
    decodeResidualBlocker(entry, `$.residualBlockers[${index}]`),
  );
  const residualBlockerIds = residualBlockers.map(({ id }) => id);
  if (new Set(residualBlockerIds).size !== residualBlockerIds.length) {
    fail("$.residualBlockers", "duplicate id");
  }

  const criteria = assertArray(root.acceptanceCriteria, "$.acceptanceCriteria");
  criteria.forEach((entry, index) =>
    decodeCriterion(entry, `$.acceptanceCriteria[${index}]`),
  );
  const actualCriterionIds = criteria.map(({ id }) => id);
  if (
    actualCriterionIds.length !== REQUIRED_AC_IDS.length ||
    actualCriterionIds.some((id, index) => id !== REQUIRED_AC_IDS[index])
  ) {
    fail(
      "$.acceptanceCriteria",
      `must contain the exact ordered criteria ${REQUIRED_AC_IDS.join(", ")}`,
    );
  }

  const secrets = assertExactKeys(root.secrets, "$.secrets", [
    "evidence",
    "scanStatus",
  ]);
  assertEnum(
    secrets.scanStatus,
    "$.secrets.scanStatus",
    new Set(["OPEN", "PASS"]),
  );
  assertArray(secrets.evidence, "$.secrets.evidence").forEach((entry, index) =>
    decodeFileBinding(entry, `$.secrets.evidence[${index}]`),
  );
  if (secrets.scanStatus === "PASS" && secrets.evidence.length === 0) {
    fail("$.secrets", "PASS requires bound evidence");
  }
  if (secrets.scanStatus === "PASS" && !allBound(secrets.evidence)) {
    fail("$.secrets", "PASS requires all evidence files to be bound");
  }

  const release = assertExactKeys(root.release, "$.release", [
    "digest",
    "digestAlgorithm",
    "status",
  ]);
  if (release.digestAlgorithm !== "sha256") {
    fail("$.release.digestAlgorithm", "must be sha256");
  }
  assertEnum(release.status, "$.release.status", BINDING_STATUSES);
  assertSha256(release.digest, "$.release.digest", { nullable: true });
  if (
    (release.status === "BOUND" && release.digest === null) ||
    (release.status === "OPEN" && release.digest !== null)
  ) {
    fail("$.release", "BOUND requires digest and OPEN requires digest=null");
  }

  return root;
};

const stableValue = (value) => {
  if (Array.isArray(value)) {
    return value.map(stableValue);
  }
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map((key) => [key, stableValue(value[key])]),
    );
  }
  return value;
};

export const canonicalClosureDigest = (manifest) => {
  const decoded = decodeCanonicalV1GoalClosure(manifest);
  const payload = structuredClone(decoded);
  payload.release = {
    digest: null,
    digestAlgorithm: "sha256",
    status: "OPEN",
  };
  return createHash("sha256")
    .update(`${JSON.stringify(stableValue(payload))}\n`, "utf8")
    .digest("hex");
};

export const isBoundFile = (binding) => binding.status === "BOUND";

export const allBound = (bindings) => bindings.every(isBoundFile);

export const isReleaseReady = (manifest) =>
  manifest.revision.worktree === "BASELINE_RELATIVE_CLEAN" &&
  manifest.revision.releaseCommit !== null &&
  manifest.revision.headCommit === manifest.revision.releaseCommit &&
  manifest.parameterSnapshot.status === "BOUND" &&
  manifest.targetTestnetParameterSnapshot.status === "BOUND" &&
  manifest.blueprint.status === "BOUND" &&
  manifest.validatorSet.status === "BOUND" &&
  allBound(manifest.validatorSet.evidence) &&
  manifest.closureArtifacts.length >= 5 &&
  manifest.closureArtifacts.every(isBoundFile) &&
  manifest.deployment.status === "BOUND" &&
  allBound(manifest.deployment.evidence) &&
  manifest.fixtureSets.length > 0 &&
  manifest.fixtureSets.every(isBoundFile) &&
  manifest.commandResults.length > 0 &&
  manifest.commandResults.every(({ exitCode }) => exitCode === 0) &&
  manifest.acceptanceCriteria.every(
    ({ status, evidence }) =>
      status === "PASS" && evidence.length > 0 && allBound(evidence),
  ) &&
  manifest.secrets.scanStatus === "PASS" &&
  manifest.secrets.evidence.length > 0 &&
  allBound(manifest.secrets.evidence) &&
  // §9.5: an unaccepted or unevidenced residual launch blocker keeps release
  // closed. An empty list is the healthy case, not a missing check.
  manifest.residualBlockers.every(
    ({ ownerAccepted, evidence }) =>
      ownerAccepted === true && evidence.length > 0 && allBound(evidence),
  ) &&
  manifest.release.status === "BOUND";

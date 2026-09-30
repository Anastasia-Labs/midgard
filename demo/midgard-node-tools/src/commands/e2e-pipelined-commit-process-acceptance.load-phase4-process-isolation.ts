import { createHash } from "node:crypto";
import {
  access,
  mkdir,
  readdir,
  readFile,
  realpath,
  writeFile,
} from "node:fs/promises";
import { dirname, isAbsolute, join, resolve } from "node:path";

import { loadDotenvFile } from "midgard-node/e2e/env";

import {
  cleanupOwnedProcessGroupAndRecord,
  generateOwnedProcessRunToken,
} from "../e2e/process-ownership.js";
import {
  decodePhase4MatchedSnapshotIdentity,
  validatePhase4PhasRegistrationProof,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-phas-registration-proof.js";
import {
  toolsCli,
  validatePhase4PhasRegistrationTransactionBody,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-phas-registration-transaction-body.js";
import {
  ACCEPTANCE_ENABLE_VALUE,
  type Phase4ProcessIsolationIdentity,
  requiredEnv,
  requiredValue,
  sha256File,
  validatePhase4ProcessIsolationValues,
} from "./e2e-pipelined-commit-process-acceptance.validate-phase4-process-isolation-values.js";

export let activeProcessIsolation: Phase4ProcessIsolationIdentity | undefined;

export let activeIsolatedChildEnv: Readonly<NodeJS.ProcessEnv> | undefined;

export let activeProcessOwnership:
  | { readonly runToken: string; readonly recordsDir: string }
  | undefined;

const PHASE4_CHILD_OS_ENV_KEYS = [
  "HOME",
  "LANG",
  "LC_ALL",
  "PATH",
  "SHELL",
  "TMPDIR",
  "TZ",
] as const;

export const buildPhase4IsolatedChildEnv = ({
  values,
  deploymentManifestPath,
  baseEnv = process.env,
}: {
  readonly values: Readonly<Record<string, string>>;
  readonly deploymentManifestPath: string;
  readonly baseEnv?: Readonly<NodeJS.ProcessEnv>;
}): Readonly<NodeJS.ProcessEnv> => {
  const osEnv = Object.fromEntries(
    PHASE4_CHILD_OS_ENV_KEYS.flatMap((key) => {
      const value = baseEnv[key];
      return value === undefined ? [] : [[key, value] as const];
    }),
  );
  return Object.freeze({
    ...osEnv,
    ...values,
    MIDGARD_DEPLOYMENT_MANIFEST_PATH: deploymentManifestPath,
  });
};

export const initializeProcessOwnership = async (
  runDir: string,
): Promise<void> => {
  const ownershipDir = join(runDir, "owned-process-groups");
  const tokenPath = join(ownershipDir, "run-token");
  await mkdir(ownershipDir, { recursive: true, mode: 0o700 });
  let runToken: string;
  try {
    runToken = (await readFile(tokenPath, "utf8")).trim();
  } catch (error) {
    if (
      typeof error !== "object" ||
      error === null ||
      !("code" in error) ||
      error.code !== "ENOENT"
    ) {
      throw error;
    }
    runToken = generateOwnedProcessRunToken();
    await writeFile(tokenPath, `${runToken}\n`, {
      encoding: "utf8",
      flag: "wx",
      mode: 0o600,
    });
  }
  if (!/^[a-f0-9]{64}$/u.test(runToken)) {
    throw new Error("Phase 4 owned process-group run token is invalid");
  }
  const orphanRecords = (await readdir(ownershipDir, { withFileTypes: true }))
    .filter((entry) => entry.isFile() && entry.name.endsWith(".json"))
    .map((entry) => join(ownershipDir, entry.name))
    .sort((left, right) => left.localeCompare(right));
  for (const recordPath of orphanRecords) {
    const cleanup = await cleanupOwnedProcessGroupAndRecord({
      spec: { recordPath, runToken },
    });
    if (!cleanup.success) {
      throw new Error(
        `Phase 4 refuses to start while owned process-group cleanup is unresolved for ${recordPath}: ${cleanup.error ?? cleanup.ownershipValidation.reason}`,
      );
    }
  }
  activeProcessOwnership = Object.freeze({
    runToken,
    recordsDir: ownershipDir,
  });
};

const loadPhase4ProcessIsolation = async (
  cwd: string,
): Promise<Phase4ProcessIsolationIdentity> => {
  const requestedEnvFile = requiredEnv("MIDGARD_PHASE4_PROCESS_ENV_FILE");
  const requestedManifest = requiredEnv(
    "MIDGARD_PHASE4_PROCESS_DEPLOYMENT_MANIFEST_PATH",
  );
  const requestedSnapshotIdentity = requiredValue(
    await loadDotenvFile(requestedEnvFile),
    "MIDGARD_PHASE4_SNAPSHOT_IDENTITY_PATH",
  );
  const requestedBlueprint = requiredValue(
    await loadDotenvFile(requestedEnvFile),
    "MIDGARD_REAL_BLUEPRINT_PATH",
  );
  if (!isAbsolute(requestedEnvFile) || !isAbsolute(requestedManifest)) {
    throw new Error(
      "Phase 4 process env and deployment manifest paths must be absolute",
    );
  }
  if (
    !isAbsolute(requestedSnapshotIdentity) ||
    !isAbsolute(requestedBlueprint)
  ) {
    throw new Error(
      "Phase 4 snapshot identity and Aiken blueprint paths must be absolute",
    );
  }
  const [
    envFile,
    deploymentManifestPath,
    snapshotIdentityPath,
    blueprintPath,
    defaultEnv,
    defaultManifest,
  ] = await Promise.all([
    realpath(requestedEnvFile),
    realpath(requestedManifest),
    realpath(requestedSnapshotIdentity),
    realpath(requestedBlueprint),
    realpath(resolve(cwd, ".env")),
    realpath(resolve(cwd, "deploymentInfo/contract-deployment-info.json")),
  ]);
  if (envFile === defaultEnv || deploymentManifestPath === defaultManifest) {
    throw new Error(
      "Phase 4 process acceptance refuses the checkout live env or deployment manifest",
    );
  }
  const values = await loadDotenvFile(envFile);
  if (
    Object.keys(values).some(
      (key) =>
        key.startsWith("MIDGARD_PHASE4_PROCESS_") ||
        key.startsWith("MIDGARD_PHASE4_T1_"),
    )
  ) {
    throw new Error(
      "Phase 4 process/T1 authorization controls may not be supplied by the child env file",
    );
  }
  const validated = validatePhase4ProcessIsolationValues(values);
  const deploymentManifestSha256 = await sha256File(deploymentManifestPath);
  const snapshotIdentitySha256 = await sha256File(snapshotIdentityPath);
  const snapshotIdentity = decodePhase4MatchedSnapshotIdentity(
    JSON.parse(await readFile(snapshotIdentityPath, "utf8")) as unknown,
  );
  if (
    snapshotIdentity.schemaVersion !==
      "midgard-phase4-matched-snapshot-identity-v1" ||
    snapshotIdentity.composeProject !== validated.composeProject ||
    snapshotIdentity.networkMagic !== validated.networkMagic ||
    snapshotIdentity.postgresDatabase !== validated.postgresDatabase ||
    snapshotIdentity.deploymentManifestSha256 !== deploymentManifestSha256 ||
    snapshotIdentity.blueprintSha256 !== (await sha256File(blueprintPath)) ||
    !Number.isSafeInteger(snapshotIdentity.cardanoTip?.slot) ||
    typeof snapshotIdentity.cardanoTip?.hash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(snapshotIdentity.cardanoTip.hash) ||
    !Number.isSafeInteger(snapshotIdentity.kupoCheckpoint) ||
    snapshotIdentity.kupoCheckpoint !== snapshotIdentity.cardanoTip.slot
  ) {
    throw new Error(
      "Phase 4 snapshot identity does not match the isolated run identity",
    );
  }
  const imageEntries = [
    snapshotIdentity.images?.cardanoNode,
    snapshotIdentity.images?.ogmios,
    snapshotIdentity.images?.kupo,
    snapshotIdentity.images?.postgres,
  ];
  if (
    imageEntries.some(
      (image) =>
        typeof image?.ref !== "string" ||
        !/@sha256:[a-f0-9]{64}$/u.test(image.ref) ||
        typeof image.id !== "string" ||
        image.id.trim().length === 0,
    )
  ) {
    throw new Error(
      "Phase 4 snapshot identity must bind immutable image refs and effective image IDs",
    );
  }
  for (const name of [
    "sourceSha256",
    "distSha256",
    "toolsSourceSha256",
    "toolsDistSha256",
    "genesisSha256",
    "configSha256",
    "acceptanceEnvSha256",
    "composeSha256",
    "phase4AssetsSha256",
    "phasRegistrationProofSha256",
  ] as const) {
    if (
      !/^[a-f0-9]{64}$/u.test(String(snapshotIdentity.artifacts?.[name] ?? ""))
    ) {
      throw new Error(
        `Phase 4 snapshot identity is missing artifact hash ${name}`,
      );
    }
  }
  const snapshotPhasRegistration = validatePhase4PhasRegistrationProof(
    snapshotIdentity.phasRegistration,
    "Phase 4 snapshot PHAS registration proof",
  );
  const snapshotTransactionBodyPath = await realpath(
    resolve(
      dirname(snapshotIdentityPath),
      "phas-registration-transaction-body.json",
    ),
  );
  const snapshotTransactionBodyBytes = await readFile(
    snapshotTransactionBodyPath,
  );
  if (
    createHash("sha256").update(snapshotTransactionBodyBytes).digest("hex") !==
    snapshotPhasRegistration.transactionBody.artifactSha256
  ) {
    throw new Error(
      "Phase 4 snapshot PHAS transaction-body artifact does not match its proof digest",
    );
  }
  const snapshotPhasRegistrationTransactionBody =
    validatePhase4PhasRegistrationTransactionBody(
      JSON.parse(snapshotTransactionBodyBytes.toString("utf8")) as unknown,
      snapshotPhasRegistration,
    );
  if (
    snapshotPhasRegistration.networkMagic !== validated.networkMagic ||
    snapshotPhasRegistration.cardanoImage.ref !==
      snapshotIdentity.images?.cardanoNode?.ref ||
    snapshotPhasRegistration.cardanoImage.id !==
      snapshotIdentity.images?.cardanoNode?.id ||
    snapshotPhasRegistration.observedAtTip.slot !==
      snapshotIdentity.cardanoTip.slot ||
    snapshotPhasRegistration.observedAtTip.hash !==
      snapshotIdentity.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 snapshot PHAS registration proof is not bound to the isolated ledger and pinned Cardano image",
    );
  }
  activeIsolatedChildEnv = buildPhase4IsolatedChildEnv({
    values,
    deploymentManifestPath,
  });
  // Database and Lucid layers execute in this controller process. Child
  // processes never inherit this mutable object; they use the frozen snapshot.
  Object.assign(process.env, values, {
    MIDGARD_DEPLOYMENT_MANIFEST_PATH: deploymentManifestPath,
  });
  return {
    envFile,
    deploymentManifestPath,
    deploymentManifestSha256,
    snapshotIdentityPath,
    snapshotIdentitySha256,
    snapshotCardanoTip: {
      slot: snapshotIdentity.cardanoTip.slot,
      hash: snapshotIdentity.cardanoTip.hash,
    },
    snapshotKupoCheckpoint: snapshotIdentity.kupoCheckpoint,
    snapshotBlueprintSha256: snapshotIdentity.blueprintSha256,
    snapshotPhasRegistrationProofSha256:
      snapshotIdentity.artifacts.phasRegistrationProofSha256,
    snapshotPhasRegistration,
    snapshotPhasRegistrationTransactionBody,
    ...validated,
  };
};

export const assertAcceptancePreconditions = async (
  cwd: string,
): Promise<Phase4ProcessIsolationIdentity> => {
  if (process.env.MIDGARD_DOTENV_MODE !== "disabled") {
    throw new Error(
      "Set MIDGARD_DOTENV_MODE=disabled before starting the Phase 4 acceptance controller",
    );
  }
  if (
    process.env.MIDGARD_PHASE4_PROCESS_ACCEPTANCE !== ACCEPTANCE_ENABLE_VALUE
  ) {
    throw new Error(
      `Set MIDGARD_PHASE4_PROCESS_ACCEPTANCE=${ACCEPTANCE_ENABLE_VALUE} to authorize the destructive matched-snapshot devnet acceptance run`,
    );
  }
  if (process.env.MIDGARD_PHASE4_PROCESS_TARGET !== "local-devnet") {
    throw new Error(
      "MIDGARD_PHASE4_PROCESS_TARGET must be local-devnet; this acceptance command refuses public-network mutation",
    );
  }
  requiredEnv("MIDGARD_PHASE4_MATCHED_RESET_COMMAND");
  requiredEnv("MIDGARD_PHASE4_T1_RECOVERY_COMMAND");
  const isolation = await loadPhase4ProcessIsolation(cwd);
  await Promise.all([
    access(isolation.envFile),
    access(resolve(cwd, "dist/index.js")),
    access(toolsCli()),
    access(isolation.deploymentManifestPath),
  ]);
  activeProcessIsolation = isolation;
  return isolation;
};

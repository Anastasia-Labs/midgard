import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { isAbsolute } from "node:path";

import { positiveSafeInteger } from "midgard-node/artifact-schema";

export const ACCEPTANCE_ENABLE_VALUE = "journal-kill-recovery-live-v1";

export const RESET_ATTESTATION_SCHEMA =
  "midgard-phase4-local-devnet-reset-attestation-v1";

export const ISOLATED_DATABASE_PREFIX = "midgard_phase4_process_";

export const ISOLATED_COMPOSE_PREFIX = "midgard_phase4_process_";

export type Phase4PhasRegistrationProof = {
  readonly schemaVersion: "midgard-phase4-phas-registration-proof-v1";
  readonly source: "cardano-cli-local-state-query";
  readonly readOnly: true;
  readonly registered: true;
  readonly cardanoImage: { readonly ref: string; readonly id: string };
  readonly networkMagic: number;
  readonly manifestId: string;
  readonly registrationTxHash: string;
  readonly rewardAddress: string;
  readonly rewardAddressBase16: string;
  readonly scriptHash: string;
  readonly transactionBody: {
    readonly schemaVersion: "midgard-phas-registration-transaction-body-v1";
    readonly artifactSha256: string;
    readonly cborSha256: string;
    readonly cborSizeBytes: number;
    readonly cardanoCliTxHash: string;
    readonly certificate: {
      readonly kind: "stake_registration";
      readonly index: 0;
      readonly count: 1;
      readonly credentialType: "script";
      readonly scriptHash: string;
    };
  };
  readonly registrationDepositLovelace: number;
  readonly confirmation: {
    readonly slot: number;
    readonly blockHeaderHash: string;
  };
  readonly observedAtTip: { readonly slot: number; readonly hash: string };
};

export type Phase4ProcessIsolationIdentity = {
  readonly envFile: string;
  readonly deploymentManifestPath: string;
  readonly deploymentManifestSha256: string;
  readonly snapshotIdentityPath: string;
  readonly snapshotIdentitySha256: string;
  readonly snapshotCardanoTip: { readonly slot: number; readonly hash: string };
  readonly snapshotKupoCheckpoint: number;
  readonly snapshotBlueprintSha256: string;
  readonly snapshotPhasRegistrationProofSha256: string;
  readonly snapshotPhasRegistration: Phase4PhasRegistrationProof;
  readonly snapshotPhasRegistrationTransactionBody: {
    readonly type: "Unwitnessed Tx ConwayEra";
    readonly description: string;
    readonly cborHex: string;
  };
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly postgresDatabase: string;
  readonly postgresPort: number;
  readonly ogmiosPort: number;
  readonly kupoPort: number;
};

export type Phase4ResetAttestation = {
  readonly schemaVersion: typeof RESET_ATTESTATION_SCHEMA;
  readonly scenarioLabel: string;
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly postgresDatabase: string;
  readonly deploymentManifestSha256: string;
  readonly snapshotSetSha256: string;
  readonly snapshotIdentitySha256: string;
  readonly phasRegistrationProofSha256: string;
  readonly phasRegistration: Phase4PhasRegistrationProof;
  readonly cardanoTip: { readonly slot: number; readonly hash: string };
  readonly kupoCheckpoint: number;
};

export type Phase4MatchedSnapshotIdentity = {
  readonly schemaVersion: "midgard-phase4-matched-snapshot-identity-v1";
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly postgresDatabase: string;
  readonly deploymentManifestSha256: string;
  readonly blueprintSha256: string;
  readonly images: Readonly<
    Record<
      "cardanoNode" | "ogmios" | "kupo" | "postgres",
      { readonly ref: string; readonly id: string }
    >
  >;
  readonly artifacts: Readonly<
    Record<
      | "sourceSha256"
      | "distSha256"
      | "toolsSourceSha256"
      | "toolsDistSha256"
      | "genesisSha256"
      | "configSha256"
      | "acceptanceEnvSha256"
      | "composeSha256"
      | "phase4AssetsSha256"
      | "phasRegistrationProofSha256",
      string
    >
  >;
  readonly phasRegistration: Phase4PhasRegistrationProof;
  readonly cardanoTip: { readonly slot: number; readonly hash: string };
  readonly kupoCheckpoint: number;
};

export const requiredEnv = (name: string): string => {
  const value = process.env[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Missing required ${name}`);
  }
  return value;
};

export const sha256File = async (path: string): Promise<string> =>
  createHash("sha256")
    .update(await readFile(path))
    .digest("hex");

export const requiredValue = (
  values: Readonly<Record<string, string>>,
  name: string,
): string => {
  const value = values[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Phase 4 process env file is missing ${name}`);
  }
  return value;
};

const positiveInteger = (raw: string, label: string): number =>
  positiveSafeInteger(Number(raw), label);

const isolatedEndpointPort = (
  raw: string,
  label: string,
  protectedPorts: ReadonlySet<number>,
): number => {
  const endpoint = new URL(raw);
  if (endpoint.hostname !== "127.0.0.1" && endpoint.hostname !== "localhost") {
    throw new Error(`${label} must use a loopback host`);
  }
  if (endpoint.port.length === 0) {
    throw new Error(`${label} must declare an explicit isolated port`);
  }
  const port = positiveInteger(endpoint.port, `${label} port`);
  if (protectedPorts.has(port)) {
    throw new Error(`${label} may not use protected live port ${port}`);
  }
  return port;
};

export const validatePhase4ProcessIsolationValues = (
  values: Readonly<Record<string, string>>,
): Omit<
  Phase4ProcessIsolationIdentity,
  | "envFile"
  | "deploymentManifestPath"
  | "deploymentManifestSha256"
  | "snapshotIdentityPath"
  | "snapshotIdentitySha256"
  | "snapshotCardanoTip"
  | "snapshotKupoCheckpoint"
  | "snapshotBlueprintSha256"
  | "snapshotPhasRegistrationProofSha256"
  | "snapshotPhasRegistration"
  | "snapshotPhasRegistrationTransactionBody"
> => {
  const postgresHost = requiredValue(values, "POSTGRES_HOST");
  if (postgresHost !== "127.0.0.1") {
    throw new Error("Phase 4 isolated POSTGRES_HOST must be 127.0.0.1");
  }
  const postgresPort = positiveInteger(
    requiredValue(values, "POSTGRES_PORT"),
    "POSTGRES_PORT",
  );
  if (postgresPort === 5432 || postgresPort === 5433) {
    throw new Error(
      `Phase 4 isolated Postgres may not use protected live port ${postgresPort}`,
    );
  }
  const postgresDatabase = requiredValue(values, "POSTGRES_DB");
  if (!postgresDatabase.startsWith(ISOLATED_DATABASE_PREFIX)) {
    throw new Error(
      `Phase 4 POSTGRES_DB must start with ${ISOLATED_DATABASE_PREFIX}`,
    );
  }
  if (requiredValue(values, "L1_PROVIDER") !== "Kupmios") {
    throw new Error("Phase 4 process acceptance requires L1_PROVIDER=Kupmios");
  }
  if (
    requiredValue(values, "MIN_FEE_A") !== "0" ||
    requiredValue(values, "MIN_FEE_B") !== "0"
  ) {
    throw new Error(
      "Phase 4 process acceptance requires pinned MIN_FEE_A=0 and MIN_FEE_B=0",
    );
  }
  if (requiredValue(values, "RUN_GENESIS_ON_STARTUP") !== "false") {
    throw new Error("Phase 4 isolated nodes must use attach/resume");
  }
  if (requiredValue(values, "MIDGARD_DOTENV_MODE") !== "disabled") {
    throw new Error(
      "Phase 4 isolated nodes must disable checkout dotenv loading",
    );
  }
  if (requiredValue(values, "NETWORK") !== "Custom") {
    throw new Error("Phase 4 process acceptance requires NETWORK=Custom");
  }
  for (const seed of [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ]) {
    requiredValue(values, seed);
  }
  // Every node refuses to start without a pinned native owner; refuse here, by
  // name, instead of waiting for a seed node that never becomes ready.
  if (!isAbsolute(requiredValue(values, "MPF_NATIVE_OWNER_BINARY_PATH"))) {
    throw new Error(
      "Phase 4 MPF_NATIVE_OWNER_BINARY_PATH must be absolute; regenerate acceptance.env with write-acceptance-env.sh",
    );
  }
  if (
    !/^[0-9a-f]{64}$/u.test(
      requiredValue(values, "MPF_NATIVE_OWNER_BINARY_SHA256"),
    )
  ) {
    throw new Error(
      "Phase 4 MPF_NATIVE_OWNER_BINARY_SHA256 must be 64 lowercase hex characters",
    );
  }
  if (values.MPF_NATIVE_OWNER_SIDECAR_PATH !== undefined) {
    throw new Error(
      "Phase 4 nodes derive their owner sidecar from their own LEDGER_MPF_DB_PATH; MPF_NATIVE_OWNER_SIDECAR_PATH must be unset",
    );
  }
  const composeProject = requiredValue(
    values,
    "MIDGARD_PHASE4_COMPOSE_PROJECT",
  );
  if (
    !composeProject.startsWith(ISOLATED_COMPOSE_PREFIX) ||
    !/^[a-z0-9_-]+$/u.test(composeProject)
  ) {
    throw new Error(
      `MIDGARD_PHASE4_COMPOSE_PROJECT must use ${ISOLATED_COMPOSE_PREFIX} and safe lowercase characters`,
    );
  }
  const networkMagic = positiveInteger(
    requiredValue(values, "MIDGARD_PHASE4_NETWORK_MAGIC"),
    "MIDGARD_PHASE4_NETWORK_MAGIC",
  );
  const ogmiosPort = isolatedEndpointPort(
    requiredValue(values, "L1_OGMIOS_KEY"),
    "L1_OGMIOS_KEY",
    new Set([1337]),
  );
  const kupoPort = isolatedEndpointPort(
    requiredValue(values, "L1_KUPO_KEY"),
    "L1_KUPO_KEY",
    new Set([1442]),
  );
  if (new Set([postgresPort, ogmiosPort, kupoPort]).size !== 3) {
    throw new Error("Phase 4 isolated service ports must be distinct");
  }
  return {
    composeProject,
    networkMagic,
    postgresDatabase,
    postgresPort,
    ogmiosPort,
    kupoPort,
  };
};

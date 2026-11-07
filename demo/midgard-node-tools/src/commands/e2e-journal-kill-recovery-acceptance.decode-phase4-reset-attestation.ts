import { join } from "node:path";

import { exactObjectKeys } from "midgard-node/exact-object-keys";

import { type OwnedProcessGroupSpec } from "../e2e/process-ownership.js";
import {
  activeIsolatedChildEnv,
  activeProcessOwnership,
} from "./e2e-journal-kill-recovery-acceptance.load-phase4-process-isolation.js";
import { validatePhase4PhasRegistrationProof } from "./e2e-journal-kill-recovery-acceptance.validate-phase4-phas-registration-proof.js";
import {
  ISOLATED_COMPOSE_PREFIX,
  ISOLATED_DATABASE_PREFIX,
  type Phase4ResetAttestation,
  RESET_ATTESTATION_SCHEMA,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-process-isolation-values.js";

export const isolatedChildEnv = (): Readonly<NodeJS.ProcessEnv> => {
  if (activeIsolatedChildEnv === undefined) {
    throw new Error("Phase 4 immutable child environment is not initialized");
  }
  return activeIsolatedChildEnv;
};

export const requiredIsolatedChildEnv = (name: string): string => {
  const value = isolatedChildEnv()[name]?.trim();
  if (value === undefined || value.length === 0) {
    throw new Error(`Phase 4 immutable child environment is missing ${name}`);
  }
  return value;
};

export const ownershipForProcess = (
  label: string,
  nodeId: string,
): OwnedProcessGroupSpec => {
  if (activeProcessOwnership === undefined) {
    throw new Error("Phase 4 process ownership identity is not initialized");
  }
  const safeName = `${label}-${nodeId}`.replaceAll(/[^a-zA-Z0-9_.-]/gu, "_");
  return {
    recordPath: join(activeProcessOwnership.recordsDir, `${safeName}.json`),
    runToken: activeProcessOwnership.runToken,
  };
};

export type ProcessResult = {
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly output: string;
  readonly timedOut: boolean;
};

export const decodePhase4ResetAttestation = (
  value: unknown,
): Phase4ResetAttestation => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "scenarioLabel",
      "composeProject",
      "networkMagic",
      "postgresDatabase",
      "deploymentManifestSha256",
      "snapshotSetSha256",
      "snapshotIdentitySha256",
      "phasRegistrationProofSha256",
      "phasRegistration",
      "cardanoTip",
      "kupoCheckpoint",
    ]) ||
    !exactObjectKeys(value.cardanoTip, ["slot", "hash"])
  ) {
    throw new Error(
      "Phase 4 reset attestation fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== RESET_ATTESTATION_SCHEMA ||
    typeof value.scenarioLabel !== "string" ||
    value.scenarioLabel.length === 0 ||
    typeof value.composeProject !== "string" ||
    !value.composeProject.startsWith(ISOLATED_COMPOSE_PREFIX) ||
    !/^[a-z0-9_-]+$/u.test(value.composeProject) ||
    !Number.isSafeInteger(value.networkMagic) ||
    (value.networkMagic as number) <= 0 ||
    typeof value.postgresDatabase !== "string" ||
    !value.postgresDatabase.startsWith(ISOLATED_DATABASE_PREFIX) ||
    typeof value.deploymentManifestSha256 !== "string"
  ) {
    throw new Error(
      "Phase 4 reset attestation contains a noncanonical V1 value",
    );
  }
  for (const key of [
    "deploymentManifestSha256",
    "snapshotSetSha256",
    "snapshotIdentitySha256",
    "phasRegistrationProofSha256",
  ] as const) {
    if (typeof value[key] !== "string" || !/^[a-f0-9]{64}$/u.test(value[key])) {
      throw new Error(`Phase 4 reset attestation has invalid ${key}`);
    }
  }
  if (
    !Number.isSafeInteger(value.cardanoTip.slot) ||
    (value.cardanoTip.slot as number) < 0 ||
    typeof value.cardanoTip.hash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.cardanoTip.hash)
  ) {
    throw new Error("Phase 4 reset attestation has invalid Cardano tip");
  }
  if (value.kupoCheckpoint !== value.cardanoTip.slot) {
    throw new Error(
      "Phase 4 reset attestation Kupo checkpoint must equal the Cardano tip slot",
    );
  }
  const phasRegistration = validatePhase4PhasRegistrationProof(
    value.phasRegistration,
    "Phase 4 reset PHAS registration proof",
  );
  if (
    phasRegistration.networkMagic !== value.networkMagic ||
    phasRegistration.observedAtTip.slot !== value.cardanoTip.slot ||
    phasRegistration.observedAtTip.hash !== value.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 reset PHAS registration proof is not bound to its attestation",
    );
  }
  return value as Phase4ResetAttestation;
};

export const parseResetAttestation = (
  output: string,
): Phase4ResetAttestation => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(output.trim());
  } catch (cause) {
    throw new Error(
      `Phase 4 reset command must emit exactly one JSON attestation: ${String(cause)}`,
    );
  }
  return decodePhase4ResetAttestation(parsed);
};

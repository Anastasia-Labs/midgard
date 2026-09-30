import { exactObjectKeys } from "midgard-node/exact-object-keys";
import { writeTextFileAtomicNoReplace } from "midgard-node/files/atomic-write";

import {
  decodePhase4T1AdvanceEvidence,
  type Phase4T1AdvanceEvidence,
  type Phase4T1RecoveryAttestation,
} from "./phase4-t1-recovery.assert-phase4-t1-noop-advance.js";
import {
  CARDANO_HASH,
  L2_HEADER_HASH,
  PHASE4_T1_PROBE_SCHEMA,
  PHASE4_T1_RECOVERY_SCHEMA,
  type Phase4T1ProbeEvidence,
  requireCardanoHash,
  requireL2HeaderHash,
  SAFE_ATTEMPT_ID,
  SHA256,
} from "./phase4-t1-recovery.decode-phase4-t1-canonical-tip.js";
import { decodePhase4T1ProbeEvidence } from "./phase4-t1-recovery.fetch-phase4-t1-canonical-state.js";

export const decodePhase4T1RecoveryAttestation = (
  value: unknown,
): Phase4T1RecoveryAttestation => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "scenarioLabel",
      "attemptId",
      "composeProject",
      "networkMagic",
      "snapshotSetSha256",
      "snapshotIdentitySha256",
      "abandonedHeaderHash",
      "abandonedSubmittedTxHash",
      "baseHeaderHash",
      "recoveredTipHeaderHash",
      "canonicalAdvanceTxHash",
      "journalSha256Before",
      "journalSha256After",
      "cardanoTip",
      "kupoCheckpoint",
    ]) ||
    !exactObjectKeys(value.cardanoTip, ["slot", "hash"])
  ) {
    throw new Error(
      "T1 recovery attestation fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== PHASE4_T1_RECOVERY_SCHEMA ||
    typeof value.scenarioLabel !== "string" ||
    value.scenarioLabel.length === 0 ||
    typeof value.attemptId !== "string" ||
    !SAFE_ATTEMPT_ID.test(value.attemptId) ||
    typeof value.composeProject !== "string" ||
    !value.composeProject.startsWith("midgard_phase4_process_") ||
    !/^[a-z0-9_-]+$/u.test(value.composeProject) ||
    !Number.isSafeInteger(value.networkMagic) ||
    (value.networkMagic as number) <= 0 ||
    typeof value.snapshotSetSha256 !== "string" ||
    !SHA256.test(value.snapshotSetSha256) ||
    typeof value.snapshotIdentitySha256 !== "string" ||
    !SHA256.test(value.snapshotIdentitySha256) ||
    typeof value.abandonedHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.abandonedHeaderHash) ||
    typeof value.abandonedSubmittedTxHash !== "string" ||
    !CARDANO_HASH.test(value.abandonedSubmittedTxHash) ||
    typeof value.baseHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.baseHeaderHash) ||
    typeof value.recoveredTipHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.recoveredTipHeaderHash) ||
    value.recoveredTipHeaderHash === value.abandonedHeaderHash ||
    value.recoveredTipHeaderHash === value.baseHeaderHash ||
    typeof value.canonicalAdvanceTxHash !== "string" ||
    !CARDANO_HASH.test(value.canonicalAdvanceTxHash) ||
    typeof value.journalSha256Before !== "string" ||
    !SHA256.test(value.journalSha256Before) ||
    value.journalSha256After !== value.journalSha256Before ||
    !Number.isSafeInteger(value.cardanoTip.slot) ||
    (value.cardanoTip.slot as number) < 0 ||
    typeof value.cardanoTip.hash !== "string" ||
    !CARDANO_HASH.test(value.cardanoTip.hash) ||
    value.kupoCheckpoint !== value.cardanoTip.slot
  ) {
    throw new Error("T1 recovery attestation contains a noncanonical V1 value");
  }
  return value as Phase4T1RecoveryAttestation;
};

export const parseAndValidatePhase4T1RecoveryAttestation = ({
  output,
  expected,
}: {
  readonly output: string;
  readonly expected: {
    readonly scenarioLabel: string;
    readonly attemptId: string;
    readonly composeProject: string;
    readonly networkMagic: number;
    readonly snapshotIdentitySha256: string;
    readonly abandonedHeaderHash: string;
    readonly abandonedSubmittedTxHash: string;
    readonly baseHeaderHash: string;
  };
}): Phase4T1RecoveryAttestation => {
  let value: unknown;
  try {
    value = JSON.parse(output.trim());
  } catch (cause) {
    throw new Error(
      `T1 recovery command must emit exactly one JSON object: ${String(cause)}`,
    );
  }
  const attestation = decodePhase4T1RecoveryAttestation(value);
  const exact = {
    schemaVersion: PHASE4_T1_RECOVERY_SCHEMA,
    scenarioLabel: expected.scenarioLabel,
    attemptId: expected.attemptId,
    composeProject: expected.composeProject,
    networkMagic: expected.networkMagic,
    snapshotIdentitySha256: expected.snapshotIdentitySha256,
    abandonedHeaderHash: requireL2HeaderHash(
      expected.abandonedHeaderHash,
      "expected abandoned header hash",
    ),
    abandonedSubmittedTxHash: requireCardanoHash(
      expected.abandonedSubmittedTxHash,
      "expected abandoned submitted tx hash",
    ),
    baseHeaderHash: requireL2HeaderHash(
      expected.baseHeaderHash,
      "expected base header hash",
    ),
  } as const;
  for (const [key, expectedValue] of Object.entries(exact)) {
    if (
      attestation[key as keyof Phase4T1RecoveryAttestation] !== expectedValue
    ) {
      throw new Error(
        `T1 recovery attestation ${key} mismatch: expected=${JSON.stringify(expectedValue) ?? "undefined"},actual=${JSON.stringify(attestation[key as keyof Phase4T1RecoveryAttestation]) ?? "undefined"}`,
      );
    }
  }
  return attestation;
};

export const writePhase4T1Evidence = async (
  path: string,
  evidence: Phase4T1ProbeEvidence | Phase4T1AdvanceEvidence,
): Promise<void> => {
  if (!path.startsWith("/")) {
    throw new Error("Phase 4 T1 evidence path must be absolute");
  }
  const decoded =
    evidence.schemaVersion === PHASE4_T1_PROBE_SCHEMA
      ? decodePhase4T1ProbeEvidence(evidence)
      : decodePhase4T1AdvanceEvidence(evidence);
  await writeTextFileAtomicNoReplace(
    path,
    `${JSON.stringify(decoded, null, 2)}\n`,
    { mode: 0o600 },
  );
};

import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";

import { Effect } from "effect";
import { positiveSafeInteger } from "midgard-node/artifact-schema";
import { Database } from "midgard-node/services/database";

import {
  captureJournalKillDatabaseState,
  type JournalKillDatabaseState,
} from "../e2e/journal-kill-process-harness.js";
import {
  isolatedChildEnv,
  parseResetAttestation,
  type ProcessResult,
} from "./e2e-journal-kill-recovery-acceptance.decode-phase4-reset-attestation.js";
import { validatePhase4PhasRegistrationProof } from "./e2e-journal-kill-recovery-acceptance.validate-phase4-phas-registration-proof.js";
import {
  type Phase4ProcessIsolationIdentity,
  type Phase4ResetAttestation,
  RESET_ATTESTATION_SCHEMA,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-process-isolation-values.js";
import { decodePhase4GenesisLedgerReport } from "./phase4-genesis-ledger.js";

export const validatePhase4ResetAttestation = ({
  output,
  scenarioLabel,
  isolation,
}: {
  readonly output: string;
  readonly scenarioLabel: string;
  readonly isolation: Phase4ProcessIsolationIdentity;
}): Phase4ResetAttestation => {
  const attestation = parseResetAttestation(output);
  const expected = {
    schemaVersion: RESET_ATTESTATION_SCHEMA,
    scenarioLabel,
    composeProject: isolation.composeProject,
    networkMagic: isolation.networkMagic,
    postgresDatabase: isolation.postgresDatabase,
    deploymentManifestSha256: isolation.deploymentManifestSha256,
  } as const;
  for (const [key, value] of Object.entries(expected)) {
    if (attestation[key as keyof typeof attestation] !== value) {
      throw new Error(
        `Phase 4 reset attestation ${key} mismatch: expected=${JSON.stringify(value) ?? "undefined"},actual=${JSON.stringify(attestation[key as keyof typeof attestation]) ?? "undefined"}`,
      );
    }
  }
  if (!/^[a-f0-9]{64}$/u.test(attestation.snapshotSetSha256)) {
    throw new Error("Phase 4 reset attestation has invalid snapshotSetSha256");
  }
  if (!/^[a-f0-9]{64}$/u.test(attestation.snapshotIdentitySha256)) {
    throw new Error(
      "Phase 4 reset attestation has invalid snapshotIdentitySha256",
    );
  }
  if (!/^[a-f0-9]{64}$/u.test(attestation.phasRegistrationProofSha256)) {
    throw new Error(
      "Phase 4 reset attestation has invalid phasRegistrationProofSha256",
    );
  }
  const phasRegistration = validatePhase4PhasRegistrationProof(
    attestation.phasRegistration,
    "Phase 4 reset PHAS registration proof",
  );
  if (
    attestation.phasRegistrationProofSha256 !==
      isolation.snapshotPhasRegistrationProofSha256 ||
    JSON.stringify(phasRegistration) !==
      JSON.stringify(isolation.snapshotPhasRegistration)
  ) {
    throw new Error(
      "Phase 4 reset PHAS registration proof does not match the frozen snapshot proof",
    );
  }
  if (
    !Number.isSafeInteger(attestation.cardanoTip?.slot) ||
    attestation.cardanoTip.slot < 0 ||
    !/^[a-f0-9]{64}$/u.test(attestation.cardanoTip.hash)
  ) {
    throw new Error("Phase 4 reset attestation has invalid Cardano tip");
  }
  if (
    phasRegistration.networkMagic !== isolation.networkMagic ||
    phasRegistration.observedAtTip.slot !== attestation.cardanoTip.slot ||
    phasRegistration.observedAtTip.hash !== attestation.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 reset PHAS registration proof is not bound to the frozen Cardano tip and network",
    );
  }
  if (
    !Number.isSafeInteger(attestation.kupoCheckpoint) ||
    attestation.kupoCheckpoint !== attestation.cardanoTip.slot
  ) {
    throw new Error(
      "Phase 4 reset attestation Kupo checkpoint must equal the Cardano tip slot",
    );
  }
  if (attestation.snapshotIdentitySha256 !== isolation.snapshotIdentitySha256) {
    throw new Error(
      `Phase 4 reset attestation snapshot identity mismatch: expected=${isolation.snapshotIdentitySha256},actual=${attestation.snapshotIdentitySha256}`,
    );
  }
  if (
    attestation.cardanoTip.slot !== isolation.snapshotCardanoTip.slot ||
    attestation.cardanoTip.hash !== isolation.snapshotCardanoTip.hash
  ) {
    throw new Error(
      `Phase 4 reset attestation frozen Cardano tip mismatch: expected=${isolation.snapshotCardanoTip.slot.toString()}:${isolation.snapshotCardanoTip.hash},actual=${attestation.cardanoTip.slot.toString()}:${attestation.cardanoTip.hash}`,
    );
  }
  if (attestation.kupoCheckpoint !== isolation.snapshotKupoCheckpoint) {
    throw new Error(
      `Phase 4 reset attestation frozen Kupo checkpoint mismatch: expected=${isolation.snapshotKupoCheckpoint.toString()},actual=${attestation.kupoCheckpoint.toString()}`,
    );
  }
  return attestation;
};

export const positiveIntegerEnv = (name: string, fallback: number): number => {
  const raw = process.env[name];
  if (raw === undefined || raw.trim().length === 0) return fallback;
  return positiveSafeInteger(Number(raw), name);
};

export const processAcceptanceTimeoutMs = (): number =>
  positiveIntegerEnv("MIDGARD_PHASE4_PROCESS_TIMEOUT_MS", 600_000);

const runProcess = ({
  command,
  args,
  cwd,
  env = isolatedChildEnv(),
  timeoutMs = processAcceptanceTimeoutMs(),
}: {
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly env?: NodeJS.ProcessEnv;
  readonly timeoutMs?: number;
}): Promise<ProcessResult> =>
  new Promise((resolvePromise, reject) => {
    const child = spawn(command, [...args], {
      cwd,
      env,
      shell: false,
      stdio: ["ignore", "pipe", "pipe"],
      detached: process.platform !== "win32",
    });
    let output = "";
    let timedOut = false;
    let forceKillTimer: NodeJS.Timeout | undefined;
    const terminate = (signal: NodeJS.Signals): void => {
      if (child.pid === undefined) return;
      try {
        if (process.platform === "win32") child.kill(signal);
        else process.kill(-child.pid, signal);
      } catch (error) {
        if (
          typeof error !== "object" ||
          error === null ||
          !("code" in error) ||
          error.code !== "ESRCH"
        ) {
          throw error;
        }
      }
    };
    const timeout = setTimeout(() => {
      timedOut = true;
      terminate("SIGTERM");
      forceKillTimer = setTimeout(() => terminate("SIGKILL"), 2_000);
    }, timeoutMs);
    child.stdout.on("data", (chunk: Buffer) => {
      output += chunk.toString("utf8");
    });
    child.stderr.on("data", (chunk: Buffer) => {
      output += chunk.toString("utf8");
    });
    child.on("error", (error) => {
      clearTimeout(timeout);
      if (forceKillTimer !== undefined) clearTimeout(forceKillTimer);
      reject(error);
    });
    child.on("close", (exitCode, signal) => {
      clearTimeout(timeout);
      if (forceKillTimer !== undefined) clearTimeout(forceKillTimer);
      resolvePromise({ exitCode, signal, output, timedOut });
    });
  });

export const runRequiredProcess = async (
  input: Parameters<typeof runProcess>[0],
  label: string,
): Promise<string> => {
  const result = await runProcess(input);
  if (result.timedOut || result.exitCode !== 0 || result.signal !== null) {
    throw new Error(
      `${label} failed (timed_out=${String(result.timedOut)},exit=${result.exitCode?.toString() ?? "null"},signal=${result.signal ?? "none"}):\n${result.output}`,
    );
  }
  return result.output;
};

export const assertPositivePreflightOutput = (
  label: string,
  output: string,
): void => {
  const negativeBoolean =
    /"(?:satisfied|ready|healthy|complete|configured)"\s*:\s*false/i;
  const negativeStatus =
    /"(?:status|verdict)"\s*:\s*"(?:failed|failure|missing|unsatisfied|blocked|unhealthy|not_ready)"/i;
  if (negativeBoolean.test(output) || negativeStatus.test(output)) {
    throw new Error(`${label} reported an unsatisfied preflight:\n${output}`);
  }
};

export const assertExactGenesisPreflightOutput = (
  label: string,
  output: string,
): void => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(output.trim()) as unknown;
  } catch (cause) {
    throw new Error(`${label} must emit exactly one JSON V1 report`, { cause });
  }
  const report = decodePhase4GenesisLedgerReport(parsed);
  if (report.mode !== "verify" || report.status !== "already_present") {
    throw new Error(`${label} did not verify the exact existing genesis state`);
  }
};

const waitFor = async ({
  label,
  timeoutMs,
  probe,
}: {
  readonly label: string;
  readonly timeoutMs: number;
  readonly probe: () => Promise<boolean>;
}): Promise<void> => {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (await probe()) return;
    await new Promise((resolvePromise) => setTimeout(resolvePromise, 250));
  }
  throw new Error(
    `${label} did not become true within ${timeoutMs.toString()}ms`,
  );
};

export const waitForReady = (port: number, timeoutMs: number): Promise<void> =>
  waitFor({
    label: `node ready on port ${port.toString()}`,
    timeoutMs,
    probe: async () => {
      const response = await fetch(
        `http://127.0.0.1:${port.toString()}/readyz`,
      ).catch(() => null);
      return response?.ok === true;
    },
  });

export const waitForNewPayloadRetention = (
  existingTxIds: ReadonlySet<string>,
  timeoutMs: number,
): Promise<void> =>
  waitFor({
    label: "second L2 payload retained in mempool or processed_mempool",
    timeoutMs,
    probe: async () => {
      const state = await captureDatabaseState();
      return [...state.mempool, ...state.processed].some(
        (entry) => !existingTxIds.has(entry.txId),
      );
    },
  });

export const waitForLogMarker = (
  path: string,
  marker: string,
  timeoutMs: number,
): Promise<void> =>
  waitFor({
    label: `log marker ${marker}`,
    timeoutMs,
    probe: async () =>
      readFile(path, "utf8").then(
        (text) => text.includes(marker),
        () => false,
      ),
  });

export const captureDatabaseState = (): Promise<JournalKillDatabaseState> =>
  Effect.runPromise(
    captureJournalKillDatabaseState.pipe(Effect.provide(Database.layer)),
  );

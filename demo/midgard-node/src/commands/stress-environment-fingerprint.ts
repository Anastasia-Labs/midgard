import { createHash } from "node:crypto";
import { cpus, hostname, totalmem } from "node:os";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

export type EnvironmentFingerprint = {
  readonly schemaVersion: 1;
  readonly gitSha: string | null;
  readonly imageDigests: {
    readonly midgardNode: string | null;
    readonly postgres: string | null;
  };
  readonly hostCpu: {
    readonly model: string | null;
    readonly count: number;
    readonly speedMhz: number | null;
  };
  readonly hostRamBytes: number;
  readonly hostname: string;
  readonly loadGenCoHosted: boolean | null;
  readonly loadGeneratorPlacement:
    | "separate-host"
    | "separate-container"
    | "node-container"
    | "unknown";
  readonly clockOffsetMs: number | null;
  readonly calibrationProofRef: string | null;
  readonly configProfileHash: string;
  readonly fixedKnobs: {
    readonly nodePostgresPoolMaxConnections: number;
    readonly validationBatchHardCap: number;
    readonly validationMinBatch: number;
    readonly validationPhaseAMaxEffectiveConcurrency: number;
  };
  readonly capturedAt: string;
  readonly notes: readonly string[];
};

const stableJson = (value: unknown): string => {
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  if (typeof value === "object" && value !== null) {
    return `{${Object.entries(value as Record<string, unknown>)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([key, entry]) => `${JSON.stringify(key)}:${stableJson(entry)}`)
      .join(",")}}`;
  }
  return JSON.stringify(value);
};

const sha256Hex = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

const readGitShaFromDotGit = async (cwd: string): Promise<string | null> => {
  const head = (await readFile(join(cwd, ".git", "HEAD"), "utf8")).trim();
  if (/^[0-9a-f]{40}$/iu.test(head)) {
    return head;
  }
  const match = /^ref:\s+(.+)$/u.exec(head);
  if (match === null) {
    return null;
  }
  return (await readFile(join(cwd, ".git", match[1]!), "utf8")).trim();
};

export const collectEnvironmentFingerprint = async ({
  cwd = process.cwd(),
  env = process.env,
  configProfile = {},
  calibrationProofRef = null,
}: {
  readonly cwd?: string;
  readonly env?: NodeJS.ProcessEnv;
  readonly configProfile?: Readonly<Record<string, unknown>>;
  readonly calibrationProofRef?: string | null;
} = {}): Promise<EnvironmentFingerprint> => {
  const notes: string[] = [];
  let gitSha = env.MIDGARD_BUILD_GIT_SHA ?? null;
  if (gitSha === null || gitSha.trim().length === 0) {
    try {
      gitSha = await readGitShaFromDotGit(cwd);
    } catch {
      gitSha = null;
      notes.push("git_sha_unavailable");
    }
  }
  const cpu = cpus();
  const rawPlacement = env.STRESS_LOAD_GENERATOR_PLACEMENT ?? "unknown";
  const loadGeneratorPlacement =
    rawPlacement === "separate-host" ||
    rawPlacement === "separate-container" ||
    rawPlacement === "node-container"
      ? rawPlacement
      : "unknown";
  const loadGenCoHosted =
    env.STRESS_LOADGEN_COHOSTED === "true"
      ? true
      : env.STRESS_LOADGEN_COHOSTED === "false"
        ? false
        : null;
  if (loadGenCoHosted === null) {
    notes.push("loadgen_cohosted_unset");
  }
  const clockOffsetMs =
    env.STRESS_CLOCK_OFFSET_MS === undefined
      ? null
      : Number(env.STRESS_CLOCK_OFFSET_MS);
  if (clockOffsetMs === null || !Number.isFinite(clockOffsetMs)) {
    notes.push("clock_sync_unverified");
  }
  const imageDigests = {
    midgardNode: env.MIDGARD_NODE_IMAGE_DIGEST ?? null,
    postgres: env.MIDGARD_POSTGRES_IMAGE_DIGEST ?? null,
  };
  if (imageDigests.midgardNode === null || imageDigests.postgres === null) {
    notes.push("image_digest_unavailable");
  }
  return {
    schemaVersion: 1,
    gitSha,
    imageDigests,
    hostCpu: {
      model: cpu[0]?.model ?? null,
      count: cpu.length,
      speedMhz: cpu[0]?.speed ?? null,
    },
    hostRamBytes: totalmem(),
    hostname: hostname(),
    loadGenCoHosted,
    loadGeneratorPlacement,
    clockOffsetMs: Number.isFinite(clockOffsetMs) ? clockOffsetMs : null,
    calibrationProofRef,
    configProfileHash: sha256Hex(stableJson(configProfile)),
    fixedKnobs: {
      nodePostgresPoolMaxConnections: 20,
      validationBatchHardCap: 1600,
      validationMinBatch: 128,
      validationPhaseAMaxEffectiveConcurrency: 8,
    },
    capturedAt: new Date().toISOString(),
    notes,
  };
};

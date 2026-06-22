import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";

export const E2E_STEP_SCHEMA_VERSION = "midgard-e2e-step-v1";

export type StepStatus =
  | "success"
  | "failed"
  | "signaled"
  | "timeout"
  | "runner_error";

export type RedactedCommand = {
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly envKeys: readonly string[];
};

export type StepSpec = {
  readonly id: string;
  readonly command: string;
  readonly args?: readonly string[];
  readonly cwd: string;
  readonly env?: Readonly<Record<string, string | undefined>>;
  readonly timeoutMs?: number;
  readonly rawLogPath: string;
};

export type StepSummary = {
  readonly schemaVersion: typeof E2E_STEP_SCHEMA_VERSION;
  readonly id: string;
  readonly status: StepStatus;
  readonly command: RedactedCommand;
  readonly pid: number | null;
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly durationMs: number;
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly timedOut: boolean;
  readonly rawLogPath: string;
  readonly observedTxHashes: readonly string[];
  readonly parsedJson: unknown | null;
  readonly error: string | null;
};

const SECRET_KEY_PATTERN =
  /(seed|secret|private|password|passphrase|api[_-]?key|blockfrost|admin[_-]?key|token)/i;
const TX_HASH_PATTERN = /\b[0-9a-f]{64}\b/gi;

export const redactArg = (arg: string): string =>
  SECRET_KEY_PATTERN.test(arg) ? "<redacted>" : arg;

export const redactEnvKeys = (
  env: Readonly<Record<string, string | undefined>> = {},
): readonly string[] =>
  Object.keys(env)
    .sort((left, right) => left.localeCompare(right))
    .map((key) => (SECRET_KEY_PATTERN.test(key) ? `${key}=<redacted>` : key));

const redactedCommand = (spec: StepSpec): RedactedCommand => ({
  command: spec.command,
  args: (spec.args ?? []).map(redactArg),
  cwd: spec.cwd,
  envKeys: redactEnvKeys(spec.env),
});

const uniqueTxHashes = (text: string): readonly string[] =>
  Array.from(
    new Set(
      (text.match(TX_HASH_PATTERN) ?? []).map((hash) => hash.toLowerCase()),
    ),
  );

const parseLastJsonLine = (text: string): unknown | null => {
  const lines = text
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.startsWith("{") && line.endsWith("}"));
  for (let index = lines.length - 1; index >= 0; index -= 1) {
    try {
      return JSON.parse(lines[index]!);
    } catch {
      continue;
    }
  }
  return null;
};

export const runCommandStep = async (spec: StepSpec): Promise<StepSummary> => {
  const startedAtDate = new Date();
  const startedAt = startedAtDate.toISOString();
  const args = [...(spec.args ?? [])];
  await mkdir(dirname(spec.rawLogPath), { recursive: true });
  const log = createWriteStream(spec.rawLogPath, { flags: "a" });
  let pid: number | null = null;
  let stdout = "";
  let combined = "";
  let timedOut = false;

  const finishSummary = ({
    status,
    exitCode,
    signal,
    error,
  }: {
    readonly status: StepStatus;
    readonly exitCode: number | null;
    readonly signal: NodeJS.Signals | null;
    readonly error: string | null;
  }): StepSummary => {
    const finishedAtDate = new Date();
    return {
      schemaVersion: E2E_STEP_SCHEMA_VERSION,
      id: spec.id,
      status,
      command: redactedCommand(spec),
      pid,
      startedAt,
      finishedAt: finishedAtDate.toISOString(),
      durationMs: Math.max(
        0,
        finishedAtDate.getTime() - startedAtDate.getTime(),
      ),
      exitCode,
      signal,
      timedOut,
      rawLogPath: spec.rawLogPath,
      observedTxHashes: uniqueTxHashes(combined),
      parsedJson: parseLastJsonLine(stdout),
      error,
    };
  };

  return await new Promise<StepSummary>((resolve) => {
    let settled = false;
    const settle = (summary: StepSummary): void => {
      if (settled) {
        return;
      }
      settled = true;
      log.end(() => resolve(summary));
    };

    const child = spawn(spec.command, args, {
      cwd: spec.cwd,
      env: {
        ...process.env,
        ...spec.env,
      },
      shell: false,
      detached: false,
      stdio: ["ignore", "pipe", "pipe"],
    });
    pid = child.pid ?? null;
    log.write(
      JSON.stringify({
        event: "started",
        id: spec.id,
        pid,
        at: startedAt,
        command: redactedCommand(spec),
      }) + "\n",
    );

    const timeout =
      spec.timeoutMs === undefined
        ? undefined
        : setTimeout(
            () => {
              timedOut = true;
              child.kill("SIGTERM");
            },
            Math.max(1, spec.timeoutMs),
          );

    child.stdout.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      stdout += text;
      combined += text;
      log.write(text);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      combined += text;
      log.write(text);
    });
    child.on("error", (error) => {
      if (timeout !== undefined) {
        clearTimeout(timeout);
      }
      settle(
        finishSummary({
          status: "runner_error",
          exitCode: null,
          signal: null,
          error: error.message,
        }),
      );
    });
    child.on("close", (exitCode, signal) => {
      if (timeout !== undefined) {
        clearTimeout(timeout);
      }
      const status: StepStatus = timedOut
        ? "timeout"
        : signal !== null
          ? "signaled"
          : exitCode === 0
            ? "success"
            : "failed";
      settle(
        finishSummary({
          status,
          exitCode,
          signal,
          error:
            status === "success"
              ? null
              : timedOut
                ? `Step timed out after ${spec.timeoutMs?.toString()}ms.`
                : `Step exited with status ${exitCode?.toString() ?? signal ?? "unknown"}.`,
        }),
      );
    });
  });
};

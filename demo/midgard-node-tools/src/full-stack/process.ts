import { randomUUID } from "node:crypto";
import { mkdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { runCommandStep } from "../e2e/runner.js";
import type { StackConfig } from "./config.js";
import { writeDurableJson } from "./journal.js";

export class StackProcesses {
  constructor(
    readonly config: StackConfig,
    readonly env: Record<string, string>,
  ) {}
  async command(
    id: string,
    command: string,
    args: readonly string[],
    overrides: Record<string, string> = {},
    cwd = this.config.nodeRoot,
  ) {
    const attempt = `${id}-${randomUUID()}`;
    const directory = join(this.config.runDirectory, "attempts");
    await mkdir(directory, { recursive: true, mode: 0o700 });
    await mkdir(join(this.config.nodeRoot, "logs"), {
      recursive: true,
      mode: 0o700,
    });
    const summary = await runCommandStep({
      id,
      command: "flock",
      args: [
        "--nonblock",
        join(this.config.nodeRoot, "logs/full-stack-command.lock"),
        command,
        ...args,
      ],
      cwd,
      envInheritance: "none",
      env: {
        PATH: process.env.PATH,
        HOME: process.env.HOME,
        MIDGARD_AIKEN_BIN: process.env.MIDGARD_AIKEN_BIN,
        ...this.env,
        MIDGARD_DOTENV_MODE: "disabled",
        ...overrides,
      },
      rawLogPath: join(directory, `${attempt}.log`),
      timeoutMs: this.config.timeoutMs,
    });
    await writeDurableJson(join(directory, `${attempt}.json`), summary);
    if (summary.status !== "success")
      throw new Error(`${id} failed; inspect ${summary.rawLogPath}`);
    return summary.parsedJson;
  }
  node(
    id: string,
    args: readonly string[],
    overrides: Record<string, string> = {},
  ) {
    return this.command(id, process.execPath, ["dist/index.js", ...args], {
      POSTGRES_HOST: "127.0.0.1",
      POSTGRES_PORT: this.env.MIDGARD_POSTGRES_HOST_PORT ?? "5433",
      ...overrides,
    });
  }
  compose(id: string, args: readonly string[], extraFile?: string) {
    return this.command(id, "bash", [
      "scripts/operator-compose.sh",
      "--env-file",
      this.config.envFile,
      "-f",
      "docker-compose.yaml",
      "-f",
      "docker-compose.kupmios.yaml",
      ...(extraFile ? ["-f", extraFile] : []),
      ...args,
    ]);
  }
}

export async function poll<T>(
  label: string,
  timeoutMs: number,
  read: () => Promise<T | undefined>,
): Promise<T> {
  const deadline = Date.now() + timeoutMs;
  do {
    const value = await read();
    if (value !== undefined) return value;
    await new Promise((resolve) => setTimeout(resolve, 5_000));
  } while (Date.now() < deadline);
  throw new Error(
    `${label} did not complete within ${timeoutMs}ms; saved state was preserved`,
  );
}

export async function getJson(
  url: string,
  headers?: Record<string, string>,
): Promise<unknown | undefined> {
  try {
    const response = await fetch(url, {
      headers,
      signal: AbortSignal.timeout(10_000),
    });
    if (!response.ok) return undefined;
    return await response.json();
  } catch {
    return undefined;
  }
}
export async function bearerHeaders(path: string) {
  const bearer = (await readFile(path, "utf8")).trim();
  if (!bearer || /\s/.test(bearer))
    throw new Error("Invalid watcher bearer file");
  return { authorization: `Bearer ${bearer}` };
}

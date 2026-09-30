import { randomUUID } from "node:crypto";
import { mkdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { runCommandStep } from "../e2e/runner.js";
import type { StackConfig } from "./config.js";
import { writeDurableJson } from "./journal.js";

/** Build tools see only the host toolchain; stack commands also see the stack environment. */
export type CommandScope = "stack" | "host";
/** The per-command lock's conflict code, distinct from any command's own failure. */
export const COMMAND_LOCK_CONFLICT_EXIT_CODE = 75;
/** The command never ran: another stack command held the per-command lock. */
export class CommandNotStartedError extends Error {
  override readonly name = "CommandNotStartedError";
}

const HOST_KEYS = [
  "PATH",
  "HOME",
  "XDG_RUNTIME_DIR",
  "MIDGARD_AIKEN_BIN",
  "GOPATH",
  "GOMODCACHE",
  "CARGO_HOME",
  "RUSTUP_HOME",
];

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
    scope: CommandScope = "stack",
  ) {
    const attempt = `${id}-${randomUUID()}`;
    const directory = join(this.config.runDirectory, "attempts");
    await mkdir(directory, { recursive: true, mode: 0o700 });
    await mkdir(join(this.config.nodeRoot, "logs"), {
      recursive: true,
      mode: 0o700,
    });
    const lock = join(this.config.nodeRoot, "logs/full-stack-command.lock");
    // The node .env may not set these (loadStackConfig refuses PATH, HOME and DOCKER_*).
    const host = Object.fromEntries(
      Object.entries(process.env).filter(
        ([key, value]) =>
          value !== undefined &&
          (HOST_KEYS.includes(key) || key.startsWith("DOCKER_")),
      ) as [string, string][],
    );
    const summary = await runCommandStep({
      id,
      command: "flock",
      args: [
        "--nonblock",
        "--conflict-exit-code",
        String(COMMAND_LOCK_CONFLICT_EXIT_CODE),
        lock,
        command,
        ...args,
      ],
      cwd,
      envInheritance: "none",
      env: {
        ...(scope === "stack" ? this.env : {}),
        ...host,
        MIDGARD_DOTENV_MODE: "disabled",
        ...overrides,
      },
      rawLogPath: join(directory, `${attempt}.log`),
      timeoutMs: this.config.timeoutMs,
    });
    await writeDurableJson(join(directory, `${attempt}.json`), summary);
    if (summary.exitCode === COMMAND_LOCK_CONFLICT_EXIT_CODE)
      throw new CommandNotStartedError(
        `${id} did not start: another stack command holds ${lock}`,
      );
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
      // loadStackConfig requires an explicit port that is not the shared test database.
      POSTGRES_PORT: this.env.MIDGARD_POSTGRES_HOST_PORT!,
      // The deployment run state lives in the run directory, not the node's default.
      MIDGARD_RUN_STATE_PATH: join(
        this.config.runDirectory,
        "deployment-run-state.json",
      ),
      ...overrides,
    });
  }
  /** The cluster that host commands reach at 127.0.0.1 on the stack's Postgres host port. */
  async hostDatabaseIdentity(): Promise<string> {
    const [{ SqlClient }, { PgClient }, { Effect, Redacted }] =
      await Promise.all([
        import("@effect/sql"),
        import("@effect/sql-pg"),
        import("effect"),
      ]);
    const query = Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql.unsafe<{ id: string }>(
        "SELECT system_identifier::text AS id FROM pg_control_system()",
      );
      return rows[0]!.id;
    });
    return Effect.runPromise(
      query.pipe(
        Effect.provide(
          PgClient.layer({
            host: "127.0.0.1",
            port: Number(this.env.MIDGARD_POSTGRES_HOST_PORT),
            username: this.env.POSTGRES_USER,
            password: Redacted.make(this.env.POSTGRES_PASSWORD ?? ""),
            database: this.env.POSTGRES_DB,
            maxConnections: 1,
          }),
        ),
      ),
    );
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

import { randomUUID } from "node:crypto";
import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";

import { runCommandStep } from "../e2e/runner.js";
import { l1KupmiosEnvironment } from "../environment.js";
import { deriveL1Origin } from "../l1-origin.js";
import type { StackConfig } from "./config.js";
import { writeDurableJson } from "./journal.js";
import { hostL1Node } from "./native-ledger.js";
import { NODE_TLS_CA_ENV, NODE_TLS_SERVERNAME_ENV } from "./node-tls.js";

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
  "GOPATH",
  "GOMODCACHE",
  "CARGO_HOME",
  "RUSTUP_HOME",
];

/** The deployment manifest the stack's node, tools and users read. */
export const stackManifestPath = (config: StackConfig) =>
  join(config.nodeRoot, "deploymentInfo/contract-deployment-info.json");

/** The role images every Compose command of a preprod-public stack runs. */
export const publicRolesPath = (config: StackConfig) =>
  join(config.runDirectory, "public-roles.yaml");

export class StackProcesses {
  /** The operator's own `L1_ORIGIN` from the stack env file, before any restore. */
  readonly configuredL1Origin: string | undefined;
  constructor(
    readonly config: StackConfig,
    readonly env: Record<string, string>,
  ) {
    this.configuredL1Origin = env.L1_ORIGIN || undefined;
  }
  /**
   * Runs one command to completion and returns the JSON it printed last.
   * `locked` commands (every node, build and Compose command) take the
   * per-command lock, so two harness processes never interleave them; the
   * user's own `midgard-user` keeps its own record and runs without it.
   */
  async command(
    id: string,
    command: string,
    args: readonly string[],
    overrides: Record<string, string> = {},
    cwd = this.config.nodeRoot,
    scope: CommandScope = "stack",
    locked = true,
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
      command: locked ? "flock" : command,
      args: locked
        ? [
            "--nonblock",
            "--conflict-exit-code",
            String(COMMAND_LOCK_CONFLICT_EXIT_CODE),
            lock,
            command,
            ...args,
          ]
        : args,
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
    if (locked && summary.exitCode === COMMAND_LOCK_CONFLICT_EXIT_CODE)
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
      ...this.publicNodeTls(),
      ...overrides,
    });
  }
  /**
   * The user's own `midgard-user`, as a user runs it: the host toolchain,
   * the deployment manifest, its own records in the run directory and the
   * stack's Kupo and Ogmios at the host ports Compose publishes them on
   * (Kupo matches every address and never prunes spent outputs). It never
   * sees the node's environment or database; `overrides` carries the seed.
   */
  user(id: string, args: readonly string[], overrides: Record<string, string>) {
    const l1 = l1KupmiosEnvironment(this.env);
    return this.command(
      id,
      process.execPath,
      ["dist/cli.js", ...args, "--json"],
      {
        MIDGARD_DEPLOYMENT_MANIFEST: stackManifestPath(this.config),
        L1_ACCESS: "kupmios",
        L1_KUPO_URL: l1.kupoUrl,
        L1_OGMIOS_URL: l1.ogmiosUrl,
        MIDGARD_USER_RECORD_DIR: join(
          this.config.runDirectory,
          "user-submissions",
        ),
        ...this.publicNodeTls(),
        ...overrides,
      },
      join(this.config.nodeRoot, "../midgard-user-cli"),
      "host",
      false,
    );
  }
  /** Host commands reach a preprod-public node over HTTPS on loopback. */
  private publicNodeTls(): Record<string, string> {
    return this.config.public === undefined
      ? {}
      : {
          NODE_OPTIONS: `--import=${new URL("./node-tls-preload.js", import.meta.url).href}`,
          [NODE_TLS_CA_ENV]: this.config.public.tlsCaFile,
          [NODE_TLS_SERVERNAME_ENV]: new URL(
            this.env.MIDGARD_PUBLIC_API_ORIGIN!,
          ).hostname,
        };
  }
  /** The recorded origin T, proven to precede the hub-oracle nonce tx's block. */
  deriveL1Origin(nonceTxHash: string, recorded: L1Origin) {
    return deriveL1Origin({ node: hostL1Node(this), nonceTxHash, recorded });
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
  compose(id: string, args: readonly string[], ...extraFiles: string[]) {
    return this.command(id, "bash", [
      "scripts/operator-compose.sh",
      "--env-file",
      this.config.envFile,
      "-f",
      "docker-compose.yaml",
      "-f",
      "docker-compose.kupmios.yaml",
      // A preprod-public stack runs the release's digest-pinned role images.
      ...(this.config.public === undefined
        ? []
        : ["-f", publicRolesPath(this.config)]),
      ...extraFiles.flatMap((file) => ["-f", file]),
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

/**
 * A /readyz body, ready (200) or not (503): an unready service still says
 * why. Anything else, or no answer, is undefined.
 */
export async function getReadiness(url: string): Promise<unknown | undefined> {
  try {
    const response = await fetch(url, { signal: AbortSignal.timeout(10_000) });
    if (response.status !== 200 && response.status !== 503) return undefined;
    return await response.json();
  } catch {
    return undefined;
  }
}

export async function getJson(url: string): Promise<unknown | undefined> {
  try {
    const response = await fetch(url, { signal: AbortSignal.timeout(10_000) });
    if (!response.ok) return undefined;
    return await response.json();
  } catch {
    return undefined;
  }
}

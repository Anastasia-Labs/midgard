import { execFileSync, spawn } from "node:child_process";
import { createHash, randomUUID } from "node:crypto";
import {
  createWriteStream,
  readdirSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { createServer } from "node:net";
import { join } from "node:path";
import { createInterface } from "node:readline";
import { setTimeout as pause } from "node:timers/promises";
import { fileURLToPath } from "node:url";

import type { JourneyHistoryTransportEnvironment } from "./history-archive-transport.js";

/** Recognize actual startup diagnostics without interpreting them as readiness. */
export const journeyStartupProgress = (line: string) => {
  let event: Record<string, unknown> | null;
  try {
    event = JSON.parse(line);
  } catch {
    return undefined;
  }
  if (
    event === null ||
    typeof event !== "object" ||
    event.packageName !== "midgard-watcher" ||
    event.command !== "start" ||
    event.state !== "starting" ||
    event.productionReady !== false ||
    typeof event.stage !== "string" ||
    typeof event.outcome !== "string" ||
    !["started", "pending", "completed", "failed"].includes(event.outcome) ||
    typeof event.elapsedMs !== "number" ||
    !Number.isFinite(event.elapsedMs) ||
    event.elapsedMs < 0 ||
    typeof event.observedAt !== "string"
  )
    return undefined;
  return {
    stage: event.stage,
    outcome: event.outcome,
    elapsedMs: event.elapsedMs,
    observedAt: event.observedAt,
    productionReady: false as const,
    ...(typeof event.error === "string" ? { error: event.error } : {}),
  };
};

export const journeyPorts = async (count: number) => {
  const servers = await Promise.all(
    Array.from({ length: count }, async () => {
      const server = createServer();
      await new Promise<void>((resolve, reject) => {
        server.once("error", reject);
        server.listen(0, "127.0.0.1", resolve);
      });
      return server;
    }),
  );
  const ports = servers.map((server) => {
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("No journey process port");
    return address.port;
  });
  await Promise.all(
    servers.map(
      (server) =>
        new Promise<void>((resolve, reject) =>
          server.close((error) => (error ? reject(error) : resolve())),
        ),
    ),
  );
  return ports;
};

export const launchJourneyWatcherProcess = (input: {
  command: "authority" | "start";
  configPath: string;
  directory: string;
  caPath: string;
  transportEnvironment?: JourneyHistoryTransportEnvironment;
}) => {
  const startedAt = new Date().toISOString();
  const attempt = `${input.command}-${startedAt.replaceAll(":", "-")}-${randomUUID()}`;
  const cliPath = fileURLToPath(
    new URL("../../../midgard-watcher/dist/cli.js", import.meta.url),
  );
  const digest = (path: string) =>
    createHash("sha256").update(readFileSync(path)).digest("hex");
  const buildDigests = Object.fromEntries(
    ["midgard-watcher", "midgard-fault-proofs"].flatMap((packageName) => {
      const directory = fileURLToPath(
        new URL(`../../../${packageName}/dist/`, import.meta.url),
      );
      return readdirSync(directory)
        .filter((name) => name.endsWith(".js"))
        .sort()
        .map((name) => [
          `${packageName}/${name}`,
          digest(join(directory, name)),
        ]);
    }),
  );
  writeFileSync(
    join(input.directory, `${attempt}.json`),
    JSON.stringify(
      {
        attempt,
        startedAt,
        command: input.command,
        nodeVersion: process.version,
        sourceRevision: execFileSync("git", ["rev-parse", "HEAD"], {
          encoding: "utf8",
        }).trim(),
        sourceDiffSha256: createHash("sha256")
          .update(
            execFileSync("git", ["diff", "HEAD", "--"], {
              maxBuffer: 32 * 1024 * 1024,
            }),
          )
          .digest("hex"),
        configSha256: digest(input.configPath),
        buildDigests,
      },
      null,
      2,
    ),
    { mode: 0o600, flag: "wx" },
  );
  const log = createWriteStream(join(input.directory, `${input.command}.log`), {
    flags: "a",
  });
  log.write(`${JSON.stringify({ attempt, startedAt })}\n`);
  const child = spawn(
    process.execPath,
    [cliPath, input.command, "--config", input.configPath],
    {
      stdio: ["ignore", "pipe", "pipe"],
      env: {
        PATH: process.env.PATH,
        NODE_EXTRA_CA_CERTS: input.caPath,
        ...input.transportEnvironment,
        MALLOC_MMAP_THRESHOLD_: "131072",
      },
    },
  );
  child.stdout.pipe(log, { end: false });
  child.stderr.pipe(log, { end: false });
  let logBytes = 0;
  let lastOutputAt: string | undefined;
  let startup: ReturnType<typeof journeyStartupProgress>;
  const tail: string[] = [];
  for (const stream of [child.stdout, child.stderr]) {
    stream.on("data", (chunk: Buffer) => {
      logBytes += chunk.length;
      lastOutputAt = new Date().toISOString();
    });
    createInterface({ input: stream }).on("line", (line: string) => {
      tail.push(line.slice(-2000));
      if (tail.length > 8) tail.shift();
      const progress = journeyStartupProgress(line);
      if (progress === undefined) return;
      startup = progress;
      console.info(
        `Live watcher startup: ${progress.stage} ${progress.outcome} after ${(progress.elapsedMs / 1000).toFixed(1)}s`,
        progress.error ?? "",
      );
    });
  }
  let failure: Error | undefined;
  let closed = false;
  const done = new Promise<void>((resolve) => {
    child.once("error", (error) => {
      failure = error;
    });
    child.once("exit", (code, signal) => {
      if (!closed)
        failure = new Error(
          `${input.command} process exited: code=${code} signal=${signal}; see ${String(log.path)}`,
        );
    });
    child.once("close", () => log.end(resolve));
  });
  return {
    observe: () => ({
      attempt,
      pid: child.pid ?? null,
      startedAt,
      state:
        child.exitCode !== null || child.signalCode !== null
          ? "exited"
          : failure !== undefined
            ? "failed"
            : child.pid === undefined
              ? "spawning"
              : "running",
      exitCode: child.exitCode,
      signalCode: child.signalCode,
      logPath: String(log.path),
      logBytes,
      lastOutputAt: lastOutputAt ?? null,
      startup: startup ?? null,
      tail: [...tail],
      ...(failure === undefined ? {} : { error: failure.message }),
    }),
    assertHealthy: () => {
      if (failure !== undefined) throw failure;
    },
    close: async () => {
      closed = true;
      if (child.exitCode === null && child.signalCode === null)
        child.kill("SIGTERM");
      await Promise.race([done, pause(10_000)]);
      if (child.exitCode === null && child.signalCode === null)
        child.kill("SIGKILL");
      await done;
    },
  };
};

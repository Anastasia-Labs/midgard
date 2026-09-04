import { mkdir, writeFile } from "node:fs/promises";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import { join } from "node:path";

import {
  createTrackedTempDirFactory,
  waitForFile,
  writeScript,
} from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import {
  E2E_MANAGED_SERVICE_SCHEMA_VERSION,
  parseManagedServiceSummary,
} from "../src/commands/e2e-service.js";
import {
  classifyServiceError,
  E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION,
  inspectPidFile,
  parseServiceSupervisorSummary,
  probeHttpEndpoint,
  superviseHostProcess,
} from "../src/e2e/service-supervisor.js";

const makeTempDir = createTrackedTempDirFactory("midgard-e2e-supervisor-");

describe("e2e service error classification", () => {
  it("classifies transient provider errors as restartable", () => {
    expect(
      classifyServiceError({ text: "fetch failed: ECONNRESET" }),
    ).toMatchObject({
      class: "transient_provider",
      restartable: true,
    });
  });

  it("treats invalid mnemonic as fatal config", () => {
    expect(
      classifyServiceError({ text: "Error: Invalid mnemonic" }),
    ).toMatchObject({
      class: "fatal_config",
      restartable: false,
    });
  });

  it("treats insufficient funds as a fatal precondition", () => {
    expect(
      classifyServiceError({ text: "insufficient lovelace for DA submitter" }),
    ).toMatchObject({
      class: "fatal_protocol_or_precondition",
      restartable: false,
    });
  });

  it("allows recent submitted tx 404 as transient only when tracked", () => {
    const txHash = "aa".repeat(32);

    expect(
      classifyServiceError({
        text: `/txs/${txHash} returned 404 Not Found`,
        recentTxHashes: new Set([txHash]),
      }),
    ).toMatchObject({
      class: "transient_provider",
      restartable: true,
    });

    expect(
      classifyServiceError({
        text: `/txs/${txHash} returned 404 Not Found`,
      }),
    ).toMatchObject({
      class: "unknown",
      restartable: false,
    });
  });
});

describe("e2e host process supervisor", () => {
  it("restarts transient failures within the configured budget", async () => {
    const dir = await makeTempDir();
    const marker = join(dir, "attempt.txt");
    const script = await writeScript(
      dir,
      "flaky.mjs",
      [
        "import { existsSync, writeFileSync } from 'node:fs';",
        `const marker = ${JSON.stringify(marker)};`,
        "if (!existsSync(marker)) {",
        "  writeFileSync(marker, 'seen');",
        "  console.error('fetch failed');",
        "  process.exit(1);",
        "}",
        "console.log('ready');",
      ].join("\n"),
    );

    const summary = await superviseHostProcess({
      service: "da-node",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "da-node.log"),
      maxRestarts: 1,
      restartBackoffMs: 1,
      sleep: async () => {},
    });

    expect(summary.status).toBe("exited_success");
    expect(parseServiceSupervisorSummary(summary)).toEqual(summary);
    expect(summary.restartCount).toBe(1);
    expect(summary.attempts).toHaveLength(2);
    expect(summary.attempts[0]?.classification.class).toBe(
      "transient_provider",
    );
    const { service: _service, ...missingService } = summary;
    expect(() => parseServiceSupervisorSummary(missingService)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseServiceSupervisorSummary({ ...summary, unexpected: true }),
    ).toThrow("unknown field");
    expect(() =>
      parseServiceSupervisorSummary({
        ...summary,
        schemaVersion: "midgard-e2e-service-supervisor-v0",
      }),
    ).toThrow(E2E_SERVICE_SUPERVISOR_SCHEMA_VERSION);
    expect(() =>
      parseServiceSupervisorSummary({
        ...summary,
        restartCount: 0,
      }),
    ).toThrow("terminal verdict or attempt history is inconsistent");
    expect(() =>
      parseServiceSupervisorSummary({
        ...summary,
        status: "failed",
      }),
    ).toThrow("terminal verdict or attempt history is inconsistent");
  });

  it("does not restart fatal configuration failures", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "fatal.mjs",
      ["console.error('Invalid mnemonic');", "process.exit(1);"].join("\n"),
    );

    const summary = await superviseHostProcess({
      service: "da-node",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "da-node.log"),
      maxRestarts: 2,
      sleep: async () => {},
    });
    expect(summary.status).toBe("failed");
    expect(summary.restartCount).toBe(0);
    expect(summary.attempts).toHaveLength(1);
    expect(summary.terminalClassification.class).toBe("fatal_config");
  });

  it("passes dotenv env into supervised host processes with redacted provenance", async () => {
    const dir = await makeTempDir();
    await writeFile(join(dir, ".env"), "NETWORK=Preview\n", "utf8");
    const script = await writeScript(
      dir,
      "env-service.mjs",
      [
        "if (process.env.NETWORK !== 'Preprod') {",
        "  console.error('NETWORK=' + process.env.NETWORK);",
        "  process.exit(1);",
        "}",
        "console.log('ready');",
      ].join("\n"),
    );

    const summary = await superviseHostProcess({
      service: "env-service",
      command: process.execPath,
      args: [script],
      cwd: dir,
      envFiles: [".env"],
      env: { NETWORK: "Preprod", L1_PROVIDER_API_KEY: "secret" },
      envInheritance: "none",
      rawLogPath: join(dir, "logs", "env-service.log"),
      maxRestarts: 0,
    });

    expect(summary.status).toBe("exited_success");
    expect(summary.command.envInheritance).toBe("none");
    expect(summary.command.envFiles).toEqual([
      { path: join(dir, ".env"), keys: ["NETWORK"] },
    ]);
    expect(summary.command.envKeys).toEqual([
      "L1_PROVIDER_API_KEY=<redacted>",
      "NETWORK",
    ]);
  });

  it("terminates a timed-out service process group", async () => {
    const dir = await makeTempDir();
    const childTerminated = join(dir, "service-child-terminated.txt");
    const child = await writeScript(
      dir,
      "service-grandchild.mjs",
      [
        "import { writeFileSync } from 'node:fs';",
        `const terminated = ${JSON.stringify(childTerminated)};`,
        "process.on('SIGTERM', () => {",
        "  writeFileSync(terminated, String(process.pid));",
        "  process.exit(0);",
        "});",
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );
    const parent = await writeScript(
      dir,
      "service-parent.mjs",
      [
        "import { spawn } from 'node:child_process';",
        `spawn(process.execPath, [${JSON.stringify(child)}], { stdio: 'ignore' });`,
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );

    const summary = await superviseHostProcess({
      service: "stress",
      command: process.execPath,
      args: [parent],
      cwd: dir,
      timeoutMs: 300,
      rawLogPath: join(dir, "logs", "stress-service.log"),
      maxRestarts: 0,
    });

    expect(summary.status).toBe("timeout");
    expect(summary.attempts[0]?.cleanup).toMatchObject({
      attempted: true,
      target: process.platform === "win32" ? "process" : "process_group",
      success: true,
    });
    if (process.platform !== "win32") {
      await waitForFile(childTerminated);
    }
  });

  it("externally SIGKILLs a service when a checkpoint marker spans output chunks", async () => {
    const dir = await makeTempDir();
    const marker =
      "pipeline_trace phase=e2e_crash_checkpoint checkpoint=speculative_mid_build";
    const script = await writeScript(
      dir,
      "checkpoint-service.mjs",
      [
        `const marker = ${JSON.stringify(marker)};`,
        "process.stdout.write(marker.slice(0, 23));",
        "setTimeout(() => process.stdout.write(marker.slice(23) + '\\n'), 10);",
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );

    const summary = await superviseHostProcess({
      service: "checkpoint-node",
      command: process.execPath,
      args: [script],
      cwd: dir,
      timeoutMs: 2_000,
      rawLogPath: join(dir, "logs", "checkpoint-node.log"),
      maxRestarts: 0,
      terminateOnOutput: { marker, signal: "SIGKILL" },
    });

    expect(summary.status).toBe("restart_budget_exhausted");
    expect(summary.attempts[0]?.signal).toBe("SIGKILL");
    expect(summary.attempts[0]?.timedOut).toBe(false);
    expect(summary.attempts[0]?.outputTermination).toMatchObject({
      marker,
      signal: "SIGKILL",
    });
    expect(summary.attempts[0]?.classification).toMatchObject({
      class: "restartable_runtime",
      restartable: true,
    });
  });

  it("externally stops a supervised service when the operator creates a stop file", async () => {
    const dir = await makeTempDir();
    const stopFile = join(dir, "stop", "node.stop");
    const script = await writeScript(
      dir,
      "stop-file-service.mjs",
      ["console.log('ready');", "setInterval(() => {}, 1000);"].join("\n"),
    );
    const summaryPromise = superviseHostProcess({
      service: "stop-file-node",
      command: process.execPath,
      args: [script],
      cwd: dir,
      timeoutMs: 2_000,
      rawLogPath: join(dir, "logs", "stop-file-node.log"),
      maxRestarts: 0,
      terminateOnFile: { path: stopFile, signal: "SIGTERM" },
    });
    await mkdir(join(dir, "stop"), { recursive: true });
    await writeFile(stopFile, "stop\n", "utf8");
    const summary = await summaryPromise;

    expect(summary.attempts[0]?.signal).toBe("SIGTERM");
    expect(summary.attempts[0]?.fileTermination).toMatchObject({
      path: stopFile,
      signal: "SIGTERM",
    });
    expect(summary.status).not.toBe("exited_success");
  });
});

describe("e2e service probes", () => {
  it("accepts only the exact managed-service V1 shape", () => {
    const probe = {
      label: "node:ready",
      url: "http://127.0.0.1:3000/readyz",
      status: "healthy",
      statusCode: 200,
      latencyMs: 1,
      json: { ready: true },
      error: null,
    } as const;
    const summary = {
      schemaVersion: E2E_MANAGED_SERVICE_SCHEMA_VERSION,
      service: "node",
      pid: 123,
      rawLogPath: "logs/node.log",
      pidFile: {
        path: "logs/node.pid",
        status: "runner_owned",
        pid: 123,
      },
      ready: probe,
      command: {
        command: "node",
        args: ["dist/index.js"],
        cwd: "/tmp",
        envKeys: [],
        envFiles: [],
        envInheritance: "none",
      },
    } as const;
    expect(parseManagedServiceSummary(summary)).toEqual(summary);
    const { pid: _pid, ...missingPid } = summary;
    expect(() => parseManagedServiceSummary(missingPid)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseManagedServiceSummary({ ...summary, unexpected: true }),
    ).toThrow("unknown field");
    expect(() =>
      parseManagedServiceSummary({
        ...summary,
        schemaVersion: "midgard-e2e-managed-service-v0",
      }),
    ).toThrow(E2E_MANAGED_SERVICE_SCHEMA_VERSION);
    expect(() =>
      parseManagedServiceSummary({
        ...summary,
        ready: { ...probe, unexpected: true },
      }),
    ).toThrow("unknown field");
    expect(() =>
      parseManagedServiceSummary({
        ...summary,
        pidFile: { ...summary.pidFile, pid: 124 },
      }),
    ).toThrow("pid ownership or readiness evidence is inconsistent");
    expect(() =>
      parseManagedServiceSummary({
        ...summary,
        ready: {
          ...probe,
          status: "not_ready",
          statusCode: 503,
          json: { ready: false },
        },
      }),
    ).toThrow("pid ownership or readiness evidence is inconsistent");
  });

  it("samples JSON HTTP health endpoints", async () => {
    const server = createServer((_request, response) => {
      response.writeHead(503, { "content-type": "application/json" });
      response.end(JSON.stringify({ ready: false }));
    });
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address() as AddressInfo;
    try {
      const sample = await probeHttpEndpoint({
        label: "readyz",
        url: `http://127.0.0.1:${address.port.toString()}/readyz`,
      });

      expect(sample.status).toBe("not_ready");
      expect(sample.statusCode).toBe(503);
      expect(sample.json).toEqual({ ready: false });
    } finally {
      await new Promise<void>((resolve, reject) =>
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        ),
      );
    }
  });

  it("classifies absent, invalid, stale, owned, and foreign pid files", async () => {
    const dir = await makeTempDir();
    const absent = await inspectPidFile({ path: join(dir, "missing.pid") });
    expect(absent.status).toBe("absent");

    const invalidPath = join(dir, "invalid.pid");
    await writeFile(invalidPath, "not-a-pid", "utf8");
    expect((await inspectPidFile({ path: invalidPath })).status).toBe(
      "invalid",
    );

    const stalePath = join(dir, "stale.pid");
    await writeFile(stalePath, "99999999", "utf8");
    expect((await inspectPidFile({ path: stalePath })).status).toBe("stale");

    const ownedPath = join(dir, "owned.pid");
    await writeFile(ownedPath, process.pid.toString(), "utf8");
    expect(
      (
        await inspectPidFile({
          path: ownedPath,
          runnerOwnedPids: new Set([process.pid]),
        })
      ).status,
    ).toBe("runner_owned");

    const foreignPath = join(dir, "foreign.pid");
    await writeFile(foreignPath, process.pid.toString(), "utf8");
    expect((await inspectPidFile({ path: foreignPath })).status).toBe(
      "foreign",
    );
  });
});

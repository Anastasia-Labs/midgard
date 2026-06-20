import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  classifyServiceError,
  inspectPidFile,
  probeHttpEndpoint,
  superviseHostProcess,
} from "@/e2e/service-supervisor.js";

let tempDirs: string[] = [];

const makeTempDir = async (): Promise<string> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-e2e-supervisor-"));
  tempDirs.push(dir);
  return dir;
};

afterEach(async () => {
  await Promise.all(
    tempDirs.map((dir) => rm(dir, { recursive: true, force: true })),
  );
  tempDirs = [];
});

const writeScript = async (
  dir: string,
  name: string,
  source: string,
): Promise<string> => {
  const path = join(dir, name);
  await writeFile(path, source, "utf8");
  return path;
};

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
    expect(summary.restartCount).toBe(1);
    expect(summary.attempts).toHaveLength(2);
    expect(summary.attempts[0]?.classification.class).toBe(
      "transient_provider",
    );
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
});

describe("e2e service probes", () => {
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

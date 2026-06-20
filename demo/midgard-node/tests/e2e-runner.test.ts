import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  E2E_STEP_SCHEMA_VERSION,
  redactArg,
  redactEnvKeys,
  runCommandStep,
} from "@/e2e/runner.js";

let tempDirs: string[] = [];

const makeTempDir = async (): Promise<string> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-e2e-runner-"));
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

describe("e2e step runner", () => {
  it("records successful JSON output and tx hashes", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "success.mjs",
      [
        "console.log('prelude');",
        "console.log(JSON.stringify({ txHash: 'aa'.repeat(32), status: 'ok' }));",
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "submit-deposit",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "submit-deposit.log"),
    });

    expect(summary).toMatchObject({
      schemaVersion: E2E_STEP_SCHEMA_VERSION,
      id: "submit-deposit",
      status: "success",
      exitCode: 0,
      signal: null,
      timedOut: false,
      observedTxHashes: ["aa".repeat(32)],
      parsedJson: {
        txHash: "aa".repeat(32),
        status: "ok",
      },
      error: null,
    });
    await expect(readFile(summary.rawLogPath, "utf8")).resolves.toContain(
      "prelude",
    );
  });

  it("does not mark timeout-after-tx as success", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "timeout.mjs",
      [
        "console.log('Transaction submitted: ' + 'bb'.repeat(32));",
        "setTimeout(() => {}, 10_000);",
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "await-confirmation",
      command: process.execPath,
      args: [script],
      cwd: dir,
      timeoutMs: 100,
      rawLogPath: join(dir, "logs", "await-confirmation.log"),
    });

    expect(summary.status).toBe("timeout");
    expect(summary.timedOut).toBe(true);
    expect(summary.observedTxHashes).toEqual(["bb".repeat(32)]);
    expect(summary.error).toContain("timed out");
  });

  it("records nonzero exits as failed with stderr in the raw log", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "fail.mjs",
      ["console.error('boom');", "process.exit(7);"].join("\n"),
    );

    const summary = await runCommandStep({
      id: "provider-preflight",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "provider-preflight.log"),
    });

    expect(summary.status).toBe("failed");
    expect(summary.exitCode).toBe(7);
    expect(summary.error).toContain("7");
    await expect(readFile(summary.rawLogPath, "utf8")).resolves.toContain(
      "boom",
    );
  });

  it("redacts sensitive argv and env metadata", () => {
    expect(redactArg("--api-key=abc")).toBe("<redacted>");
    expect(redactArg("--amount-lovelace=1000000")).toBe(
      "--amount-lovelace=1000000",
    );
    expect(
      redactEnvKeys({
        L1_PROVIDER_API_KEY: "secret",
        NETWORK: "Preprod",
        USER_SEED_PHRASE: "secret",
      }),
    ).toEqual([
      "L1_PROVIDER_API_KEY=<redacted>",
      "NETWORK",
      "USER_SEED_PHRASE=<redacted>",
    ]);
  });
});

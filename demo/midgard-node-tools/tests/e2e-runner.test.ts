import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  createTrackedTempDirFactory,
  waitForFile,
  writeScript,
} from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import {
  E2E_STEP_SCHEMA_VERSION,
  parseE2EStep,
  redactArg,
  redactEnvKeys,
  runCommandStep,
} from "../src/e2e/runner.js";

const makeTempDir = createTrackedTempDirFactory("midgard-e2e-runner-");

describe("e2e step runner", () => {
  it("accepts only the exact V1 step and command shapes", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(dir, "exact.mjs", "console.log('{}');");
    const summary = await runCommandStep({
      id: "exact-step",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "exact.log"),
    });

    expect(parseE2EStep(summary)).toEqual(summary);
    const { id: _id, ...missingId } = summary;
    expect(() => parseE2EStep(missingId)).toThrow("missing required field");
    expect(() => parseE2EStep({ ...summary, unexpected: true })).toThrow(
      "unknown field",
    );
    expect(() =>
      parseE2EStep({ ...summary, schemaVersion: "midgard-e2e-step-v0" }),
    ).toThrow(E2E_STEP_SCHEMA_VERSION);
    expect(() =>
      parseE2EStep({
        ...summary,
        command: { ...summary.command, unexpected: true },
      }),
    ).toThrow("unknown field");
    const { cwd: _cwd, ...missingCommandCwd } = summary.command;
    expect(() =>
      parseE2EStep({ ...summary, command: missingCommandCwd }),
    ).toThrow("missing required field");
    expect(() =>
      parseE2EStep({
        ...summary,
        durationMs: summary.durationMs + 1,
      }),
    ).toThrow("timing or observation identity is inconsistent");
    expect(() =>
      parseE2EStep({
        ...summary,
        status: "success",
        exitCode: 1,
        error: "failed",
      }),
    ).toThrow("status and process outcome are inconsistent");
    expect(() =>
      parseE2EStep({
        ...summary,
        hashObservations: [
          {
            hash: "aa".repeat(32),
            role: "unknown",
            source: "regex",
            stepId: summary.id,
          },
        ],
      }),
    ).toThrow("timing or observation identity is inconsistent");
  });

  it("records successful JSON output and explicit submitted tx hashes", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "success.mjs",
      [
        "console.log('prelude');",
        "console.log(JSON.stringify({ txHash: 'aa'.repeat(32), status: 'submitted' }));",
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
      txObservations: [
        {
          txHash: "aa".repeat(32),
          role: "submitted",
          status: "submitted",
          source: "parsedJson",
          field: "$.txHash",
          stepId: "submit-deposit",
        },
      ],
      parsedJson: {
        txHash: "aa".repeat(32),
        status: "submitted",
      },
      error: null,
    });
    await expect(readFile(summary.rawLogPath, "utf8")).resolves.toContain(
      "prelude",
    );
  });

  it("does not promote generic JSON tx hashes without explicit tx status", async () => {
    const dir = await makeTempDir();
    const script = await writeScript(
      dir,
      "generic-success.mjs",
      [
        "console.log(JSON.stringify({ txHash: 'ab'.repeat(32), status: 'ok' }));",
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "provider-preflight",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "generic-success.log"),
    });

    expect(summary.status).toBe("success");
    expect(summary.observedTxHashes).toEqual(["ab".repeat(32)]);
    expect(summary.hashObservations).toEqual([
      {
        hash: "ab".repeat(32),
        role: "unknown",
        source: "regex",
        stepId: "provider-preflight",
      },
    ]);
    expect(summary.txObservations).toEqual([]);
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
      timeoutMs: 2_000,
      rawLogPath: join(dir, "logs", "await-confirmation.log"),
    });

    expect(summary.status).toBe("timeout");
    expect(summary.timedOut).toBe(true);
    expect(summary.observedTxHashes).toEqual(["bb".repeat(32)]);
    expect(summary.txObservations).toContainEqual({
      txHash: "bb".repeat(32),
      role: "submitted",
      status: "submitted",
      source: "log:transaction_submitted",
      stepId: "await-confirmation",
    });
    expect(summary.error).toContain("timed out");
  });

  it("terminates a timed-out step process group", async () => {
    const dir = await makeTempDir();
    const childTerminated = join(dir, "child-terminated.txt");
    const child = await writeScript(
      dir,
      "grandchild.mjs",
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
      "parent.mjs",
      [
        "import { spawn } from 'node:child_process';",
        `spawn(process.execPath, [${JSON.stringify(child)}], { stdio: 'ignore' });`,
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "stress",
      command: process.execPath,
      args: [parent],
      cwd: dir,
      timeoutMs: 300,
      rawLogPath: join(dir, "logs", "stress.log"),
    });

    expect(summary.status).toBe("timeout");
    expect(summary.cleanup).toMatchObject({
      attempted: true,
      target: process.platform === "win32" ? "process" : "process_group",
      success: true,
    });
    if (process.platform !== "win32") {
      await waitForFile(childTerminated);
    }
  });

  it("keeps generic and prepared hashes out of submitted tx evidence", async () => {
    const dir = await makeTempDir();
    const preparedHash = "cc".repeat(32);
    const rootHash = "dd".repeat(32);
    const submittedHash = "ee".repeat(32);
    const script = await writeScript(
      dir,
      "hashes.mjs",
      [
        `console.log('Signed tx prepared: txHash=${preparedHash}');`,
        `console.log('utxosRoot=${rootHash}');`,
        `console.log('Transaction submitted: ${submittedHash}');`,
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "submit-l2-transfer-a",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "hashes.log"),
    });

    expect(summary.observedTxHashes).toEqual([
      preparedHash,
      rootHash,
      submittedHash,
    ]);
    expect(summary.txObservations).toEqual([
      {
        txHash: submittedHash,
        role: "submitted",
        status: "submitted",
        source: "log:transaction_submitted",
        stepId: "submit-l2-transfer-a",
      },
      {
        txHash: preparedHash,
        role: "prepared",
        status: "prepared",
        source: "log:signed_tx_prepared",
        stepId: "submit-l2-transfer-a",
      },
    ]);
  });

  it("records known transaction field logs as explicit submitted evidence", async () => {
    const dir = await makeTempDir();
    const registerHash = "12".repeat(32);
    const activateHash = "34".repeat(32);
    const script = await writeScript(
      dir,
      "operator.mjs",
      [
        `console.log('Operator lifecycle result: registerTxHash=${registerHash}, activateTxHash=${activateHash}, deregisterTxHash=skipped');`,
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "operator-lifecycle",
      command: process.execPath,
      args: [script],
      cwd: dir,
      rawLogPath: join(dir, "logs", "operator.log"),
    });

    expect(summary.txObservations).toEqual([
      {
        txHash: registerHash,
        role: "submitted",
        status: "submitted",
        source: "log:structured_tx_field",
        field: "$.registerTxHash",
        stepId: "operator-lifecycle",
      },
      {
        txHash: activateHash,
        role: "submitted",
        status: "submitted",
        source: "log:structured_tx_field",
        field: "$.activateTxHash",
        stepId: "operator-lifecycle",
      },
    ]);
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

  it("loads dotenv files and explicit env overrides into child processes", async () => {
    const dir = await makeTempDir();
    await writeFile(
      join(dir, ".env"),
      ['WALLET_SEED_PHRASE="alpha beta"', "NETWORK=Preview"].join("\n"),
      "utf8",
    );
    const script = await writeScript(
      dir,
      "env.mjs",
      [
        "console.log(JSON.stringify({",
        "  seed: process.env.WALLET_SEED_PHRASE,",
        "  network: process.env.NETWORK,",
        "  inherited: process.env.PATH === undefined,",
        "}));",
      ].join("\n"),
    );

    const summary = await runCommandStep({
      id: "env-step",
      command: process.execPath,
      args: [script],
      cwd: dir,
      envFiles: [".env"],
      env: { NETWORK: "Preprod" },
      envInheritance: "none",
      rawLogPath: join(dir, "logs", "env-step.log"),
    });

    expect(summary.status).toBe("success");
    expect(summary.parsedJson).toEqual({
      seed: "alpha beta",
      network: "Preprod",
      inherited: true,
    });
    expect(summary.command.envFiles).toEqual([
      {
        path: join(dir, ".env"),
        keys: ["NETWORK", "WALLET_SEED_PHRASE=<redacted>"],
      },
    ]);
    expect(summary.command.envKeys).toEqual([
      "NETWORK",
      "WALLET_SEED_PHRASE=<redacted>",
    ]);
    expect(summary.command.envInheritance).toBe("none");
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

import { spawn } from "node:child_process";
import { mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { localUrl, parseStackConfig } from "../src/full-stack/config.js";
import {
  type InitializationObservation,
  initializationRecovery,
} from "../src/full-stack/initialization-recovery.js";
import {
  parseJournal,
  readJsonIfPresent,
  writeDurableJson,
} from "../src/full-stack/journal.js";
import {
  equalAssets,
  verifyPayoutBody,
} from "../src/full-stack/payout-body.js";
import { stackIsReady } from "../src/full-stack/readiness.js";
import {
  runStackWorkflow,
  type StackStep,
} from "../src/full-stack/workflow.js";

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});
async function context() {
  const directory = await mkdtemp(join(tmpdir(), "midgard-stack-test-"));
  directories.push(directory);
  return { directory, intentDigest: "a".repeat(64) };
}
const step = (
  execute: StackStep["execute"],
  reconcile: StackStep["reconcile"],
): StackStep => ({ id: "deploy", execute, reconcile });
const readCheckpoint = (directory: string) =>
  readJsonIfPresent(join(directory, "stack-journal.json")) as Promise<{
    steps: Record<string, { status: string; attempts: number }>;
    runId: string;
  }>;

describe("durable stack recovery", () => {
  it("observes a confirmed deployment after a crash before its success checkpoint", async () => {
    const ctx = await context();
    let chainConfirmed = false;
    let submissions = 0;
    const deployment = step(
      async () => {
        submissions++;
        chainConfirmed = true;
        throw new Error("crash after confirmation");
      },
      async () =>
        chainConfirmed
          ? { status: "complete", data: { txHash: "b".repeat(64) } }
          : { status: "retry" },
    );
    await expect(runStackWorkflow(ctx, [deployment])).rejects.toThrow(
      "crash after confirmation",
    );
    const before = await readCheckpoint(ctx.directory);
    expect(before.steps.deploy!.status).toBe("running");
    await runStackWorkflow(ctx, [deployment]);
    const after = await readCheckpoint(ctx.directory);
    expect(submissions).toBe(1);
    expect(after.runId).toBe(before.runId);
    expect(after.steps.deploy!.status).toBe("complete");
  });
  it("persists transaction intent before execution and refuses an ambiguous retry", async () => {
    const ctx = await context();
    let calls = 0;
    const deployment = step(
      async () => {
        calls++;
        expect((await readCheckpoint(ctx.directory)).steps.deploy!.status).toBe(
          "running",
        );
        throw new Error("lost submission response");
      },
      async (record) => (record ? { status: "pending" } : { status: "retry" }),
    );
    await expect(runStackWorkflow(ctx, [deployment])).rejects.toThrow(
      "lost submission response",
    );
    await expect(runStackWorkflow(ctx, [deployment])).rejects.toThrow(
      "no transaction was repeated",
    );
    expect(calls).toBe(1);
  });
  it("resumes the same saved signed transaction after a lost response", async () => {
    const ctx = await context();
    const intentFile = join(ctx.directory, "intent.json");
    let builds = 0;
    const sent: unknown[] = [];
    let confirmed = false;
    const submission = step(
      async () => {
        let intent = await readJsonIfPresent(intentFile);
        if (!intent) {
          builds++;
          intent = { signedCbor: "signed immutable intent" };
          await writeDurableJson(intentFile, intent);
        }
        sent.push(intent);
        if (sent.length === 1) throw new Error("response lost");
        confirmed = true;
        return intent;
      },
      async () =>
        confirmed
          ? { status: "complete", data: await readJsonIfPresent(intentFile) }
          : { status: "retry" },
    );
    await expect(runStackWorkflow(ctx, [submission])).rejects.toThrow(
      "response lost",
    );
    await runStackWorkflow(ctx, [submission]);
    expect(builds).toBe(1);
    expect(sent[0]).toEqual(sent[1]);
  });
  it("rechecks previously completed steps against authoritative state", async () => {
    const ctx = await context();
    let observations = 0;
    const deployment = step(
      async () => null,
      async () => {
        observations++;
        return { status: "complete", data: "confirmed" };
      },
    );
    await runStackWorkflow(ctx, [deployment]);
    await runStackWorkflow(ctx, [deployment]);
    expect(observations).toBe(2);
  });
  it("rejects changed configuration before reconciliation or execution", async () => {
    const ctx = await context();
    await runStackWorkflow(ctx, []);
    let observed = false;
    await expect(
      runStackWorkflow({ ...ctx, intentDigest: "c".repeat(64) }, [
        step(
          async () => null,
          async () => {
            observed = true;
            return { status: "retry" };
          },
        ),
      ]),
    ).rejects.toThrow("identity differs");
    expect(observed).toBe(false);
  });
  it("refuses corrupt checkpoints and never treats invalid JSON as missing", async () => {
    const ctx = await context();
    const path = join(ctx.directory, "stack-journal.json");
    await writeFile(path, "{partial");
    await expect(runStackWorkflow(ctx, [])).rejects.toThrow();
    for (const steps of [
      "invalid",
      [],
      { deployment: { status: "complete", attempts: 0, data: null } },
    ])
      expect(() =>
        parseJournal(
          {
            schemaVersion: "midgard-full-stack-v1",
            runId: "run",
            intentDigest: ctx.intentDigest,
            steps,
          },
          ctx.intentDigest,
        ),
      ).toThrow();
  });
  it("writes private, complete checkpoints with exact bigint amounts", async () => {
    const ctx = await context();
    const path = join(ctx.directory, "receipt.json");
    await writeDurableJson(path, { lovelace: 9007199254740993n });
    expect(JSON.parse(await readFile(path, "utf8"))).toEqual({
      lovelace: "9007199254740993",
    });
    expect((await stat(path)).mode & 0o777).toBe(0o600);
  });
  it("does not mark unconfirmed execution complete", async () => {
    const ctx = await context();
    await expect(
      runStackWorkflow(ctx, [
        step(
          async () => ({ submitted: true }),
          async () => ({ status: "retry" }),
        ),
      ]),
    ).rejects.toThrow("not been confirmed");
    expect((await readCheckpoint(ctx.directory)).steps.deploy!.status).toBe(
      "running",
    );
  });
});

describe("stack input and payout verification", () => {
  it.each([
    "https://remote.example",
    "http://10.0.0.1:1442",
    "http://secret@localhost:1442",
    "file:///tmp/node",
  ])("refuses nonlocal or credential-bearing URL %s", (url) =>
    expect(() => localUrl(url)).toThrow(),
  );
  it("refuses incomplete configurations", () =>
    expect(() => parseStackConfig({ nodeRoot: "/tmp/node" })).toThrow(
      "missing or unknown",
    ));
  it("compares exact amounts across JSON and decoded Cardano values", () => {
    expect(
      equalAssets(
        { lovelace: "9007199254740993", token: "2" },
        { token: 2n, lovelace: 9007199254740993n, empty: 0n },
      ),
    ).toBe(true);
    expect(
      equalAssets(
        { lovelace: "10000000", token: "2" },
        { lovelace: 10000000n },
      ),
    ).toBe(false);
  });
  const address = walletFromSeed(
    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
    { network: "Preprod" },
  ).address;
  function payout(amounts: bigint[]) {
    const outputs = CML.TransactionOutputList.new();
    for (const amount of amounts)
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(amount),
        ),
      );
    return CML.Transaction.new(
      CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        outputs,
        250_000n,
      ),
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex();
  }
  it("verifies payout value separately from L1 transaction fees", () =>
    expect(
      verifyPayoutBody(payout([10_000_000n]), address, {
        lovelace: "10000000",
      }),
    ).toEqual({ outputIndex: 0 }));
  it("rejects short, excessive and duplicate payments", () => {
    for (const amounts of [
      [9_750_000n],
      [10_250_000n],
      [10_000_000n, 10_000_000n],
    ])
      expect(() =>
        verifyPayoutBody(payout(amounts), address, { lovelace: "10000000" }),
      ).toThrow("exactly one output");
  });
});

it("kernel locks prevent concurrent controllers and release after process death", async () => {
  const ctx = await context();
  const lock = join(ctx.directory, "controller.lock");
  const child = spawn(
    "flock",
    [
      "--nonblock",
      "--no-fork",
      lock,
      process.execPath,
      "-e",
      "console.log('locked');setInterval(()=>{},1000)",
    ],
    { stdio: ["ignore", "pipe", "pipe"], env: { PATH: process.env.PATH } },
  );
  const exited = new Promise<void>((resolve) =>
    child.once("exit", () => resolve()),
  );
  try {
    await new Promise<void>((resolve, reject) => {
      child.stdout.once("data", () => resolve());
      child.once("error", reject);
      child.once("exit", (code) =>
        reject(new Error(`Lock owner exited before readiness: ${code}`)),
      );
      child.stderr.once("data", (data) =>
        reject(new Error(`Lock owner stderr: ${String(data)}`)),
      );
    });
    const probe = () =>
      new Promise<number | null>((resolve, reject) => {
        const attempt = spawn(
          "flock",
          ["--nonblock", lock, process.execPath, "-e", ""],
          { stdio: "ignore", env: { PATH: process.env.PATH } },
        );
        attempt.once("exit", resolve);
        attempt.once("error", reject);
      });
    expect(await probe()).toBe(1);
    child.kill("SIGKILL");
    await exited;
    expect(await probe()).toBe(0);
  } finally {
    child.kill("SIGKILL");
    await exited;
  }
});

describe("published stack readiness schemas", () => {
  const ready = {
    manifestId: "a".repeat(64),
    recordKeyId: "b".repeat(64),
    node: { ready: true, reasons: [], settlement: { state: "waiting" } },
    watcher: {
      liveness: "live",
      readiness: "ready",
      readinessReasons: [],
      deploymentFingerprint: "a".repeat(64),
      launchScope: { complete: true },
    },
    authority: { recordAuthenticationKeyId: "b".repeat(64) },
    committees: [{ ready: true }],
  };
  it("accepts healthy automatic settlement waiting for work", () =>
    expect(stackIsReady(ready)).toBe(true));
  it("rejects live services with wrong identities or readiness reasons", () => {
    expect(
      stackIsReady({
        ...ready,
        authority: { recordAuthenticationKeyId: "c".repeat(64) },
      }),
    ).toBe(false);
    expect(
      stackIsReady({
        ...ready,
        watcher: { ...ready.watcher, deploymentFingerprint: "d".repeat(64) },
      }),
    ).toBe(false);
    expect(
      stackIsReady({
        ...ready,
        node: { ...ready.node, reasons: ["l1 unavailable"] },
      }),
    ).toBe(false);
    expect(stackIsReady({ ...ready, committees: [{ ready: false }] })).toBe(
      false,
    );
    expect(
      stackIsReady({
        ...ready,
        watcher: { ...ready.watcher, launchScope: { complete: false } },
      }),
    ).toBe(false);
  });
  it("refuses settlement errors even when node HTTP reports ready", () =>
    expect(() =>
      stackIsReady({
        ...ready,
        node: { ...ready.node, settlement: { state: "error" } },
      }),
    ).toThrow("Automatic settlement is unhealthy"));
});

describe("Cardano initialization recovery", () => {
  const initialized: InitializationObservation = {
    manifest: { ok: false },
    protocol: {
      complete: true,
      empty: false,
      hubOracleWitness: {
        txHash: "e".repeat(64),
        outputIndex: 0,
        address: "addr_test1",
        assets: { lovelace: 2_000_000n },
      },
    },
  };
  it("recovers the initialization hash from the published UTxO after an unrecorded confirmation", () =>
    expect(
      initializationRecovery(initialized, undefined, {
        status: "running",
        attempts: 1,
        data: null,
      }),
    ).toEqual({
      status: "complete",
      initHash: "e".repeat(64),
      reconstruct: true,
    }));
  it("refuses partial protocol state and ambiguous previous attempts", () => {
    const empty = {
      ...initialized,
      protocol: { ...initialized.protocol, complete: false, empty: true },
    };
    expect(initializationRecovery(empty, undefined, undefined)).toEqual({
      status: "retry",
    });
    expect(
      initializationRecovery(empty, undefined, {
        status: "running",
        attempts: 1,
        data: null,
      }),
    ).toEqual({ status: "pending" });
    expect(
      initializationRecovery(
        { ...empty, protocol: { ...empty.protocol, empty: false } },
        undefined,
        undefined,
      ),
    ).toEqual({ status: "pending" });
  });
  it("refuses finalized manifest drift and unidentified initialization", () => {
    expect(() =>
      initializationRecovery(
        initialized,
        {
          steps: {
            initProtocol: { status: "complete", txHash: "f".repeat(64) },
          },
        },
        undefined,
      ),
    ).toThrow("disagrees with Cardano");
    expect(() =>
      initializationRecovery(
        {
          ...initialized,
          protocol: { ...initialized.protocol, hubOracleWitness: null },
        },
        undefined,
        undefined,
      ),
    ).toThrow("Cannot establish");
  });
});

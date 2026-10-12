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
  it("persists submitted evidence before confirming it", async () => {
    const ctx = await context();
    await expect(
      runStackWorkflow(ctx, [
        step(
          async () => ({ signed: "evidence" }),
          async (record) => {
            if (record) throw new Error("confirmation unavailable");
            return { status: "retry" };
          },
        ),
      ]),
    ).rejects.toThrow("confirmation unavailable");
    expect((await readCheckpoint(ctx.directory)).steps.deploy).toEqual({
      status: "running",
      attempts: 1,
      data: { signed: "evidence" },
    });
  });
  it("never carries an earlier attempt's evidence into a new attempt", async () => {
    const ctx = await context();
    let fail = false;
    const deployment = step(
      async () => {
        if (fail) throw new Error("underfunded");
        return { addresses: "checked" };
      },
      async (record) => {
        // Mirrors the reconciles that accept a running record whose execute returned.
        return record?.status === "running" && record.data !== null
          ? { status: "complete", data: record.data }
          : { status: "retry" };
      },
    );
    await runStackWorkflow(ctx, [deployment]);
    // A completed step whose reconcile accepts only running records re-executes, and now fails.
    fail = true;
    await expect(runStackWorkflow(ctx, [deployment])).rejects.toThrow(
      "underfunded",
    );
    expect((await readCheckpoint(ctx.directory)).steps.deploy).toMatchObject({
      status: "running",
      data: null,
    });
    await expect(runStackWorkflow(ctx, [deployment])).rejects.toThrow(
      "underfunded",
    );
  });
  it("creates the journal exclusively and refuses a concurrent different intent", async () => {
    const ctx = await context();
    const results = await Promise.allSettled([
      runStackWorkflow(ctx, []),
      runStackWorkflow({ ...ctx, intentDigest: "c".repeat(64) }, []),
    ]);
    expect(results.map((result) => result.status).sort()).toEqual([
      "fulfilled",
      "rejected",
    ]);
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
  function payout(amounts: bigint[], to = address) {
    const outputs = CML.TransactionOutputList.new();
    for (const amount of amounts)
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(to),
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
  it("rejects the exact value paid to another address", () => {
    const other = walletFromSeed(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong",
      { network: "Preprod" },
    ).address;
    expect(() =>
      verifyPayoutBody(payout([10_000_000n], other), address, {
        lovelace: "10000000",
      }),
    ).toThrow("exactly one output");
  });
});

const committeeReady = (signerIndex: number) => ({
  ready: true,
  deployment: {
    configuredFingerprint: "a".repeat(64),
    storeMatchesConfigured: true,
  },
  peer: { signerIndex, localPeerId: `peer-${signerIndex}` },
});
describe("published stack readiness schemas", () => {
  const ready = {
    manifestId: "a".repeat(64),
    node: { ready: true, reasons: [], settlement: { state: "waiting" } },
    watcher: {
      liveness: "live",
      readiness: "ready",
      readinessReasons: [],
      deploymentFingerprint: "a".repeat(64),
      launchScope: { complete: true },
    },
    committees: [committeeReady(0)],
    committeePeerIds: ["peer-0"],
  };
  it("accepts healthy automatic settlement waiting for work", () =>
    expect(stackIsReady(ready)).toBe(true));
  it("rejects live services with wrong identities or readiness reasons", () => {
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
    expect(
      stackIsReady({
        ...ready,
        committees: [{ ...committeeReady(0), ready: false }],
      }),
    ).toBe(false);
    expect(
      stackIsReady({ ...ready, node: { ...ready.node, ready: false } }),
    ).toBe(false);
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
    expect(initializationRecovery(initialized, undefined)).toEqual({
      status: "complete",
      initHash: "e".repeat(64),
      reconstruct: true,
    }));
  it("retries an empty protocol after any earlier attempt and refuses partial state", () => {
    // The one-shot nonce lets at most one initialization land.
    const empty = {
      ...initialized,
      protocol: { ...initialized.protocol, complete: false, empty: true },
    };
    expect(initializationRecovery(empty, undefined)).toEqual({
      status: "retry",
    });
    expect(
      initializationRecovery(
        { ...empty, protocol: { ...empty.protocol, empty: false } },
        undefined,
      ),
    ).toEqual({ status: "pending" });
    expect(() =>
      initializationRecovery(empty, {
        steps: { initProtocol: { status: "complete", txHash: "e".repeat(64) } },
      }),
    ).toThrow("pending re-inclusion");
  });
  it("refuses finalized manifest drift and unidentified initialization", () => {
    expect(() =>
      initializationRecovery(initialized, {
        steps: {
          initProtocol: { status: "complete", txHash: "f".repeat(64) },
        },
      }),
    ).toThrow("disagrees with Cardano");
    expect(() =>
      initializationRecovery(
        { ...initialized, manifest: { ok: true } },
        {
          steps: {
            initProtocol: { status: "complete", txHash: "f".repeat(64) },
          },
        },
      ),
    ).toThrow("Recorded initialization transaction disagrees with Cardano");
    expect(() =>
      initializationRecovery(
        {
          ...initialized,
          protocol: { ...initialized.protocol, hubOracleWitness: null },
        },
        undefined,
      ),
    ).toThrow("Cannot establish");
  });
});

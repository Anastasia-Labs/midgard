/**
 * `midgard-user deposit` and `status` against a deployed emulator scene: what
 * the chain and the record files show, and the exit code.
 */
import { createHash } from "node:crypto";
import { existsSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { createServer, type Server } from "node:http";
import { type AddressInfo } from "node:net";
import { join } from "node:path";

import { outputReferenceToPlutusDataCbor } from "@al-ft/midgard-sdk";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  attemptFile,
  jsonFile,
  printedJson,
  printedOutcome,
  SCENE_TIMEOUT_MS,
  setupUserCliHarness,
  tempDirectory,
  type UserCliHarness,
} from "./support/user-cli.js";

const TEST_TIMEOUT_MS = 180_000;

const depositArgs = (submissionId: string, lovelace = "5000000") => [
  "deposit",
  "--submission-id",
  submissionId,
  "--l2-address",
  harness.scene.user.address,
  "--lovelace",
  lovelace,
  "--json",
];

const recordDirectoryOf = (root: string, submissionId: string) =>
  join(
    root,
    harness.manifest.manifestId,
    createHash("sha256").update(`Deposit:${submissionId}`).digest("hex"),
  );

const mode = (path: string) => statSync(path).mode & 0o777;

let harness: UserCliHarness;

beforeAll(async () => {
  harness = await setupUserCliHarness();
}, SCENE_TIMEOUT_MS);

describe("midgard-user deposit", () => {
  it(
    "lands the deposit, prints its outcome and records create-once files",
    async () => {
      const root = tempDirectory("deposit");
      const result = await harness.run(depositArgs("first"), {
        recordDir: root,
      });
      expect(result.stderr).not.toContain("midgard-user:");
      expect(result.code).toBe(0);
      const outcome = printedOutcome(result.stdout);
      expect(outcome).toMatchObject({
        kind: "Deposit",
        submissionId: "first",
        attempts: 1,
      });
      const status = await harness.scene.lucid.transactionStatus(
        outcome.admissionTxHash,
      );
      expect(status.status).toBe("confirmed");

      const directory = recordDirectoryOf(root, "first");
      expect(readdirSync(directory).sort()).toEqual([
        "attempt-0001.json",
        "intent.json",
      ]);
      expect(mode(join(root, harness.manifest.manifestId))).toBe(0o700);
      expect(mode(directory)).toBe(0o700);
      expect(mode(join(directory, "intent.json"))).toBe(0o600);
      expect(mode(join(directory, "attempt-0001.json"))).toBe(0o600);
      const intent = jsonFile(join(directory, "intent.json"));
      expect(intent).toMatchObject({
        version: 1,
        kind: "Deposit",
        submissionId: "first",
        manifestId: harness.manifest.manifestId,
        walletAddress: harness.scene.user.address,
        intent: { lovelace: "5000000", l2Address: harness.scene.user.address },
      });
      const attempt = attemptFile(join(directory, "attempt-0001.json"));
      expect(attempt).toMatchObject({
        version: 1,
        attempt: 1,
        phase: "Admission",
        txHash: outcome.admissionTxHash,
        eventId: outcome.eventId,
      });
      const [txHash, outputIndex] = attempt.nonce.split("#");
      expect(
        outputReferenceToPlutusDataCbor({
          txHash,
          outputIndex: Number(outputIndex),
        }),
      ).toBe(outcome.eventId);
      expect(JSON.stringify(intent) + JSON.stringify(attempt)).not.toContain(
        harness.scene.user.seedPhrase,
      );

      // A rerun observes the confirmed attempt; it builds nothing new.
      const rerun = await harness.run(depositArgs("first"), {
        recordDir: root,
      });
      expect(rerun.code).toBe(0);
      expect(printedOutcome(rerun.stdout).admissionTxHash).toBe(
        outcome.admissionTxHash,
      );
      expect(readdirSync(directory)).toHaveLength(2);

      // The same id cannot name another request.
      const other = await harness.run(depositArgs("first", "6000000"), {
        recordDir: root,
      });
      expect(other.code).toBe(1);
      expect(other.stderr).toContain("already names a different request");
      expect(readdirSync(directory)).toHaveLength(2);

      const shown = await harness.run(
        ["status", "--submission-id", "first", "--kind", "deposit", "--json"],
        { recordDir: root },
      );
      expect(shown.code).toBe(0);
      expect(printedJson(shown.stdout)).toMatchObject({
        verdict: "confirmed",
        attempts: [
          {
            attempt: 1,
            txHash: outcome.admissionTxHash,
            status: "confirmed",
            eventId: outcome.eventId,
          },
        ],
      });
      const missing = await harness.run(
        ["status", "--submission-id", "never", "--kind", "deposit"],
        { recordDir: root },
      );
      expect(missing.code).toBe(1);
      expect(missing.stderr).toContain("No deposit record");
    },
    TEST_TIMEOUT_MS,
  );

  it(
    "refuses a record file that does not check, naming it",
    async () => {
      const root = tempDirectory("tamper");
      expect(
        (await harness.run(depositArgs("tamper"), { recordDir: root })).code,
      ).toBe(0);
      const path = join(recordDirectoryOf(root, "tamper"), "attempt-0001.json");
      const attempt = attemptFile(path);
      writeFileSync(
        path,
        JSON.stringify({ ...attempt, ttlSlot: attempt.ttlSlot + 1 }),
      );
      const rerun = await harness.run(depositArgs("tamper"), {
        recordDir: root,
      });
      expect(rerun.code).toBe(1);
      expect(rerun.stderr).toContain(`Record file ${path} is invalid`);
      expect(rerun.stderr).toContain("ttlSlot");
    },
    TEST_TIMEOUT_MS,
  );

  it(
    "exits 75 at its deadline with the admission in flight, and a rerun lands that same transaction",
    async () => {
      const root = tempDirectory("pending");
      // Let the list predecessor an earlier test admitted leave its
      // protection, so this run builds and broadcasts at once.
      harness.scene.emulator.awaitSlot(
        Number(harness.scene.protectionDurationMs / 1000n) + 120,
      );
      // A clock that moves while the chain does not: the admission stays in
      // the mempool past the deadline.
      let now = harness.scene.emulator.now();
      const pending = await harness.run(
        [...depositArgs("pending"), "--timeout-ms", "1000"],
        {
          recordDir: root,
          overrides: {
            now: () => now,
            sleep: async (ms) => {
              now += ms;
            },
          },
        },
      );
      expect(pending.code).toBe(75);
      expect(pending.stderr).toContain(
        "pending; rerun with the same --submission-id",
      );
      const attempt = attemptFile(
        join(recordDirectoryOf(root, "pending"), "attempt-0001.json"),
      );
      expect(pending.stderr).toContain(attempt.txHash);

      const rerun = await harness.run(depositArgs("pending"), {
        recordDir: root,
      });
      expect(rerun.code).toBe(0);
      expect(printedJson(rerun.stdout)).toMatchObject({
        admissionTxHash: attempt.txHash,
        attempts: 1,
      });
    },
    TEST_TIMEOUT_MS,
  );
});

describe("midgard-user deposit through Kupmios", () => {
  let server: Server;
  let kupoUrl: string;
  const covered = new Set<string>();

  beforeAll(async () => {
    server = createServer((request, response) => {
      const address = /^\/patterns\/(.+)$/u.exec(request.url ?? "")?.[1];
      response.setHeader("content-type", "application/json");
      if (address === undefined) {
        response.statusCode = 404;
        response.end("{}");
        return;
      }
      response.end(JSON.stringify(covered.has(address) ? ["*"] : []));
    });
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    kupoUrl = `http://127.0.0.1:${(server.address() as AddressInfo).port.toString()}`;
  });
  afterAll(async () => {
    await new Promise((resolve) => server.close(resolve));
  });

  const kupmios = () => [
    "--l1",
    "kupmios",
    "--kupo-url",
    kupoUrl,
    "--ogmios-url",
    "http://127.0.0.1:9/unused",
  ];

  it(
    "refuses, naming the address, when Kupo does not index the deposit list; writes nothing",
    async () => {
      const root = tempDirectory("kupo-uncovered");
      covered.clear();
      covered.add(harness.scene.user.address);
      const result = await harness.run(depositArgs("kupo"), {
        recordDir: root,
        access: kupmios(),
        overrides: { fetch },
      });
      expect(result.code).toBe(1);
      expect(result.stderr).toContain(
        harness.scene.contracts.deposit.spendingScriptAddress,
      );
      expect(result.stderr).toContain('--match "*"');
      expect(existsSync(join(root, harness.manifest.manifestId))).toBe(false);
    },
    TEST_TIMEOUT_MS,
  );

  it(
    "lands when Kupo indexes the wallet and the deposit list",
    async () => {
      const root = tempDirectory("kupo-covered");
      covered.clear();
      covered.add(harness.scene.user.address);
      covered.add(harness.scene.contracts.deposit.spendingScriptAddress);
      const result = await harness.run(depositArgs("kupo"), {
        recordDir: root,
        access: kupmios(),
        overrides: { fetch },
      });
      expect(result.stderr).not.toContain("midgard-user:");
      expect(result.code).toBe(0);
    },
    TEST_TIMEOUT_MS,
  );
});

import { randomUUID } from "node:crypto";
import { mkdir, readFile, rm, writeFile } from "node:fs/promises";
import path from "node:path";

import { encodeMidgardProofSubmissionV1 } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect, ManagedRuntime } from "effect";
import {
  AddressHistoryDB,
  MempoolDB,
  MempoolTxDeltasDB,
  TxAdmissionsDB,
} from "midgard-node/database/index";
import {
  PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK,
  PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK_ENV,
  PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN_ENV,
  PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID_ENV,
  phase1AcceptCrashCheckpointMarker,
} from "midgard-node/e2e/phase1-accept-crash-checkpoint";
import { Database } from "midgard-node/services/database";
import { sleep } from "midgard-node/sleep";
import { breakDownTx } from "midgard-node/utils";
import { describe, expect, it } from "vitest";

import {
  type ServiceSupervisorSummary,
  superviseHostProcess,
} from "../src/e2e/service-supervisor.js";

const OPERATOR_TOKEN = "I_ACKNOWLEDGE_EXACT_KILLED_TX_OPERATOR_TEST";
const ISOLATED_DEPLOYMENT_ACK =
  "I_ACKNOWLEDGE_DESTRUCTIVE_ISOLATED_PHASE1_DEPLOYMENT";
const operatorEnabled =
  process.env.PHASE1_EXACT_CRASH_OPERATOR_TOKEN === OPERATOR_TOKEN;

const waitFor = async <A>(
  label: string,
  timeoutMs: number,
  probe: () => Promise<A | null>,
): Promise<A> => {
  const startedAt = Date.now();
  let lastError: unknown;
  while (Date.now() - startedAt <= timeoutMs) {
    try {
      const value = await probe();
      if (value !== null) {
        return value;
      }
    } catch (error) {
      lastError = error;
    }
    await sleep(250);
  }
  throw new Error(
    `Timed out waiting for ${label}${
      lastError === undefined ? "" : `: ${String(lastError)}`
    }`,
  );
};

const waitForReady = (endpoint: string, timeoutMs: number): Promise<void> =>
  waitFor("node readiness", timeoutMs, async () => {
    const response = await fetch(`${endpoint}/readyz`, {
      signal: AbortSignal.timeout(2_000),
    });
    return response.ok ? undefined : null;
  });

const metricValue = (text: string, name: string): bigint | null => {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
  const match = text.match(
    new RegExp(`^${escaped}(?:\\{[^}]*\\})?\\s+([0-9]+)$`, "mu"),
  );
  return match?.[1] === undefined ? null : BigInt(match[1]);
};

describe("Phase 1 exact killed-tx operator closure", () => {
  it.skipIf(!operatorEnabled)(
    "SIGKILLs dist listen after exact accept commit, restarts, and commits that same tx through fallback",
    async () => {
      expect(process.env.PHASE1_EXACT_CRASH_DEPLOYMENT_ACK).toBe(
        ISOLATED_DEPLOYMENT_ACK,
      );
      expect(
        process.env.NODE_ENV === "test" || process.env.NODE_ENV === "emulator",
      ).toBe(true);
      const database = process.env.POSTGRES_DB ?? "";
      expect(database).toMatch(/^midgard_phase1_crash_[a-z0-9_]+$/u);

      const txCanonicalCborHex =
        process.env.PHASE1_EXACT_CRASH_TX_CANONICAL_CBOR_HEX ?? "";
      expect(txCanonicalCborHex).toMatch(/^[0-9a-fA-F]+$/u);
      expect(txCanonicalCborHex.length % 2).toBe(0);
      const txCanonicalCbor = Buffer.from(txCanonicalCborHex, "hex");
      const txId = computeMidgardNativeTxIdV1(
        decodeMidgardNativeTxFullV1FromCanonicalCbor(txCanonicalCbor),
      );
      const txIdHex = txId.toString("hex");
      const configuredTxId =
        process.env.PHASE1_EXACT_CRASH_EXPECTED_TX_ID?.toLowerCase();
      expect(configuredTxId).toBe(txIdHex);

      // The node under test is the operator package's own build, not this
      // tooling package.
      const cwd = path.resolve(__dirname, "../../midgard-node");
      const distIndex = path.resolve(
        cwd,
        process.env.PHASE1_EXACT_CRASH_DIST_INDEX ?? "dist/index.js",
      );
      const endpoint = process.env.PHASE1_EXACT_CRASH_NODE_ENDPOINT ?? "";
      const metricsEndpoint =
        process.env.PHASE1_EXACT_CRASH_METRICS_ENDPOINT ?? "";
      expect(endpoint).toMatch(/^http:\/\/127\.0\.0\.1:\d+$/u);
      expect(metricsEndpoint).toMatch(/^http:\/\/127\.0\.0\.1:\d+\/metrics$/u);
      const timeoutMs = Number(
        process.env.PHASE1_EXACT_CRASH_TIMEOUT_MS ?? 180_000,
      );
      expect(Number.isSafeInteger(timeoutMs) && timeoutMs > 0).toBe(true);
      await readFile(distIndex);

      const processed = await Effect.runPromise(breakDownTx(txCanonicalCbor));
      expect(processed.txId).toEqual(txId);
      const producedAddresses = [
        ...new Set(processed.produced.map((entry) => entry.address)),
      ];
      expect(producedAddresses.length).toBeGreaterThan(0);

      const databaseRuntime = ManagedRuntime.make(Database.layer);
      const runDatabase = <A, E>(effect: Effect.Effect<A, E, Database>) =>
        databaseRuntime.runPromise(effect);
      const hasExactCommittedJournal = () =>
        runDatabase(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const rows = yield* sql<{
              readonly present: boolean;
            }>`SELECT EXISTS (
              SELECT 1
              FROM pending_block_finalization_txs AS member
              INNER JOIN pending_block_finalizations AS journal
                ON journal.header_hash = member.header_hash
              WHERE member.member_id = ${txId}
                AND journal.submitted_tx_hash IS NOT NULL
                AND journal.status <> 'pending_submission'
            ) AS present`;
            return rows[0]?.present === true;
          }),
        );
      const checkpointToken = randomUUID();
      const checkpointMarker = phase1AcceptCrashCheckpointMarker(
        checkpointToken,
        txIdHex,
      );
      const artifactDirectory = path.resolve(
        cwd,
        ".probe-dist/phase1-exact-crash",
      );
      const firstLog = path.resolve(artifactDirectory, "accept-crash.log");
      const restartLog = path.resolve(artifactDirectory, "restart-commit.log");
      const firstStopFile = path.resolve(artifactDirectory, "stop-first");
      const restartStopFile = path.resolve(artifactDirectory, "stop-restart");
      await mkdir(artifactDirectory, { recursive: true });
      await Promise.all([
        rm(firstStopFile, { force: true }),
        rm(restartStopFile, { force: true }),
      ]);

      let firstProcess: Promise<ServiceSupervisorSummary> | undefined;
      let restartProcess: Promise<ServiceSupervisorSummary> | undefined;
      try {
        expect(await runDatabase(MempoolDB.retrieveTxCount)).toBe(0n);
        expect(await runDatabase(TxAdmissionsDB.getByTxId(txId))).toBeNull();
        expect(
          (await runDatabase(MempoolTxDeltasDB.retrieveByTxIds([txId]))).size,
        ).toBe(0);
        expect(await hasExactCommittedJournal()).toBe(false);
        for (const address of producedAddresses) {
          expect(await runDatabase(AddressHistoryDB.retrieve(address))).toEqual(
            [],
          );
        }

        firstProcess = superviseHostProcess({
          service: "phase1-exact-accept-crash",
          command: process.execPath,
          args: [distIndex, "listen", "--with-monitoring"],
          cwd,
          envInheritance: "process",
          env: {
            WRITE_BEHIND_FLUSH_INTERVAL_MS: "60000",
            [PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN_ENV]: checkpointToken,
            [PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK_ENV]:
              PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK,
            [PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID_ENV]: txIdHex,
          },
          rawLogPath: firstLog,
          maxRestarts: 0,
          timeoutMs,
          terminateOnOutput: { marker: checkpointMarker, signal: "SIGKILL" },
          terminateOnFile: { path: firstStopFile, signal: "SIGKILL" },
        });
        await Promise.race([
          waitForReady(endpoint, timeoutMs),
          firstProcess.then((summary) => {
            throw new Error(
              `First node exited before readiness: ${JSON.stringify(summary)}`,
            );
          }),
        ]);
        const submitResponse = await fetch(`${endpoint}/submit`, {
          method: "POST",
          headers: {
            "content-type": "application/vnd.midgard.v1+cbor",
          },
          body: new Uint8Array(
            encodeMidgardProofSubmissionV1({
              transactionCbor: txCanonicalCbor,
              programMaterial: [],
            }),
          ),
          signal: AbortSignal.timeout(10_000),
        });
        const submitBody = (await submitResponse.json()) as {
          readonly txId?: string;
          readonly duplicate?: boolean;
        };
        expect(submitResponse.status, JSON.stringify(submitBody)).toBe(202);
        expect(submitBody).toMatchObject({
          txId: txIdHex,
          duplicate: false,
        });

        const firstSummary = await firstProcess;
        expect(firstSummary.status).toBe("restart_budget_exhausted");
        expect(firstSummary.attempts[0]?.signal).toBe("SIGKILL");
        expect(firstSummary.attempts[0]?.outputTermination).toMatchObject({
          marker: checkpointMarker,
          signal: "SIGKILL",
        });
        firstProcess = undefined;

        expect(
          (await runDatabase(TxAdmissionsDB.getByTxId(txId)))?.status,
        ).toBe(TxAdmissionsDB.Status.Accepted);
        expect(await runDatabase(MempoolDB.retrieveTxCount)).toBe(1n);
        expect(
          (await runDatabase(MempoolTxDeltasDB.retrieveByTxIds([txId]))).size,
        ).toBe(0);
        for (const address of producedAddresses) {
          expect(await runDatabase(AddressHistoryDB.retrieve(address))).toEqual(
            [],
          );
        }

        restartProcess = superviseHostProcess({
          service: "phase1-exact-restart-commit",
          command: process.execPath,
          args: [distIndex, "listen", "--with-monitoring"],
          cwd,
          envInheritance: "process",
          rawLogPath: restartLog,
          maxRestarts: 0,
          timeoutMs,
          terminateOnFile: { path: restartStopFile, signal: "SIGTERM" },
        });
        await Promise.race([
          waitForReady(endpoint, timeoutMs),
          restartProcess.then((summary) => {
            throw new Error(
              `Restarted node exited before readiness: ${JSON.stringify(summary)}`,
            );
          }),
        ]);
        await waitFor(
          "exact tx submitted pending-finalization membership",
          timeoutMs,
          async () => ((await hasExactCommittedJournal()) ? true : null),
        );
        const metricsResponse = await fetch(metricsEndpoint, {
          signal: AbortSignal.timeout(5_000),
        });
        expect(metricsResponse.ok).toBe(true);
        const metricsText = await metricsResponse.text();
        const cacheHits = metricValue(
          metricsText,
          "commit_tx_delta_cache_hit_total",
        );
        const fallbackDecoded = metricValue(
          metricsText,
          "commit_tx_delta_fallback_decoded_total",
        );
        expect(cacheHits).toBe(0n);
        expect(fallbackDecoded).not.toBeNull();
        expect(fallbackDecoded).toBeGreaterThanOrEqual(1n);

        await writeFile(restartStopFile, "stop\n");
        const restartSummary = await restartProcess;
        expect(restartSummary.status).toBe("restart_budget_exhausted");
        expect(restartSummary.attempts[0]?.signal).toBe("SIGTERM");
        restartProcess = undefined;

        const restartOutput = await readFile(restartLog, "utf8");
        const matchingResolutionLines = restartOutput
          .split(/\r?\n/u)
          .filter(
            (line) =>
              line.includes("Commit MPF phase tx_delta_resolution completed") &&
              line.includes("candidate_tx_count=1") &&
              line.includes("decoded_tx_count=1") &&
              line.includes("cache_hit_tx_count=0") &&
              line.includes("fallback_decoded_tx_count=1") &&
              line.includes("rejected_tx_count=0"),
          );
        expect(matchingResolutionLines.length).toBe(Number(fallbackDecoded));
        expect(await hasExactCommittedJournal()).toBe(true);
        expect(
          (await runDatabase(MempoolTxDeltasDB.retrieveByTxIds([txId]))).size,
        ).toBe(0);
        for (const address of producedAddresses) {
          expect(await runDatabase(AddressHistoryDB.retrieve(address))).toEqual(
            [],
          );
        }
      } finally {
        if (firstProcess !== undefined) {
          await writeFile(firstStopFile, "stop\n");
          await firstProcess;
        }
        if (restartProcess !== undefined) {
          await writeFile(restartStopFile, "stop\n");
          await restartProcess;
        }
        await databaseRuntime.dispose();
      }
    },
    360_000,
  );
});

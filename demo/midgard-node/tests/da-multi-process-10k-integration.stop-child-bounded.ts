import { type ChildProcess } from "node:child_process";
import { readFile } from "node:fs/promises";
import { createServer } from "node:net";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";

import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import { DaPayloadsDB } from "../src/database/index.js";

export const packageRoot = resolve(
  fileURLToPath(new URL("..", import.meta.url)),
);

export const DEPLOYMENT = "b6".repeat(32);

export const TRANSACTION_COUNT = 10_000;

export const CHILD_COUNT = 3;

export const THRESHOLD = 2;

type PeerMetric = {
  readonly peerIndex: number;
  readonly pid: number;
  readonly outcome: string;
  readonly durationMs: number;
  readonly rssBeforeBytes: number;
  readonly rssAfterBytes: number;
  readonly peakRssBytes: number;
  readonly admissionPeakActive: number;
};

export const insertFromFixture = (
  fixture: Awaited<ReturnType<typeof makePayloadFixture>>,
  envelope: Buffer,
): DaPayloadsDB.InsertInput => ({
  [DaPayloadsDB.Columns.HEADER_HASH]: Buffer.from(fixture.headerHash, "hex"),
  [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
  [DaPayloadsDB.Columns.VERSION]: 1,
  [DaPayloadsDB.Columns.PAYLOAD_CBOR]: envelope,
  [DaPayloadsDB.Columns.PAYLOAD_SHA256]: computeDaSha256Hash(envelope),
  [DaPayloadsDB.Columns.UTXOS_ROOT]: fixture.header.utxosRoot,
  [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
    fixture.header.forcedTransactionsRoot,
  [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: fixture.header.transactionsRoot,
  [DaPayloadsDB.Columns.DEPOSITS_ROOT]: fixture.header.depositsRoot,
  [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: fixture.header.withdrawalsRoot,
  [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
    fixture.header.transitionTraceRoot,
  [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: fixture.header.eventToStepRoot,
  [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]:
    fixture.header.validationTracesRoot,
  [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: fixture.header.withdrawalCount,
  [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
    fixture.header.forcedTransactionCount,
  [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]:
    fixture.header.l2TransactionCount,
  [DaPayloadsDB.Columns.DEPOSIT_COUNT]: fixture.header.depositCount,
  [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: fixture.header.totalEventCount,
  [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]:
    fixture.header.transitionStepCount,
  [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]:
    fixture.header.validationTraceCount,
  [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(1),
  [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(2),
});

const readPeerMetrics = async (
  temp: string,
  peerIndex: number,
): Promise<readonly PeerMetric[]> =>
  (await readFile(join(temp, `metrics-${peerIndex.toString()}.ndjson`), "utf8"))
    .trim()
    .split("\n")
    .filter((line) => line.length > 0)
    .map((line) => JSON.parse(line) as PeerMetric);

export const waitForPeerMetrics = async (
  temp: string,
  expectedMetricsPerPeer: number,
  timeoutMs: number,
): Promise<readonly (readonly PeerMetric[])[]> => {
  const deadline = performance.now() + timeoutMs;
  let metricsByPeer: readonly (readonly PeerMetric[])[] = [];
  while (performance.now() < deadline) {
    metricsByPeer = await Promise.all(
      Array.from({ length: CHILD_COUNT }, (_, index) =>
        readPeerMetrics(temp, index).catch(() => []),
      ),
    );
    if (
      metricsByPeer.every((metrics) => metrics.length >= expectedMetricsPerPeer)
    ) {
      return metricsByPeer;
    }
    await new Promise<void>((resolveDelay) => setTimeout(resolveDelay, 250));
  }
  return metricsByPeer;
};

export const waitForReady = (child: ChildProcess): Promise<void> =>
  new Promise((resolveReady, reject) => {
    let stdout = "";
    let stderr = "";
    const timeout = setTimeout(() => {
      reject(new Error(`committee peer readiness timed out: ${stderr}`));
    }, 15_000);
    child.stdout?.on("data", (chunk) => {
      stdout += String(chunk);
      if (stdout.includes('"ready":true')) {
        clearTimeout(timeout);
        resolveReady();
      }
    });
    child.stderr?.on("data", (chunk) => {
      stderr += String(chunk);
    });
    child.once("exit", (code) => {
      clearTimeout(timeout);
      reject(
        new Error(
          `committee peer exited before readiness code=${String(code)} stderr=${stderr}`,
        ),
      );
    });
  });

export const stopChildBounded = async (child: ChildProcess): Promise<void> => {
  if (child.exitCode !== null) return;
  await new Promise<void>((resolveStop) => {
    let settled = false;
    // Assigned after finish is defined so the closure can clear both timers.

    // eslint-disable-next-line prefer-const
    let killTimer: NodeJS.Timeout | undefined;

    // eslint-disable-next-line prefer-const
    let finishTimer: NodeJS.Timeout | undefined;
    const finish = () => {
      if (settled) return;
      settled = true;
      if (killTimer !== undefined) clearTimeout(killTimer);
      if (finishTimer !== undefined) clearTimeout(finishTimer);
      resolveStop();
    };
    child.once("exit", finish);
    if (child.exitCode !== null) {
      finish();
      return;
    }
    try {
      child.kill("SIGTERM");
    } catch {
      finish();
      return;
    }
    killTimer = setTimeout(() => {
      if (child.exitCode !== null) return;
      try {
        child.kill("SIGKILL");
      } catch {
        finish();
      }
    }, 5_000);
    killTimer.unref();
    finishTimer = setTimeout(finish, 10_000);
    finishTimer.unref();
  });
};

export const reserveLoopbackPort = (): Promise<number> =>
  new Promise((resolvePort, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (address === null || typeof address === "string") {
        reject(new Error("failed to reserve loopback port"));
        return;
      }
      const port = address.port;
      server.close((error) =>
        error === undefined ? resolvePort(port) : reject(error),
      );
    });
  });

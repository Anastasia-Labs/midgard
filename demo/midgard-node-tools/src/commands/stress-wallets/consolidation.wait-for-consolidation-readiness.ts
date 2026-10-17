import { writeFile } from "node:fs/promises";

import { formatJson } from "midgard-node/commands/command-utils";
import { sleep } from "midgard-node/sleep";

import { parseStressWalletConsolidationReadinessEvidence } from "./artifacts.js";
import { STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION } from "./constants.js";
import {
  type ConsolidationReadinessSnapshot,
  defaultFetchConsolidationReadiness,
  isFullConsolidationReadiness,
  parseConsolidationReadiness,
} from "./readiness.js";
import { nextFanoutPollDelayMs } from "./runtime.js";
import { type StressWalletConsolidateRuntime } from "./types.js";

export const waitForConsolidationReadiness = async ({
  nodeEndpoint,
  runtime,
  readinessPath,
  batchIndex,
  firstWalletId,
  timeoutMs,
  requestTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
  now,
}: {
  readonly nodeEndpoint: string;
  readonly runtime: StressWalletConsolidateRuntime;
  readonly readinessPath: string;
  readonly batchIndex: number;
  readonly firstWalletId: string;
  readonly timeoutMs: number;
  readonly requestTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
  readonly now: () => Date;
}): Promise<void> => {
  const fetchReadiness =
    runtime.fetchReadiness ??
    ((endpoint: string) =>
      defaultFetchConsolidationReadiness(endpoint, requestTimeoutMs));
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const response = await fetchReadiness(nodeEndpoint);
    let snapshot: ConsolidationReadinessSnapshot;
    try {
      snapshot = parseConsolidationReadiness(response);
    } catch (error) {
      const evidence = {
        schemaVersion: STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
        observedAt: now().toISOString(),
        batchIndex,
        firstWalletId,
        attempt,
        malformed: true,
        error: error instanceof Error ? error.message : String(error),
        response,
      };
      parseStressWalletConsolidationReadinessEvidence(evidence);
      await writeFile(readinessPath, JSON.stringify(evidence) + "\n", {
        encoding: "utf8",
        flag: "a",
        mode: 0o600,
      });
      throw error;
    }
    const fullReady = isFullConsolidationReadiness(snapshot);
    const evidence = {
      schemaVersion: STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
      observedAt: now().toISOString(),
      batchIndex,
      firstWalletId,
      attempt,
      fullReady,
      snapshot,
    };
    parseStressWalletConsolidationReadinessEvidence(evidence);
    await writeFile(readinessPath, JSON.stringify(evidence) + "\n", {
      encoding: "utf8",
      flag: "a",
      mode: 0o600,
    });
    if (fullReady) return;
    if (monotonicNow() - startedAt >= timeoutMs) {
      throw new Error(
        "Timed out waiting " +
          timeoutMs.toString() +
          "ms for full consolidation readiness before batch " +
          batchIndex.toString() +
          " (first wallet " +
          firstWalletId +
          "); last snapshot " +
          formatJson(snapshot) +
          ".",
      );
    }
    await sleepImpl(
      nextFanoutPollDelayMs({
        attempt,
        initialMs: pollInitialIntervalMs,
        maxMs: pollMaxIntervalMs,
      }),
    );
    attempt += 1;
  }
};

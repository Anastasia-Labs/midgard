#!/usr/bin/env node

import "node:fs";
import "node:path";
import "node:url";
import "@al-ft/midgard-core/cek-proof";
import "./native-tx-workload-utils.mjs";
import "./throughput-nominal-activity.parse-args.mjs";

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";

import {
  buildNativeSignedOneToOne,
  decodeCoin,
  makeWalletsFromEnv,
  parseEnv,
} from "./native-tx-workload-utils.mjs";
import {
  boolFrom,
  extractCounter,
  numberFrom,
  parseArgs,
  parseDurationMs,
  sleep,
  usage,
} from "./throughput-nominal-activity.parse-args.mjs";

const __filename = fileURLToPath(import.meta.url);

const __dirname = path.dirname(__filename);

const pkgRoot = path.resolve(__dirname, "..");

const args = parseArgs(process.argv.slice(2));

if (args.help === "true") {
  usage();
  process.exit(0);
}

const envPath =
  args["env-file"] ??
  process.env.ACTIVITY_ENV_FILE ??
  path.join(pkgRoot, ".env");

const submitEndpoint =
  args["submit-endpoint"] ??
  process.env.ACTIVITY_SUBMIT_ENDPOINT ??
  process.env.STRESS_SUBMIT_ENDPOINT ??
  "http://127.0.0.1:3000";

const metricsEndpoint =
  args["metrics-endpoint"] ??
  process.env.ACTIVITY_METRICS_ENDPOINT ??
  process.env.STRESS_METRICS_ENDPOINT ??
  "http://127.0.0.1:9464/metrics";

const durationMs = parseDurationMs(
  args.duration ??
    process.env.ACTIVITY_DURATION ??
    process.env.ACTIVITY_DURATION_SEC ??
    "10m",
);

const targetTxs = numberFrom(
  args["target-txs"] ?? process.env.ACTIVITY_TARGET_TXS,
  100,
  "target txs",
);

const minIntervalMs = numberFrom(
  args["min-interval-ms"] ?? process.env.ACTIVITY_MIN_INTERVAL_MS,
  750,
  "min interval",
);

const maxIntervalMs = numberFrom(
  args["max-interval-ms"] ?? process.env.ACTIVITY_MAX_INTERVAL_MS,
  7000,
  "max interval",
);

const walletModeRaw = (
  args["wallet-mode"] ??
  process.env.ACTIVITY_WALLET_MODE ??
  "random"
)
  .trim()
  .toLowerCase();

const walletMode = walletModeRaw === "round_robin" ? "round_robin" : "random";

const metricsPollMs = numberFrom(
  args["metrics-poll-ms"] ?? process.env.ACTIVITY_METRICS_POLL_MS,
  1000,
  "metrics poll",
);

const minLovelace = BigInt(
  process.env.ACTIVITY_MIN_LOVELACE ?? process.env.STRESS_MIN_LOVELACE ?? "0",
);

const retry503 = numberFrom(process.env.ACTIVITY_RETRY_503, 3, "retry503");

const retryDelayMs = numberFrom(
  process.env.ACTIVITY_RETRY_DELAY_MS,
  50,
  "retry delay",
);

const logEverySuccess = boolFrom(process.env.ACTIVITY_LOG_EVERY_SUCCESS, false);

const inFlightTtlMs = numberFrom(
  process.env.ACTIVITY_INFLIGHT_TTL_MS,
  45_000,
  "inflight ttl",
);

if (!fs.existsSync(envPath)) {
  throw new Error(`Env file does not exist: ${envPath}`);
}

if (durationMs <= 0) {
  throw new Error("duration must be positive");
}

if (!Number.isFinite(targetTxs) || targetTxs <= 0) {
  throw new Error("target txs must be a positive integer");
}

if (!Number.isFinite(minIntervalMs) || minIntervalMs < 0) {
  throw new Error("min interval must be >= 0");
}

if (!Number.isFinite(maxIntervalMs) || maxIntervalMs <= 0) {
  throw new Error("max interval must be > 0");
}

if (minIntervalMs > maxIntervalMs) {
  throw new Error("min interval cannot be greater than max interval");
}

/**
 * Fetches raw Prometheus metrics text from the node.
 */
const fetchMetricsText = async () => {
  const resp = await fetch(metricsEndpoint);
  if (!resp.ok) {
    throw new Error(`metrics endpoint returned ${resp.status}`);
  }
  return resp.text();
};

/**
 * Fetches and parses the counters used by the workload monitor.
 */
const readCounters = async () => {
  const text = await fetchMetricsText();
  return {
    submit: extractCounter(text, ["tx_count_total", "tx_count"]),
    accept: extractCounter(text, [
      "validation_accept_count_total",
      "validation_accept_count",
    ]),
    reject: extractCounter(text, [
      "validation_reject_count_total",
      "validation_reject_count",
    ]),
  };
};

/**
 * Fetches spendable UTxOs for a wallet address.
 */
const fetchUtxos = async (address) => {
  const resp = await fetch(
    `${submitEndpoint}/utxos?address=${encodeURIComponent(address)}`,
  );
  if (!resp.ok) {
    throw new Error(`utxos endpoint returned ${resp.status} for ${address}`);
  }
  const body = await resp.json();
  if (!Array.isArray(body.utxos)) {
    return [];
  }
  return /** @type {NodeUtxo[]} */ (body.utxos);
};

/**
 * Submits a CBOR transaction hex payload to the node.
 */
const submitTxHex = async (txHex) => {
  let attempt = 0;
  while (attempt <= retry503) {
    const body = encodeMidgardProofSubmission({
      transactionCbor: Buffer.from(txHex, "hex"),
      programMaterial: [],
    });
    const resp = await fetch(`${submitEndpoint}/submit`, {
      method: "POST",
      headers: { "content-type": "application/vnd.midgard.v1+cbor" },
      body,
    });

    if (resp.ok) {
      return { ok: true, status: resp.status };
    }

    const responseBody = await resp.text();
    if ((resp.status === 503 || resp.status === 429) && attempt < retry503) {
      attempt += 1;
      await sleep(retryDelayMs);
      continue;
    }

    return {
      ok: false,
      status: resp.status,
      body: responseBody,
    };
  }

  return { ok: false, status: 0, body: "retry loop exhausted" };
};

/**
 * Returns a randomized interval within the configured bounds.
 */
const randomIntervalMs = () => {
  if (minIntervalMs === maxIntervalMs) {
    return minIntervalMs;
  }
  const span = maxIntervalMs - minIntervalMs;
  return minIntervalMs + Math.floor(Math.random() * (span + 1));
};

/**
 * Selects the next wallet to use for a workload iteration.
 */
const chooseWallet = (wallets, sequence) => {
  if (walletMode === "round_robin") {
    return wallets[sequence % wallets.length];
  }
  return wallets[Math.floor(Math.random() * wallets.length)];
};

/**
 * Runs the nominal-activity throughput workload.
 */
const main = async () => {
  const env = parseEnv(envPath);
  const wallets = makeWalletsFromEnv(env);
  if (wallets.length === 0) {
    throw new Error(`No genesis wallet seeds found in ${envPath}`);
  }

  console.log("Starting nominal sustained activity test with config:");
  console.log(
    JSON.stringify(
      {
        envPath,
        submitEndpoint,
        metricsEndpoint,
        durationMs,
        targetTxs,
        minIntervalMs,
        maxIntervalMs,
        walletMode,
        metricsPollMs,
        minLovelace: minLovelace.toString(),
        retry503,
        retryDelayMs,
        inFlightTtlMs,
      },
      null,
      2,
    ),
  );

  const startCounters = await readCounters();
  const startedAtMs = Date.now();
  const deadlineMs = startedAtMs + durationMs;

  const inFlightSpentOutRefs = new Map();
  let successCount = 0;
  let errorCount = 0;
  let attempts = 0;
  let noUtxoCount = 0;
  let walletCursor = 0;
  /** @type {Record<string, number>} */
  const statusCounts = {};
  /** @type {string[]} */
  const firstErrors = [];

  let maxAcceptRate1s = 0;
  let maxRejectRate1s = 0;
  let maxSubmitRate1s = 0;
  let doneProducing = false;

  /**
   * Monitors workload progress and emits periodic metrics.
   */
  const monitorPromise = (async () => {
    let prev = await readCounters();
    let prevTs = Date.now();

    while (true) {
      await sleep(metricsPollMs);
      const now = await readCounters();
      const nowTs = Date.now();
      const dt = Math.max((nowTs - prevTs) / 1000, 0.001);

      const submitRate = (now.submit - prev.submit) / dt;
      const acceptRate = (now.accept - prev.accept) / dt;
      const rejectRate = (now.reject - prev.reject) / dt;

      if (submitRate > maxSubmitRate1s) maxSubmitRate1s = submitRate;
      if (acceptRate > maxAcceptRate1s) maxAcceptRate1s = acceptRate;
      if (rejectRate > maxRejectRate1s) maxRejectRate1s = rejectRate;

      console.log(
        `rate_submit=${submitRate.toFixed(2)} rate_accept=${acceptRate.toFixed(2)} rate_reject=${rejectRate.toFixed(2)} totals={submit:${now.submit},accept:${now.accept},reject:${now.reject}}`,
      );

      prev = now;
      prevTs = nowTs;

      if (doneProducing && nowTs - deadlineMs >= 10_000) {
        break;
      }
    }
  })();

  while (Date.now() < deadlineMs && successCount < targetTxs) {
    attempts += 1;
    const now = Date.now();
    for (const [outRefHex, seenAt] of inFlightSpentOutRefs.entries()) {
      if (now - seenAt > inFlightTtlMs) {
        inFlightSpentOutRefs.delete(outRefHex);
      }
    }

    const wallet = chooseWallet(wallets, walletCursor);
    walletCursor += 1;

    let utxos = [];
    try {
      utxos = await fetchUtxos(wallet.address);
    } catch (error) {
      errorCount += 1;
      if (firstErrors.length < 10) {
        firstErrors.push(
          `wallet=${wallet.key} fetch-utxos error=${error instanceof Error ? error.message : String(error)}`,
        );
      }
      await sleep(randomIntervalMs());
      continue;
    }

    const candidates = [];
    for (const utxo of utxos) {
      const outRefHex = utxo.outref.toLowerCase();
      if (inFlightSpentOutRefs.has(outRefHex)) {
        continue;
      }
      try {
        const coin = decodeCoin(utxo.value);
        if (coin < minLovelace) {
          continue;
        }
        candidates.push({
          outRefHex,
          spendOutRefCbor: Buffer.from(utxo.outref, "hex"),
          outputCbor: Buffer.from(utxo.value, "hex"),
        });
      } catch {
        // Ignore malformed UTxO encodings.
      }
    }

    if (candidates.length === 0) {
      noUtxoCount += 1;
      await sleep(randomIntervalMs());
      continue;
    }

    const selected = candidates[Math.floor(Math.random() * candidates.length)];
    const tx = buildNativeSignedOneToOne({
      spendOutRefCbor: selected.spendOutRefCbor,
      outputCbor: selected.outputCbor,
      signer: wallet.signer,
    });

    const submitResult = await submitTxHex(tx.txHex);
    const statusKey = String(submitResult.status);
    statusCounts[statusKey] = (statusCounts[statusKey] ?? 0) + 1;

    if (submitResult.ok) {
      successCount += 1;
      inFlightSpentOutRefs.set(selected.outRefHex, Date.now());
      if (logEverySuccess) {
        console.log(
          `submitted tx=${tx.txId.toString("hex")} wallet=${wallet.key} success_count=${successCount}`,
        );
      }
    } else {
      errorCount += 1;
      if (firstErrors.length < 10) {
        firstErrors.push(
          `wallet=${wallet.key} submit status=${submitResult.status} body=${submitResult.body ?? ""}`,
        );
      }
    }

    await sleep(randomIntervalMs());
  }

  doneProducing = true;
  await monitorPromise;

  const endCounters = await readCounters();
  const elapsedSec = Math.max((Date.now() - startedAtMs) / 1000, 0.001);
  const summary = {
    durationRequestedSec: durationMs / 1000,
    durationActualSec: elapsedSec,
    targetTxs,
    attempts,
    submitted: successCount,
    submitErrors: errorCount,
    noUtxoCount,
    submitStatusCounts: statusCounts,
    submitDelta: endCounters.submit - startCounters.submit,
    acceptDelta: endCounters.accept - startCounters.accept,
    rejectDelta: endCounters.reject - startCounters.reject,
    avgSubmittedTps: (endCounters.submit - startCounters.submit) / elapsedSec,
    avgAcceptedTps: (endCounters.accept - startCounters.accept) / elapsedSec,
    maxSubmitRate1s,
    maxAcceptRate1s,
    maxRejectRate1s,
    firstErrors,
    endedBy:
      successCount >= targetTxs ? "target_txs_reached" : "duration_elapsed",
  };

  console.log("Nominal activity summary:");
  console.log(JSON.stringify(summary, null, 2));

  if (successCount <= 0 && firstErrors.length > 0) {
    process.exitCode = 1;
  }
};

main().catch((error) => {
  console.error("throughput-nominal-activity failed:", error);
  process.exitCode = 1;
});

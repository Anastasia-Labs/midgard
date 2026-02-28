#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { encode as cborEncode } from "cborg";
import dotenv from "dotenv";
import { blake2b } from "@noble/hashes/blake2.js";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

const MIDGARD_NATIVE_TX_VERSION = 1n;
const MIDGARD_POSIX_TIME_NONE = -1n;
const MIDGARD_NETWORK_ID_PREPROD = 0n;
const TX_IS_VALID_CODE = 0n;
const HASH32_LEN = 32;

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const pkgRoot = path.resolve(__dirname, "..");

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const encodeCbor = (value) => Buffer.from(cborEncode(value));
const hash32 = (value) => Buffer.from(blake2b(value, { dkLen: HASH32_LEN }));

const encodeByteListPreimage = (items) =>
  encodeCbor(items.map((item) => Buffer.from(item)));

const toOutRefCbor = (txId, outputIndex) =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(txId),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  );

const parseDurationMs = (value) => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error("duration must be a non-empty string");
  }
  const trimmed = value.trim().toLowerCase();
  const match = /^(\d+)(ms|s|m|h)?$/.exec(trimmed);
  if (match === null) {
    throw new Error(`Invalid duration value: ${value}`);
  }
  const amount = Number.parseInt(match[1], 10);
  const unit = match[2] ?? "s";
  if (!Number.isFinite(amount) || amount <= 0) {
    throw new Error(`Duration must be a positive integer: ${value}`);
  }
  switch (unit) {
    case "ms":
      return amount;
    case "s":
      return amount * 1000;
    case "m":
      return amount * 60_000;
    case "h":
      return amount * 3_600_000;
    default:
      throw new Error(`Unsupported duration unit: ${unit}`);
  }
};

const parseArgs = (argv) => {
  const out = {};
  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (!token.startsWith("--")) {
      continue;
    }
    if (token === "--help" || token === "-h") {
      out.help = "true";
      continue;
    }
    const eq = token.indexOf("=");
    if (eq >= 0) {
      const key = token.slice(2, eq);
      const value = token.slice(eq + 1);
      out[key] = value;
      continue;
    }
    const key = token.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) {
      out[key] = next;
      i += 1;
    } else {
      out[key] = "true";
    }
  }
  return out;
};

const boolFrom = (value, defaultValue) => {
  if (value === undefined) return defaultValue;
  const normalized = String(value).trim().toLowerCase();
  return normalized !== "false" && normalized !== "0" && normalized !== "no";
};

const numberFrom = (value, fallback, name) => {
  if (value === undefined) return fallback;
  const parsed = Number.parseInt(String(value), 10);
  if (!Number.isFinite(parsed)) {
    throw new Error(`${name} must be a valid integer`);
  }
  return parsed;
};

const usage = () => {
  console.log(`Nominal Midgard activity generator

Usage:
  node scripts/throughput-nominal-activity.mjs [options]

Options:
  --duration <value>         Total run duration (e.g. 300s, 5m, 1h)
  --target-txs <n>           Max successful submits before stopping
  --min-interval-ms <n>      Minimum wait between submissions
  --max-interval-ms <n>      Maximum wait between submissions
  --submit-endpoint <url>    Midgard node HTTP endpoint
  --metrics-endpoint <url>   Prometheus metrics endpoint
  --env-file <path>          Env file with genesis seed phrases
  --wallet-mode <mode>       random (default) or round_robin
  --metrics-poll-ms <n>      Metrics poll interval
  --help                     Show this message
`);
};

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

/** @typedef {{ outref: string; value: string }} NodeUtxo */

const parseEnv = (filename) => {
  const raw = fs.readFileSync(filename, "utf8");
  return dotenv.parse(raw);
};

const makeWalletsFromEnv = (env) => {
  const keys = [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ];
  return keys
    .map((key) => {
      const seed = env[key];
      if (!seed || seed.trim().length === 0) {
        return null;
      }
      const wallet = walletFromSeed(seed.trim(), { network: "Preprod" });
      return {
        key,
        seed: seed.trim(),
        address: wallet.address,
        signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
      };
    })
    .filter((wallet) => wallet !== null);
};

const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

const extractCounter = (text, names) => {
  for (const name of names) {
    const pattern = `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`;
    const m = text.match(new RegExp(pattern, "m"));
    if (m !== null) {
      return Number(m[1]);
    }
  }
  return 0;
};

const fetchMetricsText = async () => {
  const resp = await fetch(metricsEndpoint);
  if (!resp.ok) {
    throw new Error(`metrics endpoint returned ${resp.status}`);
  }
  return resp.text();
};

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

const decodeCoin = (outputHex) => {
  const output = CML.TransactionOutput.from_cbor_bytes(
    Buffer.from(outputHex, "hex"),
  );
  return output.amount().coin();
};

const buildNativeSignedOneToOne = ({ spendOutRefCbor, outputCbor, signer }) => {
  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage([outputCbor]);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptIntegrityHash = hash32(EMPTY_CBOR_NULL);
  const auxiliaryDataHash = hash32(EMPTY_CBOR_NULL);

  const bodyCompact = [
    hash32(spendInputsPreimageCbor),
    hash32(referenceInputsPreimageCbor),
    hash32(outputsPreimageCbor),
    0n,
    MIDGARD_POSIX_TIME_NONE,
    MIDGARD_POSIX_TIME_NONE,
    hash32(requiredObserversPreimageCbor),
    hash32(requiredSignersPreimageCbor),
    hash32(mintPreimageCbor),
    scriptIntegrityHash,
    auxiliaryDataHash,
    MIDGARD_NETWORK_ID_PREPROD,
  ];

  const bodyHash = hash32(encodeCbor(bodyCompact));
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );

  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST;

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
  ];

  const compact = [
    MIDGARD_NATIVE_TX_VERSION,
    bodyHash,
    hash32(encodeCbor(witnessCompact)),
    TX_IS_VALID_CODE,
  ];

  const bodyFull = [
    bodyCompact[0],
    spendInputsPreimageCbor,
    bodyCompact[1],
    referenceInputsPreimageCbor,
    bodyCompact[2],
    outputsPreimageCbor,
    bodyCompact[3],
    bodyCompact[4],
    bodyCompact[5],
    bodyCompact[6],
    requiredObserversPreimageCbor,
    bodyCompact[7],
    requiredSignersPreimageCbor,
    bodyCompact[8],
    mintPreimageCbor,
    bodyCompact[9],
    bodyCompact[10],
    bodyCompact[11],
  ];

  const witnessFull = [
    witnessCompact[0],
    addrTxWitsPreimageCbor,
    witnessCompact[1],
    scriptTxWitsPreimageCbor,
    witnessCompact[2],
    redeemerTxWitsPreimageCbor,
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  const txId = hash32(encodeCbor(compact));

  return {
    txId,
    txHex: txCbor.toString("hex"),
    nextOutRef: toOutRefCbor(txId, 0),
  };
};

const submitTxHex = async (txHex) => {
  let attempt = 0;
  while (attempt <= retry503) {
    const resp = await fetch(`${submitEndpoint}/submit`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ tx_cbor: txHex }),
    });

    if (resp.ok) {
      return { ok: true, status: resp.status };
    }

    const body = await resp.text();
    if ((resp.status === 503 || resp.status === 429) && attempt < retry503) {
      attempt += 1;
      await sleep(retryDelayMs);
      continue;
    }

    return {
      ok: false,
      status: resp.status,
      body,
    };
  }

  return { ok: false, status: 0, body: "retry loop exhausted" };
};

const randomIntervalMs = () => {
  if (minIntervalMs === maxIntervalMs) {
    return minIntervalMs;
  }
  const span = maxIntervalMs - minIntervalMs;
  return minIntervalMs + Math.floor(Math.random() * (span + 1));
};

const chooseWallet = (wallets, sequence) => {
  if (walletMode === "round_robin") {
    return wallets[sequence % wallets.length];
  }
  return wallets[Math.floor(Math.random() * wallets.length)];
};

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

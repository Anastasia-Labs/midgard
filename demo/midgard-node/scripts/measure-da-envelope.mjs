#!/usr/bin/env node
import { createHash } from "node:crypto";
import { createReadStream, readFileSync } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import { createInterface } from "node:readline";

import { wrapDaPayloadV3 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

const corpusPath =
  process.argv[2] ??
  "logs/phase-1-full-corpus-20260709T002743Z/corpus/corpus.ndjson";
const outputPath =
  process.argv[3] ?? "logs/phase-5-da-hardening-measurement.json";
const scales = (
  process.env.MIDGARD_DA_MEASUREMENT_SCALES ?? "10000,50000,100000"
)
  .split(",")
  .map((value) => Number.parseInt(value, 10));
const encoder = process.env.MIDGARD_DA_MEASUREMENT_ENCODER ?? "byte";
const traceStepsPerTx = Number.parseInt(
  process.env.MIDGARD_DA_MEASUREMENT_TRACE_STEPS_PER_TX ?? "4",
  10,
);
const artifactDir = process.env.MIDGARD_DA_MEASUREMENT_ARTIFACT_DIR;
const writeInnerArtifact =
  process.env.MIDGARD_DA_MEASUREMENT_WRITE_INNER !== "false";
if (
  scales.length === 0 ||
  !scales.every((value) => Number.isSafeInteger(value) && value > 0)
) {
  throw new Error(
    "MIDGARD_DA_MEASUREMENT_SCALES must contain positive integers",
  );
}
if (encoder !== "byte" && encoder !== "lucid") {
  throw new Error("MIDGARD_DA_MEASUREMENT_ENCODER must be byte or lucid");
}
if (!Number.isSafeInteger(traceStepsPerTx) || traceStepsPerTx <= 0) {
  throw new Error(
    "MIDGARD_DA_MEASUREMENT_TRACE_STEPS_PER_TX must be a positive integer",
  );
}
const maxRows = scales.at(-1);

const rows = [];
const corpusPrefixHash = createHash("sha256");
const input = createInterface({
  input: createReadStream(corpusPath, { encoding: "utf8" }),
  crlfDelay: Infinity,
});
for await (const line of input) {
  if (line.length === 0) continue;
  const parsed = JSON.parse(line);
  corpusPrefixHash.update(line).update("\n");
  rows.push({
    txHash: parsed.txHash,
    canonicalCborHex: parsed.canonicalCborHex,
  });
  if (rows.length >= maxRows) break;
}
if (rows.length !== maxRows) {
  throw new Error(`expected ${maxRows} corpus rows, found ${rows.length}`);
}
const corpusPrefixSha256 = corpusPrefixHash.digest("hex");

const fixedLengthHex = (seed, bytes) => {
  const needed = bytes * 2;
  return seed.repeat(Math.ceil(needed / seed.length)).slice(0, needed);
};

const sortEntries = (entries) =>
  entries.sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));

const header = {
  prevUtxosRoot: "00".repeat(32),
  utxosRoot: "01".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: "02".repeat(32),
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: "03".repeat(32),
  eventToStepRoot: "04".repeat(32),
  startTime: 1n,
  endTime: 2n,
  prevHeaderHash: "05".repeat(28),
  operatorVkey: "06".repeat(28),
  protocolVersion: 1n,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
};

const measurements = [];
const processStartedAt = performance.now();

const readProcessRssBytes = () => {
  try {
    const status = readFileSync("/proc/self/status", "utf8");
    const match = /^VmRSS:\s+(\d+)\s+kB$/m.exec(status);
    return match === null ? process.memoryUsage().rss : Number(match[1]) * 1024;
  } catch {
    return process.memoryUsage().rss;
  }
};

let peakObservedRssBytes = readProcessRssBytes();
let failure;
const checkpoint = async () => {
  peakObservedRssBytes = Math.max(peakObservedRssBytes, readProcessRssBytes());
  const report = {
    schemaVersion: "midgard-phase-5-da-measurement-v1",
    generatedAt: new Date().toISOString(),
    complete: measurements.length === scales.length && failure === undefined,
    encoder,
    scenario: traceStepsPerTx === 1 ? "operational" : "max-envelope",
    traceStepsPerTx,
    corpusPath,
    corpusRowsRead: rows.length,
    corpusPrefixSha256,
    pinnedMaxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    processWallDurationMs: performance.now() - processStartedAt,
    processRssBytes: readProcessRssBytes(),
    peakObservedRssBytes: Math.max(
      peakObservedRssBytes,
      process.resourceUsage().maxRSS * 1024,
    ),
    ...(failure === undefined ? {} : { failure }),
    model: `real canonical transaction CBOR; two 69-byte UTxOs, ${traceStepsPerTx.toString()} 117-byte transition step(s), and one 10-byte event-to-step entry per transaction, based on live pending-journal octet-length observations`,
    measurements,
  };
  await writeFile(outputPath, `${JSON.stringify(report, null, 2)}\n`);
  return report;
};

for (const txCount of scales) {
  const scaleStartedAt = performance.now();
  const selected = rows.slice(0, txCount);
  const transactions = sortEntries(
    selected.map((row) => [row.txHash, row.canonicalCborHex]),
  );
  const utxos = sortEntries(
    selected.flatMap((row) => [
      [`${row.txHash}00000000`, fixedLengthHex(row.canonicalCborHex, 69)],
      [
        `${row.txHash}00000001`,
        fixedLengthHex(row.canonicalCborHex.slice(2), 69),
      ],
    ]),
  );
  const transitionTrace = sortEntries(
    traceStepsPerTx === 1
      ? selected.map((row, index) => {
          const eventKey = {
            L2TransactionEventKey: { tx_id: row.txHash },
          };
          return [
            LucidData.to(BigInt(index), LucidData.Integer()),
            LucidData.to(
              {
                schema_version: 1n,
                step_index: BigInt(index),
                event_key: eventKey,
                phase: "L2Transaction",
                pre_utxos_root: "01".repeat(32),
                post_utxos_root: "01".repeat(32),
              },
              SDK.TransitionStepSchema,
            ),
          ];
        })
      : selected.flatMap((row) =>
          Array.from({ length: traceStepsPerTx }, (_, step) => [
            `${row.txHash.slice(0, 56)}${step.toString(16).padStart(8, "0")}`,
            fixedLengthHex(`${row.canonicalCborHex}${step.toString(16)}`, 117),
          ]),
        ),
  );
  const eventToStep = sortEntries(
    traceStepsPerTx === 1
      ? selected.map((row, index) => [
          LucidData.to(
            { L2TransactionEventKey: { tx_id: row.txHash } },
            SDK.EventKeySchema,
          ),
          LucidData.to(
            { step_index: BigInt(index), phase: "L2Transaction" },
            SDK.EventToStepValueSchema,
          ),
        ])
      : selected.map((row, index) => [
          row.txHash,
          index.toString(16).padStart(20, "0"),
        ]),
  );
  const payloadHeader = {
    ...header,
    l2TransactionCount: BigInt(txCount),
    totalEventCount: BigInt(txCount),
    transitionStepCount: BigInt(transitionTrace.length),
  };
  const payloadHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeader(payloadHeader),
  );
  const payload = {
    version: SDK.DA_PAYLOAD_V2_VERSION,
    block_body: {
      header_hash: payloadHeaderHash,
      header: payloadHeader,
      utxos,
      withdrawals: [],
      forced_transactions: [],
      transactions,
      deposits: [],
      transition_trace: transitionTrace,
      event_to_step: eventToStep,
      counts: {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: BigInt(txCount),
        depositCount: 0n,
        totalEventCount: BigInt(txCount),
        transitionStepCount: BigInt(transitionTrace.length),
      },
    },
  };
  const encodeStartedAt = performance.now();
  let inner;
  try {
    inner =
      encoder === "lucid"
        ? Buffer.from(LucidData.to(payload, SDK.DaPayloadV2), "hex")
        : SDK.encodeDaPayloadV2(payload);
  } catch (error) {
    failure = {
      txCount,
      stage: "encode",
      message: error instanceof Error ? error.message : String(error),
      processWallDurationMs: performance.now() - processStartedAt,
    };
    await checkpoint();
    throw error;
  }
  const encodeDurationMs = performance.now() - encodeStartedAt;
  const compressStartedAt = performance.now();
  const envelope = await wrapDaPayloadV3(inner, {
    mode: "zstd",
    zstdLevel: 3,
  });
  const compressDurationMs = performance.now() - compressStartedAt;
  measurements.push({
    txCount,
    headerHash: payloadHeaderHash,
    modeledEntries: {
      transactions: transactions.length,
      utxos: utxos.length,
      transitionTrace: transitionTrace.length,
      eventToStep: eventToStep.length,
    },
    observedValueByteModel: {
      transaction: "canonical corpus bytes",
      utxo: 69,
      transitionTrace: 117,
      eventToStep: 10,
    },
    uncompressedBytes: inner.length,
    envelopeBytes: envelope.length,
    innerSha256: createHash("sha256").update(inner).digest("hex"),
    envelopeSha256: createHash("sha256").update(envelope).digest("hex"),
    compressionRatio: inner.length / envelope.length,
    producerEgressAtEightPeersBytes: envelope.length * 8,
    encodeDurationMs,
    compressDurationMs,
    uncompressedFitsPinnedLimit:
      inner.length <= DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    envelopeFitsPinnedLimit:
      envelope.length <= DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    scaleWallDurationMs: performance.now() - scaleStartedAt,
    processRssBytes: readProcessRssBytes(),
  });
  if (artifactDir !== undefined && txCount === maxRows) {
    await mkdir(artifactDir, { recursive: true });
    await Promise.all([
      writeFile(`${artifactDir}/envelope-${txCount.toString()}.cbor`, envelope),
      ...(writeInnerArtifact
        ? [writeFile(`${artifactDir}/inner-${txCount.toString()}.cbor`, inner)]
        : []),
    ]);
  }
  await checkpoint();
}

const report = await checkpoint();
process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);

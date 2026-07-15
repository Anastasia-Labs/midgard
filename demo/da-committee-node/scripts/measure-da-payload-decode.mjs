#!/usr/bin/env node
import { readFile } from "node:fs/promises";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

const envelopePath = process.argv[2];
if (envelopePath === undefined) {
  throw new Error("usage: measure-da-payload-decode.mjs <envelope.cbor>");
}
const mode = process.env.MIDGARD_DA_DECODE_MODE ?? "strict";
if (mode !== "strict" && mode !== "unwrap") {
  throw new Error("MIDGARD_DA_DECODE_MODE must be strict or unwrap");
}
const limitMode = process.env.MIDGARD_DA_DECODE_LIMIT_MODE ?? "v1";
if (limitMode !== "v1" && limitMode !== "protocol-proposal") {
  throw new Error(
    "MIDGARD_DA_DECODE_LIMIT_MODE must be v1 or protocol-proposal",
  );
}
const maxInnerBytes =
  limitMode === "v1"
    ? DA_TRANSPORT_LIMITS_V1.maxPayloadBytes
    : 256 * 1024 * 1024;

const report = {
  schemaVersion: "midgard-phase-5-committee-decode-v1",
  envelopePath,
  mode,
  limitMode,
  deployableUnderV1: limitMode === "v1",
  maxInnerBytes,
  startedAt: new Date().toISOString(),
  baselineRssBytes: process.memoryUsage().rss,
};

const envelope = await readFile(envelopePath);
const unwrapStartedAt = performance.now();
const unwrapped = await unwrapDaPayload(envelope, {
  maxPayloadBytes: maxInnerBytes,
  schemaVersion: 3,
});
const unwrapDurationMs = performance.now() - unwrapStartedAt;
const rssAfterUnwrapBytes = process.memoryUsage().rss;

if (mode === "unwrap") {
  const result = {
    ...report,
    completedAt: new Date().toISOString(),
    envelopeBytes: envelope.length,
    innerBytes: unwrapped.innerBytes.length,
    unwrapDurationMs,
    rssAfterUnwrapBytes,
    peakRssBytes: process.resourceUsage().maxRSS * 1024,
  };
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
  process.exit(0);
}

const strictDecodeStartedAt = performance.now();
const payload = SDK.decodeDaPayloadV2(unwrapped.innerBytes);
const canonical = SDK.encodeDaPayloadV2(payload);
if (!canonical.equals(unwrapped.innerBytes)) {
  throw new Error("decoded payload did not re-encode canonically");
}
const strictDecodeDurationMs = performance.now() - strictDecodeStartedAt;

const result = {
  ...report,
  completedAt: new Date().toISOString(),
  envelopeBytes: envelope.length,
  innerBytes: unwrapped.innerBytes.length,
  unwrapDurationMs,
  strictDecodeDurationMs,
  totalDecodeDurationMs: unwrapDurationMs + strictDecodeDurationMs,
  transactionCount: payload.block_body.transactions.length,
  transitionTraceCount: payload.block_body.transition_trace.length,
  rssAfterUnwrapBytes,
  finalRssBytes: process.memoryUsage().rss,
  peakRssBytes: process.resourceUsage().maxRSS * 1024,
};
process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);

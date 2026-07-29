import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { isAbsolute, resolve } from "node:path";

export const NODE_SLOT_CONFIG_EVIDENCE_SCHEMA =
  "midgard-node-slot-config-evidence-v1";
export const OGMIOS_GENESIS_QUERY_TIMEOUT_MS = 5_000;
export const OGMIOS_GENESIS_RESPONSE_MAX_BYTES = 1_048_576;

const LUCID_VERSION = "0.6.0";
const STATIC_SLOT_CONFIGS = Object.freeze({
  Mainnet: Object.freeze({
    zeroTime: 1_596_059_091_000,
    zeroSlot: 4_492_800,
    slotLength: 1_000,
  }),
  Preview: Object.freeze({
    zeroTime: 1_666_656_000_000,
    zeroSlot: 0,
    slotLength: 1_000,
  }),
  Preprod: Object.freeze({
    zeroTime: 1_655_769_600_000,
    zeroSlot: 86_400,
    slotLength: 1_000,
  }),
});

const exactRecord = (value, keys, label) => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    JSON.stringify(Object.keys(value).sort()) !==
      JSON.stringify([...keys].sort())
  ) {
    throw new Error(`${label} must contain exactly: ${keys.join(", ")}`);
  }
  return value;
};

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const isHash = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);
const canonicalTimestamp = (value, label) => {
  if (
    typeof value !== "string" ||
    !/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/u.test(value) ||
    new Date(value).toISOString() !== value
  ) {
    throw new Error(`${label} must be a canonical UTC timestamp`);
  }
};
const validSlotConfig = (value, label) => {
  const slotConfig = exactRecord(
    value,
    ["zeroTime", "zeroSlot", "slotLength"],
    label,
  );
  if (
    !Number.isSafeInteger(slotConfig.zeroTime) ||
    slotConfig.zeroTime < 0 ||
    !Number.isSafeInteger(slotConfig.zeroSlot) ||
    slotConfig.zeroSlot < 0 ||
    !Number.isSafeInteger(slotConfig.slotLength) ||
    slotConfig.slotLength <= 0
  ) {
    throw new Error(`${label} contains an invalid value`);
  }
  return slotConfig;
};

const canonicalJsonValue = (value) => {
  if (Array.isArray(value)) return value.map(canonicalJsonValue);
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map((key) => [key, canonicalJsonValue(value[key])]),
    );
  }
  return value;
};

export const normalizeOgmiosEvidenceUrl = (url) => {
  const parsed = new URL(url.trim());
  if (parsed.protocol === "ws:") {
    parsed.protocol = "http:";
  } else if (parsed.protocol === "wss:") {
    parsed.protocol = "https:";
  }
  if (!["http:", "https:"].includes(parsed.protocol)) {
    throw new Error("Ogmios evidence URL must use HTTP(S) or WS(S)");
  }
  parsed.hash = "";
  return parsed.toString().replace(/\/$/, "");
};

const readBoundedResponseText = async (response, maxResponseBytes) => {
  const contentLength = response.headers.get("content-length");
  if (
    contentLength !== null &&
    (!/^\d+$/u.test(contentLength) || Number(contentLength) > maxResponseBytes)
  ) {
    throw new Error(
      `Ogmios genesis response exceeds ${maxResponseBytes.toString()} bytes`,
    );
  }
  if (response.body === null) return "";

  const reader = response.body.getReader();
  const chunks = [];
  let totalBytes = 0;
  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      totalBytes += value.byteLength;
      if (totalBytes > maxResponseBytes) {
        await reader.cancel();
        throw new Error(
          `Ogmios genesis response exceeds ${maxResponseBytes.toString()} bytes`,
        );
      }
      chunks.push(Buffer.from(value));
    }
  } finally {
    reader.releaseLock();
  }
  return Buffer.concat(chunks, totalBytes).toString("utf8");
};

export const fetchOgmiosGenesisPayloadV1 = async ({
  ogmiosUrl,
  fetchImpl = fetch,
  timeoutMs = OGMIOS_GENESIS_QUERY_TIMEOUT_MS,
  maxResponseBytes = OGMIOS_GENESIS_RESPONSE_MAX_BYTES,
}) => {
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) {
    throw new Error("Ogmios genesis timeout must be a positive integer");
  }
  if (!Number.isSafeInteger(maxResponseBytes) || maxResponseBytes <= 0) {
    throw new Error("Ogmios genesis response cap must be a positive integer");
  }
  const controller = new AbortController();
  const timeout = setTimeout(
    () =>
      controller.abort(
        new Error(
          `Ogmios genesis query timed out after ${timeoutMs.toString()} ms`,
        ),
      ),
    timeoutMs,
  );
  try {
    const response = await fetchImpl(normalizeOgmiosEvidenceUrl(ogmiosUrl), {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryNetwork/genesisConfiguration",
        params: { era: "shelley" },
        id: "midgard-node-slot-config-evidence",
      }),
      signal: controller.signal,
    });
    const body = await readBoundedResponseText(response, maxResponseBytes);
    if (!response.ok) {
      throw new Error(
        `Ogmios genesis query failed with HTTP ${response.status.toString()}`,
      );
    }
    try {
      return JSON.parse(body);
    } catch {
      throw new Error("Ogmios genesis query returned invalid JSON");
    }
  } catch (cause) {
    if (controller.signal.aborted) {
      throw controller.signal.reason instanceof Error
        ? controller.signal.reason
        : new Error(
            `Ogmios genesis query timed out after ${timeoutMs.toString()} ms`,
          );
    }
    if (
      cause instanceof Error &&
      (/^Ogmios genesis response exceeds \d+ bytes$/u.test(cause.message) ||
        /^Ogmios genesis query failed with HTTP \d+$/u.test(cause.message) ||
        cause.message === "Ogmios genesis query returned invalid JSON")
    ) {
      throw cause;
    }
    throw new Error("Ogmios genesis query transport failed");
  } finally {
    clearTimeout(timeout);
  }
};

export const deriveOgmiosGenesisSlotEvidenceV1 = ({ ogmiosUrl, payload }) => {
  const root = exactRecord(
    payload,
    ["jsonrpc", "result", "id"],
    "Ogmios response",
  );
  if (
    root.jsonrpc !== "2.0" ||
    root.id !== "midgard-node-slot-config-evidence"
  ) {
    throw new Error("Unexpected Ogmios genesis response identity");
  }
  if (
    root.result === null ||
    typeof root.result !== "object" ||
    Array.isArray(root.result)
  ) {
    throw new Error("Ogmios genesis response result must be an object");
  }
  const startTime = root.result.startTime;
  const slotLength = root.result.slotLength;
  const systemStartMs = Date.parse(startTime);
  const slotLengthMs = slotLength?.milliseconds;
  if (
    typeof startTime !== "string" ||
    !/(?:Z|[+-]\d{2}:\d{2})$/iu.test(startTime) ||
    !Number.isSafeInteger(systemStartMs) ||
    systemStartMs < 0 ||
    !Number.isSafeInteger(slotLengthMs) ||
    slotLengthMs <= 0
  ) {
    throw new Error(
      "Ogmios Shelley genesis has an invalid startTime or slotLength",
    );
  }
  return {
    source: {
      kind: "local_ogmios_genesis",
      endpointIdentitySha256: sha256(
        Buffer.from(normalizeOgmiosEvidenceUrl(ogmiosUrl)),
      ),
      configurationSha256: sha256(
        Buffer.from(JSON.stringify(canonicalJsonValue(root.result))),
      ),
    },
    slotConfig: {
      zeroTime: systemStartMs,
      zeroSlot: 0,
      slotLength: slotLengthMs,
    },
  };
};

export const validateNodeSlotConfigEvidenceV1 = (value) => {
  const document = exactRecord(
    value,
    ["schemaVersion", "capturedAtIso", "network", "source", "slotConfig"],
    "Node slot-config evidence",
  );
  if (document.schemaVersion !== NODE_SLOT_CONFIG_EVIDENCE_SCHEMA) {
    throw new Error("Unsupported node slot-config evidence schema");
  }
  canonicalTimestamp(document.capturedAtIso, "capturedAtIso");
  const slotConfig = validSlotConfig(
    document.slotConfig,
    "Node slot configuration",
  );
  if (document.network === "Custom") {
    const source = exactRecord(
      document.source,
      ["kind", "endpointIdentitySha256", "configurationSha256"],
      "Custom slot-config source",
    );
    if (
      source.kind !== "local_ogmios_genesis" ||
      !isHash(source.endpointIdentitySha256) ||
      !isHash(source.configurationSha256)
    ) {
      throw new Error("Custom slot-config source is invalid");
    }
  } else {
    const source = exactRecord(
      document.source,
      ["kind", "lucidVersion"],
      "Static slot-config source",
    );
    const expected = STATIC_SLOT_CONFIGS[document.network];
    if (
      source.kind !== "lucid_network_table" ||
      source.lucidVersion !== LUCID_VERSION ||
      expected === undefined ||
      JSON.stringify(slotConfig) !== JSON.stringify(expected)
    ) {
      throw new Error(
        "Static slot configuration does not match the pinned Lucid network table",
      );
    }
  }
  return document;
};

export const readNodeSlotConfigEvidenceV1 = ({ path, expectedSha256 }) => {
  const resolvedPath = resolve(path);
  if (!isAbsolute(path) || resolvedPath !== path || !isHash(expectedSha256)) {
    throw new Error(
      "Slot-config evidence path/hash must be canonical and absolute",
    );
  }
  const bytes = readFileSync(resolvedPath);
  if (sha256(bytes) !== expectedSha256) {
    throw new Error("Node slot-config evidence SHA-256 mismatch");
  }
  return validateNodeSlotConfigEvidenceV1(JSON.parse(bytes.toString("utf8")));
};

export const buildNodeSlotConfigEvidenceV1 = ({
  network,
  ogmiosUrl,
  ogmiosGenesisPayload,
  capturedAtIso = new Date().toISOString(),
}) => {
  if (network === "Custom") {
    if (
      typeof ogmiosUrl !== "string" ||
      ogmiosUrl.trim().length === 0 ||
      ogmiosGenesisPayload === undefined
    ) {
      throw new Error(
        "Custom network requires an Ogmios URL and genesis response",
      );
    }
    const derived = deriveOgmiosGenesisSlotEvidenceV1({
      ogmiosUrl,
      payload: ogmiosGenesisPayload,
    });
    return validateNodeSlotConfigEvidenceV1({
      schemaVersion: NODE_SLOT_CONFIG_EVIDENCE_SCHEMA,
      capturedAtIso,
      network,
      source: derived.source,
      slotConfig: derived.slotConfig,
    });
  }
  const slotConfig = STATIC_SLOT_CONFIGS[network];
  if (slotConfig === undefined) {
    throw new Error("Network must be Mainnet, Preview, Preprod, or Custom");
  }
  return validateNodeSlotConfigEvidenceV1({
    schemaVersion: NODE_SLOT_CONFIG_EVIDENCE_SCHEMA,
    capturedAtIso,
    network,
    source: { kind: "lucid_network_table", lucidVersion: LUCID_VERSION },
    slotConfig,
  });
};

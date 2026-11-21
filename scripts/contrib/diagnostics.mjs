import { readFileSync } from "node:fs";
import { checkBuild } from "./build.mjs";
import { packageByName } from "./files.mjs";

const sensitive =
  /seed|mnemonic|password|secret|token|credential|authorization|private|signed|cbor|witness|transaction|payload/iu;
export const redact = (value) => {
  if (Array.isArray(value)) return value.map(redact);
  if (value && typeof value === "object")
    return Object.fromEntries(
      Object.entries(value).map(([key, item]) => [
        key,
        sensitive.test(key) ? "[redacted]" : redact(item),
      ]),
    );
  // Provider URLs can contain credentials/query keys without a sensitive key.
  if (typeof value === "string" && /^https?:\/\//u.test(value)) {
    const url = new URL(value);
    url.username = "";
    url.password = "";
    url.search = "";
    url.hash = "";
    return url.href;
  }
  return value;
};

export const classifyProgress = (status) => {
  if (
    !status ||
    typeof status !== "object" ||
    !Number.isFinite(status.observedAtMs)
  )
    return { state: "unknown", reason: "no timestamped progress observation" };
  if (status.processAlive === false)
    return { state: "dead", reason: "service process is absent" };
  if (status.safetyHold)
    return { state: "held", reason: String(status.safetyHold) };
  if (status.dependencyFailure)
    return {
      state: "dependency-outage",
      reason: String(status.dependencyFailure),
    };
  if (!Number.isSafeInteger(status.eligibleWork) || status.eligibleWork < 0)
    return { state: "unknown", reason: "eligible work was not measured" };
  if (status.eligibleWork === 0)
    return { state: "idle", reason: "no eligible obligations" };
  if (
    !Number.isFinite(status.lastSuccessfulTransitionMs) ||
    !Number.isFinite(status.expectedProgressWithinMs) ||
    status.expectedProgressWithinMs <= 0 ||
    status.lastSuccessfulTransitionMs > status.observedAtMs
  )
    return {
      state: "unknown",
      reason: "missing/contradictory progress clock or workload cadence",
    };
  return status.observedAtMs - status.lastSuccessfulTransitionMs >
    status.expectedProgressWithinMs
    ? {
        state: "stalled",
        reason: `${status.eligibleWork} eligible obligations exceeded their recorded cadence`,
      }
    : {
        state: "working",
        reason: "eligible work remains within its declared cadence",
      };
};

// Deliberately reads a snapshot/export, never instantiates stores that migrate
// or acquire ownership. HTTP capture is separate from classification so an
// endpoint cannot accidentally be mistaken for a verified progress schema.
export const diagnostics = (root, { snapshot, name = "midgard-node" }) => {
  const pkg = packageByName(root, name);
  const data = JSON.parse(readFileSync(snapshot, "utf8"));
  return redact({
    schema: "midgard-contrib-diagnostics/v1",
    capturedAt: new Date().toISOString(),
    package: pkg.name,
    build: checkBuild(root, pkg.name),
    classification: classifyProgress(data.progress),
    observation: data,
  });
};

export const captureHttp = async (address, { signal } = {}) => {
  const url = new URL(address);
  if (
    !["http:", "https:"].includes(url.protocol) ||
    url.username ||
    url.password ||
    url.search ||
    !["127.0.0.1", "localhost", "[::1]"].includes(url.hostname)
  )
    throw new Error(
      "diagnostics capture requires a local read-only URL without credentials/query parameters",
    );
  const response = await fetch(url, {
    signal: AbortSignal.any([
      signal ?? new AbortController().signal,
      AbortSignal.timeout(5000),
    ]),
    redirect: "error",
  });
  if (!response.ok)
    throw new Error(`diagnostics endpoint returned ${response.status}`);
  const reader = response.body.getReader();
  const chunks = [];
  let size = 0;
  try {
    for (;;) {
      const { value, done } = await reader.read();
      if (done) break;
      size += value.length;
      if (size > 1024 * 1024)
        throw new Error("diagnostics response exceeds 1 MiB");
      chunks.push(value);
    }
    return redact(JSON.parse(Buffer.concat(chunks).toString("utf8")));
  } finally {
    await reader.cancel();
  }
};

import { createReadStream, statSync } from "node:fs";
import { createHash } from "node:crypto";

// Streaming exact-byte measurement remains usable for multi-GB witnesses.
// It does not parse/deep-compare material or claim an L1/exunit bound.
export const measureFiles = async (
  paths,
  { maximumBytes = Number.MAX_SAFE_INTEGER, signal } = {},
) => {
  if (!paths.length || !Number.isSafeInteger(maximumBytes) || maximumBytes < 1)
    throw new Error("measure requires files and a positive integer byte bound");
  const entries = [];
  const unique = new Map();
  let totalBytes = 0;
  for (const path of paths) {
    signal?.throwIfAborted();
    const stats = statSync(path);
    if (!stats.isFile())
      throw new Error(`measurement input is not a regular file: ${path}`);
    if (stats.size > maximumBytes)
      throw new Error(
        `${path} exceeds the declared ${maximumBytes}-byte input envelope`,
      );
    const hash = createHash("sha256");
    let bytes = 0;
    for await (const chunk of createReadStream(path, { signal })) {
      bytes += chunk.length;
      hash.update(chunk);
    }
    const after = statSync(path);
    if (
      after.ino !== stats.ino ||
      after.size !== bytes ||
      after.mtimeMs !== stats.mtimeMs
    )
      throw new Error(`measurement input changed while reading: ${path}`);
    const sha256 = hash.digest("hex");
    entries.push({ path, bytes, sha256 });
    const previous = unique.get(sha256);
    if (previous !== undefined && previous !== bytes)
      throw new Error("hash collision/inconsistent immutable material");
    unique.set(sha256, bytes);
    totalBytes += bytes;
  }
  return {
    schema: "midgard-byte-measurement/v1",
    measuredAt: new Date().toISOString(),
    scope:
      "exact local serialized bytes; SHA-256 dedup identity, no protocol authentication or network guarantee",
    entries,
    totalBytes,
    uniqueBytes: [...unique.values()].reduce((sum, value) => sum + value, 0),
    duplicateBytes:
      totalBytes - [...unique.values()].reduce((sum, value) => sum + value, 0),
  };
};

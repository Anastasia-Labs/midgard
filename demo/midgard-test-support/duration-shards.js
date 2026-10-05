import { readFileSync } from "node:fs";
import { relative, sep } from "node:path";

import { BaseSequencer } from "vitest/node";

/**
 * Duration-aware `--shard` for packages whose CI job runs as several shards.
 *
 * Plain JavaScript for the same reason as `vitest.js`: a `vitest.config.ts`
 * loads it before any workspace resolution condition is in play.
 * `duration-shards.d.ts` carries the types.
 *
 * Vitest's own `--shard=i/n` sorts files by a SHA-1 of their path and cuts the
 * list into equal COUNTS. A suite whose files differ in cost by three orders
 * of magnitude then clusters its giants by hash: at 15f9e7423 the three
 * fault-proof shards ran 45, 75 and 41 minutes of Vitest. This sequencer
 * instead packs files by recorded CI seconds. It models each shard as
 * `forksPerShard` forks and places files longest first, each onto the least
 * loaded fork of all shards (LPT scheduling); inside a shard `sort` starts the
 * longest files first, which is the order the plan assumed, so a giant never
 * starts last on a fork.
 *
 * Correctness does not depend on the table. Every shard process computes the
 * same plan from the same file list and the same committed table, and the plan
 * is a partition: each file lands in exactly one shard. A file the table does
 * not know (new or renamed) weighs `defaultSeconds` and is placed like any
 * other, so it still runs exactly once. A stale table only costs balance.
 * Regenerate it from CI logs with `scripts/ci-file-durations.mjs`.
 */

/**
 * @param {string} path absolute or package-relative path of the table
 */
export const readShardDurationTable = (path) => {
  const raw = JSON.parse(readFileSync(path, "utf8"));
  const files = new Map();
  for (const [key, seconds] of Object.entries(raw.files ?? {})) {
    if (typeof seconds !== "number" || !Number.isFinite(seconds) || seconds < 0)
      throw new Error(`${path}: ${key} has no finite non-negative seconds`);
    files.set(key, seconds);
  }
  const defaultSeconds = raw.defaultSeconds;
  if (
    typeof defaultSeconds !== "number" ||
    !Number.isFinite(defaultSeconds) ||
    defaultSeconds < 0
  )
    throw new Error(`${path}: defaultSeconds must be a non-negative number`);
  const forksPerShard = raw.forksPerShard;
  if (!Number.isInteger(forksPerShard) || forksPerShard < 1)
    throw new Error(`${path}: forksPerShard must be a positive integer`);
  const reservedSeconds = new Map();
  for (const [shard, seconds] of Object.entries(raw.reservedSeconds ?? {})) {
    if (!/^\d+\/\d+$/u.test(shard) || typeof seconds !== "number")
      throw new Error(`${path}: reservedSeconds keys are "index/count"`);
    reservedSeconds.set(shard, seconds);
  }
  return { files, defaultSeconds, forksPerShard, reservedSeconds };
};

const byWeightThenKey = (left, right) =>
  right.seconds - left.seconds ||
  (left.key < right.key ? -1 : left.key > right.key ? 1 : 0);

/**
 * Partition test files into `count` shards. Each entry is `{ id, file }`: `id`
 * is unique per scheduled file (project name and path) and `file` is the
 * package-relative path the table is keyed by. Returns a Map from id to its
 * 1-based shard index. Deterministic: the result depends only on the SET of
 * entries (not their order), the table and `count` -- never on the machine
 * or the fork cap of the run computing it, so every shard of one CI run
 * computes the same partition.
 *
 * `reservedSeconds["i/n"]` is serial work the CI job for shard i of n does
 * after its tests (shard 1's traced-refusal reruns); it is charged to every
 * fork of that shard.
 *
 * @param {{ entries: readonly { id: string, file: string }[], count: number,
 *   table: ReturnType<typeof readShardDurationTable> }} input
 */
export const planDurationShards = ({ entries, count, table }) => {
  if (!Number.isInteger(count) || count < 1)
    throw new Error(`shard count must be a positive integer, got ${count}`);
  const forks = table.forksPerShard;
  const loads = Array.from(
    { length: count * forks },
    (_, slot) =>
      table.reservedSeconds.get(`${Math.floor(slot / forks) + 1}/${count}`) ??
      0,
  );
  const plan = new Map();
  const weighted = entries
    .map(({ id, file }) => ({
      key: id,
      seconds: table.files.get(file) ?? table.defaultSeconds,
    }))
    .sort(byWeightThenKey);
  for (const { key, seconds } of weighted) {
    if (plan.has(key)) throw new Error(`duplicate test file id ${key}`);
    let lightest = 0;
    for (let slot = 1; slot < loads.length; slot++)
      if (loads[slot] < loads[lightest]) lightest = slot;
    loads[lightest] += seconds;
    plan.set(key, Math.floor(lightest / forks) + 1);
  }
  return plan;
};

const specFile = (root, spec) =>
  relative(root, spec.moduleId).split(sep).join("/");
const specId = (root, spec) => `${spec.project.name}:${specFile(root, spec)}`;

/**
 * A Vitest sequencer class whose `shard` packs by the table at `tablePath`
 * and whose `sort` starts the longest known files first.
 *
 * @param {{ tablePath: string }} options
 */
export const durationShardSequencer = ({ tablePath }) => {
  const table = readShardDurationTable(tablePath);
  const weigh = (root, spec) => ({
    spec,
    key: specId(root, spec),
    seconds: table.files.get(specFile(root, spec)) ?? table.defaultSeconds,
  });
  return class DurationShardSequencer extends BaseSequencer {
    async shard(files) {
      const { root, shard } = this.ctx.config;
      const plan = planDurationShards({
        entries: files.map((spec) => ({
          id: specId(root, spec),
          file: specFile(root, spec),
        })),
        count: shard.count,
        table,
      });
      return files.filter(
        (spec) => plan.get(specId(root, spec)) === shard.index,
      );
    }

    async sort(files) {
      const { root } = this.ctx.config;
      return files
        .map((spec) => weigh(root, spec))
        .sort(byWeightThenKey)
        .map(({ spec }) => spec);
    }
  };
};

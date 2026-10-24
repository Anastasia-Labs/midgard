import { readFileSync } from "node:fs";

/**
 * The planning half of `duration-shards.js`: reading a duration table and
 * partitioning files by it. Nothing here imports Vitest, so the CI scripts
 * that refresh and check a table run on a bare Node install.
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

export const byWeightThenKey = (left, right) =>
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

/**
 * Projected seconds of each of `count` shards under `table`: the files
 * {@link planDurationShards} gives the shard, started longest first (the
 * sequencer's order) on its `forksPerShard` forks, each file taken by the
 * first fork to come free; plus the shard's `reservedSeconds`, the serial work
 * its CI job does after its tests.
 *
 * @param {{ entries: readonly { id: string, file: string }[], count: number,
 *   table: ReturnType<typeof readShardDurationTable> }} input
 * @returns {number[]} seconds per shard, shard 1 first
 */
export const projectShardSeconds = ({ entries, count, table }) => {
  const plan = planDurationShards({ entries, count, table });
  return Array.from({ length: count }, (_, index) => {
    const forks = Array.from({ length: table.forksPerShard }, () => 0);
    const files = entries
      .filter(({ id }) => plan.get(id) === index + 1)
      .map(({ id, file }) => ({
        key: id,
        seconds: table.files.get(file) ?? table.defaultSeconds,
      }))
      .sort(byWeightThenKey);
    for (const { seconds } of files) {
      let free = 0;
      for (let fork = 1; fork < forks.length; fork++)
        if (forks[fork] < forks[free]) free = fork;
      forks[free] += seconds;
    }
    return (
      Math.max(...forks) +
      (table.reservedSeconds.get(`${index + 1}/${count}`) ?? 0)
    );
  });
};

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, relative, sep } from "node:path";

import { BaseSequencer } from "vitest/node";

import {
  byWeightThenKey,
  planDurationShards,
  readShardDurationTable,
} from "./duration-plan.js";

export {
  planDurationShards,
  projectShardSeconds,
  readShardDurationTable,
} from "./duration-plan.js";

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
 *
 * The table maintains itself from CI. A package adopts all of this through
 * {@link durationShards}, whose reporter records every file's seconds when
 * `MIDGARD_FILE_DURATIONS_OUT` names a file; each CI shard uploads that record,
 * and Node CI's `test-durations` job merges a run's records into a refreshed
 * table (artifact `ci-file-durations`), warning when files the committed table
 * does not know, or knows wrongly, carry more than a tenth of the run's
 * seconds. Refresh the committed table from any run with one command:
 *
 *   node demo/midgard-test-support/scripts/ci-file-durations.mjs \
 *     --package demo/<package> --table <its table> --run <run id>
 */

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

/**
 * Seconds a file kept its fork busy: preparing the worker, loading the
 * environment, importing the setup files and the file's own module graph
 * (collection), and running its tests and hooks.
 *
 * @param {{ prepareDuration?: number, environmentLoad?: number,
 *   setupDuration?: number, collectDuration?: number,
 *   result?: { duration?: number } }} file a Vitest file task
 */
export const fileTaskSeconds = (file) =>
  ((file.prepareDuration ?? 0) +
    (file.environmentLoad ?? 0) +
    (file.setupDuration ?? 0) +
    (file.collectDuration ?? 0) +
    (file.result?.duration ?? 0)) /
  1000;

/**
 * A Vitest reporter that writes, at the end of the run, the seconds each test
 * file took (see {@link fileTaskSeconds}) to `outputPath`, keyed by the
 * package-relative path the duration table uses:
 * `{ "schema": "midgard-file-durations/v1", "shard": "i/n" | null,
 *    "files": { "tests/x.test.ts": 12.3 } }`.
 * `scripts/ci-file-durations.mjs` turns these records into a table.
 */
export class FileDurationsReporter {
  /** @param {string} outputPath */
  constructor(outputPath) {
    this.outputPath = outputPath;
  }

  onInit(ctx) {
    this.ctx = ctx;
  }

  onFinished(files = []) {
    const { root, shard } = this.ctx.config;
    const seconds = {};
    for (const file of files) {
      const key = relative(root, file.filepath).split(sep).join("/");
      seconds[key] = Math.max(seconds[key] ?? 0, fileTaskSeconds(file));
    }
    mkdirSync(dirname(this.outputPath), { recursive: true });
    writeFileSync(
      this.outputPath,
      JSON.stringify(
        {
          schema: "midgard-file-durations/v1",
          shard: shard ? `${shard.index}/${shard.count}` : null,
          files: Object.fromEntries(
            Object.entries(seconds)
              .sort(([left], [right]) =>
                left < right ? -1 : left > right ? 1 : 0,
              )
              .map(([key, value]) => [key, Math.round(value * 10) / 10]),
          ),
        },
        null,
        2,
      ) + "\n",
    );
  }
}

/**
 * The Vitest config fragment a package spreads into `test` to shard by its
 * duration table: the duration sequencer, and `reporters` plus a
 * {@link FileDurationsReporter} when `MIDGARD_FILE_DURATIONS_OUT` is set (as
 * Node CI sets it), so the run records the timings its table is refreshed
 * from. Every test file the package adds is timed and packed with no further
 * wiring.
 *
 * @param {{ tablePath: string, reporters: readonly unknown[] }} options
 */
export const durationShards = ({ tablePath, reporters }) => {
  const outputPath = process.env.MIDGARD_FILE_DURATIONS_OUT?.trim();
  return {
    reporters: outputPath
      ? [...reporters, new FileDurationsReporter(outputPath)]
      : [...reporters],
    sequence: { sequencer: durationShardSequencer({ tablePath }) },
  };
};

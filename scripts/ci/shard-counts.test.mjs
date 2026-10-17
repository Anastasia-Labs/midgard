// Node CI's sharded jobs take their shard count from their own matrix
// (`strategy.job-total`), and `test-durations` reads it back from the
// artifact names, so the matrix is the workflow's one copy of each count. The
// duration-shards contract every sharded package pins in its own suite keeps a
// second copy (`ciShardCount`), which this test holds to the matrix.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

const loadYaml = () => {
  try {
    return createRequire(join(root, "demo/package.json"))("yaml");
  } catch {
    return undefined;
  }
};
const yaml = loadYaml();
const skip =
  yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
    ? "could not check: yaml absent (run `pnpm --dir demo install`)"
    : false;

const workflowPath = join(root, ".github/workflows/midgard-node-ci.yml");
const shardedJobs = ["midgard-fault-proofs", "midgard-watcher", "midgard-node"];

test("each sharded job takes its count from its matrix", { skip }, () => {
  const text = readFileSync(workflowPath, "utf8");
  const { jobs } = yaml.parse(text);
  for (const name of shardedJobs) {
    const job = jobs[name];
    const shards = job.strategy.matrix.shard;
    assert.deepEqual(
      shards,
      Array.from({ length: shards.length }, (_, i) => i + 1),
      `${name}: the matrix lists shards 1..n`,
    );
    const steps = JSON.stringify(job.steps);
    assert.match(
      steps,
      /--shard=\$\{\{ matrix\.shard \}\}\/\$\{\{ strategy\.job-total \}\}/u,
      `${name}: the test step shards by strategy.job-total`,
    );
    assert.match(
      steps,
      /file-durations-[a-z-]+-\$\{\{ matrix\.shard \}\}-of-\$\{\{ strategy\.job-total \}\}/u,
      `${name}: the durations artifact carries the shard count`,
    );
  }
  assert.doesNotMatch(
    text,
    /--shard=[^\s]*\/\d/u,
    "no step spells a literal shard count",
  );
});

test(
  "each package's duration-shards contract pins its job's shard count",
  { skip },
  () => {
    const { jobs } = yaml.parse(readFileSync(workflowPath, "utf8"));
    for (const name of shardedJobs) {
      const contract = readFileSync(
        join(root, "demo", name, "tests/duration-shards.test.ts"),
        "utf8",
      );
      const pinned = [...contract.matchAll(/ciShardCount:\s*(\d+)/gu)];
      assert.equal(pinned.length, 1, `${name}: one ciShardCount`);
      assert.equal(
        Number(pinned[0][1]),
        jobs[name].strategy.matrix.shard.length,
        `${name}: ciShardCount matches the midgard-node-ci.yml matrix`,
      );
    }
  },
);

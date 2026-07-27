import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const fixtureDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(fixtureDirectory, "../..");
const demoRoot = resolve(packageRoot, "..");
const temporaryDirectory = mkdtempSync(
  join(tmpdir(), "midgard-cardano-p2-retained-da-"),
);
const jsonlPath = resolve(temporaryDirectory, "corpus.jsonl");
const regeneratedPath = resolve(temporaryDirectory, "regenerated.json");
const checkedFixturePath = resolve(
  fixtureDirectory,
  "cardano-capability-p2-boundary-corpus-v1.json",
);
const converterPath = resolve(
  fixtureDirectory,
  "build-cardano-capability-p2-boundary-corpus-v1.mjs",
);

const run = (command, arguments_, environment = process.env) => {
  const result = spawnSync(command, arguments_, {
    cwd: demoRoot,
    env: environment,
    stdio: "inherit",
  });
  if (result.error !== undefined) {
    throw result.error;
  }
  if (result.status !== 0) {
    throw new Error(
      `${command} ${arguments_.join(" ")} failed with status ${String(
        result.status,
      )}`,
    );
  }
};

try {
  // Producers append because separate test files contribute rows. Each
  // invocation owns a private temporary directory so concurrent verification
  // cannot cross-contaminate either artifact.
  writeFileSync(jsonlPath, "", "utf8");
  writeFileSync(regeneratedPath, "", "utf8");

  run(
    "pnpm",
    [
      "--filter",
      "@al-ft/midgard-validation",
      "exec",
      "vitest",
      "run",
      "tests/ordered-collection-boundary-v1.test.ts",
      "tests/ordered-collection-signer-witness-boundary-v1.test.ts",
      "tests/ordered-collection-spend-inputs-boundary-v1.test.ts",
      "tests/ordered-collection-reference-inputs-boundary-v1.test.ts",
      "tests/ordered-collection-observer-native-script-boundary-v1.test.ts",
      "tests/ordered-collection-mint-boundary-v1.test.ts",
      "tests/ordered-collection-redeemer-boundary-v1.test.ts",
      "tests/blob-chunk-boundary-v1.test.ts",
      "tests/nested-value-boundary-v1.test.ts",
      "tests/nested-data-boundary-v1.test.ts",
      "tests/nested-redeemer-data-boundary-v1.test.ts",
      "tests/retained-da-boundary-v1.test.ts",
      "--pool=forks",
      "--poolOptions.forks.singleFork=true",
    ],
    {
      ...process.env,
      MIDGARD_BOUNDARY_CORPUS_JSONL: jsonlPath,
    },
  );
  run(process.execPath, [converterPath, jsonlPath, regeneratedPath]);
  run("cmp", [regeneratedPath, checkedFixturePath]);
  run("pnpm", [
    "--filter",
    "@al-ft/midgard-fault-proofs",
    "exec",
    "vitest",
    "run",
    "tests/cardano-capability-retained-da-v1.test.ts",
    "--pool=forks",
    "--poolOptions.forks.singleFork=true",
  ]);
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}

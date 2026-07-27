import { spawnSync } from "node:child_process";
import { copyFileSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
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
const [mode, ...unexpectedArguments] = process.argv.slice(2);
if (
  unexpectedArguments.length !== 0 ||
  (mode !== undefined && mode !== "--update")
) {
  throw new Error(
    "usage: node verify-cardano-capability-p2-retained-da-v1.mjs [--update]",
  );
}

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
      "tests/data-breadth-boundary-v1.test.ts",
      "--pool=forks",
      "--poolOptions.forks.singleFork=true",
      "--testTimeout=360000",
      "--hookTimeout=60000",
    ],
    {
      ...process.env,
      NODE_OPTIONS: "--max-old-space-size=4096",
      MIDGARD_BOUNDARY_CORPUS_JSONL: jsonlPath,
    },
  );
  run(process.execPath, [converterPath, jsonlPath, regeneratedPath]);
  if (mode === undefined) {
    run("cmp", [regeneratedPath, checkedFixturePath]);
  }
  const generatedCorpusEnvironment = {
    ...process.env,
    MIDGARD_BOUNDARY_CORPUS_JSON: regeneratedPath,
  };
  run(
    "pnpm",
    [
      "--filter",
      "midgard-watcher",
      "exec",
      "vitest",
      "run",
      "tests/cardano-capability-retained-da-corpus-v1.test.ts",
      "--pool=forks",
      "--poolOptions.forks.singleFork=true",
      "--testTimeout=180000",
    ],
    generatedCorpusEnvironment,
  );
  run(
    "pnpm",
    [
      "--filter",
      "@al-ft/midgard-fault-proofs",
      "exec",
      "vitest",
      "run",
      "tests/cardano-capability-retained-da-v1.test.ts",
      "--pool=forks",
      "--poolOptions.forks.singleFork=true",
      "--testTimeout=180000",
    ],
    generatedCorpusEnvironment,
  );
  if (mode === "--update") {
    // Promote the regenerated fixture only after every strict production
    // consumer has accepted that exact temporary artifact.
    copyFileSync(regeneratedPath, checkedFixturePath);
    run("cmp", [regeneratedPath, checkedFixturePath]);
  }
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}

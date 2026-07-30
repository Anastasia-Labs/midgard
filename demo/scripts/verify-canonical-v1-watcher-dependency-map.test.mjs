import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { execFileSync, spawnSync } from "node:child_process";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");
const verifierPath = join(
  scriptDirectory,
  "verify-canonical-v1-watcher-dependency-map.mjs",
);
const mapPath =
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json";
const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const execGit = (args, options = {}) =>
  execFileSync("git", args, {
    cwd: repositoryRoot,
    encoding: options.encoding ?? "buffer",
    env: options.env ?? process.env,
    input: options.input,
  });

const runWithMutatedIndexedMap = async (mutate) => {
  const temporaryDirectory = await mkdtemp(
    join(tmpdir(), "midgard-watcher-map-test-"),
  );
  try {
    const indexFile = join(temporaryDirectory, "index");
    const environment = {
      ...process.env,
      GIT_INDEX_FILE: indexFile,
    };
    const startingTree = execGit(["write-tree"], {
      encoding: "utf8",
    }).trim();
    execGit(["read-tree", startingTree], { env: environment });
    const dependencyMap = JSON.parse(
      execGit(["show", `:${mapPath}`], {
        encoding: "utf8",
        env: environment,
      }),
    );
    const sourceOverrides = new Map();
    mutate(dependencyMap, {
      replaceSource(path, replacement) {
        const current = execGit(["show", `:${path}`], {
          encoding: "utf8",
          env: environment,
        });
        const next =
          typeof replacement === "function"
            ? replacement(current)
            : replacement;
        assert.notEqual(next, current, `${path} mutation must change bytes`);
        sourceOverrides.set(path, Buffer.from(next, "utf8"));
      },
    });
    for (const [path, bytes] of sourceOverrides) {
      const objectId = execGit(["hash-object", "-w", "--stdin"], {
        encoding: "utf8",
        input: bytes,
      }).trim();
      execGit(
        ["update-index", "--add", "--cacheinfo", `100644,${objectId},${path}`],
        {
          env: environment,
        },
      );
      for (const dependency of dependencyMap.dependencies) {
        if (dependency.sourcePaths.includes(path)) {
          dependency.sourceSha256[path] = sha256(bytes);
        }
      }
    }
    const mutatedBytes = Buffer.from(
      `${JSON.stringify(dependencyMap, null, 2)}\n`,
      "utf8",
    );
    const objectId = execGit(["hash-object", "-w", "--stdin"], {
      encoding: "utf8",
      input: mutatedBytes,
    }).trim();
    execGit(
      ["update-index", "--add", "--cacheinfo", `100644,${objectId},${mapPath}`],
      { env: environment },
    );
    return spawnSync(process.execPath, [verifierPath], {
      cwd: repositoryRoot,
      encoding: "utf8",
      env: environment,
    });
  } finally {
    await rm(temporaryDirectory, { recursive: true, force: true });
  }
};

test("rejects a dependency rebound to an arbitrary tracked source", async () => {
  const replacementPath = "demo/package.json";
  const replacementBytes = execGit(["show", `HEAD:${replacementPath}`]);
  const result = await runWithMutatedIndexedMap((dependencyMap) => {
    const dependency = dependencyMap.dependencies.find(
      ({ id }) => id === "public_da",
    );
    assert.ok(dependency);
    dependency.sourcePaths = [replacementPath];
    dependency.sourceSha256 = {
      [replacementPath]: sha256(replacementBytes),
    };
    dependency.sourceSymbols = ["scripts"];
  });
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source paths and symbols must match the exact required set/,
  );
});

test("rejects an empty dependency source symbol", async () => {
  const result = await runWithMutatedIndexedMap((dependencyMap) => {
    const dependency = dependencyMap.dependencies.find(
      ({ id }) => id === "public_da",
    );
    assert.ok(dependency);
    dependency.sourceSymbols = [""];
  });
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source paths and symbols must match the exact required set/,
  );
});

test("rejects arbitrary dependency capability and task claims", async () => {
  const result = await runWithMutatedIndexedMap((dependencyMap) => {
    const dependency = dependencyMap.dependencies.find(
      ({ id }) => id === "public_da",
    );
    assert.ok(dependency);
    dependency.capability = "PASS";
    dependency.state = "complete";
    dependency.remainingTasks = ["PASS"];
    dependency.watcherBoundary = "x";
    dependencyMap.f30Conclusion = {
      status: "pass",
      reason: "PASS",
      nextTasks: [],
    };
  });
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da capability, state, tasks, and boundary must remain exact/,
  );
});

test("rejects weakened post-finality recovery and durable-role claims", async () => {
  const mutations = [
    {
      mutate(dependencyMap) {
        dependencyMap.requiredWatcherPackage.strictConfiguration.finalityPolicy.postFinalityRecoveryMaxDepth = 1;
      },
      message:
        /W01 strict watcher configuration evidence is incomplete or stale/,
    },
    {
      mutate(dependencyMap) {
        dependencyMap.requiredWatcherPackage.rollbackEngine.postFinalityPolicy =
          "manual";
      },
      message: /W13 rollback-engine evidence is incomplete or stale/,
    },
    {
      mutate(dependencyMap) {
        dependencyMap.requiredWatcherPackage.stateQueueIndexer.durableRolePolicy =
          "trust caller";
      },
      message: /W14 state-queue-indexer evidence is incomplete or stale/,
    },
    {
      mutate(dependencyMap) {
        const dependency = dependencyMap.dependencies.find(
          ({ id }) => id === "l1_provider",
        );
        assert.ok(dependency);
        dependency.watcherBoundary = "always require two providers";
      },
      message:
        /l1_provider capability, state, tasks, and boundary must remain exact/,
    },
  ];
  for (const { mutate, message } of mutations) {
    const result = await runWithMutatedIndexedMap(mutate);
    assert.notEqual(result.status, 0);
    assert.match(`${result.stdout}${result.stderr}`, message);
  }
});

test("rejects a symbol declaration moved to the wrong allowed source", async () => {
  const owner = "demo/da-committee-node/src/da/payload.ts";
  const other = "demo/midgard-core/src/da-payload-envelope.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source.replace(
          "export const decodeDaPayloadV1Strict",
          "export const decodeDaPayloadV1StrictRemoved",
        ),
      );
      replaceSource(
        other,
        (source) => `${source}\nexport const decodeDaPayloadV1Strict = null;\n`,
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol decodeDaPayloadV1Strict is not declared by demo\/da-committee-node\/src\/da\/payload\.ts/,
  );
});

test("rejects a source symbol preserved only in comments", async () => {
  const owner = "demo/da-committee-node/src/da/payload.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source.replace(
          "export const decodeDaPayloadV1Strict",
          "// export const decodeDaPayloadV1Strict\nexport const decodeDaPayloadV1StrictRemoved",
        ),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol decodeDaPayloadV1Strict is not declared by demo\/da-committee-node\/src\/da\/payload\.ts/,
  );
});

test("rejects a source symbol preserved only in a regex literal", async () => {
  const owner = "demo/da-committee-node/src/da/payload.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source.replace(
          "export const decodeDaPayloadV1Strict",
          "const declarationProbe = /export const decodeDaPayloadV1Strict/;\nexport const decodeDaPayloadV1StrictRemoved",
        ),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol decodeDaPayloadV1Strict is not declared by demo\/da-committee-node\/src\/da\/payload\.ts/,
  );
});

test("rejects a runtime source symbol preserved only as a type", async () => {
  const owner = "demo/da-committee-node/src/da/payload.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source.replace(
          "export const decodeDaPayloadV1Strict",
          "export type decodeDaPayloadV1Strict = unknown;\nconst decodeDaPayloadV1StrictRemoved",
        ),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol decodeDaPayloadV1Strict is not declared by demo\/da-committee-node\/src\/da\/payload\.ts/,
  );
});

test("rejects an ambient runtime source declaration", async () => {
  const owner = "demo/da-committee-node/src/da/payload.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source.replace(
          "export const decodeDaPayloadV1Strict",
          "export declare const decodeDaPayloadV1Strict: unknown;\nconst decodeDaPayloadV1StrictRemoved",
        ),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol decodeDaPayloadV1Strict is not declared by demo\/da-committee-node\/src\/da\/payload\.ts/,
  );
});

test("rejects a class member preserved only as a nested local function", async () => {
  const owner = "demo/da-committee-node/src/da/libp2p/DaLibp2pNode.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source
          .replace("  async request({", "  async requestRemoved({")
          .replace(
            "  isStarted(): boolean {\n    return this.started;\n  }",
            "  isStarted(): boolean {\n    function request(): void {}\n    void request;\n    return this.started;\n  }",
          ),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol DaLibp2pNode\.request is not declared by demo\/da-committee-node\/src\/da\/libp2p\/DaLibp2pNode\.ts/,
  );
});

test("rejects a class member preserved only as an abstract method", async () => {
  const owner = "demo/da-committee-node/src/da/libp2p/DaLibp2pNode.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(
        owner,
        (source) =>
          `${source.replace(
            "export class DaLibp2pNode",
            "export class DaLibp2pNodeRemoved",
          )}\nexport abstract class DaLibp2pNode {\n  abstract request(): Promise<void>;\n}\n`,
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol DaLibp2pNode\.request is not declared by demo\/da-committee-node\/src\/da\/libp2p\/DaLibp2pNode\.ts/,
  );
});

test("rejects a concrete member preserved only on an abstract class", async () => {
  const owner = "demo/da-committee-node/src/da/libp2p/DaLibp2pNode.ts";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(owner, (source) =>
        source.replace(
          "export class DaLibp2pNode",
          "export abstract class DaLibp2pNode",
        ),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /public_da source symbol DaLibp2pNode\.request is not declared by demo\/da-committee-node\/src\/da\/libp2p\/DaLibp2pNode\.ts/,
  );
});

test("rejects private, protected, static, or optional class capabilities", async () => {
  const owner = "demo/da-committee-node/src/da/libp2p/DaLibp2pNode.ts";
  for (const replacement of [
    "  private async request({",
    "  protected async request({",
    "  static async request({",
    "  async request?({",
  ]) {
    const result = await runWithMutatedIndexedMap(
      (_dependencyMap, { replaceSource }) => {
        replaceSource(owner, (source) =>
          source.replace("  async request({", replacement),
        );
      },
    );
    assert.notEqual(result.status, 0);
    assert.match(
      `${result.stdout}${result.stderr}`,
      /public_da source symbol DaLibp2pNode\.request is not declared by demo\/da-committee-node\/src\/da\/libp2p\/DaLibp2pNode\.ts/,
    );
  }
});

test("rejects a dependency command preserved only in a CI comment", async () => {
  const workflow = ".github/workflows/midgard-node-ci.yml";
  const command =
    "pnpm --dir demo run watcher:dependency-map:test && pnpm --dir demo run watcher:dependency-map:verify";
  const result = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(workflow, (source) =>
        source.replace(`        run: ${command}`, `        # run: ${command}`),
      );
    },
  );
  assert.notEqual(result.status, 0);
  assert.match(
    `${result.stdout}${result.stderr}`,
    /Midgard node CI must actively run exactly one/,
  );
});

test("rejects a pull-request branch filter that bypasses the target branch", async () => {
  const workflow = ".github/workflows/midgard-node-ci.yml";
  for (const branchFilter of [
    'branches: ["main"]',
    'branches-ignore : ["tx-validation"]',
    "'branches': [\"main\"]",
  ]) {
    const result = await runWithMutatedIndexedMap(
      (_dependencyMap, { replaceSource }) => {
        replaceSource(workflow, (source) =>
          source.replace(
            "  pull_request:\n    paths:",
            `  pull_request:\n    ${branchFilter}\n    paths:`,
          ),
        );
      },
    );
    assert.notEqual(result.status, 0);
    assert.match(
      `${result.stdout}${result.stderr}`,
      /pull_request must not be restricted by branch filters/,
    );
  }
});

test("rejects missing or inert Evidence Integrity watcher gates", async () => {
  const workflow = ".github/workflows/evidence-integrity-ci.yml";
  for (const stepName of [
    "Verify canonical watcher dependency-map mutation coverage",
    "Verify canonical watcher focused-test evidence",
  ]) {
    const result = await runWithMutatedIndexedMap(
      (_dependencyMap, { replaceSource }) => {
        replaceSource(workflow, (source) =>
          source.replace(
            `      - name: ${stepName}\n`,
            `      - name: ${stepName}\n        if: false\n`,
          ),
        );
      },
    );
    assert.notEqual(result.status, 0);
    assert.match(
      `${result.stdout}${result.stderr}`,
      /Evidence Integrity CI must actively run exactly one/,
    );
  }
});

test("rejects an inert dependency step and a trigger path moved outside paths", async () => {
  const workflow = ".github/workflows/midgard-node-ci.yml";
  const command =
    "pnpm --dir demo run watcher:dependency-map:test && pnpm --dir demo run watcher:dependency-map:verify";
  const inert = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(workflow, (source) =>
        source.replace(
          `      - name: Verify canonical watcher dependency map\n        run: ${command}`,
          `      - name: Verify canonical watcher dependency map\n        if: false\n        run: ${command}`,
        ),
      );
    },
  );
  assert.notEqual(inert.status, 0);
  assert.match(
    `${inert.stdout}${inert.stderr}`,
    /Midgard node CI must actively run exactly one/,
  );

  const requiredPath =
    "demo/scripts/verify-canonical-v1-watcher-dependency-map.test.mjs";
  const misplaced = await runWithMutatedIndexedMap(
    (_dependencyMap, { replaceSource }) => {
      replaceSource(workflow, (source) =>
        source
          .replace(
            `      - "${requiredPath}"`,
            `      # removed ${requiredPath}`,
          )
          .replace(
            "    env:\n      L1_PROVIDER: Kupmios",
            `    env:\n      EVIDENCE_PATH: "${requiredPath}"\n      L1_PROVIDER: Kupmios`,
          ),
      );
    },
  );
  assert.notEqual(misplaced.status, 0);
  assert.match(
    `${misplaced.stdout}${misplaced.stderr}`,
    /Midgard node CI must actively scope push/,
  );
});

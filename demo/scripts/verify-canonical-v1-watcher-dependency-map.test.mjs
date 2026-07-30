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

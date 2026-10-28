import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  forbiddenPackedPath,
  loadableSpecifiers,
  loadFailures,
  packedManifestProblems,
  packedSecretProblems,
  parseArguments,
  unpublishableSpecifier,
} from "./sdk-packages.mjs";

const publishNames = new Set(["@al-ft/a", "@al-ft/b"]);
const cleanManifest = () => ({
  name: "@al-ft/a",
  version: "0.1.0",
  license: "Apache-2.0",
  repository: { type: "git", url: "git+https://example.invalid/repo.git" },
  main: "./dist/index.js",
  types: "./dist/index.d.ts",
  exports: {
    ".": {
      "midgard-source": "./src/index.ts",
      types: "./dist/index.d.ts",
      import: "./dist/index.js",
      require: "./dist/index.cjs",
    },
  },
  dependencies: { "@al-ft/b": "0.1.0", effect: "^3.14.22" },
});
const cleanFiles = [
  "package.json",
  "README.md",
  "dist/index.js",
  "dist/index.cjs",
  "dist/index.d.ts",
];

test("a registry-installable manifest has no problems", () => {
  const { problems, warnings } = packedManifestProblems(
    cleanManifest(),
    cleanFiles,
    publishNames,
  );
  assert.deepEqual(problems, []);
  assert.deepEqual(warnings, [
    "1 of 1 exports keep the workspace-only midgard-source condition",
  ]);
});

test("refuses specifiers the registry cannot install", () => {
  for (const specifier of [
    "workspace:*",
    "file:../core",
    "link:../core",
    "github:owner/repo#abc&path:/off-chain",
    "git+https://example.invalid/repo.git",
    "https://example.invalid/pkg.tgz",
    "owner/repo",
    "latest",
    "*",
    "",
  ])
    assert.notEqual(unpublishableSpecifier(specifier), null, specifier);
  for (const specifier of [
    "0.8.5",
    "^3.14.22",
    "~1.2.0",
    ">=1 <2",
    "npm:x@1.0.0",
  ])
    assert.equal(unpublishableSpecifier(specifier), null, specifier);
  const manifest = cleanManifest();
  manifest.dependencies.mpf = "github:owner/repo#abc";
  manifest.peerDependencies = { lucid: "latest" };
  assert.deepEqual(
    packedManifestProblems(manifest, cleanFiles, publishNames).problems,
    [
      "dependencies.mpf is a local, git or URL specifier (github:owner/repo#abc)",
      "peerDependencies.lucid is an unpinned range (latest)",
    ],
  );
});

test("refuses a dependency on a workspace package outside the publish set", () => {
  const manifest = cleanManifest();
  manifest.dependencies["@al-ft/native-transport"] = "0.1.0";
  assert.deepEqual(
    packedManifestProblems(manifest, cleanFiles, publishNames).problems,
    [
      "dependencies.@al-ft/native-transport is a workspace package outside the publish set",
    ],
  );
});

test("refuses private, unlicensed, unversioned and repository-less manifests", () => {
  const manifest = {
    ...cleanManifest(),
    private: true,
    version: "latest",
    license: undefined,
    repository: undefined,
  };
  assert.deepEqual(
    packedManifestProblems(manifest, cleanFiles, publishNames).problems,
    [
      "package is private",
      "version latest is not an exact version",
      "license is missing",
      "repository is missing",
    ],
  );
});

test("refuses exports a consumer cannot resolve", () => {
  const manifest = cleanManifest();
  manifest.exports["./tests/fixtures"] = {
    "midgard-source": "./tests/fixtures.ts",
  };
  manifest.exports["./uplc"] = {
    types: "./dist/uplc.d.ts",
    import: "./dist/uplc.js",
  };
  assert.deepEqual(
    packedManifestProblems(manifest, cleanFiles, publishNames).problems,
    [
      "export ./tests/fixtures resolves only under the midgard-source condition",
      "export ./uplc [types] -> ./dist/uplc.d.ts is not in the tarball",
      "export ./uplc [import] -> ./dist/uplc.js is not in the tarball",
    ],
  );
  const { problems } = packedManifestProblems(
    { ...cleanManifest(), main: "./dist/missing.js" },
    cleanFiles,
    publishNames,
  );
  assert.deepEqual(problems, ["main ./dist/missing.js is not in the tarball"]);
});

test("refuses tests, fixtures and credential files in the tarball", () => {
  for (const path of [
    "tests/helper.js",
    "dist/test/x.js",
    "dist/index.test.js",
    "dist/index.spec.d.ts",
    "fixtures/vector.json",
    "dist/__fixtures__/a.json",
    ".env",
    ".env.local",
    ".npmrc",
    "keys/payment.skey",
    "server.pem",
  ])
    assert.notEqual(forbiddenPackedPath(path), null, path);
  for (const path of [
    "dist/index.js",
    "dist/chunk-ABC.cjs",
    "dist/index.d.cts",
    "scripts/golden-channel.mjs",
    "README.md",
  ])
    assert.equal(forbiddenPackedPath(path), null, path);
  assert.deepEqual(
    packedManifestProblems(
      cleanManifest(),
      [...cleanFiles, "dist/fixtures/vector.json"],
      publishNames,
    ).problems,
    ["dist/fixtures/vector.json is a fixture path"],
  );
});

test("finds private key material in packed files", () => {
  const root = mkdtempSync(join(tmpdir(), "sdk-packages-secret-"));
  mkdirSync(join(root, "dist"));
  writeFileSync(join(root, "dist/index.js"), "export const value = 1;\n");
  assert.deepEqual(packedSecretProblems(root), []);
  writeFileSync(
    join(root, "dist/leak.js"),
    "const k = `-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----`;\n",
  );
  writeFileSync(
    join(root, "dist/key.json"),
    JSON.stringify({ type: "PaymentSigningKeyShelley_ed25519", cborHex: "00" }),
  );
  assert.deepEqual(packedSecretProblems(root).sort(), [
    "dist/key.json contains a Cardano signing-key envelope",
    "dist/leak.js contains a PEM private key",
  ]);
});

const writePackage = (installRoot, name, manifest, files) => {
  const root = join(installRoot, "node_modules", name);
  for (const [path, text] of Object.entries(files)) {
    mkdirSync(join(root, path, ".."), { recursive: true });
    writeFileSync(join(root, path), text);
  }
  writeFileSync(join(root, "package.json"), JSON.stringify(manifest));
};

test("loads every export by package name through import and require", () => {
  const installRoot = mkdtempSync(join(tmpdir(), "sdk-packages-load-"));
  const manifest = {
    name: "@al-ft/a",
    version: "0.1.0",
    type: "module",
    exports: {
      ".": {
        "midgard-source": "./src/index.ts",
        types: "./dist/index.d.ts",
        import: "./dist/index.js",
        require: "./dist/index.cjs",
      },
      "./esm-only": { types: "./dist/esm.d.ts", import: "./dist/esm.js" },
      "./broken": {
        import: "./dist/broken.js",
        require: "./dist/broken.cjs",
      },
    },
  };
  writePackage(installRoot, "@al-ft/a", manifest, {
    "dist/index.js": "export const a = 1;\n",
    "dist/index.cjs": "exports.a = 1;\n",
    "dist/esm.js": "export const e = 1;\n",
    "dist/broken.js": 'import "@al-ft/not-published";\n',
    "dist/broken.cjs": 'require("@al-ft/not-published");\n',
  });
  assert.deepEqual(loadableSpecifiers("@al-ft/a", manifest, "import"), [
    "@al-ft/a",
    "@al-ft/a/esm-only",
    "@al-ft/a/broken",
  ]);
  assert.deepEqual(loadableSpecifiers("@al-ft/a", manifest, "require"), [
    "@al-ft/a",
    "@al-ft/a/broken",
  ]);
  const importFailures = loadFailures(
    installRoot,
    "import",
    loadableSpecifiers("@al-ft/a", manifest, "import"),
  );
  assert.equal(importFailures.length, 1);
  assert.match(importFailures[0], /^@al-ft\/a\/broken: .*not-published/u);
  const requireFailures = loadFailures(
    installRoot,
    "require",
    loadableSpecifiers("@al-ft/a", manifest, "require"),
  );
  assert.equal(requireFailures.length, 1);
  assert.match(requireFailures[0], /^@al-ft\/a\/broken: .*not-published/u);
});

test("requires a repository root and a new output directory", () => {
  assert.throws(() => parseArguments([]), /Usage/u);
  assert.throws(() => parseArguments(["--root", "/r"]), /Usage/u);
  assert.throws(() => parseArguments(["--root", "--output", "/o"]), /Usage/u);
  assert.deepEqual(parseArguments(["--root", "/r", "--output", "/o"]), {
    root: "/r",
    output: "/o",
    types: true,
  });
  assert.equal(
    parseArguments(["--root", "/r", "--output", "/o", "--skip-types"]).types,
    false,
  );
});

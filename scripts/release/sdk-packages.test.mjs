import assert from "node:assert/strict";
import {
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { vendoredSourceNotice } from "../../demo/scripts/lib/vendored-imports.mjs";
import {
  checkPublishSet,
  forbiddenPackedPath,
  loadableSpecifiers,
  loadFailures,
  packedManifestProblems,
  packedSecretProblems,
  parseArguments,
  publishedExport,
  publishedExports,
  typesCheck,
  unpublishableSpecifier,
  vendoredImportProblems,
} from "./sdk-packages.mjs";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);

const publishNames = new Set(["@al-ft/a", "@al-ft/b"]);
const workspaceExports = () => ({
  ".": {
    "midgard-source": "./src/index.ts",
    types: "./dist/index.d.ts",
    import: "./dist/index.js",
    require: "./dist/index.cjs",
  },
  "./tests/fixtures": { "midgard-source": "./tests/fixtures.ts" },
});
const cleanManifest = () => ({
  name: "@al-ft/a",
  version: "0.1.0",
  license: "Apache-2.0",
  repository: { type: "git", url: "git+https://example.invalid/repo.git" },
  main: "./dist/index.js",
  types: "./dist/index.d.ts",
  exports: {
    ".": {
      import: { types: "./dist/index.d.ts", default: "./dist/index.js" },
      require: { types: "./dist/index.d.cts", default: "./dist/index.cjs" },
    },
  },
  dependencies: { "@al-ft/b": "0.1.0", effect: "^3.14.22" },
});
const cleanFiles = [
  "package.json",
  "LICENSE",
  "README.md",
  "dist/index.js",
  "dist/index.cjs",
  "dist/index.d.ts",
  "dist/index.d.cts",
];

test("a registry-installable manifest has no problems", () => {
  assert.deepEqual(
    packedManifestProblems(
      cleanManifest(),
      cleanFiles,
      publishNames,
      workspaceExports(),
    ),
    [],
  );
});

test("the registry form of an export drops the source condition and types each runtime", () => {
  const [source, typed, esmOnly, plain] = [
    { "midgard-source": "./tests/fixtures.ts" },
    workspaceExports()["."],
    {
      "midgard-source": "./src/uplc.ts",
      types: "./dist/uplc.d.ts",
      import: "./dist/uplc.js",
    },
    "./dist/data.json",
  ];
  assert.equal(publishedExport(source), undefined);
  assert.deepEqual(publishedExport(typed), cleanManifest().exports["."]);
  assert.deepEqual(publishedExport(esmOnly), {
    import: { types: "./dist/uplc.d.ts", default: "./dist/uplc.js" },
  });
  assert.equal(publishedExport(plain), plain);
  assert.deepEqual(
    publishedExports(workspaceExports()),
    cleanManifest().exports,
  );
});

test("refuses a packed export that is not the registry form of the workspace export", () => {
  const manifest = cleanManifest();
  manifest.exports["."] = workspaceExports()["."];
  manifest.exports["./extra"] = "./dist/index.js";
  assert.deepEqual(
    packedManifestProblems(
      manifest,
      cleanFiles,
      publishNames,
      workspaceExports(),
    ),
    [
      "export . keeps the workspace-only midgard-source condition",
      `export . is not the registry form of its workspace export; publish it as ${JSON.stringify(cleanManifest().exports["."])}`,
      "export ./extra has no workspace export",
    ],
  );
});

test("refuses a tarball without a licence file", () => {
  assert.deepEqual(
    packedManifestProblems(
      cleanManifest(),
      cleanFiles.filter((file) => file !== "LICENSE"),
      publishNames,
    ),
    ["LICENSE file is not in the tarball"],
  );
});

test("refuses package imports whose targets are not shipped", () => {
  const manifest = cleanManifest();
  manifest.imports = { "#journal": "./src/journal.ts" };
  assert.deepEqual(packedManifestProblems(manifest, cleanFiles, publishNames), [
    "imports #journal -> ./src/journal.ts is not in the tarball",
  ]);
  manifest.imports = {};
  assert.deepEqual(
    packedManifestProblems(manifest, cleanFiles, publishNames),
    [],
  );
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
  assert.deepEqual(packedManifestProblems(manifest, cleanFiles, publishNames), [
    "dependencies.mpf is a local, git or URL specifier (github:owner/repo#abc)",
    "peerDependencies.lucid is an unpinned range (latest)",
  ]);
});

test("refuses a dependency on a workspace package outside the publish set", () => {
  const manifest = cleanManifest();
  manifest.dependencies["@al-ft/native-transport"] = "0.1.0";
  assert.deepEqual(packedManifestProblems(manifest, cleanFiles, publishNames), [
    "dependencies.@al-ft/native-transport is a workspace package outside the publish set",
  ]);
});

test("refuses private, unlicensed, unversioned and repository-less manifests", () => {
  const manifest = {
    ...cleanManifest(),
    private: true,
    version: "latest",
    license: undefined,
    repository: undefined,
  };
  assert.deepEqual(packedManifestProblems(manifest, cleanFiles, publishNames), [
    "package is private",
    "version latest is not an exact version",
    "license is missing",
    "repository is missing",
  ]);
});

test("refuses exports a consumer cannot resolve", () => {
  const manifest = cleanManifest();
  manifest.exports["./tests/fixtures"] = {
    "midgard-source": "./tests/fixtures.ts",
  };
  manifest.exports["./uplc"] = {
    import: { types: "./dist/uplc.d.ts", default: "./dist/uplc.js" },
  };
  assert.deepEqual(packedManifestProblems(manifest, cleanFiles, publishNames), [
    "export ./tests/fixtures resolves only under the midgard-source condition",
    "export ./uplc [import>types] -> ./dist/uplc.d.ts is not in the tarball",
    "export ./uplc [import>default] -> ./dist/uplc.js is not in the tarball",
  ]);
  assert.deepEqual(
    packedManifestProblems(
      { ...cleanManifest(), main: "./dist/missing.js" },
      cleanFiles,
      publishNames,
    ),
    ["main ./dist/missing.js is not in the tarball"],
  );
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
    "dist/.contrib-build-v1.json",
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
    ),
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

// A workspace package whose `#vendored` import names the installed
// `dependency` for sources and its built copy otherwise, and the tarball root
// a correct pack of it extracts to.
const vendoredFixture = (dependency = "@x/vendored") => {
  const base = mkdtempSync(join(tmpdir(), "sdk-packages-vendored-"));
  const installed = join(base, "store", dependency);
  const directory = `vendor/${dependency.split("/").pop()}`;
  mkdirSync(join(installed, "dist"), { recursive: true });
  writeFileSync(
    join(installed, "package.json"),
    JSON.stringify({
      name: dependency,
      type: "module",
      exports: {
        ".": { types: "./dist/index.d.ts", import: "./dist/index.js" },
      },
      dependencies: { hash: "^2.1.4" },
    }),
  );
  writeFileSync(join(installed, "dist/index.js"), "export const root = 1;\n");
  writeFileSync(join(installed, "dist/index.d.ts"), "export const root: 1;\n");
  const workspace = join(base, "workspace");
  mkdirSync(join(workspace, "node_modules", dependency, ".."), {
    recursive: true,
  });
  symlinkSync(installed, join(workspace, "node_modules", dependency), "dir");
  const packedImport = {
    types: `./${directory}/dist/index.d.ts`,
    import: `./${directory}/dist/index.js`,
  };
  writeFileSync(
    join(workspace, "package.json"),
    JSON.stringify({
      imports: {
        "#vendored": { "midgard-source": dependency, ...packedImport },
      },
    }),
  );
  const packed = join(base, "packed");
  cpSync(installed, join(packed, directory), { recursive: true });
  const notice = vendoredSourceNotice(dependency);
  if (notice !== undefined)
    writeFileSync(join(packed, directory, "SOURCE.md"), notice);
  const manifest = {
    ...cleanManifest(),
    imports: { "#vendored": packedImport },
    dependencies: { hash: "^2.1.4" },
  };
  return { workspace, packed, manifest };
};

test("a vendored package import ships the installed package byte for byte", () => {
  const { workspace, packed, manifest } = vendoredFixture();
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), []);
  writeFileSync(
    join(packed, "vendor/vendored/dist/index.js"),
    "export const root = 2;\n",
  );
  writeFileSync(join(packed, "vendor/vendored/extra.js"), "\n");
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), [
    "vendor/vendored/dist/index.js differs from the installed @x/vendored",
    "vendor/vendored/extra.js differs from the installed @x/vendored",
  ]);
});

test("a vendored package import needs its export map and its dependencies", () => {
  const { workspace, packed, manifest } = vendoredFixture();
  manifest.imports["#vendored"] = "./vendor/vendored/dist/index.js";
  manifest.dependencies = {};
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), [
    `imports #vendored is not the export map of the vendored @x/vendored; publish it as ${JSON.stringify(vendoredFixture().manifest.imports["#vendored"])}`,
    "vendored @x/vendored needs hash, which is not a dependency",
  ]);
  const missing = vendoredFixture();
  assert.deepEqual(
    vendoredImportProblems(
      join(missing.packed, "elsewhere"),
      missing.manifest,
      missing.workspace,
    ),
    ["vendored @x/vendored is not in the tarball"],
  );
});

test("built and deployed copies resolve the vendored package import too", () => {
  const { workspace, packed, manifest } = vendoredFixture();
  const sourceOnly = {
    imports: { "#vendored": { "midgard-source": "@x/vendored" } },
  };
  writeFileSync(join(workspace, "package.json"), JSON.stringify(sourceOnly));
  const declared = JSON.stringify({
    "midgard-source": "@x/vendored",
    ...manifest.imports["#vendored"],
  });
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), [
    `workspace imports #vendored does not resolve the vendored @x/vendored outside the midgard-source condition; declare it as ${declared}`,
  ]);
});

test("the vendored Merkle Patricia Forestry ships its source notice", () => {
  const { workspace, packed, manifest } = vendoredFixture(
    "@aiken-lang/merkle-patricia-forestry",
  );
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), []);
  writeFileSync(
    join(packed, "vendor/merkle-patricia-forestry/SOURCE.md"),
    "# Source\n",
  );
  const problem =
    "vendor/merkle-patricia-forestry/SOURCE.md is missing or is not the source notice of the vendored @aiken-lang/merkle-patricia-forestry";
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), [
    problem,
  ]);
  rmSync(join(packed, "vendor/merkle-patricia-forestry/SOURCE.md"));
  assert.deepEqual(vendoredImportProblems(packed, manifest, workspace), [
    problem,
  ]);
});

// A repository root whose SDK package has `tsc` as its TypeScript 5.8
// compiler and, unless `typesNode` is false, an installed @types/node.
const typesFixture = (tsc, { typesNode = true } = {}) => {
  const root = mkdtempSync(join(tmpdir(), "sdk-packages-types-"));
  const sdk = join(root, "demo/midgard-sdk");
  mkdirSync(join(sdk, "node_modules/typescript-5-8/bin"), { recursive: true });
  writeFileSync(join(sdk, "package.json"), '{"name":"@al-ft/midgard-sdk"}\n');
  writeFileSync(join(sdk, "node_modules/typescript-5-8/bin/tsc"), tsc);
  if (typesNode)
    mkdirSync(join(sdk, "node_modules/@types/node"), { recursive: true });
  const installRoot = join(root, "install");
  mkdirSync(join(installRoot, "node_modules"), { recursive: true });
  return { root, installRoot };
};

const fakeTsc = (stdout, status) =>
  `process.stdout.write(${JSON.stringify(stdout)});\nprocess.exitCode = ${status};\n`;

test("the type probe reports a compiler failure that names no error", () => {
  const { root, installRoot } = typesFixture(
    fakeTsc("tsc ran out of memory\n", 134),
  );
  assert.deepEqual(typesCheck(root, installRoot, []), [
    "types: tsc exited 134 without reporting an error; output tail:\ntsc ran out of memory",
  ]);
});

test("the type probe reports errors in the probe, the publish set and no file, not third-party ones", () => {
  const { root, installRoot } = typesFixture(
    fakeTsc(
      [
        "error TS2688: Cannot find type definition file for 'node'.",
        "cjs.cts(1,1): error TS1479: The current file is a CommonJS module.",
        "../node_modules/@al-ft/midgard-sdk/dist/index.d.cts(2,3): error TS2304: Cannot find name 'x'.",
        "../node_modules/@al-ft/midgard-validation/vendor/merkle-patricia-forestry/dist/trie.d.ts(99,16): error TS2304: Cannot find name 'bool'.",
        "../node_modules/effect/dist/index.d.ts(1,1): error TS2304: Cannot find name 'y'.",
        "",
      ].join("\n"),
      2,
    ),
  );
  assert.deepEqual(typesCheck(root, installRoot, []), [
    "types: error TS2688: Cannot find type definition file for 'node'.",
    "types: cjs.cts(1,1): error TS1479: The current file is a CommonJS module.",
    "types: ../node_modules/@al-ft/midgard-sdk/dist/index.d.cts(2,3): error TS2304: Cannot find name 'x'.",
  ]);
  const thirdPartyOnly = typesFixture(
    fakeTsc(
      "../node_modules/effect/dist/index.d.ts(1,1): error TS2304: Cannot find name 'y'.\n",
      2,
    ),
  );
  assert.deepEqual(
    typesCheck(thirdPartyOnly.root, thirdPartyOnly.installRoot, []),
    [],
  );
});

test("the type probe reports an @types/node it cannot link", () => {
  const { root, installRoot } = typesFixture(fakeTsc("", 0), {
    typesNode: false,
  });
  assert.deepEqual(typesCheck(root, installRoot, []), [
    "types: dependency @types/node is not installed in the workspace",
  ]);
});

test("a failed pack leaves no licence in the package and keeps its built vendored copy", () => {
  const root = mkdtempSync(join(tmpdir(), "sdk-packages-failed-pack-"));
  const packFiles = join(repositoryRoot, "scripts/release/pack-files.mjs");
  const { packageManager } = JSON.parse(
    readFileSync(join(repositoryRoot, "demo/package.json"), "utf8"),
  );
  const packageDir = join(root, "demo/midgard-core");
  mkdirSync(join(packageDir, "dist"), { recursive: true });
  writeFileSync(join(packageDir, "dist/index.js"), "export {};\n");
  mkdirSync(join(packageDir, "vendor/vendored"), { recursive: true });
  writeFileSync(join(packageDir, "vendor/vendored/index.js"), "export {};\n");
  writeFileSync(
    join(packageDir, "package.json"),
    JSON.stringify({
      name: "@al-ft/midgard-core",
      version: "0.1.0",
      packageManager,
      imports: {
        "#vendored": {
          "midgard-source": "@x/vendored",
          import: "./vendor/vendored/index.js",
        },
      },
      scripts: {
        prepack: `node ${JSON.stringify(packFiles)} copy && node -e "process.exit(1)"`,
        postpack: `node ${JSON.stringify(packFiles)} remove`,
      },
    }),
  );
  assert.throws(
    () => checkPublishSet({ root, output: join(root, "check") }),
    /pnpm pack .*failed/u,
  );
  assert.equal(existsSync(join(packageDir, "LICENSE")), false);
  assert.ok(existsSync(join(packageDir, "vendor/vendored/index.js")));
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
      "./esm-nested": {
        import: { types: "./dist/esm.d.ts", default: "./dist/esm.js" },
      },
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
    "@al-ft/a/esm-nested",
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

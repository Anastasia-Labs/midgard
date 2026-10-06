import assert from "node:assert/strict";
import {
  readFileSync,
  writeFileSync,
  utimesSync,
  mkdirSync,
  cpSync,
  renameSync,
} from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { checkBuild } from "./build.mjs";
import {
  BUILD_TRACE,
  buildEnvironment,
  unboundReads,
} from "./build-inputs.mjs";
import {
  atomicJson,
  compiledDependencies,
  filesUnder,
  inputIdentity,
  outputIdentity,
} from "./files.mjs";
import { enrollBuilds } from "./enroll-builds.mjs";
import { fixture } from "./fixture.test-support.mjs";

const stamp = (root, env = process.env) =>
  atomicJson(resolve(root, "demo/example/dist/.contrib-build-v1.json"), {
    schema: "midgard-contrib-build/v1",
    root,
    package: "example",
    reads: BUILD_TRACE,
    inputs: inputIdentity(root, "example"),
    environment: buildEnvironment(root, "example", env),
    outputs: outputIdentity(root, "demo/example/dist"),
    dependencies: compiledDependencies(root, "example"),
  });
const editPackage = (root, name, edit) => {
  const path = resolve(root, `demo/${name}/package.json`);
  const pkg = JSON.parse(readFileSync(path));
  edit(pkg);
  writeFileSync(path, JSON.stringify(pkg));
};
const workspaceDependency = (root, name = "dependency") => {
  mkdirSync(resolve(root, `demo/${name}/src`), { recursive: true });
  mkdirSync(resolve(root, `demo/${name}/dist`), { recursive: true });
  writeFileSync(
    resolve(root, `demo/${name}/package.json`),
    JSON.stringify({
      name,
      scripts: { build: "compiler", "build:contrib-raw": "compiler" },
    }),
  );
  writeFileSync(
    resolve(root, `demo/${name}/src/index.ts`),
    "export const y = 1;",
  );
  writeFileSync(
    resolve(root, `demo/${name}/dist/index.js`),
    "export const y = 1;",
  );
};

test("repository receipts ignore generated site and spec outputs while binding new source inputs", (t) => {
  const root = fixture(t);
  writeFileSync(
    resolve(root, ".gitignore"),
    "technical-spec/*.pdf\ndocs-site/.source/\n",
  );
  const before = inputIdentity(root, "@repository").sha256;
  mkdirSync(resolve(root, "technical-spec"));
  writeFileSync(resolve(root, "technical-spec/midgard.pdf"), "compiled spec");
  mkdirSync(resolve(root, "docs-site/.source"), { recursive: true });
  writeFileSync(
    resolve(root, "docs-site/.source/generated.ts"),
    "compiled site",
  );
  assert.equal(inputIdentity(root, "@repository").sha256, before);
  mkdirSync(resolve(root, "scripts"));
  writeFileSync(resolve(root, "scripts/new-source.mjs"), "export const x=1;");
  assert.notEqual(inputIdentity(root, "@repository").sha256, before);
});

test("dist freshness ignores timestamps and binds actual emitted bytes", (t) => {
  const root = fixture(t);
  assert.equal(checkBuild(root, "example").status, "missing");
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  const source = resolve(root, "demo/example/src/index.ts");
  writeFileSync(source, "export const x = 2;");
  utimesSync(source, 1, 1);
  assert.match(checkBuild(root, "example").reason, /input closure changed/u);
  stamp(root);
  writeFileSync(
    resolve(root, "demo/example/dist/index.js"),
    "foreign compiled artifact",
  );
  assert.match(checkBuild(root, "example").reason, /contents changed/u);
});

test("source directories named like outputs remain bound while owned outputs are excluded", (t) => {
  const root = fixture(t);
  for (const name of [
    "build",
    "target",
    "dist",
    "coverage",
    "logs",
    ".tmp",
    ".next",
    "deploymentInfo",
  ]) {
    const source = resolve(root, `demo/example/src/${name}/action.ts`);
    mkdirSync(resolve(source, ".."), { recursive: true });
    writeFileSync(source, "export const x=1;");
    stamp(root);
    const before = inputIdentity(root, "@repository").sha256;
    writeFileSync(source, "export const x=2;");
    assert.equal(checkBuild(root, "example").status, "stale", name);
    assert.notEqual(inputIdentity(root, "@repository").sha256, before, name);
    assert.ok(filesUnder(resolve(root, "demo/example/src")).includes(source));
  }
  mkdirSync(resolve(root, "demo/example/native/target"), { recursive: true });
  writeFileSync(resolve(root, "demo/example/native/Cargo.toml"), "[package]");
  writeFileSync(resolve(root, "demo/example/native/target/generated"), "one");
  mkdirSync(resolve(root, "onchain/aiken/build"), { recursive: true });
  writeFileSync(resolve(root, "onchain/aiken/aiken.toml"), "name='test'");
  writeFileSync(resolve(root, "onchain/aiken/build/generated"), "one");
  stamp(root);
  const before = inputIdentity(root, "@repository").sha256;
  for (const path of [
    "demo/example/dist/index.js",
    "demo/example/native/target/generated",
    "onchain/aiken/build/generated",
  ]) {
    writeFileSync(resolve(root, path), "two");
  }
  assert.equal(inputIdentity(root, "@repository").sha256, before);
  // Emitted bytes are checked independently even though they are not source.
  assert.match(checkBuild(root, "example").reason, /contents changed/u);
});

test("lockfile, exports, native sources and dependency facets invalidate", (t) => {
  const root = fixture(t);
  mkdirSync(resolve(root, "demo/dependency/src"), { recursive: true });
  writeFileSync(
    resolve(root, "demo/dependency/package.json"),
    JSON.stringify({ name: "dependency" }),
  );
  writeFileSync(
    resolve(root, "demo/dependency/src/facet.ts"),
    "export const y = 1;",
  );
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.dependencies = { dependency: "workspace:*" };
  writeFileSync(path, JSON.stringify(pkg));
  for (const changed of [
    "demo/pnpm-lock.yaml",
    "demo/node_modules/.pnpm/lock.yaml",
    "demo/dependency/src/facet.ts",
    "demo/example/package.json",
    "demo/example/native/Cargo.lock",
    "scripts/pnpm.mjs",
    "scripts/bin/pnpm",
  ]) {
    stamp(root);
    mkdirSync(resolve(root, changed, ".."), { recursive: true });
    writeFileSync(
      resolve(root, changed),
      `${readFileSync(resolve(root, changed), { flag: "a+" })} `,
    );
    assert.equal(checkBuild(root, "example").status, "stale", changed);
  }
});

test("copying an identical dist from another checkout is refused", (t) => {
  const first = fixture(t);
  const second = fixture(t);
  stamp(first);
  cpSync(
    resolve(first, "demo/example/dist"),
    resolve(second, "demo/example/dist"),
    { recursive: true },
  );
  assert.match(checkBuild(second, "example").reason, /another.*checkout/u);
});

test("watcher native outputs have separate ownership from compiled JavaScript", (t) => {
  const root = fixture(t);
  const directory = "demo/midgard-watcher/dist";
  renameSync(
    resolve(root, "demo/example"),
    resolve(root, "demo/midgard-watcher"),
  );
  atomicJson(resolve(root, "demo/midgard-watcher/package.json"), {
    name: "midgard-watcher",
  });
  const before = outputIdentity(root, directory).sha256;
  mkdirSync(resolve(root, directory, "native"));
  writeFileSync(
    resolve(root, directory, "native/midgard-chain-sync"),
    "Go binary",
  );
  assert.equal(
    outputIdentity(root, directory).sha256,
    before,
    "adding the separately guarded Go binary must not invalidate TypeScript",
  );
  writeFileSync(
    resolve(root, directory, "native/unowned.js"),
    "another output",
  );
  assert.notEqual(
    outputIdentity(root, directory).sha256,
    before,
    "unowned siblings in the native directory remain compiled outputs",
  );
  const withSibling = outputIdentity(root, directory).sha256;
  writeFileSync(resolve(root, directory, "index.js"), "tampered JavaScript");
  assert.notEqual(outputIdentity(root, directory).sha256, withSibling);
  renameSync(
    resolve(root, "demo/midgard-watcher"),
    resolve(root, "demo/example"),
  );
  const ordinary = outputIdentity(root, "demo/example/dist").sha256;
  writeFileSync(
    resolve(root, "demo/example/dist/native/midgard-chain-sync"),
    "changed",
  );
  assert.notEqual(
    outputIdentity(root, "demo/example/dist").sha256,
    ordinary,
    "a native directory in another package remains a compiled output",
  );
});

test("build enrollment is idempotent and refuses unknown wrappers", (t) => {
  const root = fixture(t);
  assert.throws(() => enrollBuilds(root), /unguarded/u);
  assert.equal(enrollBuilds(root, { write: true }).length, 1);
  assert.deepEqual(enrollBuilds(root), []);
});

test("a variable the build recipe names is bound; one it cannot name is never fresh", (t) => {
  const root = fixture(t);
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] =
      'NODE_OPTIONS="${NODE_OPTIONS:-} --max-old-space-size=64" tsup src/index.ts';
  });
  writeFileSync(
    resolve(root, "demo/example/tsup.config.ts"),
    "export default { define: { FLAG: JSON.stringify(process.env.EXAMPLE_FLAG) } };",
  );
  const env = { ...process.env, NODE_OPTIONS: "", EXAMPLE_FLAG: "one" };
  assert.deepEqual(
    Object.keys(buildEnvironment(root, "example", env).variables),
    ["EXAMPLE_FLAG", "NODE_OPTIONS"],
  );
  stamp(root, env);
  assert.equal(checkBuild(root, "example", { env }).status, "fresh");
  assert.equal(
    checkBuild(root, "example", { env: { ...env, UNNAMED: "x" } }).status,
    "fresh",
  );
  for (const changed of [
    { EXAMPLE_FLAG: "two" },
    { EXAMPLE_FLAG: undefined },
    { NODE_OPTIONS: "--import ./hook.mjs" },
  ])
    assert.match(
      checkBuild(root, "example", { env: { ...env, ...changed } }).reason,
      /build environment changed/u,
      JSON.stringify(changed),
    );
  for (const config of [
    "export default { env: { ...process.env } };",
    "import { env } from 'node:process'; export default { define: env };",
  ]) {
    writeFileSync(resolve(root, "demo/example/tsup.config.ts"), config);
    stamp(root, env);
    assert.match(
      checkBuild(root, "example", { env }).reason,
      /never provably fresh/u,
      config,
    );
  }
  writeFileSync(
    resolve(root, "demo/example/tsup.config.ts"),
    "export default {};",
  );
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] = "tsup src/index.ts --env.STAMP=$(date)";
  });
  stamp(root, env);
  assert.match(
    checkBuild(root, "example", { env }).reason,
    /never provably fresh/u,
  );
});

test("a workspace dist the sources can inline is bound even without a runtime dependency", (t) => {
  const root = fixture(t);
  workspaceDependency(root);
  editPackage(root, "example", (pkg) => {
    pkg.devDependencies = { dependency: "workspace:*" };
  });
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  writeFileSync(
    resolve(root, "demo/dependency/dist/index.js"),
    "export const y = 2;",
  );
  // Not imported, so a devDependency's dist cannot reach the bundle.
  assert.equal(checkBuild(root, "example").status, "fresh");
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    'export { y } from "dependency/inner";\n',
  );
  stamp(root);
  assert.deepEqual(
    compiledDependencies(root, "example").map(({ name }) => name),
    ["dependency"],
  );
  assert.equal(checkBuild(root, "example").status, "fresh");
  writeFileSync(
    resolve(root, "demo/dependency/dist/index.js"),
    "export const y = 3;",
  );
  assert.match(
    checkBuild(root, "example").reason,
    /compiled dependency contents changed/u,
  );
});

test("build inputs outside the closure keep a dist from ever being fresh", (t) => {
  const root = fixture(t);
  workspaceDependency(root, "undeclared");
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    'import { y } from "undeclared";\nexport const x = y;\n',
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /sources name workspace package undeclared, which example does not declare/u,
  );
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    "export const x = 1;\n",
  );
  writeFileSync(
    resolve(root, "demo/example/tsconfig.json"),
    JSON.stringify({ include: ["src", "../undeclared/src"] }),
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /includes \.\.\/undeclared\/src outside the input closure/u,
  );
  editPackage(root, "example", (pkg) => {
    pkg.devDependencies = { undeclared: "workspace:*" };
  });
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
});

test("a stamp from a build whose reads were not traced is never fresh", (t) => {
  const root = fixture(t);
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  const path = resolve(root, "demo/example/dist/.contrib-build-v1.json");
  const { reads, ...untraced } = JSON.parse(readFileSync(path, "utf8"));
  assert.equal(reads, BUILD_TRACE);
  writeFileSync(path, JSON.stringify(untraced));
  assert.match(
    checkBuild(root, "example").reason,
    /stamp predates traced builds/u,
  );
});

// Each case changes a file or variable the build reads after the stamp is
// written; none of them may leave the dist fresh.
const outsidePackage = (root) => {
  mkdirSync(resolve(root, "demo/undeclared/src"), { recursive: true });
  writeFileSync(
    resolve(root, "demo/undeclared/package.json"),
    JSON.stringify({ name: "undeclared" }),
  );
  writeFileSync(
    resolve(root, "demo/undeclared/src/index.ts"),
    "export const y = 1;\n",
  );
};
const generator = (root, path, text) => {
  mkdirSync(resolve(root, "demo/example/scripts"), { recursive: true });
  writeFileSync(resolve(root, "demo/example", path), text);
};
const PROFILE_A = { ...process.env, PROFILE: "a" };
const PROFILE_B = { ...process.env, PROFILE: "b" };

test("a tsconfig path alias out of the closure is never fresh", (t) => {
  const root = fixture(t);
  outsidePackage(root);
  writeFileSync(
    resolve(root, "demo/example/tsconfig.json"),
    JSON.stringify({
      compilerOptions: {
        baseUrl: ".",
        paths: { "@ext/*": ["../undeclared/src/*"] },
      },
      include: ["src"],
    }),
  );
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    'export { y } from "@ext/index";\n',
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /maps a path to \.\.\/undeclared\/src\/\* outside the input closure/u,
  );
});

test("a relative source import out of the closure is never fresh", (t) => {
  const root = fixture(t);
  outsidePackage(root);
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    'export { y } from "../../undeclared/src/index";\n',
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /imports \.\.\/\.\.\/undeclared\/src\/index outside the input closure/u,
  );
});

test("a recipe that runs another package script is never fresh", (t) => {
  const root = fixture(t);
  generator(root, "scripts/gen.mjs", "console.log(process.env.PROFILE);\n");
  editPackage(root, "example", (pkg) => {
    pkg.scripts.gen = "node scripts/gen.mjs";
    pkg.scripts["build:contrib-raw"] = "pnpm run gen && tsup src/index.ts";
  });
  stamp(root, PROFILE_A);
  assert.match(
    checkBuild(root, "example", { env: PROFILE_A }).reason,
    /runs pnpm, which the guard does not scan/u,
  );
});

test("a module the tsup config imports is scanned for the variables it reads", (t) => {
  const root = fixture(t);
  writeFileSync(
    resolve(root, "demo/example/build-env.mjs"),
    "export const define = { PROFILE: JSON.stringify(process.env.PROFILE) };\n",
  );
  writeFileSync(
    resolve(root, "demo/example/tsup.config.ts"),
    'import { define } from "./build-env.mjs";\nexport default { define };\n',
  );
  stamp(root, PROFILE_A);
  assert.equal(checkBuild(root, "example", { env: PROFILE_A }).status, "fresh");
  assert.match(
    checkBuild(root, "example", { env: PROFILE_B }).reason,
    /build environment changed/u,
  );
  // A helper the closure does not bind cannot be scanned at all.
  mkdirSync(resolve(root, "config/build"), { recursive: true });
  writeFileSync(
    resolve(root, "config/build/env.mjs"),
    "export const define = {};\n",
  );
  writeFileSync(
    resolve(root, "demo/example/tsup.config.ts"),
    'import { define } from "../../config/build/env.mjs";\nexport default { define };\n',
  );
  stamp(root, PROFILE_A);
  assert.match(
    checkBuild(root, "example", { env: PROFILE_A }).reason,
    /build code config\/build\/env\.mjs is outside the input closure/u,
  );
});

test("node flags before a recipe script do not hide it from the scan", (t) => {
  const root = fixture(t);
  generator(root, "scripts/gen.mjs", "console.log(process.env.PROFILE);\n");
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] =
      "node --enable-source-maps scripts/gen.mjs && tsup src/index.ts";
  });
  stamp(root, PROFILE_A);
  assert.equal(checkBuild(root, "example", { env: PROFILE_A }).status, "fresh");
  assert.match(
    checkBuild(root, "example", { env: PROFILE_B }).reason,
    /build environment changed/u,
  );
  for (const [recipe, reason] of [
    ["node --require ./hook.cjs scripts/gen.mjs", /passes node --require/u],
    ["node -r ./hook.cjs scripts/gen.mjs", /passes node -r/u],
    ["node --env-file=.env scripts/gen.mjs", /passes node --env-file/u],
  ]) {
    editPackage(root, "example", (pkg) => {
      pkg.scripts["build:contrib-raw"] = recipe;
    });
    stamp(root, PROFILE_A);
    assert.match(
      checkBuild(root, "example", { env: PROFILE_A }).reason,
      reason,
      recipe,
    );
  }
});

test("a tsconfig extending a file out of the closure is never fresh", (t) => {
  const root = fixture(t);
  mkdirSync(resolve(root, "config/ts"), { recursive: true });
  writeFileSync(
    resolve(root, "config/ts/base.json"),
    JSON.stringify({ compilerOptions: { target: "es2020" } }),
  );
  writeFileSync(
    resolve(root, "demo/example/tsconfig.json"),
    JSON.stringify({ extends: "../../config/ts/base.json", include: ["src"] }),
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /tsconfig config\/ts\/base\.json is outside the input closure/u,
  );
  for (const [config, reason] of [
    [{ files: ["../../config/ts/entry.ts"] }, /lists \.\.\/\.\.\/config/u],
    [
      { references: [{ path: "../../config/ts" }] },
      /references \.\.\/\.\.\/config/u,
    ],
  ]) {
    writeFileSync(
      resolve(root, "demo/example/tsconfig.json"),
      JSON.stringify(config),
    );
    stamp(root);
    assert.match(checkBuild(root, "example").reason, reason);
  }
});

test("a recipe running a TypeScript script or an unknown tool is never fresh", (t) => {
  const root = fixture(t);
  generator(root, "scripts/gen.ts", "console.log(process.env.PROFILE);\n");
  for (const [recipe, reason] of [
    [
      "node scripts/gen.ts && tsup src/index.ts",
      /runs node on scripts\/gen\.ts/u,
    ],
    [
      "tsx scripts/gen.ts && tsup src/index.ts",
      /runs tsx, which the guard does not scan/u,
    ],
    ["tsup src/index.ts --onSuccess 'node x.mjs'", /passes tsup --onSuccess/u],
    ["tsup src/index.ts; true", /uses shell syntax ";"/u],
    ["tsup src/index.ts > log", /uses shell syntax ">"/u],
  ]) {
    editPackage(root, "example", (pkg) => {
      pkg.scripts["build:contrib-raw"] = recipe;
    });
    stamp(root, PROFILE_A);
    assert.match(
      checkBuild(root, "example", { env: PROFILE_A }).reason,
      reason,
      recipe,
    );
  }
});

test("only reads the stamp binds may produce a stamped dist", (t) => {
  const root = fixture(t);
  outsidePackage(root);
  const at = (path) => resolve(root, path);
  const store = at("demo/node_modules/.pnpm/tool@1/node_modules/tool");
  mkdirSync(store, { recursive: true });
  writeFileSync(resolve(store, "index.js"), "");
  const reasons = (records) =>
    unboundReads(root, "example", records, { dependencies: [] });
  assert.deepEqual(
    reasons([
      ["read", at("demo/example/src/index.ts")],
      ["read", at("demo/example/package.json")],
      ["read", at("demo/example/dist/index.js")],
      ["read", resolve(store, "index.js")],
      ["write", at("demo/example/tsup.config.bundled_1.mjs")],
      ["read", at("demo/example/tsup.config.bundled_1.mjs")],
      ["virtual", "tsup:shims"],
    ]),
    [],
  );
  assert.match(
    reasons([["read", at("demo/undeclared/src/index.ts")]]).join(),
    /read demo\/undeclared\/src\/index\.ts, which its input closure does not bind/u,
  );
  mkdirSync(at("demo/example/coverage"));
  writeFileSync(at("demo/example/coverage/data.json"), "{}");
  assert.match(
    reasons([["read", at("demo/example/coverage/data.json")]]).join(),
    /coverage\/data\.json, which its input closure does not bind/u,
  );
  assert.match(
    reasons([["read", at("demo/example/gone.ts")]]).join(),
    /which no longer exists/u,
  );
  assert.match(
    reasons([["untraced", "esbuild context"]]).join(),
    /esbuild context, which is not traced/u,
  );
});

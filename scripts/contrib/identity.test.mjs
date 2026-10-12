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
import { probePackageDist } from "../preflight/probes.mjs";
import {
  BUILD_TRACE,
  buildEnvironment,
  environmentRefusals,
  unboundReads,
  unstampedFix,
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

test("native outputs have separate ownership from compiled JavaScript", (t) => {
  const root = fixture(t);
  const directory = "demo/l1-node-transport/dist";
  renameSync(
    resolve(root, "demo/example"),
    resolve(root, "demo/l1-node-transport"),
  );
  atomicJson(resolve(root, "demo/l1-node-transport/package.json"), {
    name: "@al-ft/l1-node-transport",
  });
  const before = outputIdentity(root, directory).sha256;
  mkdirSync(resolve(root, directory, "native"));
  writeFileSync(
    resolve(root, directory, "native/midgard-l1-node-transport"),
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
    resolve(root, "demo/l1-node-transport"),
    resolve(root, "demo/example"),
  );
  atomicJson(resolve(root, "demo/example/package.json"), { name: "example" });
  const ordinary = outputIdentity(root, "demo/example/dist").sha256;
  writeFileSync(
    resolve(root, "demo/example/dist/native/midgard-l1-node-transport"),
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
    { NODE_OPTIONS: "--max-old-space-size=32" },
  ])
    assert.match(
      checkBuild(root, "example", { env: { ...env, ...changed } }).reason,
      /build environment changed/u,
      JSON.stringify(changed),
    );
  // Variables that load code or substitute esbuild are refused outright,
  // whether or not the recipe names them.
  for (const [changed, refusal] of [
    [{ NODE_OPTIONS: "--import ./hook.mjs" }, /NODE_OPTIONS passes --import/u],
    [{ NODE_OPTIONS: "-r ./hook.cjs" }, /NODE_OPTIONS passes -r/u],
    [
      { NODE_OPTIONS: "--max-old-space-size=64 --require=./hook.cjs" },
      /NODE_OPTIONS passes --require/u,
    ],
    [{ ESBUILD_BINARY_PATH: "/tmp/esbuild" }, /ESBUILD_BINARY_PATH is set/u],
  ]) {
    assert.deepEqual(environmentRefusals({ ...env, ...changed }).length, 1);
    const verdict = checkBuild(root, "example", {
      env: { ...env, ...changed },
    });
    assert.equal(verdict.status, "stale", JSON.stringify(changed));
    assert.match(verdict.reason, refusal);
    assert.match(verdict.reason, /never provably fresh/u);
  }
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
  // A build that read something unbound leaves its reasons and the fix
  // where the stamp would be; doctor and preflight show both.
  atomicJson(path, {
    schema: "midgard-contrib-build/v1",
    root,
    package: "example",
    unstamped: [
      "TypeScript loaded /x/node_modules/@types/y; types above checkout: /x/node_modules/@types",
    ],
    fix: "move /x/node_modules/@types aside",
  });
  assert.deepEqual(checkBuild(root, "example"), {
    status: "missing",
    reason:
      "example: dist left unstamped: TypeScript loaded /x/node_modules/@types/y; types above checkout: /x/node_modules/@types",
    fix: "move /x/node_modules/@types aside",
  });
  // The advice never replaces the rebuild.
  assert.equal(
    probePackageDist({ root, directory: "demo/example", name: "example" }).fix,
    "move /x/node_modules/@types aside, then pnpm --dir demo --filter example run build",
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

test("build code may import only builtins that run nothing the trace misses", (t) => {
  const root = fixture(t);
  generator(
    root,
    "scripts/gen.mjs",
    'import { readFileSync } from "node:fs";\n',
  );
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] =
      "node scripts/gen.mjs && tsup src/index.ts";
  });
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  for (const specifier of [
    "node:child_process",
    "child_process",
    "node:worker_threads",
    "node:vm",
    "node:net",
    "node:http",
    "node:module",
  ]) {
    generator(root, "scripts/gen.mjs", `import x from "${specifier}";\n`);
    stamp(root);
    assert.match(
      checkBuild(root, "example").reason,
      new RegExp(
        `scripts/gen\\.mjs imports ${specifier}, which build code may not import`,
        "u",
      ),
      specifier,
    );
  }
  generator(root, "scripts/gen.mjs", "\n");
  writeFileSync(
    resolve(root, "demo/example/tsup.config.ts"),
    'import { execSync } from "node:child_process";\nexport default {};\n',
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /tsup\.config\.ts imports node:child_process, which build code may not import/u,
  );
});

test("build code that reaches past the scan or the network is never fresh", (t) => {
  const root = fixture(t);
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] =
      "node scripts/gen.mjs && tsup src/index.ts";
  });
  for (const code of [
    'const { Worker } = module.constructor._load("worker_threads");',
    'process.getBuiltinModule("node:child_process");',
    'process.binding("fs");',
    "require.call(null, 'node:vm');",
    "global.process;",
  ]) {
    generator(root, "scripts/gen.mjs", `${code}\n`);
    stamp(root);
    assert.match(
      checkBuild(root, "example").reason,
      /scripts\/gen\.mjs loads code or reads globals the guard cannot follow/u,
      code,
    );
  }
  for (const name of ["fetch", "WebSocket", "EventSource", "XMLHttpRequest"]) {
    generator(root, "scripts/gen.mjs", `void ${name};\n`);
    stamp(root);
    assert.match(
      checkBuild(root, "example").reason,
      new RegExp(
        `scripts/gen\\.mjs uses ${name}, a network API whose responses no stamp binds`,
        "u",
      ),
      name,
    );
  }
});

test("comments and string text never refuse build code; code beside them still does", (t) => {
  const root = fixture(t);
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] =
      "node scripts/gen.mjs && tsup src/index.ts";
  });
  generator(
    root,
    "scripts/gen.mjs",
    [
      'import { readFileSync } from "node:fs";',
      "// we require the version, then fetch it; eval(this) is never called",
      "/* globalThis, WebSocket, process and module.constructor live here */",
      'const note = "fetch the version // not a comment";',
      "const pattern = /\\/\\/[/*]/u;",
      "const text = `fetch ${note} /* still text */ ${pattern}`;",
      "void readFileSync, text;",
      "",
    ].join("\n"),
  );
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  for (const [code, reason] of [
    ['const u = "http://x"; require(u);', /loads code or reads globals/u],
    ["const r = /[/*]/u; eval(r);", /loads code or reads globals/u],
    ['const t = `text ${eval("1")}`;', /loads code or reads globals/u],
    ['const k = ({})["constructor"];', /loads code or reads globals/u],
    ["// a note\nfetch(url);", /uses fetch, a network API/u],
    ["const t = `text ${fetch(url)}`;", /uses fetch, a network API/u],
    [
      "const p = /x/u; process[p];",
      /uses process in a way that names no variable/u,
    ],
  ]) {
    generator(root, "scripts/gen.mjs", `${code}\n`);
    stamp(root);
    assert.match(checkBuild(root, "example").reason, reason, code);
  }
});

test("a package with pre or post scripts for the recipe is never fresh", (t) => {
  const root = fixture(t);
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  for (const hook of ["prebuild:contrib-raw", "postbuild:contrib-raw"]) {
    editPackage(root, "example", (pkg) => {
      pkg.scripts[hook] = "true";
    });
    assert.match(
      checkBuild(root, "example").reason,
      new RegExp(
        `package\\.json defines ${hook}, a lifecycle script the trace does not follow`,
        "u",
      ),
    );
    editPackage(root, "example", (pkg) => {
      delete pkg.scripts[hook];
    });
  }
});

test("a public directory tsup copies must be a closure input", (t) => {
  const root = fixture(t);
  outsidePackage(root);
  mkdirSync(resolve(root, "demo/example/public"));
  writeFileSync(resolve(root, "demo/example/public/asset.txt"), "a");
  for (const [recipe, reason] of [
    [
      "tsup src/index.ts --publicDir ../undeclared",
      /build recipe copies public directory \.\.\/undeclared, which is outside the input closure/u,
    ],
    [
      "tsup src/index.ts --publicDir=../../outside-public",
      /copies public directory \.\.\/\.\.\/outside-public, which is outside/u,
    ],
    [
      'tsup src/index.ts --publicDir "$DIR"',
      /passes tsup --publicDir it cannot name/u,
    ],
  ]) {
    editPackage(root, "example", (pkg) => {
      pkg.scripts["build:contrib-raw"] = recipe;
    });
    stamp(root);
    assert.match(checkBuild(root, "example").reason, reason, recipe);
  }
  for (const recipe of [
    "tsup src/index.ts --publicDir public",
    "tsup src/index.ts --publicDir",
  ]) {
    editPackage(root, "example", (pkg) => {
      pkg.scripts["build:contrib-raw"] = recipe;
    });
    stamp(root);
    assert.equal(checkBuild(root, "example").status, "fresh", recipe);
  }
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] = "tsup src/index.ts";
  });
  for (const [config, verdict] of [
    [
      'export default { publicDir: "../undeclared" };',
      /tsup\.config\.ts copies public directory \.\.\/undeclared/u,
    ],
    [
      "export default { publicDir: dir };",
      /sets publicDir in a way the guard cannot name/u,
    ],
    ['export default { publicDir: "public" };', "fresh"],
    ["export default { publicDir: true };", "fresh"],
  ]) {
    writeFileSync(resolve(root, "demo/example/tsup.config.ts"), config);
    stamp(root);
    const checked = checkBuild(root, "example");
    if (verdict === "fresh") assert.equal(checked.status, "fresh", config);
    else assert.match(checked.reason, verdict, config);
  }
});

test("the tsconfig of an inlined workspace package is checked too", (t) => {
  const root = fixture(t);
  workspaceDependency(root);
  outsidePackage(root);
  editPackage(root, "example", (pkg) => {
    pkg.devDependencies = { dependency: "workspace:*" };
  });
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    'export { y } from "dependency";\n',
  );
  writeFileSync(
    resolve(root, "demo/example/tsconfig.json"),
    JSON.stringify({ include: ["src"] }),
  );
  writeFileSync(
    resolve(root, "demo/dependency/tsconfig.json"),
    JSON.stringify({ include: ["src", "../undeclared/src"] }),
  );
  stamp(root);
  assert.match(
    checkBuild(root, "example").reason,
    /includes \.\.\/undeclared\/src outside the input closure/u,
  );
  writeFileSync(
    resolve(root, "demo/dependency/tsconfig.json"),
    JSON.stringify({ include: ["src"] }),
  );
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
});

test("only reads the stamp binds may produce a stamped dist", (t) => {
  const root = fixture(t);
  outsidePackage(root);
  const at = (path) => resolve(root, path);
  const store = at("demo/node_modules/.pnpm/tool@1/node_modules/tool");
  mkdirSync(store, { recursive: true });
  writeFileSync(resolve(store, "index.js"), "");
  const cli = at("demo/node_modules/tsup/dist/cli-default.js");
  mkdirSync(resolve(cli, ".."), { recursive: true });
  writeFileSync(cli, "");
  // The recipe runs tsup once: a traced tsup process that reports a bundle.
  const tsup = [
    ["process", cli, 7],
    ["load", cli, 7],
    ["esbuild", "1", 7],
    ["bundle", at("demo/example/src/index.ts"), 7],
  ];
  const reasons = (records, prefix = tsup) =>
    unboundReads(root, "example", [...prefix, ...records], {
      dependencies: [],
    });
  assert.deepEqual(
    reasons([
      ["read", at("demo/example/src/index.ts"), 7],
      ["read", at("demo/example/package.json"), 7],
      ["read", at("demo/example/dist/index.js"), 7],
      ["read", resolve(store, "index.js"), 7],
      ["write", at("demo/example/tsup.config.bundled_1.mjs"), 7],
      ["read", at("demo/example/tsup.config.bundled_1.mjs"), 7],
      ["virtual", "tsup:shims", 7],
      ["service", resolve(store, "index.js"), 7],
    ]),
    [],
  );
  assert.match(
    reasons([["read", at("demo/undeclared/src/index.ts"), 7]]).join(),
    /read demo\/undeclared\/src\/index\.ts, which its input closure does not bind/u,
  );
  // A self-written file excuses only reads that come after the write.
  writeFileSync(at("demo/undeclared/generated.js"), "");
  assert.match(
    reasons([
      ["read", at("demo/undeclared/generated.js"), 7],
      ["write", at("demo/undeclared/generated.js"), 7],
    ]).join(),
    /read demo\/undeclared\/generated\.js, which its input closure does not bind/u,
  );
  assert.deepEqual(
    reasons([
      ["write", at("demo/undeclared/generated.js"), 7],
      ["read", at("demo/undeclared/generated.js"), 7],
    ]),
    [],
  );
  mkdirSync(at("demo/example/coverage"));
  writeFileSync(at("demo/example/coverage/data.json"), "{}");
  assert.match(
    reasons([["read", at("demo/example/coverage/data.json"), 7]]).join(),
    /coverage\/data\.json, which its input closure does not bind/u,
  );
  assert.match(
    reasons([["read", at("demo/example/gone.ts"), 7]]).join(),
    /which no longer exists/u,
  );
  assert.match(
    reasons([["untraced", "esbuild context", 7]]).join(),
    /build ran esbuild context, which the trace cannot follow/u,
  );
  assert.match(
    reasons([["untraced", "child process spawn cat /etc/hostname", 7]]).join(),
    /build ran child process spawn cat \/etc\/hostname, which the trace cannot follow/u,
  );
  assert.match(
    reasons([["service", at("demo/undeclared/src/index.ts"), 7]]).join(),
    /esbuild binary .* which is not installed/u,
  );
  assert.match(
    reasons([["mystery", "x", 7]]).join(),
    /unknown record mystery/u,
  );
  // Coverage: every tsup command of the recipe is a traced process, and
  // each one reports what esbuild bundled beyond its own config.
  assert.match(
    reasons([], []).join(),
    /build recipe runs tsup 1 time\(s\), but the trace saw 0/u,
  );
  assert.match(
    reasons(
      [],
      [
        ["process", cli, 7],
        ["load", cli, 7],
        ["esbuild", "1", 7],
        ["bundle", at("demo/example/tsup.config.ts"), 7],
      ],
    ).join(),
    /tsup process 7 reported no esbuild metafile/u,
  );
  // A process the module hooks never saw load anything was not traced by
  // them (hooks missing or bypassed).
  assert.match(
    reasons(
      [],
      [
        ["process", cli, 7],
        ["esbuild", "1", 7],
        ["bundle", at("demo/example/src/index.ts"), 7],
      ],
    ).join(),
    /process 7 reported no module loads, so the module hooks did not trace it/u,
  );
  // Loads are reads: one outside the closure is refused like any other.
  assert.match(
    reasons([["load", at("demo/undeclared/src/index.ts"), 7]]).join(),
    /read demo\/undeclared\/src\/index\.ts, which its input closure does not bind/u,
  );
  editPackage(root, "example", (pkg) => {
    pkg.scripts["build:contrib-raw"] =
      "tsup src/index.ts && node scripts/digest.mjs";
  });
  generator(root, "scripts/digest.mjs", "");
  assert.match(
    reasons([]).join(),
    /build recipe runs demo\/example\/scripts\/digest\.mjs, but the trace saw no such process/u,
  );
  assert.deepEqual(
    reasons([
      ["process", at("demo/example/scripts/digest.mjs"), 8],
      ["load", at("demo/example/scripts/digest.mjs"), 8],
    ]),
    [],
  );
});

test("a node_modules above the checkout is named, never deleted", (t) => {
  const parent = fixture(t);
  const root = resolve(parent, "checkout");
  mkdirSync(root);
  for (const entry of ["demo", ".git"])
    renameSync(resolve(parent, entry), resolve(root, entry));
  const types = resolve(parent, "node_modules/@types/stray/index.d.ts");
  mkdirSync(resolve(types, ".."), { recursive: true });
  writeFileSync(types, "");
  const cli = resolve(root, "demo/node_modules/tsup/dist/cli-default.js");
  mkdirSync(resolve(cli, ".."), { recursive: true });
  writeFileSync(cli, "");
  const reasons = unboundReads(
    root,
    "example",
    [
      ["process", cli, 7],
      ["load", cli, 7],
      ["bundle", resolve(root, "demo/example/src/index.ts"), 7],
      ["read", types, 7],
    ],
    { dependencies: [] },
  );
  assert.deepEqual(reasons, [
    `TypeScript loaded ${resolve(parent, "node_modules/@types/stray")}, a type package above the checkout that every build includes and no stamp binds; types above checkout: ${resolve(parent, "node_modules/@types")}`,
  ]);
  // Advice limited to @types, never a command that deletes outside the
  // repository; the probe appends the rebuild step.
  const fix = unstampedFix(reasons);
  assert.equal(
    fix,
    `TypeScript includes every node_modules/@types above the checkout; if nothing else needs ${resolve(parent, "node_modules/@types")}, move it aside`,
  );
  assert.doesNotMatch(fix, /\brm\b/u);
  const other = resolve(parent, "node_modules/stray/index.js");
  mkdirSync(resolve(other, ".."), { recursive: true });
  writeFileSync(other, "");
  const untyped = unboundReads(
    root,
    "example",
    [
      ["process", cli, 7],
      ["load", cli, 7],
      ["bundle", resolve(root, "demo/example/src/index.ts"), 7],
      ["read", other, 7],
    ],
    { dependencies: [] },
  );
  assert.deepEqual(untyped, [
    `build read ${other} from a node_modules directory above the checkout, which no stamp binds`,
  ]);
  assert.equal(unstampedFix(untyped), undefined);
  assert.equal(
    unstampedFix(["build read x, which no longer exists"]),
    undefined,
  );
});

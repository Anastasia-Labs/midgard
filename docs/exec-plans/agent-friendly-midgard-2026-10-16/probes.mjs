// Bounded design experiments against immutable Git sources, not acceptance tests.
// Usage: node probes.mjs REPOSITORY DEPENDENCY_CHECKOUT OUTPUT_DIRECTORY
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createRequire, stripTypeScriptTypes } from "node:module";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  rmSync,
  writeFileSync,
  existsSync,
} from "node:fs";
import { join, resolve, relative } from "node:path";
import vm from "node:vm";

const [repo, dependencies, destination] = process.argv.slice(2);
if (!repo || !dependencies || !destination) {
  throw new Error("Expected REPOSITORY DEPENDENCY_CHECKOUT OUTPUT_DIRECTORY");
}
const ref = "43dc8e42b7706fc79978cfe1d4b4f5ec22144189";
const git = (...args) =>
  execFileSync("git", args, {
    cwd: repo,
    encoding: "utf8",
    maxBuffer: 32 * 1024 * 1024,
  });
const at = (path) => git("show", `${ref}:${path}`);
const req = createRequire(
  resolve(dependencies, "demo/midgard-sdk/package.json"),
);
const ts = req("typescript");
const { Effect } = req("effect");
const output = resolve(destination);
mkdirSync(output, { recursive: true });
const scratch = mkdtempSync(join(output, "fixtures-"));
const result = { ref, date: new Date().toISOString(), node: process.version };
const expression = (path, name) => {
  const source = ts.createSourceFile(
    path,
    at(path),
    ts.ScriptTarget.Latest,
    true,
  );
  let value;
  const visit = (node) => {
    if (ts.isVariableDeclaration(node) && node.name.getText(source) === name) {
      value = node.initializer.getText(source);
    }
    ts.forEachChild(node, visit);
  };
  visit(source);
  assert.ok(value, name);
  return vm.runInNewContext(stripTypeScriptTypes(`(${value})`), {});
};

try {
  const core = expression(
    "demo/midgard-core/src/deployment-manifest-identity/reference-script-contracts.ts",
    "DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE",
  );
  const node = expression(
    "demo/midgard-node/src/deployment-manifest.deployment-manifest-reference-script-contract-by-role.ts",
    "DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE",
  );
  const categories = expression(
    "demo/midgard-core/src/deployment-manifest-identity/catalogue-roles.deployment-manifest-fraud-proof-contract-by-category.ts",
    "DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY",
  );
  const watcher = expression(
    "demo/midgard-watcher/src/runtime/deployment-identity.catalogue-category-to-contract.ts",
    "CATALOGUE_CATEGORY_TO_CONTRACT",
  );
  const diff = (a, b) =>
    [...new Set([...Object.keys(a), ...Object.keys(b)])].filter(
      (key) => a[key] !== b[key],
    );
  result.maps = {
    roles: Object.keys(core).length,
    categories: Object.keys(categories).length,
    roleDifferences: diff(core, node),
    categoryDifferences: diff(categories, watcher),
  };
  assert.equal(
    result.maps.roleDifferences.length + result.maps.categoryDifferences.length,
    0,
  );
  const families = expression(
    "demo/midgard-node/src/deployable-scripts.fault-proof-step-contract-names.ts",
    "REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES",
  );
  const incomplete = families.filter((name) => name !== "minAda");
  const fixture = join(scratch, "totality.ts");
  writeFileSync(
    fixture,
    [
      `type Family = ${families.map(JSON.stringify).join(" | ")};`,
      `const list = ${JSON.stringify(incomplete)} as const satisfies readonly Family[];`,
      `const missing = ${JSON.stringify(Object.fromEntries(incomplete.map((name) => [name, true])))} satisfies Record<Family, boolean>;`,
      `const complete = ${JSON.stringify(Object.fromEntries(families.map((name) => [name, true])))} satisfies Record<Family, boolean>;`,
    ].join("\n"),
  );
  const program = ts.createProgram([fixture], {
    noEmit: true,
    strict: true,
    skipLibCheck: true,
    types: [],
    target: ts.ScriptTarget.ES2022,
  });
  result.totality = ts
    .getPreEmitDiagnostics(program)
    .filter((d) => d.file?.fileName === fixture)
    .map((d) => ({
      code: d.code,
      line: d.file.getLineAndCharacterOfPosition(d.start).line + 1,
    }));
  assert.deepEqual(result.totality, [{ code: 1360, line: 3 }]);

  const completionSource = at("demo/midgard-sdk/src/tx-completion.ts")
    .replace(/^import .*;\n/gm, "")
    .replace(/export /g, "");
  const complete = new Function(
    "Effect",
    `${stripTypeScriptTypes(completionSource)}; return completeTxWithLocalUPLCEvalProgram;`,
  )(Effect);
  const currentCalls = [];
  await Effect.runPromise(
    complete(
      {
        complete: async (options) => {
          currentCalls.push(options);
          return "recorded";
        },
      },
      (error) => error,
      [],
    ),
  );
  let candidateCalls = 0;
  // Design experiment: reject at completion, even if an untyped caller bypasses construction.
  const proposed = (inputs) => {
    if (inputs.kind === "selected" && inputs.inputs.length === 0)
      throw new Error("empty selected wallet");
    return complete(
      {
        complete: async (options) => {
          candidateCalls++;
          return options;
        },
      },
      (error) => error,
      inputs.kind === "provider" ? undefined : inputs.inputs,
    );
  };
  assert.throws(
    () => proposed({ kind: "selected", inputs: [] }),
    /empty selected/,
  );
  assert.equal(candidateCalls, 0);
  const provider = await Effect.runPromise(proposed({ kind: "provider" }));
  const selected = await Effect.runPromise(
    proposed({
      kind: "selected",
      inputs: [{ txHash: "01".repeat(32), outputIndex: 0 }],
    }),
  );
  assert.equal(currentCalls.length, 1);
  assert.equal(provider.presetWalletInputs, undefined);
  assert.equal(selected.presetWalletInputs.length, 1);
  result.wallet = {
    currentEmptyBuilderCalls: currentCalls.length,
    currentOptions: currentCalls[0],
    proposedEmptyBuilderCalls: 0,
    provider,
    selected,
  };

  // Extract the actual frozen function; no reimplementation of its selector.
  const source = ts.createSourceFile(
    "tests.mjs",
    at("scripts/contrib/tests.mjs"),
    ts.ScriptTarget.Latest,
    true,
  );
  let selector;
  const visit = (n) => {
    if (
      ts.isVariableDeclaration(n) &&
      n.name.getText(source) === "testRunningOwnDist"
    )
      selector = n.initializer.getText(source);
    ts.forEachChild(n, visit);
  };
  visit(source);
  assert.ok(selector);
  const ownDist = vm.runInNewContext(`(${selector})`, {
    existsSync,
    readdirSync,
    readFileSync,
    resolve,
    join,
    relative,
  });
  mkdirSync(join(scratch, "demo/example/tests"), { recursive: true });
  const test = join(scratch, "demo/example/tests/cli.test.ts");
  writeFileSync(test, 'const path = "../dist/cli.js";\n');
  const literal = ownDist(scratch, { directory: "demo/example" });
  writeFileSync(test, 'const path = join("..", "dist", "cli.js");\n');
  const segmented = ownDist(scratch, { directory: "demo/example" }) ?? null;
  assert.ok(literal);
  assert.equal(segmented, null);
  result.ownDist = { literal, segmented };

  const paths = git("ls-tree", "-r", "--name-only", ref, "demo")
    .split("\n")
    .filter((path) => /^demo\/[^/]+\/package.json$/u.test(path));
  const packages = paths.map((path) => JSON.parse(at(path)));
  const entries = packages.flatMap((pkg) =>
    Object.entries(pkg.exports ?? {}).map(([name, value]) => ({
      package: pkg.name,
      name,
      import: !!value?.import,
      require: !!value?.require,
      default: !!value?.default,
      string: typeof value === "string",
    })),
  );
  result.exports = {
    packages: packages.length,
    runtimeEntries: entries.filter(
      (e) => e.import || e.require || e.default || e.string,
    ).length,
    requireBranches: entries.filter((e) => e.require).length,
    skippedEntries: entries.filter(
      (e) => !e.import && (e.require || e.default || e.string),
    ),
  };
  const fixturePackage = join(scratch, "node_modules/design-probe");
  mkdirSync(fixturePackage, { recursive: true });
  writeFileSync(
    join(fixturePackage, "package.json"),
    JSON.stringify({
      name: "design-probe",
      type: "module",
      exports: { ".": { import: "./index.js", require: "./index.cjs" } },
    }),
  );
  writeFileSync(join(fixturePackage, "index.js"), "export const value = 1;\n");
  writeFileSync(
    join(fixturePackage, "index.cjs"),
    'throw new Error("CJS runtime defect");\n',
  );
  writeFileSync(
    join(scratch, "consumer.mjs"),
    'import { value } from "design-probe"; export default value;\n',
  );
  assert.equal((await import(join(scratch, "consumer.mjs"))).default, 1);
  assert.throws(
    () => createRequire(join(scratch, "consumer.cjs"))("design-probe"),
    /CJS runtime defect/,
  );
  result.consumer = {
    installedSpecifierEsm: "passed",
    installedSpecifierCjs: "expected failure",
  };
  const byName = new Map(packages.map((pkg) => [pkg.name, pkg]));
  result.images = ["midgard-node", "midgard-watcher", "da-committee-node"].map(
    (service) => {
      const found = new Set();
      const visit = (name) => {
        if (found.has(name) || !byName.has(name)) return;
        found.add(name);
        Object.keys(byName.get(name).dependencies ?? {}).forEach(visit);
      };
      visit(service);
      const expected = [...found].filter(
        (name) => byName.get(name).scripts?.["build:contrib-raw"],
      );
      const declarations = [
        ...at(`demo/${service}/Dockerfile`).matchAll(
          /^RUN pnpm --filter (\S+) run build:contrib-raw$/gm,
        ),
      ].map((m) => m[1]);
      return {
        service,
        declarations: declarations.length,
        missing: expected.filter((name) => !declarations.includes(name)),
      };
    },
  );
} finally {
  rmSync(scratch, { recursive: true, force: true });
}
result.fixtureCleanupComplete = !existsSync(scratch);
writeFileSync(
  join(output, "results.json"),
  `${JSON.stringify(result, null, 2)}\n`,
);
console.log(JSON.stringify(result, null, 2));

import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import test from "node:test";

import { packageByName, runtimeBuildClosure } from "./files.mjs";

// One scoped and one unscoped package, as the demo workspace mixes them.
const workspace = (t) => {
  const root = mkdtempSync(resolve(tmpdir(), "contrib-files-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  for (const [directory, name] of [
    ["midgard-core", "@al-ft/midgard-core"],
    ["midgard-node", "midgard-node"],
  ]) {
    mkdirSync(resolve(root, "demo", directory), { recursive: true });
    writeFileSync(
      resolve(root, "demo", directory, "package.json"),
      JSON.stringify({ name }),
    );
  }
  return root;
};

test("a package is found by declared name, directory, or directory under the workspace scope", (t) => {
  const root = workspace(t);
  for (const [given, expected] of [
    ["@al-ft/midgard-core", "@al-ft/midgard-core"],
    ["midgard-core", "@al-ft/midgard-core"],
    ["midgard-node", "midgard-node"],
    ["@al-ft/midgard-node", "midgard-node"],
  ])
    assert.equal(packageByName(root, given).name, expected, given);
});

test("a scope the workspace does not use, or an unknown directory, is refused", (t) => {
  const root = workspace(t);
  for (const given of ["@other/midgard-node", "midgard-nod", "@al-ft/nope"])
    assert.throws(
      () => packageByName(root, given),
      /unknown workspace package/u,
      given,
    );
});

test("a workspace package a build keeps external must be a runtime dependency", (t) => {
  // midgard-node-tools kept midgard-watcher external as a devDependency: its
  // dist imported the watcher's dist, which no plan or build prepared.
  const tools = (dependencies) => {
    const root = workspace(t);
    mkdirSync(resolve(root, "demo/tools"));
    writeFileSync(
      resolve(root, "demo/tools/package.json"),
      JSON.stringify({
        name: "tools",
        scripts: { build: "tsup" },
        midgardTasks: {
          build: { steps: [{ command: ["tsup"] }], outputs: ["dist"] },
        },
        ...dependencies,
      }),
    );
    writeFileSync(
      resolve(root, "demo/tools/tsup.config.ts"),
      "export default { external: [/^midgard-node(\\/|$)/] };\n",
    );
    return root;
  };
  assert.throws(
    () =>
      runtimeBuildClosure(
        tools({ devDependencies: { "midgard-node": "workspace:*" } }),
        "tools",
      ),
    /tools: its build keeps midgard-node external.*not in its dependencies/u,
  );
  assert.deepEqual(
    runtimeBuildClosure(
      tools({ dependencies: { "midgard-node": "workspace:*" } }),
      "tools",
    ).map(({ name }) => name),
    ["tools"],
  );
});

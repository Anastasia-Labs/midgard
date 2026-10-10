import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import test from "node:test";

import { packageByName } from "./files.mjs";

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

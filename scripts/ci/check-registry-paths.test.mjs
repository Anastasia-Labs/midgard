import assert from "node:assert/strict";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { globToRegExp } from "../preflight/derive.mjs";
import {
  deadEntries,
  REGISTRIES,
  repositoryFiles,
} from "./check-registry-paths.mjs";

const root = fileURLToPath(new URL("../..", import.meta.url));

const FILES = [
  "tools/registry.json",
  "demo/app/tests/a.test.ts",
  "demo/app/tests/b.test.ts",
  "demo/app/src/index.ts",
];
const registry = (entries) => ({
  name: "example",
  source: "tools/registry.json",
  entries: () => entries,
});

test("a path, a directory and a glob pass while they name something, and are reported once they do not", () => {
  const entries = [
    { path: "demo/app/tests/a.test.ts", label: "a" },
    { path: "demo/app/src", label: "src" },
    { glob: "demo/app/tests/*.test.ts", label: "tests" },
  ];
  assert.deepEqual(
    deadEntries(root, { registries: [registry(entries)], files: FILES }).dead,
    [],
  );
  const { dead, counts } = deadEntries(root, {
    registries: [registry(entries)],
    files: ["tools/registry.json", "demo/app/README.md"],
  });
  assert.deepEqual(counts, { example: 3 });
  assert.deepEqual(
    dead.map(({ label, reason }) => [label, reason]),
    [
      ["a", "does not exist"],
      ["src", "does not exist"],
      ["tests", "matches no file"],
    ],
  );
  assert.equal(dead[0].source, "tools/registry.json");
});

test("a registry whose own file is gone is reported, not read as empty", () => {
  const { dead } = deadEntries(root, {
    registries: [
      registry([{ path: "demo/app/src/index.ts", label: "index" }]),
      {
        name: "tables",
        sources: /^demo\/[^/]+\/durations\.json$/u,
        entries: () => [],
      },
    ],
    files: ["demo/app/src/index.ts"],
  });
  assert.deepEqual(
    dead.map(({ registry: name, label }) => [name, label]),
    [
      ["example", "(the registry itself)"],
      ["tables", "(the registry itself)"],
    ],
  );
});

// The footgun: a file is deleted and a registry still names it. For every
// registry this check reads, deleting what one of its entries names makes it
// fail, so no registry is wired in a way that cannot.
test("deleting a file any registry names fails the check", () => {
  const files = repositoryFiles(root);
  const names = REGISTRIES.map((r) => r.name);
  assert.deepEqual(names, [
    "contrib gates",
    "preflight triggers",
    "traced-refusal inputs",
    "retained modules",
    "CI file durations",
    "validator scenario registry",
  ]);
  for (const registry of REGISTRIES) {
    const sources = registry.sources
      ? files.filter((file) => registry.sources.test(file))
      : [registry.source];
    assert.ok(sources.length > 0, `${registry.name} has no source file`);
    // A barrel or an unmapped list may hold no entries; the registry may not.
    const [source, chosen] = sources
      .flatMap((file) => registry.entries(root, file).map((e) => [file, e]))
      .at(0) ?? [sources[0]];
    assert.ok(chosen, `${registry.name} yields no entries`);
    const gone = chosen.glob
      ? (file) => globToRegExp(chosen.glob).test(file)
      : (file) => file === chosen.path || file.startsWith(`${chosen.path}/`);
    const { dead } = deadEntries(root, {
      registries: [registry],
      files: files.filter((file) => file === source || !gone(file)),
    });
    assert.ok(
      dead.some(
        (item) => item.source === source && item.label === chosen.label,
      ),
      `${source}: deleting ${chosen.label} went unnoticed`,
    );
  }
});

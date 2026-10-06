import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  caseNamePattern,
  isTestFile,
  tracedRefusalPlan,
} from "../../demo/midgard-fault-proofs/scripts/traced-refusal-plan.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const packageRoot = join(root, "demo/midgard-fault-proofs");

/** A package root holding `files` (path under tests/ to source). */
const withTree = (files, check) => {
  const directory = mkdtempSync(join(tmpdir(), "traced-plan-"));
  try {
    for (const [path, source] of Object.entries(files)) {
      const file = join(directory, "tests", path);
      mkdirSync(dirname(file), { recursive: true });
      writeFileSync(file, source);
    }
    check(directory);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

const sitesOf = (pins, file) =>
  pins
    .filter((pin) => pin.file === file)
    .flatMap(({ sites }) => sites.map(({ file: at, name }) => `${at} ${name}`));

// The runner checks only what the plan names, so every pin in the tree, in a
// test file or a support module, must be in the plan with a case that runs
// it. The pins are counted here by a reading independent of the planner's.
test("every refusedBy pin under the fault-proofs tests is planned into a case", () => {
  const tests = join(packageRoot, "tests");
  const expected = new Map();
  for (const file of readdirSync(tests, { recursive: true }).map(String)) {
    if (!/\.(?:ts|mts|js|mjs)$/u.test(file)) continue;
    const count = (
      readFileSync(join(tests, file), "utf8").match(/\brefusedBy:\s*"/gu) ?? []
    ).length;
    if (count > 0) expected.set(`tests/${file}`, count);
  }
  assert.ok(expected.size > 0, "found no refusedBy pin");
  const pins = tracedRefusalPlan(packageRoot);
  const planned = new Map();
  for (const pin of pins) {
    planned.set(pin.file, (planned.get(pin.file) ?? 0) + 1);
    assert.ok(pin.sites.length > 0, `${pin.file} ${pin.module} has no case`);
    for (const site of pin.sites) {
      assert.ok(isTestFile(site.file), `${site.file} is not a test file`);
      assert.ok(
        readFileSync(join(packageRoot, site.file), "utf8").includes(
          JSON.stringify(site.name),
        ),
        `${site.file} declares no case named ${site.name}`,
      );
    }
  }
  assert.deepEqual(
    [...planned].sort(),
    [...expected].sort(),
    "the plan and the tree disagree on where the pins are",
  );
  // A pin in a support module runs in the cases of the files importing it.
  assert.ok(
    sitesOf(
      pins,
      "tests/support/execution-source-phase-a-dominance.ts",
    ).includes(
      "tests/execution-source-script-decoding-lifecycle.test.ts keeps an honest malformed-inline rejection in the earlier Phase A witness family",
    ),
  );
});

const pinCall = (module) =>
  `await expectOnchainRefusal(build, { refusedBy: "${module}", check: /x/u });`;

test("a pin in a test case runs in that case", () => {
  withTree(
    {
      "a.test.ts": [
        'describe("a", () => {',
        '  it("first", async () => {});',
        '  it("second \\"quoted\\"", async () => {',
        `    ${pinCall("m/one")}`,
        "  });",
        "});",
      ].join("\n"),
    },
    (directory) => {
      assert.deepEqual(tracedRefusalPlan(directory), [
        {
          file: "tests/a.test.ts",
          module: "m/one",
          sites: [
            {
              file: "tests/a.test.ts",
              name: 'second "quoted"',
              template: false,
            },
          ],
        },
      ]);
    },
  );
});

test("a pin in a support module runs in every case that reaches it", () => {
  withTree(
    {
      "support/deep.ts": [
        "const unexported = async () => {",
        `  ${pinCall("m/deep")}`,
        "};",
        "export const prove = async () => {",
        "  await unexported();",
        "};",
        "export const unrelated = () => 1;",
      ].join("\n"),
      "support/middle.ts": [
        'import { prove as proveDeep } from "./deep.js";',
        "export const viaMiddle = async () => proveDeep();",
      ].join("\n"),
      "support/index.ts":
        'export { viaMiddle as reexported } from "./middle.js";',
      "direct.test.ts": [
        'import * as deep from "./support/deep.js";',
        'it("uses the namespace", async () => {',
        "  await deep.prove();",
        "});",
        'it("uses something else", async () => deep.unrelated());',
      ].join("\n"),
      "barrel.test.ts": [
        'import { reexported } from "./support/index.js";',
        'it("before", async () => {});',
        "it.each([{ label: 'x' }])(",
        '  "runs $label through the barrel",',
        "  async () => {",
        "    await reexported();",
        "  },",
        ");",
      ].join("\n"),
      "unrelated.test.ts": [
        'import { unrelated } from "./support/deep.js";',
        'it("never reaches the pin", async () => unrelated());',
      ].join("\n"),
    },
    (directory) => {
      const [pin, ...rest] = tracedRefusalPlan(directory);
      assert.equal(rest.length, 0);
      assert.equal(pin.file, "tests/support/deep.ts");
      assert.deepEqual(pin.sites, [
        {
          file: "tests/barrel.test.ts",
          name: "runs $label through the barrel",
          template: true,
        },
        {
          file: "tests/direct.test.ts",
          name: "uses the namespace",
          template: false,
        },
      ]);
    },
  );
});

// A pin no case reaches would never be checked; the plan refuses it rather
// than leaving it out.
test("a pin no case reaches is refused", () => {
  withTree(
    {
      "support/orphan.ts": `export const orphan = async () => {\n  ${pinCall("m/orphan")}\n};\n`,
      "a.test.ts": `it("a", async () => {\n  ${pinCall("m/a")}\n});\n`,
    },
    (directory) => {
      assert.throws(
        () => tracedRefusalPlan(directory),
        /no it case reaches these refusedBy pins[\s\S]*tests\/support\/orphan\.ts: the m\/orphan pin/u,
      );
    },
  );
});

test("an it.each name filters the cases its template names", () => {
  const pattern = new RegExp(
    `${caseNamePattern({ name: "proves $label (case %s)", template: true })}$`,
    "u",
  );
  assert.match("suite proves empty payload (case 1)", pattern);
  assert.doesNotMatch("suite proves  (case 1) extra", pattern);
  assert.equal(
    caseNamePattern({ name: "a.b (c)", template: false }),
    "a\\.b \\(c\\)",
  );
});

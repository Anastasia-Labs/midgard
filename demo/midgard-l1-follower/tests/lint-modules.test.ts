import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { afterAll, describe, expect, it } from "vitest";

import {
  lintDeterminismModules,
  lintDeterminismSource,
} from "../src/lint/index.js";

const PACKAGE_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");

const scratch = mkdtempSync(join(tmpdir(), "l1-follower-lint-modules-"));
afterAll(() => {
  rmSync(scratch, { recursive: true, force: true });
});

/** Writes a package tree under the scratch directory and returns its root. */
let trees = 0;
const tree = (files: Readonly<Record<string, string>>): string => {
  trees += 1;
  const root = join(scratch, `pkg${String(trees)}`);
  for (const [path, source] of Object.entries(files)) {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), source);
  }
  return root;
};

const DERIVE = `import { slotOf } from "./helpers/slot.js";
import type { Clocked } from "./helpers/typed.js";
export const derive = (slot: number): number => slotOf(slot);
export type Derived = Clocked;
`;

describe("determinism lint over projection modules by glob", () => {
  it("passes a clean tree and follows its relative imports (not vacuous)", () => {
    const root = tree({
      "src/projection/derive.ts": DERIVE,
      "src/projection/helpers/slot.ts": `export const slotOf = (slot: number): number => slot + 1;\n`,
      "src/projection/helpers/typed.ts": `export type Clocked = { at: number };\nexport const now = (): number => Date.now();\n`,
      "src/projection/derive.test.ts": `export const t = Date.now();\n`,
      "tests/derive.test.ts": `import "../src/projection/derive.js";\nexport const t = Date.now();\n`,
    });
    const report = lintDeterminismModules({
      root,
      include: ["src/projection/*.ts"],
    });
    expect(report.problems).toEqual([]);
    // The helper is linted through the import; the type-only import and the
    // test files that inject `Date.now()` are out of scope.
    expect(report.files).toEqual([
      "src/projection/derive.ts",
      "src/projection/helpers/slot.ts",
    ]);
  });

  it("flags a clock, randomness, host or environment read in a helper the projection imports (red-check)", () => {
    const root = tree({
      "src/projection/derive.ts": DERIVE,
      "src/projection/helpers/slot.ts": `import { readFileSync } from "node:fs";
import os from "os";
import * as nodeCrypto from "node:crypto";
import { stamp } from "../../shared/stamp.js";
export const slotOf = (slot: number): number =>
  slot + globalThis.Date.now() + Number(process.env.SLOT_SHIFT ?? 0) + stamp();
export const noise = (): unknown => [nodeCrypto.randomBytes(4), readFileSync, os];
`,
      "src/shared/stamp.ts": `import { readFile } from "fs/promises";
const { env } = process;
export const stamp = (): number => globalThis["Math"].random() + Number(env.X) + Number(readFile);
`,
    });
    const { problems } = lintDeterminismModules({
      root,
      include: ["src/projection/*.ts"],
    });
    expect(
      problems.map(({ path, line, rule }) => `${path}:${String(line)}:${rule}`),
    ).toEqual([
      "src/projection/helpers/slot.ts:1:host_import",
      "src/projection/helpers/slot.ts:2:host_import",
      "src/projection/helpers/slot.ts:6:clock",
      "src/projection/helpers/slot.ts:6:environment",
      "src/projection/helpers/slot.ts:7:randomness",
      "src/shared/stamp.ts:1:host_import",
      "src/shared/stamp.ts:2:environment",
      "src/shared/stamp.ts:3:randomness",
    ]);
  });

  it("fails closed on a relative import that resolves to no source file", () => {
    const root = tree({
      "src/projection/derive.ts": `import { gone } from "./missing.js";\nexport const derive = gone;\n`,
    });
    expect(
      lintDeterminismModules({ root, include: ["src/projection/*.ts"] })
        .problems,
    ).toMatchObject([
      { path: "src/projection/derive.ts", rule: "unresolved_import" },
    ]);
  });

  it("accepts only the allowed problem, reports an allowance nothing matches, and skips data imports that exist", () => {
    const root = tree({
      "src/projection/derive.ts": `import sql from "./schema.sql?raw";
import table from "./table.json";
import { retry } from "./retry.js";
export const derive = (): unknown => [sql, table, retry];
`,
      "src/projection/schema.sql": "CREATE TABLE t (x integer);\n",
      "src/projection/table.json": "{}\n",
      "src/projection/retry.ts": `export const retry = (run: () => void): unknown => setTimeout(run, 10);
export const stamp = (): number => Date.now();
`,
    });
    const allowance = (text: string) => ({
      path: "src/projection/retry.ts",
      rule: "clock" as const,
      text,
      reason: "the retry timer",
    });
    const lint = (allow: ReturnType<typeof allowance>[]) =>
      lintDeterminismModules({
        root,
        include: ["src/projection/derive.ts"],
        allow,
      }).problems.map(
        ({ path, line, rule, text }) =>
          `${path}:${String(line)}:${rule}:${text}`,
      );
    expect(lint([])).toEqual([
      "src/projection/retry.ts:1:clock:setTimeout",
      "src/projection/retry.ts:2:clock:Date",
    ]);
    expect(lint([allowance("setTimeout")])).toEqual([
      "src/projection/retry.ts:2:clock:Date",
    ]);
    expect(lint([allowance("setTimeout"), allowance("performance")])).toEqual([
      "src/projection/retry.ts:0:stale_allowance:performance (0 of 1)",
      "src/projection/retry.ts:2:clock:Date",
    ]);
  });

  it("reports an excluded module a linted module imports, instead of skipping it", () => {
    const root = tree({
      "src/projection/derive.ts": `import { stamp } from "../io/stamp.js";\nexport const derive = stamp;\n`,
      "src/io/stamp.ts": `export const stamp = (): number => Date.now();\n`,
    });
    const report = lintDeterminismModules({
      root,
      include: ["src/projection/*.ts"],
      exclude: ["src/io/**"],
    });
    expect(report.problems).toEqual([
      {
        path: "src/projection/derive.ts",
        line: 0,
        rule: "excluded_import",
        text: "../io/stamp.js",
      },
    ]);
    expect(report.files).toEqual(["src/projection/derive.ts"]);
  });

  it("accepts an allowance's exact occurrence count, and reports every occurrence past it", () => {
    const source = (timers: number) =>
      Array.from(
        { length: timers },
        (_, i) => `export const t${String(i)} = setTimeout;\n`,
      ).join("");
    const lint = (timers: number, count: number) =>
      lintDeterminismModules({
        root: tree({ "src/projection/retry.ts": source(timers) }),
        include: ["src/projection/*.ts"],
        allow: [
          {
            path: "src/projection/retry.ts",
            rule: "clock",
            text: "setTimeout",
            count,
            reason: "the retry timers",
          },
        ],
      }).problems.map(
        ({ line, rule, text }) => `${String(line)}:${rule}:${text}`,
      );
    expect(lint(2, 2)).toEqual([]);
    expect(lint(3, 2)).toEqual([
      "1:clock:setTimeout",
      "2:clock:setTimeout",
      "3:clock:setTimeout",
    ]);
    expect(lint(1, 2)).toEqual(["0:stale_allowance:setTimeout (1 of 2)"]);
  });

  it("fails closed on a data import whose file is missing", () => {
    const root = tree({
      "src/projection/derive.ts": `import sql from "./gone.sql?raw";\nexport const derive = sql;\n`,
    });
    expect(
      lintDeterminismModules({ root, include: ["src/projection/*.ts"] })
        .problems,
    ).toMatchObject([{ rule: "unresolved_import", text: "./gone.sql?raw" }]);
  });

  it("keeps the follower's own write path (apply, rewind, prune, the decoders) deterministic", () => {
    const report = lintDeterminismModules({
      root: PACKAGE_ROOT,
      include: [
        "src/store/apply.ts",
        "src/store/rewind.ts",
        "src/store/prune.ts",
        "src/decode/*.ts",
      ],
    });
    expect(report.problems).toEqual([]);
    expect(report.files).toEqual(
      expect.arrayContaining([
        "src/store/apply.ts",
        "src/store/qualify.ts",
        "src/decode/block.ts",
        "src/cbor/reader.ts",
      ]) as unknown,
    );
  });
});

describe("determinism lint: global-object and host reads in one file", () => {
  it.each([
    ["globalThis.Date.now()", "clock"],
    ['globalThis["Date"]', "clock"],
    ["global.setTimeout", "clock"],
    ["globalThis.fetch", "network_global"],
    ["globalThis.crypto.getRandomValues", "randomness"],
    ["globalThis.process.env", "environment"],
    ['process["env"]', "environment"],
    ["process.env.HOME", "environment"],
    ["const { random } = Math", "randomness"],
    ['import { hostname } from "node:os"', "host_import"],
    ['import { writeFileSync } from "fs"', "host_import"],
    ['import { env } from "node:process"', "environment"],
    ['import { hrtime } from "node:process"', "environment"],
    ['import process from "process"', "environment"],
    [
      'import { performance as perf } from "node:perf_hooks"; perf.now()',
      "clock",
    ],
    ['import { setTimeout as sleep } from "node:timers/promises"', "clock"],
    ['import { setTimeout as later } from "timers"', "clock"],
    ['import { threadId } from "node:worker_threads"', "host_import"],
    ['import v8 from "node:v8"', "host_import"],
    ["const p = process; p.env.X", "global_alias"],
    ["const g = globalThis.process; g.env.X", "global_alias"],
    ['const g = globalThis["Math"]; g.random()', "global_alias"],
    ["const m = Math; m.random()", "global_alias"],
    ["read(process)", "global_alias"],
    ["globalThis[name]", "global_alias"],
    [
      'import * as c from "node:crypto"; const k = c; k.randomBytes(4)',
      "global_alias",
    ],
    ["const n = 1; import(`./${String(n)}.js`)", "unresolved_import"],
    ["require(name)", "unresolved_import"],
  ])("%s is %s", (source, rule) => {
    expect(
      lintDeterminismSource("probe.ts", source).map((problem) => problem.rule),
    ).toContain(rule);
  });

  it("leaves pure reads alone", () => {
    expect(
      lintDeterminismSource(
        "pure.ts",
        'const at: Date | null = null; const x = globalThis.Number(1); const y = process === undefined; const z = typeof process; const m = Math.max(1, 2); void import("./x.js"); let t: ReturnType<typeof setTimeout> | null = null; void [at, x, y, z, m, t];',
      ),
    ).toEqual([]);
  });
});

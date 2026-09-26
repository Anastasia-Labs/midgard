// Tests for verify-pure-move.mjs. Each test builds a throwaway git repository
// under the OS temp directory, commits a "before" module there, writes the
// split "after" modules into its working tree, and runs the script against it.
//
//   node --test .agents/skills/splitting-oversized-modules/scripts/verify-pure-move.test.mjs
//
// The verifier needs `typescript` from the demo workspace. When it cannot be
// resolved (a checkout without `pnpm --dir demo install`), the tests that parse
// TypeScript are reported as skipped, not passed; set
// VERIFY_PURE_MOVE_REQUIRE_TYPESCRIPT=1 to make that a failure instead.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { after, describe, test } from "node:test";
import { fileURLToPath } from "node:url";

import { loadTypeScript } from "./verify-pure-move.mjs";

const SCRIPT = fileURLToPath(
  new URL("./verify-pure-move.mjs", import.meta.url),
);
const SOURCE_REPO = resolve(dirname(SCRIPT), "../../../..");
const TMP = realpathSync(tmpdir());

let typescriptMissing = false;
try {
  loadTypeScript(SOURCE_REPO);
} catch (error) {
  if (process.env.VERIFY_PURE_MOVE_REQUIRE_TYPESCRIPT === "1") throw error;
  typescriptMissing = `typescript is not installed in ${SOURCE_REPO}/demo`;
}

const made = [];
after(() => {
  for (const dir of made) rmSync(dir, { recursive: true, force: true });
});

const gitEnv = () => {
  const env = { ...process.env };
  for (const key of Object.keys(env))
    if (key.startsWith("GIT_")) delete env[key];
  return {
    ...env,
    GIT_CONFIG_GLOBAL: "/dev/null",
    GIT_CONFIG_NOSYSTEM: "1",
    GIT_AUTHOR_NAME: "Test",
    GIT_AUTHOR_EMAIL: "test@example.invalid",
    GIT_COMMITTER_NAME: "Test",
    GIT_COMMITTER_EMAIL: "test@example.invalid",
  };
};

const makeRepo = (committed) => {
  const dir = realpathSync(mkdtempSync(join(TMP, "verify-pure-move-")));
  made.push(dir);
  const git = (...args) => {
    const result = spawnSync("git", args, {
      cwd: dir,
      env: gitEnv(),
      encoding: "utf8",
    });
    assert.equal(result.status, 0, result.stderr);
  };
  git("init", "--quiet", "--initial-branch=main");
  const write = (files) => {
    for (const [path, text] of Object.entries(files)) {
      mkdirSync(dirname(join(dir, path)), { recursive: true });
      writeFileSync(join(dir, path), text);
    }
  };
  write(committed);
  git("add", "--all");
  git("commit", "--quiet", "--message", "before");
  const remove = (path) => rmSync(join(dir, path), { force: true });
  return { dir, write, remove };
};

const run = (cwd, args) => {
  const result = spawnSync(process.execPath, [SCRIPT, ...args], {
    cwd,
    env: gitEnv(),
    encoding: "utf8",
  });
  return { status: result.status, out: result.stdout + result.stderr };
};

// The module before the split: imports, comments, a JSDoc block, overloads,
// a destructuring declaration, a top-level side effect, and a namespace.
const BEFORE = `import { createHash } from "node:crypto";
import type { Buffer } from "node:buffer";

/** Sums two numbers. */
export function add(a: number, b: number): number {
  return a + b; // plain addition
}

export function negate(value: number): number {
  return -value;
}

export function parse(text: string): number;
export function parse(text: string, radix: number): number;
export function parse(text: string, radix = 10): number {
  return Number.parseInt(text, radix);
}

export interface Digest {
  readonly algorithm: "sha256";
  bytes: Buffer;
}

export type Pair<T> = readonly [T, T];

const LIMIT = 0x10;
export const { first, second } = { first: 1, second: 2 };

export const digest = (text: string): Digest => ({
  algorithm: "sha256",
  bytes: createHash("sha256").update(text).digest(),
});

export class Counter {
  #count = 0;
  next(): number {
    this.#count += 1;
    return this.#count <= LIMIT ? this.#count : LIMIT;
  }
}

export function clamp(value: number): number {
  const limit = LIMIT;
  return value > limit ? limit : value;
}

export namespace Limits {
  export const max = LIMIT;
}

Object.freeze(Limits);
`;

// The same declarations split across three modules, with every difference a
// pure move is allowed to have: new imports and re-exports, added and removed
// export modifiers, different comments, quote style, numeric spelling,
// trailing commas and line breaks, and a different declaration order.
const PURE_AFTER = {
  "src/big/math.ts": `// Arithmetic helpers moved out of big.ts.
export function negate(value: number): number { return -value }

export function add(a: number, b: number,): number {
  return a + b
}

export function parse(text: string): number;
export function parse(text: string, radix: number): number;
export function parse(text: string, radix = 10): number {
  return Number.parseInt(text, radix,);
}
`,
  "src/big/digest.ts": `import { createHash } from 'node:crypto';
import type { Buffer } from 'node:buffer';

export interface Digest {
  readonly algorithm: 'sha256';
  bytes: Buffer;
}

export type Pair<T> = readonly [T, T];

export const digest = (text: string): Digest => ({ algorithm: 'sha256', bytes: createHash('sha256').update(text).digest() });
`,
  "src/big/counter.ts": `export const LIMIT = 16;
export const { first, second } = { first: 1, second: 2, };

export class Counter {
  #count = 0;
  next(): number {
    this.#count += 1;
    return this.#count <= LIMIT
      ? this.#count
      : LIMIT;
  }
}

export function clamp(value: number): number {
  const limit = LIMIT
  return value > limit ? limit : value
}

export namespace Limits {
  export const max = LIMIT;
}

Object.freeze(Limits);
`,
  "src/big/index.ts": `export * from "./counter";
export * from "./digest";
export { add, negate, parse } from "./math";
`,
};

const split = (overrides = {}) => {
  const repo = makeRepo({ "src/big.ts": BEFORE });
  repo.remove("src/big.ts");
  repo.write({ ...PURE_AFTER, ...overrides });
  return repo;
};

const verify = (repo, extra = []) =>
  run(repo.dir, [
    "--before",
    "HEAD:src/big.ts",
    "--after",
    "src/big",
    ...extra,
  ]);

describe(
  "verify-pure-move on TypeScript fixtures",
  { skip: typescriptMissing },
  () => {
    test("confirms a pure split", () => {
      const { status, out } = verify(split());
      assert.equal(status, 0, out);
      assert.match(out, /^Pure move confirmed: 14 top-level declarations/mu);
      assert.match(out, /Not proven by this check: import targets/u);
    });

    test("refuses a split that silently changes one function body", () => {
      const math = PURE_AFTER["src/big/math.ts"].replace(
        "return a + b",
        "return a - b",
      );
      const { status, out } = verify(split({ "src/big/math.ts": math }));
      assert.equal(status, 1, out);
      assert.match(out, /^NOT a pure move: 1 declaration name\(s\) differ/mu);
      assert.match(
        out,
        /changed {5}function add: before HEAD:src\/big\.ts:5; after src\/big\/math\.ts:4/u,
      );
    });

    test("refuses a split that drops a declaration", () => {
      const counter = PURE_AFTER["src/big/counter.ts"].replace(
        "Object.freeze(Limits);\n",
        "",
      );
      const { status, out } = verify(split({ "src/big/counter.ts": counter }));
      assert.equal(status, 1, out);
      assert.match(out, /missing {5}statement Object\.freeze\(Limits\);/u);
    });

    test("refuses a declaration the before side did not have", () => {
      const math = `${PURE_AFTER["src/big/math.ts"]}export const twice = (n: number) => add(n, n);\n`;
      const { status, out } = verify(split({ "src/big/math.ts": math }));
      assert.equal(status, 1, out);
      assert.match(out, /unexpected {2}const twice/u);
    });

    test("refuses a declaration copied into two modules", () => {
      const digest = `${PURE_AFTER["src/big/digest.ts"]}export type Pair<T> = readonly [T, T];\n`;
      const { status, out } = verify(split({ "src/big/digest.ts": digest }));
      assert.equal(status, 1, out);
      assert.match(out, /copies {6}type Pair: 1 before, 2 after/u);
    });

    test("sees let/const and unary operators, which are not child nodes", () => {
      const counter = PURE_AFTER["src/big/counter.ts"].replace(
        "const limit = LIMIT",
        "let limit = LIMIT",
      );
      const math = PURE_AFTER["src/big/math.ts"].replace(
        "return -value",
        "return +value",
      );
      const { status, out } = verify(
        split({ "src/big/counter.ts": counter, "src/big/math.ts": math }),
      );
      assert.equal(status, 1, out);
      assert.match(out, /^NOT a pure move: 2 declaration name\(s\) differ/mu);
      assert.match(out, /changed {5}function clamp/u);
      assert.match(out, /changed {5}function negate/u);
    });

    test("sees a changed literal value and a changed identifier", () => {
      const digest = PURE_AFTER["src/big/digest.ts"].replace(
        "createHash('sha256')",
        "createHash('sha512')",
      );
      const counter = PURE_AFTER["src/big/counter.ts"].replace(
        "export const max = LIMIT;",
        "export const max = first;",
      );
      const { status, out } = verify(
        split({ "src/big/digest.ts": digest, "src/big/counter.ts": counter }),
      );
      assert.equal(status, 1, out);
      assert.match(out, /^NOT a pure move: 2 declaration name\(s\) differ/mu);
      assert.match(out, /changed {5}const digest/u);
      assert.match(out, /changed {5}namespace Limits/u);
    });

    test("ignores export only on the top-level statement, not inside it", () => {
      const counter = PURE_AFTER["src/big/counter.ts"].replace(
        "  export const max = LIMIT;",
        "  const max = LIMIT;",
      );
      const { status, out } = verify(split({ "src/big/counter.ts": counter }));
      assert.equal(status, 1, out);
      assert.match(out, /changed {5}namespace Limits/u);
    });

    test("tells a renamed binding from a moved one", () => {
      const counter = PURE_AFTER["src/big/counter.ts"].replace(
        "export const LIMIT = 16;",
        "export let LIMIT = 16;",
      );
      const { status, out } = verify(split({ "src/big/counter.ts": counter }));
      assert.equal(status, 1, out);
      assert.match(out, /missing {5}const LIMIT/u);
      assert.match(out, /unexpected {2}let LIMIT/u);
    });

    test("reads the before side from git, not from the working tree", () => {
      // Editing the original on disk to match a changed split must not help.
      const math = PURE_AFTER["src/big/math.ts"].replace(
        "return a + b",
        "return a - b",
      );
      const repo = split({ "src/big/math.ts": math });
      repo.write({
        "src/big.ts": BEFORE.replace("return a + b", "return a - b"),
      });
      const { status, out } = verify(repo);
      assert.equal(status, 1, out);
      assert.match(out, /changed {5}function add/u);
    });

    test("could not look: an after file that does not parse", () => {
      const { status, out } = verify(
        split({ "src/big/math.ts": "export function add(a: number {\n" }),
      );
      assert.equal(status, 3, out);
      assert.match(
        out,
        /could not look: could not parse src\/big\/math\.ts:1/u,
      );
    });

    test("could not look: a ref or path git cannot show", () => {
      const repo = split();
      let result = run(repo.dir, [
        "--before",
        "no-such-ref:src/big.ts",
        "--after",
        "src/big",
      ]);
      assert.equal(result.status, 3, result.out);
      assert.match(result.out, /git cannot resolve no-such-ref to a commit/u);
      result = run(repo.dir, [
        "--before",
        "HEAD:src/missing.ts",
        "--after",
        "src/big",
      ]);
      assert.equal(result.status, 3, result.out);
      assert.match(result.out, /git cannot show HEAD:src\/missing\.ts/u);
    });
  },
);

describe("verify-pure-move without parsing", () => {
  test("refuses a before side that is a file on disk", () => {
    const repo = makeRepo({ "src/big.ts": BEFORE });
    const { status, out } = run(repo.dir, [
      "--before",
      "src/big.ts",
      "--after",
      "src",
    ]);
    assert.equal(status, 2, out);
    assert.match(
      out,
      /is not <ref>:<path>\. The before side is always read from git/u,
    );
  });

  test("refuses a before side read from the index (empty ref)", () => {
    const repo = makeRepo({ "src/big.ts": BEFORE });
    const { status, out } = run(repo.dir, [
      "--before",
      ":src/big.ts",
      "--after",
      "src",
    ]);
    assert.equal(status, 2, out);
  });

  test("usage error without an after side", () => {
    const repo = makeRepo({ "src/big.ts": BEFORE });
    const { status } = run(repo.dir, ["--before", "HEAD:src/big.ts"]);
    assert.equal(status, 2);
  });

  test("could not look: typescript cannot be resolved", () => {
    const repo = makeRepo({ "src/big.ts": BEFORE });
    const empty = realpathSync(
      mkdtempSync(join(TMP, "verify-pure-move-nots-")),
    );
    made.push(empty);
    mkdirSync(join(empty, "demo"));
    writeFileSync(join(empty, "demo/package.json"), "{}\n");
    const { status, out } = run(repo.dir, [
      "--before",
      "HEAD:src/big.ts",
      "--after",
      "src/big.ts",
      "--typescript-root",
      empty,
    ]);
    assert.equal(status, 3, out);
    assert.match(out, /could not look: could not resolve typescript/u);
  });
});

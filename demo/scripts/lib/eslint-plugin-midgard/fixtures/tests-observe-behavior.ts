// fixture-path: midgard-node/tests/example.test.ts
declare const readFileSync: (path: unknown, encoding?: string) => string;
declare const readFile: (path: unknown, encoding?: string) => Promise<string>;
declare const writeFileSync: (path: unknown, data: string) => void;
declare const mkdtempSync: (prefix: string) => string;
declare const tmpdir: () => string;
declare const join: (...parts: string[]) => string;
declare const expect: any;
declare const assert: any;
declare const repoRoot: string;
declare const value: unknown;

// ruleid: midgard/tests-observe-behavior
readFileSync("../src/entry.ts", "utf8");

const validator = join(repoRoot, "onchain/aiken/validators/hub.ak");
// ruleid: midgard/tests-observe-behavior
readFileSync(validator, "utf8");

// ruleid: midgard/tests-observe-behavior
void readFile(new URL("../README.md", "file:///"), "utf8");

// ruleid: midgard/tests-observe-behavior
["../src/a.ts", "../src/b.ts"].map((path) => readFileSync(path, "utf8"));

for (const path of ["../src/a.ts", "../src/b.mjs"]) {
  // ruleid: midgard/tests-observe-behavior
  readFileSync(join(repoRoot, path), "utf8");
}

// ruleid: midgard/tests-observe-behavior
readFileSync(join(repoRoot, "config/deployments/env.ak.template"), "utf8");

// ruleid: midgard/tests-observe-behavior
expect(JSON.stringify(value)).toMatch(/secret/u);

// ruleid: midgard/tests-observe-behavior
assert.match(JSON.stringify(value).toLowerCase(), /secret/u);

// JSON vectors, fixtures and build outputs are data.
// ok: midgard/tests-observe-behavior
readFileSync(join(repoRoot, "onchain/aiken/plutus.json"), "utf8");

// A file the code under test wrote is its output, not source.
const summary = join(repoRoot, "summary.md");
writeFileSync(summary, "# summary");
// ok: midgard/tests-observe-behavior
readFileSync(summary, "utf8");

// ok: midgard/tests-observe-behavior
readFileSync(join(mkdtempSync(join(tmpdir(), "case-")), "out.ts"), "utf8");

// A redaction check over a serialized value stays allowed.
// ok: midgard/tests-observe-behavior
expect(JSON.stringify(value)).not.toContain("secret");

// fixture-path: scripts/preflight/example.test.ts
declare const readFileSync: (path: unknown, encoding?: string) => string;

// Tooling tests read the files the tool under test writes.
// ok: midgard/tests-observe-behavior
readFileSync("../lib/rule.mjs", "utf8");

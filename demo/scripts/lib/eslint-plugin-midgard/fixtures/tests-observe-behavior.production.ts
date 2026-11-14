// fixture-path: midgard-node/src/load.ts
declare const readFileSync: (path: unknown, encoding?: string) => string;

// Production code may read whatever it loads.
// ok: midgard/tests-observe-behavior
readFileSync("./template.md", "utf8");

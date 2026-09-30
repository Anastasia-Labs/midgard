import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = fileURLToPath(new URL("../../", import.meta.url));
const testFacets = new Set(
  JSON.parse(
    readFileSync(
      new URL("../module-test-facets.json", import.meta.url),
      "utf8",
    ),
  ).map((file) => `demo/${file}`),
);
const git = (args) =>
  execFileSync("git", args, {
    cwd: root,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
  });
const isTypeScript = (file) =>
  !file.startsWith("onchain/") && /\.(?:[cm]?ts|tsx)$/u.test(file);
const isProduction = (file) =>
  !testFacets.has(file) &&
  !/(?:^|\/)(?:tests?|e2e|benchmarks)(?:\/)|\.(?:test|spec|bench)\./u.test(
    file,
  );
const countLines = (source) => {
  const lines = source.split(/\r\n|\r|\n/u);
  if (lines.at(-1) === "") lines.pop();
  return lines.length;
};
const summarize = (rows) => ({
  files: rows.length,
  largest: Math.max(0, ...rows.map(({ lines }) => lines)),
  above500: rows.filter(({ lines }) => lines > 500).length,
  above1000: rows.filter(({ lines }) => lines > 1000).length,
  above2000: rows.filter(({ lines }) => lines > 2000).length,
});

const args = process.argv.slice(2);
let base;
let excludePrefix;
let json = false;
for (let index = 0; index < args.length; index++) {
  const argument = args[index];
  if (argument === "--json") json = true;
  else if (argument === "--base" || argument === "--exclude-prefix") {
    const value = args[++index];
    if (!value || value.startsWith("-"))
      throw new Error(`${argument} requires a value`);
    if (argument === "--base") base = value;
    else excludePrefix = value;
  } else throw new Error(`Unknown argument ${argument}`);
}
const included = (file) =>
  isTypeScript(file) && !file.startsWith(excludePrefix ?? "\0");
const currentFiles = [
  ...new Set(
    git(["ls-files", "--cached", "--others", "--exclude-standard", "-z"]).split(
      "\0",
    ),
  ),
].filter(included);
const current = currentFiles.map((file) => ({
  file,
  lines: countLines(readFileSync(resolve(root, file), "utf8")),
}));
const result = {
  measuredAt: new Date().toISOString(),
  definition:
    "Physical TypeScript lines, including comments/blanks; onchain excluded. Production excludes test/spec/bench files, test/e2e/benchmark directories and registered test facets.",
  ...(excludePrefix ? { excludePrefix } : {}),
  current: {
    all: summarize(current),
    production: summarize(current.filter(({ file }) => isProduction(file))),
  },
  oversized: current
    .filter(({ lines }) => lines > 500)
    .sort((left, right) => right.lines - left.lines),
};
if (base) {
  const commit = git(["rev-parse", "--verify", `${base}^{commit}`]).trim();
  const files = git(["ls-tree", "-r", "--name-only", "-z", commit])
    .split("\0")
    .filter(included);
  const rows = files.map((file) => ({
    file,
    lines: countLines(git(["show", `${commit}:${file}`])),
  }));
  result.baseline = {
    commit,
    all: summarize(rows),
    production: summarize(rows.filter(({ file }) => isProduction(file))),
  };
}
if (json) process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
else {
  for (const [label, scope] of [
    ["Baseline", result.baseline],
    ["Current", result.current],
  ]) {
    if (!scope) continue;
    const row = scope.production;
    process.stdout.write(
      `${label}: ${row.files} production TS files; ${row.above500} >500; ${row.above1000} >1,000 (${((100 * row.above1000) / row.files).toFixed(2)}%); ${row.above2000} >2,000 (${((100 * row.above2000) / row.files).toFixed(2)}%); largest ${row.largest}.\n`,
    );
  }
}

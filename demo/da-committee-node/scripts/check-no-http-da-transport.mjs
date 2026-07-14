#!/usr/bin/env node
import { readdir, readFile, stat } from "node:fs/promises";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const demoRoot = resolve(packageRoot, "..");
const injectedTarget = process.argv[2];
const targets =
  injectedTarget === undefined
    ? [
        join(packageRoot, "src/da/libp2p"),
        join(packageRoot, "src/da/payload.ts"),
        join(packageRoot, "src/da/source.ts"),
        join(packageRoot, "src/watcher.ts"),
        join(demoRoot, "midgard-node/src/da/libp2p-producer.ts"),
        join(demoRoot, "midgard-core/src/da-transport.ts"),
        join(demoRoot, "midgard-core/src/da-request-deadline.ts"),
      ]
    : [resolve(packageRoot, injectedTarget)];

const sourceExtensions = new Set([".ts", ".tsx", ".js", ".mjs", ".cjs"]);
const forbidden = [
  { label: "fetch()", pattern: /\bfetch\s*\(/giu },
  { label: "HTTP URL", pattern: /https?:\/\//giu },
  {
    label: "URL transport API",
    pattern: /\b(?:new\s+)?(?:URL|URLSearchParams)\s*\(/gu,
  },
  {
    label: "HTTP client import",
    pattern:
      /(?:from\s*|require\s*\(\s*)["'](?:node:)?https?(?:["']|\/)|from\s*["'](?:axios|undici)["']/gu,
  },
  {
    label: "HTTP endpoint field",
    committeeOnly: true,
    pattern:
      /\b(?:base_?urls?|http_?endpoint|committee_?endpoint|da_?endpoint)\b/giu,
  },
];

const listSourceFiles = async (target) => {
  const targetStat = await stat(target);
  if (targetStat.isFile()) {
    return sourceExtensions.has(extname(target)) ? [target] : [];
  }
  const entries = await readdir(target, { withFileTypes: true });
  const nested = await Promise.all(
    entries.map((entry) => {
      const path = join(target, entry.name);
      return entry.isDirectory()
        ? listSourceFiles(path)
        : Promise.resolve(sourceExtensions.has(extname(path)) ? [path] : []);
    }),
  );
  return nested.flat();
};

const files = (await Promise.all(targets.map(listSourceFiles))).flat().sort();
const findings = [];
for (const file of files) {
  const source = await readFile(file, "utf8");
  const lines = source.split(/\r?\n/u);
  for (const rule of forbidden) {
    if (rule.committeeOnly === true && !file.includes("/da-committee-node/")) {
      continue;
    }
    for (const match of source.matchAll(rule.pattern)) {
      const line = source.slice(0, match.index).split(/\r?\n/u).length;
      findings.push({
        file,
        line,
        label: rule.label,
        excerpt: lines[line - 1]?.trim() ?? "",
      });
    }
  }
}

if (findings.length > 0) {
  process.stderr.write(
    `Forbidden HTTP/URL DA transport token(s) found:\n${findings
      .map(
        (finding) =>
          `- ${finding.file}:${finding.line.toString()} [${finding.label}] ${finding.excerpt}`,
      )
      .join("\n")}\n`,
  );
  process.exitCode = 1;
} else {
  process.stdout.write(
    `No forbidden HTTP/URL tokens found in ${files.length.toString()} migrated DA libp2p target file(s).\n`,
  );
}

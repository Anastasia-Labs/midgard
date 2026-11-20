#!/usr/bin/env node
/** Verify repository-local Markdown links without making network requests. */
import { existsSync, lstatSync, readdirSync, readFileSync } from "node:fs";
import { dirname, extname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const markdownIgnorePatterns = readFileSync(resolve(repoRoot, ".gitignore"), "utf8")
  .split("\n")
  .map((line) => line.trim())
  .filter((line) => !line.startsWith("#") && /\.md$/u.test(line))
  .map((line) => {
    const normalized = line.replace(/^\/+|\/+$/gu, "");
    const source = normalized
      .split("*")
      .map((part) => part.replace(/[\\^$+?.()|[\]{}]/gu, "\\$&"))
      .join("[^/]*");
    return new RegExp(`^${source}$`, "u");
  });
const excludedDirectories = new Set([
  ".agents",
  ".claude",
  ".codex",
  ".git",
  ".next",
  "build",
  "coverage",
  "dist",
  "dist-newstyle",
  "logs",
  "node_modules",
  "result",
]);
const documents = [];
const walk = (directory) => {
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    if (entry.isDirectory() && excludedDirectories.has(entry.name)) continue;
    const path = join(directory, entry.name);
    if (entry.isSymbolicLink()) continue;
    if (entry.isDirectory()) walk(path);
    else if ([".md", ".mdx"].includes(extname(entry.name))) {
      const repositoryPath = relative(repoRoot, path).replaceAll("\\", "/");
      if (!markdownIgnorePatterns.some((pattern) => pattern.test(repositoryPath))) {
        documents.push(path);
      }
    }
  }
};
walk(repoRoot);

const failures = [];
const linkPattern = /!?(?:\[[^\]]*\])\(([^)]+)\)/g;
for (const document of documents.sort()) {
  const lines = readFileSync(document, "utf8").split("\n");
  lines.forEach((line, index) => {
    for (const match of line.matchAll(linkPattern)) {
      let target = match[1].trim();
      if (target.startsWith("<") && target.endsWith(">")) {
        target = target.slice(1, -1);
      }
      target = target.split(/\s+["']/u, 1)[0];
      if (
        target.length === 0 ||
        target.startsWith("#") ||
        target.startsWith("/") ||
        /^[a-z][a-z0-9+.-]*:/iu.test(target)
      ) {
        continue;
      }
      const pathPart = decodeURIComponent(target.split(/[?#]/u, 1)[0]);
      const resolved = resolve(dirname(document), pathPart);
      if (!existsSync(resolved) || lstatSync(resolved).isSymbolicLink()) {
        failures.push(
          `${relative(repoRoot, document)}:${String(index + 1)} -> ${target}`,
        );
      }
    }
  });
}

if (failures.length > 0) {
  console.error("Broken repository-local documentation links:\n");
  for (const failure of failures) console.error(`  - ${failure}`);
  process.exit(1);
}

console.log(`Docs link check passed: ${documents.length} Markdown/MDX files.`);

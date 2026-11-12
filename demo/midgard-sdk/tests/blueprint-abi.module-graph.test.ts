/**
 * Static contract, not runtime behaviour: the `@al-ft/midgard-sdk/blueprint-abi`
 * subpath is self-contained. Its static import graph, followed from the source
 * file its export declares, stays inside `src/blueprint-abi/` and names no
 * package outside `ALLOWED_PACKAGES`, so a consumer of the reader never loads
 * the SDK root or its transaction builders.
 */
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

import ts from "typescript";
import { describe, expect, it } from "vitest";

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const subpathRoot = resolve(packageRoot, "src/blueprint-abi");

const ALLOWED_PACKAGES: readonly string[] = [];

const declaredEntry = (): string => {
  const manifest = JSON.parse(
    readFileSync(resolve(packageRoot, "package.json"), "utf8"),
  ) as {
    readonly exports: Record<string, Record<string, string>>;
  };
  const source = manifest.exports["./blueprint-abi"]?.["midgard-source"];
  if (source === undefined) {
    throw new Error("package.json declares no ./blueprint-abi source export");
  }
  return resolve(packageRoot, source);
};

const staticImportGraph = (entry: string) => {
  const files = new Set<string>();
  const packages = new Set<string>();
  const pending = [entry];
  for (let file = pending.pop(); file !== undefined; file = pending.pop()) {
    if (files.has(file)) continue;
    files.add(file);
    const { importedFiles } = ts.preProcessFile(
      readFileSync(file, "utf8"),
      true,
      true,
    );
    for (const { fileName } of importedFiles) {
      if (!fileName.startsWith(".")) {
        packages.add(fileName);
        continue;
      }
      const target = resolve(dirname(file), fileName.replace(/\.js$/u, ".ts"));
      if (!existsSync(target)) {
        throw new Error(`${file} imports unresolvable ${fileName}`);
      }
      pending.push(target);
    }
  }
  return { files: [...files], packages: [...packages] };
};

describe("blueprint-abi subpath", () => {
  it("imports nothing outside itself", () => {
    const entry = declaredEntry();
    expect(entry).toBe(resolve(subpathRoot, "index.ts"));
    const graph = staticImportGraph(entry);
    expect(graph.files.length).toBeGreaterThan(1);
    expect(
      graph.files.filter((file) => !file.startsWith(`${subpathRoot}${sep}`)),
    ).toEqual([]);
    expect(graph.packages.sort()).toEqual([...ALLOWED_PACKAGES].sort());
  });
});

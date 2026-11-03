import { readdirSync, readFileSync, statSync } from "node:fs";
import { basename, dirname, extname, join } from "node:path";

// Source guards read either a retained entry and its sibling facets or an
// explicit implementation directory, including its nested source modules.
export const sourceFacetPaths = (file) => {
  if (statSync(file).isDirectory()) {
    return readdirSync(file, { recursive: true })
      .filter((entry) => /\.(ts|tsx|mjs|js)$/u.test(entry))
      .sort()
      .map((entry) => join(file, entry));
  }
  const extension = extname(file);
  if (![".ts", ".tsx", ".mjs", ".js"].includes(extension)) return [file];
  const stem = basename(file, extension).replace(/\.(test|spec|bench)$/u, "");
  return [
    file,
    ...readdirSync(dirname(file))
      .filter(
        (entry) =>
          entry.startsWith(`${stem}.`) &&
          entry.endsWith(extension) &&
          entry !== basename(file),
      )
      .sort()
      .map((entry) => join(dirname(file), entry)),
  ];
};

export const readSourceFacets = (file) =>
  sourceFacetPaths(file)
    .map((path) => readFileSync(path, "utf8"))
    .join("\n");

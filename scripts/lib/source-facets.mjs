import { readdirSync, readFileSync } from "node:fs";
import { basename, dirname, extname, join } from "node:path";

// A retained entry file and its sibling facets form one implementation for
// guards that inspect source text. Keep those guards covering moved code.
export const sourceFacetPaths = (file) => {
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

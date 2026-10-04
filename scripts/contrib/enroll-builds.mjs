import { writeFileSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { workspacePackages } from "./files.mjs";

export const enrollBuilds = (root, { write = false } = {}) => {
  const changes = [];
  for (const pkg of workspacePackages(root).filter(
    (entry) => entry.scripts?.build,
  )) {
    const path = resolve(root, pkg.directory, "package.json");
    const manifest = JSON.parse(readFileSync(path, "utf8"));
    const expected = `node ../../scripts/contrib.mjs build --package ${pkg.name}`;
    if (
      manifest.scripts.build === expected &&
      manifest.scripts["build:contrib-raw"]
    )
      continue;
    if (manifest.scripts.build.includes("contrib.mjs"))
      throw new Error(`unexpected guarded build recipe: ${pkg.name}`);
    manifest.scripts["build:contrib-raw"] = manifest.scripts.build;
    manifest.scripts.build = expected;
    changes.push(pkg.directory);
    if (write) writeFileSync(path, `${JSON.stringify(manifest, null, 2)}\n`);
  }
  if (!write && changes.length)
    throw new Error(
      `unguarded package builds: ${changes.join(", ")}; run node scripts/contrib/enroll-builds.mjs --write`,
    );
  return changes;
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  if (process.argv.slice(2).some((arg) => arg !== "--write"))
    throw new Error("usage: enroll-builds.mjs [--write]");
  const root = fileURLToPath(new URL("../..", import.meta.url));
  console.log(enrollBuilds(root, { write: process.argv.includes("--write") }));
}

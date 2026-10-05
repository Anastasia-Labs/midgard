// Lints the demo workspace one top-level entry at a time.
//
// Type-aware rules make eslint hold a TypeScript program for every project it
// visits. A single `eslint .` over the whole workspace keeps all of them
// alive at once and exhausts an 8 GB heap, so each package (and the loose
// files beside them) gets its own process. Coverage is the same as
// `eslint .`: every non-ignored entry of demo/ is linted under the same
// config, and paths the config ignores are skipped without warnings.
//
// Usage: node scripts/lint-by-package.mjs [extra eslint args...]

import { spawnSync } from "node:child_process";
import { readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const demo = join(dirname(fileURLToPath(import.meta.url)), "..");
const eslint = join(demo, "node_modules", "eslint", "bin", "eslint.js");
const skip = new Set(["node_modules", ".git"]);

const entries = readdirSync(demo, { withFileTypes: true })
  .filter((entry) => !skip.has(entry.name))
  .sort((a, b) => a.name.localeCompare(b.name, "en"));
const directories = entries
  .filter((entry) => entry.isDirectory())
  .map((entry) => entry.name);
const files = entries
  .filter((entry) => !entry.isDirectory())
  .map((entry) => entry.name);

const groups = [...directories.map((name) => [name]), files];
let failed = false;
for (const group of groups) {
  if (group.length === 0) continue;
  console.log(
    `==> eslint ${group.length === 1 ? group[0] : "workspace root files"}`,
  );
  const result = spawnSync(
    process.execPath,
    [
      "--max-old-space-size=8192",
      eslint,
      "--no-warn-ignored",
      "--max-warnings=0",
      ...process.argv.slice(2),
      ...group,
    ],
    { cwd: demo, stdio: "inherit" },
  );
  if (result.status !== 0) failed = true;
}
process.exit(failed ? 1 : 0);

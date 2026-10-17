import { packageTasks } from "../../demo/scripts/package-tasks.mjs";
import { writeFileSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { workspacePackages } from "./files.mjs";

export const enrollBuilds = (root, { write = false } = {}) => {
  const changes = [];
  for (const pkg of workspacePackages(root)) {
    const path = resolve(root, pkg.directory, "package.json");
    const manifest = JSON.parse(readFileSync(path, "utf8"));
    const tasks = packageTasks(manifest);
    if (manifest.scripts?.build && !tasks.build)
      throw new Error(
        `${pkg.name}: declare midgardTasks.build before enrolling its build`,
      );
    const expected = {
      ...(tasks.build
        ? {
            build: `node ../../scripts/contrib.mjs build --package ${pkg.name}`,
            "build:contrib-raw": "node ../scripts/package-tasks.mjs build",
          }
        : {}),
      ...(tasks.test ? { test: "node ../scripts/package-test.mjs" } : {}),
    };
    for (const hook of ["prebuild", "postbuild", "pretest", "posttest"])
      if (
        manifest.scripts?.[hook] &&
        tasks[hook.includes("build") ? "build" : "test"]
      )
        throw new Error(`${pkg.name}: move ${hook} behavior into midgardTasks`);
    if (
      Object.entries(expected).every(
        ([name, script]) => manifest.scripts?.[name] === script,
      )
    )
      continue;
    manifest.scripts = { ...manifest.scripts, ...expected };
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

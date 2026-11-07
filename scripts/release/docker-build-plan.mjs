import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import { buildClosure, workspacePackages } from "../contrib/files.mjs";

export const IMAGE_PACKAGES = [
  "midgard-node",
  "midgard-watcher",
  "da-committee-node",
];

const contextPath = (pkg) => {
  if (!/^demo\/[a-z0-9-]+$/u.test(pkg.directory))
    throw new Error(`unsupported image package directory: ${pkg.directory}`);
  return pkg.directory.slice("demo/".length);
};

/** Docker consumes the same dependency order and build recipes as contrib. */
export const imageBuildPlan = (root, name) => {
  const packages = buildClosure(root, name);
  return {
    package: name,
    manifests: workspacePackages(root).map(contextPath).sort(),
    sources: packages.map(contextPath),
    builds: packages
      .filter((pkg) => pkg.scripts?.build)
      .map((pkg) => {
        if (!/^(?:@[a-z0-9-]+\/)?[a-z0-9-]+$/u.test(pkg.name))
          throw new Error(`unsupported image package name: ${pkg.name}`);
        return pkg.name;
      }),
  };
};

export const imageBlocks = (plan) => ({
  manifests: plan.manifests
    .map((path) => `COPY ${path}/package.json ./${path}/package.json`)
    .join("\n"),
  sources: [
    "COPY scripts/package-tasks.mjs ./scripts/package-tasks.mjs",
    "COPY scripts/lib/vendored-imports.mjs ./scripts/lib/vendored-imports.mjs",
    ...plan.sources.map((path) => `COPY ${path} ./${path}`),
  ].join("\n"),
  builds: plan.builds
    .map((name) => `RUN pnpm --filter ${name} run build:contrib-raw`)
    .join("\n"),
});

/** Replace only enrolled blocks, leaving native stages and cache mounts intact. */
export const renderDockerfile = (source, plan) => {
  const blocks = imageBlocks(plan);
  let rendered = source;
  for (const kind of ["manifests", "sources", "builds", "public-builds"]) {
    const begin = `# BEGIN GENERATED ${kind}`;
    const end = `# END GENERATED ${kind}`;
    const before = rendered.split(`${begin}\n`);
    if (before.length !== 2)
      throw new Error(`expected exactly one ${begin} block`);
    const after = before[1].split(`\n${end}`);
    if (after.length !== 2)
      throw new Error(`expected exactly one ${end} block`);
    const content = blocks[kind === "public-builds" ? "builds" : kind];
    rendered = `${before[0]}${begin}\n${content}\n${end}${after[1]}`;
  }
  return rendered;
};

export const synchronizeDockerfiles = (root, { write = false } = {}) => {
  const stale = [];
  for (const name of IMAGE_PACKAGES) {
    const path = `demo/${name}/Dockerfile`;
    const file = resolve(root, path);
    const source = readFileSync(file, "utf8");
    const expected = renderDockerfile(source, imageBuildPlan(root, name));
    if (expected === source) continue;
    stale.push(path);
    if (write) writeFileSync(file, expected);
  }
  return stale;
};

if (
  process.argv[1] &&
  pathToFileURL(resolve(process.argv[1])).href === import.meta.url
) {
  const args = process.argv.slice(2);
  if (args.length !== 1 || !["--check", "--write"].includes(args[0])) {
    console.error(
      "usage: node scripts/release/docker-build-plan.mjs --check | --write",
    );
    process.exitCode = 2;
  } else {
    const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
    const stale = synchronizeDockerfiles(root, {
      write: args[0] === "--write",
    });
    if (stale.length && args[0] === "--check") {
      console.error(
        `Image build plans changed: ${stale.join(", ")}; run with --write`,
      );
      process.exitCode = 1;
    } else
      console.log(
        `Image build plans ${args[0] === "--write" ? "updated" : "verified"}: ${IMAGE_PACKAGES.length}`,
      );
  }
}

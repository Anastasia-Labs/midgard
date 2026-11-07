import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import {
  imageBuildPlan,
  imageBlocks,
  renderDockerfile,
  synchronizeDockerfiles,
} from "./docker-build-plan.mjs";

const fixture = (t) => {
  const root = mkdtempSync(resolve(tmpdir(), "midgard-image-plan-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const put = (directory, manifest) => {
    const path = resolve(root, "demo", directory, "package.json");
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, JSON.stringify(manifest));
  };
  return { root, put };
};
const compiled = (name, options = {}) => ({
  name,
  scripts: { build: "guard", "build:contrib-raw": "task owner" },
  midgardTasks: {
    build: {
      steps: [{ command: ["node", "build.mjs"] }],
      outputs: ["dist"],
      ...(options.prerequisites
        ? { prerequisites: options.prerequisites }
        : {}),
    },
  },
  ...(options.dependencies ? { dependencies: options.dependencies } : {}),
  ...(options.devDependencies
    ? { devDependencies: options.devDependencies }
    : {}),
});

test("an added runtime edge updates source copies and both build phases", (t) => {
  const { root, put } = fixture(t);
  put("app", compiled("app"));
  const first = imageBuildPlan(root, "app");
  assert.deepEqual(first.builds, ["app"]);
  put("library", compiled("library"));
  put("app", compiled("app", { dependencies: { library: "workspace:*" } }));
  const next = imageBuildPlan(root, "app");
  assert.deepEqual(next.sources, ["library", "app"]);
  const template = ["manifests", "sources", "builds", "public-builds"]
    .map((kind) => `# BEGIN GENERATED ${kind}\nold\n# END GENERATED ${kind}`)
    .join("\n");
  const rendered = renderDockerfile(template, next);
  assert.ok(rendered.includes("COPY library ./library"));
  assert.equal(
    rendered.split("RUN pnpm --filter library run build:contrib-raw").length -
      1,
    2,
  );
  assert.ok(
    rendered.indexOf("--filter library") < rendered.indexOf("--filter app"),
  );
});

test("build prerequisites are included without pulling unrelated development packages", (t) => {
  const { root, put } = fixture(t);
  put("compiler-input", compiled("compiler-input"));
  put("test-only", compiled("test-only"));
  put("runtime-js", { name: "runtime-js", exports: "./index.js" });
  put(
    "app",
    compiled("app", {
      dependencies: { "runtime-js": "workspace:*" },
      devDependencies: {
        "compiler-input": "workspace:*",
        "test-only": "workspace:*",
      },
      prerequisites: [{ kind: "build", package: "compiler-input" }],
    }),
  );
  const plan = imageBuildPlan(root, "app");
  assert.ok(plan.sources.includes("runtime-js"));
  assert.ok(plan.sources.includes("compiler-input"));
  assert.ok(!plan.sources.includes("test-only"));
  assert.deepEqual(plan.builds, ["compiler-input", "app"]);
  assert.ok(!imageBlocks(plan).builds.includes("runtime-js"));
  // All manifests remain available to frozen pnpm workspace resolution.
  assert.deepEqual(plan.manifests, [
    "app",
    "compiler-input",
    "runtime-js",
    "test-only",
  ]);
});

test("missing or duplicate generated blocks refuse instead of silently leaving old plans", () => {
  const plan = { manifests: [], sources: [], builds: [] };
  assert.throws(() => renderDockerfile("FROM node\n", plan), /exactly one/u);
  assert.throws(
    () =>
      renderDockerfile(
        "# BEGIN GENERATED manifests\n# BEGIN GENERATED manifests\n",
        plan,
      ),
    /exactly one/u,
  );
});

test("all checked-in images use current package graph and declared recipes", () => {
  const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
  assert.deepEqual(synchronizeDockerfiles(root), []);
});

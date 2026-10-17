import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

// Every place that builds the sidecar names its Go toolchain; each must be the
// one native/go.mod declares, or a build silently uses another compiler (or,
// with GOTOOLCHAIN set below go.mod's, refuses to build at all).
const demo = fileURLToPath(new URL("../../", import.meta.url));
const repository = join(demo, "..");
const read = (path: string) => readFileSync(path, "utf8");

const goModVersion = (() => {
  const match = /^go (\d+\.\d+\.\d+)$/mu.exec(
    read(join(demo, "l1-node-transport/native/go.mod")),
  );
  if (match === null) throw new Error("native/go.mod declares no go version");
  return match[1]!;
})();

/** Every Go version a build file names: images, setup-go, version asserts. */
const goVersionsIn = (text: string): string[] =>
  [
    ...text.matchAll(
      /golang:(\d[\d.]*)-|go-version:\s*"(\d[\d.]*)"|GOTOOLCHAIN:\s*"go(\d[\d.]*)"|=\s*"go(\d[\d.]*)"/gu,
    ),
  ].map((match) => match.slice(1).find((group) => group !== undefined)!);

const roleDockerfiles = [
  "midgard-node",
  "midgard-watcher",
  "da-committee-node",
];
const workflowsDirectory = join(repository, ".github/workflows");
const goWorkflows = readdirSync(workflowsDirectory)
  .filter((name) => /\.ya?ml$/u.test(name))
  .filter((name) =>
    read(join(workflowsDirectory, name)).includes("actions/setup-go"),
  );

const pinned: ReadonlyArray<readonly [string, string]> = [
  [
    "midgard-node-tools/src/full-stack/prerequisites.ts",
    join(demo, "midgard-node-tools/src/full-stack/prerequisites.ts"),
  ],
  ...roleDockerfiles.map(
    (role) => [`${role}/Dockerfile`, join(demo, role, "Dockerfile")] as const,
  ),
  ...goWorkflows.map(
    (name) =>
      [`.github/workflows/${name}`, join(workflowsDirectory, name)] as const,
  ),
];

describe("the sidecar's Go toolchain pins", () => {
  it("covers both CI workflows that build the sidecar", () => {
    expect(goWorkflows).toEqual(
      expect.arrayContaining(["midgard-node-ci.yml", "midgard-watcher-ci.yml"]),
    );
  });

  it.each(pinned)("%s pins the go.mod toolchain", (_, path) => {
    const versions = goVersionsIn(read(path));
    expect(versions.length).toBeGreaterThan(0);
    expect(new Set(versions)).toEqual(new Set([goModVersion]));
  });
});

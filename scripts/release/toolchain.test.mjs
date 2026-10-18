import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { test } from "node:test";

import {
  ENROLLED,
  MANIFEST,
  tagPins,
  toolchainProblems,
} from "./toolchain.mjs";

const MANIFEST_VERSIONS = {
  node: "22.22.2",
  go: "1.26.5",
  postgres: "15",
  kupo: "2.11.0",
};
const DOCKERFILE = [
  "ARG MIDGARD_NODE_BUILD_IMAGE=node:22.22.2",
  "ARG MIDGARD_NODE_RUNTIME_IMAGE=node:22.22.2-bookworm-slim",
  "ARG MIDGARD_GO_BUILD_IMAGE=golang:1.26.5-bookworm",
  `RUN test "$(go version | awk '{print $3}')" = "go1.26.5"`,
].join("\n");
const COMPOSE = [
  "  db:",
  "    image: postgres:15.15-alpine@sha256:d4c3",
  "  kupo:",
  "    image: cardanosolutions/kupo:${KUPO_IMAGE_TAG:-v2.11.0@sha256:0a8c}",
].join("\n");

const tree = (t, files) => {
  const root = mkdtempSync(resolve(tmpdir(), "midgard-toolchain-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  for (const [path, text] of Object.entries({
    [MANIFEST]: JSON.stringify(MANIFEST_VERSIONS),
    ...files,
  })) {
    mkdirSync(dirname(resolve(root, path)), { recursive: true });
    writeFileSync(resolve(root, path), text);
  }
  return root;
};
const enrolled = (paths) =>
  ENROLLED.filter((entry) => paths.includes(entry.path));
const SCENE = {
  "demo/midgard-node/Dockerfile": DOCKERFILE,
  "demo/midgard-node/docker-compose.yaml": COMPOSE,
};

test("a tag pins its version, a v-prefixed release or a variant of it", () => {
  assert.ok(tagPins("22.22.2", "22.22.2"));
  assert.ok(tagPins("v7.0.0", "7.0.0"));
  assert.ok(tagPins("22.22.2-bookworm-slim", "22.22.2"));
  assert.ok(tagPins("15.15-alpine", "15"));
  assert.ok(tagPins("2630.1-hotfix-3f6cb73", "2630.1-hotfix"));
  assert.ok(!tagPins("16-alpine", "15"));
  assert.ok(!tagPins("22.22.20", "22.22.2"));
  assert.ok(!tagPins("v22.22.1", "22.22.2"));
});

test("files that agree with the manifest have no problems", (t) => {
  const root = tree(t, SCENE);
  assert.deepEqual(toolchainProblems(root, enrolled(Object.keys(SCENE))), []);
});

test("a pin that differs from the manifest is named with its file", (t) => {
  const root = tree(t, {
    ...SCENE,
    "demo/midgard-node/Dockerfile": DOCKERFILE.replace(
      "golang:1.26.5-bookworm",
      "golang:1.26.4-bookworm",
    ),
    "demo/midgard-node/docker-compose.yaml": COMPOSE.replace(
      "postgres:15.15",
      "postgres:16.4",
    ),
  });
  assert.deepEqual(toolchainProblems(root, enrolled(Object.keys(SCENE))), [
    "demo/midgard-node/Dockerfile: golang:1.26.4-bookworm differs from go 1.26.5",
    "demo/midgard-node/docker-compose.yaml: postgres:16.4-alpine differs from postgres 15",
  ]);
});

test("a compose default tag is read through its variable", (t) => {
  const root = tree(t, {
    ...SCENE,
    "demo/midgard-node/docker-compose.yaml": COMPOSE.replace(
      "v2.11.0",
      "v2.10.0",
    ),
  });
  assert.deepEqual(toolchainProblems(root, enrolled(Object.keys(SCENE))), [
    "demo/midgard-node/docker-compose.yaml: kupo:v2.10.0 differs from kupo 2.11.0",
  ]);
});

test("an enrolled file with no pin left, or a manifest tool no file pins, fails", (t) => {
  const root = tree(t, {
    ...SCENE,
    "demo/midgard-node/docker-compose.yaml": "services: {}\n",
  });
  assert.deepEqual(toolchainProblems(root, enrolled(Object.keys(SCENE))), [
    "demo/midgard-node/docker-compose.yaml: no version pin found",
    `${MANIFEST}: postgres is pinned by no enrolled file`,
    `${MANIFEST}: kupo is pinned by no enrolled file`,
  ]);
});

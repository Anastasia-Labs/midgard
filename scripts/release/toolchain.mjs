#!/usr/bin/env node
// The two ways to get Midgard's toolchain and services, the Dockerfiles with
// their compose files and the Nix flake, take their versions from one file:
// demo/toolchain.json. The flake reads it directly; the Docker side cannot, so
// this check holds every version pin it carries to the manifest.
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const MANIFEST = "demo/toolchain.json";

// Image names whose tag is a manifest version.
const IMAGES = {
  node: "node",
  golang: "go",
  rust: "rust",
  postgres: "postgres",
  "cardano-node": "cardano-node",
  ogmios: "ogmios",
  kupo: "kupo",
  "mithril-client": "mithril-client",
};
// `image:tag`, or a compose default `image:${VAR:-tag}`.
const IMAGE_TAG =
  /(?<![\w.-])([a-z][\w-]*):(?:\$\{[A-Z_]+:-)?([0-9v][^\s"'@}]*)/gu;
const GO_VERSION = /\bgo([0-9]+\.[0-9]+\.[0-9]+)\b|go-version: "([^"]+)"/gu;

const images = (text) =>
  [...text.matchAll(IMAGE_TAG)]
    .filter(([, image]) => image in IMAGES)
    .map(([, image, tag]) => ({
      tool: IMAGES[image],
      tag,
      match: `${image}:${tag}`,
    }));
const goVersions = (text) =>
  [...text.matchAll(GO_VERSION)].map(([match, go, setup]) => ({
    tool: "go",
    tag: go ?? setup,
    match,
  }));

/** Each enrolled file and the pins read from it. */
export const ENROLLED = [
  ...["midgard-node", "midgard-watcher", "da-committee-node"].map((pkg) => ({
    path: `demo/${pkg}/Dockerfile`,
    pins: (text) => [...images(text), ...goVersions(text)],
  })),
  ...[
    "demo/midgard-node/docker-compose.yaml",
    "demo/midgard-node/docker-compose.dev.yaml",
    "demo/midgard-node/docker-compose.kupmios.yaml",
    "demo/midgard-node-tools/devnet/phase4-process/compose.yaml",
    "demo/midgard-node-tools/devnet/phase4-process/scripts/common.sh",
  ].map((path) => ({ path, pins: images })),
  // CI builds the transport with setup-go and every job takes Node from
  // .nvmrc; the test databases its jobs start are not a build input.
  ...[
    ".github/workflows/midgard-node-ci.yml",
    ".github/workflows/midgard-watcher-ci.yml",
  ].map((path) => ({ path, pins: goVersions })),
  {
    path: "demo/.nvmrc",
    pins: (text) => [{ tool: "node", tag: text.trim(), match: text.trim() }],
  },
];

/**
 * A tag pins `version` when it is that version (a leading `v` aside) or a
 * variant of it: `22.22.2-bookworm-slim`, `15.15-alpine` for major `15`,
 * `2630.1-hotfix-3f6cb73` for `2630.1-hotfix`.
 */
export const tagPins = (tag, version) => {
  const bare = tag.replace(/^v(?=[0-9])/u, "");
  return (
    bare === version ||
    bare.startsWith(`${version}-`) ||
    bare.startsWith(`${version}.`)
  );
};

/** Every pin that disagrees with the manifest, or that the check lost track of. */
export const toolchainProblems = (root, enrolled = ENROLLED) => {
  const manifest = JSON.parse(readFileSync(resolve(root, MANIFEST), "utf8"));
  const problems = [];
  const seen = new Set();
  for (const { path, pins } of enrolled) {
    const found = pins(readFileSync(resolve(root, path), "utf8"));
    // A file that stops matching would otherwise pass in silence.
    if (found.length === 0) problems.push(`${path}: no version pin found`);
    for (const { tool, tag, match } of found) {
      seen.add(tool);
      const version = manifest[tool];
      if (version === undefined)
        problems.push(
          `${path}: ${match} pins ${tool}, which ${MANIFEST} lacks`,
        );
      else if (!tagPins(tag, version))
        problems.push(`${path}: ${match} differs from ${tool} ${version}`);
    }
  }
  for (const tool of Object.keys(manifest))
    if (!seen.has(tool))
      problems.push(`${MANIFEST}: ${tool} is pinned by no enrolled file`);
  return problems;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const root = resolve(fileURLToPath(new URL("../..", import.meta.url)));
  const problems = toolchainProblems(root);
  for (const problem of problems) console.error(problem);
  if (problems.length > 0) {
    console.error(
      `toolchain: edit ${MANIFEST} and every pin above together (the Nix flake reads the manifest)`,
    );
    process.exitCode = 1;
  } else console.log(`toolchain: every enrolled pin matches ${MANIFEST}`);
}

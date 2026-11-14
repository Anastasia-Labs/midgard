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
const GO_VERSION =
  /\bgo([0-9]+\.[0-9]+\.[0-9]+)\b|go-version: ["']?([^"'\s]+)/gu;
// `.env.example` defaults the compose files read: `OGMIOS_IMAGE_TAG=v7.0.0@…`.
const ENV_TAG = /^([A-Z_]+)_IMAGE_TAG=([^\s@]+)/gmu;

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

const envTags = (text) =>
  [...text.matchAll(ENV_TAG)].map(([match, name, tag]) => ({
    tool: name.toLowerCase().replaceAll("_", "-"),
    tag,
    match,
  }));

/**
 * Each enrolled file, the pins read from it and the tools it must pin. A pin
 * that changes to a form the readers miss (`kupo:latest`, a digest alone)
 * then fails as a missing pin instead of passing.
 */
export const ENROLLED = [
  ...["midgard-node", "midgard-watcher", "da-committee-node"].map((pkg) => ({
    path: `demo/${pkg}/Dockerfile`,
    pins: (text) => [...images(text), ...goVersions(text)],
    tools: ["node", "go", "rust"],
  })),
  ...[
    "demo/midgard-node/docker-compose.yaml",
    "demo/midgard-node/docker-compose.dev.yaml",
  ].map((path) => ({ path, pins: images, tools: ["postgres"] })),
  {
    path: "demo/midgard-node/docker-compose.kupmios.yaml",
    pins: images,
    tools: ["cardano-node", "ogmios", "kupo", "mithril-client"],
  },
  {
    path: "demo/midgard-node/.env.example",
    pins: envTags,
    tools: ["cardano-node", "ogmios", "kupo", "mithril-client"],
  },
  ...[
    "demo/midgard-node-tools/devnet/phase4-process/compose.yaml",
    "demo/midgard-node-tools/devnet/phase4-process/scripts/common.sh",
  ].map((path) => ({
    path,
    pins: images,
    tools: ["cardano-node", "ogmios", "kupo", "postgres"],
  })),
  // CI builds the transport with setup-go and every job takes Node from
  // .nvmrc; the test databases its jobs start are not a build input.
  ...[
    ".github/workflows/midgard-node-ci.yml",
    ".github/workflows/midgard-watcher-ci.yml",
  ].map((path) => ({ path, pins: goVersions, tools: ["go"] })),
  // The module and the full-stack prerequisite build name the same Go.
  {
    path: "demo/l1-node-transport/native/go.mod",
    pins: (text) =>
      [...text.matchAll(/^go ([0-9.]+)$/gmu)].map(([match, tag]) => ({
        tool: "go",
        tag,
        match,
      })),
    tools: ["go"],
  },
  {
    path: "demo/midgard-node-tools/src/full-stack/prerequisites.ts",
    pins: goVersions,
    tools: ["go"],
  },
  {
    path: "demo/.nvmrc",
    pins: (text) => [{ tool: "node", tag: text.trim(), match: text.trim() }],
    tools: ["node"],
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
  for (const { path, pins, tools } of enrolled) {
    const found = pins(readFileSync(resolve(root, path), "utf8"));
    for (const tool of tools)
      if (!found.some((pin) => pin.tool === tool))
        problems.push(`${path}: no ${tool} version pin found`);
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

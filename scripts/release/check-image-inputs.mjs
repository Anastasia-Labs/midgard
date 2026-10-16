#!/usr/bin/env node
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { profileDigest } from "../../demo/scripts/deployment-profiles.mjs";
import {
  blueprintHash,
  blueprintSourceHash,
} from "../../demo/scripts/lib/blueprint-stamp.mjs";

export const verifyPublicImageInputs = ({ root, profile, revision }) => {
  if (
    !/^[0-9a-f]{40}$/u.test(revision) ||
    !["preprod-public", "mainnet"].includes(profile)
  )
    throw new Error(
      "Public image requires a full source revision and actual public profile selection",
    );
  if (process.versions.node !== "22.22.2")
    throw new Error("Public image must use pinned Node22.22.2");
  const blueprintPath = resolve(root, "onchain/aiken/plutus.json");
  const record = JSON.parse(
    readFileSync(`${blueprintPath}.deployment.json`, "utf8"),
  );
  const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8"));
  if (
    !Array.isArray(blueprint.validators) ||
    blueprint.validators.length === 0 ||
    new Set(blueprint.validators.map((entry) => entry.title)).size !==
      blueprint.validators.length ||
    blueprint.validators.some(
      (entry) => typeof entry.title !== "string" || entry.title.trim() === "",
    )
  )
    throw new Error(
      "Public image blueprint has no complete unique validator census",
    );
  if (
    record.profile.name !== profile ||
    profileDigest(record.profile) !== record.profileDigest ||
    record.blueprintHash !== blueprintHash(blueprintPath) ||
    record.sourceHash !== blueprintSourceHash(root)
  )
    throw new Error(
      "Public image blueprint differs from frozen source/profile",
    );
  const selected = readFileSync(
    resolve(root, "demo/midgard-core/src/generated-deployment-profiles.ts"),
    "utf8",
  );
  if (
    !new RegExp(
      `SELECTED_DEPLOYMENT_PROFILE\\s*=\\s*DEPLOYMENT_PROFILES\\[\\s*"${profile}"\\s*\\]`,
      "u",
    ).test(selected)
  )
    throw new Error(
      "Public image core module does not select its declared public profile",
    );
  const profiles = selected.match(
    /export const DEPLOYMENT_PROFILES = ([\s\S]+?) as const;/u,
  )?.[1];
  if (
    profiles === undefined ||
    profileDigest(JSON.parse(profiles)[profile]) !== record.profileDigest
  )
    throw new Error(
      "Public image generated profile differs from blueprint profile metadata",
    );
  const sha256 = (path) =>
    createHash("sha256")
      .update(readFileSync(resolve(root, path)))
      .digest("hex");
  return {
    schemaVersion: "midgard-public-image-inputs-v1",
    sourceRevision: revision,
    deploymentProfileName: profile,
    deploymentProfileDigest: record.profileDigest,
    blueprintHash: record.blueprintHash,
    aikenSourceHash: record.sourceHash,
    selectedCoreProfileHash: sha256(
      "demo/midgard-core/src/generated-deployment-profiles.ts",
    ),
    pnpmLockHash: sha256("demo/pnpm-lock.yaml"),
    aikenLockHash: sha256("onchain/aiken/aiken.lock"),
  };
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const [profile, revision, output] = process.argv.slice(2);
  const root = resolve(fileURLToPath(new URL("../..", import.meta.url)));
  if (!output || process.argv.length !== 5)
    throw new Error(
      "Usage: check-image-inputs.mjs PROFILE FULL_REVISION OUTPUT",
    );
  writeFileSync(
    output,
    `${JSON.stringify(verifyPublicImageInputs({ root, profile, revision }), null, 2)}\n`,
    { flag: "wx" },
  );
}

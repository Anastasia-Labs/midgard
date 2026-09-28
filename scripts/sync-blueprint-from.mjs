#!/usr/bin/env node

// Copy a fresh blueprint from another checkout of this repository instead of
// rebuilding it.
//
// Usage: node scripts/sync-blueprint-from.mjs <source-checkout> [--dry-run]
//
// It copies onchain/aiken/plutus.json and its gitignored build record
// onchain/aiken/plutus.json.deployment.json from <source-checkout> into the
// checkout this script lives in, and only when
//   (a) the source's blueprint stamp is fresh for the source tree, and
//   (b) every compiler input the stamp covers (onchain/aiken lib, validators,
//       env, aiken.toml, aiken.lock) and the pinned compiler are identical in
//       the two trees, and
//   (c) the source was built for the deployment profile this checkout selects
//       (SELECTED_DEPLOYMENT_PROFILE in
//       demo/midgard-core/src/generated-deployment-profiles.ts), with the same
//       profile digest. The profile is an `aiken build --env` flag, not a
//       stamped input, so (b) alone cannot see it.
// (a) and (b) use demo/scripts/lib/blueprint-stamp.mjs, the module the vitest
// global setup and scripts/doctor.mjs use, so "fresh" means exactly what they
// mean. The pair is copied beside the destination first and judged again
// there; only a fresh pair is renamed into place, so a refused copy leaves
// this checkout's blueprint untouched.
//
// Exit codes: 0 copied (or, with --dry-run, would copy); 1 refused (the
// source is stale, an input or the profile differs, or the copy is not fresh
// here); 2 bad arguments or a stamp or profile that could not be judged.

import {
  copyFileSync,
  existsSync,
  readFileSync,
  realpathSync,
  renameSync,
  rmSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  blueprintSourceFiles,
  blueprintSourceHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "../demo/scripts/lib/blueprint-stamp.mjs";
import { pinnedAikenVersion } from "../onchain/aiken/scripts/pinned-compiler.mjs";

export const EXIT = { copied: 0, refused: 1, usage: 2 };

const usage =
  "usage: node scripts/sync-blueprint-from.mjs <source-checkout> [--dry-run]";

const blueprintOf = (root) => resolve(root, "onchain/aiken/plutus.json");

/**
 * The first compiler input that differs between the two trees, as a
 * sentence, or null when every input the stamp covers is byte-identical.
 */
export const firstDifferingInput = (source, destination) => {
  const sourcePin = pinnedAikenVersion(source);
  const destinationPin = pinnedAikenVersion(destination);
  if (sourcePin !== destinationPin) {
    return `the pinned compiler (AIKEN_FORK_VERSION): source '${sourcePin}', here '${destinationPin}'`;
  }
  const inSource = new Map(
    blueprintSourceFiles(source).map((file) => [file.name, file.path]),
  );
  const inDestination = new Map(
    blueprintSourceFiles(destination).map((file) => [file.name, file.path]),
  );
  const names = [
    ...new Set([...inSource.keys(), ...inDestination.keys()]),
  ].sort((left, right) => (left < right ? -1 : left > right ? 1 : 0));
  for (const name of names) {
    const sourcePath = inSource.get(name);
    const destinationPath = inDestination.get(name);
    if (sourcePath === undefined) {
      return `onchain/aiken/${name}: exists here, not in the source`;
    }
    if (destinationPath === undefined) {
      return `onchain/aiken/${name}: exists in the source, not here`;
    }
    if (!readFileSync(sourcePath).equals(readFileSync(destinationPath))) {
      return `onchain/aiken/${name}: contents differ`;
    }
  }
  // The file-by-file walk found nothing; the stamp's own digest must agree.
  if (blueprintSourceHash(source) !== blueprintSourceHash(destination)) {
    return "the blueprint source hash (no single file identified)";
  }
  return null;
};

const profilesModule = "demo/midgard-core/src/generated-deployment-profiles.ts";

/**
 * The deployment profile a checkout selects and its digest, read from the
 * generated module the way `deployment-profiles.mjs check` reads it.
 */
export const selectedProfile = (root) => {
  const text = readFileSync(resolve(root, profilesModule), "utf8");
  const name = text.match(
    /SELECTED_DEPLOYMENT_PROFILE\s*=\s*DEPLOYMENT_PROFILES\[\s*"([^"]+)"\s*\]/u,
  )?.[1];
  if (name === undefined) {
    throw new Error(`${profilesModule} selects no deployment profile`);
  }
  const digests =
    text.match(/DEPLOYMENT_PROFILE_DIGESTS\s*=\s*\{([^}]*)\}/u)?.[1] ?? "";
  const digest = [
    ...digests.matchAll(
      /(?:"([^"]+)"|([A-Za-z_$][\w$]*))\s*:\s*"([0-9a-f]{64})"/gu,
    ),
  ].find((match) => (match[1] ?? match[2]) === name)?.[3];
  if (digest === undefined) {
    throw new Error(`${profilesModule} has no digest for profile '${name}'`);
  }
  return { name, digest };
};

/**
 * Why the build record at `recordPath` was not built for the profile `root`
 * selects, as a sentence, or null when its name and digest both match.
 */
export const profileMismatch = (recordPath, root) => {
  const record = JSON.parse(readFileSync(recordPath, "utf8"));
  const here = selectedProfile(root);
  const builtName = record?.profile?.name;
  if (builtName !== here.name) {
    return `the source was built for profile '${String(builtName)}', and this checkout selects '${here.name}'`;
  }
  if (record.profileDigest !== here.digest) {
    return `profile '${here.name}' differs: the source was built with digest ${String(record.profileDigest)}, this checkout's is ${here.digest}`;
  }
  return null;
};

export const main = (
  argv,
  {
    root = resolve(dirname(fileURLToPath(import.meta.url)), ".."),
    stdout = (text) => process.stdout.write(text),
    stderr = (text) => process.stderr.write(text),
    copyFile = copyFileSync,
  } = {},
) => {
  const dryRun = argv.includes("--dry-run");
  const positional = argv.filter((arg) => arg !== "--dry-run");
  if (positional.length !== 1 || positional[0].startsWith("-")) {
    stderr(`${usage}\n`);
    return EXIT.usage;
  }
  const source = resolve(positional[0]);
  if (!existsSync(resolve(source, "onchain/aiken"))) {
    stderr(`${source} has no onchain/aiken; is it a Midgard checkout?\n`);
    return EXIT.usage;
  }
  if (realpathSync(source) === realpathSync(root)) {
    stderr(`${source} is this checkout; nothing to copy\n`);
    return EXIT.usage;
  }

  const sourceVerdict = checkBlueprintStamp({ root: source });
  if (sourceVerdict.status === "unknown") {
    stderr(`cannot judge the source blueprint: ${sourceVerdict.detail}\n`);
    return EXIT.usage;
  }
  if (sourceVerdict.status !== "fresh") {
    stderr(
      `refused: the source blueprint is not fresh for its own tree: ${sourceVerdict.detail}\n` +
        `Rebuild it there, or build here: pnpm --dir demo deployment:build preprod-testing\n`,
    );
    return EXIT.refused;
  }

  let difference;
  try {
    difference = firstDifferingInput(source, root);
  } catch (error) {
    stderr(`cannot compare the two trees: ${error.message}\n`);
    return EXIT.usage;
  }
  if (difference !== null) {
    stderr(
      `refused: the source blueprint was built from other inputs than this tree's.\n` +
        `First differing input: ${difference}\n` +
        `Build here instead: pnpm --dir demo deployment:build preprod-testing\n`,
    );
    return EXIT.refused;
  }

  const sourceBlueprint = blueprintOf(source);
  let profileDifference;
  try {
    profileDifference = profileMismatch(buildRecordPath(sourceBlueprint), root);
  } catch (error) {
    stderr(`cannot compare the deployment profiles: ${error.message}\n`);
    return EXIT.usage;
  }
  if (profileDifference !== null) {
    stderr(
      `refused: ${profileDifference}.\n` +
        `Build here instead: pnpm --dir demo deployment:build <this checkout's profile>\n`,
    );
    return EXIT.refused;
  }

  const destinationBlueprint = blueprintOf(root);
  if (dryRun) {
    stdout(
      `would copy ${sourceBlueprint} and its build record to ${destinationBlueprint}: the source is fresh, every stamped input is identical and it was built for this checkout's profile\n`,
    );
    return EXIT.copied;
  }
  // Copy the pair beside the destination and judge it there, so a source that
  // changed after the checks above never replaces this checkout's blueprint.
  const stagedBlueprint = `${destinationBlueprint}.sync-blueprint-from.tmp`;
  const stagedRecord = buildRecordPath(stagedBlueprint);
  const unstage = () => {
    rmSync(stagedBlueprint, { force: true });
    rmSync(stagedRecord, { force: true });
  };
  let refusal = null;
  let refusalCode = EXIT.refused;
  try {
    copyFile(sourceBlueprint, stagedBlueprint);
    copyFile(buildRecordPath(sourceBlueprint), stagedRecord);
    const staged = checkBlueprintStamp({
      root,
      blueprintPath: stagedBlueprint,
    });
    if (staged.status !== "fresh") {
      refusal = staged.detail;
      if (staged.status === "unknown") refusalCode = EXIT.usage;
    } else {
      refusal = profileMismatch(stagedRecord, root);
    }
  } catch (error) {
    unstage();
    throw error;
  }
  if (refusal !== null) {
    unstage();
    stderr(
      `refused: the copied blueprint is not fresh here, so nothing was replaced: ${refusal}\n` +
        `Rebuild it: pnpm --dir demo deployment:build <this checkout's profile>\n`,
    );
    return refusalCode;
  }
  // The record names the blueprint's hash, so the pair lands together: the
  // blueprint first, then the record that vouches for it.
  renameSync(stagedBlueprint, destinationBlueprint);
  renameSync(stagedRecord, buildRecordPath(destinationBlueprint));
  stdout(
    `copied from ${source}: ${destinationBlueprint} matches its sources, the pinned compiler and this checkout's deployment profile\n`,
  );
  return EXIT.copied;
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  process.exitCode = main(process.argv.slice(2));
}

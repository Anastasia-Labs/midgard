import { spawnSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { setTimeout as pause } from "node:timers/promises";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import {
  generateProfiles,
  profileDigest,
  readProfiles,
} from "../scripts/deployment-profiles.mjs";
import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "../scripts/lib/blueprint-stamp.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export const interactiveEmulatorProfile = "preprod-emulator-testing";
export const interactiveEmulatorBlueprint = resolve(
  root,
  "onchain/aiken/build/interactive-emulator/plutus.json",
);
export const interactiveEmulatorSetup = fileURLToPath(import.meta.url);

/** Select a matching profile only inside this Vitest project's module graph. */
export const interactiveEmulatorPlugin = () => ({
  name: "midgard-interactive-emulator-profile",
  transform(code, id) {
    if (
      id !==
      resolve(root, "demo/midgard-core/src/generated-deployment-profiles.ts")
    )
      return null;
    for (const [exportName, table] of [
      ["SELECTED_DEPLOYMENT_PROFILE", "DEPLOYMENT_PROFILES"],
      ["SELECTED_DEPLOYMENT_PROFILE_DIGEST", "DEPLOYMENT_PROFILE_DIGESTS"],
    ]) {
      const pattern = new RegExp(`export const ${exportName} =[^;]+;`);
      if (!pattern.test(code))
        throw new Error(`Missing generated ${exportName}`);
      code = code.replace(
        pattern,
        `export const ${exportName} = ${table}["${interactiveEmulatorProfile}"];`,
      );
    }
    return { code, map: null };
  },
});

const fresh = () => {
  if (
    checkBlueprintStamp({ blueprintPath: interactiveEmulatorBlueprint })
      .status !== "fresh"
  )
    return false;
  const record = JSON.parse(
    readFileSync(buildRecordPath(interactiveEmulatorBlueprint), "utf8"),
  );
  return (
    record.profileDigest ===
    profileDigest(readProfiles()[interactiveEmulatorProfile])
  );
};

const lock = `${interactiveEmulatorBlueprint}.lock`;

/** Publish a lock directory holding this process's PID, or return false. */
const tryLock = () => {
  const staged = `${lock}.${process.pid}.${randomUUID()}`;
  mkdirSync(staged);
  writeFileSync(resolve(staged, "pid"), `${process.pid}\n`);
  try {
    // rename(2) refuses a non-empty target, so exactly one contender wins, and
    // the lock never exists without its holder's PID.
    renameSync(staged, lock);
    return true;
  } catch (error) {
    rmSync(staged, { recursive: true, force: true });
    if (error.code === "ENOTEMPTY" || error.code === "EEXIST") return false;
    throw error;
  }
};

/** Move the lock aside before deleting it, so no contender sees it half-removed. */
const discardLock = () => {
  const discarded = `${lock}.discarded.${randomUUID()}`;
  try {
    renameSync(lock, discarded);
  } catch (error) {
    if (error.code === "ENOENT") return;
    throw error;
  }
  rmSync(discarded, { recursive: true, force: true });
};

const lockHolder = () => {
  try {
    return Number.parseInt(readFileSync(resolve(lock, "pid"), "utf8"), 10);
  } catch (error) {
    if (error.code === "ENOENT" || error.code === "ENOTDIR") return undefined;
    throw error;
  }
};

const running = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return error.code === "EPERM";
  }
};

/**
 * A setup killed mid-build leaves its lock behind. Contenders break it one at a
 * time, re-reading the holder under the guard so a lock another contender has
 * just re-taken is never broken.
 */
const breakStaleLock = () => {
  const guard = `${lock}.break`;
  try {
    mkdirSync(guard);
  } catch (error) {
    if (error.code === "EEXIST") return;
    throw error;
  }
  try {
    const holder = lockHolder();
    if (holder !== undefined && !running(holder)) discardLock();
  } finally {
    rmSync(guard, { recursive: true, force: true });
  }
};

/** Cached separately: never changes the live-testing blueprint or selected source profile. */
export default async function setup() {
  // A stamp alone cannot prove generated constants agree with their producer.
  await generateProfiles("preprod-testing", true);
  mkdirSync(dirname(interactiveEmulatorBlueprint), { recursive: true });
  const deadline = Date.now() + 600_000;
  for (;;) {
    if (fresh()) return;
    if (tryLock()) break;
    breakStaleLock();
    if (Date.now() >= deadline)
      throw new Error(
        `Timed out waiting for emulator blueprint build lock ${lock} held by PID ${lockHolder()}`,
      );
    await pause(200);
  }
  try {
    if (fresh()) return;
    const compilerPath = defaultAikenBinary();
    const compiler = assertPinnedAiken(compilerPath);
    const profile = readProfiles()[interactiveEmulatorProfile];
    const sourceHash = blueprintSourceHash(root);
    const recordPath = buildRecordPath(interactiveEmulatorBlueprint);
    if (existsSync(recordPath)) rmSync(recordPath);
    const result = spawnSync(
      compilerPath,
      [
        "build",
        "--env",
        interactiveEmulatorProfile.replaceAll("-", "_"),
        "--out",
        interactiveEmulatorBlueprint,
      ],
      {
        cwd: resolve(root, "onchain/aiken"),
        stdio: "inherit",
      },
    );
    if (result.error) throw result.error;
    if (result.status !== 0)
      throw new Error(
        `Interactive emulator blueprint build failed (${result.status})`,
      );
    writeFileSync(
      recordPath,
      JSON.stringify(
        {
          profile,
          profileDigest: profileDigest(profile),
          blueprintHash: blueprintHash(interactiveEmulatorBlueprint),
          sourceHash,
          compiler,
        },
        null,
        2,
      ) + "\n",
    );
    if (!fresh())
      throw new Error(
        "Interactive emulator blueprint changed during its build",
      );
  } finally {
    discardLock();
  }
}

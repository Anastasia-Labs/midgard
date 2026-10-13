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

/**
 * One emulator-only profile's separately stamped blueprint, built under
 * `onchain/aiken/build/<directory>/plutus.json`, and the Vitest pieces that
 * select it: a plugin that selects the matching TypeScript profile inside the
 * project's module graph, and a global setup that builds (or admits) the
 * blueprint. The checkout's selected profile and default blueprint never
 * change.
 */
export const emulatorProfileBlueprint = (profileName, directory) => {
  const blueprintPath = resolve(
    root,
    `onchain/aiken/build/${directory}/plutus.json`,
  );

  /** Select the profile only inside this Vitest project's module graph. */
  const plugin = () => ({
    name: `midgard-${directory}-profile`,
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
          `export const ${exportName} = ${table}["${profileName}"];`,
        );
      }
      return { code, map: null };
    },
  });

  /** Why the cached blueprint cannot be used, or undefined when it can. */
  const staleness = () => {
    const verdict = checkBlueprintStamp({ blueprintPath });
    if (verdict.status !== "fresh") return verdict.detail;
    const record = JSON.parse(
      readFileSync(buildRecordPath(blueprintPath), "utf8"),
    );
    if (record.profileDigest !== profileDigest(readProfiles()[profileName]))
      return `${blueprintPath} was built for another ${profileName} profile`;
    return undefined;
  };

  const fresh = () => staleness() === undefined;

  const lock = `${blueprintPath}.lock`;

  /** Publish a lock directory holding this process's PID, or return false. */
  const tryLock = () => {
    const staged = `${lock}.${process.pid}.${randomUUID()}`;
    mkdirSync(staged);
    writeFileSync(resolve(staged, "pid"), `${process.pid}\n`);
    try {
      // rename(2) refuses a non-empty target, so exactly one contender wins,
      // and the lock never exists without its holder's PID.
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

  /**
   * A setup killed mid-build leaves its lock behind. Contenders break it one
   * at a time, re-reading the holder under the guard so a lock another
   * contender has just re-taken is never broken.
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
  const setup = async () => {
    // A stamp alone cannot prove generated constants agree with their producer.
    await generateProfiles("preprod-testing", true);
    if (prebuiltBlueprintsRequired()) {
      const reason = staleness();
      if (reason !== undefined)
        throw new Error(
          `MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS=1 forbids rebuilding the ${directory.replaceAll("-", " ")} blueprint, and the prebuilt one is unusable: ${reason}`,
        );
      return;
    }
    mkdirSync(dirname(blueprintPath), { recursive: true });
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
      const profile = readProfiles()[profileName];
      const sourceHash = blueprintSourceHash(root);
      const recordPath = buildRecordPath(blueprintPath);
      if (existsSync(recordPath)) rmSync(recordPath);
      const result = spawnSync(
        compilerPath,
        [
          "build",
          "--env",
          profileName.replaceAll("-", "_"),
          "--out",
          blueprintPath,
        ],
        {
          cwd: resolve(root, "onchain/aiken"),
          stdio: "inherit",
        },
      );
      if (result.error) throw result.error;
      if (result.status !== 0)
        throw new Error(
          `${profileName} emulator blueprint build failed (${result.status})`,
        );
      writeFileSync(
        recordPath,
        JSON.stringify(
          {
            profile,
            profileDigest: profileDigest(profile),
            blueprintHash: blueprintHash(blueprintPath),
            sourceHash,
            compiler,
          },
          null,
          2,
        ) + "\n",
      );
      if (!fresh())
        throw new Error(
          `${profileName} emulator blueprint changed during its build`,
        );
    } finally {
      discardLock();
    }
  };

  return { profile: profileName, blueprint: blueprintPath, plugin, setup };
};

/**
 * Set where an earlier job built the blueprint (CI's `test-blueprints`), so a
 * stale or mismatched copy fails the run instead of being rebuilt in place.
 */
const prebuiltBlueprintsRequired = () =>
  process.env.MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS === "1";

const running = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return error.code === "EPERM";
  }
};

const interactive = emulatorProfileBlueprint(
  "preprod-emulator-testing",
  "interactive-emulator",
);
export const interactiveEmulatorProfile = interactive.profile;
export const interactiveEmulatorBlueprint = interactive.blueprint;
export const interactiveEmulatorSetup = fileURLToPath(import.meta.url);
export const interactiveEmulatorPlugin = interactive.plugin;

export default interactive.setup;

// CLI: `node demo/midgard-test-support/interactive-emulator.js` builds the
// blueprint, or leaves a fresh one in place, outside Vitest.
if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
)
  await interactive.setup();

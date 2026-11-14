import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

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
import { aikenBinary } from "../../scripts/lib/aiken.mjs";
import { withBuildLock } from "./build-lock.js";

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
    await withBuildLock(
      {
        lock: `${blueprintPath}.lock`,
        ready: fresh,
        timeoutMs: 600_000,
      },
      () => {
        const profile = readProfiles()[profileName];
        const sourceHash = blueprintSourceHash(root);
        const recordPath = buildRecordPath(blueprintPath);
        if (existsSync(recordPath)) rmSync(recordPath);
        const result = spawnSync(
          aikenBinary(),
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
            },
            null,
            2,
          ) + "\n",
        );
        if (!fresh())
          throw new Error(
            `${profileName} emulator blueprint changed during its build`,
          );
      },
    );
  };

  return { profile: profileName, blueprint: blueprintPath, plugin, setup };
};

/**
 * Set where an earlier job built the blueprint (CI's `test-blueprints`), so a
 * stale or mismatched copy fails the run instead of being rebuilt in place.
 */
const prebuiltBlueprintsRequired = () =>
  process.env.MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS === "1";

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

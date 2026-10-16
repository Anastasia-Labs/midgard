import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  cp,
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rm,
  symlink,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { setTimeout as delay } from "node:timers/promises";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = fileURLToPath(new URL("../../", import.meta.url));

const makeFixture = async (t) => {
  const fixture = await mkdtemp(join(tmpdir(), "midgard-interactive-setup-"));
  t.after(() => rm(fixture, { recursive: true, force: true }));
  // A minimal isolated producer tree: the real aiken from demo/flake.lock
  // builds one trivial validator against the generated env modules, without
  // compiling the contracts or changing the working checkout's profile.
  for (const path of [
    "config/deployments",
    "demo/package.json",
    "demo/midgard-test-support/interactive-emulator.js",
    "demo/midgard-fault-proofs/scripts/traced-blueprint.mjs",
    "demo/scripts/deployment-profiles.mjs",
    "demo/scripts/deployment-profile-aiken.mjs",
    "demo/scripts/deployment-profile-identity.mjs",
    "demo/scripts/deployment-profile-budgets.mjs",
    "demo/scripts/lib/blueprint-stamp.mjs",
    "demo/flake.lock",
    "scripts/lib/aiken.mjs",
    "onchain/aiken/aiken.toml",
    "onchain/aiken/aiken.lock",
    "onchain/aiken/lib/midgard/common/types.ak",
  ]) {
    const target = join(fixture, path);
    await mkdir(dirname(target), { recursive: true });
    await cp(join(root, path), target, { recursive: true });
  }
  await symlink(
    join(root, "demo/node_modules"),
    join(fixture, "demo/node_modules"),
    "dir",
  );
  for (const directory of [
    "demo/midgard-core/src",
    "onchain/aiken/env",
    "onchain/aiken/validators",
    "onchain/aiken/build/interactive-emulator",
  ])
    await mkdir(join(fixture, directory), { recursive: true });
  await writeFile(
    join(fixture, "onchain/aiken/validators/always.ak"),
    "validator always {\n  spend(_d: Option<Data>, _r: Data, _o: Data, _t: Data) {\n    True\n  }\n}\n",
  );
  // The checkout's fetched dependencies, when present, save a download.
  await cp(
    join(root, "onchain/aiken/build/packages"),
    join(fixture, "onchain/aiken/build/packages"),
    { recursive: true },
  ).catch((error) => {
    if (error.code !== "ENOENT") throw error;
  });
  const load = (path) => import(pathToFileURL(join(fixture, path)).href);
  const profiles = await load("demo/scripts/deployment-profiles.mjs");
  const stamp = await load("demo/scripts/lib/blueprint-stamp.mjs");
  const setup = await load("demo/midgard-test-support/interactive-emulator.js");
  await profiles.generateProfiles("preprod-testing", false);
  const profile = profiles.readProfiles()[setup.interactiveEmulatorProfile];
  const blueprint = setup.interactiveEmulatorBlueprint;
  const record = async () =>
    writeFile(
      stamp.buildRecordPath(blueprint),
      JSON.stringify({
        profile,
        profileDigest: profiles.profileDigest(profile),
        blueprintHash: stamp.blueprintHash(blueprint),
        sourceHash: stamp.blueprintSourceHash(fixture),
      }),
    );
  return { fixture, profiles, stamp, setup, blueprint, record };
};

/**
 * Set `name` in process.env for the rest of the test (undefined deletes it).
 */
const withEnv = (t, name, value) => {
  const previous = process.env[name];
  t.after(() => {
    if (previous === undefined) delete process.env[name];
    else process.env[name] = previous;
  });
  if (value === undefined) delete process.env[name];
  else process.env[name] = value;
};

/** Whether `path` holds a blueprint the real compiler wrote. */
const isBuilt = async (path) => {
  try {
    return JSON.parse(await readFile(path, "utf8")).validators.length > 0;
  } catch {
    return false;
  }
};

for (const generated of [
  "demo/midgard-core/src/generated-deployment-profiles.ts",
  "onchain/aiken/env/preprod-emulator-testing.ak",
]) {
  test(`interactive setup refuses stale ${generated} despite a fresh cache stamp`, async (t) => {
    const { fixture, stamp, setup, blueprint, record } = await makeFixture(t);
    // Only stamp admission is under test; these bytes are never evaluated.
    await writeFile(blueprint, "cache-admission-test");
    await record();
    await setup.default();
    const target = join(fixture, generated);
    await writeFile(
      target,
      (await readFile(target, "utf8")) + "\n// stale producer output\n",
    );
    await record();
    assert.equal(
      stamp.checkBlueprintStamp({ blueprintPath: blueprint }).status,
      "fresh",
    );
    await assert.rejects(setup.default(), /Stale generated deployment file/u);
    assert.equal(await readFile(blueprint, "utf8"), "cache-admission-test");
  });
}

test(
  "interactive setup breaks a build lock left by a dead holder",
  { timeout: 60_000 },
  async (t) => {
    const { setup, blueprint } = await makeFixture(t);
    const lock = `${blueprint}.lock`;
    // A reaped child's PID names no running process. Without recovery, setup
    // waits out its ten-minute deadline and this test times out.
    const deadPid = spawnSync(process.execPath, ["-e", ""]).pid;
    await mkdir(lock);
    await writeFile(join(lock, "pid"), `${deadPid}\n`);
    await setup.default();
    assert.equal(await isBuilt(blueprint), true);
    assert.deepEqual(
      (await readdir(dirname(blueprint))).filter((name) =>
        name.startsWith("plutus.json.lock"),
      ),
      [],
    );
  },
);

test(
  "interactive setup waits on a build lock whose holder is running",
  { timeout: 60_000 },
  async (t) => {
    const { setup, blueprint, record } = await makeFixture(t);
    const lock = `${blueprint}.lock`;
    await mkdir(lock);
    await writeFile(join(lock, "pid"), `${process.pid}\n`);
    let settled = false;
    const waiting = setup.default().finally(() => {
      settled = true;
    });
    await delay(1_000);
    assert.equal(settled, false);
    // The holder publishing a fresh build is what releases a waiter.
    await writeFile(blueprint, "cache-admission-test");
    await record();
    await waiting;
    assert.equal(await readFile(join(lock, "pid"), "utf8"), `${process.pid}\n`);
  },
);

test("interactive setup rebuilds a stale blueprint by default", async (t) => {
  const { setup, blueprint, record } = await makeFixture(t);
  withEnv(t, "MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS", undefined);
  await writeFile(blueprint, "cache-admission-test");
  await record();
  await writeFile(blueprint, "edited after its record");
  await setup.default();
  assert.equal(await isBuilt(blueprint), true);
  // Fresh now, so a second setup reuses it untouched.
  const built = await readFile(blueprint, "utf8");
  await setup.default();
  assert.equal(await readFile(blueprint, "utf8"), built);
});

for (const [name, corrupt] of [
  ["missing", async ({ blueprint }) => rm(blueprint)],
  [
    "modified after its record",
    async ({ blueprint }) => writeFile(blueprint, "edited after its record"),
  ],
  [
    "built for another profile",
    async ({ stamp, blueprint }) => {
      const path = stamp.buildRecordPath(blueprint);
      const recorded = JSON.parse(await readFile(path, "utf8"));
      await writeFile(
        path,
        JSON.stringify({ ...recorded, profileDigest: "0".repeat(64) }),
      );
    },
  ],
  [
    "built from other sources",
    async ({ fixture }) =>
      writeFile(join(fixture, "onchain/aiken/aiken.toml"), "changed\n"),
  ],
]) {
  test(`required prebuilt interactive blueprint: ${name} fails instead of rebuilding`, async (t) => {
    const context = await makeFixture(t);
    const { setup, blueprint, record } = context;
    await writeFile(blueprint, "cache-admission-test");
    await record();
    withEnv(t, "MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS", "1");
    await setup.default();
    await corrupt(context);
    await assert.rejects(
      setup.default(),
      /MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS=1 forbids rebuilding the interactive emulator blueprint/u,
    );
    assert.equal(await isBuilt(blueprint), false);
  });
}

test("traced blueprint is built, then reused only while its record matches", async (t) => {
  const { fixture } = await makeFixture(t);
  withEnv(t, "MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS", undefined);
  const traced = await import(
    pathToFileURL(
      join(fixture, "demo/midgard-fault-proofs/scripts/traced-blueprint.mjs"),
    ).href
  );
  const path = traced.tracedBlueprint("preprod-testing");
  assert.equal(await isBuilt(path), true);
  const built = await readFile(path, "utf8");
  assert.equal(traced.tracedBlueprint("preprod-testing"), path);
  assert.equal(await readFile(path, "utf8"), built);
  // A blueprint swapped under its record is rebuilt, not trusted.
  await writeFile(path, "swapped");
  traced.tracedBlueprint("preprod-testing");
  assert.equal(await readFile(path, "utf8"), built);
});

for (const [name, corrupt] of [
  ["missing", async (path) => rm(path)],
  ["modified after its record", async (path) => writeFile(path, "swapped")],
  [
    "recorded for another profile",
    async (path) => {
      const record = join(dirname(path), "traced-build.json");
      const recorded = JSON.parse(await readFile(record, "utf8"));
      await writeFile(
        record,
        JSON.stringify({ ...recorded, profile: "preprod-emulator-testing" }),
      );
    },
  ],
  [
    "built from other sources",
    async (path) =>
      writeFile(join(dirname(path), "../../../aiken.toml"), "changed\n"),
  ],
]) {
  test(`required prebuilt traced blueprint: ${name} fails instead of rebuilding`, async (t) => {
    const { fixture } = await makeFixture(t);
    const traced = await import(
      pathToFileURL(
        join(fixture, "demo/midgard-fault-proofs/scripts/traced-blueprint.mjs"),
      ).href
    );
    withEnv(t, "MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS", undefined);
    const path = traced.tracedBlueprint("preprod-testing");
    withEnv(t, "MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS", "1");
    assert.equal(traced.tracedBlueprint("preprod-testing"), path);
    await corrupt(path);
    const corrupted = await readFile(path, "utf8").catch(() => null);
    assert.throws(
      () => traced.tracedBlueprint("preprod-testing"),
      /MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS=1 forbids rebuilding the traced preprod-testing blueprint/u,
    );
    // Not rebuilt: the blueprint is exactly as the corruption left it.
    assert.equal(await readFile(path, "utf8").catch(() => null), corrupted);
  });
}

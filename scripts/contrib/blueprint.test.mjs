import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";

import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
} from "../../demo/scripts/lib/blueprint-stamp.mjs";
import {
  blueprintReadiness,
  ensureBlueprint,
  syncCandidates,
} from "./blueprint.mjs";

const pin = "aiken v1.1.23+5adf783";
const digests = {
  "preprod-testing": "a".repeat(64),
  "local-devnet-testing": "b".repeat(64),
};

// A checkout with everything the stamp and the profile selection read.
const checkout = (t, { validator = "validator probe { }\n" } = {}) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-ensure-blueprint-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const write = (path, contents) => {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), contents);
  };
  for (const workflow of ["aiken-ci.yml", "midgard-node-ci.yml"])
    write(
      `.github/workflows/${workflow}`,
      `env:\n  AIKEN_FORK_VERSION: ${pin}\n`,
    );
  write("onchain/aiken/aiken.toml", 'name = "midgard/selftest"\n');
  write("onchain/aiken/aiken.lock", "# lock\n");
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 1\n");
  write("onchain/aiken/validators/probe.ak", validator);
  write("onchain/aiken/env/default.ak", "pub const network = 0\n");
  write(
    "demo/midgard-core/src/generated-deployment-profiles.ts",
    `export const DEPLOYMENT_PROFILE_DIGESTS = {\n` +
      Object.entries(digests)
        .map(([name, digest]) => `  "${name}": "${digest}",\n`)
        .join("") +
      `} as const;\n` +
      `export const SELECTED_DEPLOYMENT_PROFILE = DEPLOYMENT_PROFILES["preprod-testing"];\n`,
  );
  return root;
};

// What `deployment-profiles.mjs build` leaves: a blueprint and its record.
const build = (root, profile = "preprod-testing", contents = "{}\n") => {
  const blueprint = join(root, "onchain/aiken/plutus.json");
  writeFileSync(blueprint, contents);
  writeFileSync(
    buildRecordPath(blueprint),
    JSON.stringify({
      profile: { name: profile },
      profileDigest: digests[profile],
      blueprintHash: blueprintHash(blueprint),
      sourceHash: blueprintSourceHash(root),
      compiler: pin,
    }),
  );
  return blueprint;
};

const refuseBuild = () => {
  throw new Error("must not build");
};

test("a ready blueprint is left alone", async (t) => {
  const here = checkout(t);
  build(here);
  const result = await ensureBlueprint(here, {
    checkouts: [],
    sync: () => assert.fail("must not copy"),
    build: refuseBuild,
  });
  assert.deepEqual(result, { action: "none" });
});

test("a blueprint for another profile is not ready, though its stamp is fresh", (t) => {
  const here = checkout(t);
  build(here, "local-devnet-testing");
  const readiness = blueprintReadiness(here);
  assert.equal(readiness.ready, false);
  assert.match(readiness.reason, /local-devnet-testing/u);
});

test("a stale blueprint is copied from a checkout whose build matches this tree", async (t) => {
  const here = checkout(t);
  const source = checkout(t);
  build(source);
  const result = await ensureBlueprint(here, {
    checkouts: [source],
    build: refuseBuild,
  });
  assert.equal(result.action, "copied");
  assert.equal(result.from, source);
  assert.equal(blueprintReadiness(here).ready, true);
});

test("candidates exclude other sources, compilers and profiles; the main checkout comes first", (t) => {
  const here = checkout(t);
  const main = checkout(t);
  const older = checkout(t);
  const newer = checkout(t);
  const otherSources = checkout(t, { validator: "validator other { }\n" });
  const otherProfile = checkout(t);
  const otherCompiler = checkout(t);
  build(main);
  utimesSync(buildRecordPath(build(older)), 1, 1);
  build(newer);
  build(otherSources);
  build(otherProfile, "local-devnet-testing");
  const record = buildRecordPath(build(otherCompiler));
  writeFileSync(
    record,
    JSON.stringify({
      ...JSON.parse(readFileSync(record, "utf8")),
      compiler: "aiken v1.1.22",
    }),
  );
  assert.deepEqual(
    syncCandidates(here, [
      main,
      here,
      otherSources,
      older,
      otherProfile,
      otherCompiler,
      newer,
    ]),
    [main, newer, older],
  );
});

test("when no copy is accepted it builds, and reports the refused copy", async (t) => {
  const here = checkout(t);
  const tampered = checkout(t);
  writeFileSync(build(tampered), '{"edited":"after its record"}\n');
  const result = await ensureBlueprint(here, {
    checkouts: [tampered],
    build: async (root, profile) => {
      assert.equal(profile, "preprod-testing");
      build(root);
      return { exitCode: 0, logPath: "/dev/null" };
    },
  });
  assert.equal(result.action, "built");
  assert.equal(result.refused.length, 1);
  assert.match(result.refused[0].detail, /modified after its build record/u);
  assert.equal(blueprintReadiness(here).ready, true);
});

test("a build that fails, or leaves the blueprint unready, is an error", async (t) => {
  for (const outcome of [
    async () => ({ exitCode: 1, logPath: "/tmp/log" }),
    async (root) => {
      build(root, "local-devnet-testing");
      return { exitCode: 0, logPath: "/tmp/log" };
    },
  ]) {
    const here = checkout(t);
    await assert.rejects(
      ensureBlueprint(here, { checkouts: [], build: outcome }),
      /deployment:build preprod-testing did not leave a ready blueprint/u,
    );
  }
});

import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { profileDigest } from "../../demo/scripts/deployment-profiles.mjs";
import { blueprintSourceHash } from "../../demo/scripts/lib/blueprint-stamp.mjs";
import { pinnedAikenFork } from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import { verifyPublicImageInputs } from "./check-image-inputs.mjs";

const source = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const revision = "ab".repeat(20);
const profile = "preprod-public";
const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const fixture = () => {
  const root = mkdtempSync(resolve(tmpdir(), "midgard-public-image-inputs-"));
  const write = (name, value) => {
    const path = resolve(root, name);
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(
      path,
      typeof value === "string" ? value : JSON.stringify(value),
    );
  };
  for (const name of ["aiken-ci.yml", "midgard-node-ci.yml"])
    write(
      `.github/workflows/${name}`,
      readFileSync(resolve(source, `.github/workflows/${name}`), "utf8"),
    );
  for (const directory of ["lib", "validators", "env"])
    write(`onchain/aiken/${directory}/test.ak`, "// isolated verifier control");
  write("onchain/aiken/aiken.lock", "locked");
  write("onchain/aiken/aiken.toml", "project");
  write("demo/pnpm-lock.yaml", "locked");
  const profiles = {
    [profile]: {
      name: profile,
      network: "Preprod",
      da_transport: { retention_days: 120 },
    },
  };
  write(
    "demo/midgard-core/src/generated-deployment-profiles.ts",
    `export const DEPLOYMENT_PROFILES = ${JSON.stringify(profiles)} as const;\nexport const SELECTED_DEPLOYMENT_PROFILE = DEPLOYMENT_PROFILES["${profile}"];\n`,
  );
  const blueprint = {
    validators: [{ title: "known.spend.spend", compiledCode: "00" }],
  };
  write("onchain/aiken/plutus.json", blueprint);
  const stamp = {
    profile: profiles[profile],
    profileDigest: profileDigest(profiles[profile]),
    blueprintHash: sha256(JSON.stringify(blueprint)),
    sourceHash: blueprintSourceHash(root),
    compiler: pinnedAikenFork(root).version,
  };
  write("onchain/aiken/plutus.json.deployment.json", stamp);
  return {
    root,
    write,
    stamp,
    blueprint,
    remove: () => rmSync(root, { recursive: true, force: true }),
  };
};
const withFixture = (run) => {
  const value = fixture();
  try {
    run(value);
  } finally {
    value.remove();
  }
};

test("binds the selected public module, blueprint, frozen source and compiler inputs", () =>
  withFixture(({ root, stamp }) => {
    const result = verifyPublicImageInputs({ root, revision, profile });
    assert.equal(result.sourceRevision, revision);
    assert.equal(result.deploymentProfileDigest, stamp.profileDigest);
    assert.equal(result.blueprintHash, stamp.blueprintHash);
    assert.match(result.pnpmLockHash, /^[0-9a-f]{64}$/u);
  }));
test("refuses moving revision and a testing-profile selection", () =>
  withFixture(({ root }) => {
    for (const args of [
      { root, revision: "main", profile },
      { root, revision, profile: "preprod-testing" },
    ])
      assert.throws(
        () => verifyPublicImageInputs(args),
        /full source revision/,
      );
  }));
test("refuses changed blueprint bytes, source, profile object and compiler stamp", () => {
  for (const mutate of [
    ({ write }) =>
      write("onchain/aiken/plutus.json", {
        validators: [{ title: "known.spend.spend", compiledCode: "01" }],
      }),
    ({ write }) => write("onchain/aiken/lib/test.ak", "// different source"),
    ({ write, stamp }) =>
      write("onchain/aiken/plutus.json.deployment.json", {
        ...stamp,
        profile: { ...stamp.profile, network: "Mainnet" },
      }),
    ({ write, stamp }) =>
      write("onchain/aiken/plutus.json.deployment.json", {
        ...stamp,
        compiler: "stock",
      }),
  ])
    withFixture((value) => {
      mutate(value);
      assert.throws(
        () => verifyPublicImageInputs({ root: value.root, revision, profile }),
        /differs from frozen source/,
      );
    });
});
test("refuses a testing core module beside a public blueprint", () =>
  withFixture(({ root, write }) => {
    write(
      "demo/midgard-core/src/generated-deployment-profiles.ts",
      'export const SELECTED_DEPLOYMENT_PROFILE = DEPLOYMENT_PROFILES["preprod-testing"];',
    );
    assert.throws(
      () => verifyPublicImageInputs({ root, revision, profile }),
      /does not select/,
    );
  }));
test("refuses a stale public profile object in the actual generated module", () =>
  withFixture(({ root, write }) => {
    write(
      "demo/midgard-core/src/generated-deployment-profiles.ts",
      'export const DEPLOYMENT_PROFILES = {"preprod-public":{"name":"preprod-public"}} as const;\nexport const SELECTED_DEPLOYMENT_PROFILE = DEPLOYMENT_PROFILES["preprod-public"];',
    );
    assert.throws(
      () => verifyPublicImageInputs({ root, revision, profile }),
      /generated profile differs/,
    );
  }));
test("refuses zero, duplicate and malformed validator censuses", () => {
  for (const validators of [
    [],
    [{ title: "duplicate" }, { title: "duplicate" }],
    [{ title: 1 }],
    [{ title: "" }],
  ])
    withFixture(({ root, write }) => {
      write("onchain/aiken/plutus.json", { validators });
      assert.throws(
        () => verifyPublicImageInputs({ root, revision, profile }),
        /complete unique validator census/,
      );
    });
});

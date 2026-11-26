import assert from "node:assert/strict";
import {
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import test from "node:test";

import { root, run, temporaryRoot } from "./assets.run.mjs";

test("all shell assets parse", async () => {
  for (const name of [
    "common.sh",
    "generate.sh",
    "up.sh",
    "down.sh",
    "validate-custom-chain-config.sh",
  ]) {
    const result = await run("sh", ["-n", join(root, "scripts", name)]);
    assert.equal(result.status, 0, `${name}: ${result.stderr}`);
  }
});

test("generator refuses an existing run directory before Docker", async () => {
  const existing = mkdtempSync(
    join(temporaryRoot, "midgard-isolated-chain-existing-"),
  );
  const result = await run("sh", [join(root, "scripts/generate.sh")], {
    env: { ...process.env, MIDGARD_ISOLATED_CHAIN_RUN_DIR: existing },
  });
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /refusing to overwrite existing run directory/);
  rmSync(existing, { recursive: true, force: true });
});

/**
 * Runs `generate.sh` in a throwaway checkout up to its first Docker call and
 * returns the compose project and host ports it announced. `gitEntry` makes
 * the checkout a main checkout (`.git` directory) or a linked worktree (`.git`
 * file); the real identity module is copied in, so the derivation under test
 * is the one the repository ships.
 */
const DOCKER_REACHED_STATUS = 97;

const generatedNames = async (gitEntry, env = {}) => {
  const checkout = mkdtempSync(
    join(temporaryRoot, "midgard-isolated-chain-names-"),
  );
  try {
    const scripts = join(
      checkout,
      "demo/midgard-node-tools/devnet/isolated-chain/scripts",
    );
    const binaries = join(checkout, "bin");
    for (const directory of [
      scripts,
      binaries,
      join(checkout, "demo/midgard-node"),
      join(checkout, "scripts/lib"),
    ])
      mkdirSync(directory, { recursive: true });
    for (const name of ["common.sh", "generate.sh"]) {
      copyFileSync(join(root, "scripts", name), join(scripts, name));
    }
    copyFileSync(
      join(root, "../../../../scripts/lib/worktree-identity.mjs"),
      join(checkout, "scripts/lib/worktree-identity.mjs"),
    );
    if (gitEntry === "directory") mkdirSync(join(checkout, ".git"));
    else writeFileSync(join(checkout, ".git"), "gitdir: /elsewhere\n");
    for (const name of ["jq", "sha256sum"]) {
      writeFileSync(join(binaries, name), "#!/bin/sh\nexit 0\n", {
        mode: 0o755,
      });
    }
    writeFileSync(
      join(binaries, "docker"),
      `#!/bin/sh\nexit ${String(DOCKER_REACHED_STATUS)}\n`,
      { mode: 0o755 },
    );
    const result = await run("sh", [join(scripts, "generate.sh")], {
      env: {
        ...process.env,
        PATH: `${binaries}:${process.env.PATH}`,
        MIDGARD_ISOLATED_CHAIN_RUN_DIR: join(checkout, "runs/names"),
        ...env,
      },
    });
    assert.equal(result.status, DOCKER_REACHED_STATUS, result.stderr);
    const announced = result.stderr.match(
      /compose project (\S+), host ports ogmios=(\d+) kupo=(\d+) postgres=(\d+)/,
    );
    assert.ok(announced, result.stderr);
    const [, project, ogmios, kupo, postgres] = announced;
    return { project, ports: [ogmios, kupo, postgres].map(Number) };
  } finally {
    rmSync(checkout, { recursive: true, force: true });
  }
};

test("the main checkout uses the base devnet names and ports", async () => {
  const { project, ports } = await generatedNames("directory");
  assert.equal(project, "midgard_isolated_chain_names");
  assert.deepEqual(ports, [2337, 2442, 5544]);
});

test("a linked worktree derives its own devnet project and ports", async () => {
  const { project, ports } = await generatedNames("file");
  assert.match(project, /^midgard_isolated_chain_[0-9a-f]{8}_names$/);
  const offset = ports[0] - 2337;
  assert.ok(offset >= 10 && offset % 10 === 0, String(offset));
  assert.deepEqual(
    ports,
    [2337, 2442, 5544].map((port) => port + offset),
  );
  const explicit = await generatedNames("file", {
    MIDGARD_ISOLATED_CHAIN_POSTGRES_PORT: "6000",
  });
  assert.equal(explicit.ports[2], 6000);
});

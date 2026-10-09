import "node:assert/strict";
import "node:child_process";
import "node:crypto";
import "node:fs";
import "node:net";
import "node:os";
import "node:path";
import "node:test";
import "node:url";
import "./assets.run.mjs";

import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createServer } from "node:net";
import { basename, dirname, join } from "node:path";
import test from "node:test";

import {
  acceptanceEnvRun,
  bootstrapWithStubs,
  completeSnapshot,
  root,
  run,
  runReset,
  temporaryRoot,
  unbuiltOwnerCheckout,
  writeAcceptanceEnv,
} from "./assets.run.mjs";

test("all shell assets parse", async () => {
  for (const name of [
    "common.sh",
    "generate.sh",
    "bootstrap.sh",
    "fund-wallets.sh",
    "protocol-bootstrap.sh",
    "phas-registration-preflight.sh",
    "write-acceptance-env.sh",
    "native-owner-preflight.sh",
    "capture-snapshot.sh",
    "reset.sh",
    "validate-custom-chain-config.sh",
  ]) {
    const result = await run("sh", ["-n", join(root, "scripts", name)]);
    assert.equal(result.status, 0, `${name}: ${result.stderr}`);
  }
});

test("Kupo JSON health requires a connected exact checkpoint", async () => {
  const common = join(root, "scripts/common.sh");
  const parse = (value) =>
    run("sh", ["-c", '. "$0"; parse_kupo_checkpoint', common], {
      input: JSON.stringify(value),
    });
  const valid = await parse({
    connection_status: "connected",
    most_recent_checkpoint: 6493,
  });
  assert.equal(valid.status, 0);
  assert.equal(valid.stdout, "6493\n");
  for (const value of [
    {},
    { connection_status: "disconnected", most_recent_checkpoint: 6493 },
    ...[null, -1, 1.5, "6493"].map((checkpoint) => ({
      connection_status: "connected",
      most_recent_checkpoint: checkpoint,
    })),
  ]) {
    const invalid = await parse(value);
    assert.notEqual(invalid.status, 0);
    assert.match(invalid.stderr, /connected nonnegative integer checkpoint/);
  }
});

test("protocol bootstrap builds the operator package before running operator commands", async () => {
  const checkout = mkdtempSync(
    join(temporaryRoot, "midgard-bootstrap-routing-"),
  );
  const tools = join(checkout, "demo/midgard-node-tools");
  const operator = join(checkout, "demo/midgard-node");
  const scripts = join(tools, "devnet/phase4-process/scripts");
  const contracts = join(checkout, "onchain/aiken");
  const binaries = join(checkout, "bin");
  const runDir = join(checkout, "run");
  try {
    for (const directory of [
      scripts,
      operator,
      contracts,
      binaries,
      join(runDir, "secrets"),
      join(runDir, "work"),
    ])
      mkdirSync(directory, { recursive: true });
    for (const name of ["common.sh", "protocol-bootstrap.sh"]) {
      copyFileSync(join(root, "scripts", name), join(scripts, name));
    }
    writeFileSync(join(contracts, "plutus.json"), "{}\n");
    writeFileSync(join(runDir, "secrets/node.env"), "");
    writeFileSync(
      join(runDir, "secrets/wallets.env"),
      "TESTNET_GENESIS_WALLET_SEED_PHRASE_A=test-a\nTESTNET_GENESIS_WALLET_SEED_PHRASE_B=test-b\n",
    );
    writeFileSync(
      join(runDir, "run.env"),
      [
        `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
        "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_routing",
        "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_routing",
        "MIDGARD_PHASE4_OGMIOS_PORT=2337",
        "MIDGARD_PHASE4_KUPO_PORT=2442",
        "MIDGARD_PHASE4_POSTGRES_PORT=5544",
        "MIDGARD_PHASE4_POSTGRES_USER=test",
        "MIDGARD_PHASE4_POSTGRES_PASSWORD=test",
        "",
      ].join("\n"),
    );
    for (const name of ["aiken", "jq", "node"]) {
      writeFileSync(join(binaries, name), "#!/bin/sh\nexit 0\n", {
        mode: 0o755,
      });
    }
    // Stop at the first package build, before any database or L1 operation.
    writeFileSync(join(binaries, "pnpm"), "#!/bin/sh\npwd\nexit 73\n", {
      mode: 0o755,
    });
    const result = await run("sh", [join(scripts, "protocol-bootstrap.sh")], {
      env: {
        ...process.env,
        PATH: `${binaries}:${process.env.PATH}`,
        MIDGARD_PHASE4_RUN_DIR: runDir,
      },
    });
    assert.equal(result.status, 73, result.stderr);
    assert.equal(result.stdout.trim(), operator);
  } finally {
    rmSync(checkout, { recursive: true, force: true });
  }
});

test("acceptance env is canonical when node.env lacks run-scoped values", async () => {
  const { runDir, ownerBinary, ownerSha256 } = acceptanceEnvRun(
    (ownerBinary) =>
      // An unnormalized spelling: the child env must carry the resolved
      // absolute path, not node.env's text.
      `POSTGRES_HOST=stale\nL1_PROVIDER=Blockfrost\nMPF_NATIVE_OWNER_BINARY_PATH=${dirname(ownerBinary)}/secrets/../${basename(ownerBinary)}\nMPF_NATIVE_OWNER_SIDECAR_PATH=/shared/owner.sidecar\n`,
  );
  const result = await writeAcceptanceEnv(runDir);
  assert.equal(result.status, 0, result.stderr);
  const output = readFileSync(join(runDir, "secrets/acceptance.env"), "utf8");
  for (const expected of [
    'NETWORK="Custom"',
    'MIDGARD_PHASE4_OGMIOS_PORT="2337"',
    'MIDGARD_PHASE4_KUPO_PORT="2442"',
    'POSTGRES_HOST="127.0.0.1"',
    'POSTGRES_PORT="5544"',
    'POSTGRES_DB="midgard_phase4_process_asset_test"',
    'RUN_GENESIS_ON_STARTUP="false"',
  ]) {
    assert.match(output, new RegExp(`^${expected}$`, "m"));
  }
  assert.match(
    output,
    new RegExp(`^MPF_NATIVE_OWNER_BINARY_PATH="${ownerBinary}"$`, "m"),
  );
  assert.match(
    output,
    new RegExp(`^MPF_NATIVE_OWNER_BINARY_SHA256="${ownerSha256}"$`, "m"),
  );
  assert.doesNotMatch(output, /Blockfrost|POSTGRES_HOST="stale"/);
  assert.doesNotMatch(output, /^L1_PROVIDER=/m);
  assert.doesNotMatch(output, /MPF_NATIVE_OWNER_SIDECAR_PATH/);
  rmSync(runDir, { recursive: true, force: true });
});

test("acceptance env refuses a node.env owner path that does not exist on this host, without a build hint", async () => {
  // The image path .env.example ships: building the checkout cannot fix it.
  const { runDir } = acceptanceEnvRun(
    () => "MPF_NATIVE_OWNER_BINARY_PATH=/app/native/architecture-g-owner\n",
  );
  const result = await writeAcceptanceEnv(runDir);
  assert.notEqual(result.status, 0);
  assert.match(
    result.stderr,
    /node\.env names MPF_NATIVE_OWNER_BINARY_PATH=\/app\/native\/architecture-g-owner, which does not exist on this host; remove it/,
  );
  assert.doesNotMatch(result.stderr, /native:mpf-owner:build/);
  rmSync(runDir, { recursive: true, force: true });
});

test("acceptance env refuses an unbuilt checkout owner with a build command that works from any directory", async () => {
  const { checkout, scripts, operator } = unbuiltOwnerCheckout();
  const { runDir } = acceptanceEnvRun(() => "L1_PROVIDER=Blockfrost\n");
  try {
    const result = await writeAcceptanceEnv(runDir, scripts);
    assert.notEqual(result.status, 0);
    assert.ok(
      result.stderr.includes(
        `native owner binary is missing at ${operator}/native/mpf-event-flat-wasm/target/release/architecture-g-owner; build it with \`pnpm --dir ${operator} run native:mpf-owner:build\``,
      ),
      result.stderr,
    );
    assert.doesNotMatch(result.stderr, /node\.env names/);
  } finally {
    rmSync(checkout, { recursive: true, force: true });
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("acceptance env refuses a node.env pin that does not match the owner binary", async () => {
  const { runDir, ownerSha256 } = acceptanceEnvRun(
    (ownerBinary) =>
      `MPF_NATIVE_OWNER_BINARY_PATH=${ownerBinary}\nMPF_NATIVE_OWNER_BINARY_SHA256=${"0".repeat(64)}\n`,
  );
  const result = await writeAcceptanceEnv(runDir);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, new RegExp(`hashes to ${ownerSha256}`));
  rmSync(runDir, { recursive: true, force: true });
});

test("bootstrap refuses a missing native owner before starting the devnet", async () => {
  const { runDir } = acceptanceEnvRun(
    () => "MPF_NATIVE_OWNER_BINARY_PATH=/app/native/architecture-g-owner\n",
  );
  try {
    const result = await bootstrapWithStubs(runDir);
    assert.notEqual(result.status, 0);
    assert.match(
      result.stderr,
      /node\.env names MPF_NATIVE_OWNER_BINARY_PATH=\/app\/native\/architecture-g-owner, which does not exist on this host/,
    );
    assert.equal(result.called, "");
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("bootstrap refuses a mismatched native owner pin before starting the devnet", async () => {
  const { runDir, ownerSha256 } = acceptanceEnvRun(
    (ownerBinary) =>
      `MPF_NATIVE_OWNER_BINARY_PATH=${ownerBinary}\nMPF_NATIVE_OWNER_BINARY_SHA256=${"0".repeat(64)}\n`,
  );
  try {
    const result = await bootstrapWithStubs(runDir);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, new RegExp(`hashes to ${ownerSha256}`));
    assert.equal(result.called, "");
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("bootstrap starts the devnet once the native owner resolves", async () => {
  const { runDir, ownerBinary } = acceptanceEnvRun(
    (ownerBinary) => `MPF_NATIVE_OWNER_BINARY_PATH=${ownerBinary}\n`,
  );
  try {
    const result = await bootstrapWithStubs(runDir);
    assert.match(
      result.stdout,
      new RegExp(`^nativeOwnerBinary=${ownerBinary}$`, "m"),
    );
    // The first external call after the preflight is `compose up`.
    assert.equal(result.called, "docker\n");
    assert.equal(result.status, 97);
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("generator refuses an existing run directory before Docker", async () => {
  const existing = mkdtempSync(join(temporaryRoot, "midgard-phase4-existing-"));
  const result = await run("sh", [join(root, "scripts/generate.sh")], {
    env: { ...process.env, MIDGARD_PHASE4_RUN_DIR: existing },
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
  const checkout = mkdtempSync(join(temporaryRoot, "midgard-phase4-names-"));
  try {
    const scripts = join(
      checkout,
      "demo/midgard-node-tools/devnet/phase4-process/scripts",
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
        MIDGARD_PHASE4_RUN_DIR: join(checkout, "runs/names"),
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

test("the main checkout keeps the historical devnet names and ports", async () => {
  const { project, ports } = await generatedNames("directory");
  assert.equal(project, "midgard_phase4_process_names");
  assert.deepEqual(ports, [2337, 2442, 5544]);
});

test("a linked worktree derives its own devnet project and ports", async () => {
  const { project, ports } = await generatedNames("file");
  assert.match(project, /^midgard_phase4_process_[0-9a-f]{8}_names$/);
  const offset = ports[0] - 2337;
  assert.ok(offset >= 10 && offset % 10 === 0, String(offset));
  assert.deepEqual(
    ports,
    [2337, 2442, 5544].map((port) => port + offset),
  );
  const explicit = await generatedNames("file", {
    MIDGARD_PHASE4_POSTGRES_PORT: "6000",
  });
  assert.equal(explicit.ports[2], 6000);
});

/**
 * Builds a throwaway checkout that `capture-snapshot.sh` can actually run
 * inside, with `docker` and `curl` replaced by stubs.
 *
 * The Kupo health payload the stub serves is the input under test: the script
 * has to route it through `parse_kupo_checkpoint` and persist it, rather than
 * reading the counter out of the JSON itself. The run is stopped at the PHAS
 * preflight — the first step after checkpoint convergence — by a stub that
 * exits with a distinctive status, so "the script accepted this payload and
 * moved on" and "the script refused this payload" are two different observable
 * outcomes rather than two different exit codes from the same failure.
 */
const PREFLIGHT_REACHED_STATUS = 91;

const runCaptureSnapshot = async ({ kupoPayload, cardanoSlot }) => {
  const checkout = mkdtempSync(join(temporaryRoot, "midgard-phase4-capture-"));
  const scripts = join(
    checkout,
    "demo/midgard-node-tools/devnet/phase4-process/scripts",
  );
  const runDir = join(checkout, "run");
  const binaries = join(checkout, "bin");
  const socketDir = join(runDir, "cardano/ipc");
  let socketServer;
  try {
    for (const directory of [
      scripts,
      binaries,
      socketDir,
      join(checkout, "demo/midgard-node-tools/src"),
      join(checkout, "demo/midgard-node-tools/dist"),
      join(checkout, "demo/midgard-node/src"),
      join(checkout, "demo/midgard-node/dist"),
      join(runDir, "secrets"),
      join(runDir, "work"),
      join(runDir, "genesis"),
      join(runDir, "config"),
      join(runDir, "deploymentInfo"),
    ])
      mkdirSync(directory, { recursive: true });
    for (const name of ["common.sh", "capture-snapshot.sh"]) {
      copyFileSync(join(root, "scripts", name), join(scripts, name));
    }
    writeFileSync(
      join(scripts, "phas-registration-preflight.sh"),
      `#!/bin/sh\nexit ${PREFLIGHT_REACHED_STATUS}\n`,
      { mode: 0o755 },
    );
    writeFileSync(
      join(
        checkout,
        "demo/midgard-node-tools/devnet/phase4-process/compose.yaml",
      ),
      "services: {}\n",
    );
    for (const [path, contents] of [
      ["demo/midgard-node-tools/src/marker", "tools-src\n"],
      ["demo/midgard-node-tools/dist/marker", "tools-dist\n"],
      ["demo/midgard-node/src/marker", "node-src\n"],
      ["demo/midgard-node/dist/marker", "node-dist\n"],
    ])
      writeFileSync(join(checkout, path), contents);
    writeFileSync(join(runDir, "genesis/marker"), "genesis\n");
    writeFileSync(join(runDir, "config/marker"), "config\n");
    writeFileSync(
      join(runDir, "deploymentInfo/contract-deployment-info.json"),
      "{}\n",
    );
    writeFileSync(
      join(runDir, "deploymentInfo/phas-registration-transaction-body.json"),
      "{}\n",
    );
    writeFileSync(
      join(runDir, "work/plutus.json.sha256"),
      `${"a".repeat(64)}  plutus.json\n`,
    );
    writeFileSync(join(runDir, "secrets/acceptance.env"), 'NETWORK="Custom"\n');
    writeFileSync(
      join(runDir, "run.env"),
      [
        `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
        "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_capture",
        "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_capture",
        "MIDGARD_PHASE4_NETWORK_MAGIC=424242",
        "MIDGARD_PHASE4_OGMIOS_PORT=2337",
        "MIDGARD_PHASE4_KUPO_PORT=2442",
        "MIDGARD_PHASE4_POSTGRES_PORT=5544",
        "MIDGARD_PHASE4_POSTGRES_USER=phase4",
        "MIDGARD_PHASE4_POSTGRES_PASSWORD=test_only",
        "",
      ].join("\n"),
    );
    // `grant_cardano_socket_access` waits for a real socket inode before it
    // will let the capture continue.
    socketServer = createServer();
    await new Promise((resolve, reject) => {
      socketServer.once("error", reject);
      socketServer.listen(join(socketDir, "node.socket"), resolve);
    });
    // `docker` answers only the tip query; every compose call is a no-op.
    writeFileSync(
      join(binaries, "docker"),
      [
        "#!/bin/sh",
        'for arg in "$@"; do',
        '  if [ "$arg" = "tip" ]; then',
        `    printf '{"slot":${cardanoSlot},"hash":"%s"}\\n' "${"cd".repeat(32)}"`,
        "    exit 0",
        "  fi",
        "done",
        "exit 0",
      ].join("\n"),
      { mode: 0o755 },
    );
    writeFileSync(
      join(binaries, "curl"),
      `#!/bin/sh\ncat "$MIDGARD_TEST_KUPO_PAYLOAD_FILE"\n`,
      { mode: 0o755 },
    );
    const payloadFile = join(checkout, "kupo-health.payload");
    writeFileSync(payloadFile, kupoPayload);
    const result = await run("sh", [join(scripts, "capture-snapshot.sh")], {
      env: {
        ...process.env,
        PATH: `${binaries}:${process.env.PATH}`,
        MIDGARD_PHASE4_RUN_DIR: runDir,
        MIDGARD_TEST_KUPO_PAYLOAD_FILE: payloadFile,
      },
      timeoutMs: 20_000,
    });
    const snapshotDir = join(runDir, "snapshots/matched-v1");
    const readIfPresent = (name) => {
      try {
        return readFileSync(join(snapshotDir, name), "utf8");
      } catch {
        return null;
      }
    };
    return {
      ...result,
      kupoHealth: readIfPresent("kupo-health.json"),
      identity: readIfPresent("snapshot-identity.json"),
    };
  } finally {
    if (socketServer !== undefined) {
      await new Promise((resolve) => socketServer.close(resolve));
    }
    rmSync(checkout, { recursive: true, force: true });
  }
};

test("snapshot capture reads the Kupo checkpoint through the strict parser", async () => {
  const payload = JSON.stringify({
    connection_status: "connected",
    most_recent_checkpoint: 6493,
  });
  const accepted = await runCaptureSnapshot({
    kupoPayload: payload,
    cardanoSlot: 6493,
  });
  // Convergence was reached, so the capture proceeded to the PHAS preflight.
  assert.equal(accepted.status, PREFLIGHT_REACHED_STATUS, accepted.stderr);
  // The served payload is persisted verbatim as the snapshot's own evidence of
  // the checkpoint it froze; a capture that read the counter some other way
  // would leave nothing to check the frozen identity against.
  assert.equal(accepted.kupoHealth, payload);
});

test("snapshot capture refuses a Kupo payload the strict parser rejects", async () => {
  const rejected = await runCaptureSnapshot({
    kupoPayload: JSON.stringify({
      connection_status: "disconnected",
      most_recent_checkpoint: 6493,
    }),
    cardanoSlot: 6493,
  });
  assert.notEqual(rejected.status, 0);
  assert.notEqual(rejected.status, PREFLIGHT_REACHED_STATUS);
  assert.match(rejected.stderr, /connected nonnegative integer checkpoint/);
  // Nothing downstream of the refusal ran: no frozen identity was written.
  assert.equal(rejected.identity, null);
});

test("reset refuses an incomplete or tampered matched snapshot before touching state", async () => {
  const missingSums = await runReset((snapshotDir) => {
    completeSnapshot(snapshotDir);
  });
  assert.notEqual(missingSums.status, 0);
  assert.match(missingSums.stderr, /matched snapshot is incomplete/);

  const missingIdentity = await runReset((snapshotDir) => {
    completeSnapshot(snapshotDir);
    rmSync(join(snapshotDir, "snapshot-identity.json"));
    writeFileSync(join(snapshotDir, "SHA256SUMS"), "");
  });
  assert.notEqual(missingIdentity.status, 0);
  assert.match(missingIdentity.stderr, /matched snapshot identity is missing/);

  // Every listed file is intact and its digests agree, but the digest *of the
  // digest list* does not — the guard that catches a wholesale swap of the
  // manifest.
  const tamperedSet = await runReset((snapshotDir) => {
    completeSnapshot(snapshotDir);
    const names = [
      "config.tar.gz",
      "genesis.tar.gz",
      "acceptance.env",
      "phas-registration-proof.json",
      "phas-registration-transaction-body.json",
      "snapshot-identity.json",
    ];
    writeFileSync(
      join(snapshotDir, "SHA256SUMS"),
      names
        .map(
          (name) =>
            `${createHash("sha256").update(`${name}\n`).digest("hex")}  ${name}\n`,
        )
        .join(""),
    );
    writeFileSync(
      join(snapshotDir, "SNAPSHOT_SET_SHA256"),
      `${"0".repeat(64)}\n`,
    );
  });
  assert.notEqual(tamperedSet.status, 0);
  assert.match(tamperedSet.stderr, /snapshot-set checksum mismatch/);
});

test("reset refuses a caller-redirected snapshot directory", async () => {
  const redirected = await runReset(
    (snapshotDir) => {
      completeSnapshot(snapshotDir);
      writeFileSync(join(snapshotDir, "SHA256SUMS"), "");
    },
    { MIDGARD_PHASE4_SNAPSHOT_DIR: "/tmp/not-the-run-snapshot" },
  );
  assert.notEqual(redirected.status, 0);
  assert.match(
    redirected.stderr,
    /snapshot override is not authorized for the run-scoped matched snapshot/,
  );
});

test("reset requires an explicit scenario label", async () => {
  const runDir = mkdtempSync(
    join(temporaryRoot, "midgard-phase4-reset-label-"),
  );
  try {
    writeFileSync(
      join(runDir, "run.env"),
      [
        `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
        "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_reset",
        "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_reset",
        "",
      ].join("\n"),
    );
    const result = await run("sh", [join(root, "scripts/reset.sh")], {
      env: { ...process.env, MIDGARD_PHASE4_RUN_DIR: runDir },
    });
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /MIDGARD_PHASE4_SCENARIO_LABEL is required/);
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

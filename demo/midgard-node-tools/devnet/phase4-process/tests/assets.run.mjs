import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import {
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const root = dirname(dirname(fileURLToPath(import.meta.url)));

export const temporaryRoot = process.platform === "win32" ? tmpdir() : "/tmp";

export const run = (
  command,
  args,
  { env = process.env, input, timeoutMs = 5_000 } = {},
) =>
  new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      env,
      stdio: ["pipe", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    let timedOut = false;
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => {
      stdout += chunk;
    });
    child.stderr.on("data", (chunk) => {
      stderr += chunk;
    });
    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill("SIGKILL");
    }, timeoutMs);
    child.on("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });
    child.on("close", (status, signal) => {
      clearTimeout(timeout);
      if (timedOut) {
        reject(
          new Error(
            `${command} timed out after ${timeoutMs}ms\nstdout:\n${stdout}\nstderr:\n${stderr}`,
          ),
        );
        return;
      }
      resolve({ status, signal, stdout, stderr });
    });
    if (input === undefined) child.stdin.end();
    else child.stdin.end(input);
  });

export const NONCE_TX_HASH = "ab".repeat(32);
export const L1_ORIGIN = `1234.${"cd".repeat(32)}`;

/** What `midgard-l1-follower find-origin` prints for NONCE_TX_HASH. */
export const FIND_ORIGIN_OUTPUT = {
  l1Origin: L1_ORIGIN,
  origin: { slot: 1234, blockHash: "cd".repeat(32) },
  prepareHubOracleNonceBlock: {
    slot: 1240,
    blockHash: "ef".repeat(32),
    height: 7,
  },
  txIndex: 0,
  depth: 1,
};

/**
 * The node L1 follower inputs protocol-bootstrap.sh leaves in a run: the
 * transport binary, the host node config, the origin record for the nonce,
 * and the node.env lines naming them.
 */
export const followerInputFixture = (runDir) => {
  for (const directory of ["bin", "config", "work"])
    mkdirSync(join(runDir, directory), { recursive: true });
  writeFileSync(
    join(runDir, "bin/midgard-l1-node-transport"),
    "stand-in transport\n",
    { mode: 0o755 },
  );
  writeFileSync(join(runDir, "config/host-config.json"), "{}\n");
  writeFileSync(
    join(runDir, "work/l1-origin.json"),
    `${JSON.stringify({ l1Origin: L1_ORIGIN, nonceTxHash: NONCE_TX_HASH })}\n`,
  );
  return {
    L1_ORIGIN,
    L1_NODE_SOCKET_PATH: join(runDir, "cardano/ipc/node.socket"),
    L1_NODE_CONFIG_PATH: join(runDir, "config/host-config.json"),
    L1_NODE_TRANSPORT_BINARY_PATH: join(
      runDir,
      "bin/midgard-l1-node-transport",
    ),
    HUB_ORACLE_ONE_SHOT_TX_HASH: NONCE_TX_HASH,
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
  };
};

/**
 * A run directory that write-acceptance-env.sh accepts, with `nodeEnv` as the
 * operator's private node.env (after the follower inputs bootstrap writes,
 * unless `follower` is false) and a stand-in owner binary whose hash the
 * script must pin.
 */
export const acceptanceEnvRun = (nodeEnv, { follower = true } = {}) => {
  // Symlink-free, as the node requires of its local-node paths.
  const runDir = realpathSync(
    mkdtempSync(join(temporaryRoot, "midgard-phase4-acceptance-env-")),
  );
  mkdirSync(join(runDir, "secrets"), { recursive: true });
  mkdirSync(join(runDir, "deploymentInfo"), { recursive: true });
  const followerLines = follower
    ? Object.entries(followerInputFixture(runDir))
        .map(([key, value]) => `${key}=${value}\n`)
        .join("")
    : "";
  const ownerBinary = join(runDir, "architecture-g-owner");
  writeFileSync(ownerBinary, "stand-in owner binary\n");
  writeFileSync(
    join(runDir, "run.env"),
    [
      "MIDGARD_PHASE4_RUN_ID=asset_test",
      `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
      "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_asset_test",
      "MIDGARD_PHASE4_NETWORK_MAGIC=424242",
      "MIDGARD_PHASE4_OGMIOS_PORT=2337",
      "MIDGARD_PHASE4_KUPO_PORT=2442",
      "MIDGARD_PHASE4_POSTGRES_PORT=5544",
      "MIDGARD_PHASE4_POSTGRES_USER=phase4",
      "MIDGARD_PHASE4_POSTGRES_PASSWORD=test_only",
      "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_asset_test",
      "",
    ].join("\n"),
  );
  writeFileSync(
    join(runDir, "secrets/node.env"),
    `${followerLines}${nodeEnv(ownerBinary)}`,
  );
  writeFileSync(
    join(runDir, "secrets/wallets.env"),
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A=test-a\nTESTNET_GENESIS_WALLET_SEED_PHRASE_B=test-b\n",
  );
  writeFileSync(
    join(runDir, "deploymentInfo/contract-deployment-info.json"),
    "{}\n",
  );
  const ownerSha256 = createHash("sha256")
    .update(readFileSync(ownerBinary))
    .digest("hex");
  return { runDir, ownerBinary, ownerSha256 };
};

export const writeAcceptanceEnv = (runDir, scripts = join(root, "scripts")) =>
  run("sh", [join(scripts, "write-acceptance-env.sh")], {
    env: { ...process.env, MIDGARD_PHASE4_RUN_DIR: runDir },
  });

/**
 * A checkout whose operator package has never built the native owner: the
 * phase-4 scripts, a blueprint, and the real operator node_modules (the script
 * reads node.env with dotenv), but no native build output.
 */
export const unbuiltOwnerCheckout = () => {
  const checkout = mkdtempSync(join(temporaryRoot, "midgard-phase4-unbuilt-"));
  const scripts = join(
    checkout,
    "demo/midgard-node-tools/devnet/phase4-process/scripts",
  );
  const operator = join(checkout, "demo/midgard-node");
  for (const directory of [scripts, operator, join(checkout, "onchain/aiken")])
    mkdirSync(directory, { recursive: true });
  for (const name of [
    "common.sh",
    "write-acceptance-env.sh",
    "native-owner.mjs",
    "l1-follower-inputs.mjs",
  ])
    copyFileSync(join(root, "scripts", name), join(scripts, name));
  symlinkSync(
    join(root, "../../../midgard-node/node_modules"),
    join(operator, "node_modules"),
  );
  writeFileSync(join(checkout, "onchain/aiken/plutus.json"), "{}\n");
  return { checkout, scripts, operator };
};

/**
 * Runs bootstrap.sh with docker, curl and jq replaced by stubs that record
 * their call and fail, so the test sees whether bootstrap reached the devnet.
 */
export const bootstrapWithStubs = async (runDir) => {
  const binaries = mkdtempSync(join(temporaryRoot, "midgard-phase4-stubs-"));
  const calls = join(binaries, "calls");
  try {
    for (const name of ["docker", "curl", "jq"])
      writeFileSync(
        join(binaries, name),
        `#!/bin/sh\necho ${name} >> "${calls}"\nexit 97\n`,
        { mode: 0o755 },
      );
    const result = await run("sh", [join(root, "scripts", "bootstrap.sh")], {
      env: {
        ...process.env,
        PATH: `${binaries}:${process.env.PATH}`,
        MIDGARD_PHASE4_RUN_DIR: runDir,
      },
    });
    let called = "";
    try {
      called = readFileSync(calls, "utf8");
    } catch {
      called = "";
    }
    return { ...result, called };
  } finally {
    rmSync(binaries, { recursive: true, force: true });
  }
};

/**
 * `reset.sh` restores durable state, so every guard that can refuse must do so
 * before it touches anything. These run the real script against a fixture run
 * directory and stop at exactly those guards — no Docker involved, because the
 * refusals precede the first container.
 */
export const runReset = async (prepare, extraEnv = {}) => {
  const runDir = mkdtempSync(join(temporaryRoot, "midgard-phase4-reset-"));
  try {
    mkdirSync(join(runDir, "snapshots/matched-v1"), { recursive: true });
    mkdirSync(join(runDir, "work"), { recursive: true });
    writeFileSync(
      join(runDir, "run.env"),
      [
        `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
        "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_reset",
        "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_reset",
        "MIDGARD_PHASE4_NETWORK_MAGIC=424242",
        "MIDGARD_PHASE4_OGMIOS_PORT=2337",
        "MIDGARD_PHASE4_KUPO_PORT=2442",
        "MIDGARD_PHASE4_POSTGRES_PORT=5544",
        "MIDGARD_PHASE4_POSTGRES_USER=phase4",
        "MIDGARD_PHASE4_POSTGRES_PASSWORD=test_only",
        "",
      ].join("\n"),
    );
    prepare(join(runDir, "snapshots/matched-v1"), runDir);
    return await run("sh", [join(root, "scripts/reset.sh")], {
      env: {
        ...process.env,
        MIDGARD_PHASE4_RUN_DIR: runDir,
        MIDGARD_PHASE4_SCENARIO_LABEL: "asset_test",
        ...extraEnv,
      },
    });
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
};

export const completeSnapshot = (snapshotDir) => {
  for (const name of [
    "config.tar.gz",
    "genesis.tar.gz",
    "acceptance.env",
    "phas-registration-proof.json",
    "phas-registration-transaction-body.json",
    "snapshot-identity.json",
    "SNAPSHOT_IDENTITY_SHA256",
  ])
    writeFileSync(join(snapshotDir, name), `${name}\n`);
};

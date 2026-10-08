import assert from "node:assert/strict";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import test from "node:test";

import {
  FIND_ORIGIN_OUTPUT,
  L1_ORIGIN,
  NONCE_TX_HASH,
  root,
  run,
  temporaryRoot,
} from "./assets.run.mjs";

// protocol-bootstrap.sh's L1 follower steps: building the transport, writing
// the host node config and deriving L1_ORIGIN from the hub-oracle nonce.
// l1-follower-inputs.test.mjs covers the helper and the acceptance writer.

const scratch = (label) =>
  mkdtempSync(join(temporaryRoot, `midgard-phase4-follower-${label}-`));

/**
 * Runs protocol-bootstrap.sh in a throwaway checkout whose node, pnpm, aiken
 * and jq are stubs: the operator CLI succeeds through the nonce, the follower
 * CLI's find-origin answers with `findOrigin`, and the first command after the
 * follower inputs (deploy-reference-script-node-runtime) stops the script
 * with status 74. The helper module itself runs on the real node.
 */
const STOPPED_AFTER_ORIGIN = 74;

const bootstrapUntilReferenceScripts = async (findOrigin) => {
  const checkout = scratch("bootstrap");
  const tools = join(checkout, "demo/midgard-node-tools");
  const operator = join(checkout, "demo/midgard-node");
  const scripts = join(tools, "devnet/phase4-process/scripts");
  const transport = join(checkout, "demo/l1-node-transport");
  const follower = join(checkout, "demo/midgard-l1-follower");
  const binaries = join(checkout, "stubs");
  const runDir = join(checkout, "run");
  const calls = join(checkout, "calls");
  try {
    for (const directory of [
      scripts,
      operator,
      join(transport, "dist/native"),
      join(follower, "dist"),
      join(checkout, "onchain/aiken"),
      binaries,
      join(runDir, "secrets"),
      join(runDir, "work"),
      join(runDir, "config"),
      join(runDir, "deploymentInfo"),
    ])
      mkdirSync(directory, { recursive: true });
    for (const name of [
      "common.sh",
      "protocol-bootstrap.sh",
      "l1-follower-inputs.mjs",
    ])
      copyFileSync(join(root, "scripts", name), join(scripts, name));
    writeFileSync(join(checkout, "onchain/aiken/plutus.json"), "{}\n");
    writeFileSync(
      join(transport, "dist/native/midgard-l1-node-transport"),
      "built transport\n",
      { mode: 0o755 },
    );
    writeFileSync(join(follower, "dist/cli.js"), "");
    writeFileSync(
      join(runDir, "config/config.json"),
      JSON.stringify({ ShelleyGenesisFile: "/genesis/shelley-genesis.json" }),
    );
    // An earlier run's origin, which this run must not keep.
    writeFileSync(
      join(runDir, "secrets/node.env"),
      `L1_ORIGIN=9.${"00".repeat(32)}\n`,
    );
    writeFileSync(
      join(runDir, "secrets/wallets.env"),
      "TESTNET_GENESIS_WALLET_SEED_PHRASE_A=test-a\nTESTNET_GENESIS_WALLET_SEED_PHRASE_B=test-b\n",
    );
    writeFileSync(
      join(runDir, "run.env"),
      [
        `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
        "MIDGARD_PHASE4_RUN_ID=follower",
        "MIDGARD_PHASE4_COMPOSE_PROJECT=midgard_phase4_process_follower",
        "MIDGARD_PHASE4_NETWORK_MAGIC=424242",
        "MIDGARD_PHASE4_OGMIOS_PORT=2337",
        "MIDGARD_PHASE4_KUPO_PORT=2442",
        "MIDGARD_PHASE4_POSTGRES_PORT=5544",
        "MIDGARD_PHASE4_POSTGRES_USER=test",
        "MIDGARD_PHASE4_POSTGRES_PASSWORD=test",
        "MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_follower",
        "",
      ].join("\n"),
    );
    const stub = (name, body) =>
      writeFileSync(join(binaries, name), `#!/bin/sh\n${body}\n`, {
        mode: 0o755,
      });
    stub("aiken", "exit 0");
    stub("pnpm", `echo "pnpm $*" >>"${calls}"`);
    // The script parses the nonce transcript with jq; answer by filter.
    stub(
      "jq",
      `case "$2" in *txHash*) echo ${NONCE_TX_HASH} ;; *outputIndex*) echo 0 ;; *) exit 1 ;; esac`,
    );
    stub(
      "node",
      [
        'case "$1" in',
        "  dist/index.js)",
        `    echo "operator $2" >>"${calls}"`,
        '    case "$2" in',
        `      prepare-hub-oracle-one-shot-nonce) echo '{"txHash":"${NONCE_TX_HASH}","outputIndex":0}' ;;`,
        `      deploy-reference-script-node-runtime) exit ${String(STOPPED_AFTER_ORIGIN)} ;;`,
        "    esac",
        "    exit 0 ;;",
        "  */midgard-node-tools/dist/index.js) exit 0 ;;",
        "  */midgard-l1-follower/dist/cli.js)",
        "    shift",
        `    echo "follower $*" >>"${calls}"`,
        `    ${findOrigin}`,
        "    ;;",
        "esac",
        `exec "${process.execPath}" "$@"`,
      ].join("\n"),
    );
    const result = await run("sh", [join(scripts, "protocol-bootstrap.sh")], {
      env: {
        ...process.env,
        PATH: `${binaries}:${process.env.PATH}`,
        MIDGARD_PHASE4_RUN_DIR: runDir,
      },
      timeoutMs: 20_000,
    });
    return {
      ...result,
      runDir,
      calls: existsSync(calls) ? readFileSync(calls, "utf8") : "",
      nodeEnv: readFileSync(join(runDir, "secrets/node.env"), "utf8"),
      transportRoot: transport,
      transport: join(transport, "dist/native/midgard-l1-node-transport"),
    };
  } catch (error) {
    rmSync(checkout, { recursive: true, force: true });
    throw error;
  }
};

const cleanUp = (runDir) =>
  rmSync(join(runDir, ".."), { recursive: true, force: true });

test("protocol bootstrap derives L1_ORIGIN from the nonce and writes the follower inputs", async () => {
  const result = await bootstrapUntilReferenceScripts(
    `echo '${JSON.stringify(FIND_ORIGIN_OUTPUT)}'; exit 0`,
  );
  try {
    assert.equal(result.status, STOPPED_AFTER_ORIGIN, result.stderr);
    const { runDir } = result;
    assert.ok(
      result.calls.includes(
        `follower find-origin --tx ${NONCE_TX_HASH} --network-magic 424242 --socket ${runDir}/cardano/ipc/node.socket --sidecar ${runDir}/bin/midgard-l1-node-transport\n`,
      ),
      result.calls,
    );
    // The transport is built before the nonce spends anything on L1.
    const build = result.calls.indexOf(
      `pnpm --dir ${result.transportRoot} run native:build\n`,
    );
    assert.ok(build >= 0, result.calls);
    assert.ok(
      build <
        result.calls.indexOf("operator prepare-hub-oracle-one-shot-nonce"),
      result.calls,
    );
    for (const line of [
      `L1_ORIGIN=${L1_ORIGIN}`,
      `L1_NODE_SOCKET_PATH=${runDir}/cardano/ipc/node.socket`,
      `L1_NODE_CONFIG_PATH=${runDir}/config/host-config.json`,
      `L1_NATIVE_CHAIN_SYNC_BINARY_PATH=${runDir}/bin/midgard-l1-node-transport`,
      `HUB_ORACLE_ONE_SHOT_TX_HASH=${NONCE_TX_HASH}`,
    ])
      assert.match(result.nodeEnv, new RegExp(`^${line}$`, "m"));
    assert.equal(
      readFileSync(join(runDir, "bin/midgard-l1-node-transport"), "utf8"),
      "built transport\n",
    );
    assert.equal(
      JSON.parse(readFileSync(join(runDir, "config/host-config.json"), "utf8"))
        .ShelleyGenesisFile,
      join(runDir, "genesis/shelley-genesis.json"),
    );
    assert.equal(
      JSON.parse(readFileSync(join(runDir, "work/l1-origin.json"), "utf8"))
        .nonceTxHash,
      NONCE_TX_HASH,
    );
  } finally {
    cleanUp(result.runDir);
  }
});

test("protocol bootstrap stops, with no origin, when find-origin does not find the nonce", async () => {
  const result = await bootstrapUntilReferenceScripts(
    "echo 'the tx is not on the node chain' >&2; exit 3",
  );
  try {
    assert.notEqual(result.status, 0);
    assert.notEqual(result.status, STOPPED_AFTER_ORIGIN);
    assert.match(
      result.stderr,
      new RegExp(
        `cannot derive L1_ORIGIN from the hub-oracle nonce tx ${NONCE_TX_HASH}`,
      ),
    );
    assert.doesNotMatch(result.calls, /deploy-reference-script-node-runtime/);
    // The earlier run's origin is cleared, so the acceptance writer refuses.
    assert.match(result.nodeEnv, /^L1_ORIGIN=$/m);
    assert.doesNotMatch(result.nodeEnv, /L1_NODE_SOCKET_PATH/);
    assert.equal(existsSync(join(result.runDir, "work/l1-origin.json")), false);
  } finally {
    cleanUp(result.runDir);
  }
});

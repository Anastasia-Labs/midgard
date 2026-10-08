import assert from "node:assert/strict";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import test from "node:test";

import {
  installTransportBinary,
  recordL1Origin,
  writeHostCardanoConfig,
} from "../scripts/l1-follower-inputs.mjs";
import {
  acceptanceEnvRun,
  FIND_ORIGIN_OUTPUT,
  L1_ORIGIN,
  NONCE_TX_HASH,
  temporaryRoot,
  writeAcceptanceEnv,
} from "./assets.run.mjs";

const scratch = (label) =>
  mkdtempSync(join(temporaryRoot, `midgard-phase4-follower-${label}-`));

// The stand-in owner acceptanceEnvRun hashes; the owner pin is not under test.
const withOwner = (ownerBinary) =>
  `MPF_NATIVE_OWNER_BINARY_PATH=${ownerBinary}\n`;

test("the host node config names the run's genesis files", () => {
  const runDir = scratch("config");
  try {
    mkdirSync(join(runDir, "config"));
    writeFileSync(
      join(runDir, "config/config.json"),
      JSON.stringify({
        ShelleyGenesisFile: "/genesis/shelley-genesis.json",
        ShelleyGenesisHash: "00",
        Protocol: "Cardano",
      }),
    );
    const path = writeHostCardanoConfig(runDir);
    assert.equal(path, join(runDir, "config/host-config.json"));
    assert.deepEqual(JSON.parse(readFileSync(path, "utf8")), {
      ShelleyGenesisFile: join(runDir, "genesis/shelley-genesis.json"),
      ShelleyGenesisHash: "00",
      Protocol: "Cardano",
    });
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("the host node config refuses a run without a node config", () => {
  const runDir = scratch("no-config");
  try {
    assert.throws(() => writeHostCardanoConfig(runDir), {
      name: "Phase4L1FollowerInputError",
      message: /node config .*config\/config\.json/,
    });
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("the transport binary is copied into the run, executable", () => {
  const runDir = scratch("transport");
  try {
    const source = join(runDir, "built-transport");
    writeFileSync(source, "transport bytes\n", { mode: 0o644 });
    const installed = installTransportBinary(runDir, source);
    assert.equal(installed, join(runDir, "bin/midgard-l1-node-transport"));
    assert.equal(readFileSync(installed, "utf8"), "transport bytes\n");
    assert.equal(statSync(installed).mode & 0o777, 0o755);
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("an unbuilt transport binary is refused with its build command", () => {
  const runDir = scratch("no-transport");
  try {
    assert.throws(
      () => installTransportBinary(runDir, join(runDir, "missing")),
      {
        name: "Phase4L1FollowerInputError",
        message: /native:build/,
      },
    );
    assert.equal(
      existsSync(join(runDir, "bin/midgard-l1-node-transport")),
      false,
    );
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("find-origin output is recorded against the lowercase nonce tx", () => {
  const runDir = scratch("record");
  try {
    mkdirSync(join(runDir, "work"));
    assert.equal(
      recordL1Origin(
        runDir,
        NONCE_TX_HASH.toUpperCase(),
        JSON.stringify(FIND_ORIGIN_OUTPUT),
      ),
      L1_ORIGIN,
    );
    assert.deepEqual(
      JSON.parse(readFileSync(join(runDir, "work/l1-origin.json"), "utf8")),
      {
        l1Origin: L1_ORIGIN,
        nonceTxHash: NONCE_TX_HASH,
        prepareHubOracleNonceBlock:
          FIND_ORIGIN_OUTPUT.prepareHubOracleNonceBlock,
        recordedAt: JSON.parse(
          readFileSync(join(runDir, "work/l1-origin.json"), "utf8"),
        ).recordedAt,
      },
    );
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("find-origin output without an origin is refused", () => {
  const runDir = scratch("no-origin");
  try {
    mkdirSync(join(runDir, "work"));
    for (const [txHash, stdout] of [
      [NONCE_TX_HASH, ""],
      [NONCE_TX_HASH, "{}"],
      [
        NONCE_TX_HASH,
        JSON.stringify({ ...FIND_ORIGIN_OUTPUT, l1Origin: "1234" }),
      ],
      ["ab", JSON.stringify(FIND_ORIGIN_OUTPUT)],
    ])
      assert.throws(() => recordL1Origin(runDir, txHash, stdout), {
        name: "Phase4L1FollowerInputError",
      });
    assert.equal(existsSync(join(runDir, "work/l1-origin.json")), false);
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

test("acceptance env carries the node's L1 follower inputs", async () => {
  const { runDir } = acceptanceEnvRun(withOwner);
  try {
    const result = await writeAcceptanceEnv(runDir);
    assert.equal(result.status, 0, result.stderr);
    const output = readFileSync(join(runDir, "secrets/acceptance.env"), "utf8");
    for (const [key, value] of Object.entries({
      L1_ORIGIN,
      L1_NODE_SOCKET_PATH: join(runDir, "cardano/ipc/node.socket"),
      L1_NODE_CONFIG_PATH: join(runDir, "config/host-config.json"),
      L1_NATIVE_CHAIN_SYNC_BINARY_PATH: join(
        runDir,
        "bin/midgard-l1-node-transport",
      ),
      HUB_ORACLE_ONE_SHOT_TX_HASH: NONCE_TX_HASH,
      HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
    }))
      assert.match(output, new RegExp(`^${key}="${value}"$`, "m"));
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
});

const refusal = async (prepare) => {
  const { runDir } = acceptanceEnvRun(withOwner);
  try {
    prepare(runDir);
    const result = await writeAcceptanceEnv(runDir);
    assert.notEqual(result.status, 0);
    assert.equal(existsSync(join(runDir, "secrets/acceptance.env")), false);
    return result.stderr.replaceAll(runDir, "<run>");
  } finally {
    rmSync(runDir, { recursive: true, force: true });
  }
};

const withoutNodeEnvKey = (key) => (runDir) => {
  const path = join(runDir, "secrets/node.env");
  writeFileSync(
    path,
    readFileSync(path, "utf8")
      .split("\n")
      .filter((line) => !line.startsWith(`${key}=`))
      .join("\n"),
  );
};

for (const key of [
  "L1_ORIGIN",
  "L1_NODE_SOCKET_PATH",
  "L1_NODE_CONFIG_PATH",
  "L1_NATIVE_CHAIN_SYNC_BINARY_PATH",
  "HUB_ORACLE_ONE_SHOT_TX_HASH",
  "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX",
])
  test(`acceptance env refuses a node.env without ${key}, by name`, async () => {
    const stderr = await refusal(withoutNodeEnvKey(key));
    assert.match(
      stderr,
      new RegExp(
        `Phase4L1FollowerInputError: Phase 4 L1 follower input is missing: node\\.env lacks ${key};`,
      ),
    );
  });

test("acceptance env refuses an empty L1_ORIGIN, as an interrupted bootstrap leaves it", async () => {
  const stderr = await refusal((runDir) => {
    const path = join(runDir, "secrets/node.env");
    writeFileSync(
      path,
      readFileSync(path, "utf8").replace(/^L1_ORIGIN=.*$/mu, "L1_ORIGIN="),
    );
  });
  assert.match(stderr, /node\.env lacks L1_ORIGIN;/);
});

for (const file of [
  "bin/midgard-l1-node-transport",
  "config/host-config.json",
  "work/l1-origin.json",
])
  test(`acceptance env refuses a run without ${file}, by name`, async () => {
    const stderr = await refusal((runDir) => rmSync(join(runDir, file)));
    assert.ok(
      stderr.includes(
        `Phase4L1FollowerInputError: Phase 4 L1 follower input is missing: <run>/${file}`,
      ),
      stderr,
    );
  });

test("acceptance env refuses a local-node path outside the run", async () => {
  const stderr = await refusal((runDir) => {
    const path = join(runDir, "secrets/node.env");
    writeFileSync(
      path,
      readFileSync(path, "utf8").replace(
        /^L1_NODE_SOCKET_PATH=.*$/mu,
        "L1_NODE_SOCKET_PATH=/ipc/node.socket",
      ),
    );
  });
  assert.match(
    stderr,
    /L1_NODE_SOCKET_PATH must be <run>\/cardano\/ipc\/node\.socket, got \/ipc\/node\.socket/,
  );
});

test("acceptance env refuses an origin derived from another nonce", async () => {
  const stderr = await refusal((runDir) =>
    writeFileSync(
      join(runDir, "work/l1-origin.json"),
      JSON.stringify({ l1Origin: L1_ORIGIN, nonceTxHash: "99".repeat(32) }),
    ),
  );
  assert.match(
    stderr,
    new RegExp(
      `L1_ORIGIN ${L1_ORIGIN.replace(".", "\\.")} was not derived from this run's hub-oracle nonce ${NONCE_TX_HASH}`,
    ),
  );
});

test("acceptance env refuses a transport binary that is not executable", async () => {
  const stderr = await refusal((runDir) =>
    chmodSync(join(runDir, "bin/midgard-l1-node-transport"), 0o644),
  );
  assert.match(stderr, /the executable transport binary/);
});

test("acceptance env refuses a run directory reached through a symlink", async () => {
  const { runDir } = acceptanceEnvRun(withOwner);
  const link = `${runDir}-link`;
  try {
    symlinkSync(runDir, link);
    for (const name of ["run.env", "secrets/node.env"]) {
      const path = join(runDir, name);
      writeFileSync(path, readFileSync(path, "utf8").replaceAll(runDir, link));
    }
    const result = await writeAcceptanceEnv(link);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /a run directory without symlinks/);
    assert.equal(existsSync(join(runDir, "secrets/acceptance.env")), false);
  } finally {
    rmSync(link, { force: true });
    rmSync(runDir, { recursive: true, force: true });
  }
});

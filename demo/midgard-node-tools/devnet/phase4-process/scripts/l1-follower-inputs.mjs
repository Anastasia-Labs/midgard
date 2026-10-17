import {
  chmodSync,
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  renameSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { isAbsolute, join, normalize } from "node:path";
import { fileURLToPath } from "node:url";

/**
 * The node's L1 follower inputs for a Phase 4 run. The node follows L1 only
 * with a local node (socket, config and the midgard-l1-node-transport
 * binary), `L1_ORIGIN` and the hub-oracle one-shot; without any one of them
 * it stays unready (`l1_follower_unconfigured`), so the journal-kill-recovery
 * gate would never see `/readyz` ok. protocol-bootstrap.sh writes them into
 * node.env and write-acceptance-env.sh refuses an env that lacks one.
 */
export class Phase4L1FollowerInputError extends Error {
  name = "Phase4L1FollowerInputError";
}

const refuse = (detail) => {
  throw new Phase4L1FollowerInputError(
    `Phase 4 L1 follower input is missing: ${detail}`,
  );
};

/** Where the run keeps each durable follower input. */
export const followerPaths = (runDir) => ({
  socketPath: join(runDir, "cardano/ipc/node.socket"),
  hostConfigPath: join(runDir, "config/host-config.json"),
  transportBinaryPath: join(runDir, "bin/midgard-l1-node-transport"),
  originRecordPath: join(runDir, "work/l1-origin.json"),
});

const writeAtomically = (path, text, mode) => {
  const temporary = `${path}.tmp.${process.pid}`;
  writeFileSync(temporary, text, { mode });
  renameSync(temporary, path);
};

/**
 * The run's node config with host paths: the container config names
 * /genesis/, which a host process (the node's follower and ledger reads)
 * cannot open. Same rewrite as devnet-stack's host config.
 */
export const writeHostCardanoConfig = (runDir) => {
  const source = join(runDir, "config/config.json");
  if (!existsSync(source)) refuse(`the run's node config ${source}`);
  const config = JSON.parse(readFileSync(source, "utf8"));
  const genesis = join(runDir, "genesis");
  const rewritten = Object.fromEntries(
    Object.entries(config).map(([key, value]) => [
      key,
      typeof value === "string" && value.startsWith("/genesis/")
        ? join(genesis, value.slice("/genesis/".length))
        : value,
    ]),
  );
  const { hostConfigPath } = followerPaths(runDir);
  writeAtomically(
    hostConfigPath,
    `${JSON.stringify(rewritten, null, 2)}\n`,
    0o644,
  );
  return hostConfigPath;
};

/** Copies the built transport binary into the run, executable. */
export const installTransportBinary = (runDir, source) => {
  if (!existsSync(source))
    refuse(
      `the midgard-l1-node-transport binary ${source}; build it with \`pnpm --dir demo/l1-node-transport run native:build\``,
    );
  const { transportBinaryPath } = followerPaths(runDir);
  mkdirSync(join(runDir, "bin"), { recursive: true });
  const temporary = `${transportBinaryPath}.tmp.${process.pid}`;
  copyFileSync(source, temporary);
  chmodSync(temporary, 0o755);
  renameSync(temporary, transportBinaryPath);
  return transportBinaryPath;
};

const HEX_32 = /^[0-9a-f]{64}$/u;
const L1_ORIGIN = /^(?:0|[1-9][0-9]*)\.[0-9a-f]{64}$/u;

/**
 * Records `midgard-l1-follower find-origin` output for the run's nonce tx and
 * returns its `l1Origin`. The CLI exits non-zero unless it found the tx, so
 * a caller passes only a successful run's stdout; anything else refuses.
 */
export const recordL1Origin = (runDir, nonceTxHash, findOriginStdout) => {
  const txHash = nonceTxHash.toLowerCase();
  if (!HEX_32.test(txHash))
    refuse(`a 64-hex hub-oracle nonce tx id (got ${nonceTxHash})`);
  let found;
  try {
    found = JSON.parse(findOriginStdout);
  } catch {
    refuse(`find-origin printed no JSON for the nonce tx ${txHash}`);
  }
  if (typeof found?.l1Origin !== "string" || !L1_ORIGIN.test(found.l1Origin))
    refuse(`find-origin printed no l1Origin for the nonce tx ${txHash}`);
  const record = {
    l1Origin: found.l1Origin,
    nonceTxHash: txHash,
    prepareHubOracleNonceBlock: found.prepareHubOracleNonceBlock,
    recordedAt: new Date().toISOString(),
  };
  writeAtomically(
    followerPaths(runDir).originRecordPath,
    `${JSON.stringify(record, null, 2)}\n`,
    0o644,
  );
  return record.l1Origin;
};

const canonicalAbsolute = (value) =>
  isAbsolute(value) && normalize(value) === value;

/**
 * The follower inputs acceptance.env must carry, from node.env. Refuses by
 * name when one is missing, when a local-node path is not the run's own, when
 * the binary or host config is absent, or when L1_ORIGIN was not derived from
 * this run's hub-oracle nonce.
 */
export const followerInputs = (nodeValues, runDir) => {
  const value = (name) => nodeValues[name]?.trim() ?? "";
  const missing = [
    "L1_ORIGIN",
    "L1_NODE_SOCKET_PATH",
    "L1_NODE_CONFIG_PATH",
    "L1_NODE_TRANSPORT_BINARY_PATH",
    "HUB_ORACLE_ONE_SHOT_TX_HASH",
    "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX",
  ].filter((name) => value(name) === "");
  if (missing.length > 0)
    refuse(`node.env lacks ${missing.join(", ")}; rerun protocol-bootstrap.sh`);
  const paths = followerPaths(runDir);
  for (const [name, expected] of [
    ["L1_NODE_SOCKET_PATH", paths.socketPath],
    ["L1_NODE_CONFIG_PATH", paths.hostConfigPath],
    ["L1_NODE_TRANSPORT_BINARY_PATH", paths.transportBinaryPath],
  ]) {
    if (!canonicalAbsolute(value(name)) || value(name) !== expected)
      refuse(`${name} must be ${expected}, got ${value(name)}`);
  }
  if (!existsSync(paths.hostConfigPath))
    refuse(`the host node config ${paths.hostConfigPath}`);
  if (
    !existsSync(paths.transportBinaryPath) ||
    (statSync(paths.transportBinaryPath).mode & 0o111) === 0
  )
    refuse(`the executable transport binary ${paths.transportBinaryPath}`);
  // The node reads its local-node config only through a symlink-free path.
  if (realpathSync(paths.hostConfigPath) !== paths.hostConfigPath)
    refuse(
      `a run directory without symlinks; ${paths.hostConfigPath} resolves to ${realpathSync(paths.hostConfigPath)}`,
    );
  const txHash = value("HUB_ORACLE_ONE_SHOT_TX_HASH").toLowerCase();
  if (!HEX_32.test(txHash))
    refuse(`HUB_ORACLE_ONE_SHOT_TX_HASH is not a 64-hex tx id`);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(value("HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX")))
    refuse("HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX is not a natural number");
  if (!L1_ORIGIN.test(value("L1_ORIGIN")))
    refuse(`L1_ORIGIN is not <slot>.<block hash>: ${value("L1_ORIGIN")}`);
  if (!existsSync(paths.originRecordPath))
    refuse(`the origin record ${paths.originRecordPath}`);
  const record = JSON.parse(readFileSync(paths.originRecordPath, "utf8"));
  if (record.nonceTxHash !== txHash || record.l1Origin !== value("L1_ORIGIN"))
    refuse(
      `L1_ORIGIN ${value("L1_ORIGIN")} was not derived from this run's hub-oracle nonce ${txHash} (${paths.originRecordPath})`,
    );
  return {
    L1_ORIGIN: value("L1_ORIGIN"),
    L1_NODE_SOCKET_PATH: paths.socketPath,
    L1_NODE_CONFIG_PATH: paths.hostConfigPath,
    L1_NODE_TRANSPORT_BINARY_PATH: paths.transportBinaryPath,
    HUB_ORACLE_ONE_SHOT_TX_HASH: txHash,
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: value("HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX"),
  };
};

const main = (argv) => {
  const [command, ...args] = argv;
  switch (command) {
    case "socket-path":
      return followerPaths(args[0]).socketPath;
    case "host-config":
      return writeHostCardanoConfig(args[0]);
    case "install-transport":
      return installTransportBinary(args[0], args[1]);
    case "record-origin":
      return recordL1Origin(args[0], args[1], readFileSync(args[2], "utf8"));
    default:
      throw new Error(
        "usage: l1-follower-inputs.mjs socket-path <run dir> | host-config <run dir> | install-transport <run dir> <binary> | record-origin <run dir> <nonce tx> <find-origin stdout file>",
      );
  }
};

if (
  process.argv[1] !== undefined &&
  existsSync(process.argv[1]) &&
  realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  try {
    process.stdout.write(`${main(process.argv.slice(2))}\n`);
  } catch (error) {
    process.stderr.write(`${error.name}: ${error.message}\n`);
    process.exit(1);
  }
}

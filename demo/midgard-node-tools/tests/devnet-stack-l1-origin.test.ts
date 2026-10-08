import { mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";
import { afterEach, describe, expect, it, vi } from "vitest";

import { committeeEnvironment } from "../src/devnet-stack/da.js";
import {
  ensureL1Origin,
  recordedL1Origin,
} from "../src/devnet-stack/deployment-origin.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import { Journal } from "../src/devnet-stack/journal.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { nodeEnvironment } from "../src/devnet-stack/node-env.js";
import {
  assertNodeFollowerEnvironment,
  deriveL1Origin,
  L1OriginUndeterminedError,
  NodeFollowerUnconfiguredError,
} from "../src/l1-origin.js";
import { nodeFollowerPlan } from "./node-follower-plan.js";

const dirs: string[] = [];
const runLayout = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-origin-"));
  dirs.push(dir);
  const layout = makeLayout(dir);
  mkdirSync(layout.state, { recursive: true });
  return layout;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const run: RunEnv = {
  runId: "t",
  composeProject: "p",
  networkMagic: 42,
  ogmiosPort: 1,
  kupoPort: 2,
  postgresPort: 3,
  postgresUser: "u",
  postgresPassword: "pw",
  postgresDatabase: "d",
  cardanoImage: "c",
  postgresImage: "pg",
  portOffset: 0,
};
const identities = {
  schemaVersion: "midgard-devnet-identities-v1",
  seeds: Object.fromEntries(WALLET_ROLES.map((role) => [role, `seed-${role}`])),
  libp2p: Object.fromEntries(
    LIBP2P_IDENTITIES.map((id) => [id, "00".repeat(32)]),
  ),
  adminApiKey: "k",
  publicReaderPassword: "r",
} as Identities;
const artifacts = {
  nativeOwnerBinary: "/artifacts/owner",
  nativeOwnerSha256: "h",
  transportBinary: "/artifacts/chain-sync",
};
const oneShot = { txHash: "AB".repeat(32), outputIndex: 1 };
const origin = { slot: 41, blockHash: "cd".repeat(32) };
const nonceBlock = { slot: 42, blockHash: "ef".repeat(32) };
const found = {
  kind: "found" as const,
  origin,
  nonceBlock: { ...nonceBlock, height: 7 },
  txIndex: 0,
  depth: 3,
};

describe("the devnet node environment", () => {
  it("makes the node's L1 follower run from the run's recorded origin", async () => {
    const layout = runLayout();
    const derive = vi.fn((input: Parameters<typeof deriveL1Origin>[0]) =>
      deriveL1Origin({ ...input, find: () => Promise.resolve(found) }),
    );
    const context = { layout, run, artifacts };
    expect(await ensureL1Origin(context, oneShot, derive)).toEqual(origin);
    expect(await ensureL1Origin(context, oneShot, derive)).toEqual(origin);
    expect(derive).toHaveBeenCalledOnce();
    expect(derive.mock.calls[0]![0].node).toEqual({
      socketPath: layout.cardanoSocket,
      binaryPath: artifacts.transportBinary,
      networkMagic: run.networkMagic,
    });
    const env = nodeEnvironment({
      layout,
      run,
      identities,
      artifacts,
      oneShot,
      l1Origin: recordedL1Origin(layout, oneShot),
      historyGenesisPin: "ab".repeat(32),
      role: "listen",
    });
    expect(env.L1_ORIGIN).toBe(`41.${"cd".repeat(32)}`);
    const plan = nodeFollowerPlan(env);
    expect(plan).toMatchObject({
      kind: "run",
      socketPath: layout.cardanoSocket,
      binaryPath: artifacts.transportBinary,
      origin: { hubOracleOneShot: { index: 1 } },
    });
    if (plan.kind !== "run") throw new Error(plan.detail);
    expect(plan.origin.origin.slot).toBe(41);
    expect(plan.origin.origin.hash.toString("hex")).toBe("cd".repeat(32));
    expect(plan.origin.hubOracleOneShot.txHash.toString("hex")).toBe(
      "ab".repeat(32),
    );
  });

  it("refuses listen without a recorded origin, and never records a failed derivation", async () => {
    const layout = runLayout();
    expect(() => recordedL1Origin(layout, oneShot)).toThrow(
      L1OriginUndeterminedError,
    );
    await expect(
      ensureL1Origin({ layout, run, artifacts }, oneShot, (input) =>
        deriveL1Origin({
          ...input,
          find: () =>
            Promise.resolve({ kind: "not_found", scannedTo: "genesis" }),
        }),
      ),
    ).rejects.toThrow(L1OriginUndeterminedError);
    expect(new Journal(layout.journal).get("l1Origin")).toBeUndefined();
    expect(() =>
      nodeEnvironment({
        layout,
        run,
        identities,
        artifacts,
        oneShot,
        historyGenesisPin: "ab".repeat(32),
        role: "listen",
      }),
    ).toThrow(L1OriginUndeterminedError);
  });

  it("refuses an origin recorded for another nonce", async () => {
    const layout = runLayout();
    await ensureL1Origin({ layout, run, artifacts }, oneShot, () =>
      Promise.resolve({ origin, nonceTxHash: "ab".repeat(32), nonceBlock }),
    );
    expect(() =>
      recordedL1Origin(layout, { txHash: "ff".repeat(32), outputIndex: 0 }),
    ).toThrow(L1OriginUndeterminedError);
  });

  it("refuses a listen environment whose local node keys the node rejects", () => {
    expect(() =>
      nodeEnvironment({
        layout: runLayout(),
        run,
        identities,
        artifacts: { ...artifacts, transportBinary: "chain-sync" },
        oneShot,
        l1Origin: origin,
        historyGenesisPin: "ab".repeat(32),
        role: "listen",
      }),
    ).toThrow(NodeFollowerUnconfiguredError);
  });
});

describe("the devnet committee environment", () => {
  it("makes each committee's L1 follower run from the run's recorded origin, with no chain index", async () => {
    const layout = runLayout();
    await ensureL1Origin({ layout, run, artifacts }, oneShot, () =>
      Promise.resolve({ origin, nonceTxHash: "ab".repeat(32), nonceBlock }),
    );
    const members = {
      ...identities,
      seeds: {
        ...identities.seeds,
        operator: entropyToMnemonic("01".repeat(16)),
        daCosigner: entropyToMnemonic("02".repeat(16)),
      },
    } as Identities;
    for (const index of [0, 1]) {
      const env = committeeEnvironment({
        layout,
        run,
        identities: members,
        transportBinary: artifacts.transportBinary,
        index,
        l1Origin: recordedL1Origin(layout, oneShot),
      });
      expect(env.L1_ORIGIN).toBe(`41.${"cd".repeat(32)}`);
      expect(env.CARDANO_LOCAL_NODE_SOCKET_PATH).toBe(layout.cardanoSocket);
      expect(env.CARDANO_L1_NODE_TRANSPORT_BINARY_PATH).toBe(
        artifacts.transportBinary,
      );
      expect(
        Object.keys(env).filter((name) =>
          /KUPO|OGMIOS|PROVIDER_URL|L1_SOURCE_MODE|CHAIN_SYNC_(URL|CURSOR)/u.test(
            name,
          ),
        ),
      ).toEqual([]);
    }
  });
});

describe("assertNodeFollowerEnvironment", () => {
  const complete = {
    L1_NODE_SOCKET_PATH: "/ipc/node.socket",
    L1_NODE_CONFIG_PATH: "/cardano/config.json",
    L1_NODE_TRANSPORT_BINARY_PATH: "/bin/chain-sync",
    L1_ORIGIN: `41.${"cd".repeat(32)}`,
    HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
  };

  it("accepts exactly the environments the node's follower runs with", () => {
    expect(() => assertNodeFollowerEnvironment(complete)).not.toThrow();
    expect(nodeFollowerPlan(complete).kind).toBe("run");
  });

  it.each([
    [
      "no local node",
      { L1_NODE_SOCKET_PATH: undefined },
      /L1_NODE_SOCKET_PATH/,
    ],
    ["no origin", { L1_ORIGIN: "" }, /L1_ORIGIN is not set/],
    ["a malformed origin", { L1_ORIGIN: "41" }, /L1_ORIGIN/],
    ["no one-shot tx", { HUB_ORACLE_ONE_SHOT_TX_HASH: "" }, /HUB_ORACLE/],
    [
      "no one-shot index",
      { HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "" },
      /HUB_ORACLE/,
    ],
  ])("refuses %s", (_, change, reason) => {
    const refuse = () =>
      assertNodeFollowerEnvironment({ ...complete, ...change });
    expect(refuse).toThrow(NodeFollowerUnconfiguredError);
    expect(refuse).toThrow(reason);
  });
});

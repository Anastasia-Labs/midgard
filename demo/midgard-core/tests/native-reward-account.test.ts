import { createHash } from "node:crypto";
import { chmod, mkdtemp, realpath, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type NativeLedgerAuthority,
  nativeLedgerAuthoritySource,
  NativeLedgerKupmios,
  parseNativeRewardAccountResult,
  queryNativeRewardAccount,
  resolveNativeLedgerAuthority,
} from "../src/native-reward-account.js";

const SCRIPT_REWARD_ADDRESS =
  "stake_test17rrxhht4hajr32nu03ymgt6dascxfukfuz5wu3qqefvcdlq4a2z47";
const SCRIPT_HASH = "c66bdd75bf6438aa7c7c49b42f4dec3064f2c9e0a8ee4400ca5986fc";

const expected = {
  credential: { type: "Script" as const, hash: "ab".repeat(28) },
  startupDigest: "cd".repeat(32),
};
const registeredWithoutDelegation = () => ({
  ...expected,
  kind: "reward_account",
  depositLovelace: "2000000",
  point: { blockHash: "ef".repeat(32), blockNo: "42", slot: "99" },
  poolIdHash: null,
  registered: true,
  rewardsLovelace: "0",
  schemaVersion: NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
});

describe("native reward-account admission", () => {
  it("keeps registered credentials with no rewards or delegation registered", () => {
    expect(
      parseNativeRewardAccountResult(registeredWithoutDelegation(), expected),
    ).toEqual({ registered: true, rewards: 0n, poolId: null });
  });

  it("admits an absent ledger credential", () => {
    expect(
      parseNativeRewardAccountResult(
        {
          ...registeredWithoutDelegation(),
          registered: false,
          depositLovelace: null,
        },
        expected,
      ),
    ).toEqual({ registered: false, rewards: 0n, poolId: null });
  });

  it.each([
    { registered: true, depositLovelace: null },
    { registered: false },
    { registered: false, depositLovelace: null, rewardsLovelace: "1" },
    { registered: false, depositLovelace: null, poolIdHash: "ac".repeat(28) },
    { credential: { type: "Key", hash: expected.credential.hash } },
    { credential: { type: "Script", hash: "01".repeat(28) } },
    { startupDigest: "00".repeat(32) },
    { rewardsLovelace: "-1" },
    { point: { blockHash: "00".repeat(32), blockNo: "1.5", slot: "99" } },
    { schemaVersion: "midgard-watcher-native-chain-sync-v0" },
    { extra: true },
  ])("refuses inconsistent or substituted ledger state: %j", (change) => {
    expect(() =>
      parseNativeRewardAccountResult(
        { ...registeredWithoutDelegation(), ...change },
        expected,
      ),
    ).toThrow();
  });
});

describe("native ledger authority", () => {
  let directory: string;
  let socketPath: string;
  let binaryPath: string;
  const genesis = (networkMagic: number) =>
    JSON.stringify({ networkMagic, systemStart: "2022-06-01T00:00:00Z" });
  const writeNode = async (networkMagic: number) => {
    await writeFile(join(directory, "shelley.json"), genesis(networkMagic));
    await writeFile(
      join(directory, "config.json"),
      JSON.stringify({ ShelleyGenesisFile: "shelley.json" }),
    );
  };
  const input = (network: NativeLedgerAuthority["network"] = "Preprod") => ({
    authorityNodeId: "local-cardano-node",
    binaryPath,
    network,
    nodeConfigPath: join(directory, "config.json"),
    socketPath,
    timeoutMs: 5000,
  });
  /** A helper double that answers as the ledger would for one credential. */
  const writeHelper = async (answer: string) => {
    await writeFile(
      binaryPath,
      `#!${process.execPath}
const { createHash } = require("node:crypto");
let line = "";
process.stdin.on("data", (chunk) => (line += chunk));
process.stdin.on("end", () => {
  const startup = line.slice(0, -1);
  const request = JSON.parse(startup);
  const digest = createHash("sha256").update(startup).digest("hex");
  const answer = ${answer};
  process.stdout.write(JSON.stringify(answer(request, digest)) + "\\n");
});
`,
    );
    await chmod(binaryPath, 0o755);
  };

  beforeEach(async () => {
    directory = await realpath(
      await mkdtemp(join(tmpdir(), "midgard-native-ledger-")),
    );
    socketPath = join(directory, "node.socket");
    binaryPath = join(directory, "helper.cjs");
    await writeFile(socketPath, "");
  });
  afterEach(async () => {
    await rm(directory, { recursive: true, force: true });
  });

  it("binds the authority to the Shelley genesis the node runs", async () => {
    await writeNode(1);
    const { nodeConfigPath: _, ...rest } = input();
    expect(await resolveNativeLedgerAuthority(input())).toEqual({
      ...rest,
      genesisIdentitySha256: createHash("sha256")
        .update(genesis(1))
        .digest("hex"),
      networkMagic: 1,
    });
  });

  it.each([
    ["a public network whose genesis magic differs", "Preprod", 2],
    ["a custom network reusing a public magic", "Custom", 764_824_073],
  ] as const)("refuses %s", async (_, network, magic) => {
    await writeNode(magic);
    await expect(resolveNativeLedgerAuthority(input(network))).rejects.toThrow(
      /network magic/u,
    );
  });

  it("refuses a node config path that is not canonical", async () => {
    await writeNode(1);
    await expect(
      resolveNativeLedgerAuthority({
        ...input(),
        nodeConfigPath: `${directory}/./config.json`,
      }),
    ).rejects.toThrow(/absolute path without symlinks/u);
  });

  it("asks the helper for the address's credential and admits its bound answer", async () => {
    await writeNode(1);
    await writeHelper(`(request, digest) => ({
      credential: request.operation.credential,
      depositLovelace: "2000000",
      kind: "reward_account",
      point: { blockHash: "ef".repeat(32), blockNo: "42", slot: "99" },
      poolIdHash: null,
      registered: request.operation.credential.hash === "${SCRIPT_HASH}" &&
        request.operation.credential.type === "Script" &&
        request.operation.kind === "reward_account" &&
        request.intersection.kind === "origin" &&
        request.networkMagic === 1,
      rewardsLovelace: "0",
      schemaVersion: request.schemaVersion,
      startupDigest: digest,
    })`);
    const authority = await resolveNativeLedgerAuthority(input());
    expect(
      await queryNativeRewardAccount(authority, SCRIPT_REWARD_ADDRESS),
    ).toEqual({ registered: true, rewards: 0n, poolId: null });
  });

  it("refuses an answer bound to a different startup", async () => {
    await writeNode(1);
    await writeHelper(`(request) => ({
      credential: request.operation.credential,
      depositLovelace: "2000000",
      kind: "reward_account",
      point: { blockHash: "ef".repeat(32), blockNo: "42", slot: "99" },
      poolIdHash: null,
      registered: true,
      rewardsLovelace: "0",
      schemaVersion: request.schemaVersion,
      startupDigest: "00".repeat(32),
    })`);
    const authority = await resolveNativeLedgerAuthority(input());
    await expect(
      queryNativeRewardAccount(authority, SCRIPT_REWARD_ADDRESS),
    ).rejects.toThrow(/differs from the requested credential/u);
  });

  it("refuses a reward address from another network", async () => {
    await writeNode(1);
    await writeHelper(`() => ({})`);
    const authority = await resolveNativeLedgerAuthority(input());
    await expect(
      queryNativeRewardAccount(
        { ...authority, network: "Mainnet" },
        SCRIPT_REWARD_ADDRESS,
      ),
    ).rejects.toThrow(/differs from the native node network/u);
  });

  it("retries authority resolution after a failure instead of caching it", async () => {
    const source = nativeLedgerAuthoritySource(input());
    await expect(source()).rejects.toThrow();
    await writeNode(1);
    expect((await source())?.networkMagic).toBe(1);
  });

  it("fails reward-account reads closed without a local ledger", async () => {
    const provider = new NativeLedgerKupmios(
      "http://127.0.0.1:1442",
      "http://127.0.0.1:1337",
      nativeLedgerAuthoritySource(undefined),
    );
    await expect(
      provider.getRewardAccount(SCRIPT_REWARD_ADDRESS),
    ).rejects.toThrow(/requires a local node ledger/u);
  });
});

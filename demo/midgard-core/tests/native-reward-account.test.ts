import { createHash } from "node:crypto";
import { mkdtemp, realpath, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  chainPoint,
  closeSharedL1NodeTransports,
  type RewardAccountSnapshot,
} from "@al-ft/l1-node-transport";
import {
  LEDGER_HANDLER,
  writeFakeSidecar,
} from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  admitNativeRewardAccount,
  type NativeLedgerAuthority,
  nativeLedgerAuthoritySource,
  NativeLedgerKupmios,
  queryNativeRewardAccount,
  resolveNativeLedgerAuthority,
} from "../src/native-reward-account.js";

const SCRIPT_REWARD_ADDRESS =
  "stake_test17rrxhht4hajr32nu03ymgt6dascxfukfuz5wu3qqefvcdlq4a2z47";
const SCRIPT_HASH = "c66bdd75bf6438aa7c7c49b42f4dec3064f2c9e0a8ee4400ca5986fc";
const POOL_HASH = "ac".repeat(28);

const registeredWithoutDelegation = (): RewardAccountSnapshot => ({
  registered: true,
  depositLovelace: 2_000_000n,
  rewardsLovelace: 0n,
  poolIdHash: null,
  point: chainPoint(99n, "ef".repeat(32)),
  blockNo: 42n,
});

describe("native reward-account admission", () => {
  it("keeps registered credentials with no rewards or delegation registered", () => {
    expect(admitNativeRewardAccount(registeredWithoutDelegation())).toEqual({
      registered: true,
      rewards: 0n,
      poolId: null,
    });
  });

  it("admits an absent ledger credential", () => {
    expect(
      admitNativeRewardAccount({
        ...registeredWithoutDelegation(),
        registered: false,
        depositLovelace: null,
      }),
    ).toEqual({ registered: false, rewards: 0n, poolId: null });
  });

  it("names the delegated pool by its bech32 id", () => {
    const { poolId } = admitNativeRewardAccount({
      ...registeredWithoutDelegation(),
      poolIdHash: POOL_HASH,
    });
    expect(poolId).toMatch(/^pool1/u);
  });

  it.each<Partial<RewardAccountSnapshot>>([
    { registered: true, depositLovelace: null },
    { registered: false },
    { registered: false, depositLovelace: null, rewardsLovelace: 1n },
    { registered: false, depositLovelace: null, poolIdHash: POOL_HASH },
    { poolIdHash: "ac".repeat(27) },
  ])("refuses inconsistent ledger state (case %#)", (change) => {
    expect(() =>
      admitNativeRewardAccount({ ...registeredWithoutDelegation(), ...change }),
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
  /** A node transport whose node's ledger registers the script credential. */
  const writeTransport = async (options: Record<string, unknown> = {}) => {
    await writeFakeSidecar({
      path: binaryPath,
      handlerModule: LEDGER_HANDLER,
      options: { magic: 1, scriptHash: SCRIPT_HASH, ...options },
    });
  };

  beforeEach(async () => {
    directory = await realpath(
      await mkdtemp(join(tmpdir(), "midgard-native-ledger-")),
    );
    socketPath = join(directory, "node.socket");
    binaryPath = join(directory, "node-transport");
    await writeFile(socketPath, "");
  });
  afterEach(async () => {
    await closeSharedL1NodeTransports();
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

  it("reads the address's credential from the node ledger", async () => {
    await writeNode(1);
    await writeTransport({ rewards: 7, pool: POOL_HASH });
    const authority = await resolveNativeLedgerAuthority(input());
    const state = await queryNativeRewardAccount(
      authority,
      SCRIPT_REWARD_ADDRESS,
    );
    expect(state).toMatchObject({ registered: true, rewards: 7n });
    expect(state.poolId).toMatch(/^pool1/u);
  });

  it("keeps a registered, undelegated credential registered", async () => {
    await writeNode(1);
    await writeTransport();
    const authority = await resolveNativeLedgerAuthority(input());
    expect(
      await queryNativeRewardAccount(authority, SCRIPT_REWARD_ADDRESS),
    ).toEqual({ registered: true, rewards: 0n, poolId: null });
  });

  it("reports a credential the ledger does not hold as unregistered", async () => {
    await writeNode(1);
    await writeTransport({ scriptHash: "01".repeat(28) });
    const authority = await resolveNativeLedgerAuthority(input());
    expect(
      await queryNativeRewardAccount(authority, SCRIPT_REWARD_ADDRESS),
    ).toEqual({ registered: false, rewards: 0n, poolId: null });
  });

  it("fails a read the node does not answer within the timeout", async () => {
    await writeNode(1);
    await writeTransport({ silent: true });
    const authority = await resolveNativeLedgerAuthority(input());
    await expect(
      queryNativeRewardAccount(
        { ...authority, timeoutMs: 300 },
        SCRIPT_REWARD_ADDRESS,
      ),
    ).rejects.toThrow(/no answer within 300 ms/u);
  });

  it("refuses a reward address from another network", async () => {
    await writeNode(1);
    await writeTransport();
    const authority = await resolveNativeLedgerAuthority(input());
    await expect(
      queryNativeRewardAccount(
        { ...authority, network: "Mainnet" },
        SCRIPT_REWARD_ADDRESS,
      ),
    ).rejects.toThrow(/differs from the native node network/u);
  });

  it("refuses a transport binary path that is not canonical", async () => {
    await writeNode(1);
    await writeTransport();
    const authority = await resolveNativeLedgerAuthority({
      ...input(),
      binaryPath: `${directory}/./node-transport`,
    });
    await expect(
      queryNativeRewardAccount(authority, SCRIPT_REWARD_ADDRESS),
    ).rejects.toThrow(/absolute path without symlinks/u);
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

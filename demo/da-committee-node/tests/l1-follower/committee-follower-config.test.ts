import {
  applyChainSyncEvent,
  openSqliteFactStore,
  type OutRef,
  projectionStoreOptions,
  stepSettled,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  type SimOutput,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import {
  CML,
  credentialToAddress,
  Emulator,
  generateSeedPhrase,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { LoadedCommitteeConfig } from "../../src/config.js";
import {
  committeeL1FollowerPlan,
  committeeTracked,
  ownWalletAddress,
} from "../../src/l1/follower/committee-follower-config.js";
import { committeeFollowerStoreOptions } from "../../src/l1/follower/l1-follower.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import { minimalConfig } from "../helpers.js";
import { SIM_QUEUE } from "./queue-sim.js";
const ONE_SHOT_TX = "ab".repeat(32);
const ORIGIN_HASH = "cd".repeat(32);

const loadedConfig = (
  fields: Partial<LoadedCommitteeConfig> = {},
): LoadedCommitteeConfig => ({
  ...minimalConfig({
    manifestPath: "/unused/manifest.json",
    deploymentInfoPath: "/unused/deployment.json",
    signerSeed: "00".repeat(32),
    signerPublicKey: "11".repeat(32),
  }),
  cardanoL1Source: { networkMagic: 42 },
  ...fields,
});

const runnable: Partial<LoadedCommitteeConfig> = {
  l1Origin: { slot: 1_234, blockHash: ORIGIN_HASH },
  nativeLedger: {
    authorityNodeId: "local-cardano-node",
    socketPath: "/run/cardano/node.socket",
    nodeConfigPath: "/etc/cardano/config.json",
    binaryPath: "/opt/midgard/midgard-l1-node-transport",
  },
  contractDeploymentInfo: {
    hubOracleOneShot: { txHash: ONE_SHOT_TX, outputIndex: 3 },
  },
};

describe("how the committee's follower runs", () => {
  it("runs on the configured origin, the local node and the one-shot outref, in the committee's database", () => {
    expect(committeeL1FollowerPlan(loadedConfig(runnable))).toEqual({
      kind: "run",
      databaseUrl: "postgresql://unused.invalid/committee",
      socketPath: "/run/cardano/node.socket",
      binaryPath: "/opt/midgard/midgard-l1-node-transport",
      networkMagic: 42,
      origin: {
        origin: { slot: 1_234, hash: Buffer.from(ORIGIN_HASH, "hex") },
        hubOracleOneShot: { txHash: Buffer.from(ONE_SHOT_TX, "hex"), index: 3 },
      },
    });
  });

  it.each([
    ["no L1_ORIGIN", { l1Origin: undefined }, "L1_ORIGIN is not set"],
    [
      "no local node",
      { nativeLedger: undefined },
      "no local node ledger is configured",
    ],
    [
      "no one-shot outref",
      { contractDeploymentInfo: {} },
      "the deployment info has no hubOracleOneShot outref",
    ],
    [
      "a one-shot tx hash that is not 32 bytes of hex",
      {
        contractDeploymentInfo: {
          hubOracleOneShot: { txHash: "ab".repeat(28), outputIndex: 0 },
        },
      },
      "the deployment info has no hubOracleOneShot outref",
    ],
    [
      "a negative one-shot index",
      {
        contractDeploymentInfo: {
          hubOracleOneShot: { txHash: ONE_SHOT_TX, outputIndex: -1 },
        },
      },
      "the deployment info has no hubOracleOneShot outref",
    ],
  ] as const)(
    "is unconfigured, naming what is missing, with %s",
    (_label, fields, reason) => {
      expect(
        committeeL1FollowerPlan(
          loadedConfig({ ...runnable, ...fields } as never),
        ),
      ).toEqual({ kind: "unconfigured", reason });
    },
  );
});

const hash28 = (byte: string): string => byte.repeat(28);
const scriptAddress = (hash: string): string =>
  credentialToAddress("Preprod", { type: "Script", hash });
const stakedScriptAddress = (hash: string): string =>
  credentialToAddress(
    "Preprod",
    { type: "Script", hash },
    { type: "Key", hash: hash28("e1") },
  );
const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

describe("the committee's tracked set (plan §4.4, committee column)", () => {
  /**
   * The committee's addresses, credentials and policies, all distinct, so
   * each entry is the only one that can capture its fact.
   */
  const config = (() => {
    const base = loadedConfig();
    const deployment = base.midgardNodeDeployment;
    const withHashes = (
      entry: typeof deployment.daBondPool,
      policy: string,
      spend: string,
    ): typeof deployment.daBondPool => ({
      ...entry,
      mint: { ...entry.mint, scriptHash: policy },
      spend: { ...entry.spend, scriptHash: spend },
    });
    return loadedConfig({
      hubOraclePolicyId: hash28("a1"),
      daParamsGovernorPolicyId: hash28("a2"),
      daAttestationPolicyId: hash28("a3"),
      daParamsGovernorAddress: scriptAddress(hash28("b1")),
      daAttestationAddress: scriptAddress(hash28("b2")),
      correctionLockAddress: scriptAddress(hash28("b3")),
      midgardNodeDeployment: {
        ...deployment,
        referenceScriptAuthPolicyId: hash28("a4"),
        daParamsGovernor: withHashes(
          deployment.daParamsGovernor,
          hash28("a2"),
          hash28("c1"),
        ),
        daAttestation: withHashes(
          deployment.daAttestation,
          hash28("a3"),
          hash28("c2"),
        ),
        daBondPool: withHashes(
          deployment.daBondPool,
          hash28("a5"),
          hash28("c3"),
        ),
        availabilityChallenge: withHashes(
          deployment.availabilityChallenge,
          hash28("a6"),
          hash28("c4"),
        ),
      },
    });
  })();

  /** The facts each entry must capture, and controls nothing may. */
  const outputs: readonly (readonly [string, string, boolean])[] = [
    ["DA params governor address", config.daParamsGovernorAddress, true],
    ["DA attestation address", config.daAttestationAddress, true],
    ["correction lock address", config.correctionLockAddress, true],
    // The protocol-init tx pays the hub oracle to its policy's own script.
    ["hub oracle output", scriptAddress(hash28("a1")), true],
    ["DA params governor credential", stakedScriptAddress(hash28("c1")), true],
    ["DA attestation credential", stakedScriptAddress(hash28("c2")), true],
    ["DA bond pool credential", stakedScriptAddress(hash28("c3")), true],
    [
      "availability challenge credential",
      stakedScriptAddress(hash28("c4")),
      true,
    ],
    ["untracked address", scriptAddress(hash28("d1")), false],
    ["untracked staked address", stakedScriptAddress(hash28("d2")), false],
  ];
  const mints: readonly (readonly [string, string, boolean])[] = [
    ["hub oracle policy", hash28("a1"), true],
    ["DA params governor policy", hash28("a2"), true],
    ["DA attestation policy", hash28("a3"), true],
    ["reference-script auth policy", hash28("a4"), true],
    ["DA bond pool policy", hash28("a5"), true],
    ["availability challenge policy", hash28("a6"), true],
    ["untracked policy", hash28("d3"), false],
  ];

  it("captures an output at each tracked address and credential, and a mint under each tracked policy, and nothing else", async () => {
    const options = projectionStoreOptions(
      [committeeProjection(SIM_QUEUE, committeeTracked(config))],
      {
        securityParameter: 4,
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        },
      },
      "sqlite",
    );
    const store = openSqliteFactStore({ ...options, path: ":memory:" });
    try {
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(
        await store.initialize({
          point: SIM_ORIGIN.point,
          height: SIM_ORIGIN.height,
        }),
      ).toMatchObject({ kind: "initialized" });
      const chain = new SimChain(simUniverse(), SIM_ORIGIN, options.trackedSet);
      const untracked = addressBytes(scriptAddress(hash28("d4")));
      const output = (address: Buffer): SimOutput => ({
        address,
        lovelace: 2_000_000n,
      });
      const txs: SimTx[] = [
        ...outputs.map(([, address]) => ({
          inputs: [chain.outsideInput()],
          outputs: [output(addressBytes(address))],
          nonce: chain.nonce(),
        })),
        ...mints.map(([, policy]) => ({
          inputs: [chain.outsideInput()],
          outputs: [output(untracked)],
          mint: new Map([[policy, new Map([["01", 1n]])]]),
          nonce: chain.nonce(),
        })),
      ];
      const step = chain.forward(txs);
      expect(stepSettled(await applyChainSyncEvent(store, step.event))).toBe(
        true,
      );
      const txHash = (index: number): Buffer =>
        step.encoded.txHashes[index] as Buffer;
      const captured: Record<string, boolean> = {};
      for (const [index, [label]] of outputs.entries()) {
        const outRef: OutRef = { txHash: txHash(index), index: 0 };
        captured[label] = (await store.output(outRef)) !== null;
      }
      for (const [index, [label]] of mints.entries())
        captured[label] =
          (await store.txByHash(txHash(outputs.length + index))) !== null;
      expect(captured).toEqual(
        Object.fromEntries(
          [...outputs, ...mints].map(([label, , tracked]) => [label, tracked]),
        ),
      );
    } finally {
      await store.close();
    }
  });

  it("holds none of the committee's own wallets: they are seeded, not tracked", async () => {
    const runnableConfig = {
      ...config,
      stateQueueAddress: scriptAddress(hash28("f1")),
    };
    const wallet = credentialToAddress("Preprod", {
      type: "Key",
      hash: hash28("e5"),
    });
    const details = getAddressDetails(wallet);
    const store = openSqliteFactStore({
      ...committeeFollowerStoreOptions(
        runnableConfig,
        { confirmationDepth: 2, securityParameter: 4 },
        [addressBytes(wallet)],
        "sqlite",
      ),
      path: ":memory:",
    });
    try {
      expect(await store.start()).toMatchObject({ kind: "ready" });
      // The record is written with the store's first cursor.
      expect(
        await store.initialize({
          point: SIM_ORIGIN.point,
          height: SIM_ORIGIN.height,
        }),
      ).toMatchObject({ kind: "initialized" });
      const record = await store.trackedSetRecord();
      if (record === null) throw new Error("no tracked-set record");
      // The record holds the protocol set (not vacuous) and no wallet item.
      expect(record.trackedSet.policies.length).toBeGreaterThan(0);
      expect(record.trackedSet.addresses).not.toContain(details.address.hex);
      expect(record.trackedSet.paymentCredentials).not.toContain(hash28("e5"));
    } finally {
      await store.close();
    }
  });
});

describe("the committee's own wallet address", () => {
  it.each([
    ["a private key", () => CML.PrivateKey.generate_ed25519().to_bech32()],
    ["a seed phrase", () => generateSeedPhrase()],
  ] as const)("is the address Lucid selects for %s", async (_label, secret) => {
    const value = secret();
    const lucid = await Lucid(new Emulator([]), "Preprod");
    if (value.includes(" ")) lucid.selectWallet.fromSeed(value);
    else lucid.selectWallet.fromPrivateKey(value);
    await expect(
      ownWalletAddress(
        value.includes(" ") ? `seed:${value}` : `private-key:${value}`,
        "preprod",
      ),
    ).resolves.toBe(await lucid.wallet().address());
  });
});

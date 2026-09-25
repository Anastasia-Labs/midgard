import { NativeLedgerKupmios } from "@al-ft/midgard-core/native-reward-account";
import { describe, expect, it } from "vitest";

import {
  makeNodeKupmios,
  nativeLedgerSettingsFromEnv,
  parseNativeLedgerSettings,
} from "../src/services/native-ledger.js";

const complete = {
  L1_NODE_SOCKET_PATH: "/run/cardano/node.socket",
  L1_NODE_CONFIG_PATH: "/etc/cardano/preprod/config.json",
  L1_NATIVE_CHAIN_SYNC_BINARY_PATH: "/opt/midgard/bin/midgard-chain-sync",
};
const REWARD_ADDRESS =
  "stake_test17rrxhht4hajr32nu03ymgt6dascxfukfuz5wu3qqefvcdlq4a2z47";

describe("native ledger settings", () => {
  it("admits all three paths together", () => {
    expect(parseNativeLedgerSettings(complete)).toEqual({
      socketPath: complete.L1_NODE_SOCKET_PATH,
      nodeConfigPath: complete.L1_NODE_CONFIG_PATH,
      binaryPath: complete.L1_NATIVE_CHAIN_SYNC_BINARY_PATH,
    });
  });

  it("treats all-blank settings as unconfigured", () => {
    expect(
      parseNativeLedgerSettings({
        L1_NODE_SOCKET_PATH: "",
        L1_NODE_CONFIG_PATH: " ",
        L1_NATIVE_CHAIN_SYNC_BINARY_PATH: undefined,
      }),
    ).toBeUndefined();
    expect(nativeLedgerSettingsFromEnv({})).toBeUndefined();
  });

  it("refuses a partial configuration and names what is missing", () => {
    expect(() =>
      parseNativeLedgerSettings({
        ...complete,
        L1_NODE_CONFIG_PATH: "",
      }),
    ).toThrow(/missing L1_NODE_CONFIG_PATH$/u);
  });

  it.each([
    ["L1_NODE_SOCKET_PATH", "run/node.socket"],
    ["L1_NODE_CONFIG_PATH", "/etc/cardano/../cardano/config.json"],
    ["L1_NATIVE_CHAIN_SYNC_BINARY_PATH", "/opt/midgard//bin/helper"],
  ] as const)("refuses a non-canonical %s", (name, value) => {
    expect(() =>
      parseNativeLedgerSettings({ ...complete, [name]: value }),
    ).toThrow(`${name} must be an absolute canonical path`);
  });
});

describe("node Kupmios transport", () => {
  it("reads reward accounts through the local ledger", () => {
    expect(
      makeNodeKupmios({
        kupoUrl: "http://127.0.0.1:1442",
        ogmiosUrl: "http://127.0.0.1:1337",
        network: "Preprod",
        nativeLedger: parseNativeLedgerSettings(complete),
      }),
    ).toBeInstanceOf(NativeLedgerKupmios);
  });

  it("fails closed instead of asking Ogmios when no local ledger is configured", async () => {
    const provider = makeNodeKupmios({
      kupoUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "http://127.0.0.1:1337",
      network: "Preprod",
      nativeLedger: undefined,
    });
    await expect(provider.getRewardAccount(REWARD_ADDRESS)).rejects.toThrow(
      /requires a local node ledger/u,
    );
  });
});

import { describe, expect, it } from "vitest";

import {
  nativeLedgerSettingsFromEnv,
  parseNativeLedgerSettings,
} from "../src/services/native-ledger.js";

const complete = {
  L1_NODE_SOCKET_PATH: "/run/cardano/node.socket",
  L1_NODE_CONFIG_PATH: "/etc/cardano/preprod/config.json",
  L1_NODE_TRANSPORT_BINARY_PATH: "/opt/midgard/bin/midgard-l1-node-transport",
};

describe("native ledger settings", () => {
  it("admits all three paths together", () => {
    expect(parseNativeLedgerSettings(complete)).toEqual({
      socketPath: complete.L1_NODE_SOCKET_PATH,
      nodeConfigPath: complete.L1_NODE_CONFIG_PATH,
      binaryPath: complete.L1_NODE_TRANSPORT_BINARY_PATH,
    });
  });

  it("treats all-blank settings as unconfigured", () => {
    expect(
      parseNativeLedgerSettings({
        L1_NODE_SOCKET_PATH: "",
        L1_NODE_CONFIG_PATH: " ",
        L1_NODE_TRANSPORT_BINARY_PATH: undefined,
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
    ["L1_NODE_TRANSPORT_BINARY_PATH", "/opt/midgard//bin/helper"],
  ] as const)("refuses a non-canonical %s", (name, value) => {
    expect(() =>
      parseNativeLedgerSettings({ ...complete, [name]: value }),
    ).toThrow(`${name} must be an absolute canonical path`);
  });
});

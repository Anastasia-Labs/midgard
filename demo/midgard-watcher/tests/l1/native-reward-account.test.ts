import { describe, expect, it } from "vitest";

import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import { parseWatcherNativeRewardAccountResult } from "../../src/l1/native-reward-account.js";

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
  schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
});

describe("native reward-account admission", () => {
  it("keeps registered credentials with no rewards or delegation registered", () => {
    expect(
      parseWatcherNativeRewardAccountResult(
        registeredWithoutDelegation(),
        expected,
      ),
    ).toEqual({ registered: true, rewards: 0n, poolId: null });
  });

  it("admits an absent ledger credential", () => {
    expect(
      parseWatcherNativeRewardAccountResult(
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
    { extra: true },
  ])("refuses inconsistent or substituted ledger state: %j", (change) => {
    expect(() =>
      parseWatcherNativeRewardAccountResult(
        { ...registeredWithoutDelegation(), ...change },
        expected,
      ),
    ).toThrow();
  });
});

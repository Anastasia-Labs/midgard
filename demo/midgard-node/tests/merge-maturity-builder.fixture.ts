import type * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";

export const fetchConfig: SDKStateQueue.StateQueueFetchConfig = {
  stateQueueAddress: "addr_test1statequeue",
  stateQueuePolicyId: "00".repeat(28),
};

export const contracts = {
  daAttestation: {
    policyId: "22".repeat(28),
  },
  stateQueue: {
    mintingScriptCBOR: "",
  },
  settlement: { policyId: "33".repeat(28) },
  hubOracle: { policyId: "44".repeat(28) },
  correctionLock: { spendingScriptAddress: "addr_test1correctionlock" },
} as unknown as SDK.MidgardValidators;

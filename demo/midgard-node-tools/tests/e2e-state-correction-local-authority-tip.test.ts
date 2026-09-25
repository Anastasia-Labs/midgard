import { describe, expect, it } from "vitest";

import { RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY } from "../src/commands/e2e-release-finality-policy.js";
import { createLocalKupmiosStateCorrectionSource } from "../src/commands/e2e-state-correction-local-authority.js";

const hash = (index: number): string => index.toString(16).padStart(64, "0");

/** Ogmios v6: queryNetwork/tip carries no height; blockHeight carries it. */
const tipSource = (answer: (method: string, call: number) => unknown) => {
  const methods: string[] = [];
  const source = createLocalKupmiosStateCorrectionSource({
    provider: "Kupmios",
    providerFailover: undefined,
    kupoUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    manifestId: hash(4),
    stateQueueAddress: "addr_test1qstatequeue",
    stateQueuePolicyId: "ab".repeat(28),
    reserveAddress: "addr_test1qreserve",
    finalityPolicy: {
      confirmationDepth: 3,
      automaticRecoveryMaxDepth: 2160,
      deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
    },
    economicsPolicy: {
      requiredBondLovelace: "900000000",
      slashingPenaltyLovelace: "500000000",
      fraudProverRewardLovelace: "400000000",
      inactivitySlashingPenaltyLovelace: "100000000",
      proverCollateralFloorLovelace: "5000000",
    },
    observeDatabase: async () => ({
      unfinishedMutationJobs: 0,
      pendingFinalizations: 0,
    }),
    fetchImpl: async (_url: string, init?: RequestInit) => {
      const { method } = JSON.parse(String(init?.body)) as { method: string };
      methods.push(method);
      return new Response(
        JSON.stringify({
          jsonrpc: "2.0",
          result: answer(method, methods.length),
        }),
      );
    },
  });
  return { source, methods };
};

describe("Q57 local Kupmios live tip", () => {
  it("binds the block height to a tip read on both sides of it", async () => {
    // The first bracket straddles a tip change; the second agrees.
    const tips = [hash(1), hash(2), hash(2), hash(2)];
    let tipReads = 0;
    const { source, methods } = tipSource((method) =>
      method === "queryNetwork/tip" ? { slot: 130, id: tips[tipReads++] } : 30,
    );
    await expect(source.observeTip()).resolves.toEqual({
      slot: "130",
      blockHash: hash(2),
      height: 30,
    });
    expect(methods).toEqual([
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
    ]);
  });

  it("refuses a tip that keeps moving across a bounded number of reads", async () => {
    const { source, methods } = tipSource((method, call) =>
      method === "queryNetwork/tip" ? { slot: 100 + call, id: hash(call) } : 30,
    );
    await expect(source.observeTip()).rejects.toThrow(
      "live Ogmios tip moved during each of 5 block height reads",
    );
    expect(methods).toHaveLength(15);
  });

  it.each([["origin"], [undefined], [-1], ["30"]])(
    "fails closed on an invalid block height %j",
    async (height) => {
      const { source } = tipSource((method) =>
        method === "queryNetwork/tip" ? { slot: 130, id: hash(2) } : height,
      );
      await expect(source.observeTip()).rejects.toThrow(
        /live Ogmios block height/u,
      );
    },
  );
});

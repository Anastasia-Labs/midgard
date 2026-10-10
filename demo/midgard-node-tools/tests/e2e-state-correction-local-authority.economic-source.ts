import "./e2e-state-correction-local-authority.q57-local-kupmios-authority.js";

import type {
  WebSocketFactory,
  WebSocketLike,
} from "midgard-node/l1-external/kupmios-history";
import { describe, expect, it, vi } from "vitest";

import { RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY } from "../src/commands/e2e-release-finality-policy.js";
import { createLocalKupmiosStateCorrectionSource } from "../src/commands/e2e-state-correction-local-authority.js";
import {
  economicsPolicy,
  hash,
  payoutDestination,
  policy,
  RELEASE_DEPTH,
} from "./e2e-state-correction-local-authority.make-source.js";

const economicSource = ({
  kupoLovelace,
  rollBackTwice = false,
}: {
  readonly kupoLovelace: string;
  readonly rollBackTwice?: boolean;
}) => {
  const ancestorHash = hash(70);
  const txHash = hash(71);
  const includedAt = { slot: "100", blockHash: hash(72) };
  const fetchImpl = vi.fn(async (url: string) => {
    if (url.includes("/checkpoints/99")) {
      return new Response(
        JSON.stringify({ slot_no: 99, header_hash: ancestorHash }),
      );
    }
    if (url.includes(`/matches/*@${txHash}`)) {
      return new Response(
        JSON.stringify([
          {
            transaction_id: txHash,
            output_index: 0,
            address: payoutDestination,
            value: { coins: kupoLovelace, assets: {} },
            spent_at: null,
          },
        ]),
      );
    }
    throw new Error(`unexpected local Kupo URL ${url}`);
  });
  const webSocketFactory: WebSocketFactory = () => {
    const listeners = new Map<string, ((event: { data?: string }) => void)[]>();
    let nextBlockCalls = 0;
    const emit = (type: string, event: { data?: string } = {}) => {
      for (const listener of listeners.get(type) ?? []) listener(event);
    };
    const socket: WebSocketLike = {
      send: (data) => {
        const request = JSON.parse(data) as {
          readonly id: number;
          readonly method: string;
        };
        let result: unknown;
        if (request.method === "findIntersection") {
          result = { intersection: { slot: 99, id: ancestorHash } };
        } else {
          nextBlockCalls += 1;
          result =
            nextBlockCalls === 1 || (rollBackTwice && nextBlockCalls === 2)
              ? { direction: "backward", point: { slot: 99, id: ancestorHash } }
              : {
                  direction: "forward",
                  block: {
                    id: includedAt.blockHash,
                    slot: Number(includedAt.slot),
                    transactions: [
                      {
                        id: txHash,
                        fee: { ada: { lovelace: 5_000_000 } },
                        inputs: [],
                        references: [],
                        outputs: [
                          {
                            address: payoutDestination,
                            value: { ada: { lovelace: 3_000_000 } },
                          },
                        ],
                      },
                    ],
                  },
                };
        }
        queueMicrotask(() =>
          emit("message", {
            data: JSON.stringify({ id: request.id, result }),
          }),
        );
      },
      close: () => undefined,
      addEventListener: (type, listener) => {
        const typed = listener as unknown as (event: { data?: string }) => void;
        listeners.set(type, [...(listeners.get(type) ?? []), typed]);
        if (type === "open") queueMicrotask(() => typed({}));
      },
    };
    return socket;
  };
  return {
    source: createLocalKupmiosStateCorrectionSource({
      providerFailover: undefined,
      kupoUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "http://127.0.0.1:1337",
      manifestId: hash(4),
      stateQueueAddress: "addr_test1qstatequeue",
      stateQueuePolicyId: policy,
      reserveAddress: "addr_test1qreserve",
      finalityPolicy: {
        confirmationDepth: RELEASE_DEPTH,
        automaticRecoveryMaxDepth: 2160,
        deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
      },
      economicsPolicy,
      observeDatabase: async () => ({
        unfinishedMutationJobs: 0,
        pendingFinalizations: 0,
      }),
      fetchImpl,
      webSocketFactory,
    }),
    txHash,
    includedAt,
  };
};

describe("Q57 local Kupmios raw economic source", () => {
  it("derives exact fee and output value from matching raw sources", async () => {
    const fixture = economicSource({ kupoLovelace: "3000000" });
    await expect(
      fixture.source.observeEconomicTransaction({
        txHash: fixture.txHash,
        outputIndex: 0,
        includedAt: fixture.includedAt,
      }),
    ).resolves.toEqual({
      feeLovelace: "5000000",
      inputs: [],
      referenceInputs: [],
      outputs: [
        {
          address: payoutDestination,
          lovelace: "3000000",
          assets: {},
        },
      ],
    });
  });

  it("rejects mocked Kupo/Ogmios output disagreement", async () => {
    const fixture = economicSource({ kupoLovelace: "2999999" });
    await expect(
      fixture.source.observeEconomicTransaction({
        txHash: fixture.txHash,
        outputIndex: 0,
        includedAt: fixture.includedAt,
      }),
    ).rejects.toThrow(/live Kupo\/Ogmios output disagreement/u);
  });

  it("rejects a mocked rollback during the economic chain-sync read", async () => {
    const fixture = economicSource({
      kupoLovelace: "3000000",
      rollBackTwice: true,
    });
    await expect(
      fixture.source.observeEconomicTransaction({
        txHash: fixture.txHash,
        outputIndex: 0,
        includedAt: fixture.includedAt,
      }),
    ).rejects.toThrow(/rolled back during economic observation/u);
  });
});

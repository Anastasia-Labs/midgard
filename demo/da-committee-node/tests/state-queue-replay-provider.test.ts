import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { createLocalKupmiosStateQueueReplayProvider } from "../src/l1/state-queue-replay-provider.js";

const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string): string => `${h32(byte)}#0`;
const deployment = h32("a");
const policy = h28("b");
const target = h28("1");
const descendant = h28("2");
const transactionHash = h32("c");
const hubPolicy = h28("a");
const fraudPolicy = h28("e");
const correctionLockAddress = "addr_test_correction_lock";
const fraudProofAddress = "addr_test_fraud_proof";
const before: readonly SDK.StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: outRef("1") },
  { headerHash: descendant, outRef: outRef("2") },
];
const after: readonly SDK.StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: `${transactionHash}#0` },
];

const harness = ({ rollback = false, tipHeight = 119 } = {}) => {
  const redeemer = Data.to(
    {
      RemoveUnattestedBlockAfterTimeout: {
        timed_out_header_hash: target,
        removal_approach: {
          PruneTimedOutBlockDescendant: {
            confirmed_state_ref_input_index: 0n,
            timed_out_node_input_outref: {
              transactionId: h32("1"),
              outputIndex: 0n,
            },
            timed_out_node_output_index: 0n,
          },
        },
      },
    } satisfies SDK.StateQueueRedeemer,
    SDK.StateQueueRedeemer,
  );
  const fetchImpl = vi.fn(async (url: string, init?: RequestInit) => {
    if (url.includes("/matches/*@")) {
      return new Response(
        JSON.stringify([
          {
            transaction_id: transactionHash,
            output_index: 0,
            address: "addr_test_state_queue",
            datum_type: "inline",
            datum: Data.to(
              { data: { Node: { data: 0n } }, link: null },
              SDK.LinkedListDatum,
            ),
            value: {
              coins: 2_000_000,
              assets: {
                [`${policy}.${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${target}`]: 1,
              },
            },
          },
          {
            transaction_id: transactionHash,
            output_index: 9,
            address: correctionLockAddress,
            datum_type: "inline",
            datum: Data.to(
              {
                Locked: {
                  target_header_hash: target,
                  correction_identity: "AttestationTimeout",
                },
              },
              SDK.CorrectionLockDatum,
            ),
            value: {
              coins: 2_000_000,
              assets: {
                [`${hubPolicy}.${SDK.CORRECTION_LOCK_ASSET_NAME}`]: 1,
              },
            },
          },
        ]),
      );
    }
    if (url.includes("/matches/")) {
      const match = /matches\/(\d+)@([0-9a-f]{64})/u.exec(url)!;
      const isLockInput = match[2] === h32("f");
      return new Response(
        JSON.stringify([
          {
            transaction_id: match[2],
            output_index: Number(match[1]),
            address: isLockInput ? correctionLockAddress : undefined,
            datum_type: isLockInput ? "inline" : undefined,
            datum: isLockInput
              ? Data.to("Idle", SDK.CorrectionLockDatum)
              : null,
            value: isLockInput
              ? {
                  coins: 2_000_000,
                  assets: {
                    [`${hubPolicy}.${SDK.CORRECTION_LOCK_ASSET_NAME}`]: 1,
                  },
                }
              : undefined,
            spent_at:
              match[2] === h32("1") || match[2] === h32("2") || isLockInput
                ? {
                    slot_no: 100,
                    header_hash: h32("7"),
                    transaction_id: transactionHash,
                    input_index:
                      match[2] === h32("1") ? 0 : match[2] === h32("2") ? 1 : 2,
                    redeemer: "d87980",
                  }
                : null,
          },
        ]),
      );
    }
    if (url.includes("/checkpoints/")) {
      return new Response(
        JSON.stringify({ slot_no: 99, header_hash: h32("6") }),
      );
    }
    expect(init?.method).toBe("POST");
    return new Response(
      JSON.stringify({
        result: { id: h32("9"), slot: 130, height: tipHeight },
      }),
    );
  });
  const webSocketFactory = () => {
    const listeners = new Map<string, ((event: never) => void)[]>();
    let nextBlockCount = 0;
    const emit = (type: string, event: unknown): void => {
      for (const listener of listeners.get(type) ?? [])
        listener(event as never);
    };
    return {
      send: (payload: string) => {
        const request = JSON.parse(payload) as { id: number; method: string };
        queueMicrotask(() => {
          if (request.method === "findIntersection") {
            emit("message", {
              data: JSON.stringify({
                id: request.id,
                result: { intersection: { slot: 99, id: h32("6") } },
              }),
            });
            return;
          }
          nextBlockCount += 1;
          emit("message", {
            data: JSON.stringify({
              id: request.id,
              result:
                nextBlockCount === 1 || rollback
                  ? { direction: "backward" }
                  : {
                      direction: "forward",
                      block: {
                        id: h32("7"),
                        slot: 100,
                        height: 90,
                        transactions: [
                          {
                            id: transactionHash,
                            inputs: [
                              { transaction: { id: h32("1") }, index: 0 },
                              { transaction: { id: h32("2") }, index: 0 },
                              { transaction: { id: h32("f") }, index: 0 },
                            ],
                            references: [],
                            mint: { [policy]: { "": -1 } },
                            redeemers: [
                              {
                                redeemer,
                                validator: { purpose: "mint", index: 0 },
                              },
                            ],
                          },
                        ],
                      },
                    },
            }),
          });
        });
      },
      close: () => undefined,
      addEventListener: (type: string, listener: (event: never) => void) => {
        listeners.set(type, [...(listeners.get(type) ?? []), listener]);
        if (type === "open") queueMicrotask(() => listener({} as never));
      },
    };
  };
  return createLocalKupmiosStateQueueReplayProvider({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    stateQueueAddress: "addr_test_state_queue",
    hubOraclePolicyId: hubPolicy,
    correctionLockAddress,
    fraudProofPolicyId: fraudPolicy,
    fraudProofAddress,
    kupoUrl: "http://kupo.test",
    ogmiosUrl: "ws://ogmios.test",
    fetchImpl,
    webSocketFactory,
  });
};

describe("committee local Kupmios state-queue replay", () => {
  it("derives an exact finalized timeout checkpoint from independent Kupo/Ogmios reads", async () => {
    await expect(harness()(before, after)).resolves.toMatchObject([
      {
        checkpointKind: "timeout_correction",
        finalityDepth: "30",
        terminalTransition: { transitionKind: "timeout_correction" },
      },
    ]);
  });

  it("rejects a rollback after the chain-sync intersection handshake", async () => {
    await expect(harness({ rollback: true })(before, after)).rejects.toThrow(
      /rolled back/u,
    );
  });

  it("exposes shallow history but the SDK retention replay refuses it", async () => {
    const checkpoints = await harness({ tipHeight: 90 })(before, after);
    expect(checkpoints[0]?.finalityDepth).toBe("1");
    expect(
      SDK.replayStateQueueAuthenticatedCheckpoints({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        minimumFinalityDepth: 30n,
        anchor: { queue: before, blockNo: "0", transactionIndex: "0" },
        checkpoints,
      }),
    ).toBeNull();
  });
});

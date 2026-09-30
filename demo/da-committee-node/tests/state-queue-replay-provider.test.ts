import "node:http";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-test-support/hex";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/l1/source-integrity.js";
import "../src/l1/state-queue-replay-provider.js";
import "./state-queue-replay-provider.harness.js";

import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "../src/l1/source-integrity.js";
import {
  createLocalKupmiosStateQueueReplayProvider,
  kupoHoldsChainPoint,
} from "../src/l1/state-queue-replay-provider.js";
import {
  after,
  before,
  correctionLockAddress,
  deployment,
  fraudPolicy,
  fraudProofAddress,
  harness,
  hubPolicy,
  outRef,
  policy,
  target,
} from "./state-queue-replay-provider.harness.js";

/**
 * A queue whose one header node is respent by a datum update in every block,
 * forever: its history from the anchor never reaches `current`.
 */
const churnHarness = () => {
  const transaction = (index: number): string =>
    (0x1000 + index).toString(16).padStart(64, "a");
  const block = (index: number): string =>
    (0x2000 + index).toString(16).padStart(64, "b");
  const root = outRef(0x00);
  const nodeOutput = (index: number) => ({
    transaction_id: transaction(index),
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
  });
  const transactionIndex = (hash: string): number =>
    parseInt(hash.slice(-4), 16) - 0x1000;
  const fetchImpl = vi.fn(async (url: string) => {
    const single = /matches\/(\d+)@([0-9a-f]{64})/u.exec(url);
    if (single !== null) {
      if (`${single[2]!}#${single[1]!}` === root) {
        return new Response(
          JSON.stringify([
            {
              transaction_id: single[2],
              output_index: 0,
              datum: null,
              spent_at: null,
            },
          ]),
        );
      }
      const index = transactionIndex(single[2]!);
      return new Response(
        JSON.stringify([
          {
            ...nodeOutput(index),
            spent_at: {
              slot_no: 1_000 + index + 1,
              header_hash: block(index + 1),
              transaction_id: transaction(index + 1),
              input_index: 0,
            },
          },
        ]),
      );
    }
    const outputs = /matches\/\*@([0-9a-f]{64})/u.exec(url);
    if (outputs !== null) {
      return new Response(
        JSON.stringify([nodeOutput(transactionIndex(outputs[1]!))]),
      );
    }
    const checkpoint = /checkpoints\/(\d+)/u.exec(url);
    if (checkpoint !== null) {
      return new Response(
        JSON.stringify({
          slot_no: Number(checkpoint[1]),
          header_hash: block(Number(checkpoint[1]) - 1_000),
        }),
      );
    }
    throw new Error(`unexpected replay request ${url}`);
  });
  const webSocketFactory = () => {
    const listeners = new Map<string, ((event: never) => void)[]>();
    let ancestorSlot = 0;
    let handshake = true;
    const emit = (type: string, event: unknown): void => {
      for (const listener of listeners.get(type) ?? [])
        listener(event as never);
    };
    return {
      send: (payload: string) => {
        const request = JSON.parse(payload) as {
          id: number;
          method: string;
          params: { points?: readonly { slot: number }[] };
        };
        queueMicrotask(() => {
          if (request.method === "findIntersection") {
            ancestorSlot = request.params.points![0]!.slot;
            emit("message", {
              data: JSON.stringify({
                id: request.id,
                result: { intersection: { slot: ancestorSlot, id: h32(0x66) } },
              }),
            });
            return;
          }
          const index = ancestorSlot + 1 - 1_000;
          const result = handshake
            ? { direction: "backward" }
            : {
                direction: "forward",
                block: {
                  id: block(index),
                  slot: 1_000 + index,
                  height: 500 + index,
                  transactions: [
                    {
                      id: transaction(index),
                      inputs: [
                        {
                          transaction: { id: transaction(index - 1) },
                          index: 0,
                        },
                      ],
                      references: [],
                    },
                  ],
                },
              };
          handshake = false;
          emit("message", { data: JSON.stringify({ id: request.id, result }) });
        });
      },
      close: () => undefined,
      addEventListener: (type: string, listener: (event: never) => void) => {
        listeners.set(type, [...(listeners.get(type) ?? []), listener]);
        if (type === "open") queueMicrotask(() => listener({} as never));
      },
    };
  };
  const provider = createLocalKupmiosStateQueueReplayProvider({
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
  return {
    fetchImpl,
    replay: (limit: number) =>
      provider(
        [
          { headerHash: null, outRef: root },
          { headerHash: target, outRef: `${transaction(0)}#0` },
        ],
        [
          { headerHash: null, outRef: root },
          { headerHash: target, outRef: outRef(0x99) },
        ],
        1_000_000,
        limit,
      ),
  };
};

describe("committee local Kupmios state-queue replay", () => {
  it("authenticates availability-timeout correction identity from the native transaction", async () => {
    const provider = harness({ availability: true });
    const records = await provider(before, after);
    expect(records[0]?.terminalTransition?.correctionLockWitness).toMatchObject(
      {
        kind: "correction_transition",
        correctionIdentity: {
          AvailabilityChallenge: {
            challenge_asset_name: "44414348" + "dd".repeat(28),
          },
        },
      },
    );
  });
  it("derives an exact finalized timeout checkpoint from independent Kupo/Ogmios reads", async () => {
    await expect(harness()(before, after)).resolves.toMatchObject([
      {
        checkpointKind: "timeout_correction",
        finalityDepth: "30",
        terminalTransition: { transitionKind: "timeout_correction" },
      },
    ]);
  });

  it("rejects a rollback after the chain-sync intersection handshake as an observation failure", async () => {
    const replay = harness({ rollback: true })(before, after);
    await expect(replay).rejects.toThrow(/rolled back/u);
    await expect(replay).rejects.not.toBeInstanceOf(L1SourceIntegrityError);
  });

  it("refuses an invalid snapshot tip height as an observation failure", async () => {
    const replay = harness({ tipHeight: -1 })(before, after);
    await expect(replay).rejects.toThrow(/tip height is invalid/u);
    await expect(replay).rejects.not.toBeInstanceOf(L1SourceIntegrityError);
  });

  it("judges checkpoint depth at the snapshot tip it is given, not a later one", async () => {
    await expect(
      harness({ tipHeight: 95 })(before, after),
    ).resolves.toMatchObject([{ finalityDepth: "6" }]);
  });

  it("refuses a snapshot tip below its own history as an observation failure", async () => {
    const replay = harness({ tipHeight: 89 })(before, after);
    await expect(replay).rejects.toThrow(/tip precedes a transaction/u);
    await expect(replay).rejects.not.toBeInstanceOf(L1SourceIntegrityError);
  });

  it("classifies a queue that Kupo shows no spend for as an integrity failure", async () => {
    const replay = harness()(
      [{ headerHash: null, outRef: outRef(0x00) }],
      [{ headerHash: null, outRef: outRef(0x01) }],
    );
    await expect(replay).rejects.toThrow(/cannot advance its durable queue/u);
    await expect(replay).rejects.toBeInstanceOf(
      StateQueueHistoryNotExtendingAnchorError,
    );
  });

  it("classifies an anchor output that Kupo does not know as an integrity failure", async () => {
    const replay = harness({ unknownOutRefs: [outRef(0x22)] })(before, after);
    await expect(replay).rejects.toThrow(
      `Kupo does not know state-queue output ${outRef(0x22)} exactly once (0 matches)`,
    );
    await expect(replay).rejects.toBeInstanceOf(
      StateQueueHistoryNotExtendingAnchorError,
    );
  });

  it("walks at most its limit of history, in order, for the caller to resume from", async () => {
    const { replay, fetchImpl } = churnHarness();
    const walked = await replay(5);
    // Five checkpoints of the endless churn, oldest first, and no more read.
    expect(walked).toHaveLength(5);
    for (const [index, checkpoint] of walked.entries()) {
      expect(checkpoint.previousQueue).toEqual(
        index === 0 ? walked[0]!.previousQueue : walked[index - 1]!.nextQueue,
      );
    }
    expect(
      fetchImpl.mock.calls.filter(([url]) => url.includes("/matches/*@")),
    ).toHaveLength(2 * 5);
  });

  it("refuses an invalid walk limit", async () => {
    await expect(churnHarness().replay(0)).rejects.toThrow(
      "state-queue replay checkpoint limit is invalid",
    );
  });

  it.each([
    ["the block at that slot", { slot_no: 70, header_hash: h32(0x70) }, true],
    [
      "another block at that slot",
      { slot_no: 70, header_hash: h32(0x71) },
      false,
    ],
    ["only an earlier block", { slot_no: 69, header_hash: h32(0x69) }, false],
    [
      "the block's hash at another slot",
      { slot_no: 69, header_hash: h32(0x70) },
      false,
    ],
    ["no checkpoint", null, false],
  ] as const)(
    "holds a chain point only when Kupo's checkpoint at its slot is %s",
    async (_label, checkpoint, holds) => {
      const urls: string[] = [];
      await expect(
        kupoHoldsChainPoint(
          "http://kupo.test/",
          { slot: 70, blockHash: h32(0x70).toUpperCase() },
          async (url) => {
            urls.push(url);
            return new Response(JSON.stringify(checkpoint));
          },
        ),
      ).resolves.toBe(holds);
      expect(urls).toEqual(["http://kupo.test/checkpoints/70"]);
    },
  );

  it("fails a Kupo read that is accepted and never answered", async () => {
    const server = createServer(() => {});
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const { port } = server.address() as AddressInfo;
    try {
      await expect(
        kupoHoldsChainPoint(
          `http://127.0.0.1:${port.toString()}`,
          { slot: 70, blockHash: h32(0x70) },
          fetch,
          200,
        ),
      ).rejects.toMatchObject({ name: "TimeoutError" });
    } finally {
      server.closeAllConnections();
      await new Promise((resolve) => server.close(resolve));
    }
  }, 5_000);

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

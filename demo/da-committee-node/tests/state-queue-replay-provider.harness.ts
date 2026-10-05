import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { createLocalKupmiosStateQueueReplayProvider } from "../src/l1/state-queue-replay-provider.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import { fixtureHeaderBase } from "./helpers.js";

export const outRef = (byte: number): string => `${h32(byte)}#0`;

export const deployment = h32(0xaa);

export const policy = h28(0xbb);

export const targetHeader = {
  ...fixtureHeaderBase(),
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
};
export const descendantHeader = {
  ...targetHeader,
  endTime: targetHeader.endTime + 1n,
};
export const target = hashBlockHeader(targetHeader);

const descendant = hashBlockHeader(descendantHeader);

const transactionHash = h32(0xcc);

export const hubPolicy = h28(0xaa);

export const fraudPolicy = h28(0xee);

export const correctionLockAddress = "addr_test_correction_lock";

export const fraudProofAddress = "addr_test_fraud_proof";

export const before: readonly SDK.StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef(0x00) },
  { headerHash: target, outRef: outRef(0x11) },
  { headerHash: descendant, outRef: outRef(0x22) },
];

export const after: readonly SDK.StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef(0x00) },
  { headerHash: target, outRef: `${transactionHash}#0` },
];

export const harness = ({
  rollback = false,
  tipHeight = 119,
  rawTipHeight = tipHeight,
  omitRawTip = false,
  rawTip = undefined as unknown,
  availability = false,
  /** Output references Kupo does not know, as after a deep rollback. */
  unknownOutRefs = [] as readonly string[],
}: {
  rollback?: boolean;
  tipHeight?: number;
  rawTipHeight?: number;
  omitRawTip?: boolean;
  rawTip?: unknown;
  availability?: boolean;
  unknownOutRefs?: readonly string[];
} = {}) => {
  const challenge = "44414348" + "dd".repeat(28);
  const identity: SDK.CorrectionIdentity = availability
    ? { AvailabilityChallenge: { challenge_asset_name: challenge } }
    : "AttestationTimeout";
  const unattested = {
    RemoveUnattestedBlockAfterTimeout: {
      yield_to_ref_input_index: 0n,
      timed_out_header_hash: target,
      removal_approach: {
        PruneUnattestedBlockDescendant: {
          predecessor_ref_input_index: 0n,
          timed_out_node_input_outref: {
            transactionId: h32(0x11),
            outputIndex: 0n,
          },
          timed_out_node_output_index: 0n,
        },
      },
    },
  } satisfies SDK.StateQueueRedeemer;
  const redeemer = Data.to(
    availability
      ? {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: 0n,
            unavailable_header_hash: target,
            challenge_asset_name: challenge,
            removal_approach: {
              PruneTimedOutBlockDescendant: {
                confirmed_state_ref_input_index: 0n,
                timed_out_node_input_outref: {
                  transactionId: h32(0x11),
                  outputIndex: 0n,
                },
                timed_out_node_output_index: 0n,
              },
            },
          },
        }
      : unattested,
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
                  correction_identity: identity,
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
      if (unknownOutRefs.includes(`${match[2]!}#${match[1]!}`)) {
        return new Response(JSON.stringify([]));
      }
      const isLockInput = match[2] === h32(0xff);
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
              match[2] === h32(0x11) || match[2] === h32(0x22) || isLockInput
                ? {
                    slot_no: 100,
                    header_hash: h32(0x77),
                    transaction_id: transactionHash,
                    input_index:
                      match[2] === h32(0x11)
                        ? 0
                        : match[2] === h32(0x22)
                          ? 1
                          : 2,
                    redeemer: "d87980",
                  }
                : null,
          },
        ]),
      );
    }
    if (url.includes("/checkpoints/")) {
      return new Response(
        JSON.stringify({ slot_no: 99, header_hash: h32(0x66) }),
      );
    }
    throw new Error(`unexpected replay request ${url} ${String(init?.method)}`);
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
                result: { intersection: { slot: 99, id: h32(0x66) } },
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
                      ...(omitRawTip
                        ? {}
                        : {
                            tip:
                              rawTip ??
                              (rawTipHeight === 90
                                ? { id: h32(0x77), slot: 100, height: 90 }
                                : {
                                    id: h32(0x88),
                                    slot: rawTipHeight + 10,
                                    height: rawTipHeight,
                                  }),
                          }),
                      block: {
                        id: h32(0x77),
                        slot: 100,
                        height: 90,
                        transactions: [
                          {
                            id: transactionHash,
                            inputs: [
                              { transaction: { id: h32(0x11) }, index: 0 },
                              { transaction: { id: h32(0x22) }, index: 0 },
                              { transaction: { id: h32(0xff) }, index: 0 },
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
  // The snapshot's tip height, which the caller passes so that one tick
  // judges all finality at one tip.
  return (
    previousQueue: readonly SDK.StateQueueTransitionNode[],
    currentQueue: readonly SDK.StateQueueTransitionNode[],
  ) => provider(previousQueue, currentQueue, tipHeight, 64);
};

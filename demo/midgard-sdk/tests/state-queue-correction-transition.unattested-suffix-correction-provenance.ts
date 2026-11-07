import "./state-queue-correction-transition.state-queue-correction-transition-v1.js";

import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  deriveStateQueueAuthenticatedTransition,
  deriveStateQueueCorrectionTransition,
  parseStateQueueAuthenticatedTransition,
} from "../src/index.js";
import {
  common,
  outRef,
  timeoutLock,
  timeoutRedeemer,
} from "./state-queue-correction-transition.fraud-lock.js";

describe("unattested suffix correction provenance", () => {
  const target = h28(0x22);
  const previousQueue = [
    { headerHash: null, outRef: outRef(0x00, 0) },
    { headerHash: h28(0x11), outRef: outRef(0x11, 0) },
    { headerHash: target, outRef: outRef(0x22, 0) },
  ];
  const terminal = () => ({
    ...common,
    ...timeoutLock(target, true),
    transactionIndex: "0",
    spentInputOutRefs: [outRef(0x11, 0), outRef(0x22, 0), outRef(0xff, 0)],
    previousQueue,
    nextQueue: [
      previousQueue[0]!,
      { headerHash: h28(0x11), outRef: outRef(0xcc, 0) },
    ],
    redeemers: timeoutRedeemer({
      RemoveUnattestedBlockAfterTimeout: {
        yield_to_ref_input_index: 0n,
        timed_out_header_hash: target,
        removal_approach: {
          RemoveLastUnattestedBlock: {
            predecessor_input_outref: {
              transactionId: h32(0x11),
              outputIndex: 0n,
            },
            predecessor_output_index: 0n,
          },
        },
      },
    }),
  });
  it("authenticates terminal removal behind an untouched prefix and releases the exact lock", () => {
    const result = deriveStateQueueAuthenticatedTransition(terminal());
    expect(result?.removedHeaderHashes).toEqual([target]);
    expect(result?.correctionTransition?.removalApproach).toBe(
      "RemoveLastUnattestedBlock",
    );
    expect(parseStateQueueAuthenticatedTransition(result)).toEqual(result);
    expect(
      deriveStateQueueAuthenticatedTransition({
        ...terminal(),
        nextQueue: [
          { ...previousQueue[0]!, outRef: outRef(0xcc, 2) },
          terminal().nextQueue[1]!,
        ],
      }),
    ).toBeNull();
    expect(
      deriveStateQueueAuthenticatedTransition({
        ...terminal(),
        ...timeoutLock(h28(0x99), true),
      }),
    ).toBeNull();
  });
  it("authenticates suffix pruning behind predecessors and rejects non-descendant removal", () => {
    const input = {
      ...terminal(),
      ...timeoutLock(target, false),
      spentInputOutRefs: [outRef(0x22, 0), outRef(0x33, 0), outRef(0xff, 0)],
      previousQueue: [
        ...previousQueue,
        { headerHash: h28(0x33), outRef: outRef(0x33, 0) },
        { headerHash: h28(0x44), outRef: outRef(0x44, 0) },
      ],
      nextQueue: [
        ...previousQueue.slice(0, 2),
        { headerHash: target, outRef: outRef(0xcc, 0) },
        { headerHash: h28(0x44), outRef: outRef(0x44, 0) },
      ],
      redeemers: timeoutRedeemer({
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: target,
          removal_approach: {
            PruneUnattestedBlockDescendant: {
              predecessor_ref_input_index: 0n,
              timed_out_node_input_outref: {
                transactionId: h32(0x22),
                outputIndex: 0n,
              },
              timed_out_node_output_index: 0n,
            },
          },
        },
      }),
    };
    const result = deriveStateQueueAuthenticatedTransition(input);
    expect(result?.removedHeaderHashes).toEqual([h28(0x33)]);
    expect(parseStateQueueAuthenticatedTransition(result)).toEqual(result);
    expect(
      deriveStateQueueAuthenticatedTransition({
        ...input,
        nextQueue: [
          ...previousQueue.slice(0, 2),
          { headerHash: target, outRef: outRef(0xcc, 0) },
          input.previousQueue[3]!,
        ],
      }),
    ).toBeNull();
    expect(
      deriveStateQueueAuthenticatedTransition({
        ...input,
        nextQueue: [
          input.nextQueue[0]!,
          input.nextQueue[1]!,
          input.nextQueue[3]!,
          input.nextQueue[2]!,
        ],
      }),
    ).toBeNull();
  });
  it("keeps availability timeout head-only despite the new unattested wire", () => {
    const input = terminal();
    expect(
      deriveStateQueueCorrectionTransition({
        ...input,
        redeemers: timeoutRedeemer({
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: 0n,
            unavailable_header_hash: target,
            challenge_asset_name: h32(0x99),
            removal_approach: {
              RemoveTimedOutHead: {
                confirmed_state_input_outref: {
                  transactionId: h32(0x11),
                  outputIndex: 0n,
                },
                confirmed_state_output_index: 0n,
              },
            },
          },
        }),
      }),
    ).toBeNull();
  });
});

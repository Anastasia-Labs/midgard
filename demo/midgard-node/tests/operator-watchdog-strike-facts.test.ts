/**
 * The landed-queue facts the operator watchdog holds a takeover on (B7): the
 * tail's commit time and when the queue stops accepting appends.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { stateQueueStrikeFactsOf } from "../src/l1-operator-set/strike-facts.js";
import type {
  LandedStateQueue,
  LandedStateQueueElement,
} from "../src/l1-state-queue/index.js";

const TIMEOUT = SDK.DA_ATTESTATION_TIMEOUT_MS;

const header: SDK.Header = {
  ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: "77".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  startTime: 0n,
  endTime: 1_000n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "88".repeat(28),
  operatorVkey: "99".repeat(28),
  protocolVersion: 1n,
};

const ATTESTED = { Attested: { commitment_hash: "11".repeat(32) } } as const;
const CHALLENGED = {
  Challenged: {
    commitment_hash: "11".repeat(32),
    challenge_asset_name: "22".repeat(32),
  },
} as const;

const element = (
  blockTimeMs: bigint | null,
  status: SDK.DaAvailabilityStateQueueStatus | null,
  provenFraud: string | null = null,
): LandedStateQueueElement => ({
  outRef: `${"ab".repeat(32)}#0`,
  element: {
    datum: {
      data:
        status === null
          ? null
          : SDK.castStateQueueNodeToData({
              header,
              block_time: blockTimeMs ?? 0n,
              da_attestation: status,
              proven_fraud: provenFraud,
            }),
    },
  } as unknown as SDK.StateQueueUTxO,
  headerHash: "cd".repeat(28),
  endTimeMs: 1_000n,
  blockTimeMs,
  daStatus:
    status === null ? null : SDK.daAvailabilityStateQueueStatusIdentity(status),
  problems: [],
  created: null,
});

const queue = (
  nodes: readonly LandedStateQueueElement[],
  healthy = true,
): LandedStateQueue =>
  ({
    healthy,
    reason: null,
    detail: null,
    root: element(null, null),
    nodes,
    strays: [],
    policyOutputCount: nodes.length + 1,
  }) as unknown as LandedStateQueue;

describe("state-queue strike facts", () => {
  it("has no commit time and no closed fence for a root-only queue", () => {
    expect(stateQueueStrikeFactsOf(queue([]))).toEqual({
      tailBlockTimeMs: null,
      appendFenceClosesAtMs: null,
    });
  });

  it("closes the fence at the earliest unattested deadline", () => {
    expect(
      stateQueueStrikeFactsOf(
        queue([
          element(5_000n, ATTESTED),
          element(7_000n, "Unattested"),
          element(9_000n, "Unattested"),
        ]),
      ),
    ).toEqual({
      tailBlockTimeMs: 9_000n,
      appendFenceClosesAtMs: 7_000n + TIMEOUT,
    });
    expect(stateQueueStrikeFactsOf(queue([element(5_000n, ATTESTED)]))).toEqual(
      { tailBlockTimeMs: 5_000n, appendFenceClosesAtMs: null },
    );
  });

  it("closes the fence for good on a challenged or fraud-proven head", () => {
    for (const head of [
      element(5_000n, CHALLENGED),
      element(5_000n, ATTESTED, "ef".repeat(32)),
    ])
      expect(
        stateQueueStrikeFactsOf(queue([head, element(9_000n, ATTESTED)])),
      ).toEqual({ tailBlockTimeMs: 9_000n, appendFenceClosesAtMs: 0n });
    // A challenged node behind the head leaves appends to the head's fence.
    expect(
      stateQueueStrikeFactsOf(
        queue([element(5_000n, ATTESTED), element(9_000n, CHALLENGED)]),
      ),
    ).toEqual({ tailBlockTimeMs: 9_000n, appendFenceClosesAtMs: null });
  });

  it("reads nothing from an unhealthy queue", () => {
    expect(
      stateQueueStrikeFactsOf(queue([element(5_000n, ATTESTED)], false)),
    ).toBeNull();
  });
});

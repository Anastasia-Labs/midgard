import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  castStateQueueNodeV1ToData,
  EMPTY_MERKLE_TREE_ROOT,
  getHeaderV1FromStateQueueDatum,
  type HeaderV1,
  makeHeaderTransitionCommitmentsV1Program,
} from "../src/index.js";

const h32 = (byte: number): string => Buffer.alloc(32, byte).toString("hex");
const h28 = (byte: number): string => Buffer.alloc(28, byte).toString("hex");

const header = (): HeaderV1 => ({
  prevUtxosRoot: h32(1),
  utxosRoot: h32(2),
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: h32(3),
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: h32(4),
  eventToStepRoot: h32(5),
  validationTracesRoot: h32(8),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28(6),
  operatorVkey: h28(7),
  protocolVersion: 1n,
});

describe("HeaderV1", () => {
  it("requires one validation descriptor for every normal or forced transaction", async () => {
    const commitments = await Effect.runPromise(
      makeHeaderTransitionCommitmentsV1Program({
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: h32(3),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: h32(4),
        eventToStepRoot: h32(5),
        validationTracesRoot: h32(8),
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 1n,
        depositCount: 0n,
        validationTraceCount: 1n,
      }),
    );

    expect(commitments.validationTraceCount).toBe(1n);
    await expect(
      Effect.runPromise(
        makeHeaderTransitionCommitmentsV1Program({
          ...commitments,
          withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: h32(3),
          depositsRoot: EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    ).rejects.toThrow(/must equal/u);
  });

  it("round-trips only protocol version 1 state-queue nodes", async () => {
    const value = header();
    const datum = {
      data: castStateQueueNodeV1ToData({
        header: value,
        da_attestation: "",
      }) as Parameters<typeof Data.castFrom>[0],
    };
    await expect(
      Effect.runPromise(getHeaderV1FromStateQueueDatum(datum)),
    ).resolves.toEqual(value);

    const wrongVersionDatum = {
      data: castStateQueueNodeV1ToData({
        header: { ...value, protocolVersion: 2n },
        da_attestation: "",
      }) as Parameters<typeof Data.castFrom>[0],
    };
    await expect(
      Effect.runPromise(getHeaderV1FromStateQueueDatum(wrongVersionDatum)),
    ).rejects.toThrow(/StateQueueNodeV1/u);
  });
});

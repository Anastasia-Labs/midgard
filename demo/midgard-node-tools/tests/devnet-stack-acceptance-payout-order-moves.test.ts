import { expect, it } from "vitest";

import { verifyAcceptancePayoutLineage } from "../src/devnet-stack/acceptance-payout-lineage.js";
import {
  config,
  payoutFixture,
} from "./devnet-stack-acceptance-payout.fixtures.js";

it.each([1, 2])(
  "accepts %s exact canonical predecessor Order movements before initialize",
  (orderMoves) => {
    const input = payoutFixture({ orderMoves });
    const proof = verifyAcceptancePayoutLineage(input, config);
    expect(proof.order.txHash).toBe(input.record.txHash);
    expect(proof.currentOrder.txHash).toBe(
      input.orderSuccessors!.at(-1)!.observed.txHash,
    );
    expect(proof.currentOrder.outputIndex).toBe(1);
    expect(
      proof.lineage.filter((item) => item.phase === "order-update"),
    ).toHaveLength(orderMoves);
  },
);
it("holds on missing/disconnected or duplicate moved Order evidence", () => {
  const input = payoutFixture({ orderMoves: 2 });
  expect(() =>
    verifyAcceptancePayoutLineage({ ...input, orderSuccessors: [] }, config),
  ).toThrow(/exact preceding outref/);
  expect(() =>
    verifyAcceptancePayoutLineage(
      { ...input, orderSuccessors: input.orderSuccessors!.slice(1) },
      config,
    ),
  ).toThrow(/Order next update/);
  expect(() =>
    verifyAcceptancePayoutLineage(
      {
        ...input,
        orderSuccessors: [
          ...input.orderSuccessors!,
          input.orderSuccessors![0]!,
        ],
      },
      config,
    ),
  ).toThrow(/duplicate Order/);
});
it("refuses a moved NFT whose facts or full value changed", () => {
  expect(() =>
    verifyAcceptancePayoutLineage(
      payoutFixture({ orderMoves: 1, corruptOrder: true }),
      config,
    ),
  ).toThrow(/Order update facts/);
  expect(() =>
    verifyAcceptancePayoutLineage(
      payoutFixture({ orderMoves: 1, corruptOrderValue: true }),
      config,
    ),
  ).toThrow(/Order update.*value/);
});
it("requires each moved Order's depth and charges it to the same explicit bound", () => {
  const input = payoutFixture({ orderMoves: 1 });
  expect(() =>
    verifyAcceptancePayoutLineage(
      {
        ...input,
        orderSuccessors: input.orderSuccessors!.map((row) => ({
          ...row,
          canonicalDepth: 2159n,
        })),
      },
      config,
    ),
  ).toThrow(/selected-chain depth/);
  expect(() =>
    verifyAcceptancePayoutLineage(payoutFixture({ orderMoves: 4 }), config),
  ).toThrow(/explicit transaction bound/);
});

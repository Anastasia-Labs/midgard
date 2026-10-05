import type { CardanoDatum } from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import { verifyAcceptancePayoutLineage } from "../src/devnet-stack/acceptance-payout-lineage.js";
import {
  config,
  hash,
  payoutFixture,
  target,
} from "./devnet-stack-acceptance-payout.fixtures.js";

it("follows actual CBOR Order, initialize, unordered multiple funds and exact conclude", () => {
  const fixture = payoutFixture();
  const proof = verifyAcceptancePayoutLineage(fixture, config);
  expect(proof.order.txHash).toBe(fixture.record.txHash);
  expect(proof.beneficiary.txHash).not.toBe(fixture.record.txHash);
  expect(proof.beneficiary.outputIndex).toBe(1);
  expect(proof.assets).toEqual(target);
  expect(proof.lineage.map((item) => item.phase)).toEqual([
    "order",
    "initialize",
    "fund",
    "fund",
    "conclude",
  ]);
});
it.each([false, true])(
  "verifies %s external payload using its exact committed retained datum",
  (external) => {
    const fixture = payoutFixture({ external });
    expect(verifyAcceptancePayoutLineage(fixture, config).eventId).toBe(
      fixture.record.withdrawalEventId,
    );
    if (external) {
      expect(() =>
        verifyAcceptancePayoutLineage(
          { ...fixture, externalDatum: undefined },
          config,
        ),
      ).toThrow(/unbound retained external/);
      expect(() =>
        verifyAcceptancePayoutLineage(
          { ...fixture, externalDatum: "00" },
          config,
        ),
      ).toThrow();
    }
  },
);
it.each([
  ["wrongAddress", /beneficiary address/],
  ["wrongValue", /full value/],
  ["extraAsset", /full value/],
  ["wrongMintIndex", /mint does not bind exact Order/],
  ["wrongFundIndex", /fund output/],
  ["wrongBurnIndex", /burn exact NFT/],
  ["wrongEvent", /mint exact sole payout/],
] as const)("refuses %s at its exact lineage check", (option, reason) => {
  expect(() =>
    verifyAcceptancePayoutLineage(payoutFixture({ [option]: true }), config),
  ).toThrow(reason);
});
const datumCases: CardanoDatum[] = [
  "NoDatum",
  { DatumHash: { hash: hash("ab") } },
  { InlineDatum: { data: 42n } },
];
it.each(datumCases)("preserves exact beneficiary datum %o", (datum) => {
  const proof = verifyAcceptancePayoutLineage(payoutFixture({ datum }), config);
  if (typeof datum === "string")
    expect([proof.datum, proof.datumHash]).toEqual([undefined, undefined]);
  else if ("DatumHash" in datum)
    expect(proof.datumHash).toBe(datum.DatumHash.hash);
  else expect(proof.datum).toBe("182a");
});
it("rejects canonical depth, byte bound and missing source bytes independently", () => {
  const input = payoutFixture();
  expect(() =>
    verifyAcceptancePayoutLineage(
      { ...input, order: { ...input.order, canonicalDepth: 2159n } },
      config,
    ),
  ).toThrow(/selected-chain depth/);
  expect(() =>
    verifyAcceptancePayoutLineage(input, { ...config, maxTransactionBytes: 1 }),
  ).toThrow(/oversized canonical/);
  expect(() =>
    verifyAcceptancePayoutLineage(
      {
        ...input,
        order: {
          ...input.order,
          observed: { ...input.order.observed, transactionCbor: undefined },
        },
      },
      config,
    ),
  ).toThrow(/missing, malformed/);
});
it("refuses receipt hash substitution and a different original Order journal hash", () => {
  const input = payoutFixture();
  expect(() =>
    verifyAcceptancePayoutLineage(
      {
        ...input,
        order: {
          ...input.order,
          observed: { ...input.order.observed, txHash: hash("fe") },
        },
      },
      config,
    ),
  ).toThrow(/hash to observed/);
  expect(() =>
    verifyAcceptancePayoutLineage(
      { ...input, record: { ...input.record, txHash: hash("fe") } },
      config,
    ),
  ).toThrow(/original Order/);
});
it("refuses a signed journal from another body despite valid source bytes", () => {
  const input = payoutFixture();
  const rows = [...input.settlements];
  rows[0] = { ...rows[0]!, signedCbor: input.order.observed.transactionCbor! };
  expect(() =>
    verifyAcceptancePayoutLineage({ ...input, settlements: rows }, config),
  ).toThrow(/hash to observed/);
});
it("rejects duplicate, incomplete and excessive settlement candidates", () => {
  const input = payoutFixture();
  expect(() =>
    verifyAcceptancePayoutLineage(
      { ...input, settlements: [...input.settlements, input.settlements[0]!] },
      config,
    ),
  ).toThrow(/duplicate canonical/);
  expect(() =>
    verifyAcceptancePayoutLineage(
      {
        ...input,
        settlements: input.settlements.filter(
          (row) => row.phase !== "conclude",
        ),
      },
      config,
    ),
  ).toThrow(/missing canonical conclude/);
  expect(() =>
    verifyAcceptancePayoutLineage(input, {
      ...config,
      maxLineageTransactions: 2,
    }),
  ).toThrow(/explicit transaction bound/);
});
it("rejects wrong intent identity, L2 outref, beneficiary and full value", () => {
  const input = payoutFixture();
  for (const patch of [
    { withdrawalEventId: "00" },
    { l2OutRef: `${hash("fe")}#3` },
    { l1Address: "other" },
    { l2Value: { lovelace: "1" } },
  ])
    expect(() =>
      verifyAcceptancePayoutLineage(
        { ...input, record: { ...input.record, ...patch } },
        config,
      ),
    ).toThrow();
});
it("does not accept the fee output index from a corrupt receipt", () => {
  const input = payoutFixture();
  const rows = input.settlements.map((row) =>
    row.phase === "conclude" ? { ...row, requiredOutputs: [0] } : row,
  );
  expect(() =>
    verifyAcceptancePayoutLineage({ ...input, settlements: rows }, config),
  ).toThrow(/input\/output\/journal index/);
});

import { expect, it } from "vitest";

import { verifyAcceptanceCurrentPayouts } from "../src/devnet-stack/acceptance-payout-current.js";
import type { AcceptancePayoutProof } from "../src/devnet-stack/acceptance-payout-types.js";
import {
  beneficiaryAddress,
  hash,
  target,
} from "./devnet-stack-acceptance-payout.fixtures.js";

const proofs = (): AcceptancePayoutProof[] =>
  Array.from({ length: 4 }, (_, index) => ({
    eventId: String(index),
    eventKey: hash("11"),
    order: { txHash: hash("12"), outputIndex: index },
    currentOrder: { txHash: hash("12"), outputIndex: index },
    payout: { txHash: hash("13"), outputIndex: index },
    beneficiary: { txHash: hash("14"), outputIndex: index },
    address: beneficiaryAddress,
    assets: target,
    lineage: [],
  }));
const frame = (items = proofs()) =>
  JSON.stringify({
    jsonrpc: "2.0",
    id: "scope-owned",
    result: items.map((proof) => ({
      transaction: { id: proof.beneficiary.txHash },
      index: proof.beneficiary.outputIndex,
      address: proof.address,
      value: {
        ada: { lovelace: "LOSSLESS_ADA" },
        ["55".repeat(28)]: { ab: "LOSSLESS_ASSET" },
      },
    })),
  })
    .replaceAll('"LOSSLESS_ADA"', target.lovelace.toString())
    .replaceAll('"LOSSLESS_ASSET"', target["55".repeat(28) + "ab"]!.toString());
it("retains exact values above2^53 for all four exact current outrefs", () => {
  const result = verifyAcceptanceCurrentPayouts(frame(), proofs(), 16384);
  expect(result).toHaveLength(4);
  expect(result[0]!.assets.lovelace).toBe("9007199254740993");
  expect(result[0]!.assets["55".repeat(28) + "ab"]).toBe("9007199254740995");
});
it("refuses rounded amounts, extra assets and address aggregates with old outrefs", () => {
  const raw = frame();
  expect(() =>
    verifyAcceptanceCurrentPayouts(
      raw.replace("9007199254740993", "9007199254740992"),
      proofs(),
      16384,
    ),
  ).toThrow(/full value/);
  expect(() =>
    verifyAcceptanceCurrentPayouts(
      raw.replace('"ab":9007199254740995', '"ab":9007199254740995,"ac":1'),
      proofs(),
      16384,
    ),
  ).toThrow(/full value/);
  expect(() =>
    verifyAcceptanceCurrentPayouts(
      raw.replace(hash("14"), hash("15")),
      proofs(),
      16384,
    ),
  ).toThrow(/exact beneficiary outref/);
});
it("refuses missing/spent, duplicate and extra exact query rows", () => {
  const items = proofs();
  expect(() =>
    verifyAcceptanceCurrentPayouts(frame(items.slice(1)), items, 16384),
  ).toThrow(/spent, missing/);
  expect(() =>
    verifyAcceptanceCurrentPayouts(
      frame([items[0]!, items[0]!, items[2]!, items[3]!]),
      items,
      16384,
    ),
  ).toThrow(/duplicate acquired/);
  expect(() =>
    verifyAcceptanceCurrentPayouts(frame([...items, items[0]!]), items, 16384),
  ).toThrow(/extra acquired/);
});
it("rejects duplicate JSON keys before quantity decoding", () => {
  expect(() =>
    verifyAcceptanceCurrentPayouts(
      frame().replace('"lovelace":', '"lovelace":1,"lovelace":'),
      proofs(),
      16384,
    ),
  ).toThrow(/Duplicate key/);
});
it("rejects error/null frames, response overflow and duplicate payout claims", () => {
  for (const raw of [
    "null",
    '{"jsonrpc":"2.0","error":{"code":1}}',
    '{"jsonrpc":"2.0","result":null}',
  ])
    expect(() =>
      verifyAcceptanceCurrentPayouts(raw, proofs(), 16384),
    ).toThrow();
  expect(() => verifyAcceptanceCurrentPayouts(frame(), proofs(), 1)).toThrow(
    /explicit bound/,
  );
  const claims = proofs();
  claims[1] = claims[0]!;
  expect(() => verifyAcceptanceCurrentPayouts(frame(), claims, 16384)).toThrow(
    /four distinct/,
  );
});

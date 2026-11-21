import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  Constr,
  credentialToAddress,
  Data,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { type CardanoDatum } from "../src/ledger-state.js";
import { EventHistoryNode } from "../src/user-events/history.js";
import {
  eventHistoryMinimumNodeLovelace,
  eventHistoryMinimumOutputLovelace,
} from "../src/user-events/history-funding.js";

const key = "aa".repeat(28);
const policy = "bb".repeat(28);
const eventKey = "cc".repeat(32);
const credentials = [
  { type: "Key" as const, hash: key },
  { type: "Script" as const, hash: policy },
];
const addresses = credentials.flatMap((payment) => [
  credentialToAddress("Custom", payment),
  ...credentials.map((stake) => credentialToAddress("Custom", payment, stake)),
]);
const maximumPointer = "81" + "ff".repeat(8) + "7f";
addresses.push(
  CML.Address.from_raw_bytes(
    Buffer.from("40" + key + maximumPointer.repeat(3), "hex"),
  ).to_bech32(),
);
const assetVectors: Assets[] = [
  { lovelace: 100_000_000n },
  { lovelace: 100_000_000n, [policy + eventKey]: 1n },
  Object.fromEntries([
    ["lovelace", 100_000_000n],
    ...Array.from({ length: 10 }, (_, i) => [
      i.toString(16).padStart(56, "0") + eventKey,
      9_223_372_036_854_775_807n,
    ]),
  ]),
];
const datums: CardanoDatum[] = [
  "NoDatum",
  { DatumHash: { hash: "dd".repeat(32) } },
  { InlineDatum: { data: "ab".repeat(5000) } },
  {
    InlineDatum: {
      data: new Constr(0, [
        new Map([["ab", Array.from({ length: 500 }, () => 0n)]]),
      ]),
    },
  },
];
const actualMinimum = (address: string, assets: Assets, datum: CardanoDatum) =>
  calculateMinLovelaceFromUTxO(4310n, {
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets,
    ...(datum === "NoDatum"
      ? {}
      : "InlineDatum" in datum
        ? { datum: Data.to(datum.InlineDatum.data) }
        : { datumHash: datum.DatumHash.hash }),
  });

describe("history future-output funding envelope", () => {
  it("upper-bounds the ledger minimum across key/script addresses, datums and Values", () => {
    let cases = 0;
    for (const address of addresses)
      for (const assets of assetVectors)
        for (const datum of datums) {
          expect(
            eventHistoryMinimumOutputLovelace(assets, datum),
          ).toBeGreaterThanOrEqual(actualMinimum(address, assets, datum));
          cases++;
        }
    expect(cases).toBe(84);
  });
  it("allows insertion of a distinct full-width payout NFT without underestimating minimum ADA", () => {
    for (const assets of assetVectors)
      for (const datum of datums) {
        expect(
          eventHistoryMinimumOutputLovelace(assets, datum, true),
        ).toBeGreaterThanOrEqual(
          actualMinimum(
            addresses[1]!,
            { ...assets, ["ee".repeat(28) + eventKey]: 1n },
            datum,
          ),
        );
      }
  });
  it("funds the widest pointer and timestamp continuation", () => {
    const node: EventHistoryNode = {
      position: { Key: [eventKey] },
      next: null,
      protected_until: 0n,
      payload: { Filler: { refund_key: key } },
    };
    const assets = { lovelace: 100_000_000n, [policy + eventKey]: 1n };
    const floor = eventHistoryMinimumNodeLovelace(assets, node);
    const continued = {
      ...node,
      next: "ff".repeat(32),
      protected_until: 9_223_372_036_854_775_807n,
    };
    expect(floor).toBeGreaterThanOrEqual(
      actualMinimum(
        addresses[1]!,
        { ...assets, lovelace: floor },
        {
          InlineDatum: {
            data: Data.from(Data.to(continued, EventHistoryNode)),
          },
        },
      ),
    );
  });
});

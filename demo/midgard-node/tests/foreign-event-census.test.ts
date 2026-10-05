import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertForeignEventCensusMatchesPayload,
  type ForeignEventCensus,
} from "../src/workers/commit-block-header.foreign-event-census.js";
import { fixture } from "./foreign-block-import.fixture.js";

const empty: ForeignEventCensus = { deposits: [], withdrawals: [], forced: [] };
// Set-equality unit model only. Source admission authentication and canonical
// placement are the history owner's responsibility; this test issues no permit.
const canonicalOrder: ForeignEventCensus = {
  ...empty,
  forced: [
    {
      key: Data.to(
        { transactionId: "5a".repeat(32), outputIndex: 0n },
        SDK.OutputReference,
      ),
      datumCbor: "",
      transactionHash: "6a".repeat(32),
      transactionIndex: 0,
      outputIndex: 0,
      eventUnit: "7a".repeat(60),
    },
  ],
};

describe("independent complete foreign event set", () => {
  it("accepts exact canonical membership and an honestly empty window", async () => {
    const payload = await fixture();
    expect(() =>
      assertForeignEventCensusMatchesPayload(canonicalOrder, payload),
    ).not.toThrow();
    expect(() =>
      assertForeignEventCensusMatchesPayload(empty, {
        ...payload,
        block_body: { ...payload.block_body, forced_transactions: [] },
      }),
    ).not.toThrow();
  });
  it("refuses an omitted canonical forced event even when the claimed set is empty", async () => {
    const payload = await fixture();
    expect(() =>
      assertForeignEventCensusMatchesPayload(canonicalOrder, {
        ...payload,
        block_body: { ...payload.block_body, forced_transactions: [] },
      }),
    ).toThrow(/complete canonical source census/);
  });
  it("refuses a fabricated event in an independently empty source window", async () => {
    const payload = await fixture();
    expect(() =>
      assertForeignEventCensusMatchesPayload(empty, payload),
    ).toThrow(/complete canonical source census/);
  });
  it("refuses duplicate DA members rather than normalizing them away", async () => {
    const payload = await fixture();
    expect(() =>
      assertForeignEventCensusMatchesPayload(canonicalOrder, {
        ...payload,
        block_body: {
          ...payload.block_body,
          forced_transactions: [
            ...payload.block_body.forced_transactions,
            ...payload.block_body.forced_transactions,
          ],
        },
      }),
    ).toThrow(/complete canonical source census/);
  });
});

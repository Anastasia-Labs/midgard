import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { placeholderValidationTraceDescriptorVector } from "../../midgard-validation/tests/placeholder-validation-trace-descriptor-v1.vectors.js";
import { buildPlaceholderValidationTraceMembers } from "../src/mpf/index.js";

const normal = placeholderValidationTraceDescriptorVector(
  "normal-transaction-accepted",
);
const forced = placeholderValidationTraceDescriptorVector(
  "forced-transaction-rejected",
);

const eventKeys: readonly SDK.EventKey[] = [
  // The normal vector's event key is the transaction's own id.
  { L2TransactionEventKey: { tx_id: normal.eventKeyCbor.slice(10, 74) } },
  {
    ForcedTransactionEventKey: {
      tx_order_id: { transactionId: "44".repeat(32), outputIndex: 0n },
    },
  },
];

describe("node validation-trace members", () => {
  it("commits the shared placeholder descriptor and retains no witnesses", async () => {
    const members = await Effect.runPromise(
      buildPlaceholderValidationTraceMembers({
        transactions: [normal, forced].map((vector, index) => ({
          eventKey: eventKeys[index]!,
          canonicalTransactionCbor: Buffer.from(
            vector.transaction!.canonicalTransactionCbor,
            "hex",
          ),
          sourceKind: vector.transaction!.sourceKind,
          verdict: vector.verdict,
          rejectionCode: vector.rejectionCode as never,
        })),
      }),
    );
    expect(
      members.map((member) => ({
        keyCbor: member.keyCbor.toString("hex"),
        valueCbor: member.valueCbor.toString("hex"),
      })),
    ).toEqual(
      [normal, forced].map((vector) => ({
        keyCbor: vector.eventKeyCbor,
        valueCbor: vector.expected.descriptorDataCbor,
      })),
    );
  });

  it("fails closed on transaction bytes that do not decode", async () => {
    await expect(
      Effect.runPromise(
        buildPlaceholderValidationTraceMembers({
          transactions: [
            {
              eventKey: eventKeys[0]!,
              canonicalTransactionCbor: Buffer.from("00", "hex"),
              sourceKind: "normal",
              verdict: "accepted",
              rejectionCode: null,
            },
          ],
        }),
      ),
    ).rejects.toThrow(/placeholder validation descriptor/u);
  });
});

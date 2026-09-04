import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type ForcedLeafEvidence,
  forcedLeafVerdictSubject,
  requireForcedLeafAccepted,
  requireForcedLeafRejectedFor,
} from "../src/evidence/forced-leaf-evidence.js";

const evidence = (verdict: SDK.OperatorVerdict): ForcedLeafEvidence =>
  ({
    eventKey: {
      ForcedTransactionEventKey: {
        tx_order_id: { transactionId: "01".repeat(32), outputIndex: 0n },
      },
    },
    eventKeyFingerprint: "forced-fixture",
    leaf: {
      tx_id: "02".repeat(32),
      source: {
        compact_cbor: "00",
        witness_set_compact_cbor: "00",
        field_preimage_lengths_cbor: "00",
      },
      verdict,
    },
    fullTransactionCbor: Buffer.from("00", "hex"),
    membership: {} as ForcedLeafEvidence["membership"],
  }) satisfies ForcedLeafEvidence;

describe("forced-leaf evidence v1", () => {
  it("binds acceptance and rejection polarity", () => {
    expect(requireForcedLeafAccepted(evidence("ForcedTxValid"))).toBeDefined();
    expect(() =>
      requireForcedLeafRejectedFor(evidence("ForcedTxValid"), "EmptyInputs"),
    ).toThrow(/expected an explicit rejection/);
  });

  it("compares the complete typed reason including subject coordinates", () => {
    const rejected = evidence({
      ForcedTxInvalid: {
        reason: { InputNotFound: { source_kind: 0n, input_index: 3n } },
      },
    });
    expect(
      requireForcedLeafRejectedFor(rejected, {
        InputNotFound: { source_kind: 0n, input_index: 3n },
      }),
    ).toBe(rejected);
    expect(() =>
      requireForcedLeafRejectedFor(rejected, {
        InputNotFound: { source_kind: 1n, input_index: 3n },
      }),
    ).toThrow(/coordinate differs/);
    const subject = forcedLeafVerdictSubject({
      ...rejected,
      membership: {
        ...rejected.membership,
        key: { transactionId: "01".repeat(32), outputIndex: 0n },
      },
    });
    expect(subject.source_key).toBe(`d8799f5820${"01".repeat(32)}00ff`);
    expect(subject.rejection_reason).toStrictEqual(
      rejected.leaf.verdict === "ForcedTxValid"
        ? null
        : rejected.leaf.verdict.ForcedTxInvalid.reason,
    );
  });
});

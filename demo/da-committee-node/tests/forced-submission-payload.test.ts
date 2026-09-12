import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  encodeCbor,
} from "@al-ft/midgard-core/codec";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  decodeMidgardValidationTraceDescriptor,
  encodeMidgardValidationTraceDescriptor,
  hashMidgardValidationRejectionCode,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadRoots,
  decodeDaPayloadStrict,
} from "../src/da/payload.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import { makePayloadFixture } from "./helpers.js";

const fixture = async (verdict: SDK.OperatorVerdict = "ForcedTxValid") => {
  const base = await makePayloadFixture(1);
  const normalBytes = Buffer.from(
    base.payload.block_body.transaction_preimages[0]![1],
    "hex",
  );
  const normal = decodeMidgardNativeTxFullFromCanonicalCbor(normalBytes);
  const submitted = materializeMidgardForcedTxFromCanonical(normal);
  const bytes = encodeMidgardForcedTxCanonical(submitted);
  const source = deriveMidgardForcedTxProofSource(submitted);
  const orderKey = Data.to(
    { transactionId: "72".repeat(32), outputIndex: 0n },
    SDK.OutputReference,
  );
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: {
      tx_order_id: Data.from(orderKey, SDK.OutputReference),
    },
  };
  const eventCbor = Data.to(eventKey, SDK.EventKey);
  const leaf: SDK.ForcedInclusionTxV1 = {
    tx_id: computeMidgardNativeTxId(submitted).toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict,
  };
  const counts = {
    ...base.payload.block_body.counts,
    l2TransactionCount: 0n,
    forcedTransactionCount: 1n,
  };
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      counts,
      transactions: [],
      transaction_preimages: [],
      forced_transactions: [[orderKey, Data.to(leaf, SDK.ForcedInclusionTxV1)]],
      forced_transaction_preimages: [[orderKey, bytes.toString("hex")]],
      transition_trace: base.payload.block_body.transition_trace.map(
        ([key, value]) => [
          key,
          Data.to(
            {
              ...Data.from(value, SDK.TransitionStep),
              event_key: eventKey,
              phase: "ForcedTransaction",
            },
            SDK.TransitionStep,
          ),
        ],
      ),
      event_to_step: base.payload.block_body.event_to_step.map(([, value]) => [
        eventCbor,
        Data.to(
          {
            ...Data.from(value, SDK.EventToStepValue),
            phase: "ForcedTransaction",
          },
          SDK.EventToStepValue,
        ),
      ]),
      validation_traces: base.payload.block_body.validation_traces.map(
        ([, value]) => [
          eventCbor,
          encodeMidgardValidationTraceDescriptor({
            ...decodeMidgardValidationTraceDescriptor(
              Buffer.from(value, "hex"),
            ),
            verdict: verdict === "ForcedTxValid" ? "accepted" : "rejected",
            rejectionCodeHash:
              verdict === "ForcedTxValid"
                ? Buffer.alloc(32)
                : hashMidgardValidationRejectionCode("E_EMPTY_INPUTS"),
          }).toString("hex"),
        ],
      ),
    },
  };
  const roots = await computeDaPayloadRoots(payload);
  const header = { ...payload.block_body.header, ...counts, ...roots };
  payload.block_body.header = header;
  payload.block_body.header_hash = hashBlockHeader(header);
  return {
    payload,
    bytes,
    normalBytes,
    normal,
    submitted,
    source,
    leaf,
    orderKey,
    base,
  };
};
const decode = (payload: SDK.DaPayload) =>
  decodeDaPayloadStrict(SDK.encodeDaPayload(payload));

describe("forced public payload encoding", () => {
  it.each([
    "ForcedTxValid",
    { ForcedTxInvalid: { reason: "EmptyInputs" } },
  ] satisfies SDK.OperatorVerdict[])(
    "retains the exact submission under verdict %j",
    async (verdict) => {
      const f = await fixture(verdict);
      const decoded = decode(f.payload);
      expect(decoded.block_body.forced_transaction_preimages).toEqual([
        [f.orderKey, f.bytes.toString("hex")],
      ]);
      expect(
        Data.from(
          decoded.block_body.forced_transactions[0]![1],
          SDK.ForcedInclusionTxV1,
        ),
      ).toEqual(f.leaf);
    },
  );

  it("keeps normal four-element payloads admissible and distinguishes transport bytes from logical fee size", async () => {
    const f = await fixture();
    expect(decode(f.base.payload).block_body.transaction_preimages).toEqual(
      f.base.payload.block_body.transaction_preimages,
    );
    expect(f.bytes.length + 1).toBe(f.normalBytes.length);
    expect(
      SDK.minimumFeeFromProofSource({
        source: f.source,
        sourceKind: "forced",
        minFeeA: 1n,
        minFeeB: 0n,
      }).canonicalTxSize,
    ).toBe(
      SDK.minimumFeeFromProofSource({
        source: deriveMidgardNativeTxProofSource(f.normal),
        sourceKind: "normal",
        minFeeA: 1n,
        minFeeB: 0n,
      }).canonicalTxSize,
    );
  });

  it("changes the forced root when only the verdict changes, preserving the public preimage", async () => {
    const accepted = await fixture();
    const rejected = await fixture({
      ForcedTxInvalid: { reason: "EmptyInputs" },
    });
    expect(accepted.bytes).toEqual(rejected.bytes);
    expect(accepted.source).toEqual(rejected.source);
    expect(accepted.payload.block_body.header.forcedTransactionsRoot).not.toBe(
      rejected.payload.block_body.header.forcedTransactionsRoot,
    );
  });

  it("refuses obsolete normal bytes under a forced source key", async () => {
    const f = await fixture();
    f.payload.block_body.forced_transaction_preimages = [
      [f.orderKey, f.normalBytes.toString("hex")],
    ];
    expect(() => decode(f.payload)).toThrow();
  });

  it("refuses missing or duplicate forced material", async () => {
    const f = await fixture();
    const entry = f.payload.block_body.forced_transaction_preimages[0]!;
    f.payload.block_body.forced_transaction_preimages = [];
    expect(() => decode(f.payload)).toThrow();
    f.payload.block_body.forced_transaction_preimages = [entry, entry];
    expect(() => decode(f.payload)).toThrow();
  });

  it("refuses witness substitution even when the body-derived transaction ID is unchanged", async () => {
    const f = await fixture();
    const changed = materializeMidgardForcedTxFromCanonical({
      ...f.submitted,
      witnessSet: {
        ...f.submitted.witnessSet,
        addrTxWitsPreimageCbor: encodeCbor([Buffer.alloc(101)]),
      },
    });
    expect(computeMidgardNativeTxId(changed)).toEqual(
      computeMidgardNativeTxId(f.submitted),
    );
    f.payload.block_body.forced_transaction_preimages = [
      [f.orderKey, encodeMidgardForcedTxCanonical(changed).toString("hex")],
    ];
    expect(() => decode(f.payload)).toThrow();
  });
});

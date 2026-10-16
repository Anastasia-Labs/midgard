import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import {
  computeDaPayloadRoots,
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import { buildPlaceholderValidationTraceMembers } from "../src/mpf/index.js";

/**
 * A committed block whose single forced transaction carries the validation
 * descriptor exactly as the node's block builder produces it, with every root and count recomputed so only the trace
 * material is under test.
 */
const nodeTracedForcedPayload = async () => {
  const base = await makePayloadFixture(1);
  const normal = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(base.payload.block_body.transaction_preimages[0]![1], "hex"),
  );
  const submitted = materializeMidgardForcedTxFromCanonical(normal);
  const forcedBytes = encodeMidgardForcedTxCanonical(submitted);
  const source = deriveMidgardForcedTxProofSource(submitted);
  const txOrderId = { transactionId: "5a".repeat(32), outputIndex: 0n };
  const orderKey = Data.to(txOrderId, SDK.OutputReference);
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: txOrderId },
  };
  const eventCbor = Data.to(eventKey, SDK.EventKey);
  const txId = computeMidgardNativeTxId(submitted);
  const leaf: SDK.ForcedInclusionTxV1 = {
    tx_id: txId.toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason: "EmptyInputs" } },
  };
  const [member] = await Effect.runPromise(
    buildPlaceholderValidationTraceMembers({
      transactions: [
        {
          eventKey,
          canonicalTransactionCbor: forcedBytes,
          sourceKind: "forced",
          verdict: "rejected",
          rejectionReason: "EmptyInputs",
        },
      ],
    }),
  );
  if (member === undefined) throw new Error("node built no trace member");
  const counts = {
    ...base.payload.block_body.counts,
    l2TransactionCount: 0n,
    forcedTransactionCount: 1n,
  };
  const unhashed: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      counts,
      transactions: [],
      transaction_preimages: [],
      forced_transactions: [[orderKey, Data.to(leaf, SDK.ForcedInclusionTxV1)]],
      forced_transaction_preimages: [[orderKey, forcedBytes.toString("hex")]],
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
      validation_traces: [
        [member.keyCbor.toString("hex"), member.valueCbor.toString("hex")],
      ],
      validation_trace_witnesses: [],
    },
  };
  const roots = await computeDaPayloadRoots(unhashed);
  // The header pins the validation-trace pair to the empty tree; the body
  // keeps the node's member.
  const header = {
    ...unhashed.block_body.header,
    ...counts,
    ...roots,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
  };
  const headerHash = Effect.runSync(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...unhashed,
    block_body: { ...unhashed.block_body, header, header_hash: headerHash },
  };
  return { member, header, headerHash, payload };
};

describe("node-produced validation traces pass DA committee admission", () => {
  it("admits the node's placeholder descriptor with an empty retained-witness section", async () => {
    const { member, header, headerHash, payload } =
      await nodeTracedForcedPayload();
    expect(member.value.step_count).toBe(1n);
    const verified = await verifyDaPayloadAgainstHeader(
      await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" }),
      headerHash,
      header,
      {
        payloadSchemaVersion: 1,
        stateQueueOutRef: `${"00".repeat(32)}#0`,
      },
    );
    expect(verified.validation.headerHash).toBe(headerHash);
    expect(verified.counts.validationTraceCount).toBe(1n);
  });

  it("refuses a descriptor whose verdict differs from the forced leaf", async () => {
    const { payload } = await nodeTracedForcedPayload();
    const [accepted] = await Effect.runPromise(
      buildPlaceholderValidationTraceMembers({
        transactions: [
          {
            eventKey: Data.from(
              payload.block_body.validation_traces[0]![0],
              SDK.EventKey,
            ) as SDK.EventKey,
            canonicalTransactionCbor: Buffer.from(
              payload.block_body.forced_transaction_preimages[0]![1],
              "hex",
            ),
            sourceKind: "forced",
            verdict: "accepted",
            rejectionReason: null,
          },
        ],
      }),
    );
    expect(() =>
      decodeDaPayloadStrict(
        SDK.encodeDaPayload({
          ...payload,
          block_body: {
            ...payload.block_body,
            validation_traces: [
              [
                accepted!.keyCbor.toString("hex"),
                accepted!.valueCbor.toString("hex"),
              ],
            ],
          },
        }),
      ),
    ).toThrow(/verdict/u);
  });
});

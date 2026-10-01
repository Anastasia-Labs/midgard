import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import {
  computeDaPayloadRoots,
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "da-committee-node/da/payload";
import { hashBlockHeader } from "da-committee-node/l1/state-queue-scanner";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import { buildDeterministicValidationTraceMembers } from "../src/mpf/index.js";

const byKey = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

/**
 * A committed block whose single transaction source carries the validation
 * trace descriptor and retained witnesses exactly as the node's block builder
 * produces them, with every root and count recomputed so only the trace
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
    buildDeterministicValidationTraceMembers({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      blockEndTime: new Date(Number(base.header.endTime)),
      expectedNetworkId: base.header.expectedNetworkId,
      minFeeA: base.header.minFeeA,
      minFeeB: base.header.minFeeB,
      blockSlot: base.header.blockSlot,
      transactions: [
        {
          eventKey,
          transactionId: txId,
          canonicalTransactionCbor: forcedBytes,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            [],
          ),
          sourceKind: "forced",
          priorUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          postUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          ledgerOps: [],
          ledgerWitnessEntries: [],
          ledgerMutationSteps: [],
          verdict: "rejected",
          rejectionCode: RejectCodes.EmptyInputs,
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
      validation_trace_witnesses: byKey(member.witnesses),
    },
  };
  const roots = await computeDaPayloadRoots(unhashed);
  const header = { ...unhashed.block_body.header, ...counts, ...roots };
  const headerHash = hashBlockHeader(header);
  const payload: SDK.DaPayload = {
    ...unhashed,
    block_body: { ...unhashed.block_body, header, header_hash: headerHash },
  };
  return { member, header, headerHash, payload };
};

describe("node-produced validation traces pass DA committee admission", () => {
  it("admits the node's descriptor and every retained state and endpoint", async () => {
    const { member, header, headerHash, payload } =
      await nodeTracedForcedPayload();
    // The full dense retention: one chronological record per state plus the
    // initial and terminal endpoints.
    expect(member.witnesses).toHaveLength(Number(member.value.step_count) + 3);
    const verified = await verifyDaPayloadAgainstHeader(
      await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" }),
      headerHash,
      header,
      {
        payloadSchemaVersion: 1,
        stateQueueOutRef: `${"00".repeat(32)}#0`,
        preBlockUtxos: [],
      },
    );
    expect(verified.validation.headerHash).toBe(headerHash);
    expect(verified.counts.validationTraceCount).toBe(1n);
  });

  it.each([
    ["a chronological state", "state", /do not match state\.work_root/u],
    ["the initial endpoint", "initial", /context differs/u],
    ["the terminal endpoint", "terminal", /do not match state\.work_root/u],
  ] as const)(
    "refuses substituted work bytes at %s",
    async (_label, target, message) => {
      const { member, payload } = await nodeTracedForcedPayload();
      const stepCount = member.value.step_count;
      const coordinate =
        target === "state"
          ? SDK.retainedValidationStateCoordinate(stepCount, 1n)
          : SDK.retainedValidationEndpointCoordinate(stepCount, target);
      const witnesses = payload.block_body.validation_trace_witnesses.map(
        ([keyHex, valueHex]): SDK.DaPayloadEntry => {
          const key = SDK.decodeRetainedValidationWitnessKey(
            Buffer.from(keyHex, "hex"),
          );
          if (key.execution_index !== coordinate) return [keyHex, valueHex];
          const value = SDK.decodeRetainedValidationWitness(
            Buffer.from(valueHex, "hex"),
          );
          return [
            keyHex,
            SDK.encodeRetainedValidationWitness({
              ...value,
              witness_cbor: `${value.witness_cbor}00`,
            }).toString("hex"),
          ];
        },
      );
      expect(() =>
        decodeDaPayloadStrict(
          SDK.encodeDaPayload({
            ...payload,
            block_body: {
              ...payload.block_body,
              validation_trace_witnesses: witnesses,
            },
          }),
        ),
      ).toThrow(message);
    },
  );
});

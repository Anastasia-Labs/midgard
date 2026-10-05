import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { reconstructDaPayload } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-reconstruct";
import { requireStagedOneStepArgument } from "@al-ft/midgard-fault-proofs/test-support/validation-one-step-staging";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import {
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { assertPreSubmitDaPayloadSize } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import { fixture } from "./validation-trace-raw-auxiliary.fixture.js";
const boundCarriageReferences = (
  plan: ReturnType<
    typeof SDK.deriveTxOrderMaterial
  >["carriage"][number]["plan"],
  address: string,
) => {
  const certificatePolicyId = "ab".repeat(28);
  const referenceInputs: UTxO[] = plan.publications.map(
    (publication, index) => ({
      txHash: "cd".repeat(32),
      outputIndex: index,
      address,
      assets: { lovelace: 100_000_000n },
      datum: SDK.fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );
  if (plan.certificate !== null) {
    const certification = SDK.deriveFieldPreimageCertification(plan);
    referenceInputs.push({
      txHash: "bc".repeat(32),
      outputIndex: 0,
      address,
      assets: {
        lovelace: 100_000_000n,
        [certificatePolicyId + certification.assetNameHex]: 1n,
      },
      datum: certification.datumCbor,
    });
  }
  // Another reference changes the sorted positions; delivery may not freeze
  // the positions from the publication-only list.
  referenceInputs.push({
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n },
  });
  return { referenceInputs, certificatePolicyId };
};

describe(
  "raw retained auxiliary transport at supported field limits",
  { timeout: 120_000 },
  () => {
    it("keeps the actual rejected 1090-observer event below the decoded DA limit", async () => {
      const built = await fixture(1, 200, 1090);
      const retained = built.member.witnesses.map(([, value]) =>
        SDK.decodeRetainedValidationWitness(Buffer.from(value, "hex")),
      );
      const observers = retained.filter(
        (value) =>
          SDK.retainedValidationFieldSource(value.auxiliary)?.field_index ===
          3n,
      );
      const canonical = observers.filter((value) => value.phase === 0n).length;
      const phaseA = observers.filter((value) => value.phase === 6n).length;
      for (const retained of observers) {
        const source = SDK.retainedValidationFieldSource(retained.auxiliary)!;
        expect(source.total_length).toBe(32703n);
        expect(
          Data.to(
            source,
            asDataType<SDK.RetainedFieldSource>(SDK.RetainedFieldSourceSchema),
          ).length / 2,
        ).toBeLessThanOrEqual(64);
      }
      expect(canonical).toBe(1090);
      expect(phaseA).toBe(1090);
      const bytes = SDK.encodeDaPayload(built.payload);
      const fields = SDK.retainedValidationTransactionFields(
        built.forcedBytes,
        "forced",
      );
      expect(fields[3]!.length).toBe(32703);
      console.info(
        JSON.stringify({
          observerCount: 1090,
          canonical,
          phaseA,
          fieldBytes: fields[3]!.length,
          retainedWitnesses: retained.length,
          retainedTupleBytes: built.member.witnesses.reduce(
            (sum, entry) => sum + SDK.daPayloadEntryEncodedSize(entry),
            0,
          ),
          payloadBytes: bytes.length,
          limit: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
        }),
      );
      expect
        .soft(
          () => decodeDaPayloadStrict(bytes),
          "committee must admit the actual node-produced observer payload",
        )
        .not.toThrow();
      if (built.classifiedEntry === undefined)
        throw new Error("missing classified row");
      const transitionTraceMembers =
        built.payload.block_body.transition_trace.map(([key, value]) => ({
          stepIndex: Data.from(value, SDK.TransitionStep).step_index,
          keyCbor: Buffer.from(key, "hex"),
          valueCbor: Buffer.from(value, "hex"),
          value: Data.from(value, SDK.TransitionStep),
        }));
      const eventToStepMembers = built.payload.block_body.event_to_step.map(
        ([key, value]) => ({
          eventKey: Data.from(key, SDK.EventKey),
          keyCbor: Buffer.from(key, "hex"),
          valueCbor: Buffer.from(value, "hex"),
          value: Data.from(value, SDK.EventToStepValue),
        }),
      );
      const sized = await Effect.runPromise(
        Effect.either(
          assertPreSubmitDaPayloadSize({
            headerHash: built.headerHash,
            header: built.header,
            utxoPayloadAggregate: {
              entryCount: built.payload.block_body.utxos.length,
              encodedTupleBytes: built.payload.block_body.utxos.reduce(
                (sum, entry) => sum + SDK.daPayloadEntryEncodedSize(entry),
                0,
              ),
            },
            includedDepositEntries: [],
            includedForcedTransactionEntries: [built.classifiedEntry],
            includedWithdrawalEntries: [],
            processedMempoolTxs: [],
            transitionTraceMembers,
            eventToStepMembers,
            validationTraceMembers: [built.member],
            cekProgramMaterial: [],
            envelopeMode: "zstd",
          }),
        ),
      );
      expect
        .soft(
          sized._tag,
          "production pre-submit DA sizing must permit the decoded observer payload",
        )
        .toBe("Right");
      if (sized._tag === "Right") expect(sized.right).toBe(bytes.length);
      expect(
        bytes.length,
        "node-produced repeated observer fields must fit decoded 64MiB DA ceiling",
      ).toBeLessThanOrEqual(MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes);
      const envelope = await wrapDaPayload(bytes, { mode: "identity" });
      const verified = await verifyDaPayloadAgainstHeader(
        envelope,
        built.headerHash,
        built.header,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: `${"00".repeat(32)}#0`,
          preBlockUtxos: [[built.spent.toString("hex"), built.output]],
        },
      );
      expect(verified.counts.validationTraceCount).toBe(1n);
      const reconstructed = await reconstructDaPayload({
        payloadEnvelopeCbor: envelope,
        committedHeader: built.header,
      });
      expect(reconstructed.forcedTransactions[0]!.fullTransactionCbor).toEqual(
        built.forcedBytes,
      );
      expect(SDK.encodeDaPayload(decodeDaPayloadStrict(bytes))).toEqual(bytes);
      expect(
        SDK.retainedValidationTransactionFields(
          reconstructed.forcedTransactions[0]!.fullTransactionCbor,
          "forced",
        ),
      ).toEqual(fields);
    });
    it.each([
      [1, 20_000, 2],
      [318, 200, 7],
    ])(
      "exports and admits %i signatures with %i output bytes within the per-item bound without freezing delivery indices",
      async (signatureCount, outputBytes, fieldIndex) => {
        const built = await fixture(signatureCount, outputBytes);
        const bytes = SDK.encodeDaPayload(built.payload);
        expect(bytes.length).toBeLessThan(
          MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
        );
        const envelope = await wrapDaPayload(bytes, { mode: "identity" });
        const verified = await verifyDaPayloadAgainstHeader(
          envelope,
          built.headerHash,
          built.header,
          {
            payloadSchemaVersion: 1,
            stateQueueOutRef: `${"00".repeat(32)}#0`,
            preBlockUtxos: [[built.spent.toString("hex"), built.output]],
          },
        );
        expect(verified.counts.validationTraceCount).toBe(1n);
        const reconstruction = await reconstructDaPayload({
          payloadEnvelopeCbor: envelope,
          committedHeader: built.header,
        });
        expect(
          reconstruction.forcedTransactions[0]!.fullTransactionCbor,
        ).toEqual(built.forcedBytes);
        // Compare the complete canonical wire bytes without enumerating every
        // byte as an object property in the generic deep-equality matcher.
        expect(
          SDK.encodeDaPayload(decodeDaPayloadStrict(bytes)).equals(bytes),
        ).toBe(true);
        expect(
          SDK.retainedValidationTransactionFields(
            reconstruction.forcedTransactions[0]!.fullTransactionCbor,
            "forced",
          ),
        ).toEqual(
          SDK.retainedValidationTransactionFields(built.forcedBytes, "forced"),
        );
        const retained = built.member.witnesses.map(([, value]) =>
          SDK.decodeRetainedValidationWitness(Buffer.from(value, "hex")),
        );
        const selected = retained.find(
          (value) =>
            SDK.retainedValidationFieldSource(value.auxiliary)?.field_index ===
              BigInt(fieldIndex) &&
            (fieldIndex !== 7 || value.phase === 4n),
        );
        if (selected === undefined)
          throw new Error("missing retained field opening");
        const raw = SDK.retainedValidationFieldSource(selected.auxiliary)!;
        expect(raw.total_length).toBeGreaterThan(14_336n);
        const order = SDK.deriveTxOrderMaterial({
          submittedTxCbor: built.forcedBytes,
          owner: Buffer.alloc(28),
        });
        const plan = order.carriage.find(
          (field) => field.fieldIndex === fieldIndex,
        )!.plan;
        const references = boundCarriageReferences(plan, built.address);
        const materialized = SDK.materializeRetainedValidationAuxiliaryWitness({
          auxiliary: selected.auxiliary,
          transactionCommitment: Buffer.from(
            selected.machine_state.transaction_commitment,
            "hex",
          ),
          canonicalTransactionCbor: built.forcedBytes,
          sourceKind: "forced",
          plan,
          transactionId: Buffer.from(
            selected.machine_state.transaction_id,
            "hex",
          ),
          ...references,
        });
        const wire = Data.to(
          materialized as never,
          SDK.ValidationAuxiliaryWitnessSchema as never,
        );
        if (typeof materialized !== "object")
          throw new Error("materialized field source is missing");
        const resolvedCarriage =
          "TransactionFieldChunkWitness" in materialized
            ? materialized.TransactionFieldChunkWitness.carriage
            : "TransactionFieldItemWitness" in materialized
              ? materialized.TransactionFieldItemWitness.carriage
              : undefined;
        expect(resolvedCarriage).toEqual({
          Certified: {
            cert_ref_input_index: 1n,
            chunk_ref_input_indices: plan.publications.map((_, index) =>
              BigInt(index + 2),
            ),
          },
        });
        expect(() =>
          Data.from(wire, SDK.ValidationAuxiliaryWitnessSchema as never),
        ).not.toThrow();
        const successor = retained.find(
          (value) =>
            value.trace_proof.state_index ===
            selected.trace_proof.state_index + 1n,
        );
        if (successor === undefined)
          throw new Error("missing exact successor state");
        const transitionCbor = Buffer.from(
          Data.to(
            {
              work_witness_cbor: selected.witness_cbor,
              claimed_successor: successor.machine_state,
            },
            SDK.ValidationOneStepWitness,
          ),
          "hex",
        );
        const staged = requireStagedOneStepArgument({
          resolverIndex: Number(selected.phase),
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: Buffer.from(wire, "hex"),
        });
        expect(staged.auxiliaryWitness).toEqual(materialized);
        expect(staged.evidenceHash).toMatch(/^[0-9a-f]{64}$/u);
        expect(staged.transition.work_witness_cbor).toBe(selected.witness_cbor);
        expect(() =>
          SDK.materializeRetainedValidationAuxiliaryWitness({
            auxiliary: selected.auxiliary,
            transactionCommitment: Buffer.from(
              selected.machine_state.transaction_commitment,
              "hex",
            ),
            canonicalTransactionCbor: built.forcedBytes,
            sourceKind: "forced",
            plan,
            transactionId: Buffer.from(
              selected.machine_state.transaction_id,
              "hex",
            ),
            referenceInputs: [],
          }),
        ).toThrow();
        expect(() =>
          SDK.materializeRetainedValidationAuxiliaryWitness({
            auxiliary: selected.auxiliary,
            transactionCommitment: Buffer.from(
              selected.machine_state.transaction_commitment,
              "hex",
            ),
            canonicalTransactionCbor: built.forcedBytes,
            sourceKind: "forced",
            plan,
            transactionId: Buffer.from(
              selected.machine_state.transaction_id,
              "hex",
            ),
            ...references,
            referenceInputs: references.referenceInputs.map((utxo) =>
              utxo.datum === undefined
                ? utxo
                : { ...utxo, datum: Data.to("00") },
            ),
          }),
        ).toThrow();
        // Work, trace membership, source roots and exact transaction bytes stay
        // untouched: only a retained field commitment byte is substituted.
        const sourceBytes = Buffer.from(raw.commitment, "hex");
        sourceBytes[sourceBytes.length - 1] ^= 1;
        raw.commitment = sourceBytes.toString("hex");
        const targetCoordinate = selected.trace_proof.state_index;
        const tampered: SDK.DaPayload = {
          ...built.payload,
          block_body: {
            ...built.payload.block_body,
            validation_trace_witnesses: built.member.witnesses
              .map(([key, value]): SDK.DaPayloadEntry => {
                const record = SDK.decodeRetainedValidationWitness(
                  Buffer.from(value, "hex"),
                );
                return [
                  key,
                  record.trace_proof.state_index === targetCoordinate &&
                  SDK.retainedValidationFieldSource(record.auxiliary) !==
                    undefined
                    ? SDK.encodeRetainedValidationWitness(selected).toString(
                        "hex",
                      )
                    : value,
                ];
              })
              .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)),
          },
        };
        let committeeRefusal = "accepted";
        try {
          decodeDaPayloadStrict(SDK.encodeDaPayload(tampered));
        } catch (cause) {
          committeeRefusal =
            cause instanceof Error ? cause.message : String(cause);
        }
        expect.soft(committeeRefusal).toMatch(/Retained field source/u);
        const reconstructionRefusal = await reconstructDaPayload({
          payloadEnvelopeCbor: await wrapDaPayload(
            SDK.encodeDaPayload(tampered),
            { mode: "identity" },
          ),
          committedHeader: built.header,
        }).then(
          () => "accepted",
          (cause) => (cause instanceof Error ? cause.message : String(cause)),
        );
        expect.soft(reconstructionRefusal).toMatch(/Retained field source/u);
        console.info(
          JSON.stringify({
            signatureCount,
            outputBytes,
            retainedWitnesses: retained.length,
            retainedTupleBytes: built.member.witnesses.reduce(
              (sum, entry) => sum + SDK.daPayloadEntryEncodedSize(entry),
              0,
            ),
            payloadBytes: bytes.length,
            tier: plan.tier,
          }),
        );
      },
    );
  },
);

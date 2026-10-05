import { readFileSync } from "node:fs";

import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { planMidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { midgardFieldCommitment } from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  retainedValidationAuxiliaryWitnessData,
  validationAuxiliaryWitnessData,
} from "../src/validation-machine-data.js";
import { canonicalValidationAuxiliaryWitnesses } from "./fixtures/validation-auxiliary-witness-canonical.js";
import { makeNativeTx } from "./validation-fixtures.js";
const golden = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/validation-auxiliary-witness-v1.generated.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as { constructors: readonly { cbor: string }[] };

// The neighbouring golden producer pins the L1 auxiliary ABI. This case covers
// the separate bounded DA schema and its conversion back to all 41 unchanged arms.
describe("retained auxiliary raw source schema", () => {
  it.each(canonicalValidationAuxiliaryWitnesses)(
    "preserves onchain constructor %i through raw retention and late materialization",
    (tag, input) => {
      const auxiliary =
        input?.kind === "requiredSignerItem"
          ? { ...input, fieldIndex: 4 }
          : input;
      const original = Buffer.from(
        Data.to(validationAuxiliaryWitnessData(auxiliary) as never),
        "hex",
      );
      expect(original.toString("hex")).toBe(golden.constructors[tag]!.cbor);
      const rawCbor = Data.to(
        retainedValidationAuxiliaryWitnessData(auxiliary) as never,
      );
      const retained = Data.from(
        rawCbor,
        SDK.RetainedValidationAuxiliaryWitnessSchema as never,
      ) as SDK.RetainedValidationAuxiliaryWitness;
      const source = SDK.retainedValidationFieldSource(retained);
      if (source !== undefined) {
        expect(source.total_length).toBe(3n);
        expect(source.commitment).toBe(
          midgardFieldCommitment(Buffer.from("81411a", "hex")).toString("hex"),
        );
        expect(() =>
          Data.from(rawCbor, SDK.ValidationAuxiliaryWitnessSchema as never),
        ).toThrow();
        expect(() =>
          Data.from(
            original.toString("hex"),
            SDK.RetainedValidationAuxiliaryWitnessSchema as never,
          ),
        ).toThrow();
      } else expect(rawCbor).toBe(original.toString("hex"));
      // Synthetic opaque fields keep the existing L1 ABI vectors unchanged;
      // this is a schema round trip, not evidence of field admission.
      const field = Buffer.from("81411a", "hex");
      const canonicalTransactionCbor = encodeCbor([
        1n,
        [
          field,
          field,
          field,
          0n,
          -1n,
          -1n,
          field,
          field,
          field,
          Buffer.alloc(32),
          Buffer.alloc(32),
          0n,
        ],
        [field, field, field],
      ]);
      const transactionSource = SDK.retainedValidationTransactionSource(
        canonicalTransactionCbor,
        "forced",
      );
      const transactionId = transactionSource.transactionId;
      const materialized = SDK.materializeRetainedValidationAuxiliaryWitness({
        auxiliary: retained,
        transactionId,
        transactionCommitment: transactionSource.transactionCommitment,
        canonicalTransactionCbor,
        sourceKind: "forced",
        referenceInputs: [],
        ...(source === undefined
          ? {}
          : {
              plan: planMidgardFieldCarriage({
                owner: Buffer.alloc(28),
                txId: transactionId,
                fieldIndex: Number(source.field_index),
                preimage: field,
              }),
            }),
      });
      expect(
        Data.to(
          materialized as never,
          SDK.ValidationAuxiliaryWitnessSchema as never,
        ),
      ).toBe(original.toString("hex"));
    },
  );
});

describe("retained field reference authentication", () => {
  it.each(["normal", "forced"] as const)(
    "binds field identity, lengths, bytes and both transaction commitments for %s sources",
    (sourceKind) => {
      const native = makeNativeTx({ version: 1n });
      const canonicalTransactionCbor =
        sourceKind === "forced"
          ? encodeMidgardForcedTxCanonical(native.tx)
          : native.txCbor;
      const source = SDK.retainedValidationTransactionSource(
        canonicalTransactionCbor,
        sourceKind,
      );
      const field = source.fields[0]!;
      const auxiliary = Data.from(
        Data.to(
          retainedValidationAuxiliaryWitnessData({
            kind: "transactionFieldChunk",
            fieldIndex: 0,
            itemIndex: 0,
            fieldPreimage: Buffer.from(field),
          }) as never,
        ),
        SDK.RetainedValidationAuxiliaryWitnessSchema as never,
      ) as SDK.RetainedValidationAuxiliaryWitness;
      const plan = planMidgardFieldCarriage({
        owner: Buffer.alloc(28),
        txId: source.transactionId,
        fieldIndex: 0,
        preimage: field,
      });
      const context = {
        auxiliary,
        plan,
        transactionId: source.transactionId,
        transactionCommitment: source.transactionCommitment,
        canonicalTransactionCbor,
        sourceKind,
        referenceInputs: [],
      };
      const materialized =
        SDK.materializeRetainedValidationAuxiliaryWitness(context);
      expect(materialized).toEqual({
        TransactionFieldChunkWitness: {
          field_index: 0n,
          item_index: 0n,
          carriage: {
            Inline: { preimage: Buffer.from(field).toString("hex") },
          },
        },
      });
      const reference = SDK.retainedValidationFieldSource(auxiliary)!;
      for (const changed of [
        { ...reference, total_length: reference.total_length + 1n },
        { ...reference, commitment: "00".repeat(32) },
        { ...reference, commitment: "00" },
        { ...reference, field_index: 1n },
        { ...reference, field_index: -1n },
        { ...reference, field_index: 9n },
      ]) {
        const substituted: SDK.RetainedValidationAuxiliaryWitness = {
          TransactionFieldChunkWitness: {
            field_index: 0n,
            item_index: 0n,
            carriage: { raw_field_source: changed },
          },
        };
        expect(() =>
          SDK.validateRetainedValidationFieldSource(substituted, source.fields),
        ).toThrow(/Retained field source/u);
        expect(() =>
          SDK.materializeRetainedValidationAuxiliaryWitness({
            ...context,
            auxiliary: substituted,
          }),
        ).toThrow(/Retained field source/u);
      }
      expect(() =>
        SDK.materializeRetainedValidationAuxiliaryWitness({
          ...context,
          transactionId: Buffer.alloc(32),
        }),
      ).toThrow(/transaction identity/u);
      expect(() =>
        SDK.materializeRetainedValidationAuxiliaryWitness({
          ...context,
          transactionCommitment: Buffer.alloc(32),
        }),
      ).toThrow(/transaction identity/u);
      expect(() =>
        SDK.materializeRetainedValidationAuxiliaryWitness({
          ...context,
          sourceKind: sourceKind === "forced" ? "normal" : "forced",
        }),
      ).toThrow();
      expect(() =>
        SDK.materializeRetainedValidationAuxiliaryWitness({
          ...context,
          canonicalTransactionCbor: Buffer.from([0x80]),
        }),
      ).toThrow();
      const otherPlan = planMidgardFieldCarriage({
        owner: Buffer.alloc(28),
        txId: source.transactionId,
        fieldIndex: 0,
        preimage: source.fields[1]!,
      });
      expect(() =>
        SDK.materializeRetainedValidationAuxiliaryWitness({
          ...context,
          plan: otherPlan,
        }),
      ).toThrow(/carriage plan/u);
      const alternate = makeNativeTx({ version: 1n, omitVkeyWitness: true });
      const alternateCbor =
        sourceKind === "forced"
          ? encodeMidgardForcedTxCanonical(alternate.tx)
          : alternate.txCbor;
      const alternateSource = SDK.retainedValidationTransactionSource(
        alternateCbor,
        sourceKind,
      );
      expect(alternateSource.transactionId).toEqual(source.transactionId);
      expect(alternateSource.transactionCommitment).not.toEqual(
        source.transactionCommitment,
      );
      expect(() =>
        SDK.materializeRetainedValidationAuxiliaryWitness({
          ...context,
          canonicalTransactionCbor: alternateCbor,
        }),
      ).toThrow(/transaction identity/u);
      const wrongSigner: SDK.RetainedValidationAuxiliaryWitness = {
        RequiredSignerItemWitness: {
          carriage: { raw_field_source: { ...reference, field_index: 0n } },
          signer_proof: "NoSignerSetProof",
        },
      };
      expect(() => SDK.retainedValidationFieldSource(wrongSigner)).toThrow(
        /must be field 4/u,
      );
    },
  );
});

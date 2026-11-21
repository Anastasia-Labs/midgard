import "./validation-machine.semantic-resolver-definitions.js";

import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  type DeterministicValidationMachineTrace,
  purposeKindForRedeemerTag,
  redeemerPointerMatchesPurpose,
  redeemerTagForPurposeKind,
  type ValidationMachineFieldCarriagePlanInput,
  type ValidationMachineWorkWitness,
} from "../src/index.js";

describe("V1 purpose-kind to redeemer-pointer mapping", () => {
  it("matches the exhaustive canonical vector and rejects adjacent values", () => {
    const canonical = [
      { purposeKind: 0, redeemerTag: 0 },
      { purposeKind: 1, redeemerTag: 1 },
      { purposeKind: 2, redeemerTag: 3 },
      { purposeKind: 3, redeemerTag: 6 },
    ] as const;

    for (const { purposeKind, redeemerTag } of canonical) {
      expect(redeemerTagForPurposeKind(purposeKind)).toBe(redeemerTag);
      expect(purposeKindForRedeemerTag(redeemerTag)).toBe(purposeKind);
      expect(
        redeemerPointerMatchesPurpose({
          purposeKind,
          purposeIndex: 7n,
          redeemerTag,
          redeemerIndex: 7n,
        }),
      ).toBe(true);
      expect(
        redeemerPointerMatchesPurpose({
          purposeKind,
          purposeIndex: 7n,
          redeemerTag,
          redeemerIndex: 8n,
        }),
      ).toBe(false);
    }

    for (const purposeKind of [-1, 4]) {
      expect(redeemerTagForPurposeKind(purposeKind)).toBeNull();
      expect(
        redeemerPointerMatchesPurpose({
          purposeKind,
          purposeIndex: 7n,
          redeemerTag: 0,
          redeemerIndex: 7n,
        }),
      ).toBe(false);
    }
    for (const redeemerTag of [-1, 2, 4, 5, 7]) {
      expect(purposeKindForRedeemerTag(redeemerTag)).toBeNull();
      expect(
        redeemerPointerMatchesPurpose({
          purposeKind: 0,
          purposeIndex: 7n,
          redeemerTag,
          redeemerIndex: 7n,
        }),
      ).toBe(false);
    }
  });
});

/**
 * The field index a `canonicalDecode` step is reading.
 *
 * #597: `TransactionFieldItemWitness` carries only a `FieldCarriageV1` — the
 * phase takes both the field index and the item index from its own control, so
 * the auxiliary does not repeat them. The control is position 4 of the step's
 * work-witness array, which is where the machine writes it, so reading it here
 * asks the same question the on-chain step does.
 */
export const canonicalDecodeFieldIndex = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = decodeSingleCbor(witness.cbor);
  if (!Array.isArray(control) || control.length !== 9) {
    throw new Error("canonicalDecode control must contain nine fields");
  }
  return Number(control[4] as bigint);
};

/**
 * The carriage plan input a field-reading step carries for one field (#600).
 *
 * This replaces a `carriage: { carriage: "Inline" }` assertion, and it is
 * stronger than what it replaces rather than weaker. A step no longer names a
 * tier at all — §8.4's partition is applied at evidence commitment, where a
 * transaction exists to resolve reference inputs against — so "the producer
 * chose tier 1" is no longer a property of the trace to assert. What is worth
 * pinning is the thing the arm now means: the step read the **real** field, byte
 * for byte, out of the transaction under test.
 */
export const expectedFieldPlanInput = (
  txCbor: Buffer,
  fieldIndex: number,
): { readonly fieldIndex: number; readonly fieldPreimage: Buffer } => ({
  fieldIndex,
  fieldPreimage:
    deriveMidgardTxFieldPreimages(txCbor)[fieldIndex]!.preimageCbor,
});

/**
 * The §5.1 preimage a field-reading step read.
 *
 * Since #600 a step carries the carriage **plan input**, so this is a field read
 * rather than a tier assertion: the tier is chosen later, at evidence
 * commitment, and these rows are about which bytes a step named.
 */
export const stepFieldPreimage = (
  planInput: ValidationMachineFieldCarriagePlanInput,
): Buffer => planInput.fieldPreimage;

type MintFoldWitness = Extract<
  NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
  {
    readonly kind: "transactionFieldChunk" | "mintFoldAsset";
  }
>;

export const collectMintFoldWitnesses = (
  trace: DeterministicValidationMachineTrace,
): readonly MintFoldWitness[] =>
  trace.witnesses
    .filter((witness) => witness.phase === "scriptSources")
    .map((witness) => witness.auxiliary)
    .filter(
      (auxiliary): auxiliary is MintFoldWitness =>
        auxiliary?.kind === "mintFoldAsset" ||
        (auxiliary?.kind === "transactionFieldChunk" &&
          auxiliary.fieldIndex === 5),
    );

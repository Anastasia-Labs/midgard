import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/narrowing";
import "@al-ft/midgard-test-support/hex";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "vitest";
import "../src/index.js";
import "../src/validation-machine-data.js";
import "./validation-tail-controls-abi.tail-auxiliary-vectors.js";
import "./validation-tail-controls-abi.decode-exact-terminal-witness.js";

import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core";
import {
  decodeSingleCbor,
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import { fixtureBytes } from "@al-ft/midgard-test-support/hex";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeValidationAuxiliaryWitnessCbor,
  validationAuxiliaryWitnessData,
} from "../src/validation-machine-data.js";
import {
  decodeExactTerminalWitness,
  EXPECTED,
  terminalRejectionCbor,
} from "./validation-tail-controls-abi.decode-exact-terminal-witness.js";
import {
  bytes,
  decodeExactTailAuxiliary,
  digest,
  rejectionReasonCbor,
  tailAuxiliaryVectors,
  terminalAcceptanceCbor,
  valueAccumulatorCbor,
  valueAndMintControlCbor,
} from "./validation-tail-controls-abi.tail-auxiliary-vectors.js";

describe("canonical V1 validation tail controls", () => {
  it("freezes every V15/V16 auxiliary tag and arity with one corpus hash", () => {
    const corpus = Buffer.from(
      Data.to(
        tailAuxiliaryVectors.map(([, , value]) =>
          validationAuxiliaryWitnessData(value),
        ) as never,
      ),
      "hex",
    );
    for (const [tag, arity, value] of tailAuxiliaryVectors) {
      const cbor = encodeValidationAuxiliaryWitnessCbor(value);
      const decoded = decodeExactTailAuxiliary(cbor.toString("hex"));
      expect([decoded.index, decoded.fields.length]).toEqual([tag, arity]);
    }
    expect(digest(corpus)).toBe(EXPECTED.auxiliaryCorpusHash);

    expect(() => decodeExactTailAuxiliary(Data.to(new Constr(23, [])))).toThrow(
      /tag/u,
    );
    expect(() => decodeExactTailAuxiliary(Data.to(new Constr(36, [])))).toThrow(
      /tag/u,
    );
    expect(() =>
      decodeExactTailAuxiliary(Data.to(new Constr(24, [0n]))),
    ).toThrow(/arity/u);
  });

  it("freezes V15 accumulator and 12-field value-and-mint control bytes", () => {
    const accumulator = decodeSingleCbor(valueAccumulatorCbor);
    const control = decodeSingleCbor(valueAndMintControlCbor);
    expect(Array.isArray(accumulator) ? accumulator.length : -1).toBe(4);
    expect(Array.isArray(control) ? control.length : -1).toBe(12);
    expect(
      Array.isArray(control)
        ? (decodeSingleCbor(control[0] as Uint8Array) as unknown[]).length
        : -1,
    ).toBe(26);
    expect({
      valueAccumulatorCbor: valueAccumulatorCbor.toString("hex"),
      valueAndMintControlHash: digest(valueAndMintControlCbor),
    }).toEqual({
      valueAccumulatorCbor: EXPECTED.valueAccumulatorCbor,
      valueAndMintControlHash: EXPECTED.valueAndMintControlHash,
    });
  });

  it("freezes V17 accepted/rejected witnesses and rejects misclassification", () => {
    expect(decodeExactTerminalWitness(terminalAcceptanceCbor)).toEqual({
      outcome: "accepted",
    });
    expect(decodeExactTerminalWitness(terminalRejectionCbor)).toEqual({
      outcome: "rejected",
      ledgerRoot: fixtureBytes(0x73, 32),
    });
    expect({
      terminalAcceptanceCbor: terminalAcceptanceCbor.toString("hex"),
      terminalAcceptanceHash: hashMidgardValidationWorkWitness({
        phase: "terminal",
        programCounter: 9,
        witnessCbor: terminalAcceptanceCbor,
      }).toString("hex"),
      terminalRejectionCbor: terminalRejectionCbor.toString("hex"),
      terminalRejectionHash: hashMidgardValidationWorkWitness({
        phase: "terminal",
        programCounter: 9,
        witnessCbor: terminalRejectionCbor,
      }).toString("hex"),
    }).toEqual({
      terminalAcceptanceCbor: EXPECTED.terminalAcceptanceCbor,
      terminalAcceptanceHash: EXPECTED.terminalAcceptanceHash,
      terminalRejectionCbor: EXPECTED.terminalRejectionCbor,
      terminalRejectionHash: EXPECTED.terminalRejectionHash,
    });

    const terminal = (
      outcome: bigint,
      slot: Buffer,
      ledgerRoot: Buffer,
      evidence: Buffer,
    ): Buffer =>
      encodeCborArrayRaw([
        encodeCborInteger(outcome),
        slot,
        encodeCborBytes(ledgerRoot),
        encodeCborBytes(evidence),
      ]);
    const emptySlot = encodeCborBytes(Buffer.alloc(0));
    // Each refused witness differs from an exact one in a single position.
    expect(
      decodeExactTerminalWitness(
        terminal(2n, rejectionReasonCbor, fixtureBytes(0x73, 32), bytes("80")),
      ).outcome,
    ).toBe("rejected");
    for (const refused of [
      // An acceptance carrying a rejection reason.
      encodeCborArrayRaw([encodeCborInteger(1n), rejectionReasonCbor]),
      // An acceptance in the four-field rejection shape.
      terminal(1n, emptySlot, fixtureBytes(0x73, 32), bytes("80")),
      // An acceptance carrying a non-empty payload.
      encodeCborArrayRaw([encodeCborInteger(1n), encodeCborBytes(bytes("00"))]),
      // A rejection without a reason.
      terminal(2n, emptySlot, fixtureBytes(0x73, 32), bytes("80")),
      // A rejection naming its reason as the legacy label bytes.
      terminal(
        2n,
        encodeCborBytes(Buffer.from("E_VALUE_NOT_PRESERVED", "ascii")),
        fixtureBytes(0x73, 32),
        bytes("80"),
      ),
      // A rejection in the two-field acceptance shape.
      encodeCborArrayRaw([encodeCborInteger(2n), emptySlot]),
      // An outcome outside V1.
      terminal(3n, emptySlot, fixtureBytes(0x73, 32), bytes("80")),
    ]) {
      expect(() => decodeExactTerminalWitness(refused)).toThrow();
    }
  });
});

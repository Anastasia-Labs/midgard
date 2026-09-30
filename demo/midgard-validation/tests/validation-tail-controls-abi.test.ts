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

import {
  buildMidgardValidationLedgerDeltaFrontier,
  commitMidgardValidationMerkleFrontier,
  encodeCbor,
  hashMidgardValidationLedgerDelta,
  hashMidgardValidationLedgerDeltaOperation,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
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
  acceptanceFrontierCbor,
  bytes,
  decodeExactTailAuxiliary,
  descriptor,
  digest,
  ledgerDeltaControlCbor,
  pendingMutationCbor,
  rejectionCode,
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
    expect(() => decodeExactTailAuxiliary(Data.to(new Constr(40, [])))).toThrow(
      /tag/u,
    );
    expect(() =>
      decodeExactTailAuxiliary(Data.to(new Constr(27, [0n]))),
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

  it("freezes V16 pending mutation/control shapes and operation roots", () => {
    const pending = decodeSingleCbor(pendingMutationCbor);
    const control = decodeSingleCbor(ledgerDeltaControlCbor);
    expect(Array.isArray(pending) ? pending.length : -1).toBe(10);
    expect(Array.isArray(control) ? control.length : -1).toBe(14);
    expect({
      pendingMutationCbor: pendingMutationCbor.toString("hex"),
      ledgerDeltaControlHash: digest(ledgerDeltaControlCbor),
    }).toEqual({
      pendingMutationCbor: EXPECTED.pendingMutationCbor,
      ledgerDeltaControlHash: EXPECTED.ledgerDeltaControlHash,
    });

    const deletion = {
      type: "delete" as const,
      key: bytes("010203"),
      proofDescriptor: descriptor,
    };
    const insertion = {
      type: "insert" as const,
      key: bytes("0405"),
      value: bytes("060708"),
      proofDescriptor: descriptor,
    };
    const frontier = buildMidgardValidationLedgerDeltaFrontier([
      deletion,
      insertion,
    ]);
    expect(
      hashMidgardValidationLedgerDeltaOperation(deletion).toString("hex"),
    ).toBe("d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589");
    expect(
      hashMidgardValidationLedgerDeltaOperation(insertion).toString("hex"),
    ).toBe("f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db");
    expect(
      commitMidgardValidationMerkleFrontier(frontier).toString("hex"),
    ).toBe("b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2");
    expect(hashMidgardValidationLedgerDelta([deletion, insertion])).toEqual(
      commitMidgardValidationMerkleFrontier(frontier),
    );
  });

  it("freezes V17 accepted/rejected witnesses and rejects misclassification", () => {
    expect(decodeExactTerminalWitness(terminalAcceptanceCbor)).toEqual({
      outcome: "accepted",
      ledgerRoot: fixtureBytes(0x72, 32),
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

    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([
          1n,
          rejectionCode,
          fixtureBytes(0x72, 32),
          acceptanceFrontierCbor,
        ]),
      ),
    ).toThrow(/cannot carry a rejection/u);
    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([2n, Buffer.alloc(0), fixtureBytes(0x73, 32), bytes("80")]),
      ),
    ).toThrow(/misclassified/u);
    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([
          2n,
          rejectionCode,
          fixtureBytes(0x73, 32),
          acceptanceFrontierCbor,
        ]),
      ),
    ).toThrow(/misclassified/u);
    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([3n, Buffer.alloc(0), fixtureBytes(0x73, 32), bytes("80")]),
      ),
    ).toThrow(/outcome/u);
  });
});

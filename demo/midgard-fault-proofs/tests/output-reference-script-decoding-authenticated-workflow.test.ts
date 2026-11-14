import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  bindOutputReferenceScriptDecodingReferenceScripts,
  OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
  type OutputReferenceScriptDecodingDeploymentBinding,
  type OutputReferenceScriptDecodingReferenceScripts,
  outputReferenceScriptDecodingStageFromL1,
} from "../src/output-reference-script-decoding/index.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});
const utxo = (byte: string, outputIndex: number): UTxO => ({
  txHash: byte.repeat(64),
  outputIndex,
  address: "addr_test1vr0resolvedoutput",
  assets: { lovelace: 2_000_000n },
  scriptRef: script(byte),
});
const references = (): OutputReferenceScriptDecodingReferenceScripts => ({
  step01: utxo("1", 0),
  step02: utxo("2", 1),
  step03: utxo("3", 2),
  step04: utxo("4", 3),
  step05: utxo("5", 4),
  step06: utxo("6", 5),
  fieldPreimageCertificateMint: utxo("7", 6),
  fieldGrammarCertificateSpend: utxo("b", 10),
  witnesses: {
    computationThreadMint: utxo("8", 7),
    fraudProofMint: utxo("9", 8),
    phasMembershipWithdraw: utxo("a", 9),
  },
});

describe("outputReferenceScriptDecoding production workflow", () => {
  it("names the complete manifest roles", () => {
    expect(
      Object.values(OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS),
    ).toEqual([
      "fraudProofOutputReferenceScriptDecoding",
      "fraudProofOutputReferenceScriptDecodingStep02",
      "fraudProofOutputReferenceScriptDecodingStep03",
      "fraudProofOutputReferenceScriptDecodingStep04",
      "fraudProofOutputReferenceScriptDecodingStep05",
      "fraudProofOutputReferenceScriptDecodingStep06",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "fieldPreimageCertificateMint",
      "fieldGrammarCertificateSpend",
    ]);
  });

  it("authenticates every reference out-ref and refuses substitution", () => {
    const supplied = references();
    const names = Object.values(
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
    );
    const values = [
      supplied.step01,
      supplied.step02,
      supplied.step03,
      supplied.step04,
      supplied.step05,
      supplied.step06,
      supplied.witnesses.computationThreadMint,
      supplied.witnesses.fraudProofMint,
      supplied.witnesses.phasMembershipWithdraw,
      supplied.fieldPreimageCertificateMint,
      supplied.fieldGrammarCertificateSpend,
    ];
    const binding = {
      referenceScriptsByContract: Object.fromEntries(
        names.map((name, index) => [
          name,
          {
            outRef: `${values[index]!.txHash}#${values[index]!.outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(values[index]!.scriptRef!),
          },
        ]),
      ),
    } as unknown as OutputReferenceScriptDecodingDeploymentBinding;
    expect(
      bindOutputReferenceScriptDecodingReferenceScripts({
        binding,
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
    expect(() =>
      bindOutputReferenceScriptDecodingReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step06: { ...supplied.step06, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
  });

  it("maps all six authenticated raw-L1 physical steps", () => {
    expect(
      [1, 2, 3, 4, 5, 6].map((step) =>
        outputReferenceScriptDecodingStageFromL1({
          kind: "step",
          step,
        } as never),
      ),
    ).toEqual([
      "step01",
      "step02",
      "outputScan",
      "referenceBind",
      "scan",
      "step06",
    ]);
    expect(() =>
      outputReferenceScriptDecodingStageFromL1({
        kind: "step",
        step: 7,
      } as never),
    ).toThrow(/exceeds six-step topology/u);
  });
});

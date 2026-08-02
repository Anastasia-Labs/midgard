import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  commitMidgardScriptContextTxInInfoV1,
  commitMidgardScriptContextTxOutV1,
} from "../src/script-context-proof.js";

const OUTPUT_CBOR =
  "a400581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" +
  "01821a0012d687a1581c111111111111111111111111111111111111111111111" +
  "11111111111a1422233070246d87b9f182aff03820343010203";
const INPUT_CBOR = `825820${"bb".repeat(32)}02`;

describe("V1 script-context output commitments", () => {
  it("matches the Aiken Cardano and Midgard address-view roots", () => {
    const output = decodeMidgardTxOutput(Buffer.from(OUTPUT_CBOR, "hex"));
    expect(encodeMidgardTxOutput(output).toString("hex")).toBe(OUTPUT_CBOR);

    const cardano = commitMidgardScriptContextTxOutV1(output, "cardano");
    const midgard = commitMidgardScriptContextTxOutV1(output, "midgard");
    expect(Buffer.from(cardano.root).toString("hex")).toBe(
      "e1649a619efea4d8319405dae3455e5add966785c25f269e83fd0f15352693a7",
    );
    expect(Buffer.from(midgard.root).toString("hex")).toBe(
      "65951e5653516caf350b2cd750bab349fbd1a48811a4c278859dd825868c2d32",
    );
    expect(cardano.cborLength).toBe(133n);
    expect(cardano.memory).toBe(169n);
    expect(midgard.cborLength).toBe(cardano.cborLength);
    expect(midgard.memory).toBe(cardano.memory);
  });

  it("does not impose a separate value cap inside the bounded output", () => {
    const output: MidgardTxOutput = {
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0xaa)]),
      value: {
        lovelace: 2_000_000n,
        assets: new Map([
          [
            "11".repeat(28),
            new Map(
              Array.from({ length: 96 }, (_, index) => [
                index.toString(16).padStart(16, "0"),
                BigInt(index + 1),
              ]),
            ),
          ],
        ]),
      },
    };
    const outputCbor = encodeMidgardTxOutput(output);
    expect(outputCbor.length).toBeLessThanOrEqual(4_095);

    const commitment = commitMidgardScriptContextTxOutV1(output, "cardano");
    const maximumStructuralPreimage = Math.max(
      ...[...commitment.dataNodes.values()].map(
        ({ preimage }) => preimage.length,
      ),
      ...[...commitment.listNodes.values()].map(
        ({ preimage }) => preimage.length,
      ),
      ...[...commitment.pairNodes.values()].map(
        ({ preimage }) => preimage.length,
      ),
    );
    expect(maximumStructuralPreimage).toBeLessThan(256);
    expect(commitment.cborLength).toBeGreaterThan(1_024n);
  });

  it("matches the Aiken TxInInfo roots from bounded input and output preimages", () => {
    const output = decodeMidgardTxOutput(Buffer.from(OUTPUT_CBOR, "hex"));
    const cardano = commitMidgardScriptContextTxInInfoV1(
      INPUT_CBOR,
      output,
      "cardano",
    );
    const midgard = commitMidgardScriptContextTxInInfoV1(
      INPUT_CBOR,
      output,
      "midgard",
    );
    expect(Buffer.from(cardano.root).toString("hex")).toBe(
      "66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029",
    );
    expect(Buffer.from(midgard.root).toString("hex")).toBe(
      "097c6693964ba9a7448e88f4e73fcaff44b7e41663d4cd3ba2b52ff3afbc3f8f",
    );
    expect(cardano.cborLength).toBe(176n);
    expect(cardano.memory).toBe(218n);
    expect(midgard.cborLength).toBe(cardano.cborLength);
    expect(midgard.memory).toBe(cardano.memory);
  });
});

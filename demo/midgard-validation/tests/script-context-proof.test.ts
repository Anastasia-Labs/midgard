import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  commitMidgardScriptContextTxInInfo,
  commitMidgardScriptContextTxOut,
} from "../src/script-context-proof.js";

const OUTPUT_CBOR =
  "a400581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" +
  "01821a0012d687a1581c111111111111111111111111111111111111111111111" +
  "11111111111a1422233070246d87b9f182aff03820343010203";
// §5.3 fields 0/1: the out-ref is the fixed-index item
// `82 ‖ 58 20 tx_id ‖ 19 index_be16`, so index 2 is `190002`, never `02`.
const INPUT_CBOR = `825820${"bb".repeat(32)}190002`;

describe("V1 script-context output commitments", () => {
  it("matches the Aiken Cardano and Midgard address-view roots", () => {
    const output = decodeMidgardTxOutput(Buffer.from(OUTPUT_CBOR, "hex"));
    expect(encodeMidgardTxOutput(output).toString("hex")).toBe(OUTPUT_CBOR);

    const cardano = commitMidgardScriptContextTxOut(output, "cardano");
    const midgard = commitMidgardScriptContextTxOut(output, "midgard");
    expect(Buffer.from(cardano.root).toString("hex")).toBe(
      "9a798cedaf0cebe75f5254e541a33d035eb24d281a387c878b60bdbc41c53c98",
    );
    expect(Buffer.from(midgard.root).toString("hex")).toBe(
      "83ec6cf75d0b365e4ffb51c25ee65faf7da53b7d6d21381c97aa651bba903085",
    );
    expect(cardano.cborLength).toBe(133n);
    expect(cardano.memory).toBe(94n);
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

    const commitment = commitMidgardScriptContextTxOut(output, "cardano");
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
    const cardano = commitMidgardScriptContextTxInInfo(
      INPUT_CBOR,
      output,
      "cardano",
    );
    const midgard = commitMidgardScriptContextTxInInfo(
      INPUT_CBOR,
      output,
      "midgard",
    );
    expect(Buffer.from(cardano.root).toString("hex")).toBe(
      "a2be4f66f683151c175713c30809198758c71485bbd424f4acf903e692b733fa",
    );
    expect(Buffer.from(midgard.root).toString("hex")).toBe(
      "23ef033204c588b0f8c3903697e3790624a900fa373260b71497a61dbda7c970",
    );
    expect(cardano.cborLength).toBe(176n);
    expect(cardano.memory).toBe(115n);
    expect(midgard.cborLength).toBe(cardano.cborLength);
    expect(midgard.memory).toBe(cardano.memory);
  });
});

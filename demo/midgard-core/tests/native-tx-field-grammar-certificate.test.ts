import { describe, expect, it } from "vitest";

import {
  deriveMidgardFieldGrammarCertificate,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldGrammarCheckpoint,
  encodeMidgardFieldPreimage,
  MIDGARD_FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME,
  midgardFieldGrammarCheckpointHash,
  midgardFieldGrammarWaypoints,
  walkMidgardFieldGrammar,
} from "../src/codec/native-tx-field-access.js";

/**
 * The values below are the golden vectors of
 * `onchain/aiken/validators/field-grammar-certificate-handlers.test.ak`
 * (`field_grammar_certificate_handlers_datums_match_the_golden_vectors`): the field is that
 * module's `varied_field(2, 40)`, whose item `k` is `1 + (7k mod 40)` bytes of
 * `c3`, and the validator accepted every datum they are taken from.
 */
const variedItems = (count: number): Buffer[] =>
  Array.from({ length: count }, (_, k) =>
    Buffer.alloc(1 + ((7 * k) % 40), 0xc3),
  );

const GOLDEN_TX_ID = Buffer.from(
  "752b97ec14d1474f7eb9e94550874b87f5de218887a8a15c6fa21cafcf46f62f",
  "hex",
);
const GOLDEN_FIELD_HASH =
  "bf0c0a2fd31be6b03dfefea796a476a46c60f84b61927c9e47c7c8a12f00e877";
const GOLDEN_CHECKPOINT_AFTER_16 =
  "875820752b97ec14d1474f7eb9e94550874b87f5de218887a8a15c6fa21cafcf46f62f41025820bf0c0a2fd31be6b03dfefea796a476a46c60f84b61927c9e47c7c8a12f00e8774300036f430000284300001043000140";
const GOLDEN_CHECKPOINT_HASH_AFTER_16 =
  "dc00c1d7362c137bbe4822f55dbe3a4c287c5e1a113ad30d565833229d69c242";
const OWNER = Buffer.alloc(28, 0x66);

describe("field grammar certificate mirror", () => {
  const preimage = encodeMidgardFieldPreimage(variedItems(40));
  const grammar = walkMidgardFieldGrammar(2, preimage);

  it("walks the count and every wrapper offset", () => {
    expect(grammar.itemCount).toBe(40);
    expect(grammar.totalLength).toBe(preimage.length);
    expect(grammar.fieldHash.toString("hex")).toBe(GOLDEN_FIELD_HASH);
    let offset = encodeMidgardFieldArrayHeader(40).length;
    variedItems(40).forEach((item, k) => {
      expect(grammar.offsets[k]).toBe(offset);
      offset += encodeMidgardDefiniteBytes(item).length;
    });
    expect(grammar.offsets[40]).toBe(preimage.length);
  });

  it("matches the validator's checkpoint and waypoints", () => {
    const checkpoint = encodeMidgardFieldGrammarCheckpoint(
      GOLDEN_TX_ID,
      grammar,
      16,
    );
    expect(checkpoint.toString("hex")).toBe(GOLDEN_CHECKPOINT_AFTER_16);
    expect(midgardFieldGrammarCheckpointHash(checkpoint).toString("hex")).toBe(
      GOLDEN_CHECKPOINT_HASH_AFTER_16,
    );
    expect(midgardFieldGrammarWaypoints(grammar, 16).toString("hex")).toBe(
      "0002",
    );
    expect(midgardFieldGrammarWaypoints(grammar, 40).toString("hex")).toBe(
      "00020140029e",
    );
  });

  it("derives InProgress before the end and Complete at it", () => {
    const opened = deriveMidgardFieldGrammarCertificate({
      owner: OWNER,
      txId: GOLDEN_TX_ID,
      grammar,
      next: 16,
    });
    expect(opened.kind).toBe("InProgress");
    if (opened.kind === "InProgress")
      expect(opened.grammarCheckpointHash.toString("hex")).toBe(
        GOLDEN_CHECKPOINT_HASH_AFTER_16,
      );
    const complete = deriveMidgardFieldGrammarCertificate({
      owner: OWNER,
      txId: GOLDEN_TX_ID,
      grammar,
      next: 40,
    });
    expect(complete).toMatchObject({ kind: "Complete", itemCount: 40 });
    expect(MIDGARD_FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME.toString("ascii")).toBe(
      "MIDGARD_FIELD_GRAMMAR_CERT",
    );
  });

  it("refuses what the validator refuses", () => {
    const outputs = encodeMidgardFieldPreimage(variedItems(20));
    expect(() =>
      walkMidgardFieldGrammar(2, Buffer.concat([outputs, Buffer.from([0x40])])),
    ).toThrow();
    expect(() =>
      walkMidgardFieldGrammar(
        2,
        Buffer.concat([encodeMidgardFieldArrayHeader(21), outputs.subarray(1)]),
      ),
    ).toThrow();
    expect(() =>
      walkMidgardFieldGrammar(
        2,
        Buffer.concat([encodeMidgardFieldArrayHeader(19), outputs.subarray(1)]),
      ),
    ).toThrow();
    expect(() => walkMidgardFieldGrammar(0, outputs)).toThrow();
    expect(() => walkMidgardFieldGrammar(7, outputs)).toThrow();
    expect(
      walkMidgardFieldGrammar(6, encodeMidgardFieldPreimage([Buffer.from([1])]))
        .itemCount,
    ).toBe(1);
  });
});

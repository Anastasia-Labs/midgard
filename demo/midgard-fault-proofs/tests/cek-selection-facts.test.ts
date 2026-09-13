import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { deriveCekSelectionFacts } from "@al-ft/midgard-sdk";
import { buildMidgardCanonicalCekProgram } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

const material = () => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from("010100200101", "hex"),
  );
  return {
    envelopeCbor: program.envelopeCbor,
    programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]),
  };
};

describe("CEK selection partition facts", () => {
  it("derives empty facts only for native execution", () => {
    const facts = deriveCekSelectionFacts();
    expect(facts.envelope.term_root).toBe("");
    expect(facts.envelope.node_count).toBe(0n);
    expect(facts.material.data_roots).toEqual([]);
  });
  it("accounts for every authenticated material node and byte", () => {
    const facts = deriveCekSelectionFacts(material());
    expect(facts.envelope.term_root).toHaveLength(64);
    expect(
      facts.material.program_node_count + facts.material.data_node_count,
    ).toBe(facts.envelope.node_count);
    expect(
      facts.material.program_byte_length + facts.material.data_byte_length,
    ).toBe(facts.envelope.material_byte_length);
  });
  it("refuses missing material and changed envelopes", () => {
    const source = material();
    expect(() =>
      deriveCekSelectionFacts({
        ...source,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      }),
    ).toThrow();
    expect(() =>
      deriveCekSelectionFacts({
        ...source,
        envelopeCbor: Buffer.concat([source.envelopeCbor, Buffer.from([0])]),
      }),
    ).toThrow();
  });
});

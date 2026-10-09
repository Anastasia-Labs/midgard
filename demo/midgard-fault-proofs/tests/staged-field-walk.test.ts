import { encodeMidgardVersionedScript } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  advanceFieldGrammarCheckpoint,
  advanceFieldSemanticCheckpoint,
  decodeFieldGrammarCheckpoint,
  decodeFieldSemanticCheckpoint,
  encodeFieldGrammarCheckpoint,
  encodeFieldSemanticCheckpoint,
  fieldGrammarCheckpointIsComplete,
  fieldSemanticCheckpointIsComplete,
  hashFieldGrammarCheckpoint,
  hashFieldSemanticCheckpoint,
  initialFieldGrammarCheckpoint,
  initialFieldSemanticCheckpoint,
  resolveFieldGrammarCheckpoint,
  resolveFieldSemanticCheckpoint,
} from "../src/staged-field-walk/index.js";

const TX_ID = "11".repeat(32);
const scripts = Array.from({ length: 65 }, (_, index) => ({
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from([index]),
}));
const items = scripts.map(encodeMidgardVersionedScript);

describe("staged field-walk checkpoint twins", () => {
  it("uses the exact fixed-width grammar wire and reaches terminal in bounded batches", () => {
    const initial = initialFieldGrammarCheckpoint({
      txId: TX_ID,
      items,
    });
    const first = advanceFieldGrammarCheckpoint({
      checkpoint: initial,
      items,
      budget: 32,
    });
    const second = advanceFieldGrammarCheckpoint({
      checkpoint: first,
      items,
      budget: 32,
    });
    const terminal = advanceFieldGrammarCheckpoint({
      checkpoint: second,
      items,
      budget: 32,
    });

    const encoded = encodeFieldGrammarCheckpoint(terminal);
    expect(encoded).toHaveLength(87);
    expect(decodeFieldGrammarCheckpoint(encoded)).toEqual(terminal);
    expect(first.nextItemIndex).toBe(32);
    expect(second.nextItemIndex).toBe(64);
    expect(terminal.nextItemIndex).toBe(65);
    expect(fieldGrammarCheckpointIsComplete(terminal)).toBe(true);
    expect(hashFieldGrammarCheckpoint(terminal)).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      resolveFieldGrammarCheckpoint({
        txId: TX_ID,
        items,
        committedHash: hashFieldGrammarCheckpoint(second),
      }),
    ).toEqual(second);
  });

  it("derives semantic checkpoints only from terminal grammar and advances in bounded batches", () => {
    const initialGrammar = initialFieldGrammarCheckpoint({
      txId: TX_ID,
      items,
    });
    expect(() =>
      initialFieldSemanticCheckpoint({
        grammar: initialGrammar,
        items,
      }),
    ).toThrow(/terminal grammar/u);
    const terminalGrammar = advanceFieldGrammarCheckpoint({
      checkpoint: initialGrammar,
      items,
      budget: 32,
    });
    const terminalGrammar2 = advanceFieldGrammarCheckpoint({
      checkpoint: terminalGrammar,
      items,
      budget: 32,
    });
    const terminalGrammar3 = advanceFieldGrammarCheckpoint({
      checkpoint: terminalGrammar2,
      items,
      budget: 32,
    });
    const semantic = initialFieldSemanticCheckpoint({
      grammar: terminalGrammar3,
      items,
    });
    const first = advanceFieldSemanticCheckpoint({
      checkpoint: semantic,
      txId: TX_ID,
      items,
      budget: 32,
    });
    const second = advanceFieldSemanticCheckpoint({
      checkpoint: first,
      txId: TX_ID,
      items,
      budget: 32,
    });
    expect(first.nextItemIndex).toBe(32);
    expect(second.nextItemIndex).toBe(64);
    const terminal = advanceFieldSemanticCheckpoint({
      checkpoint: second,
      txId: TX_ID,
      items,
      budget: 32,
    });
    const encoded = encodeFieldSemanticCheckpoint(terminal);
    expect(encoded).toHaveLength(53);
    expect(decodeFieldSemanticCheckpoint(encoded)).toEqual(terminal);
    expect(fieldSemanticCheckpointIsComplete(terminal)).toBe(true);
    expect(hashFieldSemanticCheckpoint(terminal)).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      resolveFieldSemanticCheckpoint({
        txId: TX_ID,
        items,
        committedHash: hashFieldSemanticCheckpoint(second),
      }),
    ).toEqual(second);
  });

  it("rejects noncanonical, substituted, and out-of-range checkpoints", () => {
    const grammar = initialFieldGrammarCheckpoint({
      txId: TX_ID,
      items,
    });
    const encoded = encodeFieldGrammarCheckpoint(grammar);
    const forged = Buffer.from(encoded);
    forged[0] = 0x86;
    expect(() => decodeFieldGrammarCheckpoint(forged)).toThrow(
      /not canonical/u,
    );
    expect(() =>
      advanceFieldGrammarCheckpoint({
        checkpoint: { ...grammar, fieldCommitment: "22".repeat(32) },
        items,
        budget: 32,
      }),
    ).toThrow(/exact field preimage/u);
    expect(() =>
      advanceFieldGrammarCheckpoint({
        checkpoint: grammar,
        items,
        budget: 33,
      }),
    ).toThrow(/1\.\.32/u);
  });
});

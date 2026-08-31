import {
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  advanceMissingNativeScriptTxGrammarCheckpointV1,
  advanceMissingNativeScriptTxSemanticCheckpointV1,
  decodeMissingNativeScriptTxGrammarCheckpointV1,
  decodeMissingNativeScriptTxSemanticCheckpointV1,
  encodeMissingNativeScriptTxGrammarCheckpointV1,
  encodeMissingNativeScriptTxSemanticCheckpointV1,
  hashMissingNativeScriptTxGrammarCheckpointV1,
  hashMissingNativeScriptTxSemanticCheckpointV1,
  initialMissingNativeScriptTxGrammarCheckpointV1,
  initialMissingNativeScriptTxSemanticCheckpointV1,
  missingNativeScriptTxGrammarCheckpointIsCompleteV1,
  missingNativeScriptTxRequiredScriptPresentThroughV1,
  missingNativeScriptTxSemanticCheckpointIsCompleteV1,
  resolveMissingNativeScriptTxGrammarCheckpointV1,
  resolveMissingNativeScriptTxSemanticCheckpointV1,
} from "../src/missing-native-script-tx/staged-walk-v1.js";

const TX_ID = "11".repeat(32);
const scripts = Array.from({ length: 65 }, (_, index) => ({
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from([index]),
}));
const items = scripts.map(encodeMidgardVersionedScript);

describe("missing-native-script staged checkpoint twins", () => {
  it("uses the exact fixed-width grammar wire and reaches terminal in bounded batches", () => {
    const initial = initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: TX_ID,
      items,
    });
    const first = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: initial,
      items,
      budget: 32,
    });
    const second = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: first,
      items,
      budget: 32,
    });
    const terminal = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: second,
      items,
      budget: 32,
    });

    const encoded = encodeMissingNativeScriptTxGrammarCheckpointV1(terminal);
    expect(encoded).toHaveLength(87);
    expect(decodeMissingNativeScriptTxGrammarCheckpointV1(encoded)).toEqual(
      terminal,
    );
    expect(first.nextItemIndex).toBe(32);
    expect(second.nextItemIndex).toBe(64);
    expect(terminal.nextItemIndex).toBe(65);
    expect(missingNativeScriptTxGrammarCheckpointIsCompleteV1(terminal)).toBe(
      true,
    );
    expect(hashMissingNativeScriptTxGrammarCheckpointV1(terminal)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(
      resolveMissingNativeScriptTxGrammarCheckpointV1({
        txId: TX_ID,
        items,
        committedHash: hashMissingNativeScriptTxGrammarCheckpointV1(second),
      }),
    ).toEqual(second);
  });

  it("derives semantic checkpoints only from terminal grammar and finds a required script cumulatively", () => {
    const initialGrammar = initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: TX_ID,
      items,
    });
    expect(() =>
      initialMissingNativeScriptTxSemanticCheckpointV1({
        grammar: initialGrammar,
        items,
      }),
    ).toThrow(/terminal grammar/u);
    const terminalGrammar = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: initialGrammar,
      items,
      budget: 32,
    });
    const terminalGrammar2 = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: terminalGrammar,
      items,
      budget: 32,
    });
    const terminalGrammar3 = advanceMissingNativeScriptTxGrammarCheckpointV1({
      checkpoint: terminalGrammar2,
      items,
      budget: 32,
    });
    const semantic = initialMissingNativeScriptTxSemanticCheckpointV1({
      grammar: terminalGrammar3,
      items,
    });
    const first = advanceMissingNativeScriptTxSemanticCheckpointV1({
      checkpoint: semantic,
      txId: TX_ID,
      items,
      budget: 32,
    });
    const targetHash = hashMidgardVersionedScript(scripts[40]!);
    expect(
      missingNativeScriptTxRequiredScriptPresentThroughV1({
        expectedScriptHash: targetHash,
        items,
        nextItemIndex: first.nextItemIndex,
      }),
    ).toBe(false);
    const second = advanceMissingNativeScriptTxSemanticCheckpointV1({
      checkpoint: first,
      txId: TX_ID,
      items,
      budget: 32,
    });
    expect(
      missingNativeScriptTxRequiredScriptPresentThroughV1({
        expectedScriptHash: targetHash,
        items,
        nextItemIndex: second.nextItemIndex,
      }),
    ).toBe(true);
    const terminal = advanceMissingNativeScriptTxSemanticCheckpointV1({
      checkpoint: second,
      txId: TX_ID,
      items,
      budget: 32,
    });
    const encoded = encodeMissingNativeScriptTxSemanticCheckpointV1(terminal);
    expect(encoded).toHaveLength(53);
    expect(decodeMissingNativeScriptTxSemanticCheckpointV1(encoded)).toEqual(
      terminal,
    );
    expect(missingNativeScriptTxSemanticCheckpointIsCompleteV1(terminal)).toBe(
      true,
    );
    expect(hashMissingNativeScriptTxSemanticCheckpointV1(terminal)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(
      resolveMissingNativeScriptTxSemanticCheckpointV1({
        txId: TX_ID,
        items,
        committedHash: hashMissingNativeScriptTxSemanticCheckpointV1(second),
      }),
    ).toEqual(second);
  });

  it("rejects noncanonical, substituted, and out-of-range checkpoints", () => {
    const grammar = initialMissingNativeScriptTxGrammarCheckpointV1({
      txId: TX_ID,
      items,
    });
    const encoded = encodeMissingNativeScriptTxGrammarCheckpointV1(grammar);
    const forged = Buffer.from(encoded);
    forged[0] = 0x86;
    expect(() =>
      decodeMissingNativeScriptTxGrammarCheckpointV1(forged),
    ).toThrow(/not canonical/u);
    expect(() =>
      advanceMissingNativeScriptTxGrammarCheckpointV1({
        checkpoint: { ...grammar, fieldCommitment: "22".repeat(32) },
        items,
        budget: 32,
      }),
    ).toThrow(/exact field preimage/u);
    expect(() =>
      advanceMissingNativeScriptTxGrammarCheckpointV1({
        checkpoint: grammar,
        items,
        budget: 33,
      }),
    ).toThrow(/1\.\.32/u);
  });
});

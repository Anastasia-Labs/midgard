import {
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  advanceMissingNativeScriptTxSemanticCheckpoint,
  decodeMissingNativeScriptTxGrammarCheckpoint,
  decodeMissingNativeScriptTxSemanticCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxSemanticCheckpoint,
  hashMissingNativeScriptTxGrammarCheckpoint,
  hashMissingNativeScriptTxSemanticCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxSemanticCheckpoint,
  missingNativeScriptTxGrammarCheckpointIsComplete,
  missingNativeScriptTxRequiredScriptPresentThrough,
  missingNativeScriptTxSemanticCheckpointIsComplete,
  resolveMissingNativeScriptTxGrammarCheckpoint,
  resolveMissingNativeScriptTxSemanticCheckpoint,
} from "../src/missing-native-script-tx/staged-walk-v1.js";

const TX_ID = "11".repeat(32);
const scripts = Array.from({ length: 65 }, (_, index) => ({
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from([index]),
}));
const items = scripts.map(encodeMidgardVersionedScript);

describe("missing-native-script staged checkpoint twins", () => {
  it("uses the exact fixed-width grammar wire and reaches terminal in bounded batches", () => {
    const initial = initialMissingNativeScriptTxGrammarCheckpoint({
      txId: TX_ID,
      items,
    });
    const first = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: initial,
      items,
      budget: 32,
    });
    const second = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: first,
      items,
      budget: 32,
    });
    const terminal = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: second,
      items,
      budget: 32,
    });

    const encoded = encodeMissingNativeScriptTxGrammarCheckpoint(terminal);
    expect(encoded).toHaveLength(87);
    expect(decodeMissingNativeScriptTxGrammarCheckpoint(encoded)).toEqual(
      terminal,
    );
    expect(first.nextItemIndex).toBe(32);
    expect(second.nextItemIndex).toBe(64);
    expect(terminal.nextItemIndex).toBe(65);
    expect(missingNativeScriptTxGrammarCheckpointIsComplete(terminal)).toBe(
      true,
    );
    expect(hashMissingNativeScriptTxGrammarCheckpoint(terminal)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(
      resolveMissingNativeScriptTxGrammarCheckpoint({
        txId: TX_ID,
        items,
        committedHash: hashMissingNativeScriptTxGrammarCheckpoint(second),
      }),
    ).toEqual(second);
  });

  it("derives semantic checkpoints only from terminal grammar and finds a required script cumulatively", () => {
    const initialGrammar = initialMissingNativeScriptTxGrammarCheckpoint({
      txId: TX_ID,
      items,
    });
    expect(() =>
      initialMissingNativeScriptTxSemanticCheckpoint({
        grammar: initialGrammar,
        items,
      }),
    ).toThrow(/terminal grammar/u);
    const terminalGrammar = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: initialGrammar,
      items,
      budget: 32,
    });
    const terminalGrammar2 = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: terminalGrammar,
      items,
      budget: 32,
    });
    const terminalGrammar3 = advanceMissingNativeScriptTxGrammarCheckpoint({
      checkpoint: terminalGrammar2,
      items,
      budget: 32,
    });
    const semantic = initialMissingNativeScriptTxSemanticCheckpoint({
      grammar: terminalGrammar3,
      items,
    });
    const first = advanceMissingNativeScriptTxSemanticCheckpoint({
      checkpoint: semantic,
      txId: TX_ID,
      items,
      budget: 32,
    });
    const targetHash = hashMidgardVersionedScript(scripts[40]!);
    expect(
      missingNativeScriptTxRequiredScriptPresentThrough({
        expectedScriptHash: targetHash,
        items,
        nextItemIndex: first.nextItemIndex,
      }),
    ).toBe(false);
    const second = advanceMissingNativeScriptTxSemanticCheckpoint({
      checkpoint: first,
      txId: TX_ID,
      items,
      budget: 32,
    });
    expect(
      missingNativeScriptTxRequiredScriptPresentThrough({
        expectedScriptHash: targetHash,
        items,
        nextItemIndex: second.nextItemIndex,
      }),
    ).toBe(true);
    const terminal = advanceMissingNativeScriptTxSemanticCheckpoint({
      checkpoint: second,
      txId: TX_ID,
      items,
      budget: 32,
    });
    const encoded = encodeMissingNativeScriptTxSemanticCheckpoint(terminal);
    expect(encoded).toHaveLength(53);
    expect(decodeMissingNativeScriptTxSemanticCheckpoint(encoded)).toEqual(
      terminal,
    );
    expect(missingNativeScriptTxSemanticCheckpointIsComplete(terminal)).toBe(
      true,
    );
    expect(hashMissingNativeScriptTxSemanticCheckpoint(terminal)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(
      resolveMissingNativeScriptTxSemanticCheckpoint({
        txId: TX_ID,
        items,
        committedHash: hashMissingNativeScriptTxSemanticCheckpoint(second),
      }),
    ).toEqual(second);
  });

  it("rejects noncanonical, substituted, and out-of-range checkpoints", () => {
    const grammar = initialMissingNativeScriptTxGrammarCheckpoint({
      txId: TX_ID,
      items,
    });
    const encoded = encodeMissingNativeScriptTxGrammarCheckpoint(grammar);
    const forged = Buffer.from(encoded);
    forged[0] = 0x86;
    expect(() => decodeMissingNativeScriptTxGrammarCheckpoint(forged)).toThrow(
      /not canonical/u,
    );
    expect(() =>
      advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: { ...grammar, fieldCommitment: "22".repeat(32) },
        items,
        budget: 32,
      }),
    ).toThrow(/exact field preimage/u);
    expect(() =>
      advanceMissingNativeScriptTxGrammarCheckpoint({
        checkpoint: grammar,
        items,
        budget: 33,
      }),
    ).toThrow(/1\.\.32/u);
  });
});

import { describe, expect, it, vi } from "vitest";

import { createProviderBackedEvaluator } from "../src/services/provider-backed-evaluator.js";

describe("provider-backed evaluator", () => {
  it("delegates the complete transaction and additional UTxOs", async () => {
    const result = [
      {
        ex_units: { mem: 1, steps: 2 },
        redeemer_index: 0,
        redeemer_tag: "mint" as const,
      },
    ];
    const evaluateTx = vi.fn(async () => result);
    const evaluator = createProviderBackedEvaluator(evaluateTx);
    const input = {
      tx: "84a0",
      additionalUTxOs: [],
      context: {} as never,
    };

    await expect(evaluator.evaluate(input)).resolves.toEqual(result);
    expect(evaluateTx).toHaveBeenCalledExactlyOnceWith(
      input.tx,
      input.additionalUTxOs,
    );
    expect(evaluator.name).toBe("local-ogmios-cardano-node");
  });
});

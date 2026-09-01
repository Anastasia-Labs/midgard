import { describe, expect, it, vi } from "vitest";

import { createProviderBackedEvaluator } from "../src/l1/provider-backed-evaluator.js";

describe("provider-backed evaluator", () => {
  it("evaluates through Ogmios without a synthesized additional UTxO set", async () => {
    let capturedInit: RequestInit | undefined;
    const fetchImpl: typeof fetch = vi.fn(async (_input, init) => {
      capturedInit = init;
      return new Response(
        JSON.stringify({
          jsonrpc: "2.0",
          method: "evaluateTransaction",
          result: [
            {
              validator: { purpose: "mint", index: 0 },
              budget: { memory: 1, cpu: 2 },
            },
          ],
          id: "midgard-da-local-evaluation",
        }),
        { status: 200, headers: { "content-type": "application/json" } },
      );
    });
    const evaluator = createProviderBackedEvaluator(
      "http://127.0.0.1:1337",
      fetchImpl,
    );
    const input = {
      tx: "84a0",
      additionalUTxOs: [
        {
          txHash: "01".repeat(32),
          outputIndex: 0,
          address: "addr_test1vp",
          assets: { lovelace: 5_000_000n },
        },
      ],
      context: {} as never,
    };

    await expect(evaluator.evaluate(input)).resolves.toEqual([
      {
        ex_units: { mem: 1, steps: 2 },
        redeemer_index: 0,
        redeemer_tag: "mint",
      },
    ]);
    expect(fetchImpl).toHaveBeenCalledOnce();
    const body = JSON.parse(String(capturedInit?.body)) as Record<
      string,
      unknown
    >;
    expect(body).toEqual({
      jsonrpc: "2.0",
      method: "evaluateTransaction",
      params: { transaction: { cbor: input.tx } },
      id: "midgard-da-local-evaluation",
    });
    expect(evaluator.name).toBe("local-ogmios-cardano-node");
  });

  it("preserves an Ogmios JSON-RPC failure", async () => {
    const evaluator = createProviderBackedEvaluator(
      "http://127.0.0.1:1337",
      async () =>
        new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            error: { code: 3010, message: "Script execution failed" },
            id: "midgard-da-local-evaluation",
          }),
          { status: 400, headers: { "content-type": "application/json" } },
        ),
    );

    await expect(
      evaluator.evaluate({ tx: "84a0", additionalUTxOs: [], context: {} as never }),
    ).rejects.toThrow("Ogmios JSON-RPC error 3010: Script execution failed");
  });
});

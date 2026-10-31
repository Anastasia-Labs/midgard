import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  describeErrorChain,
  errorChainLinks,
  errorChainTexts,
} from "./error-chain.js";

describe("error chain", () => {
  it("reads through an Effect FiberFailure to the failure inside", async () => {
    const failure = await Effect.runPromise(
      Effect.fail(new Error("inner OutsideValidityInterval")),
    ).catch((error: unknown) => error);
    expect(errorChainTexts(failure).join(" | ")).toContain(
      "inner OutsideValidityInterval",
    );
    expect(errorChainLinks(failure)).toHaveLength(2);
  });

  it("follows causes and aggregate members, each link once", () => {
    const inner = Object.assign(new Error("RejectTx"), {
      data: { currentSlot: 3193 },
    });
    const outer = new AggregateError(
      [new Error("first", { cause: inner }), inner],
      "both failed",
    );
    expect(errorChainTexts(outer)).toEqual([
      "both failed",
      "first",
      'RejectTx {"currentSlot":3193}',
    ]);
    expect(describeErrorChain(new Error("a", { cause: new Error("b") }))).toBe(
      "a <- b",
    );
  });

  it("never throws on data it cannot stringify", () => {
    const cyclic: Record<string, unknown> = { amount: 1n };
    cyclic["self"] = cyclic;
    const texts = errorChainTexts(
      Object.assign(new Error("odd data"), { data: cyclic }),
    );
    expect(texts).toHaveLength(1);
    expect(texts[0]).toMatch(/^odd data /u);
    expect(
      errorChainTexts(Object.assign(new Error("big"), { data: { n: 2n } })),
    ).toEqual(['big {"n":"2"}']);
  });
});

import { describe, expect, it } from "vitest";

import { categoryLabel } from "../src/runtime.category-label.js";

describe("categoryLabel", () => {
  it("spells a category name in kebab case", () => {
    expect(categoryLabel("l2TxMistag")).toBe("l2-tx-mistag");
  });

  it("keeps the shortened label of the input-no-index family", () => {
    expect(categoryLabel("nonExistentInputNoIndex")).toBe("input-no-idx");
  });
});

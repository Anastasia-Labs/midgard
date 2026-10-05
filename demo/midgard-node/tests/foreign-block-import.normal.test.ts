import { describe, expect, it } from "vitest";

import { normalFixture } from "./foreign-block-import.normal-fixture.js";

describe("normal foreign event identity", () => {
  it("imports an honest signed normal transfer", async () => {
    const f = await normalFixture();
    expect(f.sourceKey).toBe(f.transactionId);
    expect((await f.imported())._tag).toBe("Right");
  });
  it("refuses a wrong normal source key even when every DA and semantic commitment is regenerated", async () => {
    const f = await normalFixture(true);
    expect(f.sourceKey).not.toBe(f.transactionId);
    const result = await f.imported();
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "invalid",
        detail: expect.stringContaining(
          "foreign normal transaction source key differs from canonical transaction id",
        ),
      });
  });
});

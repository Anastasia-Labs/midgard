import { describe, expect, it } from "vitest";

import { normalFixture } from "./foreign-block-import.normal-fixture.js";

describe("normal foreign event identity", () => {
  it("imports an honest signed normal transfer", async () => {
    const f = await normalFixture();
    expect(f.sourceKey).toBe(f.transactionId);
    const result = await f.imported();
    if (result._tag === "Left") throw result.left;
    expect(result._tag).toBe("Right");
  });
  it("imports a PlutusV3 spend under its placeholder descriptor with no retained witnesses", async () => {
    const f = await normalFixture(false, "plutus");
    expect(f.payload.block_body.cek_program_material.length).toBeGreaterThan(0);
    expect(f.payload.block_body.validation_trace_witnesses).toEqual([]);
    expect((await f.imported())._tag).toBe("Right");
  });
  it("waits for missing reachable CEK material", async () => {
    const f = await normalFixture(false, "plutus");
    f.payload.block_body.cek_program_material = [];
    const result = await f.imported();
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "missing",
        detail: expect.stringContaining("material"),
      });
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

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { withBuiltValidationTracePair } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";

describe("normal foreign event identity", () => {
  it("imports an honest signed normal transfer", async () => {
    const f = await normalFixture();
    expect(f.sourceKey).toBe(f.transactionId);
    const result = await f.imported();
    if (result._tag === "Left") throw result.left;
    expect(result._tag).toBe("Right");
  });
  it("imports an honest block with a transaction whose header pins the validation-trace pair to the empty tree", async () => {
    const f = await normalFixture();
    const { header, counts, validation_traces } = f.payload.block_body;
    expect(header.validationTracesRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(header.validationTraceCount).toBe(0n);
    expect(header.l2TransactionCount).toBe(1n);
    expect(counts.validationTraceCount).toBe(1n);
    expect(validation_traces).toHaveLength(1);
    const result = await f.imported();
    if (result._tag === "Left") throw result.left;
    expect(result._tag).toBe("Right");
  });
  it("refuses a block whose header commits its built validation-trace pair", async () => {
    const f = await normalFixture();
    const result = await f.imported(
      await withBuiltValidationTracePair(f.payload),
    );
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "invalid",
        detail: expect.stringMatching(
          /^foreign DA commitment mismatch: validation_traces_root,validationTraceCount$/,
        ),
      });
  });
  it("imports a PlutusV3 spend under its placeholder descriptor with no retained witnesses", async () => {
    const f = await normalFixture(false, "plutus");
    expect(Object.keys(f.payload.block_body)).not.toContain(
      "cek_program_material",
    );
    expect(f.payload.block_body.validation_trace_witnesses).toEqual([]);
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

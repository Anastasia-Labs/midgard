import { describe, expect, it } from "vitest";

import { deriveFraudProofRawL1FamilyStage } from "../src/workflow/index.js";
import {
  fixture,
  releaseEconomics,
} from "./support/raw-l1-terminal-fixture.js";

const derive = (value: Awaited<ReturnType<typeof fixture>>) =>
  deriveFraudProofRawL1FamilyStage({
    snapshot: value.snapshot,
    definition: value.definition,
    releaseEconomics,
  });

describe("raw L1 family bond slash fee", () => {
  it.each([
    [{ partial: true }, "800000000", "400000000"],
    [{ partial: true, bondPadding: 1n }, "800000001", "400000001"],
    [{ bondPadding: 1n }, "900000001", "500000000"],
    [{ bondPadding: 900_000_000n }, "1800000000", "500000000"],
  ])(
    "accepts the slash of a bond (%o) holding %s at fee %s",
    async (options, bond, fee) => {
      await expect(derive(await fixture(options))).resolves.toMatchObject({
        kind: "removed",
        terminal: {
          economics: {
            operatorBondInputLovelace: bond,
            slashedLovelace: fee,
            removalFeeLovelace: fee,
          },
        },
      });
    },
  );

  it.each([
    { partial: true, bondPadding: -1n },
    { bondPadding: 1n, feeDelta: 1n },
    { bondPadding: 1n, feeDelta: -1n },
    { partial: true, bondPadding: 1n, feeDelta: -1n },
    { partial: true, bondPadding: 1n, feeDelta: 1n },
  ])("rejects a slash fee off the slash of its bond (%o)", async (options) => {
    await expect(derive(await fixture(options))).rejects.toThrow(
      /release-bound slash of its bond/u,
    );
  });
});

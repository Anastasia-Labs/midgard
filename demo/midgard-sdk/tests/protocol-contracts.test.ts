import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  buildDaParamsGovernorValidator,
  parseFaultProofBlueprint,
} from "../src/index.js";

const blueprint = parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      "utf8",
    ),
  ) as unknown,
);

describe("public protocol deployment recipes", () => {
  it.each([
    { txHash: "ab", outputIndex: 0 },
    { txHash: "ab".repeat(32), outputIndex: -1 },
  ])("refuses malformed governor initialization references: %j", (outRef) => {
    expect(() =>
      buildDaParamsGovernorValidator(blueprint, "Preprod", outRef, 256, 16),
    ).toThrow();
  });
});

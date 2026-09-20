import { describe, expect, it } from "vitest";

import {
  applyBlueprintParams,
  getUnappliedScript,
  parseFaultProofBlueprint,
} from "../src/index.js";

const entry = {
  title: "example.main.spend",
  compiledCode: "49480100002221200101",
};

describe("blueprint construction boundary", () => {
  it("refuses duplicate titles through both deployment doors", () => {
    const blueprint = parseFaultProofBlueprint({ validators: [entry, entry] });
    expect(() => getUnappliedScript(blueprint, entry.title)).toThrow(
      /exactly one/,
    );
    expect(() => applyBlueprintParams(blueprint, entry.title, [])).toThrow(
      /exactly one/,
    );
  });
});

it("preserves declared type metadata when a normalized blueprint crosses an adapter", () => {
  const raw = {
    validators: [
      {
        ...entry,
        parameters: [
          {
            title: "policy",
            schema: { $ref: "#/definitions/cardano~1assets~1PolicyId" },
          },
        ],
      },
    ],
  };
  const parsed = parseFaultProofBlueprint(raw);
  expect(parseFaultProofBlueprint(parsed)).toEqual(parsed);
  expect(() =>
    applyBlueprintParams(parseFaultProofBlueprint(parsed), entry.title, ["ab"]),
  ).toThrow(/28-byte hash/);
});

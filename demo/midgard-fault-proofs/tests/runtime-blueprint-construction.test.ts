import { applyParamsToScript } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  applyBlueprintParamsExact,
  getCompiledScript,
  measureBlueprintValidatorBytes,
} from "../src/runtime.js";

const title = "example.main.spend";
const compiledCode = "49480100002221200101";
const entry = {
  title,
  compiledCode,
  parameters: [
    {
      title: "policy",
      schema: { $ref: "#/definitions/cardano~1assets~1PolicyId" },
    },
  ],
};
const blueprint = { validators: [entry] };
const policy = "ab".repeat(28);

describe("runtime blueprint construction", () => {
  it("preserves application bytes while keeping raw measurement separate from deployment", () => {
    expect(
      applyBlueprintParamsExact({ blueprint, title, params: [policy] }),
    ).toBe(applyParamsToScript(compiledCode, [policy]));
    expect(
      measureBlueprintValidatorBytes({
        blueprint,
        title,
        expectedDeclaredParameterCount: 1,
      }),
    ).toBe(10);
    expect(() => getCompiledScript(blueprint, title)).toThrow(
      /declares 1 parameter/,
    );
    expect(() =>
      measureBlueprintValidatorBytes({
        blueprint,
        title,
        expectedDeclaredParameterCount: 0,
      }),
    ).toThrow(/measured invariant/);
    expect(
      getCompiledScript({ validators: [{ title, compiledCode }] }, title),
    ).toBe(compiledCode);
  });

  it.each([{ params: [] }, { params: [policy, policy] }])(
    "refuses incorrect arity $params",
    ({ params }) => {
      expect(() =>
        applyBlueprintParamsExact({ blueprint, title, params }),
      ).toThrow(/declares 1 parameter/);
    },
  );

  it("refuses malformed parameter shapes", () => {
    expect(() =>
      applyBlueprintParamsExact({ blueprint, title, params: ["ab"] }),
    ).toThrow(/28-byte hash/);
  });

  it.each([{ validators: [] }, { validators: [entry, entry] }])(
    "refuses missing or duplicate titles at every runtime door",
    ({ validators }) => {
      const malformed = { validators };
      expect(() =>
        applyBlueprintParamsExact({
          blueprint: malformed,
          title,
          params: [policy],
        }),
      ).toThrow();
      expect(() => getCompiledScript(malformed, title)).toThrow();
      expect(() =>
        measureBlueprintValidatorBytes({
          blueprint: malformed,
          title,
          expectedDeclaredParameterCount: 1,
        }),
      ).toThrow();
    },
  );
});

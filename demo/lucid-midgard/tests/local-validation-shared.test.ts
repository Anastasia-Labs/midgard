import { existsSync, readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const sourceText = (relativeUrl: string): string =>
  readFileSync(fileURLToPath(new URL(relativeUrl, import.meta.url)), "utf8");

describe("shared local validation source", () => {
  it("keeps lucid-midgard local preflight wired to @al-ft/midgard-validation", () => {
    const builderSource = sourceText("../src/builder.ts");

    expect(builderSource).toContain('from "@al-ft/midgard-validation"');
    expect(builderSource).toContain("runPhaseAValidation");
    expect(builderSource).toContain("runPhaseBValidationWithPatch");
    expect(builderSource).not.toMatch(
      /(?:const|function|export\s+const|export\s+function)\s+runPhaseAValidation\b/,
    );
    expect(builderSource).not.toMatch(
      /(?:const|function|export\s+const|export\s+function)\s+runPhaseBValidation\b/,
    );
  });

  it("does not keep node validation compatibility shims", () => {
    const removedShimPaths = [
      "../../midgard-node/src/validation/index.ts",
      "../../midgard-node/src/validation/phase-a.ts",
      "../../midgard-node/src/validation/phase-b.ts",
      "../../midgard-node/src/validation/types.ts",
      "../../midgard-node/src/validation/local-script-eval.ts",
      "../../midgard-node/src/validation/midgard-redeemers.ts",
      "../../midgard-node/src/validation/script-context.ts",
      "../../midgard-node/src/validation/script-source.ts",
    ] as const;

    for (const path of removedShimPaths) {
      expect(existsSync(fileURLToPath(new URL(path, import.meta.url)))).toBe(
        false,
      );
    }
  });
});

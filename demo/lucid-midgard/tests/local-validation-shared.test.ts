import { readFileSync } from "node:fs";
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

  it("keeps node Phase A and Phase B modules as shared-validation shims", () => {
    const shims = [
      ["../../midgard-node/src/validation/index.ts", "@al-ft/midgard-validation"],
      [
        "../../midgard-node/src/validation/phase-a.ts",
        "@al-ft/midgard-validation/phase-a",
      ],
      [
        "../../midgard-node/src/validation/phase-b.ts",
        "@al-ft/midgard-validation/phase-b",
      ],
      [
        "../../midgard-node/src/validation/types.ts",
        "@al-ft/midgard-validation/types",
      ],
      [
        "../../midgard-node/src/validation/local-script-eval.ts",
        "@al-ft/midgard-validation/local-script-eval",
      ],
      [
        "../../midgard-node/src/validation/midgard-output.ts",
        "@al-ft/midgard-validation/midgard-output",
      ],
      [
        "../../midgard-node/src/validation/midgard-redeemers.ts",
        "@al-ft/midgard-validation/midgard-redeemers",
      ],
      [
        "../../midgard-node/src/validation/script-context.ts",
        "@al-ft/midgard-validation/script-context",
      ],
      [
        "../../midgard-node/src/validation/script-source.ts",
        "@al-ft/midgard-validation/script-source",
      ],
    ] as const;

    for (const [path, specifier] of shims) {
      expect(sourceText(path).trim()).toBe(`export * from "${specifier}";`);
    }
  });
});

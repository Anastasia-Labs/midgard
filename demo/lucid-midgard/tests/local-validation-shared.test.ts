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
});

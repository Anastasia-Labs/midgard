import { describe, expect, it } from "vitest";

import {
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";
import { GAME_CENSUS_STEPS } from "./support/validation-game-census-steps.js";
import {
  censusVariants,
  uncoveredValidationTraceSpends,
  validationTraceSpends,
} from "./support/validation-trace-census.js";

describe("validation-trace step census", () => {
  for (const step of GAME_CENSUS_STEPS) {
    for (const variant of censusVariants(step)) {
      it(variant.name, variant.run);
    }
  }

  it("covers every step it builds, and reports the rest of the blueprint as uncovered", () => {
    const blueprint = readBlueprint(realBlueprintPath);
    const spends = validationTraceSpends(blueprint);
    const uncovered = uncoveredValidationTraceSpends(
      blueprint,
      GAME_CENSUS_STEPS,
    );
    const covered = spends.filter((title) => !uncovered.includes(title));
    expect(covered.length).toBeGreaterThan(0);
    expect(new Set(covered)).toEqual(
      new Set(GAME_CENSUS_STEPS.map((step) => step.title)),
    );
    expect(uncovered.length + covered.length).toBe(spends.length);
  });
});

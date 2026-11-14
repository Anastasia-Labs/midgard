import { expect, it } from "vitest";

import { driveDirectHandoffRegression } from "./support/direct-handoff-regressions.js";

it.each(["input", "witness", "receive"] as const)(
  "refuses premature %s handoff after authentic progress",
  async (family) => {
    expect(
      await driveDirectHandoffRegression({ family, empty: false }),
    ).toMatchObject({ accepted: family === "receive" ? 2 : 1, refused: 1 });
  },
  300_000,
);

it.each(["input", "witness", "receive"] as const)(
  "preserves authentic empty %s field handoff",
  async (family) => {
    const result = await driveDirectHandoffRegression({ family, empty: true });
    expect(result.accepted).toBeGreaterThanOrEqual(3);
    expect(result.refused).toBe(family === "receive" ? 2 : 0);
  },
  300_000,
);

it.each(["unrelated", "none"] as const)(
  "pins receive target choice against %s selection",
  async (selectionRefusal) => {
    expect(
      await driveDirectHandoffRegression({
        family: "receive",
        empty: false,
        selection: true,
        selectionRefusal,
      }),
    ).toMatchObject({ accepted: 5, refused: 1 });
  },
  300_000,
);

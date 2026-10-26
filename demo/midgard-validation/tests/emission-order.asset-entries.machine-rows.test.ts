import { describe, it } from "vitest";

import { ASSET_ENTRY_ROWS } from "./emission-order.asset-entries.rows.js";
import { expectCountedRowVerdicts } from "./emission-order.counted-steps.js";

/** The rows of case (f); see `emission-order.asset-entries.rows.ts`. */
describe("emission order (f): the asset bound counts repeated entries", () => {
  it.each(ASSET_ENTRY_ROWS)("$name", expectCountedRowVerdicts, 180_000);
});

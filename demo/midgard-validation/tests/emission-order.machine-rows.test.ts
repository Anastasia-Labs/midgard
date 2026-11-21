import { describe, it } from "vitest";

import { ORDER_ROWS } from "./emission-order.rows.js";
import {
  expectControlledRows,
  expectRowVerdicts,
} from "./emission-order.support.js";

/** The rows of cases (b), (c) and (d); see `emission-order.rows.ts`. */
describe("emission order: producer, trace builder and one-step agree on the first fault", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(ORDER_ROWS, ["b_", "c_", "d_"]);
  });

  it.each(ORDER_ROWS)("$name", expectRowVerdicts, 180_000);
});

import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  ABSENT_KEYS,
  bundleControl,
  FULL,
  PLAIN,
} from "./emission-order.resolve-inputs.rows.js";
import {
  expectControlledRows,
  expectRowVerdicts,
  machineTrace,
  producer,
} from "./emission-order.support.js";

/** The rows of case (a); see `emission-order.resolve-inputs.rows.ts`. */
describe("emission order (a): merged input resolution on both Phase B routes", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(FULL, ["a_full_"]);
    expectControlledRows(PLAIN, ["a_plain_"]);
    // The plain route is the full route's twin, row for row.
    expect(PLAIN.map(({ name }) => name.replace("a_plain_", ""))).toEqual(
      FULL.map(({ name }) => name.replace("a_full_", "")),
    );
  });

  it.each([...FULL, ...PLAIN])("$name", expectRowVerdicts, 180_000);

  it("the absent-key rows have a single-fault control per fault", () => {
    expectControlledRows(ABSENT_KEYS, ["a_keys_"]);
  });

  it.each(ABSENT_KEYS)("$name", expectRowVerdicts, 180_000);

  it.each([bundleControl("full"), bundleControl("plain")])(
    "$name",
    async (row) => {
      const { rejection, route } = await producer(row);
      expect(route).toBe(row.route);
      expect(rejection?.code).toBe(RejectCodes.CekProgramMaterial);
      expect(rejection?.consensusPhase).toBe("scriptSources");
      expect(rejection?.subject).toBeUndefined();
      await expect(machineTrace(row)).rejects.toThrow(
        "V1 validation reports a ScriptSources rejection but all exact discovery and audit instructions accepted",
      );
    },
    180_000,
  );
});

import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  ABSENT_KEYS,
  FULL,
  PLAIN,
  UNSORTED_SPENDS,
} from "./emission-order.resolve-inputs.rows.js";
import {
  expectControlledRows,
  expectRowVerdicts,
  machineTrace,
  producer,
  stoppingStep,
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

  it("spend inputs out of canonical order fail canonical decode before any key resolves", async () => {
    const { rejection } = await producer(UNSORTED_SPENDS);
    expect(rejection?.code).toBe(RejectCodes.InvalidFieldType);
    expect(rejection?.consensusPhase).toBe("canonicalDecode");
    expect(rejection?.detail).toBe(
      "field[0].item[1] is out of canonical order",
    );
    expect(rejection?.subject).toEqual({
      arm: "FieldItemWidthIllegal",
      fieldIndex: 0n,
      itemIndex: 1n,
    });
    const trace = await machineTrace(UNSORTED_SPENDS);
    expect(trace.verdict).toBe("rejected");
    expect(trace.rejectionCode).toBe(RejectCodes.InvalidFieldType);
    expect(trace.rejectionReason).toStrictEqual({
      FieldItemWidthIllegal: { field_index: 0n, item_index: 1n },
    });
    expect(trace.witnesses[stoppingStep(trace)]!.phase).toBe("canonicalDecode");
  }, 180_000);
});

import { describe, expect, it } from "vitest";

import { rejectionReasonOfSubject } from "../src/index.js";
import {
  BAD_OUTPUT_CONTROL,
  FORCED_ROWS,
  WIDE_OUTPUT_ROW,
} from "./emission-order.forced.rows.js";
import {
  expectControlledRows,
  expectProducerAndTraceVerdicts,
  expectRowVerdicts,
  producer,
  stoppingStep,
} from "./emission-order.support.js";

/** The forced rows; see `emission-order.forced.rows.ts`. */
describe("emission order: a forced transaction's first fault", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(
      [...FORCED_ROWS, WIDE_OUTPUT_ROW, BAD_OUTPUT_CONTROL],
      ["f_"],
    );
  });

  it.each(FORCED_ROWS)("$name", expectRowVerdicts, 180_000);

  it(
    WIDE_OUTPUT_ROW.name,
    async () => {
      const trace = await expectProducerAndTraceVerdicts(WIDE_OUTPUT_ROW);
      expect(trace.witnesses[stoppingStep(trace)]!.phase).toBe(
        "canonicalDecode",
      );
    },
    180_000,
  );

  it(BAD_OUTPUT_CONTROL.name, async () => {
    const { rejection } = await producer(BAD_OUTPUT_CONTROL);
    expect(rejection?.code).toBe(BAD_OUTPUT_CONTROL.expected!.code);
    // The machine reads outputs at the ScriptSources scan.
    expect(rejection?.consensusPhase).toBe("scriptSources");
    expect(rejectionReasonOfSubject(rejection!.subject!, "machine")).toEqual(
      BAD_OUTPUT_CONTROL.expected!.reason,
    );
  });
});

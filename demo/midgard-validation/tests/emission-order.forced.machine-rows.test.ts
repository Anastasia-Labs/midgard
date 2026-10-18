import { describe, expect, it } from "vitest";

import {
  forcedTxOutputFaultRejection,
  locateMidgardForcedTxOutputFault,
  RejectCodes,
} from "../src/index.js";
import {
  BAD_OUTPUT_CONTROL,
  FORCED_OUTPUT_ROWS,
  FORCED_ROWS,
  UNDECODABLE_PLUTUS_REF_ROW,
  WIDE_OUTPUT_ROW,
} from "./emission-order.forced.rows.js";
import {
  expectControlledRows,
  expectProducerAndTraceVerdicts,
  expectRowVerdicts,
  expectRowVerdictsTrace,
  stoppingStep,
  transactionOf,
} from "./emission-order.support.js";

/** The forced rows; see `emission-order.forced.rows.ts`. */
describe("emission order: a forced transaction's first fault", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(
      [...FORCED_ROWS, WIDE_OUTPUT_ROW, BAD_OUTPUT_CONTROL],
      ["f_"],
    );
    expectControlledRows(FORCED_OUTPUT_ROWS, ["fo_"]);
  });

  it.each([...FORCED_ROWS, BAD_OUTPUT_CONTROL])(
    "$name",
    expectRowVerdicts,
    180_000,
  );

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
});

/** A forced transaction's faulty output, by class. */
describe("emission order: a forced transaction's faulty output", () => {
  it.each([...FORCED_OUTPUT_ROWS, UNDECODABLE_PLUTUS_REF_ROW])(
    "$name",
    async (row) => {
      const trace = await expectRowVerdictsTrace(row);
      // The machine reads outputs at the ScriptSources scan.
      if (row.expected !== null)
        expect(trace.witnesses[stoppingStep(trace)]!.phase).toBe(
          "scriptSources",
        );
    },
    180_000,
  );
});

/**
 * An undecodable Plutus reference script is its own output class: the
 * locator names it, with its decode record's class.
 */
describe("a forced transaction's undecodable Plutus reference script", () => {
  const { txCbor } = transactionOf(UNDECODABLE_PLUTUS_REF_ROW);

  it("is its own class, refused with its record's class", () => {
    const fault = locateMidgardForcedTxOutputFault(txCbor);
    // Its script bytes are not a CBOR byte string: class 1.
    expect(fault).toEqual({
      itemIndex: 0,
      itemClass: "plutusScriptUndecodable",
      refusalClass: 1,
    });
    expect(forcedTxOutputFaultRejection(fault!)).toEqual({
      code: RejectCodes.ScriptProgramEncoding,
      subject: {
        arm: "PlutusScriptUndecodable",
        itemKind: 1n,
        index: 0n,
        refusalClass: 1n,
      },
    });
  });
});

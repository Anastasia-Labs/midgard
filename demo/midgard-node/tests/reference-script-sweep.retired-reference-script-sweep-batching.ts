import "./reference-script-sweep.retired-reference-script-sweep-scope.js";

import { type Assets, assetsToValue, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  referenceScriptFee,
  referenceScriptLedgerBytes,
  summarizeReferenceScriptSweepPlan,
  valueCborBytes,
} from "../src/transactions/reference-script-sweep.js";
import {
  applyBatch,
  batchOutRefs,
  LIVE,
  plainUtxo,
  plan,
  plutusScript,
  PREPROD_LIMITS,
  refusalCheck,
  refUtxo,
  RETIRED,
  SIGNER,
  tokenName,
} from "./reference-script-sweep.plan.js";

describe("retired reference-script sweep batching", () => {
  it("derives budgets from the protocol limits with a 10% margin", () => {
    const sweep = plan({ utxos: [refUtxo({ index: 1 })] });

    expect(sweep.budgets).toEqual({
      referenceScriptBytesPerBatch: 184_320,
      txBytesPerBatch: 14_745,
      valueBytesPerOutput: 4_500,
      inputsPerBatch: 64,
    });
    expect(() =>
      plan({
        utxos: [refUtxo({ index: 1 })],
        maxReferenceScriptBytesPerBatch: 184_321,
      }),
    ).toThrow(/no larger than 184320/);
  });

  it("closes a batch exactly at the reference-script byte budget", () => {
    // 9_996-byte programs plus a 3-byte CBOR header: 9_999 ledger bytes.
    const utxos = Array.from({ length: 7 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 9_996) }),
    );
    expect(referenceScriptLedgerBytes(utxos[0]!.scriptRef!)).toBe(9_999);

    const atBoundary = plan({
      utxos,
      maxReferenceScriptBytesPerBatch: 3 * 9_999,
    });
    const belowBoundary = plan({
      utxos,
      maxReferenceScriptBytesPerBatch: 3 * 9_999 - 1,
    });

    expect(atBoundary.batches.map((batch) => batch.inputs.length)).toEqual([
      3, 3, 1,
    ]);
    expect(atBoundary.batches[0]!.referenceScriptBytes).toBe(3 * 9_999);
    expect(belowBoundary.batches.map((batch) => batch.inputs.length)).toEqual([
      2, 2, 2, 1,
    ]);
  });

  it("keeps every batch under the protocol reference-script budget", () => {
    const utxos = Array.from({ length: 60 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 14_000) }),
    );
    const sweep = plan({ utxos });

    // 14_003 ledger bytes each: 13 fit in 184_320, a 14th would not.
    expect(sweep.batches.map((batch) => batch.inputs.length)).toEqual([
      13, 13, 13, 13, 8,
    ]);
    for (const batch of sweep.batches) {
      expect(batch.referenceScriptBytes).toBeLessThanOrEqual(184_320);
    }
  });

  it("caps the input count per batch", () => {
    const utxos = Array.from({ length: 70 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 16) }),
    );

    expect(plan({ utxos }).batches.map((batch) => batch.inputs.length)).toEqual(
      [64, 6],
    );
  });

  it("keeps the estimated transaction size under its budget", () => {
    const utxos = Array.from({ length: 400 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 16) }),
    );
    const sweep = plan({ utxos, maxInputsPerBatch: 1_000 });

    expect(sweep.batches.length).toBeGreaterThan(1);
    for (const batch of sweep.batches) {
      expect(batch.estimatedTxBytes).toBeLessThanOrEqual(14_745);
    }
    // The first batch stopped because one more input would not fit.
    const first = sweep.batches[0]!;
    const next = sweep.batches[1]!.inputs[0]!;
    const extended = plan({
      utxos: [...first.inputs, next],
      maxInputsPerBatch: 1_000,
    });
    expect(extended.batches).toHaveLength(2);
  });

  it("packs a batch's tokens into as few quarantine outputs as the value size allows", () => {
    const utxos = Array.from({ length: 60 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 16) }),
    );
    const limits = { ...PREPROD_LIMITS, maxValueSize: 500 };
    const [batch] = plan({ utxos, limits }).batches;
    const budget = Math.floor((500 * 9) / 10);
    const tokenCount = (assets: Readonly<Assets>) =>
      Object.keys(assets).filter((unit) => unit !== "lovelace").length;

    expect(batch!.quarantineOutputs.length).toBeGreaterThan(1);
    for (const output of batch!.quarantineOutputs) {
      expect(valueCborBytes(output)).toBeLessThanOrEqual(budget);
    }
    // Each output but the last is full: one more token would overflow it.
    for (const output of batch!.quarantineOutputs.slice(0, -1)) {
      const units = Object.keys(output).filter((unit) => unit !== "lovelace");
      const anyOther = Object.keys(batch!.quarantineOutputs.at(-1)!).find(
        (unit) => unit !== "lovelace",
      )!;
      expect(
        valueCborBytes({
          ...output,
          lovelace: 4_000_000_000n,
          [anyOther]: 1n,
        }),
      ).toBeGreaterThan(budget);
      expect(units.length).toBeGreaterThan(0);
    }
    expect(
      batch!.quarantineOutputs.reduce(
        (total, output) => total + tokenCount(output),
        0,
      ),
    ).toBe(60);
  });

  it("gives quarantine outputs only their minimum ADA and returns the rest", () => {
    const utxos = [refUtxo({ index: 1 }), refUtxo({ index: 2 })];
    const [batch] = plan({ utxos }).batches;

    expect(batch!.quarantineOutputs).toHaveLength(1);
    const output = batch!.quarantineOutputs[0]!;
    expect(output.lovelace).toBeLessThan(2_000_000n);
    expect(batch!.netReclaimedLovelace).toBe(
      80_000_000n - batch!.estimatedFee - batch!.quarantineLovelace,
    );
  });

  it("refuses a batch whose inputs cannot pay its fee and quarantine output", () => {
    const utxos = [refUtxo({ index: 1, lovelace: 1_000_000n })];

    expect(refusalCheck(() => plan({ utxos }))).toBe("unfunded-batch");
  });

  it("re-plans idempotently from chain after each confirmed batch", () => {
    const utxos = [
      ...Array.from({ length: 5 }, (_, index) =>
        refUtxo({ index: index + 1, script: plutusScript(index + 1, 9_996) }),
      ),
      refUtxo({ index: 50, policyId: LIVE }),
      plainUtxo(60),
    ];
    const options = { maxReferenceScriptBytesPerBatch: 2 * 9_999 };
    const initial = plan({ utxos, ...options });

    expect(plan({ utxos, ...options })).toEqual(initial);
    expect(batchOutRefs(initial)).toHaveLength(3);
    const afterFirst = applyBatch(utxos, initial, 0);
    expect(batchOutRefs(plan({ utxos: afterFirst, ...options }))).toEqual(
      batchOutRefs(initial).slice(1),
    );
    const afterSecond = applyBatch(
      afterFirst,
      plan({ utxos: afterFirst, ...options }),
      0,
    );
    expect(batchOutRefs(plan({ utxos: afterSecond, ...options }))).toEqual(
      batchOutRefs(initial).slice(2),
    );
  });

  it("summarizes per-batch and total lovelace, fee and quarantine", () => {
    const utxos = Array.from({ length: 5 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 9_996) }),
    );
    const summary = summarizeReferenceScriptSweepPlan(
      plan({ utxos, maxReferenceScriptBytesPerBatch: 2 * 9_999 }),
    );

    expect(summary.totals).toMatchObject({
      batchCount: 3,
      inputCount: 5,
      inputLovelace: 200_000_000n,
      referenceScriptBytes: 5 * 9_999,
    });
    expect(summary.totals.netReclaimedLovelace).toBe(
      summary.totals.inputLovelace -
        summary.totals.estimatedFee -
        summary.totals.quarantineLovelace,
    );
    expect(summary.tokenDisposition).toBe("quarantine");
  });
});

describe("retired reference-script sweep fees and sizes", () => {
  it("prices reference-script bytes with Conway's tiers", () => {
    expect(referenceScriptFee(PREPROD_LIMITS, 0)).toBe(0n);
    expect(referenceScriptFee(PREPROD_LIMITS, 25_600)).toBe(384_000n);
    expect(referenceScriptFee(PREPROD_LIMITS, 25_601)).toBe(384_018n);
    expect(referenceScriptFee(PREPROD_LIMITS, 51_200)).toBe(844_800n);
    // 1_000 bytes at 21.6 lovelace in the third tier.
    expect(referenceScriptFee(PREPROD_LIMITS, 52_200)).toBe(866_400n);
  });

  it("measures ledger script bytes as the single-CBOR program", () => {
    expect(referenceScriptLedgerBytes(plutusScript(1, 5_200))).toBe(5_203);
    expect(
      referenceScriptLedgerBytes({
        type: "Native",
        script: "8200581c" + SIGNER,
      }),
    ).toBe(32);
  });

  it("computes value sizes exactly as the ledger serializes them", () => {
    const shapes: Assets[] = [
      { lovelace: 1_500_000n },
      { lovelace: 4_000_000_000n, [toUnit(RETIRED, tokenName(1))]: 1n },
      Object.fromEntries([
        ["lovelace", 23n],
        ...Array.from({ length: 30 }, (_, index) => [
          toUnit(RETIRED, "ff".repeat(index + 1)),
          BigInt(index * 1_000),
        ]),
        [toUnit(LIVE, ""), 70_000n],
      ]),
    ];
    for (const assets of shapes) {
      const positive = Object.fromEntries(
        Object.entries(assets).filter(
          ([unit, amount]) => unit === "lovelace" || amount > 0n,
        ),
      );
      const value = assetsToValue(positive);
      try {
        expect(valueCborBytes(positive)).toBe(value.to_cbor_bytes().length);
      } finally {
        value.free();
      }
    }
  });
});

import {
  buildMidgardLedgerOutputScanTrace,
  buildMidgardLedgerOutputValueTrace,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { expect, it } from "vitest";
it("prepares a maximum Value summary equivalence vector", () => {
  const names = new Map<string, bigint>();
  for (let i = 0; i < 1304; i++) {
    const name =
      i === 0
        ? Buffer.alloc(0)
        : i <= 256
          ? Buffer.from([i - 1])
          : Buffer.from([(i - 257) >> 8, (i - 257) & 255]);
    names.set(name.toString("hex"), i === 1303 ? 256n : 1n);
  }
  const output = encodeMidgardTxOutput({
    address: Buffer.from("60" + "aa".repeat(28), "hex"),
    value: {
      lovelace: 2_000_000n,
      assets: new Map([["44".repeat(28), names]]),
    },
  });
  const scan = buildMidgardLedgerOutputScanTrace(output);
  const value = buildMidgardLedgerOutputValueTrace({
    assets: scan.steps.flatMap((step) =>
      step.asset === null ? [] : [step.asset],
    ),
    lovelace: scan.terminal.lovelace,
  });
  const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: output,
  }).descriptor;
  const start = scan.steps.find((step) => step.control.stage === 0)!.next
    .cursor;
  const end = scan.steps.find(
    (step) => step.control.stage <= 3 && step.next.stage === 4,
  )!.next.cursor;
  const skeleton = Buffer.concat([
    output.subarray(0, start),
    Buffer.from("018200a0", "hex"),
    output.subarray(end),
  ]);
  expect(skeleton.toString("hex")).toBe(
    "a200581d60" + "aa".repeat(28) + "018200a0",
  );
  expect(Buffer.from(value.terminal.result!.root).toString("hex")).toBe(
    "add54d30058e271fc5847cd7b979e05d234fc7395d4044a3481b716b82e3be04",
  );
  expect(descriptor.cardanoTxOut.root.toString("hex")).toBe(
    "6aa793a77428e649db031f3f031958b5ad602ea5ae61fbda19aed0773038e170",
  );
  expect(descriptor.cardanoValueSize).toBe(5000);
}, 60_000);

import {
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { makeForcedOrderSettlementScenario } from "./support/forced-order-settlement.js";
import {
  type CompleteSignedTransactionMeasurement,
  makeNativeTx,
} from "./support/submit-init-emulator-shared.js";

/** SDK-admitted counterpart of tx_order_mint_variable_width_aggregate_maximum.
 * It uses a canonical output and versioned script, filling the remaining
 * aggregate space with minimum-width opaque redeemers. The mint must
 * authenticate opaque field envelopes even when ledger validation will later
 * reject an item's meaning; semantic invalidity is not an admission shortcut.
 */
const maximumVariableWidthSubmission = () => {
  const item = Buffer.from([0]);
  const build = (redeemerCount: number, lovelace: bigint) =>
    materializeMidgardForcedTxFromCanonical(
      makeNativeTx({
        spendInputCbors: [
          encodeMidgardSpendInputItem({
            txId: Buffer.alloc(32, 0x31),
            outputIndex: 0,
          }),
        ],
        referenceByte: "31",
        outputCbors: [
          encodeMidgardTxOutput({
            address: Buffer.concat([
              Buffer.from([0x60]),
              Buffer.alloc(28, 0x11),
            ]),
            value: { lovelace, assets: new Map() },
          }),
        ],
        fee: 0n,
        requiredSignerHashes: ["11".repeat(28)],
        requiredObserverHashes: ["11".repeat(28)],
        mintPreimageCbor: encodeCbor([item]),
        scriptTxWitsPreimageCbor: encodeCbor([
          encodeMidgardVersionedScript({
            language: "NativeCardano",
            nativeScript: { type: "after", slot: 0n },
            scriptBytes: Buffer.from("820400", "hex"),
          }),
        ]),
        addrTxWitsPreimageCbor: encodeCbor([
          encodeMidgardAddressWitnessItem({
            verificationKey: Buffer.alloc(32, 0x32),
            signature: Buffer.alloc(64, 0x33),
          }),
        ]),
        redeemerTxWitsPreimageCbor: encodeCbor(
          Array.from({ length: redeemerCount }, () => item),
        ),
      }),
    );
  const fieldsOf = (submitted: ReturnType<typeof build>) => [
    submitted.body.spendInputsPreimageCbor,
    submitted.body.referenceInputsPreimageCbor,
    submitted.body.outputsPreimageCbor,
    submitted.body.requiredObserversPreimageCbor,
    submitted.body.requiredSignersPreimageCbor,
    submitted.body.mintPreimageCbor,
    submitted.witnessSet.scriptTxWitsPreimageCbor,
    submitted.witnessSet.addrTxWitsPreimageCbor,
    submitted.witnessSet.redeemerTxWitsPreimageCbor,
  ];
  // Two adjacent canonical ADA widths select the parity needed by the dense
  // two-byte item envelope; the field sum remains exactly the frozen maximum.
  let lovelace = 0n;
  let base = build(0, lovelace);
  let otherBytes = fieldsOf(base)
    .slice(0, 8)
    .reduce((total, field) => total + field.length, 0);
  if ((32_768 - otherBytes - 3) % 2 !== 0) {
    lovelace = 24n;
    base = build(0, lovelace);
    otherBytes = fieldsOf(base)
      .slice(0, 8)
      .reduce((total, field) => total + field.length, 0);
  }
  const submitted = build((32_768 - otherBytes - 3) / 2, lovelace);
  const fields = fieldsOf(submitted);
  expect(fields.reduce((total, field) => total + field.length, 0)).toBe(32_768);
  return submitted;
};

describe("forced-order capacity dependency", () => {
  it("publishes the maximum variable-width carriage and exposes the unresolved mint budget gap", async () => {
    const measurements: {
      stage: string;
      measurement: CompleteSignedTransactionMeasurement;
    }[] = [];
    await expect(
      makeForcedOrderSettlementScenario({
        submitted: maximumVariableWidthSubmission(),
        onMeasurement: (stage, measurement) =>
          measurements.push({ stage, measurement }),
      }),
    ).rejects.toThrow(/budget|execution units|ExUnits/iu);
    expect(
      measurements.filter(({ stage }) => stage.endsWith("-publication")),
    ).toHaveLength(3);
    expect(
      measurements.some(({ stage }) => stage === "field-8-certificate"),
    ).toBe(true);
    expect(measurements.some(({ stage }) => stage === "order-mint")).toBe(
      false,
    );
    for (const { measurement } of measurements) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15_872);
      expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
      expect(measurement.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
    }
    // This diagnostic is evidence of an open capacity dependency, never a
    // passing maximum-order fit row. No order was accepted or settled.
    console.info(
      "forced-order maximum carriage measurements",
      JSON.stringify(measurements, (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      ),
    );
  }, 300_000);
});

import {
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardMintPolicyItem,
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
 * It uses a canonical output, filling the remaining aggregate space with
 * minimum-width native script witnesses (`82 00 40`, the narrowest item the
 * mint's admission rules accept in a variable-width field). The mint must
 * authenticate opaque field envelopes even when ledger validation will later
 * reject an item's meaning; semantic invalidity is not an admission shortcut.
 */
const maximumVariableWidthSubmission = () => {
  // The narrowest mint item the mint's rule 6 admits: one unit of one asset.
  const item = encodeMidgardMintPolicyItem({
    policyId: Buffer.alloc(28, 0x44),
    assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
  });
  /** A native versioned script with a `width`-byte payload: `3 + width` bytes. */
  const nativeWitness = (width: number) =>
    Buffer.concat([
      Buffer.from([0x82, 0x00, 0x40 + width]),
      Buffer.alloc(width, 0x00),
    ]);
  const build = (witnessCount: number, lastWidth: number) =>
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
            value: { lovelace: 2_000_000n, assets: new Map() },
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
          ...Array.from({ length: witnessCount }, () => nativeWitness(0)),
          nativeWitness(lastWidth),
        ]),
        addrTxWitsPreimageCbor: encodeCbor([
          encodeMidgardAddressWitnessItem({
            verificationKey: Buffer.alloc(32, 0x32),
            signature: Buffer.alloc(64, 0x33),
          }),
        ]),
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
  const total = (submitted: ReturnType<typeof build>) =>
    fieldsOf(submitted).reduce((sum, field) => sum + field.length, 0);
  // Every filler item is four bytes; the last one's payload width takes up the
  // remainder, so the field sum lands exactly on the frozen maximum. Its count
  // stays inside one header width (256..65,535 items), so the header does not
  // move as the count grows.
  const base = build(256, 0);
  const spare = 32_768 - total(base);
  const submitted = build(256 + Math.floor(spare / 4), spare % 4);
  expect(total(submitted)).toBe(32_768);
  return submitted;
};

describe("forced-order capacity dependency", () => {
  it("refuses the maximum variable-width carriage before anything is signed", async () => {
    // The mint's own run over this shape (tx_order_mint_variable_width_aggregate_maximum)
    // is far above the per-transaction budget, so the SDK refuses it from the
    // measured rates before publishing or signing anything.
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
    ).rejects.toThrow(/exceeds the tx-order mint's capacity/u);
    expect(measurements).toEqual([]);
  }, 300_000);
});

import { inputNoIdxSpendInputsCommitmentV1 } from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import {
  buildSignedInputNoIdxSpendInputsPublicationV1,
  inputNoIdxStep02WalletInputs,
  selectInputNoIdxStep02FeeInput,
} from "../src/submit-input-no-idx-step-02.js";

const PREPROD_EPOCH_303_BOUND_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  minFeeA: 44,
  minFeeB: 155_381,
  maxTxSize: 16_384,
  maxValSize: 5_000,
  maxTxExMem: 16_500_000n,
  maxTxExSteps: 10_000_000_000n,
  priceMem: 0.0577,
  priceStep: 0.0000721,
  coinsPerUtxoByte: 4_310n,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  minFeeRefScriptCostPerByte: 15,
} as const;

const utxo = (txHashByte: string, lovelace: bigint): UTxO => ({
  txHash: txHashByte.repeat(64),
  outputIndex: 0,
  address: "addr_test1vfee",
  assets: { lovelace },
});

describe("input-no-idx step-02 tier-2 internals", () => {
  it("excludes the exact publication out-ref from fee selection", () => {
    const publication = utxo("a", 100_000_000n);
    const feeInput = utxo("b", 20_000_000n);

    expect(
      selectInputNoIdxStep02FeeInput({
        walletUtxos: [publication, feeInput],
        publicationUtxo: publication,
      }),
    ).toBe(feeInput);
  });

  it("excludes by out-ref rather than object identity", () => {
    const publication = utxo("a", 100_000_000n);
    const providerCopy = { ...publication, assets: { ...publication.assets } };
    const feeInput = utxo("b", 20_000_000n);

    expect(
      selectInputNoIdxStep02FeeInput({
        walletUtxos: [providerCopy, feeInput],
        publicationUtxo: publication,
      }),
    ).toBe(feeInput);
  });

  it("derives transaction-local wallet inputs without poisoning the wallet snapshot", () => {
    const publication = utxo("a", 100_000_000n);
    const providerCopy = { ...publication, assets: { ...publication.assets } };
    const feeInput = utxo("b", 20_000_000n);
    const collateralInput = utxo("c", 10_000_000n);
    const walletSnapshot = [providerCopy, feeInput, collateralInput];

    const eligible = inputNoIdxStep02WalletInputs({
      walletUtxos: walletSnapshot,
      publicationUtxo: publication,
    });

    expect(eligible).toEqual([feeInput, collateralInput]);
    expect(eligible).not.toBe(walletSnapshot);
    expect(walletSnapshot).toEqual([providerCopy, feeInput, collateralInput]);
    expect(
      eligible.some(
        ({ txHash, outputIndex }) =>
          txHash === publication.txHash &&
          outputIndex === publication.outputIndex,
      ),
    ).toBe(false);
  });

  it("measures genuine signed publication-only transactions at 19/20/296", async () => {
    const prover = generateEmulatorAccount({ lovelace: 30_000_000_000n });
    const emulator = new Emulator(
      [prover],
      PREPROD_EPOCH_303_BOUND_PROTOCOL_PARAMETERS,
    );
    const lucid = await Lucid(emulator, "Custom");
    const signer = resolveProverSigner({
      network: "Preprod",
      walletSeedPhrase: prover.seedPhrase,
    });
    const measurements = [];

    for (const itemCount of [19, 20, 296] as const) {
      const inputs = Array.from({ length: itemCount }, (_, index) => ({
        tx_id: (index + 1).toString(16).padStart(64, "0"),
        output_index: BigInt(index),
      }));
      const verifiedTxInputsHash = inputNoIdxSpendInputsCommitmentV1(inputs);
      const result = await buildSignedInputNoIdxSpendInputsPublicationV1({
        lucid,
        network: "Preprod",
        signer,
        computationThreadPolicyId: "11".repeat(28),
        computationThreadAssetName: "22".repeat(32),
        verifiedTxInputsHash,
        inputsPreimage: {
          inputsPreimage: inputs,
          badInputsIndex: itemCount - 1,
        },
      });
      const { measurement } = result;
      const signedTransaction = CML.Transaction.from_cbor_hex(
        measurement.signedTxCbor,
      );

      expect(measurement.signedTxBytes).toBe(
        Buffer.from(measurement.signedTxCbor, "hex").length,
      );
      expect(measurement.signedTxBytes).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxSize,
      );
      expect(measurement.maxOutputValueBytes).toBeLessThanOrEqual(
        emulator.protocolParameters.maxValSize,
      );
      expect(measurement.txByteMargin).toBeGreaterThanOrEqual(0);
      expect(measurement.valueByteMargin).toBeGreaterThanOrEqual(0);
      expect(measurement.fee).toBe(signedTransaction.body().fee());
      expect(measurement.outputMinAda).toBe(result.lovelace);
      expect(measurement.outputMinAda).toBeGreaterThan(0n);
      expect(measurement.inputCount).toBe(1);
      expect(measurement.referenceInputCount).toBe(0);
      expect(measurement.outputCount).toBe(2);
      expect(measurement.collateralInputCount).toBe(0);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.redeemerCount).toBe(0);
      expect(signedTransaction.witness_set().vkeywitnesses()?.len()).toBe(
        measurement.vkeyWitnessCount,
      );
      expect(signedTransaction.witness_set().redeemers()).toBeUndefined();

      measurements.push({
        itemCount,
        ...measurement,
        signedTxCbor: undefined,
        executionReserve: "100% (publication-only; no redeemers)",
      });
    }

    expect(measurements).toEqual([
      {
        itemCount: 19,
        signedTxBytes: 1_188,
        signedTxCbor: undefined,
        fee: 207_829n,
        outputMinAda: 4_672_040n,
        inputCount: 1,
        referenceInputCount: 0,
        outputCount: 2,
        collateralInputCount: 0,
        vkeyWitnessCount: 1,
        redeemerCount: 0,
        publicationOutputIndex: 0,
        maxOutputValueBytes: 9,
        txByteMargin: 15_196,
        valueByteMargin: 4_991,
        executionReserve: "100% (publication-only; no redeemers)",
      },
      {
        itemCount: 20,
        signedTxBytes: 1_227,
        signedTxCbor: undefined,
        fee: 209_545n,
        outputMinAda: 4_840_130n,
        inputCount: 1,
        referenceInputCount: 0,
        outputCount: 2,
        collateralInputCount: 0,
        vkeyWitnessCount: 1,
        redeemerCount: 0,
        publicationOutputIndex: 0,
        maxOutputValueBytes: 9,
        txByteMargin: 15_157,
        valueByteMargin: 4_991,
        executionReserve: "100% (publication-only; no redeemers)",
      },
      {
        itemCount: 296,
        signedTxBytes: 12_305,
        signedTxCbor: undefined,
        fee: 696_977n,
        outputMinAda: 52_586_310n,
        inputCount: 1,
        referenceInputCount: 0,
        outputCount: 2,
        collateralInputCount: 0,
        vkeyWitnessCount: 1,
        redeemerCount: 0,
        publicationOutputIndex: 0,
        maxOutputValueBytes: 9,
        txByteMargin: 4_079,
        valueByteMargin: 4_991,
        executionReserve: "100% (publication-only; no redeemers)",
      },
    ]);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { q13Tier2Publications: measurements },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 30_000);
});

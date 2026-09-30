import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { CML, valueToAssets, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { type NodeUtxo } from "../src/commands/command-utils.js";
import {
  buildTerminalDrainTx,
  buildTransferTx,
  buildTransferTxWithMinFee,
  selectTransferInputs,
} from "../src/commands/submit-l2-transfer.js";
import { toQueuedTx } from "./helpers/local-l2-transfer.js";
import {
  mkNodeUtxo,
  mkQueued,
  OTHER_TEST_SEED,
  TEST_SEED,
} from "./submit-l2-transfer.submit-l2-transfer-config-helpers.js";

describe("submit-l2-transfer tx building", () => {
  it("attaches the canonical empty program-material sidecar for local validation", () => {
    const txId = Buffer.from("ab".repeat(32), "hex");
    const txCbor = Buffer.from("80", "hex");
    const queued = toQueuedTx({
      txId,
      txIdHex: txId.toString("hex"),
      txCbor,
      txHex: txCbor.toString("hex"),
      fee: 0n,
      senderAddress: "sender",
      destinationAddress: "destination",
      selectedInputs: [],
      requestedAssets: {},
      changeAssets: {},
    });

    expect(queued.txId).toBe(txId);
    expect(queued.txCbor).toBe(txCbor);
    expect(queued.programMaterialSidecarCbor).toEqual(
      encodeMidgardCekProgramMaterialSidecar([]),
    );
  });

  it("selects sufficient inputs and builds a valid native transfer with change", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const tokenUnit = `${"ab".repeat(28)}${"cd".repeat(2)}`;

    const utxos: readonly NodeUtxo[] = [
      mkNodeUtxo({
        txHash: "22".repeat(32),
        outputIndex: 1,
        address: sender.address,
        assets: {
          lovelace: 2_500_000n,
          [tokenUnit]: 5n,
        },
      }),
      mkNodeUtxo({
        txHash: "11".repeat(32),
        outputIndex: 0,
        address: sender.address,
        assets: {
          lovelace: 4_000_000n,
        },
      }),
    ];

    const requestedAssets = {
      lovelace: 3_000_000n,
      [tokenUnit]: 5n,
    } as const;

    const selected = selectTransferInputs(utxos, requestedAssets);
    expect(
      selected.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
    ).toEqual([`${"11".repeat(32)}#0`, `${"22".repeat(32)}#1`]);

    const built = await buildTransferTx({
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      selectedInputs: selected,
      requestedAssets,
      networkId: 0n,
    });

    expect(built.changeAssets).toEqual({
      lovelace: 3_500_000n,
    });

    const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor);
    // Field-0 items are §5.3's fixed-index form, so they must be read with the
    // field-item decoder — CML's `TransactionInput` decoder tolerates the
    // non-minimal `19 0000` index but is not the contract these bytes obey.
    const spendInputs = decodeMidgardNativeByteListPreimage(
      nativeTx.body.spendInputsPreimageCbor,
    ).map((bytes) => {
      const input = decodeMidgardSpendInputItem(bytes);
      return `${Buffer.from(input.txId).toString("hex")}#${input.outputIndex.toString()}`;
    });
    expect(spendInputs).toEqual([
      `${"11".repeat(32)}#0`,
      `${"22".repeat(32)}#1`,
    ]);

    const outputs = decodeMidgardNativeByteListPreimage(
      nativeTx.body.outputsPreimageCbor,
    ).map((bytes) => {
      expect(bytes[0] >> 5).toBe(5);
      const output = decodeMidgardTxOutput(bytes);
      return {
        address: encodeMidgardAddressText(output.address),
        assets: valueToAssets(midgardValueToCmlValue(output.value)),
      };
    });
    expect(outputs).toHaveLength(2);
    expect(outputs[0]).toEqual({
      address: destination.address,
      assets: {
        lovelace: 3_000_000n,
        [tokenUnit]: 5n,
      },
    });
    expect(outputs[1]).toEqual({
      address: sender.address,
      assets: {
        lovelace: 3_500_000n,
      },
    });

    const validation = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    expect(validation.rejected).toHaveLength(0);
    expect(validation.accepted).toHaveLength(1);
  });

  it("converges fees against signed bytes and passes local Phase A/B", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const minFeeA = 44n;
    const minFeeB = 155_381n;
    const senderUtxo = mkNodeUtxo({
      txHash: "44".repeat(32),
      outputIndex: 0,
      address: sender.address,
      assets: {
        lovelace: 8_000_000n,
      },
    });

    const built = await buildTransferTxWithMinFee({
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      availableUtxos: [senderUtxo],
      requestedAssets: { lovelace: 3_000_000n },
      networkId: 0n,
      minFeeA,
      minFeeB,
    });

    expect(built.fee).toBe(minFeeA * BigInt(built.txCbor.length) + minFeeB);
    expect(built.changeAssets).toEqual({
      lovelace: 8_000_000n - 3_000_000n - built.fee,
    });

    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA,
        minFeeB,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);

    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map([
          [senderUtxo.outrefCbor.toString("hex"), senderUtxo.outputCbor],
        ]),
        {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        },
      ),
    );
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("builds canonical V1 transfers", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, {
      network: "Preprod",
    });
    const senderUtxo = mkNodeUtxo({
      txHash: "45".repeat(32),
      outputIndex: 0,
      address: sender.address,
      assets: { lovelace: 8_000_000n },
    });

    const built = await buildTransferTxWithMinFee({
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      availableUtxos: [senderUtxo],
      requestedAssets: { lovelace: 3_000_000n },
      networkId: 0n,
      minFeeA: 44n,
      minFeeB: 155_381n,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });

    expect(
      decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor).version,
    ).toBe(1n);
    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA: 44n,
        minFeeB: 155_381n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    );
    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
  });

  it("builds a deterministic exact-zero all-input sweep that passes Phase A/B", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const minFeeA = 44n;
    const minFeeB = 155_381n;
    const inputs = [
      mkNodeUtxo({
        txHash: "62".repeat(32),
        outputIndex: 1,
        address: sender.address,
        assets: { lovelace: 2_000_000n },
      }),
      mkNodeUtxo({
        txHash: "61".repeat(32),
        outputIndex: 0,
        address: sender.address,
        assets: { lovelace: 3_000_000n },
      }),
    ];
    const args = {
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      availableUtxos: inputs,
      networkId: 0n,
      minFeeA,
      minFeeB,
      feeCap: 200_000n,
    } as const;
    const built = await buildTerminalDrainTx(args);
    const repeated = await buildTerminalDrainTx(args);
    expect(repeated.txHex).toBe(built.txHex);
    expect(built.selectedInputs.map((x) => x.txHash)).toEqual([
      "61".repeat(32),
      "62".repeat(32),
    ]);
    expect(built.changeAssets).toEqual({});
    expect((built.requestedAssets.lovelace ?? 0n) + built.fee).toBe(5_000_000n);
    expect(built.fee).toBeGreaterThanOrEqual(
      minFeeA * BigInt(built.txCbor.length) + minFeeB,
    );
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor);
    const outputs = decodeMidgardNativeByteListPreimage(
      decoded.body.outputsPreimageCbor,
    );
    expect(outputs).toHaveLength(1);
    const output = decodeMidgardTxOutput(outputs[0]!);
    expect(encodeMidgardAddressText(output.address)).toBe(destination.address);
    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA,
        minFeeB,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    expect(phaseA.rejected).toHaveLength(0);
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map(
          inputs.map((x) => [x.outrefCbor.toString("hex"), x.outputCbor]),
        ),
        {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        },
      ),
    );
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("fails terminal drain closed for non-ADA, insufficient, and over-cap sources", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const base = {
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      networkId: 0n,
      minFeeA: 44n,
      minFeeB: 155_381n,
      feeCap: 200_000n,
    };
    await expect(
      buildTerminalDrainTx({
        ...base,
        availableUtxos: [
          mkNodeUtxo({
            txHash: "71".repeat(32),
            outputIndex: 0,
            address: sender.address,
            assets: { lovelace: 2_000_000n, ["aa".repeat(28)]: 1n },
          }),
        ],
      }),
    ).rejects.toThrow("non-ADA");
    await expect(
      buildTerminalDrainTx({
        ...base,
        availableUtxos: [
          mkNodeUtxo({
            txHash: "72".repeat(32),
            outputIndex: 0,
            address: sender.address,
            assets: { lovelace: 1n },
          }),
        ],
      }),
    ).rejects.toThrow("cannot pay fee");
    await expect(
      buildTerminalDrainTx({
        ...base,
        feeCap: 1n,
        availableUtxos: [
          mkNodeUtxo({
            txHash: "73".repeat(32),
            outputIndex: 0,
            address: sender.address,
            assets: { lovelace: 2_000_000n },
          }),
        ],
      }),
    ).rejects.toThrow("exceeds cap");
  });
});

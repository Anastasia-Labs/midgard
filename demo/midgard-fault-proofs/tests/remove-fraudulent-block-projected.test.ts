import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { FraudProofTokenDatum, SchedulerDatum } from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { expect, it, vi } from "vitest";

import {
  RegisteredOperatorActivationRequiredError,
  submitRemoveFraudulentBlock,
} from "../src/remove-fraudulent-block.js";

const runDirectory = process.env.MIDGARD_FINALIZED_RUN_DIR;
type EvaluationInput = Parameters<
  ReturnType<typeof createScalusEvaluator>["evaluate"]
>[0];
type Capture = {
  transactionCborHex: string;
  additionalUTxOs: UTxO[];
  slotConfig: EvaluationInput["context"]["slotConfig"];
  protocolParameters: EvaluationInput["context"]["protocolParameters"];
  costModelsCborHex: string;
};
const readArtifact = async <T>(path: string): Promise<T> =>
  JSON.parse(
    await readFile(join(runDirectory!, path), "utf8"),
    (_key, value: unknown) =>
      typeof value === "object" &&
      value !== null &&
      Object.keys(value).length === 1 &&
      "bigint" in value &&
      typeof value.bigint === "string"
        ? BigInt(value.bigint)
        : value,
  ) as T;

// Only the locally evaluated activation's exact outputs are overlaid onto the
// captured ledger. They remain hypothetical until activation confirms on L1.
it.skipIf(runDirectory === undefined)(
  "evaluates removal against projected actual successor activation without signing",
  async () => {
    const [
      recorded,
      activation,
      walletUtxos,
      stateQueueUtxos,
      manifest,
      blueprintJson,
    ] = await Promise.all([
      readArtifact<{ captured: Capture[] }>(
        "work/removal-preflight-evaluation.json",
      ),
      readArtifact<
        Capture & {
          projectedOutputs: UTxO[];
          signed: boolean;
          submitted: boolean;
        }
      >("work/activation-preflight-evaluation.json"),
      readArtifact<UTxO[]>("work/removal-collateral-utxos.json"),
      readArtifact<UTxO[]>("work/removal-state-queue-utxos.json"),
      readArtifact<{ contracts: Record<string, { scriptHash: string }> }>(
        "deploymentInfo/manifest.json",
      ),
      readFile(join(runDirectory!, "deploymentInfo/plutus.json"), "utf8"),
    ]);
    expect(activation.signed).toBe(false);
    expect(activation.submitted).toBe(false);
    const original = recorded.captured[0]!;
    const activationTx = CML.Transaction.from_cbor_hex(
      activation.transactionCborHex,
    );
    const originalLedger = new Map(
      [
        ...original.additionalUTxOs,
        ...activation.additionalUTxOs,
        ...walletUtxos,
        ...stateQueueUtxos,
      ].map((utxo) => [`${utxo.txHash}#${utxo.outputIndex}`, utxo]),
    );
    const projectedLedger = new Map(originalLedger);
    const spent = activationTx.body().inputs();
    for (let index = 0; index < spent.len(); index++) {
      const input = spent.get(index);
      projectedLedger.delete(
        `${input.transaction_id().to_hex()}#${input.index()}`,
      );
    }
    for (const output of activation.projectedOutputs)
      projectedLedger.set(`${output.txHash}#${output.outputIndex}`, output);
    const proof = original.additionalUTxOs.find(
      (utxo) =>
        utxo.txHash ===
          "93806f20a7da66fab0ea0eac07ff0b1e7d55d6090648bdd87adf4f6c96d85e9f" &&
        utxo.outputIndex === 0,
    )!;
    const prover = Data.from(proof.datum!, FraudProofTokenDatum).fraud_prover;
    const address = credentialToAddress("Custom", {
      type: "Key",
      hash: prover,
    });
    expect(walletUtxos.length).toBeGreaterThan(0);
    expect(walletUtxos.every((utxo) => utxo.address === address)).toBe(true);
    const now =
      activation.slotConfig.zeroTime +
      (Math.max(
        Number(activationTx.body().validity_interval_start()!),
        Number(
          CML.Transaction.from_cbor_hex(original.transactionCborHex)
            .body()
            .ttl()!,
        ),
      ) +
        1 -
        activation.slotConfig.zeroSlot) *
        activation.slotConfig.slotLength;
    const validTo = BigInt(now + 300_789);
    const evaluator = createScalusEvaluator();
    const snapshots: EvaluationInput[] = [];
    const forbiddenSign = vi.fn((): never => {
      throw new Error("offline removal must not sign or submit");
    });
    const runBuilder = async (ledger: Map<string, UTxO>) => {
      const emulator = new Emulator([], activation.protocolParameters);
      emulator.time = now;
      emulator.slot =
        Math.floor(
          (now - activation.slotConfig.zeroTime) /
            activation.slotConfig.slotLength,
        ) + activation.slotConfig.zeroSlot;
      emulator.ledger = Object.fromEntries(
        [...ledger.values()].map((utxo) => [
          utxo.txHash + utxo.outputIndex,
          { utxo, spent: false },
        ]),
      );
      emulator.submitTx = forbiddenSign;
      const lucid = await Lucid(emulator, "Custom", {
        slotConfig: activation.slotConfig,
        evaluator: {
          name: "retained-projected-removal",
          evaluate: async (input) => {
            await evaluator.evaluate(input);
            snapshots.push(input);
            throw new Error("offline removal evaluated before signing");
          },
        },
      });
      return submitRemoveFraudulentBlock({
        lucid,
        blueprint: JSON.parse(blueprintJson),
        deploymentInfo: manifest,
        network: "Custom",
        signer: {
          source: "retained-public-credential",
          address,
          paymentKeyHash: prover,
          selectWallet: (wallet) =>
            wallet.selectWallet.fromAddress(address, walletUtxos),
        },
        fraudCategory: "transitionTrace",
        fraudulentHeaderHash:
          "6f14836d32741e0f6d60ffabb2930622d4a31b17a4bbaf74c1c38dd2",
        requireReferenceScripts: true,
        awaitConfirmation: false,
        validFrom: BigInt(now - 59_877),
        validTo,
        preSubmitBoundary: forbiddenSign,
      });
    };
    await expect(runBuilder(originalLedger)).rejects.toBeInstanceOf(
      RegisteredOperatorActivationRequiredError,
    );
    expect(snapshots).toHaveLength(0);
    await expect(runBuilder(projectedLedger)).rejects.toThrow(
      "offline removal evaluated before signing",
    );
    expect(snapshots).toHaveLength(1);
    const evaluated = snapshots[0]!;
    const result = CML.Transaction.from_cbor_hex(evaluated.tx);
    const outputs = result.body().outputs();
    const scheduler = Array.from({ length: outputs.len() }, (_, index) =>
      coreToTxOutput(outputs.get(index)),
    ).find(
      (output) =>
        paymentCredentialOf(output.address).hash ===
        manifest.contracts.schedulerSpend!.scriptHash,
    )!;
    const datum = Data.from(scheduler.datum!, SchedulerDatum);
    expect(datum).not.toBe("NoActiveOperators");
    if (datum === "NoActiveOperators")
      throw new Error("expected activated successor");
    expect(datum.ActiveOperator.operator).toBe(
      "0cf5f6c23cb22b1850cc0223177a7d2e2f8739991ee2742b30a71f75",
    );
    const upper =
      BigInt(evaluated.context.slotConfig.zeroTime) +
      (result.body().ttl()! - BigInt(evaluated.context.slotConfig.zeroSlot)) *
        BigInt(evaluated.context.slotConfig.slotLength) -
      1n;
    expect(datum.ActiveOperator.start_time).toBe(upper);
    expect(datum.ActiveOperator.start_time).not.toBe(validTo);
    const rawClockDatum = Data.to(
      { ActiveOperator: { ...datum.ActiveOperator, start_time: validTo } },
      SchedulerDatum,
    );
    // Replay the exact successful body with only the old raw-wall-clock datum
    // restored. The datum bytes appear exactly once, so the substitution
    // touches nothing else even when the integer widths differ.
    expect(evaluated.tx.split(scheduler.datum!)).toHaveLength(2);
    await expect(
      evaluator.evaluate({
        ...evaluated,
        tx: evaluated.tx.replace(scheduler.datum!, rawClockDatum),
      }),
    ).rejects.toMatchObject({
      message: expect.stringMatching(/^Error evaluated/u),
    });
    expect(forbiddenSign).not.toHaveBeenCalled();
  },
  120_000,
);

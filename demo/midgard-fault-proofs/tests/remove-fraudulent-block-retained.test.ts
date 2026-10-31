import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { getLinkedListNodeViewFromUTxO } from "@al-ft/midgard-sdk";
import { CML, paymentCredentialOf, type UTxO } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

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

// This opt-in test replays the exact unsigned transaction and resolved outputs
// recorded at the retained deployment's failed removal evaluation boundary.
// It neither queries the node nor changes any retained artifact.
describe.skipIf(runDirectory === undefined)(
  "retained removal scheduler preconditions",
  () => {
    it("refuses the real empty-active-set rewind while an eligible successor remains registered", async () => {
      const [captureJson, manifestJson] = await Promise.all([
        readFile(
          join(runDirectory!, "work/removal-preflight-evaluation.json"),
          "utf8",
        ),
        readFile(join(runDirectory!, "deploymentInfo/manifest.json"), "utf8"),
      ]);
      const recorded = JSON.parse(captureJson, (_key, value: unknown) => {
        if (
          typeof value === "object" &&
          value !== null &&
          Object.keys(value).length === 1 &&
          "bigint" in value &&
          typeof value.bigint === "string"
        )
          return BigInt(value.bigint);
        return value;
      }) as {
        deploymentFingerprint: string;
        captured: Capture[];
        signed: boolean;
        submitted: boolean;
      };
      const manifest = JSON.parse(manifestJson) as {
        manifestId: string;
        contracts: Record<string, { scriptHash: string }>;
      };
      expect(recorded.deploymentFingerprint).toBe(manifest.manifestId);
      expect(recorded.signed).toBe(false);
      expect(recorded.submitted).toBe(false);
      expect(recorded.captured).toHaveLength(1);
      const capture = recorded.captured[0]!;
      const tx = CML.Transaction.from_cbor_hex(capture.transactionCborHex);
      const schedulerInput = tx.body().inputs().get(2);
      const scheduler = capture.additionalUTxOs.find(
        (utxo) =>
          utxo.txHash === schedulerInput.transaction_id().to_hex() &&
          BigInt(utxo.outputIndex) === schedulerInput.index(),
      )!;
      expect(paymentCredentialOf(scheduler.address).hash).toBe(
        manifest.contracts.schedulerSpend!.scriptHash,
      );
      const registeredRoot = capture.additionalUTxOs.find((utxo) =>
        Object.keys(utxo.assets).some((unit) =>
          unit.startsWith(
            manifest.contracts.registeredOperatorsMint!.scriptHash,
          ),
        ),
      )!;
      const registered = await Effect.runPromise(
        getLinkedListNodeViewFromUTxO(registeredRoot),
      );
      expect(registered.key).toBe("Empty");
      expect(registered.next).not.toBe("Empty");
      if (registered.next === "Empty")
        throw new Error("expected captured pending registration");
      const activationTime = BigInt(`0x${registered.next.Key.key}`);
      const lower =
        BigInt(capture.slotConfig.zeroTime) +
        (tx.body().validity_interval_start()! -
          BigInt(capture.slotConfig.zeroSlot)) *
          BigInt(capture.slotConfig.slotLength);
      expect(activationTime).toBeLessThan(lower);
      await expect(
        createScalusEvaluator().evaluate({
          tx: capture.transactionCborHex,
          additionalUTxOs: capture.additionalUTxOs,
          context: {
            network: "Custom",
            slotConfig: capture.slotConfig,
            protocolParameters: capture.protocolParameters,
            costModels: CML.CostModels.from_cbor_hex(capture.costModelsCborHex),
          },
        }),
      ).rejects.toMatchObject({
        message: expect.stringMatching(/^Error evaluated/u),
      });
    }, 120_000);
  },
);

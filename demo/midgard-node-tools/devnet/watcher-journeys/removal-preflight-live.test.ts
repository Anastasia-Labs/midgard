import { join } from "node:path";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  resolveProverSigner,
  submitRemoveFraudulentBlock,
} from "@al-ft/midgard-fault-proofs";
import { CML, Lucid, utxoToCore } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import * as UPLC from "@lucid-evolution/uplc";
import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock } from "./fixture.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const selected = process.env.MIDGARD_WATCHER_JOURNEY_CAPTURE_REMOVAL === "1";

it.skipIf(runDirectory === undefined || !selected)(
  "captures and evaluates retained removal without signing or submission",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const staged = await readJourneyArtifact<{ current: JourneyBlock }>(
      join(runDirectory!, "work/journeys/transition-trace/staged.json"),
    );
    const captured: unknown[] = [];
    const scalus = createScalusEvaluator();
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: context.accounts.publisher.seedPhrase,
    });
    const lucid = await Lucid(context.provider, "Custom", {
      slotConfig: context.customNetwork.slotConfig,
      evaluator: {
        name: "retained-removal-capture",
        evaluate: async (input) => {
          const tx = CML.Transaction.from_cbor_hex(input.tx);
          const body = tx.body();
          const inputs = body.inputs();
          const orderedInputs = Array.from(
            { length: inputs.len() },
            (_, index) => {
              const value = inputs.get(index);
              const outRef = `${value.transaction_id().to_hex()}#${value.index()}`;
              const resolved = input.additionalUTxOs.find(
                (utxo) => `${utxo.txHash}#${utxo.outputIndex}` === outRef,
              );
              return { index, outRef, resolved };
            },
          );
          const evaluate = async (action: () => unknown | Promise<unknown>) => {
            try {
              await action();
              return { outcome: "passed" };
            } catch (cause) {
              return {
                outcome: "failed",
                detail: formatUnknownError(cause, { includeCause: true }),
              };
            }
          };
          const aiken = await evaluate(() => {
            const resolved = input.additionalUTxOs.map(utxoToCore);
            return UPLC.eval_phase_two_raw(
              tx.to_cbor_bytes(),
              resolved.map((utxo) => utxo.input().to_cbor_bytes()),
              resolved.map((utxo) => utxo.output().to_cbor_bytes()),
              input.context.costModels.to_cbor_bytes(),
              input.context.protocolParameters.maxTxExSteps,
              input.context.protocolParameters.maxTxExMem,
              BigInt(input.context.slotConfig.zeroTime),
              BigInt(input.context.slotConfig.zeroSlot),
              input.context.slotConfig.slotLength,
            );
          });
          const scalusResult = await evaluate(() => scalus.evaluate(input));
          captured.push({
            transactionCborHex: input.tx,
            transactionHash: CML.hash_transaction(body).to_hex(),
            orderedInputs,
            additionalUTxOs: input.additionalUTxOs,
            slotConfig: input.context.slotConfig,
            protocolParameters: input.context.protocolParameters,
            costModelsCborHex: input.context.costModels.to_cbor_hex(),
            validityStartSlot: body.validity_interval_start(),
            validityEndSlot: body.ttl(),
            aiken,
            scalus: scalusResult,
          });
          await writeJourneyArtifact(
            join(
              runDirectory!,
              "work/removal-preflight-latest-evaluation.json",
            ),
            {
              deploymentFingerprint: context.deployment.manifest.manifestId,
              captured,
              signed: false,
              submitted: false,
            },
          );
          console.info("Retained removal evaluation", {
            inputs: orderedInputs.map(({ index, outRef, resolved }) => ({
              index,
              outRef,
              address: resolved?.address,
            })),
            aiken,
            scalus: scalusResult,
          });
          throw new Error("Retained removal captured before signing");
        },
      },
    });
    signer.selectWallet(lucid);
    await expect(
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: JSON.parse(context.deployment.blueprintJson),
        deploymentInfo: context.deployment.manifest,
        network: "Custom",
        signer,
        fraudCategory: "transitionTrace",
        fraudulentHeaderHash: staged.current.headerHash,
        requireReferenceScripts: true,
        awaitConfirmation: false,
        preSubmitBoundary: async () => {
          throw new Error("Retained removal capture must never reach signing");
        },
      }),
    ).rejects.toThrow("Retained removal captured before signing");
    expect(captured).toHaveLength(1);
  },
  120_000,
);

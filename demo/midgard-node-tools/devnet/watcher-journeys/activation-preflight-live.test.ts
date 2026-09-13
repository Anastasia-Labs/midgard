import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Lucid } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";
import { activateOperatorProgram } from "midgard-node/transactions/register-active-operator";
import { expect, it } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const selected = process.env.MIDGARD_WATCHER_JOURNEY_CAPTURE_ACTIVATION === "1";

it.skipIf(runDirectory === undefined || !selected)(
  "evaluates the retained successor activation before signing",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const evaluator = createScalusEvaluator();
    let captured = false;
    const lucid = await Lucid(context.provider, "Custom", {
      slotConfig: context.customNetwork.slotConfig,
      evaluator: {
        name: "retained-activation-capture",
        evaluate: async (input) => {
          const evaluated = await evaluator.evaluate(input);
          const transaction = CML.Transaction.from_cbor_hex(input.tx);
          const body = transaction.body();
          const txHash = CML.hash_transaction(body).to_hex();
          const outputs = body.outputs();
          await writeJourneyArtifact(
            join(runDirectory!, "work/activation-preflight-evaluation.json"),
            {
              deploymentFingerprint: context.deployment.manifest.manifestId,
              transactionCborHex: input.tx,
              transactionHash: txHash,
              additionalUTxOs: input.additionalUTxOs,
              projectedOutputs: Array.from(
                { length: outputs.len() },
                (_, index) => ({
                  ...coreToTxOutput(outputs.get(index)),
                  txHash,
                  outputIndex: index,
                }),
              ),
              slotConfig: input.context.slotConfig,
              protocolParameters: input.context.protocolParameters,
              costModelsCborHex: input.context.costModels.to_cbor_hex(),
              evaluated,
              signed: false,
              submitted: false,
            },
          );
          captured = true;
          throw new Error("Retained activation captured before signing");
        },
      },
    });
    lucid.selectWallet.fromSeed(context.accounts.publisher.seedPhrase);
    await expect(
      Effect.runPromise(
        activateOperatorProgram(
          lucid,
          context.deployment.contracts,
          SDK.getProtocolParameters("Preprod").required_bond,
          context.deployment.publisherLucid,
          await context.deployment.publisherLucid.wallet().address(),
        ),
      ),
    ).rejects.toThrow("Retained activation captured before signing");
    expect(captured).toBe(true);
  },
  120_000,
);

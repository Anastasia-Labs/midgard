import { join } from "node:path";

import { Lucid, validatorToScriptHash } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { expect, it } from "vitest";

import { loadJourneyContext } from "./live-context.js";
import {
  JOURNEY_NATIVE_REFERENCE_HASH,
  publishJourneyNativeScriptReference,
} from "./native-script-publication.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "publishes the journaled native script prerequisite on the retained deployment",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const lucid = await Lucid(context.provider, "Custom", {
      slotConfig: context.customNetwork.slotConfig,
      evaluator: createScalusEvaluator(),
    });
    lucid.selectWallet.fromSeed(context.accounts.cosigner.seedPhrase);
    const publication = await publishJourneyNativeScriptReference({
      lucid,
      network: "Custom",
      deploymentFingerprint: context.deployment.manifest.manifestId,
      journalPath: join(
        context.runDirectory,
        "work/native-script-publication.json",
      ),
    });
    expect(validatorToScriptHash(publication.reference.scriptRef!)).toBe(
      JOURNEY_NATIVE_REFERENCE_HASH,
    );
    expect(publication.reference.address).not.toBe(
      await lucid.wallet().address(),
    );
    console.info("Canonical native script prerequisite published", {
      txHash: publication.txHash,
      outputIndex: publication.reference.outputIndex,
      scriptHash: publication.scriptHash,
    });
  },
  600_000,
);

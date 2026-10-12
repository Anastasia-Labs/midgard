import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  bindFraudProofWorkflowDeployment,
  resolveProverSigner,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
} from "@al-ft/midgard-fault-proofs";
import {
  captureTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";
import {
  loadWatcherSecretText,
  parseWatcherProcessConfig,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  journeyFollowedScripts,
  journeyFollowerAuthority,
  journeyFollowerNode,
  openJourneyFollowerL1,
} from "./journey-follower-l1.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined).each([false, true])(
  "captures canonical trace event history with real head advancement=%s",
  async (advanceHead) => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const config = parseWatcherProcessConfig(
      JSON.parse(
        await readFile(join(directory, "watcher-process.json"), "utf8"),
      ),
    ).watcherConfig;
    const secret = await loadWatcherSecretText(config.proverWallet.keySource);
    const signer = resolveProverSigner(
      secret.startsWith("ed25519_sk")
        ? { network: config.targetNetwork, walletPrivateKey: secret }
        : { network: config.targetNetwork, walletSeedPhrase: secret },
      Object.freeze({}),
    );
    const staged = await readJourneyArtifact<{
      current: { headerHash: string };
      depositEvent: { txHash: string; outputIndex: number };
    }>(join(directory, "staged.json"));
    const { deployment } = context;
    const binding = await bindFraudProofWorkflowDeployment({
      manifest: deployment.manifest,
      blueprintJson: deployment.blueprintJson,
      deploymentInfo: deployment.deploymentInfo,
      category: "transitionTrace",
      headerHash: staged.current.headerHash,
      proverCredential: signer.paymentKeyHash,
      stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
    });
    const started = performance.now();
    const follower = await openJourneyFollowerL1({
      authority: journeyFollowerAuthority(deployment.manifest),
      followedScripts: journeyFollowedScripts(deployment),
      node: journeyFollowerNode(context),
      origin: config.l1.origin,
      automaticRecoveryMaxDepth:
        binding.releaseFinality.policy.automaticRecoveryMaxDepth,
      storeDirectory: directory,
    });
    const capture = async () =>
      await captureTransitionTraceL1Events({
        binding,
        authority: follower.l1
          .source("journey-trace-event-acquisition")
          .snapshotAuthority({
            releaseFinality: binding.releaseFinality,
            observationDepth: "inclusion",
          }),
      });
    let advancedHead: { previous: number; current: number } | undefined;
    let outcome: unknown;
    try {
      let events = await capture();
      if (advanceHead) {
        // Capture again once the real producer has added a block: the
        // follower's facts moved, and the capture still admits the event.
        const previous = follower.height();
        await follower.atTip(previous);
        advancedHead = { previous, current: follower.height() };
        events = await capture();
      }
      const admitted = requireTransitionTraceL1Events(events);
      expect(
        admitted.events.some(
          ({ kind, utxo }) =>
            kind === "deposit" &&
            utxo.txHash === staged.depositEvent.txHash &&
            utxo.outputIndex === staged.depositEvent.outputIndex,
        ),
      ).toBe(true);
      if (advanceHead) {
        expect(advancedHead).toBeDefined();
        expect(advancedHead!.current).toBeGreaterThan(advancedHead!.previous);
      }
      outcome = { status: "passed", events };
      console.info("Actual trace event acquisition passed", outcome);
    } catch (error) {
      outcome = {
        status: "failed",
        error: error instanceof Error ? error.message : String(error),
        stack: error instanceof Error ? error.stack : undefined,
      };
      throw error;
    } finally {
      await follower.close();
      await writeJourneyArtifact(
        join(
          runDirectory!,
          advanceHead
            ? "work/trace-event-head-advance-evidence.json"
            : "work/trace-event-acquisition-evidence.json",
        ),
        {
          deploymentFingerprint: deployment.manifest.manifestId,
          observedAt: new Date().toISOString(),
          durationMs: performance.now() - started,
          outcome,
          advancedHead,
        },
      );
    }
  },
  180_000,
);

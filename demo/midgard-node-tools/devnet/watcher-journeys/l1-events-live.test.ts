import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  bindFraudProofWorkflowDeployment,
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  createLocalKupmiosHttpOgmiosRawSource,
  resolveProverSigner,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
} from "@al-ft/midgard-fault-proofs";
import {
  captureTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";
import { loadWatcherSecretText, parseWatcherConfig } from "midgard-watcher";
import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined).each([false, true])(
  "captures canonical trace event history with real head advancement=%s",
  async (advanceHead) => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const config = parseWatcherConfig(
      JSON.parse(await readFile(join(directory, "watcher.json"), "utf8")),
    );
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
    if (config.l1.source.sourceMode !== "local_node")
      throw new Error(
        "Event probe requires the actual local-node configuration",
      );
    const { queryServices } = config.l1.source;
    const endpoint = (kind: "kupo" | "ogmios") => {
      const service = queryServices.find((service) => service.kind === kind);
      if (service === undefined) throw new Error(`Missing ${kind} endpoint`);
      return service.endpoint;
    };
    const responses: {
      url: string;
      status: number;
      checkpoint: string | null;
      etag: string | null;
      elapsedMs: number;
    }[] = [];
    const started = performance.now();
    const source = createLocalKupmiosHttpOgmiosRawSource({
      sourceId: "journey-trace-event-acquisition",
      kupoHttpUrl: endpoint("kupo"),
      ogmiosUrl: endpoint("ogmios"),
      releaseFinality: binding.releaseFinality,
      timeoutMs: config.l1.requestTimeoutMs,
      fetchImpl: async (input, init) => {
        const response = await fetch(input, init);
        responses.push({
          url: String(input),
          status: response.status,
          checkpoint: response.headers.get("x-most-recent-checkpoint"),
          etag: response.headers.get("etag"),
          elapsedMs: performance.now() - started,
        });
        return response;
      },
    });
    let boundaryReads = 0;
    let advancedHead: { previous: string; current: string } | undefined;
    const observedSource = {
      ...source,
      readBoundary: async () => {
        const boundary = await source.readBoundary();
        boundaryReads += 1;
        if (advanceHead && boundaryReads === 1) {
          const previous = responses.at(-1)?.etag;
          if (previous == null)
            throw new Error("No actual boundary response head");
          const deadline = performance.now() + 120_000;
          // Hold the captured boundary until the real producer advances. Every
          // response remains authentic; this controls only the read interleaving.
          for (;;) {
            const response = await fetch(`${endpoint("kupo")}/checkpoints`, {
              signal: AbortSignal.timeout(config.l1.requestTimeoutMs),
            });
            await response.arrayBuffer();
            const current = response.headers.get("etag");
            if (response.ok && current !== null && current !== previous) {
              advancedHead = { previous, current };
              break;
            }
            if (performance.now() >= deadline)
              throw new Error(
                "Real producer did not advance within the probe window",
              );
            await pause(500);
          }
        }
        return boundary;
      },
    };
    let outcome: unknown;
    try {
      const events = await captureTransitionTraceL1Events({
        binding,
        authority: createLocalKupmiosFraudProofRawL1SnapshotAuthority({
          source: observedSource,
          releaseFinality: binding.releaseFinality,
        }),
      });
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
        expect(boundaryReads).toBeGreaterThan(3);
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
          boundaryReads,
          advancedHead,
          responses,
        },
      );
    }
  },
  180_000,
);

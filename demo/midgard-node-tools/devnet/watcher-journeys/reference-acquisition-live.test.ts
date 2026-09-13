import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  createLocalKupmiosHttpOgmiosRawSource,
  type FraudProofRawL1WebSocketLike,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosBoundary,
} from "@al-ft/midgard-fault-proofs";
import { CML, utxoToCore } from "@lucid-evolution/lucid";
import {
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherConfig,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "reads every published reference through bounded real Kupo/Ogmios sessions",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const config = parseWatcherConfig(
      JSON.parse(await readFile(join(directory, "watcher.json"), "utf8")),
    );
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: join(directory, "deployment-authority.json"),
      ruleBundlePath: join(directory, "rules.json"),
    });
    const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
      authority.deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: context.deployment.manifest.manifestId,
    });
    if (config.l1.source.sourceMode !== "local_node")
      throw new Error(
        "Reference probe requires the actual local-node configuration",
      );
    const kupo = config.l1.source.queryServices.find(
      ({ kind }) => kind === "kupo",
    )!;
    const ogmios = config.l1.source.queryServices.find(
      ({ kind }) => kind === "ogmios",
    )!;
    let active = 0;
    let peak = 0;
    let opened = 0;
    const source = createLocalKupmiosHttpOgmiosRawSource({
      sourceId: "journey-reference-acquisition",
      kupoHttpUrl: kupo.endpoint,
      ogmiosUrl: ogmios.endpoint,
      releaseFinality,
      timeoutMs: config.l1.requestTimeoutMs,
      webSocketFactory: (url) => {
        // Observe real platform sockets through physical close; no response substitution.
        const socket = new WebSocket(url);
        active += 1;
        opened += 1;
        peak = Math.max(peak, active);
        socket.addEventListener(
          "close",
          () => {
            active -= 1;
          },
          { once: true },
        );
        return socket as unknown as FraudProofRawL1WebSocketLike;
      },
    });
    const started = performance.now();
    const boundary = await readAdmittedLocalKupmiosBoundary({ source });
    const references = [...context.deployment.references.values()];
    const addresses = [...new Set(references.map(({ address }) => address))];
    const captured = new Map<string, string>();
    for (const address of addresses) {
      const outputs = await readAdmittedLocalKupmiosAddressUtxosAtPoint({
        source,
        address,
        point: boundary.kupoCheckpoint,
      });
      for (const output of outputs)
        captured.set(output.outRef, output.outputCbor);
    }
    for (const reference of references) {
      const output = captured.get(
        `${reference.txHash}#${reference.outputIndex}`,
      );
      expect(output).toBeDefined();
      expect(
        CML.TransactionOutput.from_cbor_hex(output!).to_canonical_cbor_hex(),
      ).toBe(utxoToCore(reference).output().to_canonical_cbor_hex());
    }
    expect(active).toBe(0);
    expect(peak).toBeLessThanOrEqual(4);
    const evidence = {
      verifiedAt: new Date().toISOString(),
      deploymentFingerprint: context.deployment.manifest.manifestId,
      point: boundary.kupoCheckpoint,
      durationMs: performance.now() - started,
      references: references.length,
      walletOutputs: captured.size,
      openedSessions: opened,
      peakSessions: peak,
      activeSessionsAfterRead: active,
    };
    await writeJourneyArtifact(
      join(runDirectory!, "work/reference-acquisition-evidence.json"),
      evidence,
    );
    console.info("Actual reference acquisition passed", evidence);
  },
  180_000,
);

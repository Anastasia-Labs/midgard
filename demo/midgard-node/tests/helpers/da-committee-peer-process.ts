import { appendFile, readFile } from "node:fs/promises";

import type { Libp2pDaTransportConfig } from "../../../da-committee-node/src/config.js";
import {
  createDaLibp2pPayloadRequestHandlers,
  DaLibp2pNode,
  processWideDaPayloadSubmitAdmission,
} from "../../../da-committee-node/src/da/libp2p/index.js";
import { JsonFileWatcherStore } from "../../../da-committee-node/src/store.js";

type PeerProcessConfig = {
  readonly peerIndex: number;
  readonly privateKeySource: string;
  readonly storeDir: string;
  readonly metricsPath: string;
  readonly transport: Libp2pDaTransportConfig;
};

const configPath = process.argv[2];
if (configPath === undefined) throw new Error("missing peer config path");
const input = JSON.parse(
  await readFile(configPath, "utf8"),
) as PeerProcessConfig;
const store = await JsonFileWatcherStore.open(input.storeDir);
const handlers = new Map(
  createDaLibp2pPayloadRequestHandlers({
    deploymentFingerprint: input.transport.deploymentFingerprint,
    store,
    limits: input.transport.limits,
  }),
);
const submitProtocolId = [...handlers.keys()].find((key) =>
  key.includes("payload-submit"),
);
if (submitProtocolId === undefined) throw new Error("missing submit handler");
const submit = handlers.get(submitProtocolId)!;
handlers.set(submitProtocolId, async (context) => {
  const startedAt = performance.now();
  const rssBeforeBytes = process.memoryUsage().rss;
  let outcome = "completed";
  try {
    await submit(context);
  } catch (error) {
    outcome = error instanceof Error ? error.message : String(error);
    throw error;
  } finally {
    await appendFile(
      input.metricsPath,
      `${JSON.stringify({
        peerIndex: input.peerIndex,
        pid: process.pid,
        outcome,
        durationMs: performance.now() - startedAt,
        rssBeforeBytes,
        rssAfterBytes: process.memoryUsage().rss,
        peakRssBytes: process.resourceUsage().maxRSS * 1024,
        admissionPeakActive:
          processWideDaPayloadSubmitAdmission.maxObservedActive,
      })}\n`,
    );
  }
});

const node = new DaLibp2pNode({
  config: input.transport,
  privateKeySource: input.privateKeySource,
  requestHandlers: handlers,
});
await node.start();
process.stdout.write(`${JSON.stringify({ ready: true, pid: process.pid })}\n`);

let stopping = false;
const stop = async () => {
  if (stopping) return;
  stopping = true;
  try {
    await node.stop().catch(() => undefined);
  } finally {
    await store.close?.().catch(() => undefined);
    process.exit(0);
  }
};
process.once("SIGTERM", () => void stop());
process.once("SIGINT", () => void stop());
await new Promise(() => undefined);

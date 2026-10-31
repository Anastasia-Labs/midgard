import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { join } from "node:path";
import { inspect } from "node:util";

import {
  createWatcherRetainedDaRuntime,
  type VerifiedWatcherDeploymentIdentity,
  type WatcherConfig,
} from "midgard-watcher";

import { writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock } from "./fixture.js";

/** Fetch the exact staged bytes through the same production DA runtime as the watcher. */
export const verifyJourneyPublicDa = async (input: {
  directory: string;
  watcherConfig: WatcherConfig;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  predecessor: Pick<JourneyBlock, "headerHash" | "payloadEnvelopeCbor">;
  current: Pick<JourneyBlock, "headerHash" | "payloadEnvelopeCbor">;
}) => {
  const startedAt = new Date().toISOString();
  const started = performance.now();
  const receipts: {
    headerHash: string;
    sourcePeerId: string;
    bytes: number;
    payloadEnvelopeSha256: string;
    durationMs: number;
  }[] = [];
  const record = async (outcome: "passed" | "failed", error?: unknown) => {
    const evidence = {
      startedAt,
      finishedAt: new Date().toISOString(),
      durationMs: performance.now() - started,
      deploymentFingerprint: input.deploymentIdentity.manifestId,
      peers: input.watcherConfig.da.peers,
      outcome,
      receipts,
      ...(error === undefined
        ? {}
        : { error: error instanceof Error ? error.message : inspect(error) }),
    };
    await writeJourneyArtifact(
      join(input.directory, "public-da-preflight.json"),
      evidence,
    );
    return evidence;
  };
  try {
    // No test transport or availability fallback is installed in this process.
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: input.watcherConfig,
      deploymentIdentity: input.deploymentIdentity,
    });
    try {
      assert.equal(
        runtime.deploymentFingerprint,
        input.deploymentIdentity.manifestId,
      );
      assert.equal(runtime.sources.length, input.watcherConfig.da.peers.length);
      assert(
        runtime.sources.length > 0,
        "Public DA preflight has no configured peer",
      );
      for (const [index, source] of runtime.sources.entries()) {
        const peer = input.watcherConfig.da.peers[index]!;
        for (const block of [input.predecessor, input.current]) {
          const requestedAt = performance.now();
          const result = await source.fetchPayloadByHeaderHash(
            block.headerHash,
          );
          if (!result.ok)
            throw new Error(
              `Public DA failed for ${block.headerHash}: ${JSON.stringify(result)}`,
            );
          assert.equal(
            result.sourcePeerId,
            peer.peerId,
            "Public DA returned a different source peer",
          );
          assert(
            Buffer.from(result.payloadEnvelopeCbor).equals(
              Buffer.from(block.payloadEnvelopeCbor),
            ),
            `Public DA changed retained payload ${block.headerHash}`,
          );
          const receipt = {
            headerHash: block.headerHash,
            sourcePeerId: result.sourcePeerId,
            bytes: result.payloadEnvelopeCbor.length,
            payloadEnvelopeSha256: createHash("sha256")
              .update(result.payloadEnvelopeCbor)
              .digest("hex"),
            durationMs: performance.now() - requestedAt,
          };
          receipts.push(receipt);
          console.info(
            "Public DA preflight returned exact retained bytes",
            receipt,
          );
        }
      }
    } finally {
      await runtime.close();
    }
    return await record("passed");
  } catch (cause) {
    await record("failed", cause);
    throw cause;
  }
};

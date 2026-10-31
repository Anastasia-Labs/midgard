import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";
import { inspect } from "node:util";

import { retainedDaPayloadHeaderVerifier } from "@al-ft/midgard-fault-proofs";
import {
  createWatcherRetainedDaRuntime,
  parseWatcherProcessConfig,
  type VerifiedWatcherDeploymentIdentity,
  type WatcherConfig,
} from "midgard-watcher";

import { writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock } from "./fixture.js";

/** Opens the watcher's production retained-DA runtime. */
export type OpenJourneyPublicDaRuntime = typeof createWatcherRetainedDaRuntime;

/**
 * How long a fetch keeps asking while public DA does not serve a payload yet.
 * A committee's retained copy can trail its L1 apply, so an attached stack
 * waits; without one, a single answer decides.
 */
export type JourneyPublicDaAvailability = Readonly<{
  waitMs: number;
  now?: () => number;
  pause?: () => Promise<void>;
}>;

/**
 * How long an attached stack's public retained copy of an attested payload
 * may trail its L1 apply.
 */
export const STACK_PUBLIC_DA_TRAIL_MS = 120_000;

/** Asks until an answer serves the payload or the availability wait ends. */
const fetchUntilServed = async <Answer extends { readonly ok: boolean }>(
  fetch: () => Promise<Answer>,
  availability: JourneyPublicDaAvailability | undefined,
): Promise<Answer> => {
  const now = availability?.now ?? (() => performance.now());
  const pause = availability?.pause ?? (() => delay(2_000));
  const deadline = now() + (availability?.waitMs ?? 0);
  for (;;) {
    const answer = await fetch();
    if (answer.ok || now() >= deadline) return answer;
    await pause();
  }
};

/** The DA peers and limits of the stack watcher, from its process configuration. */
export const readStackJourneyWatcherConfig = async (
  processConfigPath: string,
): Promise<WatcherConfig> =>
  parseWatcherProcessConfig(
    JSON.parse(await readFile(processConfigPath, "utf8")),
  ).watcherConfig;

/**
 * The payload public DA serves for a header, checked to commit to that
 * header, from the first configured peer that serves it.
 */
export const fetchJourneyPublicDaPayload = async (input: {
  watcherConfig: WatcherConfig;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  headerHash: string;
  availability?: JourneyPublicDaAvailability;
  openRuntime?: OpenJourneyPublicDaRuntime;
}): Promise<{ payloadEnvelopeCbor: Buffer; sourcePeerId: string }> => {
  const runtime = await (input.openRuntime ?? createWatcherRetainedDaRuntime)({
    watcherConfig: input.watcherConfig,
    deploymentIdentity: input.deploymentIdentity,
  });
  try {
    if (runtime.sources.length === 0)
      throw new Error("Public DA has no configured peer");
    const verifyPayload = retainedDaPayloadHeaderVerifier(input.headerHash);
    const answer = await fetchUntilServed(async () => {
      const failures: unknown[] = [];
      for (const source of runtime.sources) {
        const result = await source.fetchPayloadByHeaderHash(input.headerHash, {
          verifyPayload,
        });
        if (result.ok)
          return {
            ok: true as const,
            payloadEnvelopeCbor: Buffer.from(result.payloadEnvelopeCbor),
            sourcePeerId: result.sourcePeerId,
          };
        failures.push(result);
      }
      return { ok: false as const, failures };
    }, input.availability);
    if (!answer.ok)
      throw new Error(
        `Public DA serves no payload for ${input.headerHash}: ${JSON.stringify(answer.failures)}`,
      );
    return {
      payloadEnvelopeCbor: answer.payloadEnvelopeCbor,
      sourcePeerId: answer.sourcePeerId,
    };
  } finally {
    await runtime.close();
  }
};

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

/**
 * An attached stack's public DA preflight: the stack watcher's own peers
 * serve the staged blocks' exact payloads, each fetched as the stack journey
 * fetches public DA (every peer, waiting while the retained copy trails its
 * apply).
 */
export const verifyStackJourneyPublicDa = async (input: {
  directory: string;
  watcherConfig: WatcherConfig;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  predecessor: Pick<JourneyBlock, "headerHash" | "payloadEnvelopeCbor">;
  current: Pick<JourneyBlock, "headerHash" | "payloadEnvelopeCbor">;
  availability?: JourneyPublicDaAvailability;
  openRuntime?: OpenJourneyPublicDaRuntime;
}) => {
  const startedAt = new Date().toISOString();
  const receipts: {
    headerHash: string;
    sourcePeerId: string;
    bytes: number;
    payloadEnvelopeSha256: string;
  }[] = [];
  const record = async (outcome: "passed" | "failed", error?: unknown) => {
    const evidence = {
      startedAt,
      finishedAt: new Date().toISOString(),
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
    for (const block of [input.predecessor, input.current]) {
      const served = await fetchJourneyPublicDaPayload({
        watcherConfig: input.watcherConfig,
        deploymentIdentity: input.deploymentIdentity,
        headerHash: block.headerHash,
        availability: input.availability ?? {
          waitMs: STACK_PUBLIC_DA_TRAIL_MS,
        },
        ...(input.openRuntime === undefined
          ? {}
          : { openRuntime: input.openRuntime }),
      });
      if (
        !served.payloadEnvelopeCbor.equals(
          Buffer.from(block.payloadEnvelopeCbor),
        )
      )
        throw new Error(`Public DA changed staged payload ${block.headerHash}`);
      receipts.push({
        headerHash: block.headerHash,
        sourcePeerId: served.sourcePeerId,
        bytes: served.payloadEnvelopeCbor.length,
        payloadEnvelopeSha256: createHash("sha256")
          .update(served.payloadEnvelopeCbor)
          .digest("hex"),
      });
    }
    return await record("passed");
  } catch (cause) {
    await record("failed", cause);
    throw cause;
  }
};

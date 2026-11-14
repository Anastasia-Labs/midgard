import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { writeTextFileAtomic } from "@al-ft/midgard-core/atomic-write";
import {
  retainedDaPayloadHeaderVerifier,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import {
  createWatcherRetainedDaRuntime,
  parseWatcherProcessConfig,
} from "midgard-watcher";

import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import type { StackProcesses } from "./process.js";
import { verifyStackRelease } from "./release.js";

export async function verifyPublicDa(
  processes: StackProcesses,
  headerHash: string,
) {
  if (!/^[0-9a-f]{56}$/.test(headerHash))
    throw new Error("Invalid committed header hash");
  const authority = await verifyStackRelease(processes);
  const path = join(processes.config.runDirectory, `da-${headerHash}`);
  const saved = (await readJsonIfPresent(`${path}.json`)) as
    | { sha256: string; headerHash: string; deploymentId: string }
    | undefined;
  if (saved) {
    const digest = createHash("sha256")
      .update(await readFile(`${path}.cbor`))
      .digest("hex");
    if (
      saved.sha256 !== digest ||
      saved.headerHash !== headerHash ||
      saved.deploymentId !== authority.deploymentIdentity.manifestId
    )
      throw new Error("Saved public DA bytes changed");
    return saved;
  }
  const processConfig = parseWatcherProcessConfig(
    await readJsonIfPresent(
      join(processes.config.watcher.configDirectory, "watcher-process.json"),
    ),
  );
  const runtime = await createWatcherRetainedDaRuntime({
    watcherConfig: processConfig.watcherConfig,
    deploymentIdentity: authority.deploymentIdentity,
  });
  try {
    const source = runtime.sources[0];
    if (!source) throw new Error("No public DA source");
    const result = await fetchCommittedPublicDa(
      source,
      headerHash,
      processes.config.timeoutMs,
    );
    await writeTextFileAtomic(`${path}.cbor`, result.payloadEnvelopeCbor, {
      mode: 0o600,
    });
    const receipt = {
      headerHash,
      peerId: result.sourcePeerId,
      sha256: createHash("sha256")
        .update(result.payloadEnvelopeCbor)
        .digest("hex"),
      bytes: result.payloadEnvelopeCbor.length,
      deploymentId: authority.deploymentIdentity.manifestId,
    };
    await writeDurableJson(`${path}.json`, receipt);
    return receipt;
  } finally {
    await runtime.close();
  }
}

/**
 * Fetches a committed block's payload from public DA, checked to commit to
 * its header. The node reports a block committed before it has handed the
 * payload to the committee, whose store public DA serves, so an answer of
 * not_found from every peer is asked again until `timeoutMs` runs out. Any
 * other failure fails at once: a conflicting, invalid or refused copy is not
 * a delay, and an unreachable server is a stack fault, not a commit race.
 */
export async function fetchCommittedPublicDa(
  source: Pick<RetainedDaPayloadSource, "fetchPayloadByHeaderHash">,
  headerHash: string,
  timeoutMs: number,
) {
  const verifyPayload = retainedDaPayloadHeaderVerifier(headerHash);
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const result = await source.fetchPayloadByHeaderHash(headerHash, {
      verifyPayload,
    });
    if (result.ok) return result;
    const notStoredYet =
      result.attempts.length > 0 &&
      result.attempts.every((attempt) => attempt.status === "not_found");
    if (!notStoredYet || Date.now() >= deadline)
      throw new Error(
        `Public DA retrieval failed for ${headerHash}: ${JSON.stringify(result.attempts)}`,
      );
    await new Promise((resolve) => setTimeout(resolve, 5_000));
  }
}

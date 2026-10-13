/**
 * Transitional `LedgerAt` source: reads the ledger after a block from that
 * block's retained DA payload. It exists only until the watcher keeps its own
 * replayed ledger (DS-3), which replaces it; then it is deleted together with
 * the payload's UTxO section. Like every `LedgerAt` answer, its result is
 * untrusted and admitted only by `checkLedgerAgainstRoot`.
 */

import { type LedgerAt, type LedgerEntry } from "./ledger-at.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadFetchResult,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "./transition-trace/reconstruct.js";

/** The payload's UTxO section as ledger entries, each with its trie descriptor. */
export const payloadLedgerEntries = (
  reconstruction: TransitionTraceReconstruction,
): LedgerEntry[] => {
  const descriptors = new Map(
    reconstruction.rootData.utxos.entries.map(({ key, value }) => [
      Buffer.from(key).toString("hex"),
      value,
    ]),
  );
  return reconstruction.utxos.map(({ key, value }) => {
    const descriptor = descriptors.get(Buffer.from(key).toString("hex"));
    return {
      outRef: Buffer.from(key),
      output: Buffer.from(value),
      ...(descriptor === undefined
        ? {}
        : { descriptor: Buffer.from(descriptor) }),
    };
  });
};

export const retainedPayloadLedgerAt =
  ({
    sources,
    retries,
  }: {
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly retries?: number | undefined;
  }): LedgerAt =>
  async (headerHash) => {
    const fetched = await fetchRetainedDaPayloadByHeaderHash({
      headerHash,
      sources,
      ...(retries === undefined ? {} : { retries }),
    });
    const reconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      expectedHeaderHash: headerHash,
    });
    return payloadLedgerEntries(reconstruction);
  };

/**
 * Evidence and ledger reads share the first verified payload for each header
 * within one classification. Create this source at the operation boundary;
 * failed reads are not captured, so the fetch owner's retries still apply.
 */
export const retainedPayloadReplaySource = ({
  sources,
  retries,
}: {
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number | undefined;
}): {
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly ledgerAt: LedgerAt;
} => {
  const captured = new Map<string, RetainedDaPayloadFetchResult>();
  const source: RetainedDaPayloadSource = {
    sourceId: "retained-replay-snapshot",
    fetchPayloadByHeaderHash: async (headerHash) => {
      let payload = captured.get(headerHash);
      if (payload === undefined) {
        const fetched = await fetchRetainedDaPayloadByHeaderHash({
          headerHash,
          sources,
          ...(retries === undefined ? {} : { retries }),
        });
        payload = {
          ...fetched,
          payloadEnvelopeCbor: Buffer.from(fetched.payloadEnvelopeCbor),
        };
        captured.set(headerHash, payload);
      }
      return {
        ...payload,
        ok: true,
        payloadEnvelopeCbor: Buffer.from(payload.payloadEnvelopeCbor),
      };
    },
  };
  const snapshotSources = [source];
  return {
    sources: snapshotSources,
    ledgerAt: retainedPayloadLedgerAt({ sources: snapshotSources, retries }),
  };
};

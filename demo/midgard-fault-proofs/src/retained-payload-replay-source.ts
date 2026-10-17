/**
 * One classification's view of the retained DA payloads: every read of a
 * header's payload within it sees the first verified fetch. Create it at the
 * operation boundary; failed reads are not captured, so the fetch owner's
 * retries still apply.
 */

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadFetchResult,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";

export const retainedPayloadReplaySource = ({
  sources,
  retries,
}: {
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number | undefined;
}): {
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly payloadAt: (headerHash: string) => Promise<SDK.DaPayload>;
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
  return {
    sources: [source],
    payloadAt: async (headerHash) => {
      const fetched = await source.fetchPayloadByHeaderHash(headerHash);
      if (!fetched.ok)
        throw new Error("verified retained snapshot refused its own payload");
      const payload = await unwrapDaPayload(fetched.payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      });
      return SDK.decodeDaPayload(Buffer.from(payload.innerBytes));
    },
  };
};

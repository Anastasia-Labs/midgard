/**
 * One classification's view of the retained DA payloads: every read of a
 * header's payload within it sees the first verified fetch. Create it at the
 * operation boundary; failed reads are not captured, so the fetch owner's
 * retries still apply.
 *
 * With `availability`, a payload is checked against the availability
 * commitment its block's state-queue node carries
 * (`SDK.daPayloadAvailabilityBinding`): once the node carries one, a copy
 * that does not rebuild it is a failed attempt for its peer, and the fetch
 * goes on to the next. While the node is Unattested the copy is unbound;
 * `binding.release` drops an unbound copy its replay refused, and later
 * fetches of that header refuse those bytes until the node carries a
 * commitment.
 */
import { createHash } from "node:crypto";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadFetchResult,
  type RetainedDaPayloadSource,
  type RetainedDaPayloadVerifier,
} from "./transition-trace/fetch.js";

/** Released unbound copies, by header: kept across operations. */
export type RetainedPayloadReleases = Map<string, Set<string>>;
const RELEASES_LIMIT = 1_024;
const RELEASED_PER_HEADER = 64;

export type RetainedPayloadAvailability = Readonly<{
  /** The availability status of the block's state-queue node. */
  status: (headerHash: string) => Promise<SDK.DaAvailabilityStateQueueStatus>;
  /** The deployment's commitment authority. */
  authority: () => SDK.DaPayloadAvailabilityAuthority;
  /** The digests of unbound copies a replay refused, by header. */
  releases: RetainedPayloadReleases;
}>;

const digest = (bytes: Uint8Array) =>
  createHash("sha256").update(bytes).digest("hex");

const refuse = (reason: string) => ({ ok: false as const, reason });

/** How `headerHash`'s copies are checked, and whether they are unbound. */
const bindingOf = async (
  availability: RetainedPayloadAvailability | undefined,
  headerHash: string,
): Promise<{ unbound: boolean; verify?: RetainedDaPayloadVerifier }> => {
  if (availability === undefined) return { unbound: false };
  const status = await availability.status(headerHash);
  if (!SDK.daAvailabilityStatusBinds(status)) {
    const released = availability.releases.get(headerHash);
    return {
      unbound: true,
      verify: async (payload) =>
        released?.has(digest(payload)) === true
          ? refuse("this unbound copy did not replay")
          : { ok: true },
    };
  }
  availability.releases.delete(headerHash);
  const authority = availability.authority();
  return {
    unbound: false,
    verify: async (payload) =>
      SDK.daPayloadAvailabilityBinding({
        status,
        authority,
        headerHash,
        payload,
      }) === "bound"
        ? { ok: true }
        : refuse("payload differs from the attested payload commitment"),
  };
};

export const retainedPayloadReplaySource = ({
  sources,
  retries,
  availability,
}: {
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number | undefined;
  readonly availability?: RetainedPayloadAvailability | undefined;
}): {
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly payloadAt: (headerHash: string) => Promise<SDK.DaPayload>;
  /** The unbound copies this view served, and their release. */
  readonly binding: Readonly<{
    unbound: (headerHash: string) => boolean;
    release: (headerHash: string) => void;
  }>;
} => {
  const captured = new Map<string, RetainedDaPayloadFetchResult>();
  const unbound = new Set<string>();
  const source: RetainedDaPayloadSource = {
    sourceId: "retained-replay-snapshot",
    fetchPayloadByHeaderHash: async (headerHash, options) => {
      let payload = captured.get(headerHash);
      // A caller's own check (a wrapping view's) is applied to the captured
      // copy as well; one it refuses is fetched again under that check.
      if (
        payload !== undefined &&
        options?.verifyPayload !== undefined &&
        !(await options.verifyPayload(payload.payloadEnvelopeCbor)).ok
      )
        payload = undefined;
      if (payload === undefined) {
        const binding = await bindingOf(availability, headerHash);
        const own = binding.verify;
        const further = options?.verifyPayload;
        const verifyPayload: RetainedDaPayloadVerifier | undefined =
          own === undefined || further === undefined
            ? (own ?? further)
            : async (bytes) => {
                const verdict = await own(bytes);
                return verdict.ok ? further(bytes) : verdict;
              };
        const fetched = await fetchRetainedDaPayloadByHeaderHash({
          headerHash,
          sources,
          ...(retries === undefined ? {} : { retries }),
          ...(verifyPayload === undefined ? {} : { verifyPayload }),
        });
        payload = {
          ...fetched,
          payloadEnvelopeCbor: Buffer.from(fetched.payloadEnvelopeCbor),
        };
        captured.set(headerHash, payload);
        if (binding.unbound) unbound.add(headerHash);
        else unbound.delete(headerHash);
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
    binding: {
      unbound: (headerHash) => unbound.has(headerHash),
      release: (headerHash) => {
        const copy = captured.get(headerHash);
        captured.delete(headerHash);
        unbound.delete(headerHash);
        if (availability === undefined || copy === undefined) return;
        const { releases } = availability;
        const released = releases.get(headerHash) ?? new Set<string>();
        released.add(digest(copy.payloadEnvelopeCbor));
        for (const oldest of released) {
          if (released.size <= RELEASED_PER_HEADER) break;
          released.delete(oldest);
        }
        releases.delete(headerHash);
        releases.set(headerHash, released);
        for (const oldest of releases.keys()) {
          if (releases.size <= RELEASES_LIMIT) break;
          releases.delete(oldest);
        }
      },
    },
  };
};

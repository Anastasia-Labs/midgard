// The committee's F8 soak plugin (plan §14). It reads the current committee
// scanner and is deleted with it at the C1 cutover.
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import {
  type ShadowComparator,
  type ShadowContext,
  type ShadowPlugin,
  type ShadowReading,
  unavailable,
} from "@al-ft/midgard-l1-follower/shadow";
import { getAddressDetails } from "@lucid-evolution/lucid";

import { loadCommitteeConfig } from "../../config.js";
import type { ObservedStateQueueSnapshot } from "../../domain.js";
import { committeeProjection } from "../follower/projection.js";
import type { CommitteeQueueParameters } from "../follower/queue-derivation.js";
import { providerFromUrl } from "../provider.provider-from-url.js";
import {
  committeeComparator,
  type CurrentScanIdentity,
  factFedSnapshot,
} from "./comparator.js";

/**
 * The committee's configuration comes from the committee's own environment
 * (`loadCommitteeConfig`): the soak runs beside a configured committee, and
 * the projections must be built at module load, before the soak passes the
 * plugin its options.
 */
const config = await loadCommitteeConfig(process.env);

const queue: CommitteeQueueParameters = {
  stateQueueAddress: Buffer.from(
    getAddressDetails(config.stateQueueAddress).address.hex,
    "hex",
  ),
  stateQueuePolicyId: config.stateQueuePolicyId.toLowerCase(),
};

const identity: CurrentScanIdentity = {
  deploymentFingerprint: config.deploymentFingerprint,
  deploymentIdentityDigest: config.deploymentFingerprint,
  stateQueuePolicyId: config.stateQueuePolicyId,
  daAttestationPolicyId: config.daAttestationPolicyId,
  finalityDepth: config.finalityDepth,
  automaticRecoveryMaxDepth: config.automaticRecoveryMaxDepth,
};

/** The plugin's `options` in `soak.json`. */
type Options = Readonly<{
  /** k (default: the manifest's automaticRecoveryMaxDepth, plan §1 terms). */
  securityParameter?: number;
  /** Also compare against the current Kupo/Ogmios provider (default true). */
  live?: boolean;
}>;

const readOptions = (value: unknown): Options => {
  if (typeof value !== "object" || value === null)
    throw new Error("committee soak plugin: options must be an object");
  const k = (value as Record<string, unknown>).securityParameter;
  const live = (value as Record<string, unknown>).live;
  if (
    k !== undefined &&
    (typeof k !== "number" || !Number.isSafeInteger(k) || k <= 0)
  )
    throw new Error(
      "committee soak plugin: options.securityParameter must be a positive integer",
    );
  if (live !== undefined && typeof live !== "boolean")
    throw new Error("committee soak plugin: options.live must be a boolean");
  return {
    ...(k === undefined ? {} : { securityParameter: k }),
    ...(live === undefined ? {} : { live }),
  };
};

const samePoint = (
  point: Readonly<{ slot: number; blockHash: string }>,
  context: ShadowContext,
): boolean =>
  point.slot === context.at.point.slot &&
  point.blockHash.toLowerCase() === context.at.point.hash.toString("hex");

/**
 * The current provider's snapshot, used only when it was read at the very
 * block the store's cursor is at: the provider's chain point is the cursor
 * before and after the read, and the snapshot's tip height is the cursor's.
 * Any other read is a different block, not a disagreement.
 */
const liveSnapshot = (url: string) => {
  // One provider for the soak; a failed construction is retried next block.
  let opened: ReturnType<typeof providerFromUrl> | undefined;
  return async (
    context: ShadowContext,
  ): Promise<ObservedStateQueueSnapshot | ShadowReading> => {
    opened ??= providerFromUrl(url, config).catch((error: unknown) => {
      opened = undefined;
      throw error;
    });
    const provider = await opened;
    if (
      provider.fetchStateQueueSnapshot === undefined ||
      !("currentChainPoint" in provider)
    )
      return unavailable(`${url} serves no state-queue snapshot`);
    const pointed = provider as typeof provider & {
      currentChainPoint(): Promise<{ slot: number; blockHash: string }>;
    };
    if (!samePoint(await pointed.currentChainPoint(), context))
      return unavailable("the current provider is not at the compared block");
    const snapshot = await provider.fetchStateQueueSnapshot();
    if (!samePoint(await pointed.currentChainPoint(), context))
      return unavailable("the current provider moved during the read");
    if (snapshot.tipBlockNo !== context.at.height)
      return unavailable("the snapshot's tip is not the compared block");
    return snapshot;
  };
};

const plugin: ShadowPlugin = {
  role: "committee",
  projections: [committeeProjection(queue)],
  comparators: async (env): Promise<readonly ShadowComparator[]> => {
    const options = readOptions(env.options);
    const slotTime = await new L1FollowerProvider({
      store: env.store,
      transport: env.transport,
    }).slotConfig();
    const shared = {
      parameters: {
        confirmationDepth: config.finalityDepth,
        securityParameter:
          options.securityParameter ?? config.automaticRecoveryMaxDepth,
      },
      slotTime,
      identity,
    };
    const comparators: ShadowComparator[] = [
      committeeComparator({
        ...shared,
        name: "queue-logic",
        snapshot: (context) => factFedSnapshot(context.store, context, queue),
      }),
    ];
    const url = config.cardanoProviderUrls.find((candidate) =>
      candidate.startsWith("kupmios:"),
    );
    if (options.live !== false && url !== undefined)
      comparators.push(
        committeeComparator({
          ...shared,
          name: "queue-live",
          snapshot: liveSnapshot(url),
        }),
      );
    return comparators;
  },
};

export default plugin;

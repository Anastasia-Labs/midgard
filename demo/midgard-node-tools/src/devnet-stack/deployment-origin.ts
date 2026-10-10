import type { L1Origin } from "@al-ft/midgard-core/l1-origin";

import {
  type DerivedL1Origin,
  deriveL1Origin,
  L1OriginUndeterminedError,
} from "../l1-origin.js";
import type { DeployContext } from "./deploy.js";
import { Journal } from "./journal.js";
import type { Layout } from "./layout.js";
import type { HubOracleOneShot } from "./node-env.js";

const JOURNAL_KEY = "l1Origin";

type OriginRecord = DerivedL1Origin & { readonly recordedAt: string };

const recordFor = (
  layout: Layout,
  oneShot: HubOracleOneShot,
): OriginRecord | undefined => {
  const recorded = new Journal(layout.journal).get<OriginRecord>(JOURNAL_KEY);
  return recorded?.nonceTxHash === oneShot.txHash.toLowerCase()
    ? recorded
    : undefined;
};

/**
 * Records the run's L1 origin once its hub-oracle nonce is on chain: the
 * point immediately before the nonce tx's block, found by scanning the run's
 * own chain from genesis (`find-origin`). A record for another nonce is
 * replaced. `derive` replaces the scan in tests.
 */
export const ensureL1Origin = async (
  context: Pick<DeployContext, "layout" | "run" | "artifacts">,
  oneShot: HubOracleOneShot,
  derive: typeof deriveL1Origin = deriveL1Origin,
): Promise<L1Origin> => {
  const recorded = recordFor(context.layout, oneShot);
  if (recorded !== undefined) return recorded.origin;
  const derived = await derive({
    node: {
      socketPath: context.layout.cardanoSocket,
      binaryPath: context.artifacts.transportBinary,
      networkMagic: context.run.networkMagic,
    },
    nonceTxHash: oneShot.txHash,
  });
  new Journal(context.layout.journal).set(JOURNAL_KEY, {
    ...derived,
    recordedAt: new Date().toISOString(),
  } satisfies OriginRecord);
  return derived.origin;
};

/** The run's recorded L1 origin for `oneShot`, for every node `listen`. */
export const recordedL1Origin = (
  layout: Layout,
  oneShot: HubOracleOneShot,
): L1Origin => {
  const recorded = recordFor(layout, oneShot);
  if (recorded === undefined)
    throw new L1OriginUndeterminedError(
      `${layout.runDir} records no L1 origin for the hub-oracle nonce ${oneShot.txHash}; run up first`,
    );
  return recorded.origin;
};

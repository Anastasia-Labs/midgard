import type { L1Origin } from "@al-ft/midgard-core/l1-origin";

import {
  type DerivedL1Origin,
  deriveL1Origin,
  L1OriginUndeterminedError,
  runStateL1Origin,
  sameL1Origin,
} from "../l1-origin.js";
import type { DeployContext } from "./deploy.js";
import { Journal } from "./journal.js";
import type { Layout } from "./layout.js";
import type { HubOracleOneShot } from "./node-env.js";

const JOURNAL_KEY = "l1Origin";

type OriginRecord = DerivedL1Origin & { readonly recordedAt: string };

/**
 * A shape-only stand-in for the run's recorded origin, so the configuration
 * preflights can run the services' parsers before the deployment exists.
 * Never written; each service reads the recorded origin when it starts.
 */
export const PREFLIGHT_L1_ORIGIN: L1Origin = {
  slot: 1,
  blockHash: "00".repeat(32),
};

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
 * origin T the nonce command recorded in the run state before it signed,
 * proven on the run's own chain to precede the nonce tx's block
 * (`find-origin --recorded`). A record for another nonce or another origin
 * is replaced; a run state without T is refused. `derive` replaces the scan
 * in tests.
 */
export const ensureL1Origin = async (
  context: Pick<DeployContext, "layout" | "run" | "artifacts">,
  oneShot: HubOracleOneShot,
  derive: typeof deriveL1Origin = deriveL1Origin,
): Promise<L1Origin> => {
  const runOrigin = await runStateL1Origin(
    context.layout.deploymentRunState,
    oneShot.txHash,
  );
  const recorded = recordFor(context.layout, oneShot);
  if (recorded !== undefined && sameL1Origin(recorded.origin, runOrigin))
    return recorded.origin;
  const derived = await derive({
    node: {
      socketPath: context.layout.cardanoSocket,
      binaryPath: context.artifacts.transportBinary,
      networkMagic: context.run.networkMagic,
    },
    nonceTxHash: oneShot.txHash,
    recorded: runOrigin,
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

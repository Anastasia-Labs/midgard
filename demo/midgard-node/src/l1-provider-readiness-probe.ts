import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { Effect } from "effect";

/**
 * Upper bound on one L1 provider readiness probe: the exact HubOracle +
 * local-Ogmios read the background refresher runs under the control plane,
 * and the raw-provider preflight `/readyz` runs between refreshes.
 */
export const READINESS_L1_PROVIDER_PROBE_TIMEOUT_MS = 2_000;

/**
 * The exact readiness probe: reads the HubOracle through Lucid, then the local
 * Ogmios submit slot, and yields that slot.
 */
export const runCombinedL1ReadinessProbe = <A, E1, R1, E2, R2>(
  hubOracleProbe: Effect.Effect<A, E1, R1>,
  localOgmiosSlotProbe: Effect.Effect<SubmitSlotSnapshot, E2, R2>,
): Effect.Effect<SubmitSlotSnapshot, E1 | E2, R1 | R2> =>
  hubOracleProbe.pipe(Effect.zipRight(localOgmiosSlotProbe));

import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Summary of the current on-chain state-queue topology.
 */
export type StateQueueTopology = {
  readonly policyUtxoCount: number;
  readonly parsedNodeCount: number;
  readonly invalidNodeCount: number;
  readonly rootCount: number;
  readonly tailCount: number;
  readonly initialized: boolean;
  readonly healthy: boolean;
  readonly reason: string | undefined;
};

/**
 * Derives a human-readable unhealthy reason for the observed topology.
 */
const deriveReason = (
  policyUtxoCount: number,
  invalidNodeCount: number,
  rootCount: number,
  tailCount: number,
): string | undefined => {
  if (policyUtxoCount === 0) {
    return undefined;
  }
  if (invalidNodeCount > 0) {
    return `Found ${invalidNodeCount} non-decodable state_queue UTxO(s) under the configured policy`;
  }
  if (rootCount !== 1) {
    return `Expected exactly 1 state_queue root node, found ${rootCount}`;
  }
  if (tailCount !== 1) {
    return `Expected exactly 1 state_queue tail node, found ${tailCount}`;
  }
  return undefined;
};

/**
 * Summarizes the topology health of the authenticated state-queue nodes.
 */
export const summarizeStateQueueTopology = (
  policyUtxoCount: number,
  nodes: readonly SDK.StateQueueUTxO[],
): StateQueueTopology => {
  const parsedNodeCount = nodes.length;
  const invalidNodeCount = Math.max(0, policyUtxoCount - parsedNodeCount);
  const rootCount = nodes.filter((node) => node.datum.key === "Empty").length;
  const tailCount = nodes.filter((node) => node.datum.next === "Empty").length;
  const initialized = policyUtxoCount > 0;
  const reason = deriveReason(
    policyUtxoCount,
    invalidNodeCount,
    rootCount,
    tailCount,
  );
  return {
    policyUtxoCount,
    parsedNodeCount,
    invalidNodeCount,
    rootCount,
    tailCount,
    initialized,
    healthy: initialized && reason === undefined,
    reason,
  };
};

/**
 * Formats a topology summary into a compact log/metric string.
 */
export const formatStateQueueTopology = (
  topology: StateQueueTopology,
): string =>
  `policy_utxos=${topology.policyUtxoCount},parsed_nodes=${topology.parsedNodeCount},invalid_nodes=${topology.invalidNodeCount},roots=${topology.rootCount},tails=${topology.tailCount},healthy=${topology.healthy}`;

/**
 * Fetches live state-queue UTxOs and derives a topology summary from them.
 */
export const fetchStateQueueTopologyProgram = (
  lucid: LucidEvolution,
  stateQueue: SDK.AuthenticatedValidator,
): Effect.Effect<StateQueueTopology, SDK.LucidError> =>
  Effect.gen(function* () {
    const policyUtxos = yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      stateQueue.spendingScriptAddress,
      stateQueue.policyId,
    );
    const parsed = yield* SDK.utxosToStateQueueUTxOs(
      policyUtxos.map((b) => b.utxo),
      stateQueue.policyId,
    );
    return summarizeStateQueueTopology(policyUtxos.length, parsed);
  });

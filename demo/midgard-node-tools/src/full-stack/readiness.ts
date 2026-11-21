/**
 * A committee is ours only when it serves this deployment from its own store
 * as the expected signer. Any other HTTP service on the port fails, including
 * another stack's committee and the node's own /readyz.
 */
export function committeeIsReady(
  value: unknown,
  signerIndex: number,
  expected: { manifestId: string; peerIds: readonly string[] },
) {
  const committee = value as
    | {
        ready?: boolean;
        deployment?: {
          configuredFingerprint?: string;
          storeMatchesConfigured?: boolean;
        };
        peer?: { signerIndex?: number; localPeerId?: string };
      }
    | undefined;
  return (
    committee?.ready === true &&
    committee.deployment?.configuredFingerprint === expected.manifestId &&
    committee.deployment.storeMatchesConfigured === true &&
    committee.peer?.signerIndex === signerIndex &&
    committee.peer.localPeerId === expected.peerIds[signerIndex]
  );
}

/**
 * Why a committee cannot become ready by waiting, or undefined. Its startup
 * failed and holds (`committee_startup_failed`), which only a restart clears;
 * or its L1 submitter wallet check failed, which a top-up clears once the
 * committee rechecks it.
 */
export function committeeStopReason(value: unknown) {
  const committee = value as
    | {
        ready?: boolean;
        reasons?: unknown;
        detail?: string;
        peer?: { l1SubmitterPreflight?: { status?: string; detail?: unknown } };
      }
    | undefined;
  if (committee?.ready !== false) return undefined;
  if (
    Array.isArray(committee.reasons) &&
    committee.reasons.includes("committee_startup_failed")
  )
    return `its startup failed and holds: ${committee.detail ?? "committee_startup_failed"}. Fix the cause and restart it`;
  const wallet = committee.peer?.l1SubmitterPreflight;
  if (wallet?.status === "failed")
    return `its L1 submitter wallet check failed: ${JSON.stringify(wallet.detail)}. Top up the wallet; the committee rechecks it`;
  return undefined;
}

/**
 * Whether `reasons` hold the node only for its operator awaiting activation.
 * The operator step activates it, and it runs once the services are up.
 */
export const onlyAwaitingActivation = (reasons: unknown) =>
  Array.isArray(reasons) &&
  reasons.length > 0 &&
  reasons.every((reason) => reason === "operator_not_yet_active");

/**
 * The node is ready, with no reasons but its operator awaiting activation,
 * and its settlement is healthy.
 */
export function nodeIsReady(value: unknown) {
  const node = value as
    | { ready?: boolean; reasons?: unknown[]; settlement?: { state?: string } }
    | undefined;
  if (node?.settlement?.state === "error")
    throw new Error("Automatic settlement is unhealthy");
  return (
    ((node?.ready === true && node.reasons?.length === 0) ||
      onlyAwaitingActivation(node?.reasons)) &&
    ["running", "waiting"].includes(node?.settlement?.state ?? "")
  );
}

/** Functional readiness requires each service's published readiness fields. */
export function stackIsReady(input: {
  node: unknown;
  watcher: unknown;
  committees: unknown[];
  manifestId: string;
  committeePeerIds: readonly string[];
}) {
  const watcher = input.watcher as
    | {
        liveness?: string;
        readiness?: string;
        readinessReasons?: unknown[];
        deploymentFingerprint?: string;
        launchScope?: { complete?: boolean };
      }
    | undefined;
  return (
    nodeIsReady(input.node) &&
    watcher?.liveness === "live" &&
    watcher.readiness === "ready" &&
    watcher.readinessReasons?.length === 0 &&
    watcher.deploymentFingerprint === input.manifestId &&
    watcher.launchScope?.complete === true &&
    input.committees.length > 0 &&
    input.committees.length === input.committeePeerIds.length &&
    input.committees.every((value, index) =>
      committeeIsReady(value, index, {
        manifestId: input.manifestId,
        peerIds: input.committeePeerIds,
      }),
    )
  );
}

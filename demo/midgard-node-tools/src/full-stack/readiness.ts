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

/** The node is ready, with no reasons, and its settlement is healthy. */
export function nodeIsReady(value: unknown) {
  const node = value as
    | { ready?: boolean; reasons?: unknown[]; settlement?: { state?: string } }
    | undefined;
  if (node?.settlement?.state === "error")
    throw new Error("Automatic settlement is unhealthy");
  return (
    node?.ready === true &&
    node.reasons?.length === 0 &&
    ["running", "waiting"].includes(node.settlement?.state ?? "")
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

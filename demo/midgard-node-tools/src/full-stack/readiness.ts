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

/** Functional readiness requires each service's published readiness fields. */
export function stackIsReady(input: {
  node: unknown;
  watcher: unknown;
  authority: unknown;
  committees: unknown[];
  manifestId: string;
  recordKeyId: string;
  committeePeerIds: readonly string[];
}) {
  const node = input.node as
    | { ready?: boolean; reasons?: unknown[]; settlement?: { state?: string } }
    | undefined;
  const watcher = input.watcher as
    | {
        liveness?: string;
        readiness?: string;
        readinessReasons?: unknown[];
        deploymentFingerprint?: string;
        launchScope?: { complete?: boolean };
      }
    | undefined;
  const authority = input.authority as
    | { recordAuthenticationKeyId?: string }
    | undefined;
  if (node?.settlement?.state === "error")
    throw new Error("Automatic settlement is unhealthy");
  return (
    node?.ready === true &&
    node.reasons?.length === 0 &&
    ["running", "waiting"].includes(node.settlement?.state ?? "") &&
    watcher?.liveness === "live" &&
    watcher.readiness === "ready" &&
    watcher.readinessReasons?.length === 0 &&
    watcher.deploymentFingerprint === input.manifestId &&
    watcher.launchScope?.complete === true &&
    authority?.recordAuthenticationKeyId === input.recordKeyId &&
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

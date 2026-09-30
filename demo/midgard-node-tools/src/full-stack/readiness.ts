/** Functional readiness requires each service's published readiness fields. */
export function stackIsReady(input: {
  node: unknown;
  watcher: unknown;
  authority: unknown;
  committees: unknown[];
  manifestId: string;
  recordKeyId: string;
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
    input.committees.every(
      (value) =>
        (value as { ready?: boolean; reasons?: unknown[] })?.ready === true,
    )
  );
}

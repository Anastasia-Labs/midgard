import type { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { deploymentIdentity } from "./classified-fault-fixture.js";

/** The reasons `/readyz` names for this supervisor alone. */
export const readyzReasons = async (
  supervisor: ReturnType<typeof createWatcherFaultProofSupervisor>,
): Promise<readonly string[]> => {
  const operations = createWatcherOperationsObservability({
    deploymentFingerprint: deploymentIdentity.manifestId,
    supervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 1,
      requiredCategoryCount: 1,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    durableProofQueueStatus: () => supervisor.durableQueueStatus(),
    nowMs: () => 100_000n,
  });
  const response = await handleWatcherOperationsHttpRequest(
    new Request("http://127.0.0.1/readyz"),
    operations.api,
  );
  return ((await response.json()) as { reasons: readonly string[] }).reasons;
};

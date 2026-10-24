import { AsyncLocalStorage } from "node:async_hooks";

import type { RetainedDaPayloadSource } from "@al-ft/midgard-fault-proofs";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import { assertWatcherL1AvailabilityPayloadSource } from "../availability/published-payload.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";

const reads = new AsyncLocalStorage<
  Readonly<{
    scope: DaAvailabilityReadScope;
    identity: VerifiedWatcherDeploymentIdentity;
    l1Source?: RetainedDaPayloadSource;
  }>
>();

/** Request-local context: a late continuation retains its own aborted scope. */
export const withWatcherRetainedDaReadScope = async <T>(
  input: NonNullable<ReturnType<typeof reads.getStore>>,
  read: () => Promise<T>,
): Promise<T> => {
  assertVerifiedWatcherDeploymentIdentity(input.identity);
  if (input.l1Source !== undefined)
    assertWatcherL1AvailabilityPayloadSource(input.l1Source, input.identity);
  input.scope.assertCurrent();
  return await reads.run(input, async () => {
    const result = await input.scope.read(read);
    input.scope.assertCurrent();
    return result;
  });
};

export const watcherRetainedDaReadScope = (deploymentFingerprint?: string) => {
  const input = reads.getStore();
  if (input !== undefined) {
    input.scope.assertCurrent();
    if (
      deploymentFingerprint !== undefined &&
      deploymentFingerprint !== input.identity.manifestId
    )
      throw new Error("Scoped DA read belongs to another deployment");
  }
  return input;
};

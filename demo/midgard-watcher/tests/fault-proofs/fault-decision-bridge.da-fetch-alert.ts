import {
  bindWatcherRetainedDaOperations,
  type WatcherRetainedDaOperationsBinding,
} from "../../src/storage/retained-da-runtime.retained-da-request-permits.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";
import type { operations } from "./fault-decision-bridge.released-fixture.js";

const bound: WatcherRetainedDaOperationsBinding[] = [];

/**
 * The `da_fetch_failure` alert of the fixture deployment reporting to
 * `observability`, as the watcher hands it to the fault decision bridge to
 * settle (`daFetchAlert`). No DA fetcher is registered with it; one is bound
 * at a time, until `closeDaFetchAlerts`.
 */
export const daFetchAlert = (observability: ReturnType<typeof operations>) => {
  const binding = bindWatcherRetainedDaOperations({
    deploymentIdentity: makeWatcherDeploymentAuthorityFixture().result,
    sink: observability.sink,
  });
  bound.push(binding);
  return binding;
};

export const closeDaFetchAlerts = (): void => {
  for (const binding of bound.splice(0)) binding.close();
};

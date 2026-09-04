import { describe, expect, it } from "vitest";

import {
  createExecutionNativeScriptInvalidWorkflowRunnerFactory,
  createExecutionNativeScriptInvalidWorkflowRunnerSurface,
  EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS,
  EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS,
} from "../src/execution-native-script-invalid/v1.js";

describe("executionNativeScriptInvalid production authority surface", () => {
  it("admits only infrastructure and the exact thirteen physical datums", () => {
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS).toHaveLength(13);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS).toEqual([
      "manifest",
      "blueprintJson",
      "deploymentInfo",
      "headerHash",
      "lucid",
      "signer",
      "source",
      "historicalCheckpointStore",
      "historicalSource",
      "stateQueueMutationLeaseCoordinator",
      "referenceScripts",
    ]);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS).not.toEqual(
      expect.arrayContaining(["evidence", "verdict", "actuator"]),
    );
  });

  it("exports the watcher loader-compatible transaction runner factory", () => {
    const loadRuntimeConfig = async () => {
      throw new Error("not reached by construction");
    };
    const surface = createExecutionNativeScriptInvalidWorkflowRunnerSurface({
      loadRuntimeConfig,
    });
    const factory = createExecutionNativeScriptInvalidWorkflowRunnerFactory({
      loadRuntimeConfig,
    });
    expect(surface.runnerVersion).toBe(
      "midgard-production-fraud-proof-workflow-runner-v1",
    );
    expect(factory.runnerVersion).toBe(surface.runnerVersion);
    expect(typeof surface.runOrResume).toBe("function");
  });
});

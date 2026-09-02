import { describe, expect, it } from "vitest";

import {
  createExecutionNativeScriptInvalidProductionWorkflowRunnerFactoryV1,
  createExecutionNativeScriptInvalidProductionWorkflowRunnerSurfaceV1,
  EXECUTION_NATIVE_SCRIPT_INVALID_PRODUCTION_CONFIG_KEYS_V1,
  EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS_V1,
} from "../src/execution-native-script-invalid/production-v1.js";

describe("executionNativeScriptInvalid production authority surface", () => {
  it("admits only infrastructure and the exact thirteen physical datums", () => {
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS_V1).toHaveLength(
      13,
    );
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_PRODUCTION_CONFIG_KEYS_V1).toEqual([
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
    expect(
      EXECUTION_NATIVE_SCRIPT_INVALID_PRODUCTION_CONFIG_KEYS_V1,
    ).not.toEqual(expect.arrayContaining(["evidence", "verdict", "actuator"]));
  });

  it("exports the watcher loader-compatible transaction runner factory", () => {
    const loadRuntimeConfig = async () => {
      throw new Error("not reached by construction");
    };
    const surface =
      createExecutionNativeScriptInvalidProductionWorkflowRunnerSurfaceV1({
        loadRuntimeConfig,
      });
    const factory =
      createExecutionNativeScriptInvalidProductionWorkflowRunnerFactoryV1({
        loadRuntimeConfig,
      });
    expect(surface.runnerVersion).toBe(
      "midgard-production-fraud-proof-workflow-runner-v1",
    );
    expect(factory.runnerVersion).toBe(surface.runnerVersion);
    expect(typeof surface.runOrResume).toBe("function");
  });
});

import {
  type OwnedProcessGroupCleanupResult,
  type OwnedProcessGroupSpec,
  processGroupHasLiveMembers,
} from "./process-ownership.parse-owned-process-group-record.js";
import {
  removeOwnedProcessGroupRecord,
  terminateOwnedProcessGroup,
  terminatingLeaderStillMatchesCoreIdentity,
  validateOwnedProcessGroupRecord,
  waitForOwnedProcessExit,
  waitForProcessGroupWithoutLiveMembers,
} from "./process-ownership.validate-owned-process-group-record.js";

/**
 * Reclaims a controller-owned detached group and removes its record only after
 * the recorded leader is absent. Any identity mismatch is preserved and
 * returned as a failure for operator inspection.
 */
export const cleanupOwnedProcessGroupAndRecord = async ({
  spec,
  gracefulTimeoutMs = 1_000,
}: {
  readonly spec: OwnedProcessGroupSpec;
  readonly gracefulTimeoutMs?: number;
}): Promise<OwnedProcessGroupCleanupResult> => {
  let validation = validateOwnedProcessGroupRecord(spec);
  if (validation.status === "process_missing") {
    removeOwnedProcessGroupRecord(spec.recordPath);
    return {
      attempted: false,
      pid: validation.record?.pid ?? null,
      target: "none",
      signal: "SIGTERM",
      success: true,
      error: null,
      ownershipValidation: validation,
    };
  }
  if (!validation.valid) {
    return {
      attempted: false,
      pid: validation.record?.pid ?? null,
      target: "none",
      signal: "SIGTERM",
      success: false,
      error: `refusing cleanup: ${validation.reason}`,
      ownershipValidation: validation,
    };
  }
  let result = terminateOwnedProcessGroup({ spec, signal: "SIGTERM" });
  if (!result.success) return result;
  validation = await waitForOwnedProcessExit(spec, gracefulTimeoutMs);
  if (
    validation.status === "mismatch" &&
    terminatingLeaderStillMatchesCoreIdentity(validation)
  ) {
    const pgid = validation.record!.pgid;
    if (processGroupHasLiveMembers(pgid)) {
      try {
        process.kill(-pgid, "SIGKILL");
      } catch (error) {
        return {
          ...result,
          success: false,
          error: error instanceof Error ? error.message : String(error),
          ownershipValidation: validation,
        };
      }
      if (
        !(await waitForProcessGroupWithoutLiveMembers(pgid, gracefulTimeoutMs))
      ) {
        return {
          ...result,
          success: false,
          error: "owned process-group retained live members after SIGKILL",
          ownershipValidation: validation,
        };
      }
    }
    removeOwnedProcessGroupRecord(spec.recordPath);
    return {
      ...result,
      success: true,
      error: null,
      ownershipValidation: validation,
    };
  }
  if (validation.status === "matched") {
    result = terminateOwnedProcessGroup({ spec, signal: "SIGKILL" });
    if (!result.success) return result;
    validation = await waitForOwnedProcessExit(spec, gracefulTimeoutMs);
  }
  if (validation.status !== "process_missing") {
    return {
      ...result,
      success: false,
      error: `owned process-group did not exit safely: ${validation.reason}`,
      ownershipValidation: validation,
    };
  }
  removeOwnedProcessGroupRecord(spec.recordPath);
  return {
    ...result,
    success: true,
    error: null,
    ownershipValidation: validation,
  };
};

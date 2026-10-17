import { readdirSync } from "node:fs";

import {
  type OwnedProcessGroupCleanupResult,
  type OwnedProcessGroupRecord,
  type OwnedProcessGroupSpec,
  readProcCoreIdentity,
} from "./process-ownership.parse-owned-process-group-record.js";
import {
  removeOwnedProcessGroupRecord,
  terminateOwnedProcessGroup,
  validateOwnedProcessGroupRecord,
  waitForOwnedProcessExit,
} from "./process-ownership.validate-owned-process-group-record.js";

const missingProcStat = (error: unknown, pid: number): boolean =>
  typeof error === "object" &&
  error !== null &&
  "code" in error &&
  error.code === "ENOENT" &&
  "path" in error &&
  error.path === `/proc/${pid.toString()}/stat`;

const observeSignalledLeader = (
  record: OwnedProcessGroupRecord,
): "live" | "zombie" | "missing" => {
  try {
    const core = readProcCoreIdentity(record.pid);
    if (core.pgid !== record.pgid) {
      throw new Error("process group mismatch during termination");
    }
    if (core.startTicks !== record.startTicks) {
      throw new Error("process start ticks mismatch during termination");
    }
    return core.state === "Z" ? "zombie" : "live";
  } catch (error) {
    if (missingProcStat(error, record.pid)) return "missing";
    throw error;
  }
};

// Post-signal absence needs a conservative stat-only scan: cmdline/cwd can
// disappear during exit, while an unreadable entry cannot prove an empty group.
const postSignalGroupHasLiveMembers = (pgid: number): boolean =>
  readdirSync("/proc", { withFileTypes: true }).some((entry) => {
    if (!entry.isDirectory() || !/^\d+$/u.test(entry.name)) return false;
    const pid = Number(entry.name);
    try {
      const core = readProcCoreIdentity(pid);
      return core.pgid === pgid && core.state !== "Z";
    } catch (error) {
      if (missingProcStat(error, pid)) return false;
      throw error;
    }
  });

const waitForPostSignalGroupExit = async (
  pgid: number,
  timeoutMs: number,
): Promise<boolean> => {
  const deadline = Date.now() + timeoutMs;
  while (postSignalGroupHasLiveMembers(pgid) && Date.now() < deadline) {
    await new Promise((resolvePromise) => setTimeout(resolvePromise, 25));
  }
  return !postSignalGroupHasLiveMembers(pgid);
};

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
    (validation.reason === "process cmdline mismatch" ||
      validation.reason === "process cwd mismatch")
  ) {
    const record = result.ownershipValidation.record!;
    try {
      if (JSON.stringify(validation.record) !== JSON.stringify(record)) {
        throw new Error("ownership record changed during termination");
      }
      const leader = observeSignalledLeader(record);
      const pgid = record.pgid;
      if (postSignalGroupHasLiveMembers(pgid)) {
        if (
          leader === "missing" ||
          observeSignalledLeader(record) === "missing"
        ) {
          throw new Error("leader absent while group retained live members");
        }
        process.kill(-pgid, "SIGKILL");
        if (!(await waitForPostSignalGroupExit(pgid, gracefulTimeoutMs))) {
          throw new Error(
            "owned process-group retained live members after SIGKILL",
          );
        }
      }
      if (
        observeSignalledLeader(record) === "live" ||
        postSignalGroupHasLiveMembers(pgid)
      ) {
        throw new Error("owned process-group exit was not confirmed");
      }
      removeOwnedProcessGroupRecord(spec.recordPath);
      return {
        ...result,
        success: true,
        error: null,
        ownershipValidation: validation,
      };
    } catch (error) {
      return {
        ...result,
        success: false,
        error: error instanceof Error ? error.message : String(error),
        ownershipValidation: validation,
      };
    }
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

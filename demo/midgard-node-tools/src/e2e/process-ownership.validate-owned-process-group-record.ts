import { rmSync } from "node:fs";
import { resolve } from "node:path";

import {
  assertRunToken,
  ownedProcessCommandSha256,
  type OwnedProcessGroupCleanupResult,
  type OwnedProcessGroupRecord,
  type OwnedProcessGroupSpec,
  type OwnedProcessGroupValidation,
  parseRecord,
  processGroupHasLiveMembers,
  readBootId,
  readProcIdentity,
} from "./process-ownership.parse-owned-process-group-record.js";

export const validateOwnedProcessGroupRecord = ({
  recordPath,
  runToken,
}: OwnedProcessGroupSpec): OwnedProcessGroupValidation => {
  let record: OwnedProcessGroupRecord | null = null;
  try {
    assertRunToken(runToken);
    record = parseRecord(recordPath);
    if (record.runToken !== runToken) {
      return {
        valid: false,
        status: "mismatch",
        reason: "run token mismatch",
        record,
      };
    }
    if (record.bootId !== readBootId()) {
      return {
        valid: false,
        status: "mismatch",
        reason: "boot id mismatch",
        record,
      };
    }
    if (record.pid !== record.pgid) {
      return {
        valid: false,
        status: "mismatch",
        reason: "recorded process is not its process-group leader",
        record,
      };
    }
    const expectedCommandHash = ownedProcessCommandSha256({
      command: record.command,
      args: record.args,
      cwd: resolve(record.cwd),
    });
    if (record.commandSha256 !== expectedCommandHash) {
      return {
        valid: false,
        status: "mismatch",
        reason: "command hash mismatch",
        record,
      };
    }
    const proc = readProcIdentity(record.pid);
    if (proc.pgid !== record.pgid) {
      return {
        valid: false,
        status: "mismatch",
        reason: "process group mismatch",
        record,
      };
    }
    if (proc.startTicks !== record.startTicks) {
      return {
        valid: false,
        status: "mismatch",
        reason: "process start ticks mismatch",
        record,
      };
    }
    if (proc.state === "Z") {
      if (processGroupHasLiveMembers(record.pgid)) {
        return {
          valid: true,
          status: "matched",
          reason:
            "owned process-group leader is a zombie with live group members",
          record,
        };
      }
      return {
        valid: false,
        status: "process_missing",
        reason: "owned process-group has no live members",
        record,
      };
    }
    if (proc.procCmdlineSha256 !== record.procCmdlineSha256) {
      return {
        valid: false,
        status: "mismatch",
        reason: "process cmdline mismatch",
        record,
      };
    }
    if (resolve(proc.cwd) !== resolve(record.cwd)) {
      return {
        valid: false,
        status: "mismatch",
        reason: "process cwd mismatch",
        record,
      };
    }
    return {
      valid: true,
      status: "matched",
      reason: "owned process-group identity matched",
      record,
    };
  } catch (error) {
    const code =
      typeof error === "object" && error !== null && "code" in error
        ? error.code
        : undefined;
    return {
      valid: false,
      status:
        code === "ENOENT"
          ? record === null
            ? "record_missing"
            : "process_missing"
          : "mismatch",
      reason: error instanceof Error ? error.message : String(error),
      record,
    };
  }
};

export const terminateOwnedProcessGroup = ({
  spec,
  signal = "SIGTERM",
}: {
  readonly spec: OwnedProcessGroupSpec;
  readonly signal?: NodeJS.Signals;
}): OwnedProcessGroupCleanupResult => {
  const ownershipValidation = validateOwnedProcessGroupRecord(spec);
  const pid = ownershipValidation.record?.pid ?? null;
  if (!ownershipValidation.valid || ownershipValidation.record === null) {
    return {
      attempted: false,
      pid,
      target: "none",
      signal,
      success: false,
      error: `refusing cleanup: ${ownershipValidation.reason}`,
      ownershipValidation,
    };
  }
  try {
    process.kill(-ownershipValidation.record.pgid, signal);
    return {
      attempted: true,
      pid,
      target: "process_group",
      signal,
      success: true,
      error: null,
      ownershipValidation,
    };
  } catch (error) {
    return {
      attempted: true,
      pid,
      target: "process_group",
      signal,
      success: false,
      error: error instanceof Error ? error.message : String(error),
      ownershipValidation,
    };
  }
};

export const waitForOwnedProcessExit = async (
  spec: OwnedProcessGroupSpec,
  timeoutMs: number,
): Promise<OwnedProcessGroupValidation> => {
  const deadline = Date.now() + timeoutMs;
  let validation = validateOwnedProcessGroupRecord(spec);
  while (validation.status === "matched" && Date.now() < deadline) {
    await new Promise((resolvePromise) => setTimeout(resolvePromise, 25));
    validation = validateOwnedProcessGroupRecord(spec);
  }
  return validation;
};

export const terminatingLeaderStillMatchesCoreIdentity = (
  validation: OwnedProcessGroupValidation,
): boolean => {
  const record = validation.record;
  if (
    record === null ||
    (validation.reason !== "process cmdline mismatch" &&
      validation.reason !== "process cwd mismatch")
  ) {
    return false;
  }
  try {
    const proc = readProcIdentity(record.pid);
    return proc.pgid === record.pgid && proc.startTicks === record.startTicks;
  } catch {
    return false;
  }
};

export const waitForProcessGroupWithoutLiveMembers = async (
  pgid: number,
  timeoutMs: number,
): Promise<boolean> => {
  const deadline = Date.now() + timeoutMs;
  while (processGroupHasLiveMembers(pgid) && Date.now() < deadline) {
    await new Promise((resolvePromise) => setTimeout(resolvePromise, 25));
  }
  return !processGroupHasLiveMembers(pgid);
};

export const removeOwnedProcessGroupRecord = (recordPath: string): void => {
  rmSync(recordPath, { force: true });
};

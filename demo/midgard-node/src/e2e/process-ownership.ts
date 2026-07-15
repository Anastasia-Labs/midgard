import { createHash, randomBytes } from "node:crypto";
import {
  closeSync,
  linkSync,
  mkdirSync,
  openSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  rmSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";

export const OWNED_PROCESS_GROUP_SCHEMA_VERSION =
  "midgard-owned-process-group-v1";

export type OwnedProcessGroupSpec = {
  readonly recordPath: string;
  readonly runToken: string;
};

export type OwnedProcessGroupRecord = {
  readonly schemaVersion: typeof OWNED_PROCESS_GROUP_SCHEMA_VERSION;
  readonly runToken: string;
  readonly bootId: string;
  readonly pid: number;
  readonly pgid: number;
  readonly startTicks: string;
  readonly procCmdlineSha256: string;
  readonly cwd: string;
  readonly command: string;
  readonly args: readonly string[];
  readonly commandSha256: string;
  readonly createdAt: string;
};

export type OwnedProcessGroupValidation = {
  readonly valid: boolean;
  readonly status:
    | "matched"
    | "record_missing"
    | "process_missing"
    | "mismatch";
  readonly reason: string;
  readonly record: OwnedProcessGroupRecord | null;
};

export type OwnedProcessGroupCleanupResult = {
  readonly attempted: boolean;
  readonly pid: number | null;
  readonly target: "process_group" | "none";
  readonly signal: NodeJS.Signals;
  readonly success: boolean;
  readonly error: string | null;
  readonly ownershipValidation: OwnedProcessGroupValidation;
};

const sha256 = (value: string | Buffer): string =>
  createHash("sha256").update(value).digest("hex");
const EMPTY_CMDLINE_SHA256 = sha256(Buffer.alloc(0));

export const ownedProcessCommandSha256 = ({
  command,
  args,
  cwd,
}: {
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
}): string => sha256(JSON.stringify({ command, args, cwd }));

const assertRunToken = (runToken: string): void => {
  if (!/^[a-f0-9]{32,128}$/u.test(runToken)) {
    throw new Error(
      "owned process-group run token must be 32-128 lowercase hex characters",
    );
  }
};

export const generateOwnedProcessRunToken = (): string =>
  randomBytes(32).toString("hex");

const readBootId = (): string =>
  readFileSync("/proc/sys/kernel/random/boot_id", "utf8").trim();

const readProcIdentity = (
  pid: number,
): {
  readonly pgid: number;
  readonly state: string;
  readonly startTicks: string;
  readonly procCmdlineSha256: string;
  readonly cwd: string;
} => {
  const stat = readFileSync(`/proc/${pid.toString()}/stat`, "utf8");
  const commEnd = stat.lastIndexOf(")");
  if (commEnd < 0) {
    throw new Error(`invalid /proc stat for pid ${pid.toString()}`);
  }
  const fieldsFromState = stat
    .slice(commEnd + 2)
    .trim()
    .split(/\s+/u);
  const pgid = Number(fieldsFromState[2]);
  const startTicks = fieldsFromState[19];
  if (!Number.isSafeInteger(pgid) || pgid <= 0 || startTicks === undefined) {
    throw new Error(`incomplete /proc identity for pid ${pid.toString()}`);
  }
  const state = fieldsFromState[0] ?? "";
  return {
    pgid,
    state,
    startTicks,
    procCmdlineSha256:
      state === "Z"
        ? ""
        : sha256(readFileSync(`/proc/${pid.toString()}/cmdline`)),
    cwd: state === "Z" ? "" : readlinkSync(`/proc/${pid.toString()}/cwd`),
  };
};

const processGroupHasLiveMembers = (pgid: number): boolean =>
  readdirSync("/proc", { withFileTypes: true }).some((entry) => {
    if (!entry.isDirectory() || !/^\d+$/u.test(entry.name)) return false;
    try {
      const identity = readProcIdentity(Number(entry.name));
      return (
        identity.pgid === pgid &&
        identity.state !== "Z" &&
        identity.procCmdlineSha256 !== EMPTY_CMDLINE_SHA256
      );
    } catch {
      return false;
    }
  });

const parseRecord = (recordPath: string): OwnedProcessGroupRecord => {
  const parsed = JSON.parse(
    readFileSync(recordPath, "utf8"),
  ) as OwnedProcessGroupRecord;
  if (
    parsed.schemaVersion !== OWNED_PROCESS_GROUP_SCHEMA_VERSION ||
    !Number.isSafeInteger(parsed.pid) ||
    parsed.pid <= 0 ||
    !Number.isSafeInteger(parsed.pgid) ||
    parsed.pgid <= 0 ||
    !Array.isArray(parsed.args)
  ) {
    throw new Error("invalid owned process-group record schema");
  }
  return parsed;
};

export const writeOwnedProcessGroupRecord = ({
  spec,
  pid,
  command,
  args,
  cwd,
}: {
  readonly spec: OwnedProcessGroupSpec;
  readonly pid: number;
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
}): OwnedProcessGroupRecord => {
  assertRunToken(spec.runToken);
  const proc = readProcIdentity(pid);
  if (proc.pgid !== pid) {
    throw new Error(
      `refusing to record pid ${pid.toString()}: detached process group id ${proc.pgid.toString()} does not equal its leader pid`,
    );
  }
  if (resolve(proc.cwd) !== resolve(cwd)) {
    throw new Error(
      `refusing to record pid ${pid.toString()}: process cwd ${proc.cwd} does not match requested cwd ${resolve(cwd)}`,
    );
  }
  const record: OwnedProcessGroupRecord = {
    schemaVersion: OWNED_PROCESS_GROUP_SCHEMA_VERSION,
    runToken: spec.runToken,
    bootId: readBootId(),
    pid,
    pgid: proc.pgid,
    startTicks: proc.startTicks,
    procCmdlineSha256: proc.procCmdlineSha256,
    cwd: proc.cwd,
    command,
    args: [...args],
    commandSha256: ownedProcessCommandSha256({
      command,
      args,
      cwd: resolve(proc.cwd),
    }),
    createdAt: new Date().toISOString(),
  };
  mkdirSync(dirname(spec.recordPath), { recursive: true, mode: 0o700 });
  const temporaryPath = `${spec.recordPath}.tmp-${process.pid.toString()}-${randomBytes(6).toString("hex")}`;
  const fd = openSync(temporaryPath, "wx", 0o600);
  try {
    writeFileSync(fd, `${JSON.stringify(record, null, 2)}\n`, "utf8");
  } finally {
    closeSync(fd);
  }
  try {
    // Hard-link publication is atomic and, unlike rename, refuses to replace
    // an orphan record left by a crashed controller.
    linkSync(temporaryPath, spec.recordPath);
  } finally {
    unlinkSync(temporaryPath);
  }
  return record;
};

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

const waitForOwnedProcessExit = async (
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

const terminatingLeaderStillMatchesCoreIdentity = (
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

const waitForProcessGroupWithoutLiveMembers = async (
  pgid: number,
  timeoutMs: number,
): Promise<boolean> => {
  const deadline = Date.now() + timeoutMs;
  while (processGroupHasLiveMembers(pgid) && Date.now() < deadline) {
    await new Promise((resolvePromise) => setTimeout(resolvePromise, 25));
  }
  return !processGroupHasLiveMembers(pgid);
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

export const removeOwnedProcessGroupRecord = (recordPath: string): void => {
  rmSync(recordPath, { force: true });
};

import { randomBytes } from "node:crypto";
import {
  closeSync,
  linkSync,
  mkdirSync,
  openSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";

import {
  arrayOf,
  exactRecord,
  isoTimestamp,
  nonEmptyString,
  positiveInteger,
  stringValue,
} from "midgard-node/artifact-schema";
import { sha256Hex } from "midgard-node/sha256";

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

const EMPTY_CMDLINE_SHA256 = sha256Hex(Buffer.alloc(0));

export const ownedProcessCommandSha256 = ({
  command,
  args,
  cwd,
}: {
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
}): string => sha256Hex(JSON.stringify({ command, args, cwd }));

export const assertRunToken = (runToken: string): void => {
  if (!/^[a-f0-9]{32,128}$/u.test(runToken)) {
    throw new Error(
      "owned process-group run token must be 32-128 lowercase hex characters",
    );
  }
};

const lowerHex64 = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(parsed)) {
    throw new Error(`${label} must be 64 lowercase hexadecimal characters`);
  }
  return parsed;
};

const canonicalString = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (parsed !== parsed.trim()) {
    throw new Error(`${label} must not contain surrounding whitespace`);
  }
  return parsed;
};

export const parseOwnedProcessGroupRecord = (
  value: unknown,
): OwnedProcessGroupRecord => {
  const input = exactRecord(value, "owned process-group record", [
    "schemaVersion",
    "runToken",
    "bootId",
    "pid",
    "pgid",
    "startTicks",
    "procCmdlineSha256",
    "cwd",
    "command",
    "args",
    "commandSha256",
    "createdAt",
  ]);
  if (input.schemaVersion !== OWNED_PROCESS_GROUP_SCHEMA_VERSION) {
    throw new Error(
      `owned process-group record.schemaVersion must be ${OWNED_PROCESS_GROUP_SCHEMA_VERSION}`,
    );
  }
  const runToken = nonEmptyString(
    input.runToken,
    "owned process-group record.runToken",
  );
  assertRunToken(runToken);
  const startTicks = canonicalString(
    input.startTicks,
    "owned process-group record.startTicks",
  );
  if (!/^[1-9][0-9]*$/u.test(startTicks)) {
    throw new Error(
      "owned process-group record.startTicks must be a canonical positive decimal",
    );
  }
  const bootId = canonicalString(
    input.bootId,
    "owned process-group record.bootId",
  );
  if (
    !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/u.test(
      bootId,
    )
  ) {
    throw new Error(
      "owned process-group record.bootId must be a lowercase UUID",
    );
  }
  const cwd = canonicalString(input.cwd, "owned process-group record.cwd");
  if (!cwd.startsWith("/") || resolve(cwd) !== cwd) {
    throw new Error(
      "owned process-group record.cwd must be canonical absolute",
    );
  }
  const pid = positiveInteger(input.pid, "owned process-group record.pid");
  const pgid = positiveInteger(input.pgid, "owned process-group record.pgid");
  if (pid !== pgid) {
    throw new Error(
      "owned process-group record must identify its process-group leader",
    );
  }
  const record: OwnedProcessGroupRecord = {
    schemaVersion: OWNED_PROCESS_GROUP_SCHEMA_VERSION,
    runToken,
    bootId,
    pid,
    pgid,
    startTicks,
    procCmdlineSha256: lowerHex64(
      input.procCmdlineSha256,
      "owned process-group record.procCmdlineSha256",
    ),
    cwd,
    command: canonicalString(
      input.command,
      "owned process-group record.command",
    ),
    args: arrayOf(input.args, "owned process-group record.args", stringValue),
    commandSha256: lowerHex64(
      input.commandSha256,
      "owned process-group record.commandSha256",
    ),
    createdAt: isoTimestamp(
      input.createdAt,
      "owned process-group record.createdAt",
    ),
  };
  const expectedCommandHash = ownedProcessCommandSha256(record);
  if (record.commandSha256 !== expectedCommandHash) {
    throw new Error("owned process-group record command hash mismatch");
  }
  return record;
};

export const generateOwnedProcessRunToken = (): string =>
  randomBytes(32).toString("hex");

export const readBootId = (): string =>
  readFileSync("/proc/sys/kernel/random/boot_id", "utf8").trim();

export const readProcCoreIdentity = (
  pid: number,
): {
  readonly pgid: number;
  readonly state: string;
  readonly startTicks: string;
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
  const pgidField = fieldsFromState[2];
  const pgid = Number(pgidField);
  const startTicks = fieldsFromState[19];
  if (
    !Number.isSafeInteger(pgid) ||
    pgid < 0 ||
    (pgid === 0 && pgidField !== "0") ||
    startTicks === undefined
  ) {
    throw new Error(`incomplete /proc identity for pid ${pid.toString()}`);
  }
  const state = fieldsFromState[0] ?? "";
  return {
    pgid,
    state,
    startTicks,
  };
};

export const readProcIdentity = (
  pid: number,
): ReturnType<typeof readProcCoreIdentity> & {
  readonly procCmdlineSha256: string;
  readonly cwd: string;
} => {
  const core = readProcCoreIdentity(pid);
  return {
    ...core,
    procCmdlineSha256:
      core.state === "Z"
        ? ""
        : sha256Hex(readFileSync(`/proc/${pid.toString()}/cmdline`)),
    cwd: core.state === "Z" ? "" : readlinkSync(`/proc/${pid.toString()}/cwd`),
  };
};

export const processGroupHasLiveMembers = (pgid: number): boolean =>
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

export const parseRecord = (recordPath: string): OwnedProcessGroupRecord => {
  return parseOwnedProcessGroupRecord(
    JSON.parse(readFileSync(recordPath, "utf8")),
  );
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
  const record = parseOwnedProcessGroupRecord({
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
  });
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

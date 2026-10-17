import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

import {
  CANONICAL_UNSIGNED_DECIMAL,
  canonicalIsoTimestamp,
  CONTAINER_ID,
  cpuSet,
  execFileAsync,
  IMAGE_ID,
  normalizedImageId,
  sanitizedDockerExecOptions,
  validateTrustedPhase3DockerRuntime,
} from "./phase3-architecture-g-load-generator-isolation.validate-trusted-phase3-docker-runtime.mjs";

export const disjointCpuLists = (left, right) => {
  const rightSet = cpuSet(right);
  return [...cpuSet(left)].every((cpu) => !rightSet.has(cpu));
};

const cgroupV2 = (raw, readFileSync = fs.readFileSync) => {
  const match = /^0::(.+)$/mu.exec(raw);
  if (match === null || !match[1].startsWith("/")) {
    throw new Error("formal isolation requires a unified cgroup-v2 path");
  }
  const cgroupPath = match[1];
  const root = path.join("/sys/fs/cgroup", cgroupPath);
  const read = (name) => readFileSync(path.join(root, name), "utf8").trim();
  const memoryMax = read("memory.max");
  const cpuMax = read("cpu.max");
  const cpusetEffective = read("cpuset.cpus.effective");
  if (!/^\d+$/u.test(memoryMax) || Number(memoryMax) <= 0) {
    throw new Error(
      "formal load-generator cgroup must have a finite memory.max",
    );
  }
  cpuSet(cpusetEffective);
  if (!/^(?:max|\d+) \d+$/u.test(cpuMax)) {
    throw new Error("formal isolation cgroup has malformed cpu.max");
  }
  return { path: cgroupPath, memoryMax, cpuMax, cpusetEffective };
};

const processStartTicks = (stat) => {
  const close = stat.lastIndexOf(")");
  if (close < 0) throw new Error("process stat has no command boundary");
  const startTicks = stat
    .slice(close + 2)
    .trim()
    .split(/\s+/u)[19];
  if (!/^\d+$/u.test(startTicks ?? "")) {
    throw new Error("process stat has no stable start tick identity");
  }
  return startTicks;
};

const processUid = (status, pid) => {
  const values = /^Uid:\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)$/mu.exec(status);
  if (values === null) {
    throw new Error(`PID ${pid.toString()} has no Linux Uid identity`);
  }
  return {
    real: Number(values[1]),
    effective: Number(values[2]),
    savedSet: Number(values[3]),
    fileSystem: Number(values[4]),
  };
};

export const capturePhase3ProcessIdentity = (
  pid,
  { readFileSync = fs.readFileSync, readlinkSync = fs.readlinkSync } = {},
) => {
  if (!Number.isSafeInteger(pid) || pid <= 0) {
    throw new Error("process identity requires a positive PID");
  }
  const root = `/proc/${pid.toString()}`;
  const startTicksBefore = processStartTicks(
    readFileSync(path.join(root, "stat"), "utf8"),
  );
  const status = readFileSync(path.join(root, "status"), "utf8");
  const cpus = /^Cpus_allowed_list:\s*(\S+)$/mu.exec(status)?.[1];
  if (cpus === undefined)
    throw new Error(`PID ${pid.toString()} has no CPU affinity`);
  cpuSet(cpus);
  const cgroup = readFileSync(path.join(root, "cgroup"), "utf8").trim();
  const cgroupIdentity = cgroupV2(cgroup, readFileSync);
  const commandLine = readFileSync(path.join(root, "cmdline"));
  const executable = readlinkSync(path.join(root, "exe"));
  if (!path.isAbsolute(executable) || commandLine.byteLength === 0) {
    throw new Error(`PID ${pid.toString()} has no executable identity`);
  }
  const bootId = readFileSync("/proc/sys/kernel/random/boot_id", "utf8").trim();
  const pidNamespace = readlinkSync(path.join(root, "ns/pid"));
  const identity = {
    pid,
    startTicks: startTicksBefore,
    uid: processUid(status, pid),
    executable,
    commandLineSha256: createHash("sha256").update(commandLine).digest("hex"),
    cgroup,
    cgroupV2: cgroupIdentity,
    cpusAllowedList: cpus,
    pidNamespace,
    bootId,
  };
  const startTicksAfter = processStartTicks(
    readFileSync(path.join(root, "stat"), "utf8"),
  );
  if (startTicksBefore !== startTicksAfter) {
    throw new Error(`PID ${pid.toString()} changed during identity capture`);
  }
  return identity;
};

export const parsedEndpoint = (value, expectedPath, label) => {
  let url;
  try {
    url = new URL(value);
  } catch {
    throw new Error(`${label} must be an absolute HTTP URL`);
  }
  if (
    url.protocol !== "http:" ||
    url.hostname !== "127.0.0.1" ||
    url.port.length === 0 ||
    url.pathname !== expectedPath ||
    url.username.length > 0 ||
    url.password.length > 0 ||
    url.search.length > 0 ||
    url.hash.length > 0
  ) {
    throw new Error(
      `${label} must use http://127.0.0.1:<published-port>${expectedPath}`,
    );
  }
  return {
    url: url.href,
    protocol: url.protocol,
    hostname: url.hostname,
    hostPort: url.port,
    pathname: url.pathname,
  };
};

export const canonicalPhase3NodeEndpoint = (value, expectedPath, label) =>
  parsedEndpoint(value, expectedPath, label).url;

const publishedEndpoint = ({ inspection, value, expectedPath, label }) => {
  const endpoint = parsedEndpoint(value, expectedPath, label);
  const matches = [];
  for (const [containerPort, bindings] of Object.entries(
    inspection?.NetworkSettings?.Ports ?? {},
  )) {
    if (!/^\d+\/tcp$/u.test(containerPort) || !Array.isArray(bindings)) {
      continue;
    }
    for (const binding of bindings) {
      if (
        binding?.HostPort === endpoint.hostPort &&
        new Set(["", "0.0.0.0", "127.0.0.1"]).has(binding?.HostIp ?? "")
      ) {
        matches.push({
          ...endpoint,
          containerPort,
          publishedHostIp: binding?.HostIp ?? "",
        });
      }
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `${label} is not uniquely published by the Phase 1 node container`,
    );
  }
  return matches[0];
};

export const inspectNodeContainer = async ({
  containerId,
  imageId,
  readyUrl,
  metricsUrl,
  dockerRuntime,
  execDocker = execFileAsync,
}) => {
  if (!CONTAINER_ID.test(containerId ?? "")) {
    throw new Error("Phase 1 node container ID must be exact 64-byte hex");
  }
  if (!IMAGE_ID.test(imageId ?? "")) {
    throw new Error("Phase 1 node image ID is invalid");
  }
  validateTrustedPhase3DockerRuntime(dockerRuntime);
  const { stdout } = await execDocker(
    dockerRuntime.client.realPath,
    ["inspect", "--type", "container", containerId],
    sanitizedDockerExecOptions(),
  );
  const inspections = JSON.parse(stdout);
  if (!Array.isArray(inspections) || inspections.length !== 1) {
    throw new Error("docker inspect did not return one exact node container");
  }
  const inspection = inspections[0];
  const healthcheckCommand = inspection?.Config?.Healthcheck?.Test;
  const binding = {
    phase1ContainerId: containerId,
    phase1ImageId: imageId,
    inspectedContainerId: inspection?.Id,
    inspectedImageId: inspection?.Image,
    configuredImageReference: inspection?.Config?.Image,
    hostPid: inspection?.State?.Pid,
    running: inspection?.State?.Running,
    status: inspection?.State?.Status,
    healthStatus: inspection?.State?.Health?.Status,
    startedAt: canonicalIsoTimestamp(inspection?.State?.StartedAt),
    restartCount: inspection?.RestartCount,
    healthcheckCommand,
    readyEndpoint: publishedEndpoint({
      inspection,
      value: readyUrl,
      expectedPath: "/readyz",
      label: "readiness endpoint",
    }),
    metricsEndpoint: publishedEndpoint({
      inspection,
      value: metricsUrl,
      expectedPath: "/metrics",
      label: "metrics endpoint",
    }),
  };
  if (
    binding.inspectedContainerId !== binding.phase1ContainerId ||
    normalizedImageId(binding.inspectedImageId) !==
      normalizedImageId(binding.phase1ImageId) ||
    !Number.isSafeInteger(binding.hostPid) ||
    binding.hostPid <= 0 ||
    binding.running !== true ||
    binding.status !== "running" ||
    binding.healthStatus !== "healthy" ||
    !Number.isSafeInteger(binding.restartCount) ||
    binding.restartCount < 0 ||
    !Number.isFinite(Date.parse(binding.startedAt ?? "")) ||
    !Array.isArray(binding.healthcheckCommand) ||
    !binding.healthcheckCommand.some(
      (entry) =>
        typeof entry === "string" &&
        entry.includes(binding.readyEndpoint.pathname),
    )
  ) {
    throw new Error(
      "Phase 1 node container is not the exact healthy Architecture G runtime",
    );
  }
  return binding;
};

const containerInspectProjection = (value) => {
  const projection = { ...(value ?? {}) };
  delete projection.hostProcessStartTicks;
  return projection;
};

export const sameContainerInspection = (left, right) =>
  JSON.stringify(containerInspectProjection(left)) ===
  JSON.stringify(containerInspectProjection(right));

export const sameProcIdentity = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

export const sameLoadGeneratorScope = (left, right) =>
  JSON.stringify({
    uid: left?.uid,
    executable: left?.executable,
    cgroup: left?.cgroup,
    cgroupV2: left?.cgroupV2,
    cpusAllowedList: left?.cpusAllowedList,
    pidNamespace: left?.pidNamespace,
    bootId: left?.bootId,
  }) ===
  JSON.stringify({
    uid: right?.uid,
    executable: right?.executable,
    cgroup: right?.cgroup,
    cgroupV2: right?.cgroupV2,
    cpusAllowedList: right?.cpusAllowedList,
    pidNamespace: right?.pidNamespace,
    bootId: right?.bootId,
  });

export const boundedCgroupIdentity = (value) =>
  typeof value?.path === "string" &&
  value.path.startsWith("/") &&
  value.path !== "/" &&
  path.posix.normalize(value.path) === value.path &&
  CANONICAL_UNSIGNED_DECIMAL.test(value?.memoryMax ?? "") &&
  Number(value.memoryMax) > 0 &&
  /^(?:max|(?:0|[1-9][0-9]*)) (?:0|[1-9][0-9]*)$/u.test(value?.cpuMax ?? "") &&
  typeof value?.cpusetEffective === "string";

export const validUidIdentity = (value) =>
  [value?.real, value?.effective, value?.savedSet, value?.fileSystem].every(
    (entry) => Number.isSafeInteger(entry) && entry >= 0,
  );

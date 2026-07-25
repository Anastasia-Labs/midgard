import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { promisify } from "node:util";

import {
  assertRegularFile,
  readJson,
  sha256File,
  writeAtomicImmutableJson,
} from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA =
  "midgard-phase3-load-generator-isolation-v1";
export const PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA =
  "midgard-phase3-node-pre-lifecycle-revalidation-v1";

const execFileAsync = promisify(execFile);
const CONTAINER_ID = /^[0-9a-f]{64}$/u;
const IMAGE_ID = /^(?:sha256:)?[0-9a-f]{64}$/u;
const SHA256 = /^[0-9a-f]{64}$/u;
const DOCKER_INSPECT_TIMEOUT_MS = 15_000;
const TRUSTED_DOCKER_CLIENT_PATH = "/usr/bin/docker";
const TRUSTED_DOCKER_SOCKET_PATH = "/var/run/docker.sock";
const TRUSTED_DOCKER_ENDPOINT = `unix://${TRUSTED_DOCKER_SOCKET_PATH}`;
const SANITIZED_DOCKER_ENV = Object.freeze({
  PATH: "/usr/bin:/bin",
  HOME: "/nonexistent",
  XDG_CONFIG_HOME: "/nonexistent",
  DOCKER_HOST: TRUSTED_DOCKER_ENDPOINT,
});

const normalizedImageId = (value) =>
  typeof value === "string" ? value.replace(/^sha256:/u, "") : value;

const fileIdentity = (filePath) => {
  const realPath = fs.realpathSync(filePath);
  const stat = fs.statSync(realPath);
  if (!stat.isFile()) throw new Error(`${filePath} is not a regular file`);
  return {
    path: filePath,
    realPath,
    sha256: sha256File(realPath),
    bytes: stat.size,
    mode: stat.mode & 0o7777,
    uid: stat.uid,
    gid: stat.gid,
    dev: stat.dev.toString(),
    ino: stat.ino.toString(),
  };
};

const socketIdentity = (socketPath) => {
  const realPath = fs.realpathSync(socketPath);
  const stat = fs.statSync(realPath);
  if (!stat.isSocket()) throw new Error(`${socketPath} is not a Unix socket`);
  return {
    path: socketPath,
    realPath,
    endpoint: TRUSTED_DOCKER_ENDPOINT,
    mode: stat.mode & 0o7777,
    uid: stat.uid,
    gid: stat.gid,
    dev: stat.dev.toString(),
    ino: stat.ino.toString(),
  };
};

const callerDockerEnvironment = (env) => {
  for (const name of ["DOCKER_HOST", "DOCKER_CONTEXT", "DOCKER_CONFIG"]) {
    if (env[name] !== undefined) {
      throw new Error(`${name} must be unset for the formal Phase 3 soak`);
    }
  }
  const entries = String(env.PATH ?? "").split(path.delimiter);
  if (
    entries.length === 0 ||
    entries.some((entry) => entry.length === 0 || !path.isAbsolute(entry))
  ) {
    throw new Error("PATH contains an empty or relative executable directory");
  }
  const trustedRealPath = fs.realpathSync(TRUSTED_DOCKER_CLIENT_PATH);
  let firstDocker = null;
  for (const entry of entries) {
    const candidate = path.join(entry, "docker");
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      firstDocker = fs.realpathSync(candidate);
      break;
    } catch {
      // Non-existent PATH entries cannot intercept Docker resolution.
    }
  }
  if (firstDocker !== trustedRealPath) {
    throw new Error(
      "PATH does not resolve Docker to the trusted absolute client",
    );
  }
};

const sanitizedDockerExecOptions = () => ({
  env: { ...SANITIZED_DOCKER_ENV },
  maxBuffer: 16 * 1024 * 1024,
  timeout: DOCKER_INSPECT_TIMEOUT_MS,
});

export const captureTrustedPhase3DockerRuntime = async ({
  env = process.env,
  execDocker = execFileAsync,
} = {}) => {
  callerDockerEnvironment(env);
  const client = fileIdentity(TRUSTED_DOCKER_CLIENT_PATH);
  if ((client.mode & 0o111) === 0) {
    throw new Error("trusted Docker client is not executable");
  }
  const socket = socketIdentity(TRUSTED_DOCKER_SOCKET_PATH);
  if (socket.endpoint !== TRUSTED_DOCKER_ENDPOINT) {
    throw new Error("Docker socket does not resolve to the trusted local path");
  }
  const { stdout } = await execDocker(
    client.realPath,
    ["info", "--format", "{{json .}}"],
    sanitizedDockerExecOptions(),
  );
  const info = JSON.parse(stdout);
  const daemon = {
    id: info?.ID,
    name: info?.Name,
    serverVersion: info?.ServerVersion,
    operatingSystem: info?.OperatingSystem,
    osType: info?.OSType,
    architecture: info?.Architecture,
  };
  if (
    typeof daemon.id !== "string" ||
    daemon.id.length === 0 ||
    typeof daemon.name !== "string" ||
    daemon.name.length === 0 ||
    typeof daemon.serverVersion !== "string" ||
    daemon.serverVersion.length === 0 ||
    daemon.osType !== "linux" ||
    typeof daemon.architecture !== "string" ||
    daemon.architecture.length === 0
  ) {
    throw new Error("trusted local Docker daemon identity is incomplete");
  }
  return {
    schemaVersion: "midgard-phase3-trusted-docker-runtime-v1",
    client,
    socket,
    daemon,
    environment: {
      inheritedDockerVariables: [],
      pathResolutionRealPath: client.realPath,
      daemonEndpoint: TRUSTED_DOCKER_ENDPOINT,
      home: SANITIZED_DOCKER_ENV.HOME,
    },
  };
};

const validFileIdentity = (value) =>
  value?.path === TRUSTED_DOCKER_CLIENT_PATH &&
  typeof value?.realPath === "string" &&
  path.isAbsolute(value.realPath) &&
  SHA256.test(value?.sha256 ?? "") &&
  Number.isSafeInteger(value?.bytes) &&
  value.bytes > 0 &&
  Number.isSafeInteger(value?.mode) &&
  Number.isSafeInteger(value?.uid) &&
  Number.isSafeInteger(value?.gid) &&
  /^\d+$/u.test(value?.dev ?? "") &&
  /^\d+$/u.test(value?.ino ?? "");

const validSocketIdentity = (value) =>
  value?.path === TRUSTED_DOCKER_SOCKET_PATH &&
  typeof value?.realPath === "string" &&
  path.isAbsolute(value.realPath) &&
  value?.endpoint === TRUSTED_DOCKER_ENDPOINT &&
  Number.isSafeInteger(value?.mode) &&
  Number.isSafeInteger(value?.uid) &&
  Number.isSafeInteger(value?.gid) &&
  /^\d+$/u.test(value?.dev ?? "") &&
  /^\d+$/u.test(value?.ino ?? "");

export const validateTrustedPhase3DockerRuntime = (runtime) => {
  if (
    runtime?.schemaVersion !== "midgard-phase3-trusted-docker-runtime-v1" ||
    !validFileIdentity(runtime?.client) ||
    !validSocketIdentity(runtime?.socket) ||
    typeof runtime?.daemon?.id !== "string" ||
    runtime.daemon.id.length === 0 ||
    typeof runtime?.daemon?.name !== "string" ||
    runtime.daemon.name.length === 0 ||
    typeof runtime?.daemon?.serverVersion !== "string" ||
    runtime.daemon.serverVersion.length === 0 ||
    runtime?.daemon?.osType !== "linux" ||
    typeof runtime?.daemon?.architecture !== "string" ||
    runtime.daemon.architecture.length === 0 ||
    JSON.stringify(runtime?.environment?.inheritedDockerVariables) !== "[]" ||
    runtime?.environment?.pathResolutionRealPath !== runtime.client.realPath ||
    runtime?.environment?.daemonEndpoint !== TRUSTED_DOCKER_ENDPOINT ||
    runtime?.environment?.home !== SANITIZED_DOCKER_ENV.HOME
  ) {
    throw new Error("trusted local Docker runtime binding is invalid");
  }
  return runtime;
};

export const validateTrustedPhase3DockerRuntimeArtifacts = (runtime) => {
  validateTrustedPhase3DockerRuntime(runtime);
  if (
    JSON.stringify(fileIdentity(TRUSTED_DOCKER_CLIENT_PATH)) !==
      JSON.stringify(runtime.client) ||
    JSON.stringify(socketIdentity(TRUSTED_DOCKER_SOCKET_PATH)) !==
      JSON.stringify(runtime.socket)
  ) {
    throw new Error("trusted Docker client or local socket identity changed");
  }
  return runtime;
};

const cpuSet = (value) => {
  if (
    typeof value !== "string" ||
    !/^\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*$/u.test(value)
  ) {
    throw new Error(`invalid Linux CPU-list ${String(value)}`);
  }
  const result = new Set();
  for (const range of value.split(",")) {
    const [first, last = first] = range.split("-").map(Number);
    if (last < first) throw new Error(`invalid Linux CPU range ${range}`);
    for (let cpu = first; cpu <= last; cpu += 1) result.add(cpu);
  }
  return result;
};

const disjointCpuLists = (left, right) => {
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

const parsedEndpoint = (value, expectedPath, label) => {
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

const inspectNodeContainer = async ({
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
  const engine = (inspection?.Config?.Env ?? [])
    .find((entry) => entry.startsWith("MPF_ENGINE="))
    ?.slice("MPF_ENGINE=".length);
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
    startedAt: inspection?.State?.StartedAt,
    restartCount: inspection?.RestartCount,
    engine,
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
    binding.engine !== "architecture_g" ||
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

const sameContainerInspection = (left, right) =>
  JSON.stringify(containerInspectProjection(left)) ===
  JSON.stringify(containerInspectProjection(right));

const sameProcIdentity = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

const sameLoadGeneratorScope = (left, right) =>
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

const boundedCgroupIdentity = (value) =>
  typeof value?.path === "string" &&
  value.path.startsWith("/") &&
  value.path !== "/" &&
  /^\d+$/u.test(value?.memoryMax ?? "") &&
  Number(value.memoryMax) > 0 &&
  /^(?:max|\d+) \d+$/u.test(value?.cpuMax ?? "") &&
  typeof value?.cpusetEffective === "string";

const validUidIdentity = (value) =>
  [value?.real, value?.effective, value?.savedSet, value?.fileSystem].every(
    (entry) => Number.isSafeInteger(entry) && entry >= 0,
  );

const validProcIdentity = (value) =>
  Number.isSafeInteger(value?.pid) &&
  value.pid > 0 &&
  /^\d+$/u.test(value?.startTicks ?? "") &&
  validUidIdentity(value?.uid) &&
  typeof value?.executable === "string" &&
  path.isAbsolute(value.executable) &&
  SHA256.test(value?.commandLineSha256 ?? "") &&
  typeof value?.cgroup === "string" &&
  value.cgroup.length > 0 &&
  typeof value?.cpusAllowedList === "string" &&
  typeof value?.pidNamespace === "string" &&
  value.pidNamespace.length > 0 &&
  typeof value?.bootId === "string" &&
  value.bootId.length > 0 &&
  boundedCgroupIdentity(value?.cgroupV2) &&
  value.cgroupV2.cpusetEffective === value.cpusAllowedList;

const validEndpointBinding = (value, pathname) => {
  let canonical;
  try {
    canonical = parsedEndpoint(value?.url, pathname, "bound node endpoint");
  } catch {
    return false;
  }
  return (
    canonical.url === value.url &&
    canonical.protocol === value.protocol &&
    canonical.hostname === value.hostname &&
    canonical.hostPort === value.hostPort &&
    canonical.pathname === value.pathname &&
    /^\d+\/tcp$/u.test(value?.containerPort ?? "") &&
    new Set(["", "0.0.0.0", "127.0.0.1"]).has(value?.publishedHostIp ?? "")
  );
};

export const validatePhase3LoadGeneratorIsolationDocument = (
  document,
  { expectedNodeContainerId, expectedNodeImageId } = {},
) => {
  const loadGenerator = document?.loadGenerator;
  const node = document?.node;
  const container = document?.nodeContainer;
  try {
    validateTrustedPhase3DockerRuntime(document?.docker);
  } catch {
    throw new Error("formal load generator Docker runtime binding is invalid");
  }
  if (
    document?.schemaVersion !== PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA ||
    document?.placement !== "measured-bounded-cgroup-v2" ||
    document?.cohosted !== true ||
    document?.clock?.source !== "shared-linux-kernel" ||
    document?.clock?.offsetMs !== 0 ||
    document?.clock?.bootId !== loadGenerator?.bootId ||
    !Number.isSafeInteger(document?.capturedAtMs) ||
    document.capturedAtMs <= 0 ||
    !validProcIdentity(loadGenerator) ||
    !validProcIdentity(node) ||
    loadGenerator.uid.effective === 0 ||
    loadGenerator?.bootId !== node?.bootId ||
    loadGenerator.cgroup === node.cgroup ||
    loadGenerator.pidNamespace === node.pidNamespace ||
    !disjointCpuLists(loadGenerator.cpusAllowedList, node.cpusAllowedList) ||
    !CONTAINER_ID.test(container?.phase1ContainerId ?? "") ||
    !IMAGE_ID.test(container?.phase1ImageId ?? "") ||
    container.inspectedContainerId !== container.phase1ContainerId ||
    normalizedImageId(container.inspectedImageId) !==
      normalizedImageId(container.phase1ImageId) ||
    (expectedNodeContainerId !== undefined &&
      container.phase1ContainerId !== expectedNodeContainerId) ||
    (expectedNodeImageId !== undefined &&
      normalizedImageId(container.phase1ImageId) !==
        normalizedImageId(expectedNodeImageId)) ||
    container.hostPid !== node.pid ||
    container.hostProcessStartTicks !== node.startTicks ||
    container.running !== true ||
    container.status !== "running" ||
    container.healthStatus !== "healthy" ||
    container.engine !== "architecture_g" ||
    !Number.isSafeInteger(container.restartCount) ||
    container.restartCount < 0 ||
    !Number.isFinite(Date.parse(container.startedAt ?? "")) ||
    typeof container.configuredImageReference !== "string" ||
    container.configuredImageReference.length === 0 ||
    !Array.isArray(container.healthcheckCommand) ||
    !container.healthcheckCommand.some(
      (entry) =>
        typeof entry === "string" &&
        entry.includes(container.readyEndpoint?.pathname),
    ) ||
    !validEndpointBinding(container.readyEndpoint, "/readyz") ||
    !validEndpointBinding(container.metricsEndpoint, "/metrics") ||
    container.readyEndpoint.containerPort ===
      container.metricsEndpoint.containerPort ||
    document?.checks?.distinctCgroup !== true ||
    document?.checks?.distinctPidNamespace !== true ||
    document?.checks?.disjointCpuAffinity !== true ||
    document?.checks?.sharedBootClock !== true ||
    document?.checks?.nonRootLoadGenerator !== true ||
    document?.checks?.exactPhase1Container !== true ||
    document?.checks?.exactPhase1Image !== true ||
    document?.checks?.hostPidFromDockerInspect !== true ||
    document?.checks?.readinessPublishedByNodeContainer !== true ||
    document?.checks?.metricsPublishedByNodeContainer !== true ||
    document?.checks?.stableAfterProcCapture !== true
  ) {
    throw new Error(
      "formal load generator or Phase 1 node-container binding is invalid",
    );
  }
  return document;
};

const isolationSummary = (artifactPath, artifactSha256, document) => ({
  path: artifactPath,
  sha256: artifactSha256,
  bytes: fs.lstatSync(artifactPath).size,
  schemaVersion: document.schemaVersion,
  placement: document.placement,
  cohosted: document.cohosted,
  clockOffsetMs: document.clock.offsetMs,
  loadGeneratorCpusAllowedList: document.loadGenerator.cpusAllowedList,
  loadGeneratorEffectiveUid: document.loadGenerator.uid.effective,
  nodeCpusAllowedList: document.node.cpusAllowedList,
  nodeContainerId: document.nodeContainer.phase1ContainerId,
  nodeImageId: document.nodeContainer.phase1ImageId,
  nodeHostPid: document.node.pid,
  nodeStartTicks: document.node.startTicks,
  readyUrl: document.nodeContainer.readyEndpoint.url,
  metricsUrl: document.nodeContainer.metricsEndpoint.url,
  dockerClientRealPath: document.docker.client.realPath,
  dockerClientSha256: document.docker.client.sha256,
  dockerSocketRealPath: document.docker.socket.realPath,
  dockerSocketDev: document.docker.socket.dev,
  dockerSocketIno: document.docker.socket.ino,
  dockerDaemonId: document.docker.daemon.id,
});

export const createPhase3LoadGeneratorIsolation = async ({
  outPath,
  phase1NodeContainerId,
  phase1NodeImageId,
  readyUrl,
  metricsUrl,
  env = process.env,
  captureDockerRuntime = captureTrustedPhase3DockerRuntime,
  inspectContainer = inspectNodeContainer,
  readProcessIdentity = capturePhase3ProcessIdentity,
}) => {
  if (!path.isAbsolute(outPath))
    throw new Error("isolation output must be absolute");
  if (
    env.STRESS_LOAD_GENERATOR_PLACEMENT !== "measured-cgroup" ||
    String(env.STRESS_LOADGEN_COHOSTED).toLowerCase() !== "true" ||
    Number(env.STRESS_CLOCK_OFFSET_MS) !== 0
  ) {
    throw new Error(
      "Phase 3 formal soak requires measured-cgroup, cohosted=true, and shared-kernel clock offset 0",
    );
  }
  const docker = await captureDockerRuntime({ env });
  const nodeContainer = await inspectContainer({
    containerId: phase1NodeContainerId,
    imageId: phase1NodeImageId,
    readyUrl,
    metricsUrl,
    dockerRuntime: docker,
  });
  const loadGenerator = readProcessIdentity(process.pid);
  const node = readProcessIdentity(nodeContainer.hostPid);
  const nodeContainerAfterCapture = await inspectContainer({
    containerId: phase1NodeContainerId,
    imageId: phase1NodeImageId,
    readyUrl,
    metricsUrl,
    dockerRuntime: docker,
  });
  if (!sameContainerInspection(nodeContainer, nodeContainerAfterCapture)) {
    throw new Error("node container changed during process identity capture");
  }
  nodeContainer.hostProcessStartTicks = node.startTicks;
  const document = validatePhase3LoadGeneratorIsolationDocument(
    {
      schemaVersion: PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
      capturedAtMs: Date.now(),
      docker,
      placement: "measured-bounded-cgroup-v2",
      cohosted: true,
      clock: {
        source: "shared-linux-kernel",
        offsetMs: 0,
        bootId: loadGenerator.bootId,
      },
      loadGenerator,
      nodeContainer,
      node,
      checks: {
        distinctCgroup: loadGenerator.cgroup !== node.cgroup,
        distinctPidNamespace: loadGenerator.pidNamespace !== node.pidNamespace,
        disjointCpuAffinity: disjointCpuLists(
          loadGenerator.cpusAllowedList,
          node.cpusAllowedList,
        ),
        sharedBootClock: loadGenerator.bootId === node.bootId,
        nonRootLoadGenerator: loadGenerator.uid.effective > 0,
        exactPhase1Container:
          nodeContainer.inspectedContainerId === phase1NodeContainerId,
        exactPhase1Image:
          normalizedImageId(nodeContainer.inspectedImageId) ===
          normalizedImageId(phase1NodeImageId),
        hostPidFromDockerInspect: nodeContainer.hostPid === node.pid,
        readinessPublishedByNodeContainer:
          nodeContainer.readyEndpoint.url ===
          parsedEndpoint(readyUrl, "/readyz", "readiness endpoint").url,
        metricsPublishedByNodeContainer:
          nodeContainer.metricsEndpoint.url ===
          parsedEndpoint(metricsUrl, "/metrics", "metrics endpoint").url,
        stableAfterProcCapture: true,
      },
    },
    {
      expectedNodeContainerId: phase1NodeContainerId,
      expectedNodeImageId: phase1NodeImageId,
    },
  );
  writeAtomicImmutableJson(outPath, document);
  return isolationSummary(outPath, sha256File(outPath), document);
};

export const validatePhase3NodePreLifecycleRevalidationDocument = (
  document,
  isolationDocument,
) => {
  if (
    document?.schemaVersion !== PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA ||
    !Number.isSafeInteger(document?.observedAtMs) ||
    document.observedAtMs < isolationDocument?.capturedAtMs ||
    typeof document?.isolation?.path !== "string" ||
    !path.isAbsolute(document.isolation.path) ||
    !SHA256.test(document?.isolation?.sha256 ?? "") ||
    JSON.stringify(document?.docker) !==
      JSON.stringify(isolationDocument?.docker) ||
    !sameContainerInspection(
      document?.nodeContainer,
      isolationDocument?.nodeContainer,
    ) ||
    !sameProcIdentity(
      document?.loadGenerator,
      isolationDocument?.loadGenerator,
    ) ||
    !sameProcIdentity(document?.node, isolationDocument?.node) ||
    document?.nodeContainer?.hostPid !== document?.node?.pid ||
    document?.nodeContainer?.hostProcessStartTicks !==
      document?.node?.startTicks ||
    document?.checks?.trustedDockerRuntimeUnchanged !== true ||
    document?.checks?.stableContainerBeforeAndAfterProcCapture !== true ||
    document?.checks?.loadGeneratorIdentityUnchanged !== true ||
    document?.checks?.nodeIdentityUnchanged !== true
  ) {
    throw new Error("pre-lifecycle node revalidation evidence is invalid");
  }
  return document;
};

const preLifecycleRevalidationSummary = (
  artifactPath,
  artifactSha256,
  document,
) => ({
  path: artifactPath,
  sha256: artifactSha256,
  bytes: fs.lstatSync(artifactPath).size,
  schemaVersion: document.schemaVersion,
  observedAtMs: document.observedAtMs,
  isolationPath: document.isolation.path,
  isolationSha256: document.isolation.sha256,
  nodeContainerId: document.nodeContainer.phase1ContainerId,
  nodeImageId: document.nodeContainer.phase1ImageId,
  nodeHostPid: document.node.pid,
  nodeStartTicks: document.node.startTicks,
  nodeRestartCount: document.nodeContainer.restartCount,
  nodeHealthStatus: document.nodeContainer.healthStatus,
  readyUrl: document.nodeContainer.readyEndpoint.url,
  metricsUrl: document.nodeContainer.metricsEndpoint.url,
  dockerClientSha256: document.docker.client.sha256,
  dockerSocketDev: document.docker.socket.dev,
  dockerSocketIno: document.docker.socket.ino,
  dockerDaemonId: document.docker.daemon.id,
});

export const createPhase3NodePreLifecycleRevalidation = async ({
  outPath,
  isolationArtifactPath,
  isolationArtifactSha256,
  env = process.env,
  captureDockerRuntime = captureTrustedPhase3DockerRuntime,
  inspectContainer = inspectNodeContainer,
  readProcessIdentity = capturePhase3ProcessIdentity,
}) => {
  if (!path.isAbsolute(outPath)) {
    throw new Error("pre-lifecycle revalidation output must be absolute");
  }
  assertRegularFile(isolationArtifactPath, "load-generator isolation artifact");
  if (sha256File(isolationArtifactPath) !== isolationArtifactSha256) {
    throw new Error("load-generator isolation artifact SHA-256 mismatch");
  }
  const isolationDocument = validatePhase3LoadGeneratorIsolationDocument(
    readJson(isolationArtifactPath),
  );
  const docker = await captureDockerRuntime({ env });
  if (JSON.stringify(docker) !== JSON.stringify(isolationDocument.docker)) {
    throw new Error("trusted Docker runtime changed before lifecycle start");
  }
  const inspectArgs = {
    containerId: isolationDocument.nodeContainer.phase1ContainerId,
    imageId: isolationDocument.nodeContainer.phase1ImageId,
    readyUrl: isolationDocument.nodeContainer.readyEndpoint.url,
    metricsUrl: isolationDocument.nodeContainer.metricsEndpoint.url,
    dockerRuntime: docker,
  };
  const nodeContainerBefore = await inspectContainer(inspectArgs);
  if (
    !sameContainerInspection(
      nodeContainerBefore,
      isolationDocument.nodeContainer,
    )
  ) {
    throw new Error("node container changed before lifecycle revalidation");
  }
  const loadGenerator = readProcessIdentity(process.pid);
  const node = readProcessIdentity(nodeContainerBefore.hostPid);
  const nodeContainerAfter = await inspectContainer(inspectArgs);
  if (
    !sameContainerInspection(nodeContainerBefore, nodeContainerAfter) ||
    !sameProcIdentity(loadGenerator, isolationDocument.loadGenerator) ||
    !sameProcIdentity(node, isolationDocument.node)
  ) {
    throw new Error("process or container changed before lifecycle start");
  }
  nodeContainerAfter.hostProcessStartTicks = node.startTicks;
  const document = validatePhase3NodePreLifecycleRevalidationDocument(
    {
      schemaVersion: PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA,
      observedAtMs: Date.now(),
      isolation: {
        path: isolationArtifactPath,
        sha256: isolationArtifactSha256,
      },
      docker,
      loadGenerator,
      nodeContainer: nodeContainerAfter,
      node,
      checks: {
        trustedDockerRuntimeUnchanged: true,
        stableContainerBeforeAndAfterProcCapture: true,
        loadGeneratorIdentityUnchanged: true,
        nodeIdentityUnchanged: true,
      },
    },
    isolationDocument,
  );
  writeAtomicImmutableJson(outPath, document);
  return preLifecycleRevalidationSummary(
    outPath,
    sha256File(outPath),
    document,
  );
};

export const consumePhase3LoadGeneratorIsolation = ({
  artifactPath,
  artifactSha256,
}) => {
  if (typeof artifactPath !== "string" || !path.isAbsolute(artifactPath)) {
    throw new Error(
      "formal load-generator isolation artifact path is required",
    );
  }
  assertRegularFile(artifactPath, "load-generator isolation artifact");
  if (sha256File(artifactPath) !== artifactSha256) {
    throw new Error("load-generator isolation artifact SHA-256 mismatch");
  }
  const document = validatePhase3LoadGeneratorIsolationDocument(
    readJson(artifactPath),
  );
  validateTrustedPhase3DockerRuntimeArtifacts(document.docker);
  const current = capturePhase3ProcessIdentity(process.pid);
  if (!sameLoadGeneratorScope(current, document.loadGenerator)) {
    throw new Error(
      "workload process escaped the measured load-generator isolation",
    );
  }
  const node = capturePhase3ProcessIdentity(document.nodeContainer.hostPid);
  if (!sameProcIdentity(node, document.node)) {
    throw new Error("node process identity changed after isolation preflight");
  }
  return isolationSummary(artifactPath, artifactSha256, document);
};

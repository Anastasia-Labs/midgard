import fs from "node:fs";

import {
  boundedCgroupIdentity,
  disjointCpuLists,
  parsedEndpoint,
  validUidIdentity,
} from "./phase3-architecture-g-load-generator-isolation.inspect-node-container.mjs";
import {
  CANONICAL_ISO_TIMESTAMP,
  CANONICAL_UNSIGNED_DECIMAL,
  canonicalAbsolutePath,
  canonicalNonEmptyString,
  CONTAINER_ID,
  IMAGE_ID,
  normalizedImageId,
  PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
  requireExactV1Object,
  SHA256,
  validateTrustedPhase3DockerRuntime,
} from "./phase3-architecture-g-load-generator-isolation.validate-trusted-phase3-docker-runtime.mjs";

const validProcIdentity = (value) =>
  Number.isSafeInteger(value?.pid) &&
  value.pid > 0 &&
  CANONICAL_UNSIGNED_DECIMAL.test(value?.startTicks ?? "") &&
  validUidIdentity(value?.uid) &&
  canonicalAbsolutePath(value?.executable) &&
  SHA256.test(value?.commandLineSha256 ?? "") &&
  typeof value?.cgroup === "string" &&
  value.cgroup.length > 0 &&
  typeof value?.cpusAllowedList === "string" &&
  canonicalNonEmptyString(value?.pidNamespace) &&
  canonicalNonEmptyString(value?.bootId) &&
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
  requireExactV1Object(
    document,
    [
      "schemaVersion",
      "capturedAtMs",
      "docker",
      "placement",
      "cohosted",
      "clock",
      "loadGenerator",
      "nodeContainer",
      "node",
      "checks",
    ],
    "load-generator isolation artifact",
  );
  requireExactV1Object(
    document.clock,
    ["source", "offsetMs", "bootId"],
    "load-generator isolation clock",
  );
  for (const [label, processIdentity] of [
    ["load generator", document.loadGenerator],
    ["node", document.node],
  ]) {
    requireExactV1Object(
      processIdentity,
      [
        "pid",
        "startTicks",
        "uid",
        "executable",
        "commandLineSha256",
        "cgroup",
        "cgroupV2",
        "cpusAllowedList",
        "pidNamespace",
        "bootId",
      ],
      `${label} process identity`,
    );
    requireExactV1Object(
      processIdentity.uid,
      ["real", "effective", "savedSet", "fileSystem"],
      `${label} UID identity`,
    );
    requireExactV1Object(
      processIdentity.cgroupV2,
      ["path", "memoryMax", "cpuMax", "cpusetEffective"],
      `${label} cgroup-v2 identity`,
    );
  }
  requireExactV1Object(
    document.nodeContainer,
    [
      "phase1ContainerId",
      "phase1ImageId",
      "inspectedContainerId",
      "inspectedImageId",
      "configuredImageReference",
      "hostPid",
      "hostProcessStartTicks",
      "running",
      "status",
      "healthStatus",
      "startedAt",
      "restartCount",
      "healthcheckCommand",
      "readyEndpoint",
      "metricsEndpoint",
    ],
    "load-generator isolation node-container identity",
  );
  for (const [label, endpoint] of [
    ["readiness", document.nodeContainer.readyEndpoint],
    ["metrics", document.nodeContainer.metricsEndpoint],
  ]) {
    requireExactV1Object(
      endpoint,
      [
        "url",
        "protocol",
        "hostname",
        "hostPort",
        "pathname",
        "containerPort",
        "publishedHostIp",
      ],
      `${label} endpoint binding`,
    );
  }
  requireExactV1Object(
    document.checks,
    [
      "distinctCgroup",
      "distinctPidNamespace",
      "disjointCpuAffinity",
      "sharedBootClock",
      "nonRootLoadGenerator",
      "exactPhase1Container",
      "exactPhase1Image",
      "hostPidFromDockerInspect",
      "readinessPublishedByNodeContainer",
      "metricsPublishedByNodeContainer",
      "stableAfterProcCapture",
    ],
    "load-generator isolation checks",
  );
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
    !Number.isSafeInteger(container.restartCount) ||
    container.restartCount < 0 ||
    !CANONICAL_ISO_TIMESTAMP.test(container.startedAt ?? "") ||
    new Date(container.startedAt).toISOString() !== container.startedAt ||
    !canonicalNonEmptyString(container.configuredImageReference) ||
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

export const isolationSummary = (artifactPath, artifactSha256, document) => ({
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

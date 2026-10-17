import fs from "node:fs";
import path from "node:path";

import {
  assertRegularFile,
  readJson,
  sha256File,
  writeAtomicImmutableJson,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  capturePhase3ProcessIdentity,
  disjointCpuLists,
  inspectNodeContainer,
  parsedEndpoint,
  sameContainerInspection,
  sameLoadGeneratorScope,
  sameProcIdentity,
} from "./phase3-architecture-g-load-generator-isolation.inspect-node-container.mjs";
import {
  isolationSummary,
  validatePhase3LoadGeneratorIsolationDocument,
} from "./phase3-architecture-g-load-generator-isolation.validate-phase3-load-generator-isolation-document.mjs";
import {
  captureTrustedPhase3DockerRuntime,
  normalizedImageId,
  PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
  PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA,
  requireExactV1Object,
  SHA256,
  validateTrustedPhase3DockerRuntimeArtifacts,
} from "./phase3-architecture-g-load-generator-isolation.validate-trusted-phase3-docker-runtime.mjs";

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
  requireExactV1Object(
    document,
    [
      "schemaVersion",
      "observedAtMs",
      "isolation",
      "docker",
      "loadGenerator",
      "nodeContainer",
      "node",
      "checks",
    ],
    "pre-lifecycle node revalidation artifact",
  );
  requireExactV1Object(
    document.isolation,
    ["path", "sha256"],
    "pre-lifecycle isolation identity",
  );
  requireExactV1Object(
    document.checks,
    [
      "trustedDockerRuntimeUnchanged",
      "stableContainerBeforeAndAfterProcCapture",
      "loadGeneratorIdentityUnchanged",
      "nodeIdentityUnchanged",
    ],
    "pre-lifecycle node revalidation checks",
  );
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

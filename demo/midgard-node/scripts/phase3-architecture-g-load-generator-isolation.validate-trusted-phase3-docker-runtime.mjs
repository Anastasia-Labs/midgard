import { execFile } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { promisify } from "node:util";

import { sha256File } from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA =
  "midgard-phase3-load-generator-isolation-v1";

export const PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA =
  "midgard-phase3-node-pre-lifecycle-revalidation-v1";

export const execFileAsync = promisify(execFile);

export const CONTAINER_ID = /^[0-9a-f]{64}$/u;

export const IMAGE_ID = /^(?:sha256:)?[0-9a-f]{64}$/u;

export const SHA256 = /^[0-9a-f]{64}$/u;

export const CANONICAL_UNSIGNED_DECIMAL = /^(?:0|[1-9][0-9]*)$/u;

export const CANONICAL_ISO_TIMESTAMP =
  /^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}Z$/u;

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

export const requireExactV1Object = (value, expectedKeys, label) => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a V1 JSON object`);
  }
  const actual = Object.keys(value).sort();
  const expected = [...expectedKeys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} must use the exact V1 keys`);
  }
  return value;
};

export const canonicalNonEmptyString = (value) =>
  typeof value === "string" && value.length > 0 && value === value.trim();

export const canonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  path.isAbsolute(value) &&
  path.resolve(value) === value;

export const canonicalIsoTimestamp = (value) => {
  const timestamp = Date.parse(value ?? "");
  return Number.isFinite(timestamp) ? new Date(timestamp).toISOString() : value;
};

export const normalizedImageId = (value) =>
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

export const sanitizedDockerExecOptions = () => ({
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
  canonicalAbsolutePath(value?.realPath) &&
  SHA256.test(value?.sha256 ?? "") &&
  Number.isSafeInteger(value?.bytes) &&
  value.bytes > 0 &&
  Number.isSafeInteger(value?.mode) &&
  Number.isSafeInteger(value?.uid) &&
  Number.isSafeInteger(value?.gid) &&
  CANONICAL_UNSIGNED_DECIMAL.test(value?.dev ?? "") &&
  CANONICAL_UNSIGNED_DECIMAL.test(value?.ino ?? "");

const validSocketIdentity = (value) =>
  value?.path === TRUSTED_DOCKER_SOCKET_PATH &&
  canonicalAbsolutePath(value?.realPath) &&
  value?.endpoint === TRUSTED_DOCKER_ENDPOINT &&
  Number.isSafeInteger(value?.mode) &&
  Number.isSafeInteger(value?.uid) &&
  Number.isSafeInteger(value?.gid) &&
  CANONICAL_UNSIGNED_DECIMAL.test(value?.dev ?? "") &&
  CANONICAL_UNSIGNED_DECIMAL.test(value?.ino ?? "");

export const validateTrustedPhase3DockerRuntime = (runtime) => {
  requireExactV1Object(
    runtime,
    ["schemaVersion", "client", "socket", "daemon", "environment"],
    "trusted Docker runtime",
  );
  requireExactV1Object(
    runtime.client,
    ["path", "realPath", "sha256", "bytes", "mode", "uid", "gid", "dev", "ino"],
    "trusted Docker client identity",
  );
  requireExactV1Object(
    runtime.socket,
    ["path", "realPath", "endpoint", "mode", "uid", "gid", "dev", "ino"],
    "trusted Docker socket identity",
  );
  requireExactV1Object(
    runtime.daemon,
    [
      "id",
      "name",
      "serverVersion",
      "operatingSystem",
      "osType",
      "architecture",
    ],
    "trusted Docker daemon identity",
  );
  requireExactV1Object(
    runtime.environment,
    [
      "inheritedDockerVariables",
      "pathResolutionRealPath",
      "daemonEndpoint",
      "home",
    ],
    "trusted Docker environment identity",
  );
  if (
    runtime?.schemaVersion !== "midgard-phase3-trusted-docker-runtime-v1" ||
    !validFileIdentity(runtime?.client) ||
    !validSocketIdentity(runtime?.socket) ||
    !canonicalNonEmptyString(runtime?.daemon?.id) ||
    !canonicalNonEmptyString(runtime?.daemon?.name) ||
    !canonicalNonEmptyString(runtime?.daemon?.serverVersion) ||
    !canonicalNonEmptyString(runtime?.daemon?.operatingSystem) ||
    runtime?.daemon?.osType !== "linux" ||
    !canonicalNonEmptyString(runtime?.daemon?.architecture) ||
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

export const cpuSet = (value) => {
  if (
    typeof value !== "string" ||
    !/^(?:0|[1-9][0-9]*)(?:-(?:0|[1-9][0-9]*))?(?:,(?:0|[1-9][0-9]*)(?:-(?:0|[1-9][0-9]*))?)*$/u.test(
      value,
    )
  ) {
    throw new Error(`invalid Linux CPU-list ${String(value)}`);
  }
  const result = new Set();
  let previous = -1;
  for (const range of value.split(",")) {
    const [first, last = first] = range.split("-").map(Number);
    if (
      last < first ||
      first <= previous ||
      (range.includes("-") && first === last)
    ) {
      throw new Error(`noncanonical Linux CPU range ${range}`);
    }
    for (let cpu = first; cpu <= last; cpu += 1) result.add(cpu);
    previous = last;
  }
  return result;
};

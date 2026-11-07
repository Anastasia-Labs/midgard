export const WATCHDOG_SCHEMA_VERSION = "midgard-throughput-watchdog-v1";

export const DEFAULT_REQUIRED_LABEL = "midgard.benchmark.load=true";

export const DOCKER_COMMAND_TIMEOUT_MS = 15_000;

const MAX_EVIDENCE_LINE_BYTES = 64 * 1024;

export const MAX_EVIDENCE_STRING_CHARS = 4_096;

export const WATCHDOG_EVENT_FIELDS = Object.freeze({
  target_verified: [],
  preflight_probe: [
    "probeStatus",
    "probeSignal",
    "probeStdout",
    "probeStderr",
    "probeError",
  ],
  preflight_failed: [],
  start_started: [],
  start_finished: [],
  sample_probe: [
    "probeStatus",
    "probeSignal",
    "probeStdout",
    "probeStderr",
    "probeError",
  ],
  completed: ["exitCode"],
  load_failed: ["exitCode"],
  stop_started: ["reason", "stopTimeoutSeconds"],
  stop_failed: ["reason", "error"],
  stop_verification_failed: ["reason", "error"],
  kill_started: ["reason"],
  kill_failed: ["reason", "error"],
  kill_finished: ["reason", "running", "exitCode"],
  kill_verification_failed: ["reason", "error"],
  stop_finished: ["reason", "stopMode", "running", "exitCode"],
  watchdog_error: ["error"],
});

const boundedString = (value, fieldName, { nullable = false } = {}) => {
  if (nullable && value === null) return null;
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.length > MAX_EVIDENCE_STRING_CHARS
  ) {
    throw new Error(
      `${fieldName} must be a nonempty string of at most ${MAX_EVIDENCE_STRING_CHARS.toString()} characters`,
    );
  }
  return value;
};

const canonicalTimestamp = (value) => {
  if (value === null) return null;
  boundedString(value, "watchdog evidence at");
  const timestamp = new Date(value);
  if (
    !Number.isFinite(timestamp.getTime()) ||
    timestamp.toISOString() !== value
  ) {
    throw new Error("watchdog evidence at must be canonical ISO-8601");
  }
  return value;
};

const exactInteger = (value, fieldName, minimum = Number.MIN_SAFE_INTEGER) => {
  if (!Number.isSafeInteger(value) || value < minimum) {
    throw new Error(`${fieldName} is outside its canonical integer bound`);
  }
  return value;
};

const canonicalProbeField = (fieldName, value) => {
  if (fieldName === "probeStatus") {
    return value === null
      ? null
      : exactInteger(value, "watchdog evidence probeStatus");
  }
  if (
    value !== null &&
    (typeof value !== "string" || value.length > MAX_EVIDENCE_STRING_CHARS)
  ) {
    throw new Error(
      `watchdog evidence ${fieldName} must be null or at most ${MAX_EVIDENCE_STRING_CHARS.toString()} characters`,
    );
  }
  return value;
};

export const canonicalWatchdogEvidenceRecordV1 = (record, expectedSequence) => {
  if (record === null || typeof record !== "object" || Array.isArray(record)) {
    throw new Error("watchdog evidence record must be an object");
  }
  if (record.schemaVersion !== WATCHDOG_SCHEMA_VERSION) {
    throw new Error("watchdog evidence schemaVersion must be exact V1");
  }
  const sequence = exactInteger(
    record.sequence,
    "watchdog evidence sequence",
    1,
  );
  if (
    expectedSequence !== undefined &&
    sequence !== exactInteger(expectedSequence, "expected sequence", 1)
  ) {
    throw new Error("watchdog evidence sequence is not contiguous");
  }
  const event = boundedString(record.event, "watchdog evidence event");
  const eventFields = WATCHDOG_EVENT_FIELDS[event];
  if (eventFields === undefined) {
    throw new Error(`unknown watchdog evidence event ${event}`);
  }
  const expectedFields = [
    "schemaVersion",
    "sequence",
    "at",
    "event",
    "containerId",
    "containerName",
    ...eventFields,
  ];
  const actualFields = Object.keys(record);
  if (
    actualFields.length !== expectedFields.length ||
    actualFields.some((field) => !expectedFields.includes(field))
  ) {
    throw new Error(
      `watchdog evidence ${event} fields must be exact: ${expectedFields.join(",")}`,
    );
  }

  const canonical = {
    schemaVersion: WATCHDOG_SCHEMA_VERSION,
    sequence,
    at: canonicalTimestamp(record.at),
    event,
    containerId: boundedString(
      record.containerId,
      "watchdog evidence containerId",
    ),
    containerName: boundedString(
      record.containerName,
      "watchdog evidence containerName",
    ),
  };
  for (const fieldName of eventFields) {
    const value = record[fieldName];
    if (fieldName.startsWith("probe")) {
      canonical[fieldName] = canonicalProbeField(fieldName, value);
    } else if (fieldName === "exitCode" || fieldName === "stopTimeoutSeconds") {
      canonical[fieldName] = exactInteger(
        value,
        `watchdog evidence ${fieldName}`,
        fieldName === "stopTimeoutSeconds" ? 0 : Number.MIN_SAFE_INTEGER,
      );
    } else if (fieldName === "running") {
      if (typeof value !== "boolean") {
        throw new Error("watchdog evidence running must be boolean");
      }
      canonical[fieldName] = value;
    } else if (fieldName === "stopMode") {
      if (value !== "graceful" && value !== "kill") {
        throw new Error("watchdog evidence stopMode must be graceful or kill");
      }
      canonical[fieldName] = value;
    } else {
      canonical[fieldName] = boundedString(
        value,
        `watchdog evidence ${fieldName}`,
      );
    }
  }
  return Object.freeze(canonical);
};

export const parseThroughputWatchdogEvidenceLineV1 = (
  line,
  expectedSequence,
) => {
  if (
    typeof line !== "string" ||
    line.length === 0 ||
    Buffer.byteLength(line, "utf8") > MAX_EVIDENCE_LINE_BYTES ||
    line.includes("\n") ||
    line.includes("\r")
  ) {
    throw new Error("watchdog evidence line is empty, oversized, or multiline");
  }
  let parsed;
  try {
    parsed = JSON.parse(line);
  } catch {
    throw new Error("watchdog evidence line must be valid JSON");
  }
  const canonical = canonicalWatchdogEvidenceRecordV1(parsed, expectedSequence);
  if (JSON.stringify(canonical) !== line) {
    throw new Error("watchdog evidence line is not canonical JSON");
  }
  return canonical;
};

const requireInteger = (value, name, minimum) => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < minimum) {
    throw new Error(`${name} must be an integer >= ${minimum.toString()}`);
  }
  return parsed;
};

const requireValue = (args, index, name) => {
  const value = args[index + 1];
  if (value === undefined || value.startsWith("--")) {
    throw new Error(`${name} requires a value`);
  }
  return value;
};

export const parseWatchdogArgs = (argv) => {
  const separator = argv.indexOf("--");
  if (separator < 0 || separator === argv.length - 1) {
    throw new Error("watchdog requires a probe command after --");
  }
  const optionArgs = argv.slice(0, separator);
  const probeCommand = argv.slice(separator + 1);
  const options = {
    requiredLabel: DEFAULT_REQUIRED_LABEL,
    intervalMs: 5_000,
    stopTimeoutSeconds: 5,
    probeTimeoutMs: 15_000,
  };

  for (let index = 0; index < optionArgs.length; index += 1) {
    const argument = optionArgs[index];
    if (argument === "--container") {
      options.container = requireValue(optionArgs, index, argument);
      index += 1;
    } else if (argument === "--evidence") {
      options.evidencePath = requireValue(optionArgs, index, argument);
      index += 1;
    } else if (argument === "--required-label") {
      options.requiredLabel = requireValue(optionArgs, index, argument);
      index += 1;
    } else if (argument === "--interval-ms") {
      options.intervalMs = requireInteger(
        requireValue(optionArgs, index, argument),
        argument,
        1,
      );
      index += 1;
    } else if (argument === "--stop-timeout-seconds") {
      options.stopTimeoutSeconds = requireInteger(
        requireValue(optionArgs, index, argument),
        argument,
        0,
      );
      index += 1;
    } else if (argument === "--probe-timeout-ms") {
      options.probeTimeoutMs = requireInteger(
        requireValue(optionArgs, index, argument),
        argument,
        1,
      );
      index += 1;
    } else {
      throw new Error(`unknown watchdog option: ${argument}`);
    }
  }

  if (typeof options.container !== "string" || options.container.length === 0) {
    throw new Error("--container is required");
  }
  if (
    typeof options.evidencePath !== "string" ||
    options.evidencePath.length === 0
  ) {
    throw new Error("--evidence is required");
  }
  const labelSeparator = options.requiredLabel.indexOf("=");
  if (labelSeparator <= 0) {
    throw new Error("--required-label must use key=value syntax");
  }
  options.requiredLabelKey = options.requiredLabel.slice(0, labelSeparator);
  options.requiredLabelValue = options.requiredLabel.slice(labelSeparator + 1);
  options.probeCommand = probeCommand;
  return options;
};

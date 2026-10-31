import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import {
  DEFAULT_REQUIRED_LABEL,
  WATCHDOG_SCHEMA_VERSION,
  createEvidenceWriter,
  parseThroughputWatchdogEvidenceLineV1,
  parseWatchdogArgs,
  runThroughputLoadWatchdog,
} from "./throughput-load-watchdog.mjs";

const target = (overrides = {}) => ({
  id: "load-container-id",
  name: "load-container",
  status: "created",
  running: false,
  exitCode: 0,
  labels: { "midgard.benchmark.load": "true" },
  ...overrides,
});

const harness = ({ probes, initial = target() }) => {
  const events = [];
  const calls = [];
  let state = initial;
  return {
    events,
    calls,
    runtime: {
      now: () => new Date("2026-07-11T04:00:00.000Z"),
      inspect: async (container) => {
        calls.push(["inspect", container]);
        return state;
      },
      start: async (containerId) => {
        calls.push(["start", containerId]);
        state = target({ status: "running", running: true });
      },
      stop: async (containerId, timeoutSeconds) => {
        calls.push(["stop", containerId, timeoutSeconds]);
        state = target({ status: "exited", running: false, exitCode: 137 });
      },
      kill: async (containerId) => {
        calls.push(["kill", containerId]);
        state = target({ status: "exited", running: false, exitCode: 137 });
      },
      probe: async (phase) => {
        calls.push(["probe", phase]);
        return probes.shift();
      },
      sleep: async (milliseconds) => {
        calls.push(["sleep", milliseconds]);
      },
    },
    record: (event) => events.push(event),
  };
};

const options = (fixture) => ({
  container: "load-container",
  requiredLabelKey: "midgard.benchmark.load",
  requiredLabelValue: "true",
  intervalMs: 5_000,
  stopTimeoutSeconds: 5,
  runtime: fixture.runtime,
  record: fixture.record,
});

test("parser requires an atomic target, evidence path, label, and probe", () => {
  const parsed = parseWatchdogArgs([
    "--container",
    "load",
    "--evidence",
    "evidence.ndjson",
    "--",
    "node",
    "probe.mjs",
  ]);
  assert.equal(parsed.container, "load");
  assert.equal(parsed.requiredLabel, DEFAULT_REQUIRED_LABEL);
  assert.equal(parsed.requiredLabelKey, "midgard.benchmark.load");
  assert.equal(parsed.requiredLabelValue, "true");
  assert.deepEqual(parsed.probeCommand, ["node", "probe.mjs"]);
  assert.throws(() => parseWatchdogArgs(["--container", "load"]));
  assert.throws(() =>
    parseWatchdogArgs([
      "--container",
      "load",
      "--evidence",
      "evidence.ndjson",
      "--required-label",
      "invalid",
      "--",
      "node",
      "probe.mjs",
    ]),
  );
});

test("evidence writer emits only canonical contiguous V1 records", () => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-watchdog-v1-"));
  const evidencePath = join(directory, "watchdog.ndjson");
  try {
    const writer = createEvidenceWriter(evidencePath);
    writer.record({
      at: "2026-07-11T04:00:00.000Z",
      event: "target_verified",
      containerId: "load-container-id",
      containerName: "load-container",
    });
    assert.throws(
      () =>
        writer.record({
          schemaVersion: "midgard-throughput-watchdog-v999",
          at: "2026-07-11T04:00:00.000Z",
          event: "target_verified",
          containerId: "load-container-id",
          containerName: "load-container",
        }),
      /must not override its V1 identity/u,
    );
    writer.record({
      at: "2026-07-11T04:00:00.000Z",
      event: "preflight_probe",
      containerId: "load-container-id",
      containerName: "load-container",
      probeStatus: 0,
      probeSignal: null,
      probeStdout: "",
      probeStderr: "",
      probeError: null,
    });
    writer.close();

    const [line, probeLine] = readFileSync(evidencePath, "utf8")
      .trimEnd()
      .split("\n");
    assert.ok(line);
    assert.ok(probeLine);
    const parsed = parseThroughputWatchdogEvidenceLineV1(line, 1);
    assert.equal(
      parseThroughputWatchdogEvidenceLineV1(probeLine, 2).probeStdout,
      "",
    );
    assert.equal(parsed.schemaVersion, WATCHDOG_SCHEMA_VERSION);
    assert.equal(parsed.sequence, 1);
    assert.throws(
      () =>
        parseThroughputWatchdogEvidenceLineV1(
          line.replace(WATCHDOG_SCHEMA_VERSION, "midgard-watchdog-v2"),
          1,
        ),
      /schemaVersion must be exact V1/u,
    );
    assert.throws(
      () => parseThroughputWatchdogEvidenceLineV1(line, 2),
      /sequence is not contiguous/u,
    );
    assert.throws(
      () =>
        parseThroughputWatchdogEvidenceLineV1(
          line.replace(
            '"containerName":"load-container"',
            '"containerName":"load-container","unknown":true',
          ),
          1,
        ),
      /fields must be exact/u,
    );
    assert.throws(
      () =>
        parseThroughputWatchdogEvidenceLineV1(
          line.replace("2026-07-11T04:00:00.000Z", "2026-07-11T04:00:00Z"),
          1,
        ),
      /canonical ISO-8601/u,
    );
    assert.throws(
      () => parseThroughputWatchdogEvidenceLineV1(` ${line}`, 1),
      /not canonical JSON/u,
    );
    assert.throws(() => createEvidenceWriter(evidencePath));
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

test("oversized cleanup diagnostics do not consume an evidence sequence", async () => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-watchdog-v1-"));
  const evidencePath = join(directory, "watchdog.ndjson");
  try {
    const writer = createEvidenceWriter(evidencePath);
    const fixture = harness({ probes: [{ status: 0 }, { status: 23 }] });
    fixture.runtime.stop = async () => {
      throw new Error("x".repeat(4_097));
    };
    try {
      const result = await runThroughputLoadWatchdog({
        ...options(fixture),
        record: writer.record,
      });
      assert.equal(result.status, "tripped");
    } finally {
      writer.close();
    }

    const lines = readFileSync(evidencePath, "utf8").trimEnd().split("\n");
    assert.equal(lines.length, 9);
    lines.forEach((line, index) => {
      const parsed = parseThroughputWatchdogEvidenceLineV1(line, index + 1);
      assert.equal(parsed.sequence, index + 1);
    });
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

test("preflight failure never starts or stops the load container", async () => {
  const fixture = harness({ probes: [{ status: 7 }] });
  const result = await runThroughputLoadWatchdog(options(fixture));
  assert.equal(result.status, "preflight_failed");
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "start" || action === "stop"),
    [],
  );
  assert.deepEqual(
    fixture.events.map(({ event }) => event),
    ["target_verified", "preflight_probe", "preflight_failed"],
  );
});

test("target state is revalidated after preflight before start", async () => {
  const fixture = harness({ probes: [{ status: 0 }] });
  let inspections = 0;
  const inspect = fixture.runtime.inspect;
  fixture.runtime.inspect = async (container) => {
    const observed = await inspect(container);
    inspections += 1;
    return inspections === 2
      ? target({ status: "running", running: true })
      : observed;
  };
  await assert.rejects(
    () => runThroughputLoadWatchdog(options(fixture)),
    /changed state or identity during preflight/u,
  );
  assert.equal(
    fixture.calls.some(([action]) => action === "start" || action === "stop"),
    false,
  );
});

test("first failing sample immediately stops only the captured immutable ID", async () => {
  const fixture = harness({ probes: [{ status: 0 }, { status: 23 }] });
  const result = await runThroughputLoadWatchdog(options(fixture));
  assert.equal(result.status, "tripped");
  assert.deepEqual(fixture.calls, [
    ["inspect", "load-container"],
    ["probe", "preflight"],
    ["inspect", "load-container-id"],
    ["start", "load-container-id"],
    ["inspect", "load-container-id"],
    ["probe", "sample"],
    ["stop", "load-container-id", 5],
    ["inspect", "load-container-id"],
  ]);
  assert.deepEqual(
    fixture.events.map(({ event }) => event),
    [
      "target_verified",
      "preflight_probe",
      "start_started",
      "start_finished",
      "sample_probe",
      "stop_started",
      "stop_finished",
    ],
  );
  assert.equal(
    fixture.events.find(({ event }) => event === "stop_started").reason,
    "probe_exit_23",
  );
});

test("a normal load exit is reported without a stop action", async () => {
  const fixture = harness({ probes: [{ status: 0 }, { status: 0 }] });
  let inspections = 0;
  const inspect = fixture.runtime.inspect;
  fixture.runtime.inspect = async (container) => {
    const observed = await inspect(container);
    inspections += 1;
    if (inspections === 3) {
      return target({ status: "exited", running: false, exitCode: 0 });
    }
    return observed;
  };
  const result = await runThroughputLoadWatchdog(options(fixture));
  assert.equal(result.status, "completed");
  assert.equal(
    fixture.calls.some(([action]) => action === "stop"),
    false,
  );
  assert.equal(fixture.events.at(-1).event, "completed");
});

test("watchdog errors after start fail closed on the captured immutable ID", async () => {
  const fixture = harness({ probes: [{ status: 0 }, { status: 0 }] });
  fixture.runtime.sleep = async () => {
    throw new Error("monitor interrupted");
  };
  await assert.rejects(
    () => runThroughputLoadWatchdog(options(fixture)),
    /monitor interrupted/u,
  );
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "start" || action === "stop"),
    [
      ["start", "load-container-id"],
      ["stop", "load-container-id", 5],
    ],
  );
  assert.deepEqual(
    fixture.events.slice(-3).map(({ event }) => event),
    ["watchdog_error", "stop_started", "stop_finished"],
  );
});

test("an evidence write failure after start cannot bypass exact-ID stop", async () => {
  const fixture = harness({ probes: [{ status: 0 }] });
  const recordedEvents = [];
  let recordCalls = 0;
  const record = (event) => {
    recordCalls += 1;
    if (recordCalls >= 4) {
      throw new Error("evidence disk failure");
    }
    recordedEvents.push(event);
  };
  await assert.rejects(
    () => runThroughputLoadWatchdog({ ...options(fixture), record }),
    /evidence disk failure/u,
  );
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "start" || action === "stop"),
    [
      ["start", "load-container-id"],
      ["stop", "load-container-id", 5],
    ],
  );
  assert.deepEqual(
    recordedEvents.map(({ event }) => event),
    ["target_verified", "preflight_probe", "start_started"],
  );
});

test("a monitoring inspect failure cannot bypass exact-ID stop", async () => {
  const fixture = harness({ probes: [{ status: 0 }] });
  let inspections = 0;
  const inspect = fixture.runtime.inspect;
  fixture.runtime.inspect = async (container) => {
    inspections += 1;
    if (inspections === 3) {
      fixture.calls.push(["inspect", container]);
      throw new Error("docker inspect unavailable");
    }
    return inspect(container);
  };
  await assert.rejects(
    () => runThroughputLoadWatchdog(options(fixture)),
    /docker inspect unavailable/u,
  );
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "start" || action === "stop"),
    [
      ["start", "load-container-id"],
      ["stop", "load-container-id", 5],
    ],
  );
});

test("an abort observed after start stops the captured immutable ID", async () => {
  const fixture = harness({ probes: [{ status: 0 }] });
  const controller = new AbortController();
  const start = fixture.runtime.start;
  fixture.runtime.start = async (containerId) => {
    await start(containerId);
    controller.abort(new Error("operator interrupted watchdog"));
  };
  await assert.rejects(
    () =>
      runThroughputLoadWatchdog({
        ...options(fixture),
        signal: controller.signal,
      }),
    /operator interrupted watchdog/u,
  );
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "start" || action === "stop"),
    [
      ["start", "load-container-id"],
      ["stop", "load-container-id", 5],
    ],
  );
});

test("post-stop inspection failure triggers exact-ID kill fallback", async () => {
  const fixture = harness({ probes: [{ status: 0 }, { status: 11 }] });
  let inspections = 0;
  const inspect = fixture.runtime.inspect;
  fixture.runtime.inspect = async (container) => {
    inspections += 1;
    if (inspections === 4) {
      fixture.calls.push(["inspect", container]);
      throw new Error("post-stop inspection failed");
    }
    return inspect(container);
  };
  const result = await runThroughputLoadWatchdog(options(fixture));
  assert.equal(result.status, "tripped");
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "stop" || action === "kill"),
    [
      ["stop", "load-container-id", 5],
      ["kill", "load-container-id"],
    ],
  );
  assert.equal(
    fixture.events.some(({ event }) => event === "stop_verification_failed"),
    true,
  );
  assert.equal(fixture.events.at(-1).stopMode, "kill");
});

test("a failed graceful stop falls back to killing only the captured ID", async () => {
  const fixture = harness({ probes: [{ status: 0 }, { status: 9 }] });
  fixture.runtime.stop = async (containerId, timeoutSeconds) => {
    fixture.calls.push(["stop", containerId, timeoutSeconds]);
    throw new Error("graceful stop timed out");
  };
  const result = await runThroughputLoadWatchdog(options(fixture));
  assert.equal(result.status, "tripped");
  assert.deepEqual(
    fixture.calls.filter(([action]) => action === "stop" || action === "kill"),
    [
      ["stop", "load-container-id", 5],
      ["kill", "load-container-id"],
    ],
  );
  assert.deepEqual(
    fixture.events.slice(-5).map(({ event }) => event),
    [
      "stop_started",
      "stop_failed",
      "kill_started",
      "kill_finished",
      "stop_finished",
    ],
  );
  assert.equal(fixture.events.at(-1).stopMode, "kill");
});

test("running or unlabeled targets are rejected before probing", async () => {
  for (const initial of [
    target({ status: "running", running: true }),
    target({ labels: {} }),
  ]) {
    const fixture = harness({ probes: [], initial });
    await assert.rejects(() => runThroughputLoadWatchdog(options(fixture)));
    assert.deepEqual(fixture.calls, [["inspect", "load-container"]]);
    assert.deepEqual(fixture.events, []);
  }
});

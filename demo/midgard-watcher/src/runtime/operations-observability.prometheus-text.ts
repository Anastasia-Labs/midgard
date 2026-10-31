import {
  WATCHER_REPLAY_REFUSAL_KINDS,
  WATCHER_REPLAY_REFUSAL_PHASES,
} from "../fault-proofs/replay-refusals.js";
import {
  WATCHER_ALERT_CODES,
  type WatcherOperationsMetrics,
  type WatcherOperationsStatus,
} from "./operations-observability.watcher-operations-metrics.js";

/** The content type of the Prometheus text exposition format. */
export const WATCHER_PROMETHEUS_CONTENT_TYPE =
  "text/plain; version=0.0.4; charset=utf-8";

type Sample = readonly [
  labels: Readonly<Record<string, string>>,
  value: string,
];

const escape = (value: string) =>
  value.replace(/\\/gu, "\\\\").replace(/\n/gu, "\\n").replace(/"/gu, '\\"');

const family = (
  name: string,
  type: "gauge" | "counter",
  help: string,
  samples: readonly Sample[],
): string =>
  [
    `# HELP ${name} ${help}`,
    `# TYPE ${name} ${type}`,
    ...samples.map(([labels, value]) => {
      const entries = Object.entries(labels);
      return entries.length === 0
        ? `${name} ${value}`
        : `${name}{${entries.map(([key, label]) => `${key}="${escape(label)}"`).join(",")}} ${value}`;
    }),
  ].join("\n");

/**
 * `/metrics`: the operations status and metrics in the Prometheus text
 * format, for the alert rules in `demo/midgard-watcher/alert-rules.yml`.
 * Every family is declared even when it has no sample, and the alert and
 * replay-refusal families carry every code and kind, zeros included.
 */
export const watcherPrometheusText = (
  status: WatcherOperationsStatus,
  metrics: WatcherOperationsMetrics,
): string => {
  const activeAlerts = new Map<string, number>();
  for (const { code } of status.activeAlerts)
    activeAlerts.set(code, (activeAlerts.get(code) ?? 0) + 1);
  return `${[
    family(
      "midgard_watcher_ready",
      "gauge",
      "1 when the watcher is ready, 0 when not.",
      [[{}, status.readiness === "ready" ? "1" : "0"]],
    ),
    family(
      "midgard_watcher_readiness_reason",
      "gauge",
      "1 for each reason the watcher is not ready.",
      [...new Set(status.readinessReasons)].map((reason) => [{ reason }, "1"]),
    ),
    family(
      "midgard_watcher_l1_readiness",
      "gauge",
      "Held objectives or holds per named L1 readiness reason.",
      Object.entries(metrics.l1Readiness).map(([reason, count]) => [
        { reason },
        count,
      ]),
    ),
    family(
      "midgard_watcher_l1_degradation",
      "gauge",
      "Count per named L1 degradation; never a readiness reason.",
      Object.entries(metrics.l1Degradations).map(([reason, count]) => [
        { reason },
        count,
      ]),
    ),
    family(
      "midgard_watcher_active_alerts",
      "gauge",
      "Active operational alerts per code.",
      WATCHER_ALERT_CODES.map((code) => [
        { code },
        (activeAlerts.get(code) ?? 0).toString(),
      ]),
    ),
    family(
      "midgard_watcher_replay_refusals_total",
      "counter",
      "Newly observed replayed-ledger refusals per phase and kind.",
      WATCHER_REPLAY_REFUSAL_PHASES.flatMap((phase) =>
        WATCHER_REPLAY_REFUSAL_KINDS.map(
          (kind): Sample => [
            { phase, kind },
            status.replayRefusals.totals[phase][kind],
          ],
        ),
      ),
    ),
  ].join("\n")}\n`;
};

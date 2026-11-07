import { createServer } from "node:net";

import * as NodeSdk from "@effect/opentelemetry/NodeSdk";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { Effect, ManagedRuntime } from "effect";

/** One sample of a Prometheus text exposition. */
export type PrometheusSample = Readonly<{
  name: string;
  labels: Readonly<Record<string, string>>;
  value: number;
}>;

const freePort = () =>
  new Promise<number>((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      server.close((error) => {
        if (error !== undefined) reject(error);
        else if (address === null || typeof address === "string")
          reject(new Error("no local TCP address"));
        else resolve(address.port);
      });
    });
  });

/**
 * The node's production metrics path (`listen --with-monitoring`): the
 * OpenTelemetry Prometheus exporter on a free local port, the
 * `NodeSdk.layer` that bridges Effect metrics into it, and a scrape of its
 * `/metrics` endpoint.
 */
export const startPrometheusExporter = async () => {
  const port = await freePort();
  const exporter = await new Promise<PrometheusExporter>((resolve, reject) => {
    const started: PrometheusExporter = new PrometheusExporter(
      { host: "127.0.0.1", port },
      (error) => (error === undefined ? resolve(started) : reject(error)),
    );
  });
  // Effect metrics live in a process-global registry; the SDK layer only
  // bridges them into the exporter, so it stays up for the whole file.
  const sdk = ManagedRuntime.make(
    NodeSdk.layer(() => ({
      resource: { serviceName: "midgard-node" },
      metricReader: exporter,
    })),
  );
  await sdk.runPromise(Effect.void);
  return {
    scrape: async (): Promise<string> => {
      const response = await fetch(`http://127.0.0.1:${port}/metrics`, {
        signal: AbortSignal.timeout(5_000),
      });
      if (response.status !== 200)
        throw new Error(`metrics scrape answered ${response.status}`);
      return await response.text();
    },
    // Shutting the SDK down shuts the exporter and its server down.
    stop: () => sdk.dispose(),
  };
};

const SAMPLE = /^([A-Za-z_:][A-Za-z0-9_:]*)(?:\{(.*)\})?\s+(\S+)(?:\s+\S+)?$/u;
const LABEL = /([A-Za-z_][A-Za-z0-9_]*)="((?:[^"\\]|\\.)*)"/gu;

/** The samples of a Prometheus text exposition. */
export const parsePrometheusText = (
  text: string,
): readonly PrometheusSample[] =>
  text
    .split("\n")
    .filter((line) => line.trim() !== "" && !line.startsWith("#"))
    .map((line) => {
      const match = SAMPLE.exec(line);
      if (match === null) throw new Error(`unparsed metrics line: ${line}`);
      const labels: Record<string, string> = {};
      for (const [, key, value] of (match[2] ?? "").matchAll(LABEL))
        labels[key!] = value!.replace(/\\(.)/gu, "$1");
      return { name: match[1]!, labels, value: Number(match[3]) };
    });

/** The metric family names a text exposition declares (`# TYPE`) or samples. */
export const prometheusFamilies = (text: string): ReadonlySet<string> =>
  new Set([
    ...text
      .split("\n")
      .filter((line) => line.startsWith("# TYPE "))
      .map((line) => line.split(" ")[2]!),
    ...parsePrometheusText(text).map(({ name }) => name),
  ]);

/** The value of the one sample of `name` whose labels include `labels`. */
export const sampleValue = (
  text: string,
  name: string,
  labels: Readonly<Record<string, string>> = {},
): number | undefined => {
  const matches = parsePrometheusText(text).filter(
    (sample) =>
      sample.name === name &&
      Object.entries(labels).every(
        ([key, value]) => sample.labels[key] === value,
      ),
  );
  if (matches.length > 1)
    throw new Error(`${matches.length.toString()} samples of ${name} match`);
  return matches[0]?.value;
};

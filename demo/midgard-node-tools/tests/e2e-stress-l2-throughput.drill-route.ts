import { readFile } from "node:fs/promises";

import { CLIENT_ACTIVE_MAX } from "midgard-node/commands/listen-http-rate-limit";
import { describe, expect, it } from "vitest";

import {
  E2E_L2_STRESS_ROUTE_LABELS,
  parseE2EL2StressConfig,
  parseE2EL2StressConfigArtifact,
  parseE2EL2StressSummary,
  runE2EL2StressThroughput,
} from "../src/commands/e2e-stress-l2-throughput/index.js";
import {
  fakeUtxo,
  makeClock,
  makeTempDir,
  responseJson,
  TEST_SEED,
  txHashForIndex,
} from "./e2e-stress-l2-throughput.e2e-stress-l2-throughput-config.js";

describe("e2e-stress-l2-throughput drill route", () => {
  // Each drill route is evidence of one kind only (functional through the
  // ingress as one client, or node capacity over the socket), always with
  // the node's rate limits on.
  const openLoop = {
    loadModel: "open-loop-upper-bound",
    corpusPath: "/corpus.ndjson",
  } as const;
  it.each([
    [
      "an open-loop run through the ingress",
      {
        route: "public-ingress",
        endpoint: "https://node.example",
        ...openLoop,
      },
      "closed-loop functional check",
    ],
    [
      "a plain-HTTP ingress endpoint",
      { route: "public-ingress", endpoint: "http://node.example" },
      "https:// endpoint",
    ],
    [
      "a socket on the ingress route",
      {
        route: "public-ingress",
        endpoint: "https://node.example",
        nodeSocket: "/run/node.sock",
      },
      "--node-socket applies only with --route node",
    ],
    [
      "a socket without a route",
      { nodeSocket: "/run/node.sock" },
      "--node-socket applies only with --route node",
    ],
    [
      "a closed-loop node capacity run",
      {
        route: "node",
        endpoint: "http://localhost",
        nodeSocket: "/run/n.sock",
      },
      "open-loop capacity run",
    ],
    [
      "a node route without its socket",
      { route: "node", endpoint: "http://localhost", ...openLoop },
      "--route node needs --node-socket",
    ],
    [
      "a relative socket path",
      {
        route: "node",
        endpoint: "http://localhost",
        nodeSocket: "node.sock",
        ...openLoop,
      },
      "--route node needs --node-socket",
    ],
    [
      "TLS over the node's plain socket",
      {
        route: "node",
        endpoint: "https://node.example",
        nodeSocket: "/run/node.sock",
        ...openLoop,
      },
      "plain HTTP",
    ],
    [
      "an unknown route",
      { route: "ingress", endpoint: "https://node.example" },
      "--route must be one of",
    ],
  ])("refuses %s", (_case, options, message) => {
    expect(() =>
      parseE2EL2StressConfig({ walletSeedPhrase: TEST_SEED, ...options }),
    ).toThrow(message);
  });

  it.each([
    { route: "public-ingress", endpoint: "https://node.example" },
    {
      route: "node",
      endpoint: "http://localhost",
      nodeSocket: "/run/node.sock",
      ...openLoop,
    },
  ])("refuses the $route route with the node's rate limits off", (options) => {
    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        env: { HTTP_RATE_LIMIT_ENABLED: "false" },
        ...options,
      }),
    ).toThrow("rate limits on");
  });

  it("labels a public-ingress drill as one-client functional evidence in every artifact", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      route: "public-ingress",
      endpoint: "https://node.example",
      walletSeedPhrase: TEST_SEED,
      count: "1",
      outDir,
      pollIntervalMs: "1",
    });
    const result = await runE2EL2StressThroughput(config, {
      submitTransfer: async (request) => ({
        txId: txHashForIndex(request.index + 1),
        status: "queued",
        senderAddress: request.walletAddress,
        destinationAddress: request.destinationAddress,
        selectedInputs: [`${"22".repeat(32)}#0`],
        requestedAssets: { lovelace: 1_000_000n },
        changeAssets: { lovelace: 9_000_000n },
        walletSeedSource: request.walletSeedSource,
        nodeEndpoint: request.config.nodeEndpoint,
      }),
      fetch: (async () =>
        responseJson({ status: "committed" })) as unknown as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });
    const route = {
      kind: "public-ingress",
      label: E2E_L2_STRESS_ROUTE_LABELS["public-ingress"],
      clientActiveMax: CLIENT_ACTIVE_MAX,
    };
    const parsedConfig = JSON.parse(
      await readFile(result.configJsonPath, "utf8"),
    ) as Record<string, unknown>;
    expect(parsedConfig.route).toEqual(route);
    expect(parseE2EL2StressConfigArtifact(parsedConfig)).toEqual(parsedConfig);
    const nodeRoute = {
      kind: "node",
      label: E2E_L2_STRESS_ROUTE_LABELS.node,
      clientActiveMax: CLIENT_ACTIVE_MAX,
      socketPath: "/run/node.sock",
      clientSpread: 1,
    };
    // A closed-loop run cannot claim node capacity.
    expect(() =>
      parseE2EL2StressConfigArtifact({ ...parsedConfig, route: nodeRoute }),
    ).toThrow("evidence only for loadModel open-loop-upper-bound");
    const parsedSummary = JSON.parse(
      await readFile(result.summaryJsonPath, "utf8"),
    ) as Record<string, unknown>;
    expect(parsedSummary.route).toEqual(route);
    expect(parseE2EL2StressSummary(parsedSummary)).toEqual(parsedSummary);
    await expect(
      readFile(result.summaryMarkdownPath, "utf8"),
    ).resolves.toContain(`- evidence: ${route.label} (route public-ingress`);
    // Nor under either label.
    expect(() =>
      parseE2EL2StressSummary({
        ...parsedSummary,
        route: { ...route, label: E2E_L2_STRESS_ROUTE_LABELS.node },
      }),
    ).toThrow("label must be");
    expect(() =>
      parseE2EL2StressSummary({
        ...parsedSummary,
        route: nodeRoute,
      }),
    ).toThrow("evidence only for loadModel open-loop-upper-bound");
  });
});

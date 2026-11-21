import { isAbsolute } from "node:path";

import { ConfigProvider, Effect } from "effect";
import {
  exactRecord,
  nonEmptyString,
  oneOf,
  positiveInteger,
} from "midgard-node/artifact-schema";
import { CLIENT_ACTIVE_MAX } from "midgard-node/commands/listen-http-rate-limit";
import { httpRateLimitConfig } from "midgard-node/services/config.http-proxy-peers";
import {
  type HttpRate,
  type HttpRateLimitPolicy,
} from "midgard-node/services/http-rate-limits";

import { type E2EL2StressLoadModel } from "./types.js";

/**
 * Where a rehearsal drill sends its load: `public-ingress` is the functional
 * check through the public TLS ingress as one client inside that client's
 * limits; `node` is the capacity run straight at the node's Unix socket with
 * the node's rate limiter on, its load spread over synthetic clients.
 */
export const E2E_L2_STRESS_ROUTES = ["public-ingress", "node"] as const;
export type E2EL2StressRouteKind = (typeof E2E_L2_STRESS_ROUTES)[number];

/** The evidence label of each route, per the benchmark reference. */
export const E2E_L2_STRESS_ROUTE_LABELS = {
  "public-ingress": "Functional E2E through the public ingress from one client",
  node: "Class A node capacity, load generator cohosted on the node socket",
} as const satisfies Record<E2EL2StressRouteKind, string>;

/**
 * The fraction of a client's refill the drill uses: each synthetic client of
 * the node route, and the one client of the public-ingress route, sends at
 * most this share of the rate the node refills its buckets at, so
 * whole-second refills, timer jitter and two clients sharing a hashed cell
 * never bind.
 */
export const CLIENT_SHARE_OF_REFILL = 1 / 2;

/**
 * The requests a bucket of `rate` refills a second, as the node refills it:
 * the node's hashed buckets refill in whole seconds and never past their
 * burst, so a bucket whose burst is below its rate gives only its burst.
 */
export const sustainedPerSecond = (rate: HttpRate | undefined): number =>
  rate === undefined
    ? Number.POSITIVE_INFINITY
    : Math.min(rate.perSecond, rate.burst);

export type E2EL2StressRoute =
  | {
      readonly kind: "public-ingress";
      readonly label: (typeof E2E_L2_STRESS_ROUTE_LABELS)["public-ingress"];
      /** Requests in flight at once, the node's per-client cap. */
      readonly clientActiveMax: number;
      /** The node's resolved rate limits, which pace each request class. */
      readonly rateLimits: HttpRateLimitPolicy;
    }
  | {
      readonly kind: "node";
      readonly label: (typeof E2E_L2_STRESS_ROUTE_LABELS)["node"];
      readonly clientActiveMax: number;
      readonly socketPath: string;
      /** Synthetic clients, each its own /24, the load spreads over. */
      readonly clientSpread: number;
    };

/** The route as config.json and summary.json record it. */
export type E2EL2StressRouteEvidence = {
  readonly kind: E2EL2StressRouteKind;
  readonly label: string;
  readonly clientActiveMax: number;
  readonly socketPath?: string;
  readonly clientSpread?: number;
};

/**
 * The synthetic clients an open-loop run at `targetRateTps` with
 * `maxInFlight` requests in flight spreads over: enough that each carries
 * at most `CLIENT_SHARE_OF_REFILL` of the write class's per-client and
 * per-prefix refill (one client per /24, so the prefix is its own), and
 * enough that the requests in flight fit `CLIENT_ACTIVE_MAX` each.
 */
export const nodeRouteClientSpread = ({
  targetRateTps,
  maxInFlight,
  rateLimits,
}: {
  readonly targetRateTps: number;
  readonly maxInFlight: number;
  readonly rateLimits: HttpRateLimitPolicy;
}): number => {
  const write = rateLimits.classes.write;
  const perClient = Math.min(
    sustainedPerSecond(write.peer),
    sustainedPerSecond(write.prefix),
  );
  return Math.max(
    Math.ceil(targetRateTps / (perClient * CLIENT_SHARE_OF_REFILL)),
    Math.ceil(maxInFlight / CLIENT_ACTIVE_MAX),
  );
};

/**
 * The node's rate limits as the node resolves them from `env`: its own
 * configuration reader over the same variables (`HTTP_RATE_LIMIT_<CLASS>`
 * overrides, else the defaults). The drill runs with the node's
 * environment, so this is the policy the node enforces.
 */
export const nodeRateLimitsFromEnv = (
  env: NodeJS.ProcessEnv,
): HttpRateLimitPolicy =>
  Effect.runSync(
    Effect.withConfigProvider(
      httpRateLimitConfig,
      ConfigProvider.fromMap(
        new Map(
          Object.entries(env).filter(
            (entry): entry is [string, string] => entry[1] !== undefined,
          ),
        ),
      ),
    ),
  );

export const parseE2EL2StressRoute = ({
  route,
  nodeSocket,
  loadModel,
  endpoint,
  targetRateTps,
  openLoopMaxInFlight,
  env,
}: {
  readonly route: string | undefined;
  readonly nodeSocket: string | undefined;
  readonly loadModel: E2EL2StressLoadModel;
  readonly endpoint: string;
  readonly targetRateTps: number;
  readonly openLoopMaxInFlight: number;
  readonly env: NodeJS.ProcessEnv;
}): E2EL2StressRoute | undefined => {
  const kind = route?.trim();
  const socketPath = nodeSocket?.trim();
  if (kind === undefined || kind.length === 0) {
    if (socketPath !== undefined && socketPath.length > 0)
      throw new Error("--node-socket applies only with --route node.");
    return undefined;
  }
  if (!(E2E_L2_STRESS_ROUTES as readonly string[]).includes(kind))
    throw new Error(
      `--route must be one of: ${E2E_L2_STRESS_ROUTES.join(", ")}.`,
    );
  const rateLimits = nodeRateLimitsFromEnv(env);
  if (!rateLimits.enabled)
    throw new Error(
      `--route ${kind} runs with the node's rate limits on; unset HTTP_RATE_LIMIT_ENABLED=false.`,
    );
  if (kind === "public-ingress") {
    if (socketPath !== undefined && socketPath.length > 0)
      throw new Error("--node-socket applies only with --route node.");
    if (loadModel !== "closed-loop-smoke")
      throw new Error(
        "--route public-ingress is the closed-loop functional check from one client; an open-loop run would outrun one client's limits.",
      );
    if (new URL(endpoint).protocol !== "https:")
      throw new Error(
        "--route public-ingress needs the ingress's https:// endpoint.",
      );
    return {
      kind,
      label: E2E_L2_STRESS_ROUTE_LABELS[kind],
      clientActiveMax: CLIENT_ACTIVE_MAX,
      rateLimits,
    };
  }
  if (loadModel !== "open-loop-upper-bound")
    throw new Error(
      "--route node is the open-loop capacity run; use --load-model open-loop-upper-bound.",
    );
  if (socketPath === undefined || !isAbsolute(socketPath))
    throw new Error(
      "--route node needs --node-socket, the absolute host path of the node's HTTP socket.",
    );
  // The socket serves plain HTTP; the endpoint only names it.
  if (new URL(endpoint).protocol !== "http:")
    throw new Error(
      "--route node speaks plain HTTP over the node's socket; pass an http:// --endpoint, such as http://localhost, which only names the socket.",
    );
  return {
    kind: "node",
    label: E2E_L2_STRESS_ROUTE_LABELS.node,
    clientActiveMax: CLIENT_ACTIVE_MAX,
    socketPath,
    clientSpread: nodeRouteClientSpread({
      targetRateTps,
      maxInFlight: openLoopMaxInFlight,
      rateLimits,
    }),
  };
};

export const routeEvidence = (
  route: E2EL2StressRoute | undefined,
): E2EL2StressRouteEvidence | undefined =>
  route === undefined
    ? undefined
    : route.kind === "public-ingress"
      ? {
          kind: route.kind,
          label: route.label,
          clientActiveMax: route.clientActiveMax,
        }
      : {
          kind: route.kind,
          label: route.label,
          clientActiveMax: route.clientActiveMax,
          socketPath: route.socketPath,
          clientSpread: route.clientSpread,
        };

export const assertRouteEvidence = (
  value: unknown,
  label: string,
): E2EL2StressRouteEvidence => {
  const input = exactRecord(
    value,
    label,
    ["kind", "label", "clientActiveMax"],
    ["socketPath", "clientSpread"],
  );
  const kind = oneOf(input.kind, `${label}.kind`, E2E_L2_STRESS_ROUTES);
  if (input.label !== E2E_L2_STRESS_ROUTE_LABELS[kind])
    throw new Error(
      `${label}.label must be "${E2E_L2_STRESS_ROUTE_LABELS[kind]}" for route ${kind}`,
    );
  positiveInteger(input.clientActiveMax, `${label}.clientActiveMax`);
  const nodeFields = [input.socketPath, input.clientSpread];
  if (kind === "node") {
    nonEmptyString(input.socketPath, `${label}.socketPath`);
    positiveInteger(input.clientSpread, `${label}.clientSpread`);
  } else if (nodeFields.some((field) => field !== undefined))
    throw new Error(
      `${label} names a socket or client spread only for route node`,
    );
  return input as E2EL2StressRouteEvidence;
};

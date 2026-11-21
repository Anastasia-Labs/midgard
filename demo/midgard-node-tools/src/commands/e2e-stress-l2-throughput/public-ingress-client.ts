import { setTimeout as sleep } from "node:timers/promises";

import {
  httpRateClassOf,
  httpRouteKindOf,
} from "midgard-node/commands/http/routes";
import { CLIENT_ACTIVE_MAX } from "midgard-node/commands/listen-http-rate-limit";
import {
  type HttpRateClass,
  type HttpRateLimitPolicy,
} from "midgard-node/services/http-rate-limits";
import { Agent, type Dispatcher, setGlobalDispatcher } from "undici";

import { nodeTlsConnectFromEnv } from "../../full-stack/node-tls.js";
import { CLIENT_SHARE_OF_REFILL, sustainedPerSecond } from "./route.js";
import { type E2EL2StressConfig } from "./types.js";

/**
 * The milliseconds one client leaves between two requests of `kind`: its
 * share of the slowest bucket the node charges one client for that class
 * (its own, its prefix's, and the global one every client shares).
 */
export const clientPaceMs = (
  rateLimits: HttpRateLimitPolicy,
  kind: HttpRateClass,
): number => {
  const tiers = rateLimits.classes[kind];
  const perSecond = Math.min(
    sustainedPerSecond(tiers.peer),
    sustainedPerSecond(tiers.prefix),
    sustainedPerSecond(tiers.global),
  );
  return 1_000 / (perSecond * CLIENT_SHARE_OF_REFILL);
};

/** A first-in, first-out count of the requests one client has in flight. */
const createSlots = (max: number) => {
  let active = 0;
  const waiting: (() => void)[] = [];
  return {
    acquire: async () => {
      if (active < max) {
        active += 1;
        return;
      }
      await new Promise<void>((resolve) => waiting.push(resolve));
    },
    release: () => {
      const next = waiting.shift();
      if (next === undefined) active -= 1;
      else next();
    },
  };
};

/**
 * Calls `once` (idempotent) when the response ends, fails or upgrades; each
 * callback passes through to the request's own handler.
 */
const releasingHandler = (
  handler: Dispatcher.DispatchHandler,
  once: () => void,
): Dispatcher.DispatchHandler => ({
  onRequestStart: (controller, context) =>
    handler.onRequestStart?.(controller, context),
  onRequestUpgrade: (controller, statusCode, headers, socket) => {
    once();
    handler.onRequestUpgrade?.(controller, statusCode, headers, socket);
  },
  onResponseStart: (controller, statusCode, headers, statusMessage) =>
    handler.onResponseStart?.(controller, statusCode, headers, statusMessage),
  onResponseData: (controller, chunk) =>
    handler.onResponseData?.(controller, chunk),
  onResponseEnd: (controller, trailers) => {
    once();
    handler.onResponseEnd?.(controller, trailers);
  },
  onResponseError: (controller, error) => {
    once();
    handler.onResponseError?.(controller, error);
  },
});

/**
 * The HTTP dispatcher of the drill's public-ingress route: the requests of
 * this process to `nodeEndpoint` behave as one client inside the limits the
 * node and its ingress set for one client. At most `CLIENT_ACTIVE_MAX` are in
 * flight at once (the node's per-client cap, and the ingress's per-address
 * connection cap), and each rate class is paced to its share of the node's
 * resolved limits, timed when the request leaves, so requests that waited
 * for a slot do not leave together. Requests to any other origin pass
 * untouched. `connect` carries the TLS trust of the node-tls preload.
 */
export const publicIngressDispatcher = ({
  nodeEndpoint,
  rateLimits,
  connect,
}: {
  readonly nodeEndpoint: string;
  readonly rateLimits: HttpRateLimitPolicy;
  readonly connect?: Agent.Options["connect"];
}): Dispatcher => {
  const nodeOrigin = new URL(nodeEndpoint).origin;
  const slots = createSlots(CLIENT_ACTIVE_MAX);
  const nextAt = new Map<HttpRateClass, number>();
  const paceUntilDue = async (kind: HttpRateClass) => {
    const now = performance.now();
    const due = Math.max(now, nextAt.get(kind) ?? now);
    nextAt.set(kind, due + clientPaceMs(rateLimits, kind));
    if (due > now) await sleep(due - now);
  };
  const oneClient: Dispatcher.DispatcherComposeInterceptor =
    (dispatch) => (options, handler) => {
      if (
        options.origin === undefined ||
        new URL(options.origin).origin !== nodeOrigin
      )
        return dispatch(options, handler);
      const path = options.path.split("?", 1)[0]!;
      const routeKind = httpRouteKindOf(path);
      void (async () => {
        await slots.acquire();
        let released = false;
        const release = () => {
          if (released) return;
          released = true;
          slots.release();
        };
        try {
          // A route the node does not serve is refused before any bucket.
          if (routeKind !== undefined)
            await paceUntilDue(
              httpRateClassOf(options.method, path, routeKind),
            );
          dispatch(options, releasingHandler(handler, release));
        } catch (error) {
          release();
          handler.onResponseError?.(
            undefined as unknown as Dispatcher.DispatchController,
            error instanceof Error ? error : new Error(String(error)),
          );
        }
      })();
      return true;
    };
  return new Agent({
    connections: CLIENT_ACTIVE_MAX,
    pipelining: 1,
    ...(connect === undefined ? {} : { connect }),
  }).compose(oneClient);
};

/**
 * On the public-ingress route, makes every fetch of this process go through
 * the one-client dispatcher, keeping the TLS trust the node-tls preload
 * would set from the same variables.
 */
export const installRouteHttpClient = (
  config: E2EL2StressConfig,
  env: NodeJS.ProcessEnv,
): void => {
  if (config.route?.kind !== "public-ingress") return;
  const connect = nodeTlsConnectFromEnv(env);
  setGlobalDispatcher(
    publicIngressDispatcher({
      nodeEndpoint: config.nodeEndpoint,
      rateLimits: config.route.rateLimits,
      ...(connect === undefined ? {} : { connect }),
    }),
  );
};

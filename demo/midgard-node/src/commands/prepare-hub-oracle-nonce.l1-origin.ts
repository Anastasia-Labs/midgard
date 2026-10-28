/**
 * The deployment's L1 origin as the nonce command records it: the local
 * node's tip, read before the hub-oracle nonce is first signed and settled
 * under `confirmationDepth` blocks before the nonce is broadcast. The point is
 * written with the signed transaction, ahead of any submission, so every
 * block that holds an output created after the broadcast lies after it.
 */
import {
  chainPoint,
  IntersectNotFoundError,
  L1NodeTransport,
  ORIGIN,
  TransportFailedError,
  type TransportFailedReason,
  TransportUnavailableError,
  type TransportUnreadyReason,
} from "@al-ft/l1-node-transport";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { formatL1Origin, type L1Origin } from "@al-ft/midgard-core/l1-origin";
import type { NativeLedgerNetwork } from "@al-ft/midgard-l1-follower/native-reward-account";
import { Data, Duration, Effect, type Scope } from "effect";

import {
  NATIVE_LEDGER_SETTING_NAMES,
  nativeLedgerNetworkMagic,
  type NativeLedgerSettings,
} from "../services/native-ledger.js";

/** The local node's chain, as the origin record reads it. */
export type L1ChainObserver = Readonly<{
  /** The node's tip; undefined while its chain holds no block. */
  tip: () => Promise<L1Origin | undefined>;
  /**
   * Follows the node's chain from `point` until `depth` blocks extend it
   * (`settled`), or until the chain no longer holds `point` (`rolled_back`).
   * Stops following once `signal` aborts.
   */
  settle: (
    point: L1Origin,
    depth: number,
    signal?: AbortSignal,
  ) => Promise<"settled" | "rolled_back">;
}>;

/**
 * Where the nonce command reads the origin, how deep it settles it, and how
 * long it waits for the node to extend a tip by `depth` blocks.
 */
export type HubOracleNonceOriginSource = Readonly<{
  observer: L1ChainObserver;
  depth: number;
  settleBoundMs: number;
}>;

/**
 * The settle wait's factor over the expected time for the blocks it waits
 * for. The deployment profiles' chain-growth bound already allows three
 * expected block intervals per block; ten leaves a slow or briefly
 * disconnected local node room on top of it.
 */
export const ORIGIN_SETTLE_BOUND_FACTOR = 10;

/**
 * The bound on the settle wait for `depth` blocks: `depth` × the profile's
 * expected block interval (`slot_length_ms / active_slot_coeff`, 20 s on
 * every current profile) × `ORIGIN_SETTLE_BOUND_FACTOR`; 2,000 s at the
 * testing profiles' depth of 10 and 6,000 s at the public profiles' 30.
 */
export const originSettleBoundMs = (
  depth: number,
  l1Finality: Readonly<{
    slot_length_ms: number;
    active_slot_coeff: string;
  }> = SELECTED_DEPLOYMENT_PROFILE.l1_finality,
): number =>
  Math.ceil(
    (depth * l1Finality.slot_length_ms * ORIGIN_SETTLE_BOUND_FACTOR) /
      Number(l1Finality.active_slot_coeff),
  );

/** Tips read before the command gives up with nothing signed. */
export const ORIGIN_SETTLE_ATTEMPTS = 5;

export class HubOracleNonceOriginError extends Data.TaggedError(
  "HubOracleNonceOriginError",
)<{ readonly message: string; readonly cause?: unknown }> {}

/** The refusal of a fresh nonce on a node with no local node configured. */
export const noLocalNodeOriginError = (): HubOracleNonceOriginError =>
  new HubOracleNonceOriginError({
    message: `No local node is configured (${NATIVE_LEDGER_SETTING_NAMES.join(", ")}): the deployment's L1 origin is read from it before the hub-oracle nonce is signed, so no nonce was signed`,
  });

const observing = <A>(
  what: string,
  read: (signal: AbortSignal) => Promise<A>,
) =>
  Effect.tryPromise({
    try: read,
    catch: (cause) =>
      new HubOracleNonceOriginError({
        message: `Failed to ${what}: ${formatUnknownError(cause)}`,
        cause,
      }),
  });

/**
 * Reads the node's tip and follows its chain until `depth` blocks extend it.
 * A tip that leaves the chain first is read again; after
 * `ORIGIN_SETTLE_ATTEMPTS` such tips the command fails with nothing signed.
 * A tip the node does not extend by `depth` blocks within `settleBoundMs`
 * fails the command too, with nothing signed.
 */
export const settledL1Origin = ({
  observer,
  depth,
  settleBoundMs,
}: HubOracleNonceOriginSource): Effect.Effect<
  L1Origin,
  HubOracleNonceOriginError
> =>
  Effect.gen(function* () {
    for (let attempt = 1; attempt <= ORIGIN_SETTLE_ATTEMPTS; attempt += 1) {
      const tip = yield* observing("read the local node's tip", observer.tip);
      if (tip === undefined)
        return yield* Effect.fail(
          new HubOracleNonceOriginError({
            message:
              "The local node's chain holds no block yet, so no L1 origin can be recorded before the hub-oracle nonce",
          }),
        );
      const settled = yield* observing(
        `follow the local node's chain from ${formatL1Origin(tip)}`,
        (signal) => observer.settle(tip, depth, signal),
      ).pipe(
        Effect.timeoutFail({
          duration: Duration.millis(settleBoundMs),
          onTimeout: () =>
            new HubOracleNonceOriginError({
              message: `The local node did not extend its tip ${formatL1Origin(tip)} by ${depth.toString()} blocks within ${Math.ceil(settleBoundMs / 1000).toString()} s; no hub-oracle nonce was signed. Check that the local node is synced and following the network, then run the command again`,
            }),
        }),
      );
      if (settled === "settled") return tip;
      yield* Effect.logWarning(
        `L1 origin ${formatL1Origin(tip)} left the local node's chain before ${depth.toString()} blocks extended it; reading the tip again`,
      );
    }
    return yield* Effect.fail(
      new HubOracleNonceOriginError({
        message: `The local node's tip left its chain ${ORIGIN_SETTLE_ATTEMPTS.toString()} times before ${depth.toString()} blocks extended it; no hub-oracle nonce was signed`,
      }),
    );
  });

const SETTLE_CREDIT = 16;

/** The observer over the local node's chain-sync. */
export const l1NodeChainObserver = (
  transport: Pick<L1NodeTransport, "openChainSync">,
): L1ChainObserver => ({
  tip: async () => {
    const stream = transport.openChainSync({
      points: [ORIGIN],
      credit: 1,
      resume: false,
    });
    try {
      const { tip } = await stream.opened;
      return tip.point.kind === "origin"
        ? undefined
        : { slot: Number(tip.point.slot), blockHash: tip.point.hash };
    } finally {
      await stream.close();
    }
  },
  settle: async (point, depth, signal) => {
    const stream = transport.openChainSync({
      points: [chainPoint(BigInt(point.slot), point.blockHash)],
      credit: SETTLE_CREDIT,
      resume: false,
    });
    const stop = () => void stream.close();
    signal?.addEventListener("abort", stop, { once: true });
    try {
      try {
        await stream.opened;
      } catch (error) {
        if (error instanceof IntersectNotFoundError) return "rolled_back";
        throw error;
      }
      // The hashes of the blocks on top of `point`, oldest first.
      const above: string[] = [];
      while (above.length < depth) {
        const event = await stream.next();
        if (event === undefined)
          throw new Error(
            "the chain-sync stream ended before the origin settled",
          );
        stream.ack(event.seq);
        if (event.kind === "roll_forward") {
          above.push(event.point.hash);
          continue;
        }
        const to = event.point;
        if (to.kind === "point" && to.hash === point.blockHash) {
          above.length = 0;
          continue;
        }
        const kept = to.kind === "point" ? above.indexOf(to.hash) : -1;
        if (kept < 0) return "rolled_back";
        above.length = kept + 1;
      }
      return "settled";
    } finally {
      signal?.removeEventListener("abort", stop);
      await stream.close();
    }
  },
});

/**
 * The production origin source: the local node's chain, settled under the
 * deployment's confirmation depth (the depth its manifest records) within
 * `originSettleBoundMs` of that depth.
 */
export const l1NodeOriginSource = (
  transport: Pick<L1NodeTransport, "openChainSync">,
): HubOracleNonceOriginSource => ({
  observer: l1NodeChainObserver(transport),
  depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  settleBoundMs: originSettleBoundMs(
    DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  ),
});

/** The node settings that name its local node. */
export type LocalNodeConfig = Readonly<{
  L1_NATIVE_LEDGER?: NativeLedgerSettings | undefined;
  NETWORK: NativeLedgerNetwork;
}>;

/**
 * How long a one-shot command waits for its local node transport to be
 * ready: over the sidecar's 30 s handshake bound, so a slow handshake is not
 * cut short.
 */
export const LOCAL_NODE_READY_TIMEOUT_MS = 60_000;

/** The local node transport was not ready within the bound. */
export class LocalNodeNotReadyError extends Data.TaggedError(
  "LocalNodeNotReadyError",
)<{
  readonly reason: TransportUnreadyReason | TransportFailedReason;
  readonly message: string;
  readonly cause?: unknown;
}> {}

/** What an operator checks for each reason the transport is not ready. */
const notReadyHint = (reason: string): string => {
  switch (reason) {
    case "sidecar_unavailable":
      return "check that L1_NODE_TRANSPORT_BINARY_PATH names the midgard-l1-node-transport binary";
    case "node_unreachable":
      return "check that the cardano-node is running and that L1_NODE_SOCKET_PATH names its socket";
    default:
      return "check the cardano-node and the node transport";
  }
};

/**
 * The transport to the node's configured local node, open and ready for the
 * scope, or undefined when no local node is configured. A transport not ready
 * within `readyTimeoutMs` fails with `LocalNodeNotReadyError`.
 */
export const openLocalNodeTransport = (
  config: LocalNodeConfig,
  readyTimeoutMs: number = LOCAL_NODE_READY_TIMEOUT_MS,
): Effect.Effect<
  L1NodeTransport | undefined,
  HubOracleNonceOriginError | LocalNodeNotReadyError,
  Scope.Scope
> =>
  Effect.gen(function* () {
    const settings = config.L1_NATIVE_LEDGER;
    if (settings === undefined) return undefined;
    const networkMagic = yield* observing(
      "read the local node's network magic",
      () => nativeLedgerNetworkMagic(settings, config.NETWORK),
    );
    const transport = yield* Effect.acquireRelease(
      Effect.try({
        try: () =>
          new L1NodeTransport({
            binaryPath: settings.binaryPath,
            socketPath: settings.socketPath,
            networkMagic,
          }),
        catch: (cause) =>
          new HubOracleNonceOriginError({
            message: `Failed to open the local node's transport: ${formatUnknownError(cause)}`,
            cause,
          }),
      }),
      (opened) => Effect.promise(() => opened.close()),
    );
    yield* Effect.tryPromise({
      try: () => transport.whenReady(readyTimeoutMs),
      catch: (cause) =>
        cause instanceof TransportUnavailableError ||
        cause instanceof TransportFailedError
          ? new LocalNodeNotReadyError({
              reason: cause.reason,
              message: `The local node transport was not ready within ${Math.ceil(readyTimeoutMs / 1000).toString()} s (${cause.reason}: ${cause.detail}); ${notReadyHint(cause.reason)}`,
              cause,
            })
          : new HubOracleNonceOriginError({
              message: `Failed to wait for the local node's transport: ${formatUnknownError(cause)}`,
              cause,
            }),
    });
    return transport;
  });

/**
 * The origin source of the node's configured local node, open for the scope,
 * or undefined when no local node is configured (the nonce command then
 * refuses before signing).
 */
export const nodeHubOracleNonceOriginSource = (
  config: LocalNodeConfig,
  readyTimeoutMs?: number,
): Effect.Effect<
  HubOracleNonceOriginSource | undefined,
  HubOracleNonceOriginError | LocalNodeNotReadyError,
  Scope.Scope
> =>
  Effect.map(openLocalNodeTransport(config, readyTimeoutMs), (transport) =>
    transport === undefined ? undefined : l1NodeOriginSource(transport),
  );

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
} from "@al-ft/l1-node-transport";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { formatL1Origin, type L1Origin } from "@al-ft/midgard-core/l1-origin";
import type { NativeLedgerNetwork } from "@al-ft/midgard-core/native-reward-account";
import { Data, Effect, type Scope } from "effect";

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
   */
  settle: (
    point: L1Origin,
    depth: number,
  ) => Promise<"settled" | "rolled_back">;
}>;

/** Where the nonce command reads the origin, and how deep it settles it. */
export type HubOracleNonceOriginSource = Readonly<{
  observer: L1ChainObserver;
  depth: number;
}>;

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

const observing = <A>(what: string, read: () => Promise<A>) =>
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
 */
export const settledL1Origin = ({
  observer,
  depth,
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
        () => observer.settle(tip, depth),
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
  settle: async (point, depth) => {
    const stream = transport.openChainSync({
      points: [chainPoint(BigInt(point.slot), point.blockHash)],
      credit: SETTLE_CREDIT,
      resume: false,
    });
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
      await stream.close();
    }
  },
});

/**
 * The production origin source: the local node's chain, settled under the
 * deployment's confirmation depth (the depth its manifest records).
 */
export const l1NodeOriginSource = (
  transport: Pick<L1NodeTransport, "openChainSync">,
): HubOracleNonceOriginSource => ({
  observer: l1NodeChainObserver(transport),
  depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
});

/**
 * The origin source of the node's configured local node, open for the scope,
 * or undefined when no local node is configured (the nonce command then
 * refuses before signing).
 */
export const nodeHubOracleNonceOriginSource = (
  config: Readonly<{
    L1_NATIVE_LEDGER?: NativeLedgerSettings | undefined;
    NETWORK: NativeLedgerNetwork;
  }>,
): Effect.Effect<
  HubOracleNonceOriginSource | undefined,
  HubOracleNonceOriginError,
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
    return l1NodeOriginSource(transport);
  });

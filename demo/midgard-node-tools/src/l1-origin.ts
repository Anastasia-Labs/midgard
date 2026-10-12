/**
 * The node's L1 follower inputs, as the devnet and full-stack builders write
 * them. The follower runs only with a local node, `L1_ORIGIN` and the
 * hub-oracle one-shot (`midgard-node` `services/l1-follower.plan.ts`), so a
 * builder derives the origin from the deployment's own record, the
 * `prepare-hub-oracle-one-shot-nonce` tx, and refuses to write a node
 * environment that would leave the follower unconfigured.
 */
import { L1NodeTransport, ORIGIN } from "@al-ft/l1-node-transport";
import {
  formatL1Origin,
  type L1Origin,
  parseL1Origin,
} from "@al-ft/midgard-core/l1-origin";
import { findOrigin, type FindOriginResult } from "@al-ft/midgard-l1-follower";
import {
  NATIVE_LEDGER_SETTING_NAMES,
  parseNativeLedgerSettings,
} from "midgard-node/services/native-ledger";

/** The deployment's origin could not be determined; no node env is written. */
export class L1OriginUndeterminedError extends Error {
  override readonly name = "L1OriginUndeterminedError";
}

/** A node environment would leave the node's L1 follower unconfigured. */
export class NodeFollowerUnconfiguredError extends Error {
  override readonly name = "NodeFollowerUnconfiguredError";
}

/** How a builder reaches the run's local Cardano node. */
export type L1NodeConnection = Readonly<{
  socketPath: string;
  /** The compiled `midgard-l1-node-transport` binary. */
  binaryPath: string;
  networkMagic: number;
}>;

/** What the builders record: the origin and the nonce tx it was derived from. */
export type DerivedL1Origin = Readonly<{
  origin: L1Origin;
  nonceTxHash: string;
  nonceBlock: L1Origin;
}>;

const withTransport = async <T>(
  node: L1NodeConnection,
  use: (transport: L1NodeTransport) => Promise<T>,
): Promise<T> => {
  const transport = new L1NodeTransport(node);
  try {
    return await use(transport);
  } finally {
    await transport.close();
  }
};

const describeFailure = (
  result: Exclude<FindOriginResult, { kind: "found" }>,
): string => {
  switch (result.kind) {
    case "not_found":
      return `the node's chain holds no such tx after the scan start (scanned to ${
        result.scannedTo === "genesis"
          ? "genesis"
          : formatL1Origin(result.scannedTo)
      })`;
    case "from_not_on_chain":
      return "the scan start is not on the node's chain";
    case "no_preceding_point":
      return "the tx is in the chain's first block, which has no preceding point";
  }
};

/**
 * The deployment origin O from the node's chain: the point immediately
 * before the block holding the `prepare-hub-oracle-one-shot-nonce` tx
 * (`find-origin`, plan §5.3). `from` is a point known to precede that block;
 * without it the scan starts at genesis.
 */
export const deriveL1Origin = async (input: {
  readonly node: L1NodeConnection;
  readonly nonceTxHash: string;
  readonly from?: L1Origin;
  /** The scan; the builders' tests replace it. Default: `findOrigin` on the node. */
  readonly find?: (options: {
    readonly txHash: string;
    readonly from?: L1Origin;
  }) => Promise<FindOriginResult>;
}): Promise<DerivedL1Origin> => {
  const txHash = input.nonceTxHash.toLowerCase();
  const find =
    input.find ??
    ((options) =>
      withTransport(input.node, (transport) =>
        findOrigin({ transport, ...options }),
      ));
  const result = await find({
    txHash,
    ...(input.from === undefined ? {} : { from: input.from }),
  });
  if (result.kind !== "found")
    throw new L1OriginUndeterminedError(
      `cannot derive L1_ORIGIN from the hub-oracle nonce tx ${txHash}: ${describeFailure(result)}`,
    );
  return {
    origin: result.origin,
    nonceTxHash: txHash,
    nonceBlock: {
      slot: result.nonceBlock.slot,
      blockHash: result.nonceBlock.blockHash,
    },
  };
};

/**
 * The node's current tip, or undefined at genesis. Read before the nonce tx
 * is first signed, it precedes the nonce block, so it bounds the origin scan.
 */
export const readL1NodeTip = (
  node: L1NodeConnection,
): Promise<L1Origin | undefined> =>
  withTransport(node, async (transport) => {
    const stream = transport.openChainSync({ points: [ORIGIN], credit: 1 });
    try {
      const { tip } = await stream.opened;
      return tip.point.kind === "origin"
        ? undefined
        : { slot: Number(tip.point.slot), blockHash: tip.point.hash };
    } finally {
      await stream.close();
    }
  });

const HEX_32 = /^[0-9a-f]{64}$/u;

/**
 * Refuses a node environment that would leave its L1 follower unconfigured:
 * the local node keys, `L1_ORIGIN` and the hub-oracle one-shot, read with the
 * node's own parsers.
 */
export const assertNodeFollowerEnvironment = (
  env: Readonly<Record<string, string | undefined>>,
): void => {
  const refuse = (detail: string): never => {
    throw new NodeFollowerUnconfiguredError(
      `the node environment leaves the L1 follower unconfigured: ${detail}`,
    );
  };
  try {
    if (
      parseNativeLedgerSettings(
        Object.fromEntries(
          NATIVE_LEDGER_SETTING_NAMES.map((name) => [name, env[name]]),
        ) as Parameters<typeof parseNativeLedgerSettings>[0],
      ) === undefined
    )
      refuse(`${NATIVE_LEDGER_SETTING_NAMES.join(", ")} are not set`);
  } catch (error) {
    if (error instanceof NodeFollowerUnconfiguredError) throw error;
    refuse((error as Error).message);
  }
  const origin = env.L1_ORIGIN ?? "";
  if (origin === "") refuse("L1_ORIGIN is not set");
  try {
    parseL1Origin(origin, "L1_ORIGIN");
  } catch (error) {
    refuse((error as Error).message);
  }
  const index = Number(env.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX ?? "");
  if (
    !HEX_32.test(env.HUB_ORACLE_ONE_SHOT_TX_HASH?.toLowerCase() ?? "") ||
    !/^(?:0|[1-9][0-9]*)$/u.test(env.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX ?? "") ||
    !Number.isSafeInteger(index)
  )
    refuse(
      "HUB_ORACLE_ONE_SHOT_TX_HASH and HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX are not set",
    );
};

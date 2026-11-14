/**
 * The node's L1 follower inputs, as the devnet and full-stack builders write
 * them. The follower runs only with a local node, `L1_ORIGIN` and the
 * hub-oracle one-shot (`midgard-node` `services/l1-follower.plan.ts`), so a
 * builder takes the origin from the deployment's own record, the point the
 * `prepare-hub-oracle-one-shot-nonce` command recorded in run state before it
 * signed the nonce, proves it on the node's chain, and refuses to write a node
 * environment that would leave the follower unconfigured.
 */
import { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  formatL1Origin,
  type L1Origin,
  parseL1Origin,
} from "@al-ft/midgard-core/l1-origin";
import { findOrigin, type FindOriginResult } from "@al-ft/midgard-l1-follower";
import { loadDeploymentRunState } from "midgard-node/e2e/run-state";
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

/** What the builders record: the origin and the nonce tx it was proven for. */
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
  recorded: L1Origin,
  result: Exclude<FindOriginResult, { kind: "found" }>,
): string => {
  switch (result.kind) {
    case "not_found":
      return `the node's chain after the recorded origin ${formatL1Origin(recorded)} holds no such tx (scanned to ${
        result.scannedTo === "genesis"
          ? "genesis"
          : formatL1Origin(result.scannedTo)
      }), so the recorded origin does not precede the tx's block, or the node is not synced`;
    case "from_not_on_chain":
      return `the recorded origin ${formatL1Origin(recorded)} is not on the node's chain`;
    case "no_preceding_point":
      return "the tx is in the chain's first block, which no recorded origin can precede";
  }
};

/**
 * Proves the deployment's recorded origin T on the node's chain: the scan
 * from T must find the `prepare-hub-oracle-one-shot-nonce` tx, so T is on
 * the chain and precedes the block holding it (`find-origin --recorded`,
 * plan §5.3). Returns T with that block; refuses otherwise.
 */
export const deriveL1Origin = async (input: {
  readonly node: L1NodeConnection;
  readonly nonceTxHash: string;
  /** T, as the run state's `identity.l1Origin` records it. */
  readonly recorded: L1Origin;
  /** The scan; the builders' tests replace it. Default: `findOrigin` on the node. */
  readonly find?: (options: {
    readonly txHash: string;
    readonly from: L1Origin;
  }) => Promise<FindOriginResult>;
}): Promise<DerivedL1Origin> => {
  const txHash = input.nonceTxHash.toLowerCase();
  const find =
    input.find ??
    ((options) =>
      withTransport(input.node, (transport) =>
        findOrigin({ transport, ...options }),
      ));
  const result = await find({ txHash, from: input.recorded });
  if (result.kind !== "found")
    throw new L1OriginUndeterminedError(
      `cannot prove L1_ORIGIN for the hub-oracle nonce tx ${txHash}: ${describeFailure(input.recorded, result)}`,
    );
  return {
    origin: input.recorded,
    nonceTxHash: txHash,
    nonceBlock: {
      slot: result.nonceBlock.slot,
      blockHash: result.nonceBlock.blockHash,
    },
  };
};

/**
 * T for `nonceTxHash`: the origin the deployment run state at `statePath`
 * recorded before the nonce was first signed. Refuses when the run state
 * records another nonce or no origin, as a run state written before origins
 * were recorded does; such a deployment is redeployed.
 */
export const runStateL1Origin = async (
  statePath: string,
  nonceTxHash: string,
): Promise<L1Origin> => {
  const identity = (await loadDeploymentRunState(statePath))?.identity;
  const txHash = nonceTxHash.toLowerCase();
  if (identity?.hubOracleOneShot?.txHash.toLowerCase() !== txHash)
    throw new L1OriginUndeterminedError(
      `the deployment run state ${statePath} does not record the hub-oracle nonce ${txHash}`,
    );
  if (identity.l1Origin === undefined)
    throw new L1OriginUndeterminedError(
      `the deployment run state ${statePath} records no L1 origin for the hub-oracle nonce ${txHash}; the nonce command records one before it signs, and a deployment without one is redeployed`,
    );
  return identity.l1Origin;
};

/** Whether `a` and `b` are the same point. */
export const sameL1Origin = (a: L1Origin, b: L1Origin): boolean =>
  a.slot === b.slot && a.blockHash === b.blockHash;

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

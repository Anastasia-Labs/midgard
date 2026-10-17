/**
 * The availability command's canonical source, over the tool L1 access
 * `--l1` selects (`l1-command-access.ts`; option E). The command is a tool:
 * it never reads a role's follower store, so it runs before any `listen`.
 *
 * - `--l1 node`: the local node's ledger. The boundary is the ledger tip
 *   (point and block number from one acquired state) and an anchor is
 *   checked by acquiring it. The ledger holds no transaction history, so a
 *   read that needs it (a transaction's block, a foreign spend, the Open's
 *   commitment preimage) refuses, naming `--l1 kupmios`
 *   (`AvailabilityHistoryUnavailableError`). Every action that submits or
 *   recovers observes its operations' inclusion, so only `status` runs on
 *   node; the others are refused before anything is built or submitted
 *   (`assertAvailabilityActionServed`).
 * - `--l1 kupmios`: the whole command, history included, over Kupo and
 *   Ogmios (`../l1-external/kupmios-availability-source.ts`, loaded only
 *   here).
 * - `--l1 blockfrost`: refused (no history reader is built for it).
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { NodeLedgerAccess } from "../services/l1-node-ledger-access.js";
import type {
  AvailabilityCommandAction,
  AvailabilityUnitHistory,
} from "./availability-challenge.plan-availability-command-action.js";
import type { ToolL1Access } from "./l1-command-access.js";

export type CanonicalBoundary = Readonly<{
  pointId: string;
  slot: number;
  blockNo: number;
  blockHash: string;
}>;

/** What the availability command reads the chain through. */
export type AvailabilityCanonicalSource = Readonly<{
  readBoundary: () => Promise<CanonicalBoundary>;
  assertCanonicalAncestor: (
    anchor: Readonly<{ slot: number; blockHash: string }>,
  ) => Promise<void>;
  observe: SDK.DaAvailabilityOperationContext["observe"];
  /** The datums of every output that held a unit, live or spent. */
  unitHistory: AvailabilityUnitHistory;
}>;

/** The chosen L1 access cannot serve a read the availability command needs. */
export class AvailabilityHistoryUnavailableError extends Error {
  override readonly name = "AvailabilityHistoryUnavailableError";
  readonly reason = "availability_history_unavailable";
  constructor(
    readonly access: ToolL1Access["kind"],
    what: string,
  ) {
    super(
      `availability under --l1 ${access} cannot read ${what}: run it with --l1 kupmios (L1_KUPO_URL, L1_OGMIOS_URL), which reads chain history`,
    );
  }
}

/**
 * Refuses, before anything is built or submitted, an action the access
 * cannot see through: on the node ledger every action but `status` observes
 * its operations' inclusion (the SDK observer resolves the including block
 * whenever a status has no depth, and the ledger has no block history), so
 * it would submit and then fail observing what it submitted.
 */
export const assertAvailabilityActionServed = (
  action: AvailabilityCommandAction,
  access: Pick<ToolL1Access, "kind">,
): void => {
  if (access.kind === "node" && action !== "status")
    throw new AvailabilityHistoryUnavailableError(
      "node",
      action === "recover"
        ? "the inclusion of the operations it recovers"
        : `the inclusion of the ${action} transaction it would submit`,
    );
};

const boundaryOf = (
  tip: Readonly<{ slot: number; hash: string; blockNo: number }>,
): CanonicalBoundary => ({
  pointId: `${tip.slot.toString()}:${tip.hash}`,
  slot: tip.slot,
  blockNo: tip.blockNo,
  blockHash: tip.hash,
});

/**
 * The node-ledger source: the boundary and anchor checks from the ledger,
 * every history read refused by name.
 */
export const availabilityNodeLedgerSource = (input: {
  readonly lucid: LucidEvolution;
  readonly access: Pick<NodeLedgerAccess, "readTip" | "pointStatus">;
}): AvailabilityCanonicalSource => {
  const readBoundary = async () => boundaryOf(await input.access.readTip());
  const refuse = (what: string) => () =>
    Promise.reject(new AvailabilityHistoryUnavailableError("node", what));
  return {
    readBoundary,
    assertCanonicalAncestor: async (anchor) => {
      const status = await input.access.pointStatus({
        slot: anchor.slot,
        hash: anchor.blockHash,
      });
      // Only a point the node can still acquire is known canonical: one
      // past its volatile window is unverifiable here, so it fails closed
      // and the rerun anchors afresh.
      if (status !== "on_chain")
        throw new Error(
          `Availability command canonical generation changed; recover durable intents before new work (anchor ${anchor.slot.toString()}:${anchor.blockHash} is ${status === "immutable" ? "past the node's volatile window" : "no longer on the node's chain"})`,
        );
    },
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      resolveInclusion: refuse("the block that included a transaction"),
      resolveForeignSpend: refuse("the transaction that spent an input"),
    }),
    unitHistory: refuse("the spent outputs that held a unit"),
  };
};

/** The canonical source over the tool access `--l1` selected. */
export const availabilityCommandCanonicalSource = async (input: {
  readonly lucid: LucidEvolution;
  readonly access: ToolL1Access;
}): Promise<AvailabilityCanonicalSource> => {
  const access = input.access;
  switch (access.kind) {
    case "node":
      return availabilityNodeLedgerSource({ lucid: input.lucid, access });
    case "kupmios": {
      const { availabilityKupmiosSource } = await import(
        "../l1-external/kupmios-availability-source.js"
      );
      return availabilityKupmiosSource({
        lucid: input.lucid,
        kupoUrl: access.kupoUrl,
        ogmiosUrl: access.ogmiosUrl,
      });
    }
    case "blockfrost":
      throw new AvailabilityHistoryUnavailableError(
        "blockfrost",
        "its canonical boundary or chain history",
      );
  }
};

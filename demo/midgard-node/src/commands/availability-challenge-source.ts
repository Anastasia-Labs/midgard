/**
 * The availability command's canonical source, over the tool L1 access
 * `--l1` selects (`l1-command-access.ts`; option E). The command is a tool:
 * it never reads a role's follower store, so it runs before any `listen`.
 * It reads chain history (a transaction's block, a foreign spend, the
 * Open's commitment preimage), so only `--l1 kupmios` serves it, over Kupo
 * and Ogmios (`../l1-external/kupmios-availability-source.ts`, loaded only
 * here). `--l1 node` and `--l1 blockfrost` are refused before anything is
 * read (`assertAvailabilityAccessServed`).
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { AvailabilityUnitHistory } from "./availability-challenge.plan-availability-command-action.js";
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

/** Refuses, before anything is read, an access with no history reader. */
export function assertAvailabilityAccessServed(
  access: Pick<ToolL1Access, "kind">,
): asserts access is Readonly<{ kind: "kupmios" }> {
  if (access.kind !== "kupmios")
    throw new AvailabilityHistoryUnavailableError(
      access.kind,
      "its canonical boundary or chain history",
    );
}

/** The canonical source over the tool access `--l1` selected. */
export const availabilityCommandCanonicalSource = async (input: {
  readonly lucid: LucidEvolution;
  readonly access: ToolL1Access;
}): Promise<AvailabilityCanonicalSource> => {
  const access = input.access;
  assertAvailabilityAccessServed(access);
  const { availabilityKupmiosSource } = await import(
    "../l1-external/kupmios-availability-source.js"
  );
  return availabilityKupmiosSource({
    lucid: input.lucid,
    kupoUrl: access.kupoUrl,
    ogmiosUrl: access.ogmiosUrl,
  });
};

/**
 * The production fold's ports in tests that fold rows they seeded straight
 * into the node database: the view held, writes behind the follower write
 * gate, and a ledger store that retains every root, so a fold opens the
 * next frontier's root and rebuilds nothing.
 */
import type { Ledger } from "@al-ft/midgard-fault-proofs/ledger-store";
import { Effect } from "effect";

import type { LandedBlockPorts } from "../../src/landed-blocks/ports.js";
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";

export const foldPorts = {
  confirmView: () => Effect.succeed(true),
  write: <A, E, R>(work: Effect.Effect<A, E, R>) => withFollowerWrite(work),
  retainedLedger: (root: string) =>
    Effect.succeed({ root } as unknown as Ledger),
  persistLedger: () => Effect.void,
} as unknown as LandedBlockPorts<never>;

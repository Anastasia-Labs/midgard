/**
 * The landed ledger's two roots from one trie: `confirmed_ledger`'s at the
 * frontier, then, with the processed deltas after it applied in order, the
 * queue tail's. Run in the verification's worker thread
 * (`workers/confirmed-ledger-root.ts`).
 */
import { Effect } from "effect";

import type {
  ConfirmedLedgerRoots,
  ConfirmedLedgerRootsInput,
} from "./confirmed-ledger-root-message.js";
import { MpfError } from "./errors.js";
import { ledgerEntryToInsertBatchOp } from "./ledger-delta.js";
import { hydrateLedgerMpfFromLedgerEntries } from "./ledger-hydration.js";
import { MidgardMpf } from "./store.js";

/** The roots of `input`, from a scratch trie built for it. */
export const deriveConfirmedLedgerRoots = (
  input: ConfirmedLedgerRootsInput,
): Effect.Effect<ConfirmedLedgerRoots, MpfError> =>
  Effect.gen(function* () {
    const mpf = yield* MidgardMpf.createScratch("confirmed-ledger-roots");
    const frontierRoot = yield* hydrateLedgerMpfFromLedgerEntries(
      mpf,
      input.confirmed,
    );
    const ops = yield* Effect.try({
      try: () =>
        input.delta.map((op) =>
          op.kind === "spent"
            ? { type: "delete" as const, key: Buffer.from(op.outref) }
            : ledgerEntryToInsertBatchOp(op),
        ),
      catch: (cause) => MpfError.rootBuild("landed ledger delta", cause),
    });
    yield* mpf.applyBatch(ops);
    return { frontierRoot, tailRoot: yield* mpf.rootHex() };
  });

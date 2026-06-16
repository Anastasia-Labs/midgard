import { Data, Effect, Schedule } from "effect";

import {
  MidgardMpf,
  MPF_EMPTY_ROOT_HEX,
  type MpfError,
} from "@/workers/utils/mpf.js";

const TRIE_NAME = "ledger-non-membership";

// The persisted ledger store is held by the commit worker only while a block is
// being committed. Opening a second handle during that window fails on the
// LevelDB lock, so retry briefly to ride over an in-flight commit.
const OPEN_RETRY_SCHEDULE = Schedule.intersect(
  Schedule.spaced("500 millis"),
  Schedule.recurs(40),
);

/**
 * Raised when the requested input is actually present in the prev-utxos ledger
 * at the given root — i.e. it is a real, existing input, so there is no
 * non-existent-input fraud to prove.
 */
export class LedgerInputPresentError extends Data.TaggedError(
  "LedgerInputPresentError",
)<{
  readonly root: string;
  readonly inputCbor: string;
}> {}

export type LedgerNonMembershipResult = {
  readonly prevUtxosRoot: string;
  readonly input: string;
  readonly proofCbor: string;
};

/**
 * Builds a ledger non-membership (exclusion) proof for `inputCbor` (a Cardano
 * `TransactionInput` CBOR, the ledger trie key) against the historical ledger
 * root `prevUtxosRoot`. The proof reconstructs that exact committed root, which
 * is what the on-chain non-existent-input step-03 (`pexcludes`) validator
 * checks against the disputed block header's `prevUtxosRoot`.
 *
 * The empty root needs no persisted state, so it is served from a scratch trie
 * (also lets the empty-genesis drill work without touching LevelDB). Any other
 * root is reconstructed from the node's persisted ledger store in overlay mode,
 * so the persisted store is never mutated.
 */
export const buildLedgerNonMembershipProof = ({
  ledgerDbPath,
  prevUtxosRoot,
  inputCbor,
}: {
  readonly ledgerDbPath: string;
  readonly prevUtxosRoot: string;
  readonly inputCbor: string;
}): Effect.Effect<
  LedgerNonMembershipResult,
  MpfError | LedgerInputPresentError
> => {
  const rootBuf = Buffer.from(prevUtxosRoot, "hex");
  const absentKey = Buffer.from(inputCbor, "hex");
  const acquire =
    prevUtxosRoot === MPF_EMPTY_ROOT_HEX
      ? MidgardMpf.createScratch(TRIE_NAME)
      : MidgardMpf.loadReadOnlyOverlay(TRIE_NAME, ledgerDbPath, rootBuf).pipe(
          Effect.retry(OPEN_RETRY_SCHEDULE),
        );
  return Effect.acquireUseRelease(
    acquire,
    (mpf) =>
      Effect.gen(function* () {
        const result = yield* mpf.proveNonMembership(absentKey);
        if (result.present) {
          return yield* Effect.fail(
            new LedgerInputPresentError({
              root: prevUtxosRoot,
              inputCbor,
            }),
          );
        }
        return {
          prevUtxosRoot,
          input: inputCbor,
          proofCbor: result.proofCbor.toString("hex"),
        } satisfies LedgerNonMembershipResult;
      }),
    (mpf) => mpf.close().pipe(Effect.catchAll(() => Effect.void)),
  );
};

import { randomBytes } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
  prepareRetainedNativeHistoryRecoveryPlan,
  retainedPreparedRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import { SignedIntentReplacementIntegrityError } from "./canonical-journal-recovery.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import { openRetainedNativeOwner } from "./history-expired-intent-release.open-retained-native-owner.js";
import { ownedBy } from "./history-expired-intent-release.owned.js";
import { heldRootRefusal } from "./history-expired-intent-release.retained-journal-digest.js";
import { journalIdentity } from "./history-expired-intent-release.signed-commit-node.js";
import { C, failure, sha } from "./history-expired-intent-release.table.js";
import { nativeOwnerOpenWait } from "./state-queue-correction-rewind.prepare-state-queue-correction-rewind.js";

/** Immutable recovery identity for the winning journal and every displaced
 * replay. Status is checked by the caller's fresh proof, not used as identity. */
export const displacementIdentity = (
  winner: Pending.Record,
  displaced: readonly Pending.Record[],
) =>
  sha(
    eventHistoryCanonicalJson({
      winner: journalIdentity(winner),
      displaced: displaced.map(journalIdentity),
    }),
  );

/** A displaced root-moving chain's retained CAS and SQL repair. The caller
 * proves again, under recovery authority, that the canonical winner occupies
 * the base slot and all displaced journals are absent from canonical L1. No
 * native mutation happens until the plan is durable; SQL reinclusion and its
 * applied receipt commit together. A crash after CAS resumes the same ID. */
export const recoverDisplacement = <E, R>(input: {
  readonly bindingDigest: string;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly winner: Pending.Record;
  readonly displaced: readonly Pending.Record[];
  readonly verify: Effect.Effect<void, E, R>;
  readonly repair: Effect.Effect<void, E, R>;
  readonly afterSqlCommit: Effect.Effect<void>;
}) =>
  Effect.gen(function* () {
    const { winner, displaced, checkpoint, preparation } = input;
    const header = winner[C.HEADER_HASH].toString("hex");
    const targetRoot = winner[C.BASE_UTXOS_ROOT];
    const owned = ownedBy(preparation);
    const globals = yield* Globals;
    const owner = yield* openRetainedNativeOwner(globals, input.config).pipe(
      Effect.catchIf(
        (error) => nativeOwnerOpenWait(error) !== undefined,
        (error) =>
          Effect.logWarning(nativeOwnerOpenWait(error)!).pipe(
            Effect.as(undefined),
          ),
      ),
    );
    if (owner === undefined) return false;
    yield* preparation.assertCurrent;
    const { durableRoot } = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Retained native displacement diagnostics failed", cause),
    });
    const accepted = new Set([
      targetRoot,
      ...displaced.map((block) => block[C.EXPECTED_UTXOS_ROOT]),
    ]);
    const retained = yield* owned(
      retainedPreparedRecoveryPlan(input.bindingDigest),
    );
    const candidateRoot =
      retained?.kind === "displaced_block_revival"
        ? retained.expectedRoot
        : durableRoot;
    if (!accepted.has(durableRoot) || !accepted.has(candidateRoot))
      return yield* Effect.fail(
        new SignedIntentReplacementIntegrityError(
          header,
          `native root ${durableRoot} or retained CAS root ${candidateRoot} is outside the displaced replay chain`,
        ),
      );
    const journalDigest = displacementIdentity(winner, displaced);
    const plan = yield* owned(
      input.verify.pipe(
        Effect.zipRight(
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            {
              operationNonce:
                retained?.kind === "displaced_block_revival"
                  ? retained.operationNonce
                  : randomBytes(32).toString("hex"),
              bindingDigest: input.bindingDigest,
              manifestId: checkpoint.manifestId,
              headerHash: header,
              signedTransactionHash:
                winner[C.INTENDED_TX_HASH]!.toString("hex"),
              signedTransactionCborSha256: sha(winner[C.SIGNED_TX_CBOR]!),
              targetRoot,
              journalDigest,
              displacedHeaderHashes: displaced.map((record) =>
                record[C.HEADER_HASH].toString("hex"),
              ),
            },
            sha(
              eventHistoryCanonicalJson({
                journalDigest,
                point: checkpoint.head,
                snapshot: checkpoint.capture.snapshotDigest,
              }),
            ),
            { durableRoot, candidateRoot },
            DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
          ).pipe(
            Effect.mapError(
              heldRootRefusal(header, {
                durableRoot,
                targetRoot,
                candidateRoot,
              }),
            ),
          ),
        ),
      ),
    );
    yield* executeHistoryDependentRecovery({
      checkpoint,
      preparation,
      plan,
      owner,
      repair: Effect.gen(function* () {
        yield* input.verify;
        const parent = yield* Pending.retrieveByHeaderHash(
          winner[C.BASE_TAIL_HEADER_HASH],
        );
        const aggregate =
          Option.isSome(parent) &&
          parent.value[C.EXPECTED_UTXOS_ROOT] === targetRoot
            ? parent.value.utxoPayloadAggregate
            : undefined;
        const sql = yield* SqlClient.SqlClient;
        const rows =
          yield* sql`UPDATE mpf_engine_state SET root_hex = ${targetRoot},
        utxo_payload_entry_count = ${aggregate?.entryCount ?? null},
        utxo_payload_encoded_tuple_bytes = ${aggregate?.encodedTupleBytes ?? null}, updated_at = NOW()
        WHERE store_name = 'ledger' AND root_hex IN ${sql.in([...accepted])} RETURNING store_name`;
        if (rows.length !== 1)
          return yield* Effect.fail(
            new SignedIntentReplacementIntegrityError(
              header,
              "the SQL ledger marker is outside the displaced replay chain",
            ),
          );
        yield* input.repair;
      }),
      afterSqlCommit: input.afterSqlCommit,
    });
    return true;
  });

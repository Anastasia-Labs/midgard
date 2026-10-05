import { parseStateQueueAuthenticatedTransition } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { DaPayloadRetirementProof } from "../database/daPayloads.js";
import { recoveryRelevantJournal } from "../database/retention-holds.js";
import type { StateQueueCorrectionObserverSource } from "../services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";

/** One sweep re-proves at most this many terminal payloads; later sweeps retry. */
export const DA_RETIREMENT_PROOF_BATCH_LIMIT = 32;

/**
 * Fresh canonical proof for exact terminal identities and payload bytes. The
 * source checks their canonical input spends and tip. Its depth includes the
 * inclusion block, so output depth > k means inclusive depth > k + 1.
 * No process memo or persisted confirmation-depth value authorizes retirement.
 */
export const fetchDaPayloadRetirementProofs = (args: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly automaticRecoveryMaxDepth: number;
  readonly source: Pick<StateQueueCorrectionObserverSource, "canonicalDepth">;
}): Effect.Effect<
  {
    readonly proofs: readonly DaPayloadRetirementProof[];
    readonly unavailable: boolean;
  },
  unknown,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const identity = Buffer.from(args.deploymentIdentityDigest, "hex");
    const rows = yield* sql<{
      readonly header_hash: Buffer;
      readonly payload_sha256: Buffer;
      readonly transaction_hash: Buffer;
      readonly block_hash: Buffer;
      readonly transition_digest: Buffer;
      readonly transition_record: unknown;
      readonly terminal_outcome: string;
      readonly transition_kind: string;
    }>`SELECT terminal.header_hash, payload.payload_sha256,
        terminal.transaction_hash, terminal.block_hash, terminal.transition_digest,
        terminal.transition_record, terminal.terminal_outcome, terminal.transition_kind
      FROM da_payload_terminal_outcomes terminal
      JOIN da_payloads payload ON payload.header_hash = terminal.header_hash
      WHERE terminal.deployment_identity_digest = ${identity}
        AND NOT ${recoveryRelevantJournal(sql, "terminal.header_hash", identity)}
      ORDER BY terminal.block_no, terminal.transaction_index, terminal.header_hash
      LIMIT ${DA_RETIREMENT_PROOF_BATCH_LIMIT}`;
    let unavailable = false;
    const proofs = yield* Effect.forEach(
      rows,
      (row) =>
        Effect.gen(function* () {
          const record =
            typeof row.transition_record === "string"
              ? JSON.parse(row.transition_record)
              : row.transition_record;
          const transition = parseStateQueueAuthenticatedTransition(record);
          if (
            transition === null ||
            transition.deploymentIdentityDigest !==
              args.deploymentIdentityDigest ||
            transition.stateQueuePolicyId !== args.stateQueuePolicyId ||
            transition.removedHeaderHashes.length !== 1 ||
            transition.transitionKind !== row.transition_kind ||
            (transition.transitionKind === "merge" ? "merged" : "removed") !==
              row.terminal_outcome ||
            transition.removedHeaderHashes[0] !==
              row.header_hash.toString("hex") ||
            transition.transactionHash !==
              row.transaction_hash.toString("hex") ||
            transition.blockHash !== row.block_hash.toString("hex") ||
            transition.transitionDigest !==
              row.transition_digest.toString("hex")
          ) {
            unavailable = true;
            return undefined;
          }
          const depth = yield* Effect.tryPromise(() =>
            args.source.canonicalDepth(transition),
          );
          if (depth === null) {
            unavailable = true;
            return undefined;
          }
          if (depth <= BigInt(args.automaticRecoveryMaxDepth) + 1n) {
            return undefined;
          }
          return {
            headerHash: row.header_hash.toString("hex"),
            payloadSha256: row.payload_sha256.toString("hex"),
            transactionHash: transition.transactionHash,
            blockHash: transition.blockHash,
            transitionDigest: transition.transitionDigest,
          } satisfies DaPayloadRetirementProof;
        }),
      { concurrency: 4 },
    );
    return {
      proofs: proofs.filter(
        (proof): proof is DaPayloadRetirementProof => proof !== undefined,
      ),
      unavailable,
    };
  });

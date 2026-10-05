import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { reconcileStateQueueCorrectionObserver } from "../../src/services/state-queue-correction-observer.js";
import { authorizeStateQueueCorrectionReinclusion } from "../../src/services/state-queue-correction-recovery.js";
import {
  BASE_HEADER,
  BASE_OUT,
  bytes,
  hex,
} from "./history-expired-intent-release-before-ttl.js";
import {
  authority,
  S_HEADER,
} from "./history-expired-intent-release-displaced-sibling.js";

export const suffixRemoval = (header: Buffer) => {
  const target = header.toString("hex");
  const tx = hex("compensation:remove-child");
  const lock = `${hex("compensation:lock")}#0`;
  const previousQueue = [
    { headerHash: null, outRef: `${hex("root-tx")}#0` },
    { headerHash: BASE_HEADER.toString("hex"), outRef: BASE_OUT },
    {
      headerHash: S_HEADER.toString("hex"),
      outRef: `${hex("compensation:s-after-child")}#0`,
    },
    { headerHash: target, outRef: `${hex("compensation:child-node")}#0` },
  ];
  const nextQueue = [
    ...previousQueue.slice(0, 2),
    { headerHash: S_HEADER.toString("hex"), outRef: `${tx}#0` },
  ];
  const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: authority.manifestId,
    stateQueuePolicyId: authority.stateQueuePolicyId,
    transactionHash: tx,
    blockHash: hex("compensation:remove-block"),
    slot: "1900",
    blockNo: "8",
    transactionIndex: "0",
    chainPointId: hex("compensation:remove-point"),
    finalityDepth: "1",
    mintPolicyIds: [authority.stateQueuePolicyId],
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            RemoveFraudulentBlockHeader: {
              yield_to_ref_input_index: 0n,
              fraudulent_operator: bytes("compensation:operator", 28).toString(
                "hex",
              ),
              fraudulent_blocks_header_hash: target,
              slashing_approach: {
                OperatorAlreadySlashed: {
                  active_operators_element_ref_input_index: 0n,
                  retired_operators_element_ref_input_index: 1n,
                },
              },
              fraud_proof_ref_input_index: 0n,
              block_removal_approach: {
                RemoveLastFraudulentBlock: {
                  anchor_element_input_outref: {
                    transactionId: previousQueue[2]!.outRef.slice(0, 64),
                    outputIndex: 0n,
                  },
                  anchor_element_output_index: 0n,
                },
              },
            },
          },
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [
      previousQueue[2]!.outRef,
      previousQueue[3]!.outRef,
      lock,
    ],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: lock,
      continuedOutRef: `${tx}#9`,
      targetHeaderHash: target,
      correctionIdentity: {
        FraudProof: { fraud_proof_asset_name: `00000001${target}` },
      },
      previousDatum: "Idle",
      nextDatum: "Idle",
    },
    previousQueue,
    nextQueue,
  });
  if (checkpoint === null)
    throw new Error("Suffix removal checkpoint failed authentication");
  return { checkpoint, previousQueue, nextQueue, tx };
};
export const admitSuffixRemoval = (
  checkpoint: SDK.StateQueueAuthenticatedReplayCheckpoint,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const run = <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) =>
      Effect.runPromise(
        work.pipe(Effect.provideService(SqlClient.SqlClient, sql)),
      );
    return yield* Effect.tryPromise({
      try: () =>
        reconcileStateQueueCorrectionObserver({
          deploymentIdentityDigest: authority.manifestId,
          stateQueuePolicyId: authority.stateQueuePolicyId,
          requiredFinalityDepth: authority.requiredFinalityDepth,
          source: {
            readQueue: async () => checkpoint.nextQueue,
            observeTransitions: async () => [checkpoint],
            canonicalDepth: async () => 3n,
          },
          store: {
            load: async () => {
              const rows = await run(
                sql<{
                  state_record: unknown;
                }>`SELECT state_record FROM state_queue_terminal_observer_states`,
              );
              const raw = rows[0]!.state_record;
              return typeof raw === "string"
                ? (JSON.parse(raw) as unknown)
                : raw;
            },
            save: async (state) => {
              await run(
                sql`UPDATE state_queue_terminal_observer_states SET state_record = ${JSON.stringify(state)}, state_digest = ${Buffer.from(state.stateDigest, "hex")}`,
              );
            },
          },
          reinclude: async (transition) => {
            authorizeStateQueueCorrectionReinclusion(transition, {
              expectedDeploymentIdentityDigest: authority.manifestId,
              requiredFinalityDepth: authority.requiredFinalityDepth,
            });
          },
          restoreAfterRollback: async () => {
            throw new Error("Unexpected correction rollback");
          },
          provenFinal: new Set(),
        }),
      catch: (cause) => cause,
    });
  });

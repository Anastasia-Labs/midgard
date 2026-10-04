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
  queueNode,
  root,
  S_HEADER,
} from "./history-expired-intent-release-displaced-sibling.js";

export const A_HEADER = bytes("correction:fraudulent-ancestor", 28);
export const A_OUT = `${hex("correction:ancestor-node")}#0`;
export const S_OUT = `${hex("reversal:s-node-tx")}#0`;
const tx = hex("correction:fraud-link");
const lock = `${hex("correction:lock")}#0`;
const a = A_HEADER.toString("hex");
const d = BASE_HEADER.toString("hex");
const s = S_HEADER.toString("hex");
const identity = {
  FraudProof: { fraud_proof_asset_name: `00000001${a}` },
} as const;

// Fraud link removal deletes D, the successor of fraudulent A, and retains S.
// The lock remains on A until the rest of its fraudulent suffix is removed.
export const previousQueue = [
  { headerHash: null, outRef: `${hex("root-tx")}#0` },
  { headerHash: a, outRef: A_OUT },
  { headerHash: d, outRef: BASE_OUT },
  { headerHash: s, outRef: S_OUT },
];
export const nextQueue = [
  previousQueue[0]!,
  { headerHash: a, outRef: `${tx}#0` },
  previousQueue[3]!,
];
export const before = {
  root,
  nodes: [
    root,
    queueNode(a, root.headerHash, d, A_OUT),
    queueNode(d, a, s, BASE_OUT),
    queueNode(s, d, undefined, S_OUT),
  ],
};
export const after = {
  root,
  nodes: [
    root,
    queueNode(a, root.headerHash, s, `${tx}#0`),
    queueNode(s, a, undefined, S_OUT),
  ],
};

export const correction = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
  deploymentIdentityDigest: authority.manifestId,
  stateQueuePolicyId: authority.stateQueuePolicyId,
  transactionHash: tx,
  blockHash: hex("correction:block"),
  slot: "1900",
  blockNo: "8",
  transactionIndex: "0",
  chainPointId: hex("correction:point"),
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
            fraudulent_operator: bytes("operator", 28).toString("hex"),
            fraudulent_blocks_header_hash: a,
            slashing_approach: {
              OperatorAlreadySlashed: {
                active_operators_element_ref_input_index: 0n,
                retired_operators_element_ref_input_index: 1n,
              },
            },
            fraud_proof_ref_input_index: 0n,
            block_removal_approach: {
              RemoveFraudulentBlocksLink: {
                fraudulent_node_input_outref: {
                  transactionId: A_OUT.slice(0, 64),
                  outputIndex: 0n,
                },
                fraudulent_node_output_index: 0n,
              },
            },
          },
        },
        SDK.StateQueueRedeemer,
      ),
    },
  ],
  spentInputOutRefs: [A_OUT, BASE_OUT, lock],
  referenceInputOutRefs: [],
  correctionLockWitness: {
    kind: "correction_transition",
    consumedOutRef: lock,
    continuedOutRef: `${tx}#9`,
    targetHeaderHash: a,
    correctionIdentity: identity,
    previousDatum: "Idle",
    nextDatum: {
      Locked: { target_header_hash: a, correction_identity: identity },
    },
  },
  previousQueue,
  nextQueue,
});

/** Replays the actual fraud-link envelope through the production observer.
 * Transport depth is modelled; admission, parsing, authorization and SQL
 * persisted state are real. This does not claim emulator validator acceptance. */
export const admitCorrection = Effect.gen(function* () {
  if (correction === null)
    throw new Error("Fraud-link checkpoint did not authenticate");
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
          readQueue: async () => nextQueue,
          observeTransitions: async () => [correction],
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
            return typeof raw === "string" ? (JSON.parse(raw) as unknown) : raw;
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
          throw new Error("Unexpected rollback");
        },
        provenFinal: new Set(),
      }),
    catch: (cause) => cause,
  });
});

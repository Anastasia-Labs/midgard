import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  hashHexWithBlake2b,
  type HashingError,
  type MerkleRoot,
  OutputReference,
} from "../common.js";
import { DepositInfo } from "../ledger-state.js";
import { CompletedFraudWitnessSchema } from "../state-queue.js";
import { DepositDatum } from "../user-events/deposit.js";
import {
  type ChallengedHeaderHash,
  type CommittedDepositSourceProof,
  FabricatedDepositEvidenceVerdict,
  FabricatedDepositFault,
  FabricatedDepositStep01DatumSchema,
  FabricatedDepositStep02DatumSchema,
  FabricatedDepositStep02State,
  FabricatedDepositStep03DatumSchema,
  FabricatedDepositStep03State,
  FabricatedDepositStep04DatumSchema,
  FabricatedDepositStep04State,
} from "./fabricated-deposit.fabricated-deposit-fault-schema.js";
import { faultProofStepRedeemerSchema } from "./native.js";

export const FabricatedDepositStep04ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Index of the fraud-proof mint redeemer. */
  fraud_proof_mint_redeemer_index: Data.Integer(),
  completed_fraud_witness: CompletedFraudWitnessSchema,
});

export type FabricatedDepositStep04Args = Data.Static<
  typeof FabricatedDepositStep04ArgsSchema
>;

export const FabricatedDepositStep04Args =
  asDataType<FabricatedDepositStep04Args>(FabricatedDepositStep04ArgsSchema);

export const FabricatedDepositStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep04ArgsSchema);

export type FabricatedDepositStep04SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep04SpendRedeemerSchema
>;

export const FabricatedDepositStep04SpendRedeemer =
  asDataType<FabricatedDepositStep04SpendRedeemer>(
    FabricatedDepositStep04SpendRedeemerSchema,
  );

// ## Step resolver

export const FABRICATED_DEPOSIT_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
] as const;

export type FabricatedDepositStepName =
  (typeof FABRICATED_DEPOSIT_STEP_NAMES)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch: adding
 * a step without adding its schema fails to compile.
 */
export const fabricatedDepositStepDatumSchema = (
  step: FabricatedDepositStepName,
) => {
  switch (step) {
    case "step_01":
      return FabricatedDepositStep01DatumSchema;
    case "step_02":
      return FabricatedDepositStep02DatumSchema;
    case "step_03":
      return FabricatedDepositStep03DatumSchema;
    case "step_04":
      return FabricatedDepositStep04DatumSchema;
  }
};

// ## Commitments

/**
 * Blake2b-256 of a `DepositInfo`'s canonical bytes — the commitment the thread
 * carries in place of the `DepositInfo` itself, so no step's L1 footprint
 * depends on the depositor-chosen size of `l2_datum`.
 *
 * Twin of `step_01.committed_deposit_info_hash_v1` /
 * `utils.serialise_and_hash_32`.
 */
export const depositInfoCommitment = (
  info: DepositInfo,
): Effect.Effect<string, HashingError> =>
  depositInfoCommitmentCbor(Data.to(info, DepositInfo));

/** Hash authenticated raw info without collapsing opaque datum map pairs. */
export const depositInfoCommitmentCbor = (
  infoCbor: string,
): Effect.Effect<string, HashingError> => {
  Data.from(infoCbor, DepositInfo);
  return hashHexWithBlake2b(
    aikenSerialisedPlutusDataCborPreservingMapOrder(infoCbor),
    32,
  );
};

/**
 * Blake2b-256 of a deposit event datum's canonical bytes — step-02's retained
 * commitment, whose preimage step-03 re-opens.
 */
export const depositEventDatumCommitment = (
  datum: DepositDatum,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(Data.to(datum, DepositDatum), 32);

/**
 * The deposit event NFT asset name for a committed identity: Blake2b-256 of the
 * `DepositId`'s canonical bytes. Twin of `user_events.out_ref_to_nonce`, with the existing user event ID preserved.
 */
export const depositEventNonce = (
  depositId: OutputReference,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(Data.to(depositId, OutputReference), 32);

/** The canonical bytes of a committed deposit leaf's MPF key. */
export const committedDepositKeyBytes = (depositId: OutputReference): string =>
  Data.to(depositId, OutputReference);

/** The canonical bytes of a committed deposit leaf's MPF value. */
export const committedDepositValueBytes = (info: DepositInfo): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(Data.to(info, DepositInfo));

// ## Handoffs

/**
 * The step-01 → step-02 handoff, derived from the authenticated header facts and
 * the committed leaf the membership witness opens. Twin of the step-01
 * validator's `expected_output_state`.
 */
export const fabricatedDepositStep02State = ({
  stateQueuePolicy,
  challengedHeaderHash,
  headerStartTime,
  headerEndTime,
  committedDeposit,
}: {
  readonly stateQueuePolicy: string;
  readonly challengedHeaderHash: ChallengedHeaderHash;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly committedDeposit: CommittedDepositSourceProof;
}): Effect.Effect<FabricatedDepositStep02State, HashingError> =>
  Effect.map(
    depositInfoCommitment(committedDeposit.value),
    (committed_deposit_info_hash) => ({
      state_queue_policy: stateQueuePolicy,
      challenged_header_hash: challengedHeaderHash,
      header_start_time: headerStartTime,
      header_end_time: headerEndTime,
      committed_deposit_id: committedDeposit.key,
      committed_deposit_info_hash,
    }),
  );

/** The step-02 → step-03 handoff: the same facts plus the authenticated verdict. */
export const fabricatedDepositStep03State = (
  state: FabricatedDepositStep02State,
  verdict: FabricatedDepositEvidenceVerdict,
): FabricatedDepositStep03State => ({
  state_queue_policy: state.state_queue_policy,
  challenged_header_hash: state.challenged_header_hash,
  header_start_time: state.header_start_time,
  header_end_time: state.header_end_time,
  committed_deposit_id: state.committed_deposit_id,
  committed_deposit_info_hash: state.committed_deposit_info_hash,
  verdict,
});

/** The step-03 → step-04 handoff: the classified fault replaces the verdict. */
export const fabricatedDepositStep04State = (
  state: FabricatedDepositStep03State,
  fault: FabricatedDepositFault,
): FabricatedDepositStep04State => ({
  state_queue_policy: state.state_queue_policy,
  challenged_header_hash: state.challenged_header_hash,
  header_start_time: state.header_start_time,
  header_end_time: state.header_end_time,
  committed_deposit_id: state.committed_deposit_id,
  fault,
});

// ## The rule

/**
 * Twin of `step_04.fabricated_deposit_fault_is_established_v1`: a carried fault
 * is a fabricated-deposit fault when either the identity was absent, or the two
 * content commitments differ *and* the authentic event was due for the
 * challenged block (`start_time < inclusion_time <= end_time`).
 */
export const isFabricatedDepositFault = (
  state: FabricatedDepositStep04State,
): boolean => {
  const { fault } = state;
  if (fault === "NonexistentDepositIdentity") {
    return true;
  }
  if ("IneligibleDepositEvent" in fault) {
    const time = fault.IneligibleDepositEvent.event_inclusion_time;
    return !(state.header_start_time < time && time <= state.header_end_time);
  }
  const {
    committed_deposit_info_hash,
    authentic_deposit_info_hash,
    event_inclusion_time,
  } = fault.MismatchedDepositContent;
  return (
    committed_deposit_info_hash !== authentic_deposit_info_hash &&
    state.header_start_time < event_inclusion_time &&
    event_inclusion_time <= state.header_end_time
  );
};

/**
 * The counted `deposits_root` a header must carry for a raw deposits MPF root
 * and cardinality. Re-exported through the family so a builder never re-derives
 * the counted-root tag itself.
 */
export type FabricatedDepositCountedRootInput = {
  readonly phasRoot: MerkleRoot;
  readonly count: bigint;
};

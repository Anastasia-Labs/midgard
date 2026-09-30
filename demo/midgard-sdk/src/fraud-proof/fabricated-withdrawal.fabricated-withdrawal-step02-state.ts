import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  hashHexWithBlake2b,
  type HashingError,
  OutputReference,
  OutputReferenceSchema,
  POSIXTimeSchema,
} from "../common.js";
import {
  WithdrawalBody,
  WithdrawalBodySchema,
  WithdrawalInfo,
  type WithdrawalSignature,
  WithdrawalSignatureSchema,
} from "../ledger-state.js";
import { CompletedFraudWitnessSchema } from "../state-queue.js";
import { WithdrawalOrderDatum } from "../user-events/withdrawal.js";
import {
  type CommittedWithdrawalSourceProof,
  type FabricatedWithdrawalChallengedHeaderHash,
  FabricatedWithdrawalChallengedHeaderHashSchema,
  FabricatedWithdrawalEvidenceVerdict,
  FabricatedWithdrawalFault,
  FabricatedWithdrawalFaultSchema,
  FabricatedWithdrawalStep01DatumSchema,
  FabricatedWithdrawalStep02DatumSchema,
  FabricatedWithdrawalStep02State,
  FabricatedWithdrawalStep03DatumSchema,
  FabricatedWithdrawalStep03State,
} from "./fabricated-withdrawal.fabricated-withdrawal-fault-schema.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const FabricatedWithdrawalStep04StateSchema = Data.Object({
  state_queue_policy: Data.Bytes({ minLength: 28, maxLength: 28 }),
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: FabricatedWithdrawalChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed withdrawal identity — an L1 output reference. */
  committed_withdrawal_id: OutputReferenceSchema,
  /** The classified fault. */
  fault: FabricatedWithdrawalFaultSchema,
});

export type FabricatedWithdrawalStep04State = Data.Static<
  typeof FabricatedWithdrawalStep04StateSchema
>;

export const FabricatedWithdrawalStep04State =
  asDataType<FabricatedWithdrawalStep04State>(
    FabricatedWithdrawalStep04StateSchema,
  );

export const FabricatedWithdrawalStep04DatumSchema = faultProofStepDatumSchema(
  FabricatedWithdrawalStep04StateSchema,
);

export type FabricatedWithdrawalStep04Datum = Data.Static<
  typeof FabricatedWithdrawalStep04DatumSchema
>;

export const FabricatedWithdrawalStep04Datum =
  asDataType<FabricatedWithdrawalStep04Datum>(
    FabricatedWithdrawalStep04DatumSchema,
  );

export const FabricatedWithdrawalStep04ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Index of the fraud-proof mint redeemer. */
  fraud_proof_mint_redeemer_index: Data.Integer(),
  completed_fraud_witness: CompletedFraudWitnessSchema,
});

export type FabricatedWithdrawalStep04Args = Data.Static<
  typeof FabricatedWithdrawalStep04ArgsSchema
>;

export const FabricatedWithdrawalStep04Args =
  asDataType<FabricatedWithdrawalStep04Args>(
    FabricatedWithdrawalStep04ArgsSchema,
  );

export const FabricatedWithdrawalStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep04ArgsSchema);

export type FabricatedWithdrawalStep04SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep04SpendRedeemerSchema
>;

export const FabricatedWithdrawalStep04SpendRedeemer =
  asDataType<FabricatedWithdrawalStep04SpendRedeemer>(
    FabricatedWithdrawalStep04SpendRedeemerSchema,
  );

// ## Step resolver

export const FABRICATED_WITHDRAWAL_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
] as const;

export type FabricatedWithdrawalStepName =
  (typeof FABRICATED_WITHDRAWAL_STEP_NAMES)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch: adding a
 * step without adding its schema fails to compile.
 */
export const fabricatedWithdrawalStepDatumSchema = (
  step: FabricatedWithdrawalStepName,
) => {
  switch (step) {
    case "step_01":
      return FabricatedWithdrawalStep01DatumSchema;
    case "step_02":
      return FabricatedWithdrawalStep02DatumSchema;
    case "step_03":
      return FabricatedWithdrawalStep03DatumSchema;
    case "step_04":
      return FabricatedWithdrawalStep04DatumSchema;
  }
};

// ## Commitments
//
// Every one of these normalises Lucid's typed output to `serialiseData` form
// before it hashes or returns it — see this module's header note on indefinite
// versus definite Plutus maps.

/** The canonical bytes of a committed withdrawal leaf's MPF key. */
export const committedWithdrawalKeyBytes = (
  withdrawalId: OutputReference,
): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(withdrawalId, OutputReference),
  );

/** The canonical bytes of a committed withdrawal leaf's MPF value. */
export const committedWithdrawalValueBytes = (info: WithdrawalInfo): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(info, WithdrawalInfo),
  );

/** The canonical bytes of a withdrawal event's datum. */
export const withdrawalEventDatumBytes = (
  datum: WithdrawalOrderDatum,
): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(datum, WithdrawalOrderDatum),
  );

/**
 * The part of a committed withdrawal leaf the authentic L1 order fixes: its
 * `body` and its `signature`. Twin of `step_01.WithdrawalContentV1`; `validity`
 * is absent because the operator owns it (decision 0007).
 */
export const WithdrawalContentSchema = Data.Object({
  body: WithdrawalBodySchema,
  signature: WithdrawalSignatureSchema,
});

export type WithdrawalContent = Data.Static<typeof WithdrawalContentSchema>;

export const WithdrawalContent = asDataType<WithdrawalContent>(
  WithdrawalContentSchema,
);

/** The `(body, signature)` content of a `WithdrawalInfo`. */
export const withdrawalContentOf = (
  info: WithdrawalInfo,
): WithdrawalContent => ({
  body: info.body as WithdrawalBody,
  signature: info.signature as WithdrawalSignature,
});

/** The canonical bytes a withdrawal's `(body, signature)` content commits to. */
export const withdrawalContentBytes = (info: WithdrawalInfo): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(withdrawalContentOf(info), WithdrawalContent),
  );

/**
 * Blake2b-256 of a withdrawal's `(body, signature)` canonical bytes — the
 * commitment the thread carries in place of the content itself, so no step's L1
 * footprint depends on the withdrawer-chosen size of `l2_value` or `l1_datum`.
 *
 * Twin of `step_01.withdrawal_content_hash_v1`. One inequality between two of
 * these settles body and signature fidelity at once; the committed `validity`
 * verdict is excluded, because the operator owns it and a verdict the chain
 * contradicts is `withdrawalMistag`'s fault (decision 0007).
 */
export const withdrawalContentCommitment = (
  info: WithdrawalInfo,
): Effect.Effect<string, HashingError> =>
  withdrawalContentCommitmentCbor(Data.to(info, WithdrawalInfo));

/** The raw body/signature pair, excluding the operator-owned validity field. */
export const withdrawalContentBytesCbor = (infoCbor: string): string => {
  Data.from(infoCbor, WithdrawalInfo);
  const content = replacePlutusConstrFieldCbor(
    "d8799f0000ff",
    [0],
    plutusConstrFieldCbor(infoCbor, [0]),
  );
  return aikenSerialisedPlutusDataCborPreservingMapOrder(
    replacePlutusConstrFieldCbor(
      content,
      [1],
      plutusConstrFieldCbor(infoCbor, [1]),
    ),
  );
};

export const withdrawalContentCommitmentCbor = (
  infoCbor: string,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(withdrawalContentBytesCbor(infoCbor), 32);

/**
 * Blake2b-256 of a withdrawal event datum's canonical bytes — step-02's retained
 * commitment, whose preimage step-03 re-opens after the event NFT is burned.
 */
export const withdrawalEventDatumCommitment = (
  datum: WithdrawalOrderDatum,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(withdrawalEventDatumBytes(datum), 32);

/**
 * The withdrawal event NFT asset name for a committed identity: Blake2b-256 of
 * the `WithdrawalId`'s canonical bytes. Twin of `user_events.out_ref_to_nonce`,
 * with the existing user event ID preserved.
 */
export const withdrawalEventNonce = (
  withdrawalId: OutputReference,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(committedWithdrawalKeyBytes(withdrawalId), 32);

// ## Handoffs

/**
 * The step-01 → step-02 handoff, derived from the authenticated header facts and
 * the committed leaf the membership witness opens. Twin of the step-01 validator's
 * `expected_output_state`.
 */
export const fabricatedWithdrawalStep02State = ({
  stateQueuePolicy,
  challengedHeaderHash,
  headerStartTime,
  headerEndTime,
  committedWithdrawal,
}: {
  readonly stateQueuePolicy: string;
  readonly challengedHeaderHash: FabricatedWithdrawalChallengedHeaderHash;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly committedWithdrawal: CommittedWithdrawalSourceProof;
}): Effect.Effect<FabricatedWithdrawalStep02State, HashingError> =>
  Effect.map(
    withdrawalContentCommitment(committedWithdrawal.value),
    (committed_withdrawal_content_hash) => ({
      state_queue_policy: stateQueuePolicy,
      challenged_header_hash: challengedHeaderHash,
      header_start_time: headerStartTime,
      header_end_time: headerEndTime,
      committed_withdrawal_id: committedWithdrawal.key,
      committed_withdrawal_content_hash,
    }),
  );

/** The step-02 → step-03 handoff: the same facts plus the authenticated verdict. */
export const fabricatedWithdrawalStep03State = (
  state: FabricatedWithdrawalStep02State,
  verdict: FabricatedWithdrawalEvidenceVerdict,
): FabricatedWithdrawalStep03State => ({
  state_queue_policy: state.state_queue_policy,
  challenged_header_hash: state.challenged_header_hash,
  header_start_time: state.header_start_time,
  header_end_time: state.header_end_time,
  committed_withdrawal_id: state.committed_withdrawal_id,
  committed_withdrawal_content_hash: state.committed_withdrawal_content_hash,
  verdict,
});

/** The step-03 → step-04 handoff: the classified fault replaces the verdict. */
export const fabricatedWithdrawalStep04State = (
  state: FabricatedWithdrawalStep03State,
  fault: FabricatedWithdrawalFault,
): FabricatedWithdrawalStep04State => ({
  state_queue_policy: state.state_queue_policy,
  challenged_header_hash: state.challenged_header_hash,
  header_start_time: state.header_start_time,
  header_end_time: state.header_end_time,
  committed_withdrawal_id: state.committed_withdrawal_id,
  fault,
});

// ## The rule

/**
 * Twin of `step_04.fabricated_withdrawal_fault_is_established_v1`: a carried fault
 * is a fabricated-withdrawal fault when either the identity was absent, or the two
 * content commitments differ *and* the authentic event was due for the challenged
 * block (`start_time < inclusion_time <= end_time`).
 */
export const isFabricatedWithdrawalFault = (
  state: FabricatedWithdrawalStep04State,
): boolean => {
  const { fault } = state;
  if (fault === "NonexistentWithdrawalIdentity") {
    return true;
  }
  if ("IneligibleWithdrawalEvent" in fault) {
    const time = fault.IneligibleWithdrawalEvent.event_inclusion_time;
    return !(state.header_start_time < time && time <= state.header_end_time);
  }
  const {
    committed_withdrawal_content_hash,
    authentic_withdrawal_content_hash,
    event_inclusion_time,
  } = fault.MismatchedWithdrawalContent;
  return (
    committed_withdrawal_content_hash !== authentic_withdrawal_content_hash &&
    state.header_start_time < event_inclusion_time &&
    event_inclusion_time <= state.header_end_time
  );
};

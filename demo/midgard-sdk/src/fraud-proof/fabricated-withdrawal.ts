/**
 * `fabricated-withdrawal` family (Goal task `Q40`) — off-chain codec and rule
 * twin.
 *
 * Proves a block header commits a withdrawal leaf that is not the authentic L1
 * withdrawal event pair: either no withdrawal event with the committed
 * `WithdrawalId` was ever authenticated (`NonexistentWithdrawalIdentity`), or the
 * authentic event exists and was due for the block but its `WithdrawalInfo` — its
 * body, its signature, or its validity verdict — is not the committed one
 * (`MismatchedWithdrawalContent`).
 *
 * Violation: `fabricated-withdrawal`.
 * Production catalogue category: `fabricatedWithdrawal` (`0000000c`).
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2,3,4}.ak`
 * field for field and constructor index for constructor index, and the exact
 * bytes are pinned in `tests/fabricated-withdrawal.test.ts` against values
 * measured out of those Aiken modules.
 *
 * ### Why the commitments normalise their CBOR
 *
 * A `WithdrawalInfo` embeds `WithdrawalBody.l2_value`, a `Value` map — and
 * Lucid's typed encoder writes non-empty Plutus maps in **indefinite** form
 * (`bf … ff`) while Plutus' own `serialiseData`, which is what Aiken's
 * `cbor.serialise` calls on chain, writes them **definite** (`a1 …`). Committed
 * withdrawal leaves are checked on chain by re-serialising the typed leaf
 * (`transition_trace.verify_root_membership_with_bytes` over
 * `cbor.serialise(membership.key/value)`, both in this family's step-01 and in
 * `transition_trace/proof`'s `WithdrawalSourceMembership` arm), so the bytes that
 * bind are the `serialiseData` ones. Every helper here therefore passes its Lucid
 * output through `aikenSerialisedPlutusDataCbor`, exactly as
 * `midgard-node`'s withdrawal MPF insertion and `reserve-payout`'s withdrawal
 * override already do. Dropping that normalisation would silently produce
 * commitments and leaf bytes no on-chain step can reproduce; the twin test pins
 * both forms so the difference cannot regress unnoticed.
 */
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  H32Schema,
  hashHexWithBlake2b,
  type HashingError,
  type MerkleRoot,
  OutputReference,
  OutputReferenceSchema,
  POSIXTimeSchema,
} from "../common.js";
import { WithdrawalInfo } from "../ledger-state.js";
import {
  type RootMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";
import {
  WithdrawalOrderDatum,
  WithdrawalOrderDatumSchema,
} from "../user-events/withdrawal.js";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "./catalogue.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

/** Normative violation identifier (§9.1 output 1). */
export const FABRICATED_WITHDRAWAL_VIOLATION_ID =
  "fabricated-withdrawal" as const;

/**
 * Catalogue identifier of the `fabricatedWithdrawal` category.
 *
 * A category id is the 4-byte big-endian index of the category's position in the
 * append-only `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` in `./catalogue.js` — the
 * same derivation that gives `transitionTrace` (index 4) its `00000004`.
 * `fabricatedWithdrawal` is fixed at index 12, after `fabricatedDeposit`, and
 * is the byte twin of `step_01.fabricated_withdrawal_fraud_category_id` in Aiken.
 */
export const FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fabricatedWithdrawal;

/**
 * 28-byte hash of the challenged block header. Family-scoped: the deposit twin
 * exports an unscoped `ChallengedHeaderHashSchema`, and two `export *` sources for
 * one name make the `fraud-proof` barrel ambiguous (TS2308) the moment this family
 * is registered.
 */
export const FabricatedWithdrawalChallengedHeaderHashSchema = Data.Bytes({
  minLength: 28,
  maxLength: 28,
});
export type FabricatedWithdrawalChallengedHeaderHash = Data.Static<
  typeof FabricatedWithdrawalChallengedHeaderHashSchema
>;

/**
 * A fabricated-withdrawal computation-thread token's asset name: this family's
 * category id followed by the challenged header hash.
 */
export const fabricatedWithdrawalThreadTokenAssetName = (
  challengedHeaderHash: FabricatedWithdrawalChallengedHeaderHash,
): string =>
  `${FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID}${challengedHeaderHash}`;

// ## Step 01 — committed withdrawal-source membership

/**
 * Membership witness for one `(WithdrawalId, WithdrawalInfo)` leaf of
 * `withdrawals_root`.
 */
export const CommittedWithdrawalSourceProofSchema =
  WithdrawalSourceMembershipProofSchema;
export type CommittedWithdrawalSourceProof = RootMembershipProof<
  OutputReference,
  WithdrawalInfo
>;

export const FabricatedWithdrawalStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type FabricatedWithdrawalStep01Datum = Data.Static<
  typeof FabricatedWithdrawalStep01DatumSchema
>;
export const FabricatedWithdrawalStep01Datum =
  FabricatedWithdrawalStep01DatumSchema as unknown as FabricatedWithdrawalStep01Datum;

export const FabricatedWithdrawalStep01ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Reference-input index of the hub oracle. */
  hub_ref_input_index: Data.Integer(),
  /** Reference-input index of the challenged block's state-queue node. */
  state_queue_node_ref_input_index: Data.Integer(),
  /** The committed withdrawal leaf this thread challenges. */
  committed_withdrawal: CommittedWithdrawalSourceProofSchema,
});
export type FabricatedWithdrawalStep01Args = Data.Static<
  typeof FabricatedWithdrawalStep01ArgsSchema
>;
export const FabricatedWithdrawalStep01Args =
  FabricatedWithdrawalStep01ArgsSchema as unknown as FabricatedWithdrawalStep01Args;

export const FabricatedWithdrawalStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep01ArgsSchema);
export type FabricatedWithdrawalStep01SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep01SpendRedeemerSchema
>;
export const FabricatedWithdrawalStep01SpendRedeemer =
  FabricatedWithdrawalStep01SpendRedeemerSchema as unknown as FabricatedWithdrawalStep01SpendRedeemer;

// ## Step 02 — authenticated L1 withdrawal evidence

export const FabricatedWithdrawalStep02StateSchema = Data.Object({
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: FabricatedWithdrawalChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed withdrawal identity — an L1 output reference. */
  committed_withdrawal_id: OutputReferenceSchema,
  /** Blake2b-256 of the committed `WithdrawalInfo`'s canonical bytes. */
  committed_withdrawal_info_hash: H32Schema,
});
export type FabricatedWithdrawalStep02State = Data.Static<
  typeof FabricatedWithdrawalStep02StateSchema
>;
export const FabricatedWithdrawalStep02State =
  FabricatedWithdrawalStep02StateSchema as unknown as FabricatedWithdrawalStep02State;

export const FabricatedWithdrawalStep02DatumSchema = faultProofStepDatumSchema(
  FabricatedWithdrawalStep02StateSchema,
);
export type FabricatedWithdrawalStep02Datum = Data.Static<
  typeof FabricatedWithdrawalStep02DatumSchema
>;
export const FabricatedWithdrawalStep02Datum =
  FabricatedWithdrawalStep02DatumSchema as unknown as FabricatedWithdrawalStep02Datum;

/** The prover's chosen L1 witness about the committed withdrawal identity. */
export const FabricatedWithdrawalEvidenceSchema = Data.Enum([
  Data.Object({
    AbsentWithdrawalIdentity: Data.Object({
      unspent_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PresentWithdrawalEvent: Data.Object({
      hub_ref_input_index: Data.Integer(),
      event_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type FabricatedWithdrawalEvidence = Data.Static<
  typeof FabricatedWithdrawalEvidenceSchema
>;
export const FabricatedWithdrawalEvidence =
  FabricatedWithdrawalEvidenceSchema as unknown as FabricatedWithdrawalEvidence;

/** What L1 says about the committed identity, once authenticated. */
export const FabricatedWithdrawalEvidenceVerdictSchema = Data.Enum([
  Data.Literal("WithdrawalIdentityAbsent"),
  Data.Object({
    WithdrawalEventObserved: Data.Object({
      event_datum_hash: H32Schema,
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
]);
export type FabricatedWithdrawalEvidenceVerdict = Data.Static<
  typeof FabricatedWithdrawalEvidenceVerdictSchema
>;
export const FabricatedWithdrawalEvidenceVerdict =
  FabricatedWithdrawalEvidenceVerdictSchema as unknown as FabricatedWithdrawalEvidenceVerdict;

export const FabricatedWithdrawalStep02ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** The prover's chosen L1 witness. */
  evidence: FabricatedWithdrawalEvidenceSchema,
});
export type FabricatedWithdrawalStep02Args = Data.Static<
  typeof FabricatedWithdrawalStep02ArgsSchema
>;
export const FabricatedWithdrawalStep02Args =
  FabricatedWithdrawalStep02ArgsSchema as unknown as FabricatedWithdrawalStep02Args;

export const FabricatedWithdrawalStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep02ArgsSchema);
export type FabricatedWithdrawalStep02SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep02SpendRedeemerSchema
>;
export const FabricatedWithdrawalStep02SpendRedeemer =
  FabricatedWithdrawalStep02SpendRedeemerSchema as unknown as FabricatedWithdrawalStep02SpendRedeemer;

// ## Step 03 — fault classification

export const FabricatedWithdrawalStep03StateSchema = Data.Object({
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: FabricatedWithdrawalChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed withdrawal identity — an L1 output reference. */
  committed_withdrawal_id: OutputReferenceSchema,
  /** Blake2b-256 of the committed `WithdrawalInfo`'s canonical bytes. */
  committed_withdrawal_info_hash: H32Schema,
  /** The authenticated verdict about L1. */
  verdict: FabricatedWithdrawalEvidenceVerdictSchema,
});
export type FabricatedWithdrawalStep03State = Data.Static<
  typeof FabricatedWithdrawalStep03StateSchema
>;
export const FabricatedWithdrawalStep03State =
  FabricatedWithdrawalStep03StateSchema as unknown as FabricatedWithdrawalStep03State;

export const FabricatedWithdrawalStep03DatumSchema = faultProofStepDatumSchema(
  FabricatedWithdrawalStep03StateSchema,
);
export type FabricatedWithdrawalStep03Datum = Data.Static<
  typeof FabricatedWithdrawalStep03DatumSchema
>;
export const FabricatedWithdrawalStep03Datum =
  FabricatedWithdrawalStep03DatumSchema as unknown as FabricatedWithdrawalStep03Datum;

/**
 * The prover's opening of step-02's retained event-datum commitment.
 *
 * On chain the `RetainedEventDatum` field is an opaque `Data`, because the step
 * hashes it before it decodes it. Off chain it is typed as the withdrawal event
 * datum it must decode to: the wire bytes are identical, and the tighter type
 * means a builder cannot assemble an opening the L1 step would reject at decode
 * time.
 */
export const FabricatedWithdrawalAuthenticContentOpeningSchema = Data.Enum([
  Data.Literal("NoAuthenticContent"),
  Data.Object({
    RetainedEventDatum: Data.Object({
      event_datum: WithdrawalOrderDatumSchema,
    }),
  }),
]);
export type FabricatedWithdrawalAuthenticContentOpening = Data.Static<
  typeof FabricatedWithdrawalAuthenticContentOpeningSchema
>;
export const FabricatedWithdrawalAuthenticContentOpening =
  FabricatedWithdrawalAuthenticContentOpeningSchema as unknown as FabricatedWithdrawalAuthenticContentOpening;

export const FabricatedWithdrawalStep03ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** The prover's opening of step-02's retained commitment. */
  authentic_content: FabricatedWithdrawalAuthenticContentOpeningSchema,
});
export type FabricatedWithdrawalStep03Args = Data.Static<
  typeof FabricatedWithdrawalStep03ArgsSchema
>;
export const FabricatedWithdrawalStep03Args =
  FabricatedWithdrawalStep03ArgsSchema as unknown as FabricatedWithdrawalStep03Args;

export const FabricatedWithdrawalStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep03ArgsSchema);
export type FabricatedWithdrawalStep03SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep03SpendRedeemerSchema
>;
export const FabricatedWithdrawalStep03SpendRedeemer =
  FabricatedWithdrawalStep03SpendRedeemerSchema as unknown as FabricatedWithdrawalStep03SpendRedeemer;

// ## Step 04 — the established fault

/** The `FabricatedWithdrawal` violation, in its two shapes. */
export const FabricatedWithdrawalFaultSchema = Data.Enum([
  Data.Literal("NonexistentWithdrawalIdentity"),
  Data.Object({
    MismatchedWithdrawalContent: Data.Object({
      committed_withdrawal_info_hash: H32Schema,
      authentic_withdrawal_info_hash: H32Schema,
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
]);
export type FabricatedWithdrawalFault = Data.Static<
  typeof FabricatedWithdrawalFaultSchema
>;
export const FabricatedWithdrawalFault =
  FabricatedWithdrawalFaultSchema as unknown as FabricatedWithdrawalFault;

export const FabricatedWithdrawalStep04StateSchema = Data.Object({
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
  FabricatedWithdrawalStep04StateSchema as unknown as FabricatedWithdrawalStep04State;

export const FabricatedWithdrawalStep04DatumSchema = faultProofStepDatumSchema(
  FabricatedWithdrawalStep04StateSchema,
);
export type FabricatedWithdrawalStep04Datum = Data.Static<
  typeof FabricatedWithdrawalStep04DatumSchema
>;
export const FabricatedWithdrawalStep04Datum =
  FabricatedWithdrawalStep04DatumSchema as unknown as FabricatedWithdrawalStep04Datum;

export const FabricatedWithdrawalStep04ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Index of the fraud-proof mint redeemer. */
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type FabricatedWithdrawalStep04Args = Data.Static<
  typeof FabricatedWithdrawalStep04ArgsSchema
>;
export const FabricatedWithdrawalStep04Args =
  FabricatedWithdrawalStep04ArgsSchema as unknown as FabricatedWithdrawalStep04Args;

export const FabricatedWithdrawalStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep04ArgsSchema);
export type FabricatedWithdrawalStep04SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep04SpendRedeemerSchema
>;
export const FabricatedWithdrawalStep04SpendRedeemer =
  FabricatedWithdrawalStep04SpendRedeemerSchema as unknown as FabricatedWithdrawalStep04SpendRedeemer;

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
  aikenSerialisedPlutusDataCbor(Data.to(withdrawalId, OutputReference));

/** The canonical bytes of a committed withdrawal leaf's MPF value. */
export const committedWithdrawalValueBytes = (info: WithdrawalInfo): string =>
  aikenSerialisedPlutusDataCbor(Data.to(info, WithdrawalInfo));

/** The canonical bytes of a withdrawal event's datum. */
export const withdrawalEventDatumBytes = (
  datum: WithdrawalOrderDatum,
): string =>
  aikenSerialisedPlutusDataCbor(Data.to(datum, WithdrawalOrderDatum));

/**
 * Blake2b-256 of a `WithdrawalInfo`'s canonical bytes — the commitment the thread
 * carries in place of the `WithdrawalInfo` itself, so no step's L1 footprint
 * depends on the withdrawer-chosen size of `l2_value` or `l1_datum`.
 *
 * Twin of `step_01.committed_withdrawal_info_hash_v1` /
 * `utils.serialise_and_hash_32`. One inequality between two of these settles body,
 * signature and validity fidelity at once.
 */
export const withdrawalInfoCommitment = (
  info: WithdrawalInfo,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(committedWithdrawalValueBytes(info), 32);

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
 * and the reason a still-unspent output at that reference proves no such event was
 * ever authenticated.
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
  challengedHeaderHash,
  headerStartTime,
  headerEndTime,
  committedWithdrawal,
}: {
  readonly challengedHeaderHash: FabricatedWithdrawalChallengedHeaderHash;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly committedWithdrawal: CommittedWithdrawalSourceProof;
}): Effect.Effect<FabricatedWithdrawalStep02State, HashingError> =>
  Effect.map(
    withdrawalInfoCommitment(committedWithdrawal.value),
    (committed_withdrawal_info_hash) => ({
      challenged_header_hash: challengedHeaderHash,
      header_start_time: headerStartTime,
      header_end_time: headerEndTime,
      committed_withdrawal_id: committedWithdrawal.key,
      committed_withdrawal_info_hash,
    }),
  );

/** The step-02 → step-03 handoff: the same facts plus the authenticated verdict. */
export const fabricatedWithdrawalStep03State = (
  state: FabricatedWithdrawalStep02State,
  verdict: FabricatedWithdrawalEvidenceVerdict,
): FabricatedWithdrawalStep03State => ({
  challenged_header_hash: state.challenged_header_hash,
  header_start_time: state.header_start_time,
  header_end_time: state.header_end_time,
  committed_withdrawal_id: state.committed_withdrawal_id,
  committed_withdrawal_info_hash: state.committed_withdrawal_info_hash,
  verdict,
});

/** The step-03 → step-04 handoff: the classified fault replaces the verdict. */
export const fabricatedWithdrawalStep04State = (
  state: FabricatedWithdrawalStep03State,
  fault: FabricatedWithdrawalFault,
): FabricatedWithdrawalStep04State => ({
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
  const {
    committed_withdrawal_info_hash,
    authentic_withdrawal_info_hash,
    event_inclusion_time,
  } = fault.MismatchedWithdrawalContent;
  return (
    committed_withdrawal_info_hash !== authentic_withdrawal_info_hash &&
    state.header_start_time < event_inclusion_time &&
    event_inclusion_time <= state.header_end_time
  );
};

/**
 * The counted `withdrawals_root` a header must carry for a raw withdrawals MPF
 * root and cardinality. Re-exported through the family so a builder never
 * re-derives the counted-root tag itself.
 */
export type FabricatedWithdrawalCountedRootInput = {
  readonly phasRoot: MerkleRoot;
  readonly count: bigint;
};

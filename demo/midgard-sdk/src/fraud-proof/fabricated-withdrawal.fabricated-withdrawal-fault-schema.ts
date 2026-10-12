import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  H32Schema,
  OutputReference,
  OutputReferenceSchema,
  POSIXTimeSchema,
  ValueSchema,
} from "../common.js";
import { WithdrawalInfo } from "../ledger-state.js";
import {
  type RootMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";
import {
  EventHistoryCommitmentSchema,
  EventHistoryPayloadSchema,
} from "../user-events/history.js";
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
 * A category id is the explicit 4-byte big-endian value pinned in
 * `FRAUD_PROOF_CATALOGUE_CATEGORY_IDS` in `./catalogue.js`.
 * `fabricatedWithdrawal` is `0000000c` and is the byte twin of
 * `step_01.fabricated_withdrawal_fraud_category_id` in Aiken.
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
  asDataType<FabricatedWithdrawalStep01Datum>(
    FabricatedWithdrawalStep01DatumSchema,
  );

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
  asDataType<FabricatedWithdrawalStep01Args>(
    FabricatedWithdrawalStep01ArgsSchema,
  );

export const FabricatedWithdrawalStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep01ArgsSchema);

export type FabricatedWithdrawalStep01SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep01SpendRedeemerSchema
>;

export const FabricatedWithdrawalStep01SpendRedeemer =
  asDataType<FabricatedWithdrawalStep01SpendRedeemer>(
    FabricatedWithdrawalStep01SpendRedeemerSchema,
  );

// ## Step 02 — authenticated L1 withdrawal evidence

export const FabricatedWithdrawalStep02StateSchema = Data.Object({
  state_queue_policy: Data.Bytes({ minLength: 28, maxLength: 28 }),
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: FabricatedWithdrawalChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed withdrawal identity — an L1 output reference. */
  committed_withdrawal_id: OutputReferenceSchema,
  /** Blake2b-256 of the committed withdrawal's `(body, signature)` bytes. */
  committed_withdrawal_content_hash: H32Schema,
});

export type FabricatedWithdrawalStep02State = Data.Static<
  typeof FabricatedWithdrawalStep02StateSchema
>;

export const FabricatedWithdrawalStep02State =
  asDataType<FabricatedWithdrawalStep02State>(
    FabricatedWithdrawalStep02StateSchema,
  );

export const FabricatedWithdrawalStep02DatumSchema = faultProofStepDatumSchema(
  FabricatedWithdrawalStep02StateSchema,
);

export type FabricatedWithdrawalStep02Datum = Data.Static<
  typeof FabricatedWithdrawalStep02DatumSchema
>;

export const FabricatedWithdrawalStep02Datum =
  asDataType<FabricatedWithdrawalStep02Datum>(
    FabricatedWithdrawalStep02DatumSchema,
  );

/** The prover's chosen L1 witness about the committed withdrawal identity. */
export const FabricatedWithdrawalEvidenceSchema = Data.Enum([
  Data.Object({
    AbsentWithdrawalIdentity: Data.Object({
      hub_ref_input_index: Data.Integer(),
      history_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PresentWithdrawalEvent: Data.Object({
      hub_ref_input_index: Data.Integer(),
      event_ref_input_index: Data.Integer(),
      external_ref_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
]);

export type FabricatedWithdrawalEvidence = Data.Static<
  typeof FabricatedWithdrawalEvidenceSchema
>;

export const FabricatedWithdrawalEvidence =
  asDataType<FabricatedWithdrawalEvidence>(FabricatedWithdrawalEvidenceSchema);

/** What L1 says about the committed identity, once authenticated. */
export const FabricatedWithdrawalEvidenceVerdictSchema = Data.Enum([
  Data.Literal("WithdrawalIdentityAbsent"),
  Data.Object({
    WithdrawalEventObserved: Data.Object({
      commitment: EventHistoryCommitmentSchema,
    }),
  }),
]);

export type FabricatedWithdrawalEvidenceVerdict = Data.Static<
  typeof FabricatedWithdrawalEvidenceVerdictSchema
>;

export const FabricatedWithdrawalEvidenceVerdict =
  asDataType<FabricatedWithdrawalEvidenceVerdict>(
    FabricatedWithdrawalEvidenceVerdictSchema,
  );

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
  asDataType<FabricatedWithdrawalStep02Args>(
    FabricatedWithdrawalStep02ArgsSchema,
  );

export const FabricatedWithdrawalStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep02ArgsSchema);

export type FabricatedWithdrawalStep02SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep02SpendRedeemerSchema
>;

export const FabricatedWithdrawalStep02SpendRedeemer =
  asDataType<FabricatedWithdrawalStep02SpendRedeemer>(
    FabricatedWithdrawalStep02SpendRedeemerSchema,
  );

// ## Step 03 — fault classification

export const FabricatedWithdrawalStep03StateSchema = Data.Object({
  state_queue_policy: Data.Bytes({ minLength: 28, maxLength: 28 }),
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: FabricatedWithdrawalChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed withdrawal identity — an L1 output reference. */
  committed_withdrawal_id: OutputReferenceSchema,
  /** Blake2b-256 of the committed withdrawal's `(body, signature)` bytes. */
  committed_withdrawal_content_hash: H32Schema,
  /** The authenticated verdict about L1. */
  verdict: FabricatedWithdrawalEvidenceVerdictSchema,
});

export type FabricatedWithdrawalStep03State = Data.Static<
  typeof FabricatedWithdrawalStep03StateSchema
>;

export const FabricatedWithdrawalStep03State =
  asDataType<FabricatedWithdrawalStep03State>(
    FabricatedWithdrawalStep03StateSchema,
  );

export const FabricatedWithdrawalStep03DatumSchema = faultProofStepDatumSchema(
  FabricatedWithdrawalStep03StateSchema,
);

export type FabricatedWithdrawalStep03Datum = Data.Static<
  typeof FabricatedWithdrawalStep03DatumSchema
>;

export const FabricatedWithdrawalStep03Datum =
  asDataType<FabricatedWithdrawalStep03Datum>(
    FabricatedWithdrawalStep03DatumSchema,
  );

/** Reopen the authenticated payload and original L1 Value, independent of pointers. */
export const FabricatedWithdrawalAuthenticContentOpeningSchema = Data.Enum([
  Data.Literal("NoAuthenticContent"),
  Data.Object({
    RetainedEventData: Data.Object({
      payload: EventHistoryPayloadSchema,
      original_assets: ValueSchema,
    }),
  }),
]);

export type FabricatedWithdrawalAuthenticContentOpening = Data.Static<
  typeof FabricatedWithdrawalAuthenticContentOpeningSchema
>;

export const FabricatedWithdrawalAuthenticContentOpening =
  asDataType<FabricatedWithdrawalAuthenticContentOpening>(
    FabricatedWithdrawalAuthenticContentOpeningSchema,
  );

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
  asDataType<FabricatedWithdrawalStep03Args>(
    FabricatedWithdrawalStep03ArgsSchema,
  );

export const FabricatedWithdrawalStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedWithdrawalStep03ArgsSchema);

export type FabricatedWithdrawalStep03SpendRedeemer = Data.Static<
  typeof FabricatedWithdrawalStep03SpendRedeemerSchema
>;

export const FabricatedWithdrawalStep03SpendRedeemer =
  asDataType<FabricatedWithdrawalStep03SpendRedeemer>(
    FabricatedWithdrawalStep03SpendRedeemerSchema,
  );

// ## Step 04 — the established fault

/** Authenticated absence, content mismatch, or ineligible timing. */
export const FabricatedWithdrawalFaultSchema = Data.Enum([
  Data.Literal("NonexistentWithdrawalIdentity"),
  Data.Object({
    MismatchedWithdrawalContent: Data.Object({
      committed_withdrawal_content_hash: H32Schema,
      authentic_withdrawal_content_hash: H32Schema,
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
  Data.Object({
    IneligibleWithdrawalEvent: Data.Object({
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
]);

export type FabricatedWithdrawalFault = Data.Static<
  typeof FabricatedWithdrawalFaultSchema
>;

export const FabricatedWithdrawalFault = asDataType<FabricatedWithdrawalFault>(
  FabricatedWithdrawalFaultSchema,
);

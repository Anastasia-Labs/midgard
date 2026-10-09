import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  H32Schema,
  OutputReference,
  OutputReferenceSchema,
  POSIXTimeSchema,
  ValueSchema,
} from "../common.js";
import { DepositInfo } from "../ledger-state.js";
import {
  DepositSourceMembershipProofSchema,
  type RootMembershipProof,
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
export const FABRICATED_DEPOSIT_VIOLATION_ID = "fabricated-deposit" as const;

/**
 * Catalogue identifier of the `fabricatedDeposit` category.
 *
 * A category id is the explicit 4-byte big-endian value pinned in
 * `FRAUD_PROOF_CATALOGUE_CATEGORY_IDS` in `./catalogue.js`.
 * `fabricatedDeposit` is `0000000b` and is the byte twin of
 * `step_01.fabricated_deposit_fraud_category_id` in Aiken.
 */
export const FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fabricatedDeposit;

/** 28-byte hash of the challenged block header. */
export const ChallengedHeaderHashSchema = Data.Bytes({
  minLength: 28,
  maxLength: 28,
});

export type ChallengedHeaderHash = Data.Static<
  typeof ChallengedHeaderHashSchema
>;

/**
 * A fabricated-deposit computation-thread token's asset name: this family's
 * category id followed by the challenged header hash.
 */
export const fabricatedDepositThreadTokenAssetName = (
  challengedHeaderHash: ChallengedHeaderHash,
): string => `${FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID}${challengedHeaderHash}`;

// ## Step 01 — committed deposit-source membership

/** Membership witness for one `(DepositId, DepositInfo)` leaf of `deposits_root`. */
export const CommittedDepositSourceProofSchema =
  DepositSourceMembershipProofSchema;

export type CommittedDepositSourceProof = RootMembershipProof<
  OutputReference,
  DepositInfo
>;

export const FabricatedDepositStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);

export type FabricatedDepositStep01Datum = Data.Static<
  typeof FabricatedDepositStep01DatumSchema
>;

export const FabricatedDepositStep01Datum =
  asDataType<FabricatedDepositStep01Datum>(FabricatedDepositStep01DatumSchema);

export const FabricatedDepositStep01ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Reference-input index of the hub oracle. */
  hub_ref_input_index: Data.Integer(),
  /** Reference-input index of the challenged block's state-queue node. */
  state_queue_node_ref_input_index: Data.Integer(),
  /** The committed deposit leaf this thread challenges. */
  committed_deposit: CommittedDepositSourceProofSchema,
});

export type FabricatedDepositStep01Args = Data.Static<
  typeof FabricatedDepositStep01ArgsSchema
>;

export const FabricatedDepositStep01Args =
  asDataType<FabricatedDepositStep01Args>(FabricatedDepositStep01ArgsSchema);

export const FabricatedDepositStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep01ArgsSchema);

export type FabricatedDepositStep01SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep01SpendRedeemerSchema
>;

export const FabricatedDepositStep01SpendRedeemer =
  asDataType<FabricatedDepositStep01SpendRedeemer>(
    FabricatedDepositStep01SpendRedeemerSchema,
  );

// ## Step 02 — authenticated L1 deposit evidence

export const FabricatedDepositStep02StateSchema = Data.Object({
  state_queue_policy: Data.Bytes({ minLength: 28, maxLength: 28 }),
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: ChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed deposit identity — an L1 output reference. */
  committed_deposit_id: OutputReferenceSchema,
  /** Blake2b-256 of the committed `DepositInfo`'s canonical bytes. */
  committed_deposit_info_hash: H32Schema,
});

export type FabricatedDepositStep02State = Data.Static<
  typeof FabricatedDepositStep02StateSchema
>;

export const FabricatedDepositStep02State =
  asDataType<FabricatedDepositStep02State>(FabricatedDepositStep02StateSchema);

export const FabricatedDepositStep02DatumSchema = faultProofStepDatumSchema(
  FabricatedDepositStep02StateSchema,
);

export type FabricatedDepositStep02Datum = Data.Static<
  typeof FabricatedDepositStep02DatumSchema
>;

export const FabricatedDepositStep02Datum =
  asDataType<FabricatedDepositStep02Datum>(FabricatedDepositStep02DatumSchema);

/** The prover's chosen L1 witness about the committed deposit identity. */
export const FabricatedDepositEvidenceSchema = Data.Enum([
  Data.Object({
    AbsentDepositIdentity: Data.Object({
      hub_ref_input_index: Data.Integer(),
      history_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PresentDepositEvent: Data.Object({
      hub_ref_input_index: Data.Integer(),
      event_ref_input_index: Data.Integer(),
      external_ref_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
]);

export type FabricatedDepositEvidence = Data.Static<
  typeof FabricatedDepositEvidenceSchema
>;

export const FabricatedDepositEvidence = asDataType<FabricatedDepositEvidence>(
  FabricatedDepositEvidenceSchema,
);

/** What L1 says about the committed identity, once authenticated. */
export const FabricatedDepositEvidenceVerdictSchema = Data.Enum([
  Data.Literal("DepositIdentityAbsent"),
  Data.Object({
    DepositEventObserved: Data.Object({
      commitment: EventHistoryCommitmentSchema,
    }),
  }),
]);

export type FabricatedDepositEvidenceVerdict = Data.Static<
  typeof FabricatedDepositEvidenceVerdictSchema
>;

export const FabricatedDepositEvidenceVerdict =
  asDataType<FabricatedDepositEvidenceVerdict>(
    FabricatedDepositEvidenceVerdictSchema,
  );

export const FabricatedDepositStep02ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** The prover's chosen L1 witness. */
  evidence: FabricatedDepositEvidenceSchema,
});

export type FabricatedDepositStep02Args = Data.Static<
  typeof FabricatedDepositStep02ArgsSchema
>;

export const FabricatedDepositStep02Args =
  asDataType<FabricatedDepositStep02Args>(FabricatedDepositStep02ArgsSchema);

export const FabricatedDepositStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep02ArgsSchema);

export type FabricatedDepositStep02SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep02SpendRedeemerSchema
>;

export const FabricatedDepositStep02SpendRedeemer =
  asDataType<FabricatedDepositStep02SpendRedeemer>(
    FabricatedDepositStep02SpendRedeemerSchema,
  );

// ## Step 03 — fault classification

export const FabricatedDepositStep03StateSchema = Data.Object({
  state_queue_policy: Data.Bytes({ minLength: 28, maxLength: 28 }),
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: ChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed deposit identity — an L1 output reference. */
  committed_deposit_id: OutputReferenceSchema,
  /** Blake2b-256 of the committed `DepositInfo`'s canonical bytes. */
  committed_deposit_info_hash: H32Schema,
  /** The authenticated verdict about L1. */
  verdict: FabricatedDepositEvidenceVerdictSchema,
});

export type FabricatedDepositStep03State = Data.Static<
  typeof FabricatedDepositStep03StateSchema
>;

export const FabricatedDepositStep03State =
  asDataType<FabricatedDepositStep03State>(FabricatedDepositStep03StateSchema);

export const FabricatedDepositStep03DatumSchema = faultProofStepDatumSchema(
  FabricatedDepositStep03StateSchema,
);

export type FabricatedDepositStep03Datum = Data.Static<
  typeof FabricatedDepositStep03DatumSchema
>;

export const FabricatedDepositStep03Datum =
  asDataType<FabricatedDepositStep03Datum>(FabricatedDepositStep03DatumSchema);

/** Reopen the authenticated payload and original L1 Value, independent of pointers. */
export const FabricatedDepositAuthenticContentOpeningSchema = Data.Enum([
  Data.Literal("NoAuthenticContent"),
  Data.Object({
    RetainedEventData: Data.Object({
      payload: EventHistoryPayloadSchema,
      original_assets: ValueSchema,
    }),
  }),
]);

export type FabricatedDepositAuthenticContentOpening = Data.Static<
  typeof FabricatedDepositAuthenticContentOpeningSchema
>;

export const FabricatedDepositAuthenticContentOpening =
  asDataType<FabricatedDepositAuthenticContentOpening>(
    FabricatedDepositAuthenticContentOpeningSchema,
  );

export const FabricatedDepositStep03ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** The prover's opening of step-02's retained commitment. */
  authentic_content: FabricatedDepositAuthenticContentOpeningSchema,
});

export type FabricatedDepositStep03Args = Data.Static<
  typeof FabricatedDepositStep03ArgsSchema
>;

export const FabricatedDepositStep03Args =
  asDataType<FabricatedDepositStep03Args>(FabricatedDepositStep03ArgsSchema);

export const FabricatedDepositStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep03ArgsSchema);

export type FabricatedDepositStep03SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep03SpendRedeemerSchema
>;

export const FabricatedDepositStep03SpendRedeemer =
  asDataType<FabricatedDepositStep03SpendRedeemer>(
    FabricatedDepositStep03SpendRedeemerSchema,
  );

// ## Step 04 — the established fault

/** Authenticated absence, content mismatch, or ineligible timing. */
export const FabricatedDepositFaultSchema = Data.Enum([
  Data.Literal("NonexistentDepositIdentity"),
  Data.Object({
    MismatchedDepositContent: Data.Object({
      committed_deposit_info_hash: H32Schema,
      authentic_deposit_info_hash: H32Schema,
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
  Data.Object({
    IneligibleDepositEvent: Data.Object({
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
]);

export type FabricatedDepositFault = Data.Static<
  typeof FabricatedDepositFaultSchema
>;

export const FabricatedDepositFault = asDataType<FabricatedDepositFault>(
  FabricatedDepositFaultSchema,
);

export const FabricatedDepositStep04StateSchema = Data.Object({
  state_queue_policy: Data.Bytes({ minLength: 28, maxLength: 28 }),
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: ChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed deposit identity — an L1 output reference. */
  committed_deposit_id: OutputReferenceSchema,
  /** The classified fault. */
  fault: FabricatedDepositFaultSchema,
});

export type FabricatedDepositStep04State = Data.Static<
  typeof FabricatedDepositStep04StateSchema
>;

export const FabricatedDepositStep04State =
  asDataType<FabricatedDepositStep04State>(FabricatedDepositStep04StateSchema);

export const FabricatedDepositStep04DatumSchema = faultProofStepDatumSchema(
  FabricatedDepositStep04StateSchema,
);

export type FabricatedDepositStep04Datum = Data.Static<
  typeof FabricatedDepositStep04DatumSchema
>;

export const FabricatedDepositStep04Datum =
  asDataType<FabricatedDepositStep04Datum>(FabricatedDepositStep04DatumSchema);

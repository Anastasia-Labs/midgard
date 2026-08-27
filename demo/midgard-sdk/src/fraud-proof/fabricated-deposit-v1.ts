/**
 * `fabricated-deposit` family (Goal task `Q39`) — off-chain codec and rule twin.
 *
 * Proves a block header commits a deposit leaf that is not the authentic L1
 * deposit event pair: either no deposit event with the committed `DepositId` was
 * ever authenticated (`NonexistentDepositIdentity`), or the authentic event
 * exists and was due for the block but its `DepositInfo` is not the committed
 * one (`MismatchedDepositContent`).
 *
 * Violation: `fabricated-deposit`.
 * Production catalogue category: `fabricatedDeposit` (`0000000b`).
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-0{1,2,3,4}.ak`
 * field for field and constructor index for constructor index, and the exact
 * bytes are pinned in `tests/fabricated-deposit-v1.test.ts` against values
 * measured out of those Aiken modules.
 */
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
} from "@/common.js";
import { DepositInfo } from "@/ledger-state.js";
import {
  DepositSourceMembershipProofSchema,
  type RootMembershipProof,
} from "@/transition-trace.js";
import { DepositDatum, DepositDatumSchema } from "@/user-events/deposit.js";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "./catalogue.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

/** Normative violation identifier (§9.1 output 1). */
export const FABRICATED_DEPOSIT_VIOLATION_ID_V1 = "fabricated-deposit" as const;

/**
 * Catalogue identifier of the `fabricatedDeposit` category.
 *
 * A category id is the 4-byte big-endian index of the category's position in the
 * append-only `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` in `./catalogue.js` — the
 * same derivation that gives `transitionTrace` (index 4) its `00000004`.
 * `fabricatedDeposit` is fixed at index 11 and is the byte twin of
 * `step_01.fabricated_deposit_fraud_category_id` in Aiken.
 */
export const FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1 =
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
export const fabricatedDepositThreadTokenAssetNameV1 = (
  challengedHeaderHash: ChallengedHeaderHash,
): string =>
  `${FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1}${challengedHeaderHash}`;

// ## Step 01 — committed deposit-source membership

/** Membership witness for one `(DepositId, DepositInfo)` leaf of `deposits_root`. */
export const CommittedDepositSourceProofV1Schema =
  DepositSourceMembershipProofSchema;
export type CommittedDepositSourceProofV1 = RootMembershipProof<
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
  FabricatedDepositStep01DatumSchema as unknown as FabricatedDepositStep01Datum;

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
  committed_deposit: CommittedDepositSourceProofV1Schema,
});
export type FabricatedDepositStep01Args = Data.Static<
  typeof FabricatedDepositStep01ArgsSchema
>;
export const FabricatedDepositStep01Args =
  FabricatedDepositStep01ArgsSchema as unknown as FabricatedDepositStep01Args;

export const FabricatedDepositStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep01ArgsSchema);
export type FabricatedDepositStep01SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep01SpendRedeemerSchema
>;
export const FabricatedDepositStep01SpendRedeemer =
  FabricatedDepositStep01SpendRedeemerSchema as unknown as FabricatedDepositStep01SpendRedeemer;

// ## Step 02 — authenticated L1 deposit evidence

export const FabricatedDepositStep02StateSchema = Data.Object({
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
  FabricatedDepositStep02StateSchema as unknown as FabricatedDepositStep02State;

export const FabricatedDepositStep02DatumSchema = faultProofStepDatumSchema(
  FabricatedDepositStep02StateSchema,
);
export type FabricatedDepositStep02Datum = Data.Static<
  typeof FabricatedDepositStep02DatumSchema
>;
export const FabricatedDepositStep02Datum =
  FabricatedDepositStep02DatumSchema as unknown as FabricatedDepositStep02Datum;

/** The prover's chosen L1 witness about the committed deposit identity. */
export const FabricatedDepositEvidenceV1Schema = Data.Enum([
  Data.Object({
    AbsentDepositIdentity: Data.Object({
      unspent_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PresentDepositEvent: Data.Object({
      hub_ref_input_index: Data.Integer(),
      event_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type FabricatedDepositEvidenceV1 = Data.Static<
  typeof FabricatedDepositEvidenceV1Schema
>;
export const FabricatedDepositEvidenceV1 =
  FabricatedDepositEvidenceV1Schema as unknown as FabricatedDepositEvidenceV1;

/** What L1 says about the committed identity, once authenticated. */
export const FabricatedDepositEvidenceVerdictV1Schema = Data.Enum([
  Data.Literal("DepositIdentityAbsent"),
  Data.Object({
    DepositEventObserved: Data.Object({
      event_datum_hash: H32Schema,
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
]);
export type FabricatedDepositEvidenceVerdictV1 = Data.Static<
  typeof FabricatedDepositEvidenceVerdictV1Schema
>;
export const FabricatedDepositEvidenceVerdictV1 =
  FabricatedDepositEvidenceVerdictV1Schema as unknown as FabricatedDepositEvidenceVerdictV1;

export const FabricatedDepositStep02ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** The prover's chosen L1 witness. */
  evidence: FabricatedDepositEvidenceV1Schema,
});
export type FabricatedDepositStep02Args = Data.Static<
  typeof FabricatedDepositStep02ArgsSchema
>;
export const FabricatedDepositStep02Args =
  FabricatedDepositStep02ArgsSchema as unknown as FabricatedDepositStep02Args;

export const FabricatedDepositStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep02ArgsSchema);
export type FabricatedDepositStep02SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep02SpendRedeemerSchema
>;
export const FabricatedDepositStep02SpendRedeemer =
  FabricatedDepositStep02SpendRedeemerSchema as unknown as FabricatedDepositStep02SpendRedeemer;

// ## Step 03 — fault classification

export const FabricatedDepositStep03StateSchema = Data.Object({
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
  verdict: FabricatedDepositEvidenceVerdictV1Schema,
});
export type FabricatedDepositStep03State = Data.Static<
  typeof FabricatedDepositStep03StateSchema
>;
export const FabricatedDepositStep03State =
  FabricatedDepositStep03StateSchema as unknown as FabricatedDepositStep03State;

export const FabricatedDepositStep03DatumSchema = faultProofStepDatumSchema(
  FabricatedDepositStep03StateSchema,
);
export type FabricatedDepositStep03Datum = Data.Static<
  typeof FabricatedDepositStep03DatumSchema
>;
export const FabricatedDepositStep03Datum =
  FabricatedDepositStep03DatumSchema as unknown as FabricatedDepositStep03Datum;

/**
 * The prover's opening of step-02's retained event-datum commitment.
 *
 * On chain the `RetainedEventDatum` field is an opaque `Data`, because the step
 * hashes it before it decodes it. Off chain it is typed as the `DepositDatum` it
 * must decode to: the wire bytes are identical, and the tighter type means a
 * builder cannot assemble an opening the L1 step would reject at decode time.
 */
export const FabricatedDepositAuthenticContentOpeningV1Schema = Data.Enum([
  Data.Literal("NoAuthenticContent"),
  Data.Object({
    RetainedEventDatum: Data.Object({ event_datum: DepositDatumSchema }),
  }),
]);
export type FabricatedDepositAuthenticContentOpeningV1 = Data.Static<
  typeof FabricatedDepositAuthenticContentOpeningV1Schema
>;
export const FabricatedDepositAuthenticContentOpeningV1 =
  FabricatedDepositAuthenticContentOpeningV1Schema as unknown as FabricatedDepositAuthenticContentOpeningV1;

export const FabricatedDepositStep03ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** The prover's opening of step-02's retained commitment. */
  authentic_content: FabricatedDepositAuthenticContentOpeningV1Schema,
});
export type FabricatedDepositStep03Args = Data.Static<
  typeof FabricatedDepositStep03ArgsSchema
>;
export const FabricatedDepositStep03Args =
  FabricatedDepositStep03ArgsSchema as unknown as FabricatedDepositStep03Args;

export const FabricatedDepositStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep03ArgsSchema);
export type FabricatedDepositStep03SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep03SpendRedeemerSchema
>;
export const FabricatedDepositStep03SpendRedeemer =
  FabricatedDepositStep03SpendRedeemerSchema as unknown as FabricatedDepositStep03SpendRedeemer;

// ## Step 04 — the established fault

/** The `FabricatedDeposit` violation, in its two shapes. */
export const FabricatedDepositFaultV1Schema = Data.Enum([
  Data.Literal("NonexistentDepositIdentity"),
  Data.Object({
    MismatchedDepositContent: Data.Object({
      committed_deposit_info_hash: H32Schema,
      authentic_deposit_info_hash: H32Schema,
      event_inclusion_time: POSIXTimeSchema,
    }),
  }),
]);
export type FabricatedDepositFaultV1 = Data.Static<
  typeof FabricatedDepositFaultV1Schema
>;
export const FabricatedDepositFaultV1 =
  FabricatedDepositFaultV1Schema as unknown as FabricatedDepositFaultV1;

export const FabricatedDepositStep04StateSchema = Data.Object({
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: ChallengedHeaderHashSchema,
  /** Challenged header's `start_time`. */
  header_start_time: POSIXTimeSchema,
  /** Challenged header's `end_time`. */
  header_end_time: POSIXTimeSchema,
  /** The committed deposit identity — an L1 output reference. */
  committed_deposit_id: OutputReferenceSchema,
  /** The classified fault. */
  fault: FabricatedDepositFaultV1Schema,
});
export type FabricatedDepositStep04State = Data.Static<
  typeof FabricatedDepositStep04StateSchema
>;
export const FabricatedDepositStep04State =
  FabricatedDepositStep04StateSchema as unknown as FabricatedDepositStep04State;

export const FabricatedDepositStep04DatumSchema = faultProofStepDatumSchema(
  FabricatedDepositStep04StateSchema,
);
export type FabricatedDepositStep04Datum = Data.Static<
  typeof FabricatedDepositStep04DatumSchema
>;
export const FabricatedDepositStep04Datum =
  FabricatedDepositStep04DatumSchema as unknown as FabricatedDepositStep04Datum;

export const FabricatedDepositStep04ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Index of the fraud-proof mint redeemer. */
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type FabricatedDepositStep04Args = Data.Static<
  typeof FabricatedDepositStep04ArgsSchema
>;
export const FabricatedDepositStep04Args =
  FabricatedDepositStep04ArgsSchema as unknown as FabricatedDepositStep04Args;

export const FabricatedDepositStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(FabricatedDepositStep04ArgsSchema);
export type FabricatedDepositStep04SpendRedeemer = Data.Static<
  typeof FabricatedDepositStep04SpendRedeemerSchema
>;
export const FabricatedDepositStep04SpendRedeemer =
  FabricatedDepositStep04SpendRedeemerSchema as unknown as FabricatedDepositStep04SpendRedeemer;

// ## Step resolver

export const FABRICATED_DEPOSIT_STEP_NAMES_V1 = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
] as const;
export type FabricatedDepositStepNameV1 =
  (typeof FABRICATED_DEPOSIT_STEP_NAMES_V1)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch: adding
 * a step without adding its schema fails to compile.
 */
export const fabricatedDepositStepDatumSchemaV1 = (
  step: FabricatedDepositStepNameV1,
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
export const depositInfoCommitmentV1 = (
  info: DepositInfo,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(Data.to(info, DepositInfo), 32);

/**
 * Blake2b-256 of a deposit event datum's canonical bytes — step-02's retained
 * commitment, whose preimage step-03 re-opens.
 */
export const depositEventDatumCommitmentV1 = (
  datum: DepositDatum,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(Data.to(datum, DepositDatum), 32);

/**
 * The deposit event NFT asset name for a committed identity: Blake2b-256 of the
 * `DepositId`'s canonical bytes. Twin of `user_events.out_ref_to_nonce`, and the
 * reason a still-unspent output at that reference proves no such event was ever
 * authenticated.
 */
export const depositEventNonceV1 = (
  depositId: OutputReference,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(Data.to(depositId, OutputReference), 32);

/** The canonical bytes of a committed deposit leaf's MPF key. */
export const committedDepositKeyBytesV1 = (
  depositId: OutputReference,
): string => Data.to(depositId, OutputReference);

/** The canonical bytes of a committed deposit leaf's MPF value. */
export const committedDepositValueBytesV1 = (info: DepositInfo): string =>
  Data.to(info, DepositInfo);

// ## Handoffs

/**
 * The step-01 → step-02 handoff, derived from the authenticated header facts and
 * the committed leaf the membership witness opens. Twin of the step-01
 * validator's `expected_output_state`.
 */
export const fabricatedDepositStep02StateV1 = ({
  challengedHeaderHash,
  headerStartTime,
  headerEndTime,
  committedDeposit,
}: {
  readonly challengedHeaderHash: ChallengedHeaderHash;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly committedDeposit: CommittedDepositSourceProofV1;
}): Effect.Effect<FabricatedDepositStep02State, HashingError> =>
  Effect.map(
    depositInfoCommitmentV1(committedDeposit.value),
    (committed_deposit_info_hash) => ({
      challenged_header_hash: challengedHeaderHash,
      header_start_time: headerStartTime,
      header_end_time: headerEndTime,
      committed_deposit_id: committedDeposit.key,
      committed_deposit_info_hash,
    }),
  );

/** The step-02 → step-03 handoff: the same facts plus the authenticated verdict. */
export const fabricatedDepositStep03StateV1 = (
  state: FabricatedDepositStep02State,
  verdict: FabricatedDepositEvidenceVerdictV1,
): FabricatedDepositStep03State => ({
  challenged_header_hash: state.challenged_header_hash,
  header_start_time: state.header_start_time,
  header_end_time: state.header_end_time,
  committed_deposit_id: state.committed_deposit_id,
  committed_deposit_info_hash: state.committed_deposit_info_hash,
  verdict,
});

/** The step-03 → step-04 handoff: the classified fault replaces the verdict. */
export const fabricatedDepositStep04StateV1 = (
  state: FabricatedDepositStep03State,
  fault: FabricatedDepositFaultV1,
): FabricatedDepositStep04State => ({
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
export const isFabricatedDepositFaultV1 = (
  state: FabricatedDepositStep04State,
): boolean => {
  const { fault } = state;
  if (fault === "NonexistentDepositIdentity") {
    return true;
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
export type FabricatedDepositCountedRootInputV1 = {
  readonly phasRoot: MerkleRoot;
  readonly count: bigint;
};

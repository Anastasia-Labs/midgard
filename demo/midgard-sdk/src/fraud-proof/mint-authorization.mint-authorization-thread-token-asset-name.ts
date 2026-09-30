import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { MerkleRootSchema } from "../common.js";
import {
  EventKeySchema,
  EventToStepValueSchema,
  HeaderSchema,
  TransitionStepSchema,
} from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { type ChallengedHeaderHash } from "./fabricated-deposit.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

/** Normative violation identifier. */
export const MINT_AUTHORIZATION_VIOLATION_ID = "mint-authorization" as const;

// ## Engine constants (twin of `engine.ak`), as `Data` integers

/** Direction A: no script source with the claimed policy hash. */
export const MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT = 0n;

/** Direction B: the policy's native script evaluates unsatisfied. */
export const MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED = 1n;

// ## Thread NFT asset name

/**
 * A mint-authorization computation-thread token's asset name: the family's
 * category id (4 bytes, allocated at registration) followed by the
 * challenged header hash.
 */
export const mintAuthorizationThreadTokenAssetName = (
  categoryId: string,
  challengedHeaderHash: ChallengedHeaderHash,
): string => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error(
      "mint-authorization category id must be 4 bytes of lowercase hex",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error("challenged header hash must be 28 bytes of lowercase hex");
  }
  return `${categoryId}${challengedHeaderHash}`;
};

// ## Thread states

/**
 * Step-02's input state (step-01's output): the §2.5 anchor of the disputed
 * transaction plus its committed validity interval. Twin of
 * `step_02.State`.
 */
export const MintAuthorizationStep02StateSchema = Data.Object({
  bad_tx_id: Data.Bytes(),
  bad_tx_witness_set_hash: Data.Bytes(),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
});

export type MintAuthorizationStep02State = Data.Static<
  typeof MintAuthorizationStep02StateSchema
>;

export const MintAuthorizationStep02State =
  asDataType<MintAuthorizationStep02State>(MintAuthorizationStep02StateSchema);

/** Step-03's input state (step-02's output). Twin of `step_03.State`. */
export const MintAuthorizationStep03StateSchema = Data.Object({
  /** The claimed policy id, read off the committed field-5 item by step-02. */
  policy_id: Data.Bytes(),
  direction: Data.Integer(),
  bad_tx_id: Data.Bytes(),
  bad_tx_witness_set_hash: Data.Bytes(),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  /** The transition step's `pre_utxos_root`. */
  prior_ledger_root: MerkleRootSchema,
});

export type MintAuthorizationStep03State = Data.Static<
  typeof MintAuthorizationStep03StateSchema
>;

export const MintAuthorizationStep03State =
  asDataType<MintAuthorizationStep03State>(MintAuthorizationStep03StateSchema);

/**
 * Step-04's input state (also its self-loop output). Twin of
 * `step_04.State`.
 */
export const MintAuthorizationStep04StateSchema = Data.Object({
  policy_id: Data.Bytes(),
  bad_tx_id: Data.Bytes(),
  prior_ledger_root: MerkleRootSchema,
  /** Next field-1 ordinal to resolve; step-03's direction-A arm writes 0. */
  ref_cursor: Data.Integer(),
});

export type MintAuthorizationStep04State = Data.Static<
  typeof MintAuthorizationStep04StateSchema
>;

export const MintAuthorizationStep04State =
  asDataType<MintAuthorizationStep04State>(MintAuthorizationStep04StateSchema);

/** Step-05's input state — the closed verdict. Twin of `step_05.State`. */
export const MintAuthorizationStep05StateSchema = Data.Object({
  policy_id: Data.Bytes(),
  direction: Data.Integer(),
});

export type MintAuthorizationStep05State = Data.Static<
  typeof MintAuthorizationStep05StateSchema
>;

export const MintAuthorizationStep05State =
  asDataType<MintAuthorizationStep05State>(MintAuthorizationStep05StateSchema);

// ## Step 01 — bind the accepted committed transaction

export const MintAuthorizationStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);

export type MintAuthorizationStep01Datum = Data.Static<
  typeof MintAuthorizationStep01DatumSchema
>;

export const MintAuthorizationStep01Datum =
  asDataType<MintAuthorizationStep01Datum>(MintAuthorizationStep01DatumSchema);

/** Twin of `step_01.Args`. */
export const MintAuthorizationStep01ArgsSchema = Data.Object({
  carriage: NativeTxInclusionCarriageSchema,
});

export type MintAuthorizationStep01Args = Data.Static<
  typeof MintAuthorizationStep01ArgsSchema
>;

export const MintAuthorizationStep01Args =
  asDataType<MintAuthorizationStep01Args>(MintAuthorizationStep01ArgsSchema);

export const MintAuthorizationStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep01ArgsSchema);

export type MintAuthorizationStep01SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep01SpendRedeemerSchema
>;

export const MintAuthorizationStep01SpendRedeemer =
  asDataType<MintAuthorizationStep01SpendRedeemer>(
    MintAuthorizationStep01SpendRedeemerSchema,
  );

// ## Step 02 — committed-claim openings

export const MintAuthorizationStep02DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep02StateSchema,
);

export type MintAuthorizationStep02Datum = Data.Static<
  typeof MintAuthorizationStep02DatumSchema
>;

export const MintAuthorizationStep02Datum =
  asDataType<MintAuthorizationStep02Datum>(MintAuthorizationStep02DatumSchema);

export const MintAuthorizationMintScanControlSchema = Data.Object({
  item_count: Data.Integer(),
  item_index: Data.Integer(),
  cursor: Data.Integer(),
  item_end: Data.Integer(),
  remaining_assets: Data.Integer(),
  previous_asset_name: Data.Nullable(Data.Bytes()),
  policy_id: Data.Bytes(),
  selected_complete: Data.Boolean(),
});

export type MintAuthorizationMintScanControl = Data.Static<
  typeof MintAuthorizationMintScanControlSchema
>;

export const MintAuthorizationMintScanControl =
  asDataType<MintAuthorizationMintScanControl>(
    MintAuthorizationMintScanControlSchema,
  );

export const MintAuthorizationMintScanStateSchema = Data.Object({
  bad_tx_id: Data.Bytes(),
  bad_tx_witness_set_hash: Data.Bytes(),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  prior_ledger_root: Data.Bytes(),
  policy_index: Data.Integer(),
  direction: Data.Integer(),
  field_hash: Data.Bytes(),
  control: MintAuthorizationMintScanControlSchema,
});

export type MintAuthorizationMintScanState = Data.Static<
  typeof MintAuthorizationMintScanStateSchema
>;

export const MintAuthorizationMintScanState =
  asDataType<MintAuthorizationMintScanState>(
    MintAuthorizationMintScanStateSchema,
  );

export const MintAuthorizationStep02ThreadDatumSchema =
  faultProofStepDatumSchema(
    Data.Enum([
      Data.Object({ Bound: MintAuthorizationStep02StateSchema }),
      Data.Object({ Scan: MintAuthorizationMintScanStateSchema }),
    ]),
  );

export type MintAuthorizationStep02ThreadDatum = Data.Static<
  typeof MintAuthorizationStep02ThreadDatumSchema
>;

export const MintAuthorizationStep02ThreadDatum =
  asDataType<MintAuthorizationStep02ThreadDatum>(
    MintAuthorizationStep02ThreadDatumSchema,
  );

export const MintAuthorizationEventToStepMembershipSchema =
  rootMembershipProofSchema(EventKeySchema, EventToStepValueSchema);

export const MintAuthorizationTransitionStepMembershipSchema =
  rootMembershipProofSchema(Data.Integer(), TransitionStepSchema);

/** Twin of `step_02.Args`. */
export const MintAuthorizationStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  /** The disputed block's header, bound to the thread NFT's asset name. */
  header: HeaderSchema,
  event_to_step_membership: MintAuthorizationEventToStepMembershipSchema,
  transition_step_membership: MintAuthorizationTransitionStepMembershipSchema,
  /**
   * Ordinal of the accused field-5 policy item. The policy id itself is
   * read off the committed item, never supplied.
   */
  policy_index: Data.Integer(),
  direction: Data.Integer(),
  mint_opening: FieldOpeningSchema,
});

export type MintAuthorizationStep02Args = Data.Static<
  typeof MintAuthorizationStep02ArgsSchema
>;

export const MintAuthorizationStep02Args =
  asDataType<MintAuthorizationStep02Args>(MintAuthorizationStep02ArgsSchema);

export const MintAuthorizationStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep02ArgsSchema);

export type MintAuthorizationStep02SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep02SpendRedeemerSchema
>;

export const MintAuthorizationStep02SpendRedeemer =
  asDataType<MintAuthorizationStep02SpendRedeemer>(
    MintAuthorizationStep02SpendRedeemerSchema,
  );

export const MintAuthorizationClaimEvidenceSchema = Data.Object({
  header: HeaderSchema,
  event_to_step_membership: MintAuthorizationEventToStepMembershipSchema,
  transition_step_membership: MintAuthorizationTransitionStepMembershipSchema,
  policy_index: Data.Integer(),
  direction: Data.Integer(),
});

export type MintAuthorizationClaimEvidence = Data.Static<
  typeof MintAuthorizationClaimEvidenceSchema
>;

export const MintAuthorizationClaimEvidence =
  asDataType<MintAuthorizationClaimEvidence>(
    MintAuthorizationClaimEvidenceSchema,
  );

export const MintAuthorizationStep02PublishedSpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Enum([
      Data.Object({ Inline: MintAuthorizationStep02ArgsSchema }),
      Data.Object({
        Published: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          evidence: Data.Any(),
          mint_opening: FieldOpeningSchema,
        }),
      }),
      Data.Object({
        AdvanceScan: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          mint_opening: FieldOpeningSchema,
        }),
      }),
    ]),
  );

export type MintAuthorizationStep02PublishedSpendRedeemer = Data.Static<
  typeof MintAuthorizationStep02PublishedSpendRedeemerSchema
>;

export const MintAuthorizationStep02PublishedSpendRedeemer =
  asDataType<MintAuthorizationStep02PublishedSpendRedeemer>(
    MintAuthorizationStep02PublishedSpendRedeemerSchema,
  );

// ## Step 03 — direction dispatch

export const MintAuthorizationStep03DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep03StateSchema,
);

export type MintAuthorizationStep03Datum = Data.Static<
  typeof MintAuthorizationStep03DatumSchema
>;

export const MintAuthorizationStep03Datum =
  asDataType<MintAuthorizationStep03Datum>(MintAuthorizationStep03DatumSchema);

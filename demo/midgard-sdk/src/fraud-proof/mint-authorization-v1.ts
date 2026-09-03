/**
 * `mint-authorization` family — off-chain codec twins.
 *
 * Proves that an operator-ACCEPTED committed L2 transaction mints or burns
 * under a policy id that never authorized it, in either direction:
 * direction A (script absent — no script with that hash among the
 * transaction's machine-consulted script sources) or direction B (script
 * present but its native payload evaluates unsatisfied against the committed
 * signer set and validity interval).
 *
 * Violation: `mint-authorization`.
 * Catalogue category: **not registered yet** — this module is reached by
 * direct import rather than through `fraud-proof/catalogue.ts`, and the
 * asset-name helper is parameterized on the category id instead of pinning
 * one. Emulator wiring reserves `0000001b`.
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/mint-authorization/
 * step-0{1,2,3,4,5}.ak` field for field and constructor index for
 * constructor index.
 */
import { Data } from "@lucid-evolution/lucid";

import { MerkleRootSchema, ProofSchema } from "../common.js";
import {
  EventKeySchema,
  EventToStepValueSchema,
  HeaderSchema,
  TransitionStepSchema,
} from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { type ChallengedHeaderHash } from "./fabricated-deposit-v1.js";
import { FieldOpeningSchema } from "./field-opening-v1.js";
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
  MintAuthorizationStep02StateSchema as unknown as MintAuthorizationStep02State;

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
  MintAuthorizationStep03StateSchema as unknown as MintAuthorizationStep03State;

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
  MintAuthorizationStep04StateSchema as unknown as MintAuthorizationStep04State;

/** Step-05's input state — the closed verdict. Twin of `step_05.State`. */
export const MintAuthorizationStep05StateSchema = Data.Object({
  policy_id: Data.Bytes(),
  direction: Data.Integer(),
});
export type MintAuthorizationStep05State = Data.Static<
  typeof MintAuthorizationStep05StateSchema
>;
export const MintAuthorizationStep05State =
  MintAuthorizationStep05StateSchema as unknown as MintAuthorizationStep05State;

// ## Step 01 — bind the accepted committed transaction

export const MintAuthorizationStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type MintAuthorizationStep01Datum = Data.Static<
  typeof MintAuthorizationStep01DatumSchema
>;
export const MintAuthorizationStep01Datum =
  MintAuthorizationStep01DatumSchema as unknown as MintAuthorizationStep01Datum;

/** Twin of `step_01.Args`. */
export const MintAuthorizationStep01ArgsSchema = Data.Object({
  carriage: NativeTxInclusionCarriageSchema,
});
export type MintAuthorizationStep01Args = Data.Static<
  typeof MintAuthorizationStep01ArgsSchema
>;
export const MintAuthorizationStep01Args =
  MintAuthorizationStep01ArgsSchema as unknown as MintAuthorizationStep01Args;

export const MintAuthorizationStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep01ArgsSchema);
export type MintAuthorizationStep01SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep01SpendRedeemerSchema
>;
export const MintAuthorizationStep01SpendRedeemer =
  MintAuthorizationStep01SpendRedeemerSchema as unknown as MintAuthorizationStep01SpendRedeemer;

// ## Step 02 — committed-claim openings

export const MintAuthorizationStep02DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep02StateSchema,
);
export type MintAuthorizationStep02Datum = Data.Static<
  typeof MintAuthorizationStep02DatumSchema
>;
export const MintAuthorizationStep02Datum =
  MintAuthorizationStep02DatumSchema as unknown as MintAuthorizationStep02Datum;

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
  MintAuthorizationStep02ArgsSchema as unknown as MintAuthorizationStep02Args;

export const MintAuthorizationStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep02ArgsSchema);
export type MintAuthorizationStep02SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep02SpendRedeemerSchema
>;
export const MintAuthorizationStep02SpendRedeemer =
  MintAuthorizationStep02SpendRedeemerSchema as unknown as MintAuthorizationStep02SpendRedeemer;

// ## Step 03 — direction dispatch

export const MintAuthorizationStep03DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep03StateSchema,
);
export type MintAuthorizationStep03Datum = Data.Static<
  typeof MintAuthorizationStep03DatumSchema
>;
export const MintAuthorizationStep03Datum =
  MintAuthorizationStep03DatumSchema as unknown as MintAuthorizationStep03Datum;

/**
 * Twin of `step_03.Args`: `WitnessAbsence` (direction A's inline half,
 * routing into step-04's scan) is constructor 0, `EvaluateUnsatisfied`
 * (direction B, closing straight to step-05) is constructor 1.
 */
export const MintAuthorizationStep03ArgsSchema = Data.Enum([
  Data.Object({
    WitnessAbsence: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    EvaluateUnsatisfied: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      /** The policy's native payload, pinned by hash to the policy id. */
      script_bytes: Data.Bytes(),
      addr_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
]);
export type MintAuthorizationStep03Args = Data.Static<
  typeof MintAuthorizationStep03ArgsSchema
>;
export const MintAuthorizationStep03Args =
  MintAuthorizationStep03ArgsSchema as unknown as MintAuthorizationStep03Args;

export const MintAuthorizationStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep03ArgsSchema);
export type MintAuthorizationStep03SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep03SpendRedeemerSchema
>;
export const MintAuthorizationStep03SpendRedeemer =
  MintAuthorizationStep03SpendRedeemerSchema as unknown as MintAuthorizationStep03SpendRedeemer;

// ## Step 04 — direction-A reference-input scan (self-loop)

export const MintAuthorizationStep04DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep04StateSchema,
);
export type MintAuthorizationStep04Datum = Data.Static<
  typeof MintAuthorizationStep04DatumSchema
>;
export const MintAuthorizationStep04Datum =
  MintAuthorizationStep04DatumSchema as unknown as MintAuthorizationStep04Datum;

/**
 * Twin of `step_04.Args`: `ResolveNext` (self-loop over the cursor) is
 * constructor 0, `AdvanceComplete` (cursor equals the authenticated field-1
 * item count) is constructor 1.
 */
export const MintAuthorizationStep04ArgsSchema = Data.Enum([
  Data.Object({
    ResolveNext: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningSchema,
      descriptor_cbor: Data.Bytes(),
      ledger_membership_proof: ProofSchema,
    }),
  }),
  Data.Object({
    AdvanceComplete: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningSchema,
    }),
  }),
]);
export type MintAuthorizationStep04Args = Data.Static<
  typeof MintAuthorizationStep04ArgsSchema
>;
export const MintAuthorizationStep04Args =
  MintAuthorizationStep04ArgsSchema as unknown as MintAuthorizationStep04Args;

export const MintAuthorizationStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep04ArgsSchema);
export type MintAuthorizationStep04SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep04SpendRedeemerSchema
>;
export const MintAuthorizationStep04SpendRedeemer =
  MintAuthorizationStep04SpendRedeemerSchema as unknown as MintAuthorizationStep04SpendRedeemer;

// ## Step 05 — finalize

export const MintAuthorizationStep05DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep05StateSchema,
);
export type MintAuthorizationStep05Datum = Data.Static<
  typeof MintAuthorizationStep05DatumSchema
>;
export const MintAuthorizationStep05Datum =
  MintAuthorizationStep05DatumSchema as unknown as MintAuthorizationStep05Datum;

/** Twin of `step_05.Args`. */
export const MintAuthorizationStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type MintAuthorizationStep05Args = Data.Static<
  typeof MintAuthorizationStep05ArgsSchema
>;
export const MintAuthorizationStep05Args =
  MintAuthorizationStep05ArgsSchema as unknown as MintAuthorizationStep05Args;

export const MintAuthorizationStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep05ArgsSchema);
export type MintAuthorizationStep05SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep05SpendRedeemerSchema
>;
export const MintAuthorizationStep05SpendRedeemer =
  MintAuthorizationStep05SpendRedeemerSchema as unknown as MintAuthorizationStep05SpendRedeemer;

// ## Step resolver

export const MINT_AUTHORIZATION_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
  "step_05",
] as const;
export type MintAuthorizationStepName =
  (typeof MINT_AUTHORIZATION_STEP_NAMES)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch:
 * adding a step without adding its schema fails to compile.
 */
export const mintAuthorizationStepDatumSchema = (
  step: MintAuthorizationStepName,
) => {
  switch (step) {
    case "step_01":
      return MintAuthorizationStep01DatumSchema;
    case "step_02":
      return MintAuthorizationStep02DatumSchema;
    case "step_03":
      return MintAuthorizationStep03DatumSchema;
    case "step_04":
      return MintAuthorizationStep04DatumSchema;
    case "step_05":
      return MintAuthorizationStep05DatumSchema;
  }
};

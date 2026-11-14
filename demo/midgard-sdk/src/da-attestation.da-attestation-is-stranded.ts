import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, fromText, toUnit } from "@lucid-evolution/lucid";

import { DaAvailabilityCommitmentSchema } from "./availability-challenge.js";
import { AddressSchema, type AuthenticatedValidator } from "./common.js";
import { HeaderHashSchema } from "./ledger-state.js";

export const DA_PARAMS_ASSET_NAME = fromText("MIDGARD_DA_PARAMS");

export const DA_ATTESTATION_ASSET_NAME_PREFIX = fromText("DAAT");

export const EMPTY_ATTESTED_SIGNER_BITMAP =
  "0000000000000000000000000000000000000000000000000000000000000000";

const ATTESTED_SIGNER_BITMAP_BYTES = 32;

export const ATTESTED_SIGNER_BITMAP_HEX_LENGTH =
  ATTESTED_SIGNER_BITMAP_BYTES * 2;

export const SIGNATURE_HEX_LENGTH = 64 * 2;

export const VERIFICATION_KEY_HEX_LENGTH = 32 * 2;

export const DaParamsDatumSchema = Data.Object({
  committee: Data.Bytes(),
  committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  da_threshold: Data.Integer(),
  owners: Data.Array(Data.Bytes({ minLength: 28, maxLength: 28 })),
  update_threshold: Data.Integer(),
});

export type DaParamsDatum = Data.Static<typeof DaParamsDatumSchema>;

export const DaParamsDatum = asDataType<DaParamsDatum>(DaParamsDatumSchema);

/**
 * Smallest owner set the DA params governor will represent: **one**.
 *
 * This was two until the 2026-08-13 in-session owner ruling recorded on #602
 * dropped the governor's owner-set minimum, making a one-owner set carrying
 * `updateThreshold === 1` representable. Single-key governance — one key
 * rotating the committee and both thresholds — is accepted behaviour by owner
 * decision, not an unreachable state.
 *
 * On-chain there is no longer a matching constant: at one the check would be a
 * guard that cannot fail, because `sorted_unique_len_at_most` aborts on an
 * empty list before a count exists, so `da-params-governor.ak` carries the
 * non-emptiness refusal structurally and declares no `min_owner_count`.
 *
 * Off-chain the constant survives because the check here is *not* vacuous:
 * {@link daParamsFloorViolations} takes `ownerCount` as caller-supplied data
 * and a caller can pass zero. It is reported as its own
 * `owner_set_below_minimum` class so a caller can tell an empty set apart from
 * a threshold that merely sits below its floor.
 *
 * Source: `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md`
 * §4 (Q63, ACCEPTED; §4 amended 2026-08-11 and 2026-08-13).
 */
export const MIN_DA_OWNER_COUNT = 1;

/**
 * Smallest DA committee the governor will represent: **one**.
 *
 * The committee-side twin of {@link MIN_DA_OWNER_COUNT}, and it exists for the
 * same reason. On-chain the empty committee is refused structurally — the same
 * `sorted_unique_*` walker aborts before any count exists — so the validator
 * declares no constant for it either. Off-chain {@link daParamsFloorViolations}
 * takes `committeeLength` as caller-supplied data and a caller can pass zero,
 * where the threshold bounds alone would say nothing: `governedThresholdFloor(0)
 * === 0`, so `daThreshold: 0` sits on that floor and does not exceed the empty
 * committee either. Without this class an empty committee would be reported as
 * no violation at all.
 */
export const MIN_DA_COMMITTEE_SIZE = 1;

/**
 * F04 §4 governed threshold floor: `ceil(2*setLength/3)`, defined for
 * `setLength >= 1`.
 *
 * TypeScript twin of `governed_threshold_floor` in
 * `onchain/aiken/validators/da-params-governor.ak`. Both evaluate the ceiling
 * as `(2*setLength + 2) / 3` under integer division, and neither carries a
 * lower clamp: the 2026-08-11 owner ruling lifted the 1-of-1 prohibition, so a
 * one-member DA committee floors at one and a single-key attest loop is
 * representable. The 2026-08-13 ruling extended the same shape to the owner
 * set — see {@link MIN_DA_OWNER_COUNT} — so a lone owner governs at
 * `updateThreshold === 1`. What the floor still guarantees at every set size is
 * that it never returns less than one: no set can name a threshold of zero.
 *
 * How far the cross-language agreement is actually measured differs by side,
 * and the two should not be conflated. Off-chain,
 * `tests/da-governor-safety.test.ts` sweeps every representable set size — 0
 * through `max_indexed_signer_count` (256), the largest committee the
 * attested-signer bitmap can index — and pins the whole table by digest.
 * On-chain, the equivalent Aiken test pins a *sample* of set sizes (the lifted
 * region, the boundary where the ceiling overtakes two, and the top of the
 * range), because a full sweep in a Plutus test is not practical. So:
 * full-table off-chain, sample-pinned on-chain, against the same shared
 * vectors.
 */
export const governedThresholdFloor = (setLength: number): number => {
  if (!Number.isSafeInteger(setLength) || setLength < 0) {
    throw new Error(
      `governed threshold floor requires a non-negative integer set size, received ${String(setLength)}`,
    );
  }
  return Math.floor((2 * setLength + 2) / 3);
};

/** One governed-bound violation class reported by {@link daParamsFloorViolations}. */
export type DaParamsFloorViolation =
  | "committee_below_minimum"
  | "owner_set_below_minimum"
  | "da_threshold_below_floor"
  | "da_threshold_exceeds_committee"
  | "update_threshold_below_floor"
  | "update_threshold_exceeds_owner_set";

/**
 * Off-chain twin of the governed bounds `valid_datum` enforces in
 * `da-params-governor.ak`. Returns every violated class so a caller can reject
 * DA params before submitting a transaction the governor would refuse.
 *
 * This deliberately covers only the governed thresholds and set sizes; the
 * sorted-unique committee encoding and the `committee_signers_hash` binding
 * remain the on-chain validator's checks.
 */
export const daParamsFloorViolations = (params: {
  readonly committeeLength: number;
  readonly daThreshold: number;
  readonly ownerCount: number;
  readonly updateThreshold: number;
}): DaParamsFloorViolation[] => {
  const violations: DaParamsFloorViolation[] = [];

  if (params.committeeLength < MIN_DA_COMMITTEE_SIZE) {
    violations.push("committee_below_minimum");
  }
  if (params.daThreshold < governedThresholdFloor(params.committeeLength)) {
    violations.push("da_threshold_below_floor");
  }
  if (params.daThreshold > params.committeeLength) {
    violations.push("da_threshold_exceeds_committee");
  }
  if (params.ownerCount < MIN_DA_OWNER_COUNT) {
    violations.push("owner_set_below_minimum");
  }
  if (params.updateThreshold < governedThresholdFloor(params.ownerCount)) {
    violations.push("update_threshold_below_floor");
  }
  if (params.updateThreshold > params.ownerCount) {
    violations.push("update_threshold_exceeds_owner_set");
  }

  return violations;
};

export const DaAttestationDatumSchema = Data.Object({
  header_hash: HeaderHashSchema,
  availability_commitment: DaAvailabilityCommitmentSchema,
  da_threshold: Data.Integer(),
  committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  rescue_beneficiary: AddressSchema,
  attested_signers: Data.Bytes({ minLength: 32, maxLength: 32 }),
  attestation_count: Data.Integer(),
});

export type DaAttestationDatum = Data.Static<typeof DaAttestationDatumSchema>;

export const DaAttestationDatum = asDataType<DaAttestationDatum>(
  DaAttestationDatumSchema,
);

export const DaAttestationMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      state_queue_ref_input_index: Data.Integer(),
      state_queue_mint_ref_script_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ApplyToStateQueue: Data.Object({
      da_attestation_input_index: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      state_queue_mint_ref_script_input_index: Data.Integer(),
      /**
       * The authentic DA bond pool reference input, indexed over the ledger's
       * sorted reference-input set. It must be `Bonded` and back at least one
       * `da_bond_lovelace` above its floor.
       */
      pool_ref_input_index: Data.Integer(),
      /**
       * Receives exactly the burned attestation's value (less its DAAT) at
       * `rescue_beneficiary`.
       */
      refund_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RescueStrandedAttestation: Data.Object({
      da_attestation_input_index: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      refund_output_index: Data.Integer(),
    }),
  }),
]);

export type DaAttestationMintRedeemer = Data.Static<
  typeof DaAttestationMintRedeemerSchema
>;

export const DaAttestationMintRedeemer = asDataType<DaAttestationMintRedeemer>(
  DaAttestationMintRedeemerSchema,
);

export const DaAttestationSpendRedeemerSchema = Data.Enum([
  Data.Object({
    AddSignatures: Data.Object({
      output_index: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      signatures: Data.Bytes(),
    }),
  }),
  Data.Object({
    BurnForStateQueue: Data.Object({
      mint_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnForRescue: Data.Object({
      mint_redeemer_index: Data.Integer(),
    }),
  }),
]);

export type DaAttestationSpendRedeemer = Data.Static<
  typeof DaAttestationSpendRedeemerSchema
>;

export const DaAttestationSpendRedeemer =
  asDataType<DaAttestationSpendRedeemer>(DaAttestationSpendRedeemerSchema);

/**
 * The rescue path's entire authorization condition, mirrored off-chain
 * (decision row D-DA5 clause c).
 *
 * An attestation freezes both governed values at Init, and `ApplyToStateQueue`
 * requires both to still match. So it is stranded exactly when *either* has
 * moved — and that disjunction is the exact complement of the apply gate, which
 * is load-bearing rather than tidy.
 *
 * Testing only the committee hash would leave a second, silent strand:
 * governance may change `da_threshold` over an unchanged committee, and such an
 * attestation could then never apply (the threshold no longer matches) and
 * never be rescued (the committee hash still matches), while `AddSignatures`
 * kept accepting signatures that could never amount to anything. Its ADA would
 * be locked for good — the exact failure clause (c) exists to rule out.
 *
 * Nor is the disjunction too permissive: whenever it holds, the apply gate is
 * unsatisfiable no matter how many further signatures are gathered, so this can
 * never take value from an attestation still in flight. Rescuable and appliable
 * are complements, never both.
 *
 * There is deliberately no deadline and no configured value here: the state
 * condition *is* the proof of strandedness.
 *
 * The Aiken twin is the `expect or { ... }` in the `RescueStrandedAttestation`
 * branch of `onchain/aiken/validators/da-attestation.ak`.
 */
export const daAttestationIsStranded = (params: {
  readonly attestationDatum: Pick<
    DaAttestationDatum,
    "committee_signers_hash" | "da_threshold"
  >;
  readonly daParamsDatum: Pick<
    DaParamsDatum,
    "committee_signers_hash" | "da_threshold"
  >;
}): boolean =>
  params.attestationDatum.committee_signers_hash !==
    params.daParamsDatum.committee_signers_hash ||
  params.attestationDatum.da_threshold !== params.daParamsDatum.da_threshold;

export const daParamsUnit = (
  daParamsGovernor: AuthenticatedValidator,
): string => toUnit(daParamsGovernor.policyId, DA_PARAMS_ASSET_NAME);

export const daAttestationAssetName = (headerHash: string): string =>
  DA_ATTESTATION_ASSET_NAME_PREFIX + headerHash;

export const daAttestationUnit = (
  daAttestation: AuthenticatedValidator,
  headerHash: string,
): string => toUnit(daAttestation.policyId, daAttestationAssetName(headerHash));

export const prefixAttestedSignerBitmap = (signatureCount: number): string => {
  const bitmap = Buffer.alloc(32);
  for (let signerIndex = 0; signerIndex < signatureCount; signerIndex += 1) {
    const byteIndex = Math.floor(signerIndex / 8);
    const bitInByte = signerIndex % 8;
    bitmap[byteIndex] |= 1 << (7 - bitInByte);
  }
  return bitmap.toString("hex");
};

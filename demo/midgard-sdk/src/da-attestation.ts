import { assetsEqual } from "@al-ft/midgard-core/assets";
import {
  type BuildTxWithRedeemer,
  Data,
  fromText,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  assertCanonicalDaAvailabilityCommitment,
  daAvailabilityBondAssetName,
  type DaAvailabilityCommitment,
  DaAvailabilityCommitmentSchema,
  type DaAvailabilityMintRedeemer,
  DaAvailabilityMintRedeemerSchema,
  encodeDaAvailabilityBondDatum,
} from "./availability-challenge-v1.js";
import {
  type AddressData,
  addressDataFromBech32,
  AddressSchema,
  type AuthenticatedValidator,
  type GenericErrorFields,
  type MidgardValidators,
  outputReferenceFromUTxO,
} from "./common.js";
import {
  castStateQueueNodeToData,
  type HeaderHash,
  HeaderHashSchema,
  type StateQueueNode,
} from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "./linked-list.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import {
  DA_ATTESTATION_TIMEOUT_MS,
  StateQueueSpendRedeemer,
  type StateQueueUTxO,
} from "./state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

export const DA_PARAMS_ASSET_NAME = fromText("MIDGARD_DA_PARAMS");
export const DA_ATTESTATION_ASSET_NAME_PREFIX = fromText("DAAT");
export const EMPTY_ATTESTED_SIGNER_BITMAP =
  "0000000000000000000000000000000000000000000000000000000000000000";
const ATTESTED_SIGNER_BITMAP_BYTES = 32;
const ATTESTED_SIGNER_BITMAP_HEX_LENGTH = ATTESTED_SIGNER_BITMAP_BYTES * 2;
const SIGNATURE_HEX_LENGTH = 64 * 2;
const VERIFICATION_KEY_HEX_LENGTH = 32 * 2;

export const DaParamsDatumSchema = Data.Object({
  committee: Data.Bytes(),
  committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  da_threshold: Data.Integer(),
  owners: Data.Array(Data.Bytes({ minLength: 28, maxLength: 28 })),
  update_threshold: Data.Integer(),
});
export type DaParamsDatum = Data.Static<typeof DaParamsDatumSchema>;
export const DaParamsDatum = DaParamsDatumSchema as unknown as DaParamsDatum;

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
 * `tests/da-governor-safety-v1.test.ts` sweeps every representable set size — 0
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
export const DaAttestationDatum =
  DaAttestationDatumSchema as unknown as DaAttestationDatum;

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
      availability_mint_redeemer_index: Data.Integer(),
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
export const DaAttestationMintRedeemer =
  DaAttestationMintRedeemerSchema as unknown as DaAttestationMintRedeemer;

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
  DaAttestationSpendRedeemerSchema as unknown as DaAttestationSpendRedeemer;

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

export class DaAttestationBuildError extends EffectData.TaggedError(
  "DaAttestationBuildError",
)<GenericErrorFields> {}

export type DaAttestationReferenceScripts = {
  readonly daAttestationMinting: UTxO;
  readonly daAttestationSpending: UTxO;
  readonly availabilityChallengeMinting: UTxO;
  readonly stateQueueMinting: UTxO;
  readonly stateQueueSpending: UTxO;
};

export type DaAttestationStateQueueTarget = {
  readonly stateQueueUtxo: StateQueueUTxO;
  readonly stateQueueNode: StateQueueNode;
  readonly headerHash: HeaderHash;
};

export type DaAttestationUtxo = {
  readonly utxo: UTxO;
  readonly datum: DaAttestationDatum;
};

export type DaAttestationSignatureWitness = {
  readonly signerIndex: number;
  readonly signatureHex: string;
};

const failBuild = (
  message: string,
  cause: unknown,
): Effect.Effect<never, DaAttestationBuildError> =>
  Effect.fail(new DaAttestationBuildError({ message, cause }));

const isHexOfLength = (value: string, length: number): boolean =>
  value.length === length && /^[0-9a-fA-F]*$/.test(value);

const validateAttestedSignerBitmap = (
  attestedSignersHex: string,
): Effect.Effect<void, DaAttestationBuildError> =>
  isHexOfLength(attestedSignersHex, ATTESTED_SIGNER_BITMAP_HEX_LENGTH)
    ? Effect.void
    : failBuild(
        "Invalid DA attested-signer bitmap",
        `expected_hex_chars=${ATTESTED_SIGNER_BITMAP_HEX_LENGTH.toString()},actual_hex_chars=${attestedSignersHex.length.toString()}`,
      );

const validateSignerIndex = (
  signerIndex: number,
): Effect.Effect<void, DaAttestationBuildError> =>
  Number.isInteger(signerIndex) && signerIndex >= 0 && signerIndex <= 255
    ? Effect.void
    : failBuild(
        "Invalid DA signer index",
        `signer_index=${signerIndex.toString()}`,
      );

const validateSignatureHex = (
  signatureHex: string,
): Effect.Effect<void, DaAttestationBuildError> =>
  isHexOfLength(signatureHex, SIGNATURE_HEX_LENGTH)
    ? Effect.void
    : failBuild(
        "Invalid DA signature witness",
        `expected_hex_chars=${SIGNATURE_HEX_LENGTH.toString()},actual_hex_chars=${signatureHex.length.toString()}`,
      );

const validateCommitteeSize = (
  committeeSize: number,
): Effect.Effect<void, DaAttestationBuildError> =>
  Number.isInteger(committeeSize) && committeeSize >= 0 && committeeSize <= 256
    ? Effect.void
    : failBuild(
        "Invalid DA committee size",
        `committee_size=${committeeSize.toString()}`,
      );

const committeeSizeFromParamsDatum = (
  daParamsDatum: DaParamsDatum,
): Effect.Effect<number, DaAttestationBuildError> => {
  if (!/^[0-9a-fA-F]*$/.test(daParamsDatum.committee)) {
    return failBuild("Invalid DA committee bytes", "committee is not hex");
  }
  if (daParamsDatum.committee.length % VERIFICATION_KEY_HEX_LENGTH !== 0) {
    return failBuild(
      "Invalid DA committee bytes",
      `hex_chars=${daParamsDatum.committee.length.toString()}`,
    );
  }
  return Effect.succeed(
    daParamsDatum.committee.length / VERIFICATION_KEY_HEX_LENGTH,
  );
};

export const signerIndexIsDaAttested = (
  attestedSignersHex: string,
  signerIndex: number,
): boolean => {
  if (!Number.isInteger(signerIndex) || signerIndex < 0) {
    return false;
  }
  const bytes = Buffer.from(attestedSignersHex, "hex");
  const byteIndex = Math.floor(signerIndex / 8);
  const byte = bytes[byteIndex];
  if (byte === undefined) {
    return false;
  }
  const bitInByte = signerIndex % 8;
  return (byte & (1 << (7 - bitInByte))) !== 0;
};

export const countDaAttestedSigners = (
  attestedSignersHex: string,
): Effect.Effect<bigint, DaAttestationBuildError> =>
  validateAttestedSignerBitmap(attestedSignersHex).pipe(
    Effect.andThen(() => {
      let count = 0n;
      for (const byte of Buffer.from(attestedSignersHex, "hex")) {
        let value = byte;
        while (value !== 0) {
          count += BigInt(value & 1);
          value >>= 1;
        }
      }
      return count;
    }),
  );

export const encodeDaAttestationSignatureWitnesses = (
  witnesses: readonly DaAttestationSignatureWitness[],
): Effect.Effect<string, DaAttestationBuildError> =>
  Effect.gen(function* () {
    const seen = new Set<number>();
    const sorted = [...witnesses].sort(
      (left, right) => left.signerIndex - right.signerIndex,
    );
    const chunks: string[] = [];
    for (const witness of sorted) {
      yield* validateSignerIndex(witness.signerIndex);
      yield* validateSignatureHex(witness.signatureHex);
      if (seen.has(witness.signerIndex)) {
        return yield* failBuild(
          "Duplicate DA signature witness",
          `signer_index=${witness.signerIndex.toString()}`,
        );
      }
      seen.add(witness.signerIndex);
      chunks.push(
        `${witness.signerIndex.toString(16).padStart(2, "0")}${witness.signatureHex.toLowerCase()}`,
      );
    }
    return chunks.join("");
  });

export const applyDaAttestationSignatureWitnesses = (config: {
  readonly attestedSignersHex: string;
  readonly witnesses: readonly DaAttestationSignatureWitness[];
  readonly committeeSize?: number;
}): Effect.Effect<
  {
    readonly attestedSigners: string;
    readonly attestationCount: bigint;
    readonly packedWitnesses: string;
  },
  DaAttestationBuildError
> =>
  Effect.gen(function* () {
    yield* validateAttestedSignerBitmap(config.attestedSignersHex);
    if (config.committeeSize !== undefined) {
      yield* validateCommitteeSize(config.committeeSize);
    }
    const bytes = Buffer.from(config.attestedSignersHex, "hex");
    for (const witness of config.witnesses) {
      yield* validateSignerIndex(witness.signerIndex);
      if (
        config.committeeSize !== undefined &&
        witness.signerIndex >= config.committeeSize
      ) {
        return yield* failBuild(
          "DA signature witness is outside committee",
          `signer_index=${witness.signerIndex.toString()},committee_size=${config.committeeSize.toString()}`,
        );
      }
      if (
        signerIndexIsDaAttested(config.attestedSignersHex, witness.signerIndex)
      ) {
        return yield* failBuild(
          "DA signature witness is already attested",
          `signer_index=${witness.signerIndex.toString()}`,
        );
      }
      const byteIndex = Math.floor(witness.signerIndex / 8);
      const bitInByte = witness.signerIndex % 8;
      bytes[byteIndex] |= 1 << (7 - bitInByte);
    }
    const packedWitnesses = yield* encodeDaAttestationSignatureWitnesses(
      config.witnesses,
    );
    const attestedSigners = bytes.toString("hex");
    const attestationCount = yield* countDaAttestedSigners(attestedSigners);
    return {
      attestedSigners,
      attestationCount,
      packedWitnesses,
    };
  });

export const incompleteInitDaAttestationTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly target: DaAttestationStateQueueTarget;
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationMinting" | "stateQueueMinting"
    >;
    readonly attestationOutputLovelace: bigint;
    readonly rescueBeneficiary: AddressData;
    readonly availabilityCommitment: DaAvailabilityCommitment;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    assertCanonicalDaAvailabilityCommitment(config.availabilityCommitment);
    if (
      config.availabilityCommitment.header_hash !== config.target.headerHash
    ) {
      return yield* failBuild(
        "DA availability commitment header does not match state-queue target",
        `commitment=${config.availabilityCommitment.header_hash},target=${config.target.headerHash}`,
      );
    }
    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.target.headerHash,
    );
    const attestationDatum: DaAttestationDatum = {
      header_hash: config.target.headerHash,
      availability_commitment: config.availabilityCommitment,
      da_threshold: config.daParamsDatum.da_threshold,
      committee_signers_hash: config.daParamsDatum.committee_signers_hash,
      rescue_beneficiary: config.rescueBeneficiary,
      attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
      attestation_count: 0n,
    };
    const encodedAttestationDatum = Data.to(
      attestationDatum as never,
      DaAttestationDatum as never,
    );
    const initRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation init",
      );
      return Data.to(
        {
          Init: {
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.daAttestation.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedAttestationDatum) &&
                (output.assets[attestationUnit] ?? 0n) === 1n,
              "DA attestation init",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation init DA params",
            ),
            state_queue_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation init state queue",
            ),
            state_queue_mint_ref_script_input_index: requireReferenceInputIndex(
              ctx,
              config.referenceScripts.stateQueueMinting,
              "DA attestation init state_queue mint reference script",
            ),
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .readFrom([
        config.daParamsUtxo,
        config.target.stateQueueUtxo.utxo,
        config.referenceScripts.daAttestationMinting,
        config.referenceScripts.stateQueueMinting,
      ])
      .mintAssets({ [attestationUnit]: 1n }, initRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedAttestationDatum },
        {
          lovelace: config.attestationOutputLovelace,
          [attestationUnit]: 1n,
        },
      );
  });

export const incompleteAddDaAttestationSignaturesTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly attestation: DaAttestationUtxo;
    readonly witnesses: readonly DaAttestationSignatureWitness[];
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationSpending"
    >;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    if (
      config.attestation.datum.da_threshold !==
      config.daParamsDatum.da_threshold
    ) {
      return yield* failBuild(
        "DA attestation datum threshold does not match DA params",
        `attestation=${config.attestation.datum.da_threshold.toString()},params=${config.daParamsDatum.da_threshold.toString()}`,
      );
    }
    if (
      config.attestation.datum.committee_signers_hash !==
      config.daParamsDatum.committee_signers_hash
    ) {
      return yield* failBuild(
        "DA attestation datum committee hash does not match DA params",
        `attestation=${config.attestation.datum.committee_signers_hash},params=${config.daParamsDatum.committee_signers_hash}`,
      );
    }
    const committeeSize = yield* committeeSizeFromParamsDatum(
      config.daParamsDatum,
    );
    const applied = yield* applyDaAttestationSignatureWitnesses({
      attestedSignersHex: config.attestation.datum.attested_signers,
      witnesses: config.witnesses,
      committeeSize,
    });
    const updatedDatum: DaAttestationDatum = {
      ...config.attestation.datum,
      attested_signers: applied.attestedSigners,
      attestation_count: applied.attestationCount,
    };
    const encodedUpdatedDatum = Data.to(
      updatedDatum as never,
      DaAttestationDatum as never,
    );
    const addSignaturesRedeemer = ((ctx) =>
      Data.to(
        {
          AddSignatures: {
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.daAttestation.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedUpdatedDatum) &&
                assetsEqual(output.assets, config.attestation.utxo.assets),
              "DA attestation add-signatures",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation add-signatures DA params",
            ),
            signatures: applied.packedWitnesses,
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .readFrom([
        config.daParamsUtxo,
        config.referenceScripts.daAttestationSpending,
      ])
      .collectFrom([config.attestation.utxo], addSignaturesRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedUpdatedDatum },
        config.attestation.utxo.assets,
      );
  });

export const incompleteApplyDaAttestationToStateQueueTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<
    MidgardValidators,
    "availabilityChallenge" | "daAttestation" | "stateQueue"
  >,
  config: {
    readonly hubOracleRefInput: UTxO;
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly target: DaAttestationStateQueueTarget;
    readonly attestation: DaAttestationUtxo;
    readonly referenceScripts: DaAttestationReferenceScripts;
    readonly validityRange: {
      readonly validFrom: bigint;
      readonly validTo: bigint;
    };
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    if (
      config.validityRange.validTo < config.validityRange.validFrom ||
      config.validityRange.validTo - config.validityRange.validFrom >
        MAX_VALIDITY_RANGE_LENGTH_MS
    ) {
      return yield* failBuild(
        "DA attestation apply requires a short closed validity range",
        `valid_from=${config.validityRange.validFrom.toString()},valid_to=${config.validityRange.validTo.toString()},max_length=${MAX_VALIDITY_RANGE_LENGTH_MS.toString()}`,
      );
    }
    const attestationDeadline =
      config.target.stateQueueNode.header.endTime + DA_ATTESTATION_TIMEOUT_MS;
    if (config.validityRange.validTo > attestationDeadline) {
      return yield* failBuild(
        "DA attestation apply validity range exceeds the attestation deadline",
        `valid_to=${config.validityRange.validTo.toString()},deadline=${attestationDeadline.toString()}`,
      );
    }
    // Decision row D-DA4: committee rotation is retroactive, so apply now reads
    // the current governed params on-chain and requires the attestation's
    // frozen pair to still equal them. Refusing to build the transaction here
    // is not the enforcement — the validator is — but it turns a rotation into
    // a legible build error instead of a script failure at submission.
    //
    // The two branches are split for the diagnostic, not for the decision: they
    // are together exactly `daAttestationIsStranded`, so anything this refuses
    // to apply the rescue path can pick up. Nothing falls between them.
    if (
      config.attestation.datum.committee_signers_hash !==
      config.daParamsDatum.committee_signers_hash
    ) {
      return yield* failBuild(
        "DA committee rotated away from the attestation's frozen committee; this attestation can no longer apply and must be rescued",
        `frozen=${config.attestation.datum.committee_signers_hash},governed=${config.daParamsDatum.committee_signers_hash}`,
      );
    }
    if (
      config.attestation.datum.da_threshold !==
      config.daParamsDatum.da_threshold
    ) {
      return yield* failBuild(
        "DA threshold changed since the attestation froze it; this attestation can no longer apply and must be rescued",
        `frozen=${config.attestation.datum.da_threshold.toString()},governed=${config.daParamsDatum.da_threshold.toString()}`,
      );
    }
    if (config.attestation.datum.header_hash !== config.target.headerHash) {
      return yield* failBuild(
        "DA attestation header does not match state-queue target",
        `attestation=${config.attestation.datum.header_hash},target=${config.target.headerHash}`,
      );
    }
    if (
      config.attestation.datum.attestation_count <
      config.attestation.datum.da_threshold
    ) {
      return yield* failBuild(
        "DA attestation has not reached threshold",
        `attestation_count=${config.attestation.datum.attestation_count.toString()},threshold=${config.attestation.datum.da_threshold.toString()}`,
      );
    }
    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.target.headerHash,
    );
    const bondAssetName = daAvailabilityBondAssetName(
      outputReferenceFromUTxO(config.attestation.utxo),
    );
    const bondUnit = toUnit(
      contracts.availabilityChallenge.policyId,
      bondAssetName,
    );
    const encodedBondDatum = encodeDaAvailabilityBondDatum({
      Available: {
        commitment: config.attestation.datum.availability_commitment,
        da_bond_asset_name: bondAssetName,
        committee_signers_hash: config.attestation.datum.committee_signers_hash,
        attested_signers: config.attestation.datum.attested_signers,
      },
    });
    const updatedStateQueueDatum = encodeLinkedListNodeView({
      ...config.target.stateQueueUtxo.datum,
      data: castStateQueueNodeToData({
        header: config.target.stateQueueNode.header,
        da_attestation: {
          Attested: { da_bond_asset_name: bondAssetName },
        },
      }) as LinkedListNodeView["data"],
    });
    const daMintRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation apply mint",
      );
      return Data.to(
        {
          ApplyToStateQueue: {
            da_attestation_input_index: requireInputIndex(
              ctx,
              config.attestation.utxo,
              "DA attestation apply DA attestation",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation apply DA params",
            ),
            state_queue_input_index: requireInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            state_queue_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === contracts.stateQueue.spendingScriptAddress &&
                outputDatumCborMatches(output, updatedStateQueueDatum) &&
                assetsEqual(
                  output.assets,
                  config.target.stateQueueUtxo.utxo.assets,
                ),
              "DA attestation apply state queue",
            ),
            state_queue_mint_ref_script_input_index: requireReferenceInputIndex(
              ctx,
              config.referenceScripts.stateQueueMinting,
              "DA attestation apply state_queue mint reference script",
            ),
            availability_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.availabilityChallenge.policyId,
              "DA attestation apply availability bond mint",
            ),
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;
    const daSpendRedeemer = ((ctx) =>
      Data.to(
        {
          BurnForStateQueue: {
            mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;
    const availabilityMintRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.availabilityChallenge.policyId,
        "DA attestation apply availability bond mint",
      );
      return Data.to(
        {
          MintBondFromAttestation: {
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.hubOracleRefInput,
              "DA attestation apply hub oracle",
            ),
            da_attestation_input_index: requireInputIndex(
              ctx,
              config.attestation.utxo,
              "DA attestation apply DA attestation",
            ),
            da_attestation_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
            bond_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.availabilityChallenge.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedBondDatum) &&
                (output.assets.lovelace ?? 0n) ===
                  (config.attestation.utxo.assets.lovelace ?? 0n) &&
                (output.assets[bondUnit] ?? 0n) === 1n,
              "DA attestation apply availability bond",
            ),
            state_queue_input_index: requireInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            state_queue_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === contracts.stateQueue.spendingScriptAddress &&
                outputDatumCborMatches(output, updatedStateQueueDatum) &&
                assetsEqual(
                  output.assets,
                  config.target.stateQueueUtxo.utxo.assets,
                ),
              "DA attestation apply state queue",
            ),
          },
        } satisfies DaAvailabilityMintRedeemer as never,
        DaAvailabilityMintRedeemerSchema as never,
      );
    }) satisfies BuildTxWithRedeemer;
    const stateQueueSpendRedeemer = ((ctx) =>
      Data.to(
        {
          AttachDaAttestation: {
            state_queue_input_index: requireInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            da_attestation_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
          },
        } satisfies StateQueueSpendRedeemer as never,
        StateQueueSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .validFrom(Number(config.validityRange.validFrom))
      .validTo(Number(config.validityRange.validTo))
      .readFrom([
        config.hubOracleRefInput,
        config.daParamsUtxo,
        config.referenceScripts.availabilityChallengeMinting,
        config.referenceScripts.daAttestationMinting,
        config.referenceScripts.daAttestationSpending,
        config.referenceScripts.stateQueueMinting,
        config.referenceScripts.stateQueueSpending,
      ])
      .collectFrom([config.attestation.utxo], daSpendRedeemer)
      .collectFrom([config.target.stateQueueUtxo.utxo], stateQueueSpendRedeemer)
      .pay.ToContract(
        contracts.stateQueue.spendingScriptAddress,
        { kind: "inline", value: updatedStateQueueDatum },
        config.target.stateQueueUtxo.utxo.assets,
      )
      .pay.ToContract(
        contracts.availabilityChallenge.spendingScriptAddress,
        { kind: "inline", value: encodedBondDatum },
        {
          lovelace: config.attestation.utxo.assets.lovelace ?? 0n,
          [bondUnit]: 1n,
        },
      )
      .mintAssets({ [attestationUnit]: -1n }, daMintRedeemer)
      .mintAssets({ [bondUnit]: 1n }, availabilityMintRedeemer);
  });

/**
 * Refunds an attestation that a mid-flight committee rotation stranded
 * (decision row D-DA5 clause c).
 *
 * The transaction burns the DAAT and pays the attestation's entire remaining
 * value to one address. There is no state-queue leg: a stranded attestation is
 * by definition one that can never reach quorum, so there is nothing to attach.
 */
export const incompleteRescueStrandedDaAttestationTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly attestation: DaAttestationUtxo;
    readonly refundAddress: string;
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationMinting" | "daAttestationSpending"
    >;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    if (
      !daAttestationIsStranded({
        attestationDatum: config.attestation.datum,
        daParamsDatum: config.daParamsDatum,
      })
    ) {
      return yield* failBuild(
        "DA attestation is not stranded: its committee is still the governed one, so it may still be signed and applied",
        `frozen=${config.attestation.datum.committee_signers_hash},governed=${config.daParamsDatum.committee_signers_hash}`,
      );
    }
    if (
      config.refundAddress === contracts.daAttestation.spendingScriptAddress
    ) {
      return yield* failBuild(
        "DA attestation rescue refund may not return to the attestation script; the burnt DAAT would leave it unspendable",
        config.refundAddress,
      );
    }
    const refundAddressData = yield* addressDataFromBech32(
      config.refundAddress,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DaAttestationBuildError({
            message: "Failed to decode DA attestation rescue refund address",
            cause,
          }),
      ),
    );
    const encodedRefundAddress = Data.to(
      refundAddressData as never,
      AddressSchema as never,
    );
    const encodedBeneficiary = Data.to(
      config.attestation.datum.rescue_beneficiary as never,
      AddressSchema as never,
    );
    if (encodedRefundAddress !== encodedBeneficiary) {
      return yield* failBuild(
        "DA attestation rescue refund address does not match the frozen beneficiary",
        config.refundAddress,
      );
    }

    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.attestation.datum.header_hash,
    );
    const refundAssets = Object.fromEntries(
      Object.entries(config.attestation.utxo.assets).filter(
        ([unit]) => unit !== attestationUnit,
      ),
    );

    const rescueMintRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation rescue mint",
      );
      return Data.to(
        {
          RescueStrandedAttestation: {
            da_attestation_input_index: requireInputIndex(
              ctx,
              config.attestation.utxo,
              "DA attestation rescue DA attestation",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation rescue DA params",
            ),
            refund_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === config.refundAddress &&
                assetsEqual(output.assets, refundAssets),
              "DA attestation rescue refund",
            ),
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;

    const rescueSpendRedeemer = ((ctx) =>
      Data.to(
        {
          BurnForRescue: {
            mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation rescue DA attestation mint",
            ),
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .readFrom([
        config.daParamsUtxo,
        config.referenceScripts.daAttestationMinting,
        config.referenceScripts.daAttestationSpending,
      ])
      .collectFrom([config.attestation.utxo], rescueSpendRedeemer)
      .pay.ToAddress(config.refundAddress, refundAssets)
      .mintAssets({ [attestationUnit]: -1n }, rescueMintRedeemer);
  });

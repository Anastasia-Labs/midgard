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

import type {
  AuthenticatedValidator,
  GenericErrorFields,
  MidgardValidators,
} from "@/common.js";
import {
  castStateQueueNodeV1ToData,
  type HeaderHash,
  HeaderHashSchema,
  type StateQueueNodeV1,
} from "@/ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "@/linked-list.js";
import { StateQueueSpendRedeemer, type StateQueueUTxO } from "@/state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";
import { outputDatumCborMatches } from "@/tx-output-utils.js";

export const DA_PARAMS_ASSET_NAME = fromText("MIDGARD_DA_PARAMS");
export const DA_ATTESTATION_ASSET_NAME_PREFIX = fromText("DAAT");
export const EMPTY_ATTESTED_SIGNER_BITMAP =
  "0000000000000000000000000000000000000000000000000000000000000000";
export const DA_ATTESTATION_MESSAGE_PREFIX = "MidgardDAAttestationV1";
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
 * Smallest owner set the DA params governor will represent. A lone owner
 * cannot carry an `update_threshold` of at least two, so this constant is also
 * the owner-set drain protection.
 *
 * Source: `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md`
 * §4 (Q63, ACCEPTED).
 */
export const MIN_DA_OWNER_COUNT = 2;

/**
 * F04 §4 governed threshold floor: `max(2, ceil(2*setLength/3))`.
 *
 * TypeScript twin of `governed_threshold_floor` in
 * `onchain/aiken/validators/da-params-governor.ak`. Both evaluate the ceiling
 * as `(2*setLength + 2) / 3` under integer division so that the two languages
 * agree on every set size; `tests/da-governor-safety-v1.test.ts` pins the
 * shared vectors.
 */
export const governedThresholdFloor = (setLength: number): number => {
  if (!Number.isSafeInteger(setLength) || setLength < 0) {
    throw new Error(
      `governed threshold floor requires a non-negative integer set size, received ${String(setLength)}`,
    );
  }
  const twoThirdsCeiling = Math.floor((2 * setLength + 2) / 3);
  return twoThirdsCeiling > MIN_DA_OWNER_COUNT
    ? twoThirdsCeiling
    : MIN_DA_OWNER_COUNT;
};

/** One governed-bound violation class reported by {@link daParamsFloorViolations}. */
export type DaParamsFloorViolation =
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
  da_threshold: Data.Integer(),
  committee_signers_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
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
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      state_queue_mint_ref_script_input_index: Data.Integer(),
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
]);
export type DaAttestationSpendRedeemer = Data.Static<
  typeof DaAttestationSpendRedeemerSchema
>;
export const DaAttestationSpendRedeemer =
  DaAttestationSpendRedeemerSchema as unknown as DaAttestationSpendRedeemer;

export const daParamsUnit = (
  daParamsGovernor: AuthenticatedValidator,
): string => toUnit(daParamsGovernor.policyId, DA_PARAMS_ASSET_NAME);

export const daAttestationAssetName = (headerHash: string): string =>
  DA_ATTESTATION_ASSET_NAME_PREFIX + headerHash;

export const daAttestationUnit = (
  daAttestation: AuthenticatedValidator,
  headerHash: string,
): string => toUnit(daAttestation.policyId, daAttestationAssetName(headerHash));

export const daAttestationMessage = (headerHash: string): Buffer =>
  Buffer.concat([
    Buffer.from(DA_ATTESTATION_MESSAGE_PREFIX, "utf8"),
    Buffer.from(headerHash, "hex"),
  ]);

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
  readonly stateQueueMinting: UTxO;
  readonly stateQueueSpending: UTxO;
};

export type DaAttestationStateQueueTarget = {
  readonly stateQueueUtxo: StateQueueUTxO;
  readonly stateQueueNode: StateQueueNodeV1;
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
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.target.headerHash,
    );
    const attestationDatum: DaAttestationDatum = {
      header_hash: config.target.headerHash,
      da_threshold: config.daParamsDatum.da_threshold,
      committee_signers_hash: config.daParamsDatum.committee_signers_hash,
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
  contracts: Pick<MidgardValidators, "daAttestation" | "stateQueue">,
  config: {
    readonly target: DaAttestationStateQueueTarget;
    readonly attestation: DaAttestationUtxo;
    readonly referenceScripts: DaAttestationReferenceScripts;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
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
    const updatedStateQueueDatum = encodeLinkedListNodeView({
      ...config.target.stateQueueUtxo.datum,
      data: castStateQueueNodeV1ToData({
        header: config.target.stateQueueNode.header,
        da_attestation: contracts.daAttestation.policyId,
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
      .readFrom([
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
      .mintAssets({ [attestationUnit]: -1n }, daMintRedeemer);
  });

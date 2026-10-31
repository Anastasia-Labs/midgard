import { type UTxO } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { type GenericErrorFields } from "./common.js";
import {
  ATTESTED_SIGNER_BITMAP_HEX_LENGTH,
  DaAttestationDatum,
  DaParamsDatum,
  SIGNATURE_HEX_LENGTH,
  VERIFICATION_KEY_HEX_LENGTH,
} from "./da-attestation.da-attestation-is-stranded.js";
import { type HeaderHash, type StateQueueNode } from "./ledger-state.js";
import { type StateQueueUTxO } from "./state-queue.js";

/**
 * Why a DA attestation build refused. Every refusal in this module carries one
 * of these codes so a caller — and a test — can tell *which* precondition
 * failed instead of only that some precondition did.
 */
export type DaAttestationBuildFailureReason =
  | "attestation_header_mismatch"
  | "attestation_not_stranded"
  | "availability_commitment_header_mismatch"
  | "committee_rotated"
  | "duplicate_signature_witness"
  | "invalid_attested_signer_bitmap"
  | "invalid_committee_bytes"
  | "invalid_committee_size"
  | "invalid_signature_hex"
  | "invalid_signer_index"
  | "invalid_validity_range"
  | "missing_network"
  | "params_committee_hash_mismatch"
  | "params_threshold_mismatch"
  | "pool-under-backed"
  | "pool-unavailable"
  | "pool-withdrawing"
  | "rescue_refund_address_undecodable"
  | "rescue_refund_beneficiary_mismatch"
  | "rescue_refund_to_attestation_script"
  | "signer_already_attested"
  | "signer_outside_committee"
  | "threshold_changed"
  | "threshold_not_reached"
  | "validity_range_past_deadline";

export class DaAttestationBuildError extends EffectData.TaggedError(
  "DaAttestationBuildError",
)<GenericErrorFields & { readonly reason: DaAttestationBuildFailureReason }> {}

/**
 * The reference scripts the attestation builders read. Apply needs all four:
 * the DA attestation mint (DAAT burn) and spend (`BurnForStateQueue`), the
 * state-queue mint (authenticated through the reference-script auth policy,
 * `state_queue_mint_ref_script_input_index`) and the state-queue spend
 * (`AttachDaAttestation`). Apply mints nothing under the availability policy,
 * so no availability-challenge script is read.
 */
export type DaAttestationReferenceScripts = {
  readonly daAttestationMinting: UTxO;
  readonly daAttestationSpending: UTxO;
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

export const failBuild = (
  reason: DaAttestationBuildFailureReason,
  message: string,
  cause: unknown,
): Effect.Effect<never, DaAttestationBuildError> =>
  Effect.fail(new DaAttestationBuildError({ reason, message, cause }));

const isHexOfLength = (value: string, length: number): boolean =>
  value.length === length && /^[0-9a-fA-F]*$/.test(value);

const validateAttestedSignerBitmap = (
  attestedSignersHex: string,
): Effect.Effect<void, DaAttestationBuildError> =>
  isHexOfLength(attestedSignersHex, ATTESTED_SIGNER_BITMAP_HEX_LENGTH)
    ? Effect.void
    : failBuild(
        "invalid_attested_signer_bitmap",
        "Invalid DA attested-signer bitmap",
        `expected_hex_chars=${ATTESTED_SIGNER_BITMAP_HEX_LENGTH.toString()},actual_hex_chars=${attestedSignersHex.length.toString()}`,
      );

const validateSignerIndex = (
  signerIndex: number,
): Effect.Effect<void, DaAttestationBuildError> =>
  Number.isInteger(signerIndex) && signerIndex >= 0 && signerIndex <= 255
    ? Effect.void
    : failBuild(
        "invalid_signer_index",
        "Invalid DA signer index",
        `signer_index=${signerIndex.toString()}`,
      );

const validateSignatureHex = (
  signatureHex: string,
): Effect.Effect<void, DaAttestationBuildError> =>
  isHexOfLength(signatureHex, SIGNATURE_HEX_LENGTH)
    ? Effect.void
    : failBuild(
        "invalid_signature_hex",
        "Invalid DA signature witness",
        `expected_hex_chars=${SIGNATURE_HEX_LENGTH.toString()},actual_hex_chars=${signatureHex.length.toString()}`,
      );

const validateCommitteeSize = (
  committeeSize: number,
): Effect.Effect<void, DaAttestationBuildError> =>
  Number.isInteger(committeeSize) && committeeSize >= 0 && committeeSize <= 256
    ? Effect.void
    : failBuild(
        "invalid_committee_size",
        "Invalid DA committee size",
        `committee_size=${committeeSize.toString()}`,
      );

export const committeeSizeFromParamsDatum = (
  daParamsDatum: DaParamsDatum,
): Effect.Effect<number, DaAttestationBuildError> => {
  if (!/^[0-9a-fA-F]*$/.test(daParamsDatum.committee)) {
    return failBuild(
      "invalid_committee_bytes",
      "Invalid DA committee bytes",
      "committee is not hex",
    );
  }
  if (daParamsDatum.committee.length % VERIFICATION_KEY_HEX_LENGTH !== 0) {
    return failBuild(
      "invalid_committee_bytes",
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
          "duplicate_signature_witness",
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
          "signer_outside_committee",
          "DA signature witness is outside committee",
          `signer_index=${witness.signerIndex.toString()},committee_size=${config.committeeSize.toString()}`,
        );
      }
      if (
        signerIndexIsDaAttested(config.attestedSignersHex, witness.signerIndex)
      ) {
        return yield* failBuild(
          "signer_already_attested",
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

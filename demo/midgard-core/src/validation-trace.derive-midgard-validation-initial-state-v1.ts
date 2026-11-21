import { asBigInt, encodeCbor } from "./codec/cbor.js";
import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxCompact,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "./codec/forced.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  type MidgardNativeTxProofSource,
} from "./codec/native.js";
import {
  MIDGARD_CONSENSUS_PROFILE_ID,
  MIDGARD_VALIDATION_MACHINE_VERSION,
} from "./consensus-profile.js";
import { aikenSerialisedPlutusDataCbor } from "./plutus-data-cbor.js";
import {
  hashMidgardValidationContext,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "./validation-trace.decode-midgard-validation-machine-state.js";
import {
  hashMidgardValidationEventKey,
  type MidgardValidationMachineState,
  type MidgardValidationSourceKindName,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

const VALIDATION_CONTEXT_VERSION_V1 = 1n;

/** The header values the validation context commits to. */
export type MidgardValidationContextHeaderV1 = {
  /** `header.end_time`; a block with L2 transactions ends at its commit time. */
  readonly endTimeMs: bigint;
  readonly blockSlot: bigint;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
};

/** The canonical transaction bytes a source variant carries. */
export type MidgardValidationSourceTransactionV1 = {
  readonly canonicalTransactionCbor: Uint8Array;
};

/**
 * The twin of `validation_claim_v1.ValidationSourceMembershipV1`: exactly one
 * of a forced source (`ForcedValidationSource`, a forced submission) or a
 * normal one (`NormalValidationSource`, an L2 transaction). The state's source
 * kind follows from the variant, as in the claim; it is never an input.
 */
export type MidgardValidationSourceV1 =
  | {
      readonly forced: MidgardValidationSourceTransactionV1;
      readonly normal?: never;
    }
  | {
      readonly normal: MidgardValidationSourceTransactionV1;
      readonly forced?: never;
    };

export type MidgardValidationInitialStateInputV1 =
  MidgardValidationContextHeaderV1 & {
    readonly eventKeyCbor: Uint8Array;
    readonly source: MidgardValidationSourceV1;
    readonly preUtxosRoot: Uint8Array;
  };

type DerivedSourceV1 = {
  readonly sourceKind: MidgardValidationSourceKindName;
  readonly proofSource: MidgardNativeTxProofSource;
  readonly transactionId: Buffer;
  readonly transactionCommitment: Buffer;
};

/**
 * The source kind, triple, transaction id and commitment of whichever variant
 * `source` is; the forced variant takes the forced codec and commitment.
 */
const deriveSourceV1 = (source: MidgardValidationSourceV1): DerivedSourceV1 => {
  const { forced, normal } = source;
  if (forced !== undefined && normal === undefined) {
    const proofSource = deriveMidgardForcedTxProofSourceFromCanonicalCbor(
      forced.canonicalTransactionCbor,
    );
    return {
      sourceKind: "forced",
      proofSource,
      transactionId: computeMidgardNativeTxId(
        decodeMidgardForcedTxCompact(proofSource.compactCbor),
      ),
      transactionCommitment: computeMidgardForcedTxProofCommitment(proofSource),
    };
  }
  if (normal !== undefined && forced === undefined) {
    const proofSource = deriveMidgardNativeTxProofSourceFromCanonicalCbor(
      normal.canonicalTransactionCbor,
    );
    return {
      sourceKind: "normal",
      proofSource,
      transactionId: computeMidgardNativeTxId(
        decodeMidgardNativeTxCompact(proofSource.compactCbor),
      ),
      transactionCommitment: computeMidgardNativeTxProofCommitment(proofSource),
    };
  }
  throw new Error(
    "validation source must be exactly one of the forced or normal variants",
  );
};

/**
 * The twin of `validation_claim_v1.derive_validation_context_cbor`: the
 * Plutus Data CBOR of `[1, profile_id, end_time, expected_network_id,
 * min_fee_a, min_fee_b, block_slot]`, serialised as Aiken serialises it.
 */
export const deriveMidgardValidationContextCborV1 = (
  header: MidgardValidationContextHeaderV1,
): Buffer =>
  Buffer.from(
    aikenSerialisedPlutusDataCbor(
      encodeCbor([
        VALIDATION_CONTEXT_VERSION_V1,
        Buffer.from(MIDGARD_CONSENSUS_PROFILE_ID, "ascii"),
        asBigInt(header.endTimeMs, "end_time"),
        asBigInt(header.expectedNetworkId, "expected_network_id"),
        asBigInt(header.minFeeA, "min_fee_a"),
        asBigInt(header.minFeeB, "min_fee_b"),
        asBigInt(header.blockSlot, "block_slot"),
      ]).toString("hex"),
    ),
    "hex",
  );

/**
 * The twin of `validation_claim_v1.derive_initial_state`: state 0 of a
 * transaction's validation trace, built from the header context, the event
 * key, the source variant and the step's pre-state root alone.
 */
export const deriveMidgardValidationInitialStateV1 = (
  input: MidgardValidationInitialStateInputV1,
): MidgardValidationMachineState => {
  const contextCbor = deriveMidgardValidationContextCborV1(input);
  const { sourceKind, proofSource, transactionId, transactionCommitment } =
    deriveSourceV1(input.source);
  return {
    machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
    eventKeyHash: hashMidgardValidationEventKey(input.eventKeyCbor),
    transactionId: ensureHash32(transactionId, "transaction_id"),
    transactionCommitment: ensureHash32(
      transactionCommitment,
      "transaction_commitment",
    ),
    validationContextHash: hashMidgardValidationContext(contextCbor),
    sourceKind,
    priorLedgerRoot: ensureHash32(
      Buffer.from(input.preUtxosRoot),
      "prior_ledger_root",
    ),
    phase: "canonicalDecode",
    programCounter: 0,
    // The first work witness: the field scan at field 0, item 0, chunk 0,
    // before any item has been counted.
    workRoot: hashMidgardValidationWorkWitness({
      phase: "canonicalDecode",
      programCounter: 0,
      witnessCbor: encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        0n,
        0n,
        0n,
        -1n,
        0n,
      ]),
    }),
    executionCpu: 0n,
    executionMemory: 0n,
    verdict: "pending",
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  };
};

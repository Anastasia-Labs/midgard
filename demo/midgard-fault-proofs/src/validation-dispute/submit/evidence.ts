import { asArray, computeHash32, decodeSingleCbor } from "@al-ft/midgard-core";
import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { type MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import {
  ValidationAuxiliaryWitness,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema,
  type ValidationCekMaterialRoute,
  ValidationCekMaterialRouteSchema,
  ValidationOneStepWitness,
  ValidationPrepareSelectedSpendRedeemerSchema,
} from "@al-ft/midgard-sdk";
import {
  type CekProgramMaterialNecessityReceiptSet,
  type CekRouteMaterial,
  parseCekProgramMaterialNecessityReceiptSet,
  validateCekRouteMaterial,
} from "@al-ft/midgard-validation";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";

import { MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES } from "./validity.js";

const VALIDATION_ONE_STEP_EVIDENCE_DOMAIN = Buffer.from(
  "MidgardValidationOneStepEvidenceV1",
  "ascii",
);

export type PlutusDataValue = Data;
type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

type RuntimeSchemaEncoder = (
  data: unknown,
  schema: PlutusDataSchema,
) => ReturnType<typeof Data.to>;

/*
 * Lucid's runtime encoder accepts a TypeBox schema as its second argument, but
 * its public generic types model that argument as the decoded static value.
 * Expanding Exact<T> for the 42-variant auxiliary witness exceeds TypeScript's
 * instantiation limit. Keep each caller's value exactly typed and isolate that
 * declaration mismatch at this runtime-schema boundary.
 */
export const encodeWithRuntimeSchema =
  Data.to as unknown as RuntimeSchemaEncoder;
const validationCekMaterialRouteRuntimeSchema = asDataType<PlutusDataSchema>(
  ValidationCekMaterialRouteSchema,
);
export const validationPrepareSelectedSpendRedeemerRuntimeSchema =
  asDataType<PlutusDataSchema>(ValidationPrepareSelectedSpendRedeemerSchema);
export const validationCanonicalDecodePrepareSelectedSpendRedeemerRuntimeSchema =
  asDataType<PlutusDataSchema>(
    ValidationCanonicalDecodePrepareSelectedSpendRedeemerSchema,
  );

/**
 * The `material_route` field of the CEK execution-selection semantic action
 * (`cek_execution_selection_semantic_v1.VerifyExecutionSelection`), as Plutus
 * data. The route is resolver evidence — it names the consuming transaction's
 * own reference inputs — so it is never part of the committed evidence hash.
 */
export const validationCekMaterialRouteData = (
  route: ValidationCekMaterialRoute,
): PlutusDataValue =>
  Data.from(
    encodeWithRuntimeSchema(route, validationCekMaterialRouteRuntimeSchema),
  );

export type ValidationOneStepSubmissionArgument = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: Uint8Array;
  readonly auxiliaryCbor: Uint8Array;
  readonly cekRouteMaterial?: CekRouteMaterial;
  /** Presence selects the receipt-justified incremental route. */
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSet;
  /** Exact adjacent canonical replay bytes; include in durable evidence identity. */
  readonly cekContextSuccessorWorkWitnessCbor?: Uint8Array;
  readonly ledgerOutputProofSuccessorWorkWitnessCbor?: Uint8Array;
};

export type ValidatedCekSubmissionEvidence = {
  readonly cekContextSuccessorWorkWitnessCbor?: Uint8Array;
  readonly cekRouteMaterial?: CekRouteMaterial;
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSet;
};

/**
 * The §8 carriage material for a tiers-2/3 one-step argument: the producer's
 * carriage plan together with the ledger UTxOs that hold its published bytes
 * (#600 ruling D3-A, re-scoped by #619/#621).
 *
 * A tier-1 argument needs none of this: `Inline` carries its own bytes and
 * indexes nothing. Above §8.3's 14,336-byte cap the carriage is *only*
 * positional reference-input indices, so a submitter has to hand the builder
 * the material those indices will name — otherwise the door-running
 * transaction cannot reference the carriage at all.
 *
 * `plan` is the producer's own `planMidgardFieldCarriageV1` output, never a
 * hand-assembled record. Since Option B the committed evidence is
 * transition-only, so no index is frozen anywhere on chain: the builder
 * resolves the plan by content (§8.7) against the door transaction's own
 * reference-input set at build time and puts *that* carriage on the wire —
 * the plan is the content source the resolution works from.
 */
export type ValidationFieldCarriageMaterial = {
  readonly plan: MidgardFieldCarriagePlan;
  /**
   * The carriage UTxOs the door transaction must read: the plan's publications
   * under tier 2, and the publications plus the §8.6 certificate under tier 3.
   */
  readonly referenceUtxos: readonly UTxO[];
  /** Required at tier 3; the §8.6 minting policy the door is parameterised by. */
  readonly certificatePolicyId?: string;
};

/**
 * The staged auxiliary's carriage, read back out as the
 * `MidgardFieldCarriage` the SDK resolvers speak.
 *
 * Constructor order is the frozen §8.1 one — `Inline` 0, `RawUtxo` 1,
 * `Certified` 2 — and every arm is checked for arity rather than
 * pattern-matched loosely. Since Option B the evidence hash no longer commits
 * this value; what a misread would silently corrupt is the §8.4 tier the
 * builder routes by and, at tier 1, the preimage bytes the delivery carries.
 */
export const midgardFieldCarriageFromData = (
  value: PlutusDataValue,
  label: string,
): MidgardFieldCarriage => {
  if (!(value instanceof Constr)) {
    throw new Error(`${label} is not a §8 FieldCarriageV1 constructor`);
  }
  if (value.index === 0 && value.fields.length === 1) {
    const preimage = value.fields[0];
    if (typeof preimage !== "string" || preimage.length % 2 !== 0) {
      throw new Error(`${label} tier-1 Inline bytes are malformed`);
    }
    return { carriage: "Inline", preimage: Buffer.from(preimage, "hex") };
  }
  if (value.index === 1 && value.fields.length === 1) {
    const refInputIndex = value.fields[0];
    if (typeof refInputIndex !== "bigint" || refInputIndex < 0n) {
      throw new Error(`${label} tier-2 reference-input index is malformed`);
    }
    return { carriage: "RawUtxo", refInputIndex: Number(refInputIndex) };
  }
  if (value.index === 2 && value.fields.length === 2) {
    const certRefInputIndex = value.fields[0];
    const chunkRefInputIndices = value.fields[1];
    if (typeof certRefInputIndex !== "bigint" || certRefInputIndex < 0n) {
      throw new Error(`${label} tier-3 certificate index is malformed`);
    }
    if (!Array.isArray(chunkRefInputIndices)) {
      throw new Error(`${label} tier-3 chunk index vector is malformed`);
    }
    return {
      carriage: "Certified",
      certRefInputIndex: Number(certRefInputIndex),
      chunkRefInputIndices: chunkRefInputIndices.map((index) => {
        if (typeof index !== "bigint" || index < 0n) {
          throw new Error(`${label} tier-3 chunk index is malformed`);
        }
        return Number(index);
      }),
    };
  }
  throw new Error(`${label} is not a §8 FieldCarriageV1 constructor`);
};

/**
 * Encodes a resolved `MidgardFieldCarriage` back onto the frozen §8.1 wire —
 * `Inline` 0, `RawUtxo` 1, `Certified` 2, mirroring
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak:168` — for the
 * observe redeemer. Since Option B the carriage on the wire is the one the
 * builder resolved against the door transaction's own reference inputs at
 * build time (#621), not a value replayed from the staged auxiliary, so the
 * encoder is the inverse of {@link midgardFieldCarriageFromData} above.
 */
export const midgardFieldCarriageToData = (
  carriage: MidgardFieldCarriage,
): PlutusDataValue => {
  switch (carriage.carriage) {
    case "Inline":
      return new Constr(0, [carriage.preimage.toString("hex")]);
    case "RawUtxo":
      return new Constr(1, [BigInt(carriage.refInputIndex)]);
    case "Certified":
      return new Constr(2, [
        BigInt(carriage.certRefInputIndex),
        carriage.chunkRefInputIndices.map((index) => BigInt(index)),
      ]);
  }
};

export const exactPlutusDataFromCbor = (
  value: Uint8Array,
  label: string,
): PlutusDataValue => {
  const bytes = Buffer.from(value);
  if (
    bytes.length === 0 ||
    bytes.length >= MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `${label} must be non-empty and strictly below the L1 proof envelope`,
    );
  }
  const decoded = Data.from(bytes.toString("hex"));
  const encoded = Buffer.from(Data.to(decoded), "hex");
  if (!encoded.equals(bytes)) {
    throw new Error(`${label} is not exact canonical V1 Plutus Data`);
  }
  return decoded;
};

/**
 * Enforces the selection-only C28 evidence ABI before any transaction builder
 * uses it. A Plutus/Midgard CEK selection (resolver 11, semantic resolver 1 —
 * `cek_execution_selection_semantic_v1`) must carry complete route material;
 * later CEK steps, ValueAndMint, and every other staged phase must not carry
 * it.
 */
export const validateCekSubmissionEvidence = (
  argument: ValidationOneStepSubmissionArgument,
): ValidatedCekSubmissionEvidence => {
  exactPlutusDataFromCbor(
    argument.auxiliaryCbor,
    "validation auxiliary witness",
  );
  const auxiliaryWitness = Data.from(
    Buffer.from(argument.auxiliaryCbor).toString("hex"),
    ValidationAuxiliaryWitness,
  );
  const selection =
    typeof auxiliaryWitness === "object" &&
    auxiliaryWitness !== null &&
    "NativeExecutionScanWitness" in auxiliaryWitness
      ? auxiliaryWitness.NativeExecutionScanWitness
      : undefined;
  const contextSuccessor = argument.cekContextSuccessorWorkWitnessCbor;
  if (contextSuccessor !== undefined) {
    if (argument.resolverIndex !== 11 || argument.semanticResolverIndex !== 2) {
      throw new Error(
        "CEK context successor bytes require the context semantic resolver",
      );
    }
    exactPlutusDataFromCbor(argument.transitionCbor, "validation transition");
    const transition = Data.from(
      Buffer.from(argument.transitionCbor).toString("hex"),
      ValidationOneStepWitness,
    );
    const successor = transition.claimed_successor;
    const programCounter = Number(successor.program_counter);
    if (
      successor.phase !== "Cek" ||
      !Number.isSafeInteger(programCounter) ||
      programCounter < 0 ||
      hashMidgardValidationWorkWitness({
        phase: "cek",
        programCounter,
        witnessCbor: contextSuccessor,
      }).toString("hex") !== successor.work_root
    ) {
      throw new Error(
        "CEK context successor witness does not match frozen successor work root",
      );
    }
    if (
      asArray(
        decodeSingleCbor(contextSuccessor),
        "CEK context successor witness",
      ).length !== 9
    ) {
      throw new Error(
        "CEK context successor witness must have exactly nine fields",
      );
    }
  }
  const isProgramSelection =
    argument.resolverIndex === 11 &&
    argument.semanticResolverIndex === 1 &&
    selection !== undefined &&
    (selection.language_tag === 3n || selection.language_tag === 128n);
  if (!isProgramSelection) {
    if (
      argument.cekRouteMaterial !== undefined ||
      argument.cekIncrementalNecessityReceiptSet !== undefined
    ) {
      throw new Error(
        "CEK route material and necessity receipts are permitted only for an exact program-selection witness",
      );
    }
    return Object.freeze(
      contextSuccessor === undefined
        ? {}
        : { cekContextSuccessorWorkWitnessCbor: Buffer.from(contextSuccessor) },
    );
  }
  if (argument.cekRouteMaterial === undefined) {
    throw new Error(
      "Plutus/Midgard CEK selection requires complete route material",
    );
  }
  const cekRouteMaterial = validateCekRouteMaterial({
    value: argument.cekRouteMaterial,
    firstSourceChunk: Buffer.from(selection.first_chunk_proof.chunk, "hex"),
    languageTag: Number(selection.language_tag) as 3 | 128,
  });
  if (argument.cekIncrementalNecessityReceiptSet === undefined) {
    return Object.freeze({ cekRouteMaterial });
  }
  const cekIncrementalNecessityReceiptSet =
    parseCekProgramMaterialNecessityReceiptSet(
      argument.cekIncrementalNecessityReceiptSet,
    );
  if (
    cekIncrementalNecessityReceiptSet.programEnvelopeHash !==
    cekRouteMaterial.programEnvelopeHash.toString("hex")
  ) {
    throw new Error(
      "CEK incremental necessity receipts are bound to another program envelope",
    );
  }
  return Object.freeze({
    cekRouteMaterial,
    cekIncrementalNecessityReceiptSet,
  });
};

export const requireConstr = ({
  value,
  index,
  fields,
  label,
}: {
  readonly value: PlutusDataValue;
  readonly index: number;
  readonly fields: number;
  readonly label: string;
}): Constr<PlutusDataValue> => {
  if (
    !(value instanceof Constr) ||
    value.index !== index ||
    value.fields.length !== fields
  ) {
    throw new Error(
      `${label} must be constructor ${index.toString()} with ${fields.toString()} fields`,
    );
  }
  return value;
};

export const validationOneStepEvidenceHashFromData = (
  transition: PlutusDataValue,
  auxiliary: PlutusDataValue,
): string => {
  const evidencePayload = Buffer.from(Data.to([transition, auxiliary]), "hex");
  return computeHash32(
    Buffer.concat([VALIDATION_ONE_STEP_EVIDENCE_DOMAIN, evidencePayload]),
  ).toString("hex");
};

export const validationOneStepEvidenceHash = ({
  transitionCbor,
  auxiliaryCbor,
}: Pick<
  ValidationOneStepSubmissionArgument,
  "transitionCbor" | "auxiliaryCbor"
>): string =>
  validationOneStepEvidenceHashFromData(
    exactPlutusDataFromCbor(transitionCbor, "validation transition"),
    exactPlutusDataFromCbor(auxiliaryCbor, "validation auxiliary witness"),
  );

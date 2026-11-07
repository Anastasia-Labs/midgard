/** A field of the disputed transaction a move of ours reads by reference
 * rather than inline: our publication of it, or, past one publication's
 * bytes, its publications and their certificate. A move names them by their
 * positions among its own reference inputs, in ledger order. */
import {
  decodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  type MidgardFieldCarriage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import type { ValidationMachineFieldCarriageResolver } from "@al-ft/midgard-validation";
import {
  Constr,
  Data,
  type OutRef,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

/** A field a move of ours reads by reference: its index and its bytes. */
export type ValidationTraceCarriedField = Readonly<{
  fieldIndex: number;
  preimage: Buffer;
}>;

/** The field-preimage certificate policy the dispute's validators are
 * applied with, and its published minting script: a field of ours longer
 * than one publication is certified under it. Without it a step that reads
 * such a field by reference waits. */
export type ValidationTraceFieldPreimageCertificate = Readonly<{
  policyId: string;
  mintingScript: Script;
  reference: UTxO;
}>;

/** Our published carriage of a field: its publications, in chunk order, and
 * their certificate past one publication. */
export type ValidationTraceFieldCarriage = Readonly<{
  publications: readonly UTxO[];
  certificate?: UTxO;
}>;

/** The outputs a move reads to carry `carriage`. */
export const carriageReads = ({
  publications,
  certificate,
}: ValidationTraceFieldCarriage): readonly UTxO[] =>
  certificate === undefined ? publications : [...publications, certificate];

const label = ({ txHash, outputIndex }: OutRef) =>
  `${txHash}#${outputIndex.toString()}`;

/**
 * The carriage naming `carriage` among `references`, the reference inputs of
 * the move that reads it (ledger order is the out-refs' order, duplicates
 * once): tier 2 for one publication, tier 3 for publications under a
 * certificate. Throws unless each output it names is one of them.
 */
export const carriageAmong = (
  carriage: ValidationTraceFieldCarriage,
  references: readonly OutRef[],
): MidgardFieldCarriage => {
  const ordered = [...references]
    .sort(compareOutRefs)
    .map(label)
    .filter((outRef, at, all) => at === 0 || all[at - 1] !== outRef);
  const indexOf = (utxo: UTxO) => {
    const index = ordered.indexOf(label(utxo));
    if (index === -1)
      throw new Error(
        `validation-trace field carriage ${label(utxo)} is not among the move's reference inputs`,
      );
    return index;
  };
  const { publications, certificate } = carriage;
  if (certificate === undefined) {
    if (publications.length !== 1)
      throw new Error(
        "validation-trace field carriage over several publications has no certificate",
      );
    return { carriage: "RawUtxo", refInputIndex: indexOf(publications[0]!) };
  }
  return {
    carriage: "Certified",
    certRefInputIndex: indexOf(certificate),
    chunkRefInputIndices: publications.map(indexOf),
  };
};

/** `carriage` as the CBOR of its `FieldCarriageV1`. */
export const carriageCbor = (carriage: MidgardFieldCarriage): string =>
  Data.to(
    carriage.carriage === "Inline"
      ? new Constr(0, [carriage.preimage.toString("hex")])
      : carriage.carriage === "RawUtxo"
        ? new Constr(1, [BigInt(carriage.refInputIndex)])
        : new Constr(2, [
            BigInt(carriage.certRefInputIndex),
            carriage.chunkRefInputIndices.map(BigInt),
          ]),
  );

/** The number of publications §8 splits a field of `length` bytes into. */
const chunkCount = (length: number) =>
  Math.ceil(length / MIDGARD_CHUNK_BYTES_K);

/** The longest preimage of a field a resolver carries inline: one bound
 * for every field, or each field's own (none: §8.4's tier-1 bound). */
export type ValidationTraceInlineLimit =
  | number
  | ((field: ValidationTraceCarriedField) => number | undefined)
  | undefined;

/**
 * A resolver that carries a field inline within `inlineMaxPreimageBytes`,
 * and past it names our carriage at placeholder positions, recording each
 * field so carried in `carried`: the evidence's shape before the move that
 * reads it is built.
 */
export const placeholderCarriageResolver =
  (
    carried: ValidationTraceCarriedField[],
    inlineMaxPreimageBytes?: ValidationTraceInlineLimit,
  ): ValidationMachineFieldCarriageResolver =>
  ({ fieldIndex, fieldPreimage }) => {
    const field = { fieldIndex, preimage: Buffer.from(fieldPreimage) };
    const { preimage } = field;
    const tier = selectMidgardFieldCarriageTier(preimage.length);
    const max =
      typeof inlineMaxPreimageBytes === "function"
        ? inlineMaxPreimageBytes(field)
        : inlineMaxPreimageBytes;
    if (tier === "Inline" && (max === undefined || preimage.length <= max))
      return { carriage: "Inline", preimage };
    carried.push(field);
    return tier === "Certified"
      ? {
          carriage: "Certified",
          certRefInputIndex: 0,
          chunkRefInputIndices: Array.from(
            { length: chunkCount(preimage.length) },
            () => 0,
          ),
        }
      : { carriage: "RawUtxo", refInputIndex: 0 };
  };

/** A resolver that names `carriage` for the field `field`, and refuses
 * any other field. */
export const fixedCarriageResolver =
  (
    field: ValidationTraceCarriedField,
    carriage: MidgardFieldCarriage,
  ): ValidationMachineFieldCarriageResolver =>
  ({ fieldIndex, fieldPreimage }) => {
    if (
      fieldIndex !== field.fieldIndex ||
      !field.preimage.equals(Buffer.from(fieldPreimage))
    )
      throw new Error(
        `validation-trace evidence reads field ${fieldIndex.toString()}, not the field its carriage carries`,
      );
    return carriage;
  };

/**
 * The one-step argument `build` builds with a carriage resolver, carrying
 * by reference, at placeholder positions, each field it reads past the
 * inline limit `inlineMax` names for it in that argument (none: §8.4's
 * tier-1 bound); and the one field so carried, if any. Throws for a step
 * that carries more than one.
 */
export const carriedArgument = <A>(
  build: (resolveFieldCarriage: ValidationMachineFieldCarriageResolver) => A,
  inlineMax: (
    argument: A,
    field: ValidationTraceCarriedField,
  ) => number | undefined,
): Readonly<{
  argument: A;
  field: ValidationTraceCarriedField | undefined;
}> => {
  const read: ValidationTraceCarriedField[] = [];
  const probe = build(placeholderCarriageResolver(read, 0));
  if (read.length === 0) return { argument: probe, field: undefined };
  const carried: ValidationTraceCarriedField[] = [];
  const argument = build(
    placeholderCarriageResolver(carried, (field) => inlineMax(probe, field)),
  );
  if (carried.length > 1)
    throw new Error(
      "validation-trace step carries more than one field by reference",
    );
  return { argument, field: carried[0] };
};

/** The required observer at `index` of a required-observer field whose
 * preimage is `preimage`, and how many the field holds. */
export const requiredObserverAt = (
  preimage: Buffer,
  index: number,
): Readonly<{ hash: string; count: number }> => {
  const items = decodeMidgardFieldPreimage(preimage);
  const item = items[index];
  if (item === undefined)
    throw new Error("validation-trace observer is not in its field");
  return { hash: Buffer.from(item).toString("hex"), count: items.length };
};

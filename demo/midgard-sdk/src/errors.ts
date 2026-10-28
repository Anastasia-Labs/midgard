import { Data as EffectData } from "effect";

export type GenericErrorFields = {
  readonly message: string;
  // Effect's tagged-error unions currently rely on an open cause channel.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly cause: any;
};

export class AssetError extends EffectData.TaggedError(
  "AssetError",
)<GenericErrorFields> {}

export class Bech32DeserializationError extends EffectData.TaggedError(
  "Bech32DeserializationError",
)<GenericErrorFields> {}

export class CborSerializationError extends EffectData.TaggedError(
  "CborSerializationError",
)<GenericErrorFields> {}

export class CborDeserializationError extends EffectData.TaggedError(
  "CborDeserializationError",
)<GenericErrorFields> {}

export class CmlUnexpectedError extends EffectData.TaggedError(
  "CmlUnexpectedError",
)<GenericErrorFields> {}

export class CmlDeserializationError extends EffectData.TaggedError(
  "CmlDeserializationError",
)<GenericErrorFields> {}

export class DataCoercionError extends EffectData.TaggedError(
  "DataCoercionError",
)<GenericErrorFields> {}

export class HashingError extends EffectData.TaggedError(
  "HashingError",
)<GenericErrorFields> {}

export class LucidError extends EffectData.TaggedError(
  "LucidError",
)<GenericErrorFields> {}

export class MissingDatumError extends EffectData.TaggedError(
  "MissingDatumError",
)<GenericErrorFields> {}

export class UnauthenticUtxoError extends EffectData.TaggedError(
  "UnauthenticUtxoError",
)<GenericErrorFields> {}

export class UnspecifiedNetworkError extends EffectData.TaggedError(
  "UnspecifiedNetworkError",
)<GenericErrorFields> {}

/**
 * A caller- or provider-supplied UTxO that is not a well-formed ledger output.
 * `role` names where it came from (a config field such as `walletInputs[2]`,
 * or `wallet[0]` for the selected wallet's provider view) and `outRef` its
 * `<txHash>#<outputIndex>` when those two fields are readable.
 */
export class InvalidUtxoError extends EffectData.TaggedError(
  "InvalidUtxoError",
)<
  GenericErrorFields & {
    readonly role: string;
    readonly outRef: string | null;
  }
> {}

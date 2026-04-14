import { Data as EffectData } from "effect";

export type GenericErrorFields = {
  readonly message: string;
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

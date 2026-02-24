export type DecodeMode = "strict" | "dual";
export type EncodeMode = "strict" | "legacy";

export type Hash32 = Buffer;

export type MidgardTransactionFull = {
  readonly transactionBodyCbor: Buffer;
  readonly transactionWitnessSetCbor: Buffer;
  readonly isValid: boolean;
  readonly auxiliaryData: null;
};

export type MidgardTransactionCompact = {
  readonly transactionBodyHash: Hash32;
  readonly transactionWitnessSetHash: Hash32;
  readonly isValid: boolean;
};

export type MidgardTransactionCompactLegacy = {
  readonly body: Hash32;
  readonly wits: Hash32;
  readonly validity: boolean | number | string;
};

export type MidgardTransactionBodyCompact = {
  readonly inputsHash: Hash32;
  readonly outputsHash: Hash32;
  readonly fee: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly auxiliaryDataHash?: Hash32;
  readonly validityIntervalStart?: bigint;
  readonly mintHash?: Hash32;
  readonly scriptDataHash?: Hash32;
  readonly requiredSignersHash?: Hash32;
  readonly networkId?: bigint;
  readonly referenceInputsHash?: Hash32;
  readonly requiredObserversHash?: Hash32;
};

export type MidgardTransactionWitnessSetCompact = {
  readonly vkeyWitnessesHash?: Hash32;
  readonly nativeScriptsHash?: Hash32;
  readonly redeemersHash?: Hash32;
  readonly plutusV3ScriptsHash?: Hash32;
};

export type CardanoToMidgardOptions = {
  readonly strictAuxiliaryDataNull?: boolean;
};

export type MidgardCodecRoundTrip = {
  readonly original: Buffer;
  readonly normalized: Buffer;
};

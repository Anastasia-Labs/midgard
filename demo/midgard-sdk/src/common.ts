import {
  Data,
  getAddressDetails,
  Script,
  ScriptHash,
} from "@lucid-evolution/lucid";
import {
  Data as EffectData,
  Array as EffectArray,
  Effect,
  Option,
} from "effect";
import {
  Address,
  Assets as LucidAssets,
  Credential,
  LucidEvolution,
  PolicyId,
  UTxO,
  fromHex,
  fromUnit,
  toHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { ActiveOperatorUTxO } from "./active-operators.js";
import { RetiredOperatorUTxO } from "./retired-operators.js";

export const makeReturn = <A, E>(program: Effect.Effect<A, E>) => {
  return {
    unsafeRun: () => Effect.runPromise(program),
    safeRun: () => Effect.runPromise(Effect.either(program)),
    program: () => program,
  };
};

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

export const getSingleAssetApartFromAda = (
  assets: LucidAssets,
): Effect.Effect<[PolicyId, string, bigint], AssetError> =>
  Effect.gen(function* () {
    const flattenedAssets: [string, bigint][] = Object.entries(assets);
    const woLovelace: [string, bigint][] = flattenedAssets.filter(
      ([unit, _qty]) => !(unit === "" || unit === "lovelace"),
    );
    if (woLovelace.length === 1) {
      const explodedUnit = fromUnit(woLovelace[0][0]);
      return [
        explodedUnit.policyId,
        explodedUnit.assetName ?? "",
        woLovelace[0][1],
      ];
    } else {
      return yield* Effect.fail(
        new AssetError({
          message: "Failed to get single asset apart from ADA",
          cause: "Expected exactly 1 additional asset apart from ADA",
        }),
      );
    }
  });

/**
 * Similar to `getSingleAssetApartFromAda`, with the additional requirement for
 * the quantity to be exactly 1.
 */
export const getStateToken = (
  assets: LucidAssets,
): Effect.Effect<[PolicyId, string], UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to get the beacon token from assets";
    const [policyId, assetName, qty] = yield* getSingleAssetApartFromAda(
      assets,
    ).pipe(
      Effect.mapError(
        (e) =>
          new UnauthenticUtxoError({
            message: errorMessage,
            cause: e,
          }),
      ),
    );
    if (qty !== 1n) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: errorMessage,
          cause: `The quantity of the beacon token was expected to be exactly 1, but it was ${qty.toString()}`,
        }),
      );
    }
    return [policyId, assetName];
  });

/**
 * Silently drops the UTxOs without proper authentication NFTs.
 */
export const utxosAtByNFTPolicyId = (
  lucid: LucidEvolution,
  addressOrCred: Address | Credential,
  policyId: PolicyId,
): Effect.Effect<UTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(addressOrCred),
      catch: (e) => {
        return new LucidError({
          message: `Failed to fetch UTxOs at: ${addressOrCred}`,
          cause: e,
        });
      },
    });
    const nftEffects: Effect.Effect<UTxO, UnauthenticUtxoError>[] =
      allUTxOs.map((u: UTxO) => {
        const nftsEffect = getStateToken(u.assets);
        return Effect.andThen(
          nftsEffect,
          ([sym, _tn]): Effect.Effect<UTxO, UnauthenticUtxoError> => {
            if (sym === policyId) {
              return Effect.succeed(u);
            } else {
              return Effect.fail(
                new UnauthenticUtxoError({
                  message: "Failed to get assets from fetched UTxOs",
                  cause: "UTxO doesn't have the expected NFT policy ID",
                }),
              );
            }
          },
        );
      });
    const authenticUTxOs = yield* Effect.allSuccesses(nftEffects);
    return authenticUTxOs;
  }).pipe(
    Effect.catchAllDefect(
      (d) =>
        new LucidError({
          message: `Unexpected error while fetching UTxOs at: ${addressOrCred}`,
          cause: d,
        }),
    ),
  );

// TODO: Might be good to define an `EventUTxO` type.
export const isEventUTxOInclusionTimeInBounds = (
  eventUTxO: { datum: { inclusionTime: bigint } },
  inclusionTimeLowerBound?: POSIXTime,
  inclusionTimeUpperBound?: POSIXTime,
): boolean => {
  const eventDatum = eventUTxO.datum;

  const biggerThanLower =
    inclusionTimeLowerBound === undefined ||
    inclusionTimeLowerBound < eventDatum.inclusionTime;
  const smallerThanUpper =
    inclusionTimeUpperBound === undefined ||
    eventDatum.inclusionTime <= inclusionTimeUpperBound;

  return biggerThanLower && smallerThanUpper;
};

const blake2bHelper = (
  msg: string,
  dkLen: number,
  functionName: string,
): Effect.Effect<string, HashingError> => {
  const errorMessage = `Failed to hash using ${functionName} function`;
  if (isHexString(msg)) {
    try {
      return Effect.succeed(toHex(blake2b(fromHex(msg), { dkLen })));
    } catch (e) {
      return Effect.fail(
        new HashingError({
          message: errorMessage,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new HashingError({
        message: errorMessage,
        cause: `Invalid message provided`,
      }),
    );
  }
};

export const hashHexWithBlake2b224 = (
  msg: string,
): Effect.Effect<string, HashingError> => blake2bHelper(msg, 28, "Blake2b224");

export const hashHexWithBlake2b256 = (
  msg: string,
): Effect.Effect<string, HashingError> => blake2bHelper(msg, 32, "Blake2b256");

export const bufferToHex = (buf: Buffer): string => {
  try {
    return buf.toString("hex");
  } catch (_) {
    return "<no hex for undefined>";
  }
};

export const H32Schema = Data.Bytes({ minLength: 32, maxLength: 32 });
export type H32 = Data.Static<typeof H32Schema>;
export const H32 = H32Schema as unknown as H32;
export type MintingValidator = {
  mintingScriptCBOR: string;
  mintingScript: Script;
  policyId: PolicyId;
};

export type SpendingValidator = {
  spendingScriptCBOR: string;
  spendingScript: Script;
  spendingScriptHash: ScriptHash;
  spendingScriptAddress: Address;
};

export type WithdrawalValidator = {
  withdrawalScriptCBOR: string;
  withdrawalScript: Script;
  withdrawalScriptHash: ScriptHash;
};

export type AuthenticatedValidator = SpendingValidator & MintingValidator;

// TODO: We'll need a more elaborate design to allow multiple steps for each
//       proof.
export type FraudProofs = {
  doubleSpend: SpendingValidator;
  nonExistentInput: SpendingValidator;
  nonExistentInputNoIndex: SpendingValidator;
  invalidRange: SpendingValidator;
};

export type MidgardValidators = {
  hubOracle: MintingValidator;
  stateQueue: AuthenticatedValidator;
  scheduler: AuthenticatedValidator;
  registeredOperators: AuthenticatedValidator;
  activeOperators: AuthenticatedValidator;
  retiredOperators: AuthenticatedValidator;
  escapeHatch: AuthenticatedValidator;
  fraudProofCatalogue: AuthenticatedValidator;
  fraudProof: AuthenticatedValidator;
  deposit: AuthenticatedValidator;
  withdrawal: AuthenticatedValidator;
  txOrder: AuthenticatedValidator;
  settlement: AuthenticatedValidator;
  reserve: SpendingValidator & WithdrawalValidator;
  payout: AuthenticatedValidator;
  fraudProofs: FraudProofs;
};

export const OutputReferenceSchema = Data.Object({
  txHash: Data.Object({ hash: Data.Bytes({ minLength: 32, maxLength: 32 }) }),
  outputIndex: Data.Integer(),
});
export type OutputReference = Data.Static<typeof OutputReferenceSchema>;
export const OutputReference =
  OutputReferenceSchema as unknown as OutputReference;

export const AssetsSchema = Data.Object({
  policyId: Data.Bytes(),
  assetName: Data.Bytes(),
});
export type Assets = Data.Static<typeof AssetsSchema>;
export const Assets = AssetsSchema as unknown as Assets;

export const ValueSchema = Data.Object({
  inner: Data.Map(Data.Bytes(), Data.Map(Data.Bytes(), Data.Integer())),
});
export type Value = Data.Static<typeof ValueSchema>;
export const Value = ValueSchema as unknown as Value;

export const POSIXTimeSchema = Data.Integer();
export type POSIXTime = Data.Static<typeof POSIXTimeSchema>;
export const POSIXTime = POSIXTimeSchema as unknown as POSIXTime;

export const PosixTimeDurationSchema = Data.Integer();
export type PosixTimeDuration = Data.Static<typeof PosixTimeDurationSchema>;
export const PosixTimeDuration =
  PosixTimeDurationSchema as unknown as PosixTimeDuration;

export const VerificationKeyHashSchema = Data.Bytes({
  minLength: 28,
  maxLength: 28,
});

export const PubKeyHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });

export const ScriptHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });

export const PolicyIdSchema = ScriptHashSchema;

export const MerkleRootSchema = Data.Bytes({ minLength: 32, maxLength: 32 });
export type MerkleRoot = Data.Static<typeof MerkleRootSchema>;
export const MerkleRoot = MerkleRootSchema as unknown as MerkleRoot;

export const CredentialSchema = Data.Enum([
  Data.Object({
    PublicKeyCredential: Data.Tuple([PubKeyHashSchema]),
  }),
  Data.Object({
    ScriptCredential: Data.Tuple([ScriptHashSchema]),
  }),
]);
export type CredentialD = Data.Static<typeof CredentialSchema>;
export const CredentialD = CredentialSchema as unknown as CredentialD;

export const AddressSchema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(
    Data.Enum([
      Data.Object({ Inline: Data.Tuple([CredentialSchema]) }),
      Data.Object({
        Pointer: Data.Tuple([
          Data.Object({
            slotNumber: Data.Integer(),
            transactionIndex: Data.Integer(),
            certificateIndex: Data.Integer(),
          }),
        ]),
      }),
    ]),
  ),
});
export type AddressData = Data.Static<typeof AddressSchema>;
export const AddressData = AddressSchema as unknown as AddressData;

export const NeighborSchema = Data.Object({
  Neighbor: Data.Object({
    nibble: Data.Integer(),
    prefix: Data.Bytes(),
    root: Data.Bytes(),
  }),
});
export type Neighbor = Data.Static<typeof NeighborSchema>;
export const Neighbor = NeighborSchema as unknown as Neighbor;

export const ProofStepSchema = Data.Enum([
  Data.Object({
    Branch: Data.Object({
      skip: Data.Integer(),
      neighbors: Data.Bytes(),
    }),
  }),
  Data.Object({
    Fork: Data.Object({
      skip: Data.Integer(),
      neighbor: NeighborSchema,
    }),
  }),
  Data.Object({
    Leaf: Data.Object({
      skip: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
]);
export type ProofStep = Data.Static<typeof ProofStepSchema>;
export const ProofStep = ProofStepSchema as unknown as ProofStep;

export const ProofSchema = Data.Array(ProofStepSchema);
export type Proof = Data.Static<typeof ProofSchema>;
export const Proof = ProofSchema as unknown as Proof;

/**
 * TODO: Note that this function does not support pointer addresses.
 */
export const addressDataFromBech32 = (
  address: Address,
): Effect.Effect<AddressData, Bech32DeserializationError> =>
  Effect.gen(function* () {
    const addressDetails = yield* Effect.try({
      try: () => getAddressDetails(address),
      catch: (error) =>
        new Bech32DeserializationError({
          message: `Failed to parse address: ${address}`,
          cause: error,
        }),
    });
    const { paymentCredential, stakeCredential } = addressDetails;

    if (!paymentCredential) {
      return yield* Effect.fail(
        new Bech32DeserializationError({
          message: "Address missing payment credential",
          cause: `Invalid address: ${address}`,
        }),
      );
    }

    return {
      paymentCredential:
        paymentCredential.type === "Key"
          ? { PublicKeyCredential: [paymentCredential.hash] }
          : { ScriptCredential: [paymentCredential.hash] },
      stakeCredential: stakeCredential
        ? {
            Inline: [
              stakeCredential.type === "Key"
                ? { PublicKeyCredential: [stakeCredential.hash] }
                : { ScriptCredential: [stakeCredential.hash] },
            ],
          }
        : null,
    };
  });

export type AuthenticUTxO<TDatum, TExtra = undefined> = {
  utxo: UTxO;
  datum: TDatum;
  assetName: string;
  extra?: TExtra;
};

export const getDatumFromUTxO = <TDatum>(
  nodeUTxO: UTxO,
  schema: any,
): Effect.Effect<TDatum, DataCoercionError> =>
  Effect.gen(function* () {
    const datumCBOR = nodeUTxO.datum;
    if (!datumCBOR) {
      return yield* Effect.fail(
        new DataCoercionError({
          message: `Datum coercion failed`,
          cause: `No datum found`,
        }),
      );
    }
    const datum: TDatum = yield* Effect.try({
      try: () => Data.from(datumCBOR, schema),
      catch: (e) =>
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to the expected datum type`,
          cause: e,
        }),
    });
    return datum;
  });
/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToAuthenticUTxO = <TDatum, TExtra = undefined>(
  utxo: UTxO,
  nftPolicy: string,
  schema: any,
  extraFields?: (datum: TDatum) => TExtra,
): Effect.Effect<
  AuthenticUTxO<TDatum, TExtra>,
  DataCoercionError | UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const datum = yield* getDatumFromUTxO<TDatum>(utxo, schema);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: `Failed to convert UTxO to AuthenticUTxO`,
          cause: `UTxO's NFT policy ID is not the same as the expected policy ID`,
        }),
      );
    }
    const extra = extraFields ? extraFields(datum) : ({} as TExtra);

    return {
      utxo,
      datum,
      assetName,
      ...extra,
    };
  });
/**
 * Silently drops invalid UTxOs.
 */
export const utxosToAuthenticUTxOs = <TDatum, TExtra = undefined>(
  utxos: UTxO[],
  nftPolicy: string,
  schema: any,
  extraFields?: (datum: TDatum) => TExtra,
): Effect.Effect<AuthenticUTxO<TDatum, TExtra>[]> => {
  const effects = utxos.map((u) =>
    utxoToAuthenticUTxO<TDatum, TExtra>(u, nftPolicy, schema, extraFields),
  );
  return Effect.allSuccesses(effects);
};

export const findOperatorByPKH = (
  activeOperators: ActiveOperatorUTxO[],
  retiredOperators: RetiredOperatorUTxO[],
  operatorPKH: string,
): Effect.Effect<
  | (ActiveOperatorUTxO & { isActive: true })
  | (RetiredOperatorUTxO & { isActive: false }),
  LucidError
> => {
  const activeOperatorMatch = EffectArray.findFirst(
    activeOperators,
    (utxo) => utxo.datum.key === operatorPKH,
  );

  if (Option.isSome(activeOperatorMatch)) {
    return Effect.succeed({ ...activeOperatorMatch.value, isActive: true });
  }

  const retiredOperatorMatch = EffectArray.findFirst(
    retiredOperators,
    (utxo) => utxo.datum.key === operatorPKH,
  );

  if (Option.isSome(retiredOperatorMatch)) {
    return Effect.succeed({ ...retiredOperatorMatch.value, isActive: false });
  }

  return Effect.fail(
    new LucidError({
      message: `No Operator UTxO with key "${operatorPKH}" found`,
      cause: "Operator not found in active or retired UTxOs",
    }),
  );
};

export type GenericErrorFields = {
  readonly message: string;
  readonly cause: any;
};

export class CmlUnexpectedError extends EffectData.TaggedError(
  "CmlUnexpectedError",
)<GenericErrorFields> {}

export class CmlDeserializationError extends EffectData.TaggedError(
  "CmlDeserializationError",
)<GenericErrorFields> {}

export class CborSerializationError extends EffectData.TaggedError(
  "CborSerializationError",
)<GenericErrorFields> {}

export class CborDeserializationError extends EffectData.TaggedError(
  "CborDeserializationError",
)<GenericErrorFields> {}

export class Bech32DeserializationError extends EffectData.TaggedError(
  "Bech32DeserializationError",
)<GenericErrorFields> {}

export class DataCoercionError extends EffectData.TaggedError(
  "DataCoercionError",
)<GenericErrorFields> {}

export class UnauthenticUtxoError extends EffectData.TaggedError(
  "UnauthenticUtxoError",
)<GenericErrorFields> {}

export class MissingDatumError extends EffectData.TaggedError(
  "MissingDatumError",
)<GenericErrorFields> {}

export class LucidError extends EffectData.TaggedError(
  "LucidError",
)<GenericErrorFields> {}

export class HashingError extends EffectData.TaggedError(
  "HashingError",
)<GenericErrorFields> {}

export class AssetError extends EffectData.TaggedError(
  "AssetError",
)<GenericErrorFields> {}

export class UnspecifiedNetworkError extends EffectData.TaggedError(
  "UnspecifiedNetworkError",
)<GenericErrorFields> {}

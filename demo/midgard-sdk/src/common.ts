import {
  Data,
  getAddressDetails,
  Script,
  ScriptHash,
  Network,
  AddressDetails,
  credentialToAddress,
} from "@lucid-evolution/lucid";
import { Data as EffectData } from "effect";
import { Effect } from "effect";
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

/**
 * Shared SDK primitives used across Midgard client modules.
 *
 * This file intentionally mixes three kinds of helpers:
 * 1. Runtime adapters around Effect programs.
 * 2. Validation/authentication helpers for Lucid-facing values.
 * 3. Data schemas and typed errors shared by multiple validators and
 *    transaction builders.
 */
export const makeReturn = <A, E>(program: Effect.Effect<A, E>) => {
  return {
    unsafeRun: () => Effect.runPromise(program),
    safeRun: () => Effect.runPromise(Effect.either(program)),
    program: () => program,
  };
};

/**
 * Returns `true` only for non-empty hexadecimal strings.
 */
export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

/**
 * Extracts the single non-ADA asset from a value map.
 *
 * Many Midgard UTxOs are authenticated by exactly one NFT or beacon token. The
 * helper enforces that invariant up-front so downstream code can treat the
 * returned policy id / asset name pair as unambiguous.
 */
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
 * Narrows `getSingleAssetApartFromAda` to the common "exactly one beacon NFT"
 * case used by Midgard authenticated state UTxOs.
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
 * Fetches UTxOs at an address/credential and keeps only those authenticated by
 * the expected NFT policy id.
 *
 * Non-matching or malformed UTxOs are intentionally discarded instead of
 * failing the whole query. This lets callers work against noisy addresses while
 * still rejecting provider-level fetch failures.
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

/**
 * Shared Blake2b wrapper that validates the input shape once and normalizes the
 * emitted hashing error structure.
 */
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

/**
 * Hashes a hex-encoded payload into a 28-byte Blake2b digest.
 */
export const hashHexWithBlake2b224 = (
  msg: string,
): Effect.Effect<string, HashingError> => blake2bHelper(msg, 28, "Blake2b224");

/**
 * Hashes a hex-encoded payload into a 32-byte Blake2b digest.
 */
export const hashHexWithBlake2b256 = (
  msg: string,
): Effect.Effect<string, HashingError> => blake2bHelper(msg, 32, "Blake2b256");

/**
 * Best-effort buffer-to-hex conversion used in logs and debug paths.
 */
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

/**
 * Assumes the given Bech32 string is that of a Cardano address (TODO).
 */
export const midgardAddressFromBech32 = (
  bechStr: string,
): Effect.Effect<MidgardAddress, Bech32DeserializationError> =>
  Effect.gen(function* () {
    const addressDetails: AddressDetails = yield* Effect.try({
      try: () => getAddressDetails(bechStr),
      catch: (e) =>
        new Bech32DeserializationError({
          message: `Failed to break down ${bechStr} to its details.`,
          cause: e,
        }),
    });
    const cred = addressDetails.paymentCredential;
    if (cred === undefined) {
      return yield* new Bech32DeserializationError({
        message: `Failed extracting the payment credential from ${bechStr}.`,
        cause: "Unknown cause.",
      });
    } else {
      if (cred.type === "Key") {
        const midgardAddress: MidgardAddress = {
          PublicKeyCredential: [cred.hash],
        };
        return midgardAddress;
      } else {
        const midgardAddress: MidgardAddress = {
          ScriptCredential: [cred.hash],
        };
        return midgardAddress;
      }
    }
  });

/**
 * Taking Cardano `Network` as the first argument is temporary (TODO).
 */
export const midgardAddressToBech32 = (
  network: Network,
  addr: MidgardAddress,
): string => {
  if ("PublicKeyCredential" in addr) {
    const [pubKeyHex] = addr.PublicKeyCredential;
    const cred: Credential = {
      type: "Key",
      hash: pubKeyHex,
    };
    return credentialToAddress(network, cred);
  } else {
    const [scriptHashHex] = addr.ScriptCredential;
    const cred: Credential = {
      type: "Script",
      hash: scriptHashHex,
    };
    return credentialToAddress(network, cred);
  }
};

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
  transactionId: Data.Bytes({ minLength: 32, maxLength: 32 }),
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

export const MidgardAddressSchema = CredentialSchema;
export type MidgardAddress = CredentialD;
export const MidgardAddress = MidgardAddressSchema as unknown as MidgardAddress;

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

/**
 * Converts a bech32 Cardano address into the on-chain data shape used by
 * Midgard validators.
 *
 * Pointer addresses are still unsupported, so the conversion only emits either
 * an inline stake credential or `null`.
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

export type GenericErrorFields = {
  readonly message: string;
  readonly cause: any;
};

/**
 * Error family shared across SDK modules so callers can pattern-match on a
 * stable `_tag` while still preserving the original cause.
 */
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

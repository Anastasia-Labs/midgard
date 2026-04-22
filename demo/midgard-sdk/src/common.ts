import {
  AddressDetails,
  credentialToAddress,
  Data,
  getAddressDetails,
  Network,
  Script,
  ScriptHash,
} from "@lucid-evolution/lucid";
import { Array as EffectArray, Effect, Option } from "effect";
import {
  Address,
  Credential,
  LucidEvolution,
  PolicyId,
  UTxO,
  fromHex,
  toHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { ActiveOperatorUTxO } from "./active-operators.js";
import { RetiredOperatorUTxO } from "./retired-operators.js";
import {
  Bech32DeserializationError,
  HashingError,
  LucidError,
  UnauthenticUtxoError,
} from "./errors.js";
import { getStateToken } from "./internals.js";

export * from "./errors.js";

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

/**
 * `StateUTxO` would probably be a better name, but it'd be confusing next to
 * our state queue UTxOs.
 */
export type BeaconUTxO = {
  utxo: UTxO;
  policyId: PolicyId;
  assetName: string;
};

/**
 * Silently drops the UTxOs without proper authentication NFTs.
 */
export const utxosAtByNFTPolicyId = (
  lucid: LucidEvolution,
  addressOrCred: Address | Credential,
  policyId: PolicyId,
): Effect.Effect<BeaconUTxO[], LucidError> =>
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

    const nftEffects: Effect.Effect<BeaconUTxO, UnauthenticUtxoError>[] =
      allUTxOs.map((u: UTxO) => {
        const nftsEffect = getStateToken(u.assets);
        return Effect.andThen(
          nftsEffect,
          ([sym, assetName]): Effect.Effect<
            BeaconUTxO,
            UnauthenticUtxoError
          > => {
            if (sym === policyId) {
              return Effect.succeed({ utxo: u, policyId, assetName });
            }

            return Effect.fail(
              new UnauthenticUtxoError({
                message: "Failed to get assets from fetched UTxOs",
                cause: "UTxO doesn't have the expected NFT policy ID",
              }),
            );
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
  hubOracle: AuthenticatedValidator;
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

export const MidgardAddressSchema = CredentialSchema;
export type MidgardAddress = CredentialD;
export const MidgardAddress = MidgardAddressSchema as unknown as MidgardAddress;

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

/**
 * TODO: Move to the `operatorDirectory` module after refactoring.`
 */
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

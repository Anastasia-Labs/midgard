import {
  Address,
  Credential,
  Data,
  fromHex,
  getAddressDetails,
  LucidEvolution,
  PolicyId,
  Script,
  ScriptHash,
  toHex,
  UTxO,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import { ActiveOperatorUTxO } from "./active-operators.js";
import {
  Bech32DeserializationError,
  HashingError,
  LucidError,
  UnauthenticUtxoError,
} from "./errors.js";
import { getStateToken } from "./internals.js";
import { RetiredOperatorUTxO } from "./retired-operators.js";

export * from "./errors.js";

export const makeReturn = <A, E>(program: Effect.Effect<A, E>) => ({
  unsafeRun: () => Effect.runPromise(program),
  safeRun: () => Effect.runPromise(Effect.either(program)),
  program: () => program,
});

export const isHexString = (str: string): boolean => /^[0-9A-Fa-f]+$/.test(str);

/**
 * `StateUTxO` would probably be a better name, but it'd be confusing next to
 * our state queue UTxOs.
 */
export type BeaconUTxO = {
  utxo: UTxO;
  policyId: PolicyId;
  assetName: string;
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const validateProviderUtxos = (value: unknown): UTxO[] => {
  if (!Array.isArray(value)) {
    throw new Error("Provider UTxO result must be an array");
  }
  for (const [index, entry] of value.entries()) {
    if (
      !isRecord(entry) ||
      typeof entry.txHash !== "string" ||
      entry.txHash.length === 0 ||
      typeof entry.outputIndex !== "number" ||
      !Number.isSafeInteger(entry.outputIndex) ||
      entry.outputIndex < 0 ||
      typeof entry.address !== "string" ||
      entry.address.length === 0 ||
      !isRecord(entry.assets)
    ) {
      throw new Error(`Provider UTxO result has an invalid entry at ${index}`);
    }
    for (const [unit, quantity] of Object.entries(entry.assets)) {
      if (unit.length === 0 || typeof quantity !== "bigint") {
        throw new Error(
          `Provider UTxO result has invalid assets at entry ${index}`,
        );
      }
    }
  }
  return value as UTxO[];
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
    const providerResult: unknown = yield* Effect.tryPromise({
      // Lucid 0.6 provides a provider-neutral policy query with a native
      // Kupmios fast path and a correct address-wide fallback.
      try: () =>
        (
          lucid as unknown as {
            utxosAtWithPolicy(
              address: Address | Credential,
              policy: PolicyId,
            ): Promise<unknown>;
          }
        ).utxosAtWithPolicy(addressOrCred, policyId),
      catch: (e) => {
        return new LucidError({
          message: `Failed to fetch UTxOs at: ${addressOrCred}`,
          cause: e,
        });
      },
    });
    const allUTxOs = yield* Effect.try({
      try: () => validateProviderUtxos(providerResult),
      catch: (e) =>
        new LucidError({
          message: `Failed to fetch UTxOs at: ${addressOrCred}`,
          cause: e,
        }),
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

    return yield* Effect.allSuccesses(nftEffects);
  }).pipe(
    Effect.catchAllDefect(
      (d) =>
        new LucidError({
          message: `Unexpected error while fetching UTxOs at: ${addressOrCred}`,
          cause: d,
        }),
    ),
  );

export const hashHexWithBlake2b = (
  msg: string,
  digestByteLength: 28 | 32,
): Effect.Effect<string, HashingError> => {
  const functionName = digestByteLength === 28 ? "Blake2b224" : "Blake2b256";
  const errorMessage = `Failed to hash using ${functionName} function`;
  if (!isHexString(msg)) {
    return Effect.fail(
      new HashingError({
        message: errorMessage,
        cause: `Invalid message provided`,
      }),
    );
  }

  try {
    return Effect.succeed(
      toHex(blake2b(fromHex(msg), { dkLen: digestByteLength })),
    );
  } catch (e) {
    return Effect.fail(
      new HashingError({
        message: errorMessage,
        cause: e,
      }),
    );
  }
};

export const bufferToHex = (buf: Buffer): string => buf.toString("hex");

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

export type ValidationTraceDisputeValidators = SpendingValidator & {
  readonly source: SpendingValidator;
  readonly game: SpendingValidator;
  readonly boundary: SpendingValidator;
  readonly timeout: SpendingValidator;
  readonly award: SpendingValidator;
};

export type FraudProofs = {
  doubleSpend: SpendingValidator;
  nonExistentInput: SpendingValidator;
  nonExistentInputNoIndex: SpendingValidator;
  invalidRange: SpendingValidator;
  transitionTrace: SpendingValidator;
  /**
   * V1 stateful dispute game for a transaction-validation trace.
   */
  validationTraceDispute: ValidationTraceDisputeValidators;
  zeroInput: SpendingValidator;
  /**
   * Q44 `da-hash-preimage`: a committed `transactions_root` leaf whose key is
   * not the canonical native-V1 transaction id of its own value.
   */
  daHashPreimage: SpendingValidator;
  /**
   * Q18 `no-reference-input`: a committed transaction reads an input that
   * never existed in the block's prev ledger and was not produced in-block.
   */
  noReferenceInput: SpendingValidator;
  /**
   * Q31 `reference-input-no-idx`: a committed transaction reads an output
   * index its in-block producing transaction never created.
   */
  referenceInputNoIdx: SpendingValidator;
  /**
   * Q15 `invalid-signature`: a committed transaction's address-witness set
   * does not authorize one of the inputs it spends.
   */
  invalidSignature: SpendingValidator;
};

export type MidgardValidators = {
  referenceScriptAuth: MintingValidator;
  hubOracle: AuthenticatedValidator;
  daParamsGovernor: AuthenticatedValidator;
  daAttestation: AuthenticatedValidator;
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
  /**
   * V1 field fragments remain locked here until the corresponding
   * tx-order NFT is burned.
   */
  txOrderFieldPreimage: SpendingValidator;
  /**
   * V1 receipts are minted only after L1 verifies one referenced field
   * preimage and remain locked until the corresponding tx-order NFT burns.
   */
  txOrderFieldReceipt: SpendingValidator & MintingValidator;
  /**
   * Permissionless append-only L1 availability for content-addressed V1
   * CEK material. Its validator has no successful spending path.
   */
  cekProgramMaterial: SpendingValidator;
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

export const outputReferenceFromUTxO = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): OutputReference => ({
  transactionId: utxo.txHash,
  outputIndex: BigInt(utxo.outputIndex),
});

export const AssetsSchema = Data.Object({
  policyId: Data.Bytes(),
  assetName: Data.Bytes(),
});
export type Assets = Data.Static<typeof AssetsSchema>;
export const Assets = AssetsSchema as unknown as Assets;

export const ValueSchema = Data.Map(
  Data.Bytes(),
  Data.Map(Data.Bytes(), Data.Integer()),
);
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
  nibble: Data.Integer(),
  prefix: Data.Bytes(),
  root: Data.Bytes(),
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
  const activeOperatorMatch = activeOperators.find((utxo) =>
    utxo.assetName.endsWith(operatorPKH),
  );
  if (activeOperatorMatch !== undefined) {
    return Effect.succeed({ ...activeOperatorMatch, isActive: true });
  }

  const retiredOperatorMatch = retiredOperators.find((utxo) =>
    utxo.assetName.endsWith(operatorPKH),
  );
  if (retiredOperatorMatch !== undefined) {
    return Effect.succeed({ ...retiredOperatorMatch, isActive: false });
  }

  return Effect.fail(
    new LucidError({
      message: `No Operator UTxO with key "${operatorPKH}" found`,
      cause: "Operator not found in active or retired UTxOs",
    }),
  );
};

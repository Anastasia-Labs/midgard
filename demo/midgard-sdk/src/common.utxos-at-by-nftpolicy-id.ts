import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  Address,
  Credential,
  Data,
  fromHex,
  LucidEvolution,
  PolicyId,
  Script,
  ScriptHash,
  toHex,
  UTxO,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import { HashingError, LucidError, UnauthenticUtxoError } from "./errors.js";
import { getStateToken } from "./internals.js";

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
const addressOrCredentialLabel = (
  addressOrCred: Address | Credential,
): string =>
  typeof addressOrCred === "string"
    ? addressOrCred
    : `${addressOrCred.type}:${addressOrCred.hash}`;

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
          message: `Failed to fetch UTxOs at: ${addressOrCredentialLabel(addressOrCred)}`,
          cause: e,
        });
      },
    });
    const allUTxOs = yield* Effect.try({
      try: () => validateProviderUtxos(providerResult),
      catch: (e) =>
        new LucidError({
          message: `Failed to fetch UTxOs at: ${addressOrCredentialLabel(addressOrCred)}`,
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
          message: `Unexpected error while fetching UTxOs at: ${addressOrCredentialLabel(addressOrCred)}`,
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

export const H32 = asDataType<H32>(H32Schema);

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

export type AvailabilityChallengeYieldValidators = {
  readonly open: WithdrawalValidator;
  readonly settle: WithdrawalValidator;
  readonly close: WithdrawalValidator;
  readonly timeout: WithdrawalValidator;
};

export type AvailabilityChallengeValidator = AuthenticatedValidator & {
  readonly yields: AvailabilityChallengeYieldValidators;
};

export type StateQueueYieldValidators = {
  readonly commit: WithdrawalValidator;
  readonly unattestedTimeout: WithdrawalValidator;
  readonly unavailableTimeout: WithdrawalValidator;
  readonly fraudRemoval: WithdrawalValidator;
  readonly merge: WithdrawalValidator;
};

export type StateQueueValidator = AuthenticatedValidator & {
  readonly yields: StateQueueYieldValidators;
};

export type ValidationTraceDisputeValidators = SpendingValidator & {
  readonly source: SpendingValidator;
  readonly game: SpendingValidator;
  readonly boundary: SpendingValidator;
  readonly timeout: SpendingValidator;
  readonly award: SpendingValidator;
};

import {
  Address,
  Data,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator as LucidSpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../../common.js";
import {
  declaredParameters,
  describeDeclaredParameters,
  type FaultProofBlueprint,
  getBlueprintValidator,
} from "./blueprint.parse-fault-proof-blueprint.js";

/**
 * The same fail-closed reading for validators deployed with no parameters at
 * all: a title that silently grows a parameter must not keep being deployed
 * bare, which is under-application by the whole parameter list.
 */
export const getUnappliedScript = (
  blueprint: FaultProofBlueprint,
  title: string,
): string => {
  const validator = getBlueprintValidator(blueprint, title);
  if (declaredParameters(validator).length !== 0) {
    throw new Error(
      `Blueprint validator "${title}" declares ` +
        `${declaredParameters(validator).length.toString()} parameter(s) ` +
        `(${describeDeclaredParameters(validator)}) but is deployed with none ` +
        "applied, which is an always-succeeds script (#609).",
    );
  }
  return validator.compiledCode;
};

/**
 * A Plutus V3 script's hash, and the enterprise address it yields on a given
 * network, are pure functions of the script bytes (and the network): Lucid
 * decodes the script through CML and hashes it on every call, which costs
 * milliseconds per validator and dominated repeated contract resolution (each
 * submit path re-derives its whole family chain from the blueprint). Memoizing
 * on the exact inputs cannot change any hash or address: a hit is a proof the
 * inputs were byte-identical, a miss runs the same Lucid function, and a throw
 * caches nothing. Only immutable strings are cached; every caller still gets
 * fresh validator objects.
 *
 * Each cache holds at most PLUTUS_V3_SCRIPT_CACHE_LIMIT entries and evicts its
 * oldest first. The blueprint compiles about 570 distinct scripts, so one
 * deployment's whole set fits and a running node never evicts; a process that
 * applies many parameter sets (tests, multi-deployment tooling) stops growing
 * at the cap instead of keeping every script it ever hashed.
 */
export const PLUTUS_V3_SCRIPT_CACHE_LIMIT = 1024;
const plutusV3ScriptHashCache = new Map<string, string>();
const plutusV3EnterpriseAddressCache = new Map<string, string>();

export const memoized = (
  cache: Map<string, string>,
  key: string,
  compute: () => string,
  limit: number = PLUTUS_V3_SCRIPT_CACHE_LIMIT,
): string => {
  const cached = cache.get(key);
  if (cached !== undefined) {
    return cached;
  }
  const value = compute();
  if (cache.size >= limit) {
    const oldest = cache.keys().next().value;
    if (oldest !== undefined) cache.delete(oldest);
  }
  cache.set(key, value);
  return value;
};

export const makeMintingPolicy = (
  mintingScriptCBOR: string,
): MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: memoized(plutusV3ScriptHashCache, mintingScriptCBOR, () =>
      mintingPolicyToId(mintingScript),
    ),
  };
};

export const makeSpendingValidator = (
  network: Network,
  spendingScriptCBOR: string,
): SpendingValidator => {
  const spendingScript: LucidSpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: memoized(
      plutusV3EnterpriseAddressCache,
      `${network}|${spendingScriptCBOR}`,
      () => validatorToAddress(network, spendingScript),
    ),
    spendingScriptHash: memoized(
      plutusV3ScriptHashCache,
      spendingScriptCBOR,
      () => validatorToScriptHash(spendingScript),
    ),
  };
};

export const makeWithdrawalValidator = (
  withdrawalScriptCBOR: string,
): WithdrawalValidator => {
  const withdrawalScript = {
    type: "PlutusV3" as const,
    script: withdrawalScriptCBOR,
  };
  return {
    withdrawalScriptCBOR,
    withdrawalScript,
    withdrawalScriptHash: memoized(
      plutusV3ScriptHashCache,
      withdrawalScriptCBOR,
      () => validatorToScriptHash(withdrawalScript),
    ),
  };
};

export const makeAuthenticatedValidator = (
  network: Network,
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeSpendingValidator(network, spendingScriptCBOR),
  ...makeMintingPolicy(mintingScriptCBOR),
});

export const asAddressDataParam = (
  address: Address,
): Effect.Effect<Data, Error> =>
  addressDataFromBech32(address).pipe(
    Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to encode fraud proof token address parameter: ${cause.message}`,
        ),
    ),
  );

export const tryBuild = <A>(
  description: string,
  build: () => A,
): Effect.Effect<A, Error> =>
  Effect.try({
    try: build,
    catch: (cause) =>
      new Error(
        `${description}: ${cause instanceof Error ? cause.message : String(cause)}`,
      ),
  });

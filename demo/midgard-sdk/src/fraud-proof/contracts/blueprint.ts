/**
 * Blueprint parsing and parameter application for fault-proof validators.
 */

import {
  Address,
  applyParamsToScript,
  Data,
  fromHex,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator as LucidSpendingValidator,
  toHex,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../../common.js";

/**
 * One entry of a blueprint validator's `parameters[]`: the compiler's own
 * record of a `validator main(...)` parameter, in declaration order.
 */
export type FaultProofBlueprintParameter = {
  readonly title: string;
};

export type FaultProofBlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  /**
   * The parameters the compiled script declares, in declaration order. Carried
   * (rather than dropped at parse time, as it was before #609) because it is
   * the only authority on how many terms must be applied before the script is
   * a complete validator — see {@link applyBlueprintParams}.
   */
  readonly parameters: readonly FaultProofBlueprintParameter[];
};

export type FaultProofBlueprint = {
  readonly validators: readonly FaultProofBlueprintValidator[];
};

export const deriveValidationTraceDeploymentIdV1 = (
  fraudProofCataloguePolicyId: string,
): string => {
  if (!/^[0-9a-fA-F]{56}$/u.test(fraudProofCataloguePolicyId)) {
    throw new Error(
      "Fraud-proof catalogue policy id must be exactly 28 bytes of hexadecimal",
    );
  }
  return toHex(blake2b(fromHex(fraudProofCataloguePolicyId), { dkLen: 32 }));
};

export const parseFaultProofBlueprint = (
  value: unknown,
): FaultProofBlueprint => {
  if (typeof value !== "object" || value === null) {
    throw new Error("Fault proof blueprint must be a JSON object");
  }

  const validators = (value as { readonly validators?: unknown }).validators;
  if (!Array.isArray(validators)) {
    throw new Error("Fault proof blueprint must contain validators[]");
  }

  return {
    validators: validators.map((validator, index) => {
      if (typeof validator !== "object" || validator === null) {
        throw new Error(`validators[${index}] must be an object`);
      }
      const candidate = validator as {
        readonly title?: unknown;
        readonly compiledCode?: unknown;
        readonly parameters?: unknown;
      };
      if (typeof candidate.title !== "string") {
        throw new Error(`validators[${index}].title must be a string`);
      }
      if (typeof candidate.compiledCode !== "string") {
        throw new Error(`validators[${index}].compiledCode must be a string`);
      }
      // A validator that takes no parameters omits the key entirely, so absent
      // means zero declared — never "unknown, skip the check".
      const rawParameters = candidate.parameters ?? [];
      if (!Array.isArray(rawParameters)) {
        throw new Error(
          `validators[${index}].parameters must be an array when present`,
        );
      }
      return {
        title: candidate.title,
        compiledCode: candidate.compiledCode,
        parameters: rawParameters.map((parameter, parameterIndex) => {
          const parameterTitle = (parameter as { readonly title?: unknown })
            .title;
          if (typeof parameterTitle !== "string") {
            throw new Error(
              `validators[${index}].parameters[${parameterIndex}].title must be a string`,
            );
          }
          return { title: parameterTitle };
        }),
      };
    }),
  };
};

export const getBlueprintValidator = (
  blueprint: FaultProofBlueprint,
  title: string,
): FaultProofBlueprintValidator => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found in blueprint`);
  }
  return found;
};

/**
 * The parameters a blueprint entry declares.
 *
 * A validator that takes none omits the key entirely — that is the compiler's
 * format, so ABSENT MEANS ZERO, never "unknown, skip the check". Read through
 * this accessor rather than the field so a caller handing us a raw `plutus.json`
 * object (where the key is simply missing on nullary validators) is checked by
 * the same rule as one that went through {@link parseFaultProofBlueprint}.
 */
export const declaredParameters = (
  validator: FaultProofBlueprintValidator,
): readonly FaultProofBlueprintParameter[] => validator.parameters ?? [];

const describeDeclaredParameters = (
  validator: FaultProofBlueprintValidator,
): string =>
  declaredParameters(validator).length === 0
    ? "none"
    : declaredParameters(validator)
        .map((parameter) => parameter.title)
        .join(", ");

/**
 * The single place this package turns a blueprint entry into a deployable
 * script, and the only permitted caller of `applyParamsToScript` here.
 *
 * `applyParamsToScript` applies whatever list it is handed and never checks it
 * against the script's own declared arity. Applying too FEW terms is silent and
 * catastrophic: the remaining `validator main(...)` parameters stay as lambdas,
 * so the ledger's single Plutus V3 script-context application reduces to a
 * lambda VALUE instead of running the validator body. Evaluation terminates
 * without error, and the ledger reads "no error" as SUCCESS — the deployment is
 * an unconditional always-succeeds script whose Aiken guards never execute.
 * That is exactly how ten validation-trace semantic resolvers shipped after
 * #592 added their `field_preimage_certificate_policy_id` parameter (#605/#609).
 * Applying too MANY is a well-formed script with a wrong hash, which surfaces
 * days later as a credential that matches nothing on chain.
 *
 * Refusing both directions here converts that whole class into a build-time
 * failure at the load site, for every validator this package deploys.
 */
export const applyBlueprintParams = (
  blueprint: FaultProofBlueprint,
  title: string,
  params: readonly Data[],
): string => {
  const validator = getBlueprintValidator(blueprint, title);
  if (declaredParameters(validator).length !== params.length) {
    throw new Error(
      `Blueprint validator "${title}" declares ` +
        `${declaredParameters(validator).length.toString()} parameter(s) ` +
        `(${describeDeclaredParameters(validator)}) but ` +
        `${params.length.toString()} were applied. Under-application deploys an ` +
        "always-succeeds script and over-application deploys a wrong hash; " +
        "apply exactly the declared parameters (#609).",
    );
  }
  const cacheKey = appliedScriptCacheKey(validator.compiledCode, params);
  const cached = appliedScriptCache.get(cacheKey);
  if (cached !== undefined) {
    return cached;
  }
  const applied = applyParamsToScript(validator.compiledCode, [...params]);
  appliedScriptCache.set(cacheKey, applied);
  return applied;
};

/**
 * `applyParamsToScript` is pure — the applied script is a function of nothing
 * but the compiled code and the CBOR of the parameters — and it dominates
 * contract construction (3–65 ms per validator, ~14 s across a full
 * fault-proof contract build). Memoizing on the exact inputs therefore cannot
 * change any deployed byte: a cache hit is a proof the inputs were identical.
 * The #609 arity guard above runs before the lookup on every call, cached or
 * not, so under-/over-application still fails closed.
 */
const appliedScriptCache = new Map<string, string>();

const appliedScriptCacheKey = (
  compiledCode: string,
  params: readonly Data[],
): string =>
  toHex(
    blake2b(
      new TextEncoder().encode(
        `${compiledCode}|${params.map((param) => Data.to(param)).join("|")}`,
      ),
      { dkLen: 32 },
    ),
  );

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
    policyId: mintingPolicyToId(mintingScript),
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
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
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
    withdrawalScriptHash: validatorToScriptHash(withdrawalScript),
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

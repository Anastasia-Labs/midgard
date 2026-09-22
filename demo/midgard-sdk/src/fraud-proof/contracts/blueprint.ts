/**
 * Blueprint parsing and parameter application for fault-proof validators.
 */

import { encodeCborArrayRaw, readCborBytes } from "@al-ft/midgard-core/codec";
import {
  Address,
  applyDoubleCborEncoding,
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
import * as UPLC from "@lucid-evolution/uplc";
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
  /**
   * The compiler's `schema.$ref` for the parameter, when it is a reference
   * (`#/definitions/aiken~1crypto~1ScriptHash`, `#/definitions/Int`, ...).
   * Carried so {@link applyBlueprintParams} can check each applied term has
   * the SHAPE the declared type promises — see {@link assertParameterShapes}.
   */
  readonly schemaRef?: string;
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

export const deriveValidationTraceDeploymentId = (
  fraudProofCataloguePolicyId: string,
): string => {
  if (!/^[0-9a-fA-F]{56}$/u.test(fraudProofCataloguePolicyId)) {
    throw new Error(
      "Fraud-proof catalogue policy id must be exactly 28 bytes of hexadecimal",
    );
  }
  const deploymentId = toHex(
    blake2b(fromHex(fraudProofCataloguePolicyId), { dkLen: 32 }),
  );
  // The stage-one script-sources validators take this id as a plain
  // `ByteArray` parameter and used to re-check its 32-byte width on every
  // execution. Deployment parameterization is trusted on chain, so the width is
  // asserted once here, where the value is produced, instead.
  if (!/^[0-9a-f]{64}$/u.test(deploymentId)) {
    throw new Error(
      "Validation-trace deployment id must be exactly 32 bytes of hexadecimal",
    );
  }
  return deploymentId;
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
          const candidateParameter = parameter as {
            readonly title?: unknown;
            readonly schema?: unknown;
            readonly schemaRef?: unknown;
          };
          const parameterTitle = candidateParameter.title;
          if (typeof parameterTitle !== "string") {
            throw new Error(
              `validators[${index}].parameters[${parameterIndex}].title must be a string`,
            );
          }
          const schemaRef =
            typeof candidateParameter.schema === "object" &&
            candidateParameter.schema !== null
              ? (candidateParameter.schema as { readonly $ref?: unknown }).$ref
              : candidateParameter.schemaRef;
          return typeof schemaRef === "string"
            ? { title: parameterTitle, schemaRef }
            : { title: parameterTitle };
        }),
      };
    }),
  };
};

export const getBlueprintValidator = (
  blueprint: FaultProofBlueprint,
  title: string,
): FaultProofBlueprintValidator => {
  const matches = blueprint.validators.filter(
    (validator) => validator.title === title,
  );
  if (matches.length > 1) {
    throw new Error(
      `Blueprint must contain exactly one validator with title "${title}"; found ${matches.length.toString()}.`,
    );
  }
  const found = matches[0];
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
 * script, and the only permitted caller of {@link applyParamsToScriptExactly}.
 *
 * Parameter application applies whatever list it is handed and never checks it
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
  assertParameterShapes(validator, params);
  const cacheKey = appliedScriptCacheKey(validator.compiledCode, params);
  const cached = appliedScriptCache.get(cacheKey);
  if (cached !== undefined) {
    return cached;
  }
  const applied = applyParamsToScriptExactly(validator.compiledCode, params);
  appliedScriptCache.set(cacheKey, applied);
  return applied;
};

/**
 * Applies parameters with the Aiken `uplc` crate — the code `aiken blueprint
 * apply` itself runs — compiled to wasm, rather than with Lucid's JavaScript
 * `applyParamsToScript`. The two produce byte-identical scripts (checked over
 * every parameterised validator of the blueprint with four parameter samples
 * each: 4,536 applications, no difference), and the wasm path is roughly ten
 * times faster, which matters because every fault-proof test process applies
 * a whole family's validators once before its first transaction.
 *
 * The blueprint's `compiledCode` is the flat program wrapped in one CBOR
 * bytestring; `apply_params_to_script` takes exactly that and the parameters
 * as one CBOR array of Plutus data, and returns the applied program wrapped
 * the same way. The result is double-wrapped like every other script this
 * package hands to Lucid.
 */
const applyParamsToScriptExactly = (
  compiledCode: string,
  params: readonly Data[],
): string => {
  const singleWrapped = readCborBytes(
    fromHex(applyDoubleCborEncoding(compiledCode)),
    0,
    "compiledCode",
  ).value;
  const applied = UPLC.apply_params_to_script(
    encodeCborArrayRaw(params.map((param) => fromHex(Data.to(param)))),
    singleWrapped,
  );
  return applyDoubleCborEncoding(toHex(applied));
};

/**
 * Blueprint definitions whose values are 28-byte Blake2b-224 hashes. A term
 * applied to such a parameter must be exactly 56 hexadecimal characters.
 */
const HASH28_DEFINITION_REFS: ReadonlySet<string> = new Set([
  "#/definitions/aiken~1crypto~1ScriptHash",
  "#/definitions/cardano~1assets~1PolicyId",
  "#/definitions/aiken~1crypto~1VerificationKeyHash",
]);

const INT_DEFINITION_REF = "#/definitions/Int";
const BYTEARRAY_DEFINITION_REF = "#/definitions/ByteArray";
const LIST_DEFINITION_REF = /^#\/definitions\/List<(?<element>.+)>$/u;

const HEX28 = /^[0-9a-f]{56}$/u;
const HEX = /^(?:[0-9a-f]{2})*$/u;

const describeShape = (value: Data): string =>
  typeof value === "string"
    ? `${(value.length / 2).toString()}-byte bytestring`
    : typeof value === "bigint"
      ? "integer"
      : Array.isArray(value)
        ? `list of ${value.length.toString()}`
        : "constructor or map";

/**
 * Checks one applied term against the parameter's declared definition. Only
 * definitions with a fixed on-chain shape are checked; `Address` and other
 * structured parameters are encoded by dedicated helpers and pass through.
 */
const assertParameterShape = (
  validatorTitle: string,
  parameterTitle: string,
  ref: string,
  value: Data,
): void => {
  const refuse = (expected: string): never => {
    throw new Error(
      `Blueprint validator "${validatorTitle}" parameter "${parameterTitle}" ` +
        `is declared ${ref.replace("#/definitions/", "").replace(/~1/gu, "/")} ` +
        `and must be ${expected}, but a ${describeShape(value)} was applied. ` +
        "On-chain code trusts deployment parameters; this is the only check.",
    );
  };
  if (HASH28_DEFINITION_REFS.has(ref)) {
    if (typeof value !== "string" || !HEX28.test(value)) {
      refuse("a 28-byte hash as 56 lowercase hexadecimal characters");
    }
    return;
  }
  if (ref === INT_DEFINITION_REF) {
    if (typeof value !== "bigint") {
      refuse("an integer");
    }
    return;
  }
  if (ref === BYTEARRAY_DEFINITION_REF) {
    if (typeof value !== "string" || !HEX.test(value)) {
      refuse("a bytestring as lowercase hexadecimal characters");
    }
    return;
  }
  const list = LIST_DEFINITION_REF.exec(ref);
  const elementDefinition = list?.groups?.["element"];
  if (elementDefinition !== undefined) {
    const elements: readonly Data[] = Array.isArray(value)
      ? value
      : refuse("a list");
    const elementRef = `#/definitions/${elementDefinition}`;
    elements.forEach((element, index) => {
      assertParameterShape(
        validatorTitle,
        `${parameterTitle}[${index.toString()}]`,
        elementRef,
        element,
      );
    });
  }
};

/**
 * Midgard's on-chain code assumes every deployed script was parameterized
 * honestly and correctly, so validators do not re-check the width of a script
 * hash parameter, the cardinality of a deployed resolver list, or the domain
 * of a deployment constant on every execution — doing so spent execution units
 * re-proving a fixed fact. Those checks belong exactly here, at the one place
 * this package applies parameters, and they are driven by the compiler's own
 * declared parameter types rather than by a per-family list that could go
 * stale: every parameter declared as a 28-byte hash, an integer, a bytestring,
 * or a list of those is checked against the term actually applied to it.
 * Cardinalities and value domains that are not expressible in the blueprint
 * schema (a phase group's resolver count, the supported network ids) are
 * asserted by the family builders next to the constant they mirror.
 */
export const assertParameterShapes = (
  validator: FaultProofBlueprintValidator,
  params: readonly Data[],
): void => {
  declaredParameters(validator).forEach((parameter, index) => {
    const value = params[index];
    if (parameter.schemaRef !== undefined && value !== undefined) {
      assertParameterShape(
        validator.title,
        parameter.title,
        parameter.schemaRef,
        value,
      );
    }
  });
};

/**
 * Parameter application is pure — the applied script is a function of nothing
 * but the compiled code and the CBOR of the parameters — and it dominated
 * contract construction with the JavaScript applier (3–65 ms per validator,
 * ~14 s across a full fault-proof contract build; the wasm applier above cuts
 * that to roughly a tenth). Memoizing on the exact inputs therefore cannot
 * change any deployed byte: a cache hit is a proof the inputs were identical.
 * The #609 arity guard above runs before the lookup on every call, cached or
 * not, so under-/over-application still fails closed.
 */
const appliedScriptCache = new Map<string, string>();

// The key is the exact inputs themselves rather than a digest of them: a
// digest bought nothing (the map already compares keys byte-for-byte) and
// hashing tens of kilobytes of compiled code in JavaScript on every lookup
// cost more than the cache saved for contract builds that hit it repeatedly.
const appliedScriptCacheKey = (
  compiledCode: string,
  params: readonly Data[],
): string =>
  `${compiledCode}|${params.map((param) => Data.to(param)).join("|")}`;

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

/** Link a minting policy to its spending validator in declared parameter order. */
export const buildAuthenticatedBlueprintValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  titles: { readonly mint: string; readonly spend: string },
  mintParams: readonly Data[],
  spendParams?: (policyId: string) => readonly Data[],
): AuthenticatedValidator => {
  const minting = makeMintingPolicy(
    applyBlueprintParams(blueprint, titles.mint, mintParams),
  );
  const spendingScript =
    spendParams === undefined
      ? getUnappliedScript(blueprint, titles.spend)
      : applyBlueprintParams(
          blueprint,
          titles.spend,
          spendParams(minting.policyId),
        );
  return { ...makeSpendingValidator(network, spendingScript), ...minting };
};

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

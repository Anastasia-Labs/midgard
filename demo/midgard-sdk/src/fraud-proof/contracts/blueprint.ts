/**
 * Blueprint parsing and parameter application for fault-proof validators.
 */

import "@al-ft/midgard-core/codec";
import "@lucid-evolution/lucid";
import "@lucid-evolution/uplc";
import "@noble/hashes/blake2.js";
import "effect";
import "../../common.js";
import "./blueprint.parse-fault-proof-blueprint.js";
import "./blueprint.get-unapplied-script.js";

import { encodeCborArrayRaw, readCborBytes } from "@al-ft/midgard-core/codec";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  releaseRecord,
  releaseString,
} from "@al-ft/midgard-core/deployment-release";
import {
  applyDoubleCborEncoding,
  Data,
  fromHex,
  Network,
  toHex,
} from "@lucid-evolution/lucid";
import * as UPLC from "@lucid-evolution/uplc";

import { AuthenticatedValidator } from "../../common.js";
import {
  getUnappliedScript,
  makeMintingPolicy,
  makeSpendingValidator,
} from "./blueprint.get-unapplied-script.js";
import {
  appliedScriptCache,
  appliedScriptCacheKey,
  assertParameterShapes,
  declaredParameters,
  describeDeclaredParameters,
  type FaultProofBlueprint,
  getBlueprintValidator,
} from "./blueprint.parse-fault-proof-blueprint.js";
import {
  type BlueprintDeploymentScript,
  buildBlueprintDeploymentRegistry,
  recordBlueprintApplication,
} from "./blueprint-applications.js";

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
    recordBlueprintApplication(
      blueprint,
      title,
      params.map((param) => Data.to(param)),
      cached,
    );
    return cached;
  }
  const applied = applyParamsToScriptExactly(validator.compiledCode, params);
  appliedScriptCache.set(cacheKey, applied);
  recordBlueprintApplication(
    blueprint,
    title,
    params.map((param) => Data.to(param)),
    applied,
  );
  return applied;
};

/** The parameter through which a thread holder releases its thread at expiry. */
export const THREAD_EXPIRY_PARAMETER_TITLE = "thread_expiry_script_hash";

/**
 * Applies a validation-trace thread holder's parameters. A holder that
 * declares the thread expiry script hash takes it as its LAST parameter, after
 * the parameters the caller passes; a validator that does not declare it is
 * applied with the caller's parameters alone. The declared list stays the only
 * authority, so a holder declaring the hash anywhere but last is refused, and
 * {@link applyBlueprintParams} still refuses any other arity mismatch.
 */
export const applyThreadHolderParams = (
  blueprint: FaultProofBlueprint,
  title: string,
  params: readonly Data[],
  threadExpiryScriptHash: string,
): string => {
  const declared = declaredParameters(getBlueprintValidator(blueprint, title));
  const expiryPosition = declared.findIndex(
    ({ title: parameterTitle }) =>
      parameterTitle === THREAD_EXPIRY_PARAMETER_TITLE,
  );
  if (expiryPosition === -1) {
    return applyBlueprintParams(blueprint, title, params);
  }
  if (expiryPosition !== declared.length - 1) {
    throw new Error(
      `Blueprint validator "${title}" declares ` +
        `${THREAD_EXPIRY_PARAMETER_TITLE} at position ` +
        `${expiryPosition.toString()}, not last`,
    );
  }
  return applyBlueprintParams(blueprint, title, [
    ...params,
    threadExpiryScriptHash,
  ]);
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
export {
  asAddressDataParam,
  getUnappliedScript,
  makeAuthenticatedValidator,
  makeMintingPolicy,
  makeSpendingValidator,
  makeWithdrawalValidator,
  tryBuild,
} from "./blueprint.get-unapplied-script.js";
export {
  assertParameterShapes,
  declaredParameters,
  deriveValidationTraceDeploymentId,
  type FaultProofBlueprint,
  type FaultProofBlueprintParameter,
  type FaultProofBlueprintValidator,
  getBlueprintValidator,
  parseFaultProofBlueprint,
} from "./blueprint.parse-fault-proof-blueprint.js";
export {
  type BlueprintApplicationRecord,
  blueprintApplicationRecords,
  type BlueprintDeploymentScript,
  buildBlueprintDeploymentRegistry,
} from "./blueprint-applications.js";

/** Independently reapplies every retained parameter record before a release signs it. */
export const verifyBlueprintDeploymentRegistry = (
  blueprint: FaultProofBlueprint,
  value: unknown,
) => {
  const registry = releaseRecord(value, "$.deploymentRegistry", [
    "schemaVersion",
    "validators",
    "contracts",
  ]);
  if (
    registry.schemaVersion !== "midgard-blueprint-deployment-registry-v1" ||
    !Array.isArray(registry.validators) ||
    !Array.isArray(registry.contracts)
  )
    throw new Error("Invalid blueprint deployment registry");
  const nonDeployed: Record<string, string> = Object.create(null) as Record<
    string,
    string
  >;
  registry.validators.forEach((value, index) => {
    const path = `$.deploymentRegistry.validators[${index.toString()}]`;
    const entry = releaseRecord(
      value,
      path,
      ["title", "disposition"],
      ["contracts", "reason"],
    );
    const title = releaseString(entry.title, `${path}.title`, /^.+$/u);
    if (entry.disposition === "not_deployed") {
      nonDeployed[title] = releaseString(
        entry.reason,
        `${path}.reason`,
        /^.+$/u,
      );
    } else if (entry.disposition !== "deployed")
      throw new Error("Invalid blueprint validator disposition");
  });
  const scripts: BlueprintDeploymentScript[] = registry.contracts.map(
    (value, index) => {
      const path = `$.deploymentRegistry.contracts[${index.toString()}]`;
      const entry = releaseRecord(value, path, [
        "contract",
        "purpose",
        "script",
        "scriptHash",
        "referenceScriptRole",
        "application",
      ]);
      const script = releaseRecord(entry.script, `${path}.script`, [
        "type",
        "script",
      ]);
      if (script.type !== "PlutusV3" && script.type !== "Native")
        throw new Error("Invalid deployment registry script language");
      if (
        entry.purpose !== "spend" &&
        entry.purpose !== "mint" &&
        entry.purpose !== "withdraw"
      )
        throw new Error("Invalid deployment registry purpose");
      const scriptCbor = releaseString(
        script.script,
        `${path}.script.script`,
        /^(?:[0-9a-f]{2})+$/u,
      );
      if (script.type === "Native") {
        if (entry.application !== null)
          throw new Error("Native deployment script has Plutus parameters");
      } else {
        const application = releaseRecord(
          entry.application,
          `${path}.application`,
          ["validatorTitle", "parameters", "appliedScriptCborHex"],
        );
        const title = releaseString(
          application.validatorTitle,
          `${path}.application.validatorTitle`,
          /^.+$/u,
        );
        if (!Array.isArray(application.parameters))
          throw new Error("Deployment parameters must be ordered");
        const validator = getBlueprintValidator(blueprint, title);
        const parameters = application.parameters.map(
          (value, parameterIndex) => {
            const parameterPath = `${path}.application.parameters[${parameterIndex.toString()}]`;
            const parameter = releaseRecord(
              value,
              parameterPath,
              ["title", "cborHex"],
              ["schemaRef"],
            );
            const declared = declaredParameters(validator)[parameterIndex];
            if (
              declared === undefined ||
              parameter.title !== declared.title ||
              parameter.schemaRef !== declared.schemaRef
            )
              throw new Error(
                "Retained parameter declaration differs from raw blueprint",
              );
            const cbor = releaseString(
              parameter.cborHex,
              `${parameterPath}.cborHex`,
              /^(?:[0-9a-f]{2})+$/u,
            );
            const decoded = Data.from(cbor);
            if (Data.to(decoded) !== cbor)
              throw new Error("Deployment parameter CBOR is not canonical");
            return decoded;
          },
        );
        const applied = applyBlueprintParams(blueprint, title, parameters);
        if (
          applied !== applyDoubleCborEncoding(scriptCbor) ||
          applied !== application.appliedScriptCborHex
        )
          throw new Error(
            "Retained parameters do not produce the deployed script",
          );
      }
      return {
        contract: releaseString(
          entry.contract,
          `${path}.contract`,
          /^[a-z][A-Za-z0-9]*$/u,
        ),
        purpose: entry.purpose,
        script: { type: script.type, script: scriptCbor },
        scriptHash: releaseString(
          entry.scriptHash,
          `${path}.scriptHash`,
          /^[0-9a-f]{56}$/u,
        ),
        referenceScriptRole:
          entry.referenceScriptRole === null
            ? null
            : releaseString(
                entry.referenceScriptRole,
                `${path}.referenceScriptRole`,
                /^.+$/u,
              ),
      };
    },
  );
  const rebuilt = buildBlueprintDeploymentRegistry(
    blueprint,
    scripts,
    nonDeployed,
  );
  if (
    computeDeploymentManifestJsonDigest(rebuilt) !==
    computeDeploymentManifestJsonDigest(registry)
  )
    throw new Error(
      "Retained deployment registry differs from its reconstructed census",
    );
  return rebuilt;
};

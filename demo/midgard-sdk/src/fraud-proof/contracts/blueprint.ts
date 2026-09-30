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

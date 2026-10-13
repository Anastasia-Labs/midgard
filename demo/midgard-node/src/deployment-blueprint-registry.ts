import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { releaseRecord } from "@al-ft/midgard-core/deployment-release";
import * as SDK from "@al-ft/midgard-sdk";
import { applyDoubleCborEncoding } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type DeployableScript } from "./deployable-scripts.fault-proof-step-contract-names.js";
import {
  cachedRealBlueprint,
  loadRealBlueprint,
} from "./services/midgard-contracts.load-reference-script-auth-validator.js";

/** Public publication has no implicit bucket for an unrequested raw validator. */
export const publicDeploymentBlueprintRegistry = (
  scripts: readonly DeployableScript[],
) => {
  if (!["preprod-public", "mainnet"].includes(SELECTED_DEPLOYMENT_PROFILE.name))
    return undefined;
  const path = process.env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE;
  if (path === undefined || !isAbsolute(path))
    throw new Error(
      "Public deployment requires MIDGARD_BLUEPRINT_CLASSIFICATION_FILE as an absolute explicit validator-classification file",
    );
  const blueprint =
    cachedRealBlueprint?.blueprint ?? Effect.runSync(loadRealBlueprint());
  if (blueprint === undefined)
    throw new Error(
      "Public deployment has no authenticated real blueprint application context",
    );
  const nonDeployedValidators: unknown = JSON.parse(readFileSync(path, "utf8"));
  if (
    typeof nonDeployedValidators !== "object" ||
    nonDeployedValidators === null ||
    Array.isArray(nonDeployedValidators) ||
    Object.values(nonDeployedValidators).some(
      (reason) => typeof reason !== "string" || reason.trim() === "",
    )
  )
    throw new Error(
      "Blueprint classifications must map exact unused validator titles to nonempty reasons",
    );
  // PHAS is loaded through its own parser. Zero declared parameters and exact
  // raw bytes suffice to record that same application on this shared context.
  for (const entry of scripts) {
    if (entry.script.type !== "PlutusV3") continue;
    const script = applyDoubleCborEncoding(entry.script.script);
    if (
      SDK.blueprintApplicationRecords(blueprint).some(
        (record) => record.appliedScriptCborHex === script,
      )
    )
      continue;
    const sameBytes = blueprint.validators.filter(
      (validator) =>
        validator.parameters.length === 0 &&
        applyDoubleCborEncoding(validator.compiledCode) === script,
    );
    const samePurpose = sameBytes.filter((validator) =>
      validator.title.endsWith(`.${entry.purpose}`),
    );
    const bare =
      samePurpose.length > 0
        ? samePurpose
        : entry.purpose === "withdraw"
          ? sameBytes.filter((validator) => validator.title.endsWith(".else"))
          : [];
    if (bare.length === 1) SDK.getUnappliedScript(blueprint, bare[0]!.title);
  }
  const deployed = scripts.map((entry) => {
    if (entry.script.type !== "PlutusV3" && entry.script.type !== "Native")
      throw new Error(
        `Unsupported public deployment script language: ${entry.contract}`,
      );
    return {
      contract: entry.contract,
      purpose: entry.purpose,
      script: { type: entry.script.type, script: entry.script.script },
      scriptHash: entry.scriptHash,
      referenceScriptRole: entry.role ?? null,
    };
  });
  const retainedPath = process.env.MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE;
  if (retainedPath !== undefined) {
    if (!isAbsolute(retainedPath))
      throw new Error(
        "Retained deployment parameter registry path must be absolute",
      );
    const retained = releaseRecord(
      JSON.parse(readFileSync(retainedPath, "utf8")),
      "$.parameterRegistry",
      [
        "schemaVersion",
        "manifestId",
        "blueprintHash",
        "deployment",
        "referenceScripts",
      ],
    );
    if (
      retained.schemaVersion !== "midgard-deployment-parameter-registry-v1" ||
      retained.blueprintHash !==
        createHash("sha256")
          .update(readFileSync(cachedRealBlueprint!.path))
          .digest("hex")
    )
      throw new Error(
        "Retained parameter registry differs from actual blueprint input",
      );
    const reconstructed = SDK.verifyBlueprintDeploymentRegistry(
      blueprint,
      retained.deployment,
    );
    const retainedScripts = reconstructed.contracts.map(
      ({ application: _application, ...entry }) => entry,
    );
    if (
      computeDeploymentManifestJsonDigest(retainedScripts) !==
      computeDeploymentManifestJsonDigest(deployed)
    )
      throw new Error(
        "Retained parameter registry differs from the actual deployment contracts",
      );
  }
  return SDK.buildBlueprintDeploymentRegistry(
    blueprint,
    deployed,
    nonDeployedValidators as Readonly<Record<string, string>>,
  );
};

/**
 * The installed scene's finalized deployment manifest, which the watcher's
 * signed identity binds: the fixture's signed deployment carrying the
 * scene's contracts by their manifest names, its fraud-proof catalogue, its
 * reference-script auth policy and hub-oracle one-shot, and the removal's
 * published reference scripts. A removal run under it is funded as the
 * deployment's own, its bond-backed slash among them.
 */
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
  type DeploymentManifestFraudProofCatalogueCategory,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Script, validatorToScriptHash } from "@lucid-evolution/lucid";

import type { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { signedDeploymentFixture } from "./validation-trace-dispute-funded-roster.js";
import type { InstalledScene } from "./validation-trace-dispute-installed-scene.js";

type Document = ReturnType<typeof buildRemovalDeploymentInfo>;

type Entry = Readonly<{
  scriptHash: string;
  refScriptUTxO?: Readonly<{ txHash: string; outputIndex: number }> | null;
}>;

const SCRIPT_TYPES = new Set(["Native", "PlutusV1", "PlutusV2", "PlutusV3"]);

/** Every script inside `value`, by its hash. */
const scriptsByHash = (value: unknown): ReadonlyMap<string, Script> => {
  const found = new Map<string, Script>();
  const seen = new Set<object>();
  const walk = (inner: unknown): void => {
    if (typeof inner !== "object" || inner === null || seen.has(inner)) return;
    seen.add(inner);
    const { type, script } = inner as Partial<Script>;
    if (typeof script === "string" && SCRIPT_TYPES.has(String(type))) {
      const found_ = { type: type!, script };
      found.set(validatorToScriptHash(found_), found_);
      return;
    }
    for (const nested of Object.values(inner)) walk(nested);
  };
  walk(value);
  return found;
};

/**
 * The fixture's signed deployment over the installed scene, whose manifest
 * describes the deployment `document` (the scene's legacy deployment
 * document) names: each of its contracts the manifest names, with the
 * script the scene deployed under that hash and the reference the scene
 * published for it.
 */
export const installedSceneSignedDeployment = (
  { scene }: InstalledScene,
  document: Document,
) => {
  const { h } = scene;
  const scripts = scriptsByHash([h.contracts, scene.chain]);
  const scriptOf = (hash: string): Script => {
    const script = scripts.get(hash);
    if (script === undefined)
      throw new Error(`the installed scene deploys no script ${hash}`);
    return script;
  };
  const names = new Set<string>(DEPLOYMENT_MANIFEST_CONTRACT_NAMES);
  const auth = document.referenceScriptAuthPolicy as Readonly<{
    policyId: string;
    nativeScript: Readonly<{ cborHex: string }>;
  }>;
  return signedDeploymentFixture(scene, {
    options: {
      hubOracleOneShotOutRef: `${h.nonceUtxo.txHash}#${h.nonceUtxo.outputIndex.toString()}`,
      referenceScriptAuthPolicy: auth,
    },
    overlay: (contractSet, deployed) => {
      for (const [name, entry] of Object.entries(
        document.contracts as Readonly<Record<string, Entry>>,
      ))
        if (names.has(name))
          deployed(
            name,
            scriptOf(entry.scriptHash),
            entry.scriptHash,
            entry.refScriptUTxO ?? undefined,
          );
      const { categories } = h.catalogue;
      for (const [category, { scriptHash }] of Object.entries(categories))
        deployed(
          DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[
            category as DeploymentManifestFraudProofCatalogueCategory
          ],
          scriptOf(scriptHash),
          scriptHash,
        );
      deployed(
        "referenceScriptAuthMint",
        { type: "Native", script: auth.nativeScript.cborHex },
        auth.policyId,
      );
      for (const [role, reference] of Object.entries(
        contractSet.referenceScripts,
      ))
        contractSet.referenceScripts[role] = {
          ...reference,
          roleUnit:
            auth.policyId +
            Buffer.from(
              DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
                role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
              ],
              "utf8",
            ).toString("hex"),
        };
      const catalogue = { root: h.catalogue.root, categories } as never;
      contractSet.contracts.fraudProofCatalogueMint!.fraudProofCatalogue =
        catalogue;
      return { ...contractSet, fraudProofCatalogue: catalogue };
    },
  });
};

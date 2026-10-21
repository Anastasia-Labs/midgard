/** The one-step resolution on the Open scene: the reference scripts of the
 * holders a thread of ours visits past the game, published once, and the
 * deployment the runner drives the resolution against; with CEK script
 * selections, the selection's yields under their roles' authentication
 * tokens and the script-decode deployment whose records it reads. */
import type { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

import {
  cekCoreSemanticResolver,
  cekSelectionSemanticResolver,
  VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import { CEK_RESOLVER_INDEX } from "../../src/validation-trace-dispute/one-step-plan.js";
import type {
  ValidationTraceDisputeDeployment,
  ValidationTraceOneStepReferences,
} from "../../src/validation-trace-dispute/transactions.js";
import { publishAuthenticatedValidationDisputeControl } from "./emulator/reference-scripts.js";
import {
  registerEmulatorRewardAccount,
  scriptDecodeDeploymentOf,
} from "./script-decode-emulator.js";
import { publishPlainReferenceScriptUtxo } from "./submit-init-emulator-shared.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";
import { runnerDeployment } from "./validation-trace-dispute-runner.js";

type OneStepScript = Readonly<{
  spendingScript: Script;
  spendingScriptHash: string;
}>;

/** The scene's script for each holder a manifest names among the one-step
 * holders, by that holder. */
export const oneStepScripts = (
  scene: Scene,
): Readonly<
  Record<keyof typeof VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES, OneStepScript>
> => {
  const { chain } = scene;
  const arms = chain.cekCoreStages;
  const deployment = runnerDeployment(scene);
  const resolver = ({ spendingScriptHash }: { spendingScriptHash: string }) =>
    chain.semanticResolvers.find(
      (found) => found.spendingScriptHash === spendingScriptHash,
    )!;
  return Object.freeze({
    boundary: chain.boundary,
    prepare: chain.prepareResolvers[CEK_RESOLVER_INDEX],
    core: resolver(cekCoreSemanticResolver(deployment)),
    selection: resolver(cekSelectionSemanticResolver(deployment)),
    compute: arms.compute,
    machine: arms.machine,
    mapConversion: arms.mapConversion,
    caseConstant: arms.caseConstant,
    settle: arms.settle,
  });
};

/** Publishes the reference scripts of the one-step holders. */
export const publishOneStepReferences = async (
  scene: Scene,
): Promise<ValidationTraceOneStepReferences> => {
  const publish = async ({ spendingScript }: OneStepScript, label: string) =>
    (
      await publishPlainReferenceScriptUtxo({
        lucid: scene.h.proverLucid,
        script: spendingScript,
        label,
      })
    ).utxo;
  const scripts = oneStepScripts(scene);
  return Object.freeze({
    boundary: await publish(scripts.boundary, "boundary"),
    cekPrepare: await publish(scripts.prepare, "CEK resolver"),
    cekCore: await publish(scripts.core, "CEK core step"),
    settle: await publish(scripts.settle, "CEK core settle"),
    arms: Object.freeze({
      compute: await publish(scripts.compute, "CEK compute arm"),
      machine: await publish(scripts.machine, "CEK machine arm"),
      mapConversion: await publish(
        scripts.mapConversion,
        "CEK map-conversion arm",
      ),
      caseConstant: await publish(
        scripts.caseConstant,
        "CEK case-constant arm",
      ),
    }),
  });
};

/**
 * Publishes the selection resolver's reference script, and its two yields'
 * under their roles' authentication tokens, with the yields' reward accounts
 * registered.
 */
export const publishSelectionReferences = async (
  scene: Scene,
): Promise<NonNullable<ValidationTraceOneStepReferences["cekSelection"]>> => {
  const { h, chain } = scene;
  const yield_ = async (
    name:
      | "V1 validation-trace CEK selection authenticate yield"
      | "V1 validation-trace CEK selection successor yield",
    script: Script,
  ) => {
    registerEmulatorRewardAccount(h.emulator, script);
    return (
      await publishAuthenticatedValidationDisputeControl({
        lucid: h.proverLucid,
        authPolicy: h.contracts.referenceScriptAuth as Awaited<
          ReturnType<typeof createReferenceScriptAuthPolicy>
        >,
        publisher: h.contracts.referenceScriptPublisher,
        target: { name, control: name, script },
      })
    ).utxo;
  };
  return Object.freeze({
    semantic: (
      await publishPlainReferenceScriptUtxo({
        lucid: h.proverLucid,
        script: oneStepScripts(scene).selection.spendingScript,
        label: "CEK execution selection",
      })
    ).utxo,
    authenticate: await yield_(
      "V1 validation-trace CEK selection authenticate yield",
      chain.yields.cekSelectionAuthenticate.withdrawalScript,
    ),
    successor: await yield_(
      "V1 validation-trace CEK selection successor yield",
      chain.yields.cekSelectionSuccessor.withdrawalScript,
    ),
  });
};

/**
 * The runner's deployment with the one-step holders' references; with
 * `selections`, it drives CEK script selections over the emulator's
 * script-decode deployment.
 */
export const oneStepDeployment = (
  scene: Scene,
  oneStep: ValidationTraceOneStepReferences,
  deploymentFingerprint?: string,
): ValidationTraceDisputeDeployment => {
  const deployment = runnerDeployment(scene, deploymentFingerprint);
  return {
    ...deployment,
    references: { ...deployment.references, oneStep },
    ...(oneStep.cekSelection === undefined
      ? {}
      : { scriptDecode: scriptDecodeDeploymentOf(scene.h.emulator) }),
  };
};

/** The one-step resolution on the Open scene: the reference scripts of the
 * holders a thread of ours visits past the game, published once, and the
 * deployment the runner drives the resolution against. */
import type { Script } from "@lucid-evolution/lucid";

import { cekCoreSemanticResolver } from "../../src/validation-trace-dispute/one-step-holders.js";
import { CEK_RESOLVER_INDEX } from "../../src/validation-trace-dispute/one-step-plan.js";
import type {
  ValidationTraceDisputeDeployment,
  ValidationTraceOneStepReferences,
} from "../../src/validation-trace-dispute/transactions.js";
import { publishPlainReferenceScriptUtxo } from "./submit-init-emulator-shared.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";
import { runnerDeployment } from "./validation-trace-dispute-runner.js";

/** Publishes the reference scripts of the one-step holders. */
export const publishOneStepReferences = async (
  scene: Scene,
): Promise<ValidationTraceOneStepReferences> => {
  const { h, chain } = scene;
  const publish = async (script: Script, label: string) =>
    (
      await publishPlainReferenceScriptUtxo({
        lucid: h.proverLucid,
        script,
        label,
      })
    ).utxo;
  const prepare = chain.prepareResolvers[CEK_RESOLVER_INDEX];
  const deployment = runnerDeployment(scene);
  const arms = chain.cekCoreStages;
  return Object.freeze({
    boundary: await publish(chain.boundary.spendingScript, "boundary"),
    cekPrepare: await publish(prepare.spendingScript, "CEK resolver"),
    cekCore: await publish(
      chain.semanticResolvers.find(
        (resolver) =>
          resolver.spendingScriptHash ===
          cekCoreSemanticResolver(deployment).spendingScriptHash,
      )!.spendingScript,
      "CEK core step",
    ),
    settle: await publish(arms.settle.spendingScript, "CEK core settle"),
    arms: Object.freeze({
      compute: await publish(arms.compute.spendingScript, "CEK compute arm"),
      machine: await publish(arms.machine.spendingScript, "CEK machine arm"),
      mapConversion: await publish(
        arms.mapConversion.spendingScript,
        "CEK map-conversion arm",
      ),
      caseConstant: await publish(
        arms.caseConstant.spendingScript,
        "CEK case-constant arm",
      ),
    }),
  });
};

/** The runner's deployment with the one-step holders' references. */
export const oneStepDeployment = (
  scene: Scene,
  oneStep: ValidationTraceOneStepReferences,
): ValidationTraceDisputeDeployment => {
  const deployment = runnerDeployment(scene);
  return {
    ...deployment,
    references: { ...deployment.references, oneStep },
  };
};

/** The one-step resolution on the Open scene: the reference scripts of the
 * holders a thread of ours visits past the game, published once, and the
 * deployment the runner drives the resolution against; with CEK script
 * selections, the selection's yields under their roles' authentication
 * tokens and the script-decode deployment whose records it reads; the
 * canonical-decode holders; and the phase holders, with the yields their
 * semantic resolvers invoke under their roles' authentication tokens. */
import type { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import type { Script, UTxO } from "@lucid-evolution/lucid";

import {
  CANONICAL_DECODE_HOLDERS,
  canonicalDecodeValidators,
  type ValidationTraceCanonicalDecodeHolder,
} from "../../src/validation-trace-dispute/one-step-canonical-decode.js";
import { DRIVEN_CEK_CONTEXT_STAGES } from "../../src/validation-trace-dispute/one-step-context.js";
import {
  CONTEXT_FOLLOWER_HOLDERS,
  contextFollowerValidators,
  type ValidationTraceContextFollowerHolder,
} from "../../src/validation-trace-dispute/one-step-context-followers.js";
import {
  CONTEXT_ITEM_HOLDERS,
  contextItemValidators,
  type ValidationTraceContextItemHolder,
} from "../../src/validation-trace-dispute/one-step-context-item-holders.js";
import {
  CEK_CONTEXT_STAGE_HOLDERS,
  cekContextSemanticResolver,
  cekCoreSemanticResolver,
  cekSelectionSemanticResolver,
  VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES,
  type ValidationTraceContextStageHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  PHASE_HOLDERS,
  phaseValidators,
  type ValidationTracePhaseHolder,
  type ValidationTracePhaseYield,
} from "../../src/validation-trace-dispute/one-step-phase.js";
import { CEK_RESOLVER_INDEX } from "../../src/validation-trace-dispute/one-step-plan.js";
import type {
  ValidationTraceDisputeDeployment,
  ValidationTraceOneStepReferences,
} from "../../src/validation-trace-dispute/transactions.js";
import { publishAuthenticatedValidationDisputeControl } from "./emulator/reference-scripts.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import { publishPlainReferenceScriptUtxo } from "./submit-init-emulator-shared.js";
import type {
  Scene,
  SceneContracts,
} from "./validation-trace-dispute-open-scene.js";
import { runnerDeployment } from "./validation-trace-dispute-runner.js";

type OneStepScript = Readonly<{
  spendingScript: Script;
  spendingScriptHash: string;
}>;

/** The scene's script for each holder a manifest names among the one-step
 * holders, by that holder. */
export const oneStepScripts = (
  scene: SceneContracts,
): Readonly<
  Record<keyof typeof VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES, OneStepScript>
> => {
  const { chain } = scene;
  const arms = chain.cekCoreStages;
  const context = chain.cekContextStages;
  const stages = {} as Record<ValidationTraceContextStageHolder, OneStepScript>;
  for (const stage of DRIVEN_CEK_CONTEXT_STAGES)
    stages[CEK_CONTEXT_STAGE_HOLDERS[stage]] = context[stage];
  const resolver = ({ spendingScriptHash }: { spendingScriptHash: string }) =>
    chain.semanticResolvers.find(
      (found) => found.spendingScriptHash === spendingScriptHash,
    )!;
  return Object.freeze({
    boundary: chain.boundary,
    prepare: chain.prepareResolvers[CEK_RESOLVER_INDEX],
    core: resolver(cekCoreSemanticResolver(scene)),
    selection: resolver(cekSelectionSemanticResolver(scene)),
    compute: arms.compute,
    machine: arms.machine,
    mapConversion: arms.mapConversion,
    caseConstant: arms.caseConstant,
    settle: arms.settle,
    context: resolver(cekContextSemanticResolver(scene)),
    contextControl: context.control,
    ...stages,
    ...contextItemValidators(chain),
    ...contextFollowerValidators(context),
    contextSettle: context.settle,
    ...canonicalDecodeValidators(chain),
    ...phaseValidators(chain),
  });
};

/** Publishes the reference scripts of the one-step holders. */
export const publishOneStepReferences = async (
  scene: SceneContracts,
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

/** Publishes `script` under the authentication token of the role `name`. */
const publishAuthenticated = async (
  { h }: SceneContracts,
  name: Parameters<
    typeof publishAuthenticatedValidationDisputeControl
  >[0]["target"]["name"],
  script: Script,
): Promise<UTxO> =>
  (
    await publishAuthenticatedValidationDisputeControl({
      lucid: h.proverLucid,
      authPolicy: h.contracts.referenceScriptAuth as Awaited<
        ReturnType<typeof createReferenceScriptAuthPolicy>
      >,
      publisher: h.contracts.referenceScriptPublisher,
      target: { name, control: name, script },
    })
  ).utxo;

/**
 * Publishes the selection resolver's reference script, and its two yields'
 * under their roles' authentication tokens; the scene's setup registers the
 * yields' reward accounts.
 */
export const publishSelectionReferences = async (
  scene: SceneContracts,
): Promise<NonNullable<ValidationTraceOneStepReferences["cekSelection"]>> => {
  const { h, chain } = scene;
  const yield_ = async (
    name:
      | "V1 validation-trace CEK selection authenticate yield"
      | "V1 validation-trace CEK selection successor yield",
    script: Script,
  ) => await publishAuthenticated(scene, name, script);
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

/** Publishes the reference scripts of the CEK context step's holders, of
 * the holders of the redeemer item step its item bind stage binds, and of
 * the holders past a multi-hop stage's first. */
export const publishContextReferences = async (
  scene: SceneContracts,
): Promise<NonNullable<ValidationTraceOneStepReferences["cekContext"]>> => {
  const scripts = oneStepScripts(scene);
  const publish = async ({ spendingScript }: OneStepScript, label: string) =>
    (
      await publishPlainReferenceScriptUtxo({
        lucid: scene.h.proverLucid,
        script: spendingScript,
        label,
      })
    ).utxo;
  const stages: Partial<
    Record<(typeof DRIVEN_CEK_CONTEXT_STAGES)[number], UTxO>
  > = {};
  for (const stage of DRIVEN_CEK_CONTEXT_STAGES)
    stages[stage] = await publish(
      scripts[CEK_CONTEXT_STAGE_HOLDERS[stage]],
      `CEK context ${stage}`,
    );
  const items: Partial<Record<ValidationTraceContextItemHolder, UTxO>> = {};
  for (const holder of CONTEXT_ITEM_HOLDERS)
    items[holder] = await publish(scripts[holder], `CEK ${holder}`);
  const followers: Partial<Record<ValidationTraceContextFollowerHolder, UTxO>> =
    {};
  for (const holder of CONTEXT_FOLLOWER_HOLDERS)
    followers[holder] = await publish(scripts[holder], `CEK ${holder}`);
  return Object.freeze({
    semantic: await publish(scripts.context, "CEK context step"),
    control: await publish(scripts.contextControl, "CEK context control"),
    settle: await publish(scripts.contextSettle, "CEK context settle"),
    stages: Object.freeze(
      stages as Record<(typeof DRIVEN_CEK_CONTEXT_STAGES)[number], UTxO>,
    ),
    items: Object.freeze(
      items as Record<ValidationTraceContextItemHolder, UTxO>,
    ),
    followers: Object.freeze(
      followers as Record<ValidationTraceContextFollowerHolder, UTxO>,
    ),
  });
};

/** Publishes the reference scripts of the canonical-decode holders. */
export const publishCanonicalDecodeReferences = async (
  scene: SceneContracts,
): Promise<
  NonNullable<ValidationTraceOneStepReferences["canonicalDecode"]>
> => {
  const scripts = oneStepScripts(scene);
  const references: Partial<
    Record<ValidationTraceCanonicalDecodeHolder, UTxO>
  > = {};
  for (const holder of CANONICAL_DECODE_HOLDERS)
    references[holder] = (
      await publishPlainReferenceScriptUtxo({
        lucid: scene.h.proverLucid,
        script: scripts[holder].spendingScript,
        label: holder,
      })
    ).utxo;
  return Object.freeze(
    references as Record<ValidationTraceCanonicalDecodeHolder, UTxO>,
  );
};

/** Publishes the reference scripts of the phase holders. */
export const publishPhaseReferences = async (
  scene: SceneContracts,
): Promise<Readonly<Record<ValidationTracePhaseHolder, UTxO>>> => {
  const scripts = oneStepScripts(scene);
  const references: Partial<Record<ValidationTracePhaseHolder, UTxO>> = {};
  for (const holder of PHASE_HOLDERS)
    references[holder] = (
      await publishPlainReferenceScriptUtxo({
        lucid: scene.h.proverLucid,
        script: scripts[holder].spendingScript,
        label: holder,
      })
    ).utxo;
  return Object.freeze(references as Record<ValidationTracePhaseHolder, UTxO>);
};

/** Each phase yield's role. */
const PHASE_YIELD_ROLES = Object.freeze({
  phaseANativeItemNative:
    "V1 validation-trace phase-A native item native yield",
  phaseANativeItemForeign:
    "V1 validation-trace phase-A native item foreign yield",
} as const satisfies Readonly<Record<ValidationTracePhaseYield, string>>);

/** Publishes the yields the phases' semantic resolvers invoke, under their
 * roles' authentication tokens; the scene's setup registers the yields'
 * reward accounts. */
export const publishPhaseYieldReferences = async (
  scene: SceneContracts,
): Promise<Readonly<Record<ValidationTracePhaseYield, UTxO>>> => {
  const references: Partial<Record<ValidationTracePhaseYield, UTxO>> = {};
  for (const [name, role] of Object.entries(PHASE_YIELD_ROLES) as [
    ValidationTracePhaseYield,
    (typeof PHASE_YIELD_ROLES)[ValidationTracePhaseYield],
  ][])
    references[name] = await publishAuthenticated(
      scene,
      role,
      scene.chain.yields[name].withdrawalScript,
    );
  return Object.freeze(references as Record<ValidationTracePhaseYield, UTxO>);
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

/**
 * The validation-trace dispute's Open scene as the installed application
 * runs it: the one-step resolution's references and the removal's
 * published before the scene's blocks, the prover's wallet drained to its
 * funding, and the roster the deployment's manifest names, each published
 * reference by its manifest member.
 */
import { DEPLOYMENT_MEMBERS } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import { VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES } from "../../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceOneStepReferences } from "../../src/validation-trace-dispute/transactions.js";
import { VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS } from "../../src/validation-trace-dispute/workflow.js";
import { SCRIPT_DECODE_SPEND_ROLE } from "../../src/workflow/script-decode-prerequisite.js";
import { drainProver } from "./installed-prover-wallet.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import { publishRemovalReferenceScripts } from "./submit-init-emulator-shared.js";
import { FUNDED } from "./validation-trace-dispute-funded-verify-source.js";
import {
  oneStepScripts,
  publishBuiltinArmReferences,
  publishCanonicalDecodeReferences,
  publishContextReferences,
  publishOneStepReferences,
  publishPhaseReferences,
  publishPhaseYieldReferences,
  publishScriptSourcesRedeemerReferences,
  publishSelectionReferences,
} from "./validation-trace-dispute-one-step.js";
import {
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";

const CATEGORY = "validationTraceDispute" as const;

type Removal = Awaited<ReturnType<typeof publishRemovalReferenceScripts>>;

/** The Open scene with the one-step resolution's references and the
 * removal's published before its blocks, so they take none of a dispute
 * move's time from the failing block's end. */
export type InstalledScene = Readonly<{
  scene: Scene;
  /** The one-step references as the drive tests publish them. */
  oneStep: ValidationTraceOneStepReferences;
  removal: Removal;
}>;

export const setUpInstalledScene = async (): Promise<InstalledScene> => {
  let oneStep: ValidationTraceOneStepReferences | undefined;
  let removal: Removal | undefined;
  const scene = await setUpScene(async (contracts) => {
    oneStep = {
      ...(await publishOneStepReferences(contracts)),
      cekSelection: await publishSelectionReferences(contracts),
      cekContext: await publishContextReferences(contracts),
      cekBuiltins: await publishBuiltinArmReferences(contracts),
      canonicalDecode: await publishCanonicalDecodeReferences(contracts),
      phases: await publishPhaseReferences(contracts),
      phaseYields: await publishPhaseYieldReferences(contracts),
      scriptSourcesRedeemers:
        await publishScriptSourcesRedeemerReferences(contracts),
    };
    removal = await publishRemovalReferenceScripts({
      lucid: contracts.h.proverLucid,
      contracts: contracts.h.contracts,
    });
  });
  // The prover's wallet holds its collateral and the dispute's funding.
  await drainProver(scene.h as never, FUNDED);
  return { scene, oneStep: oneStep!, removal: removal! };
};

/** Every reference UTxO inside `value`. */
const utxosIn = (value: unknown): UTxO[] =>
  typeof value !== "object" || value === null
    ? []
    : "txHash" in value && "outputIndex" in value
      ? [value as UTxO]
      : Object.values(value).flatMap(utxosIn);

export const scriptHashOf = (utxo: UTxO): string =>
  validatorToScriptHash(utxo.scriptRef!);

/**
 * The scene's published references by the manifest member that names each:
 * the dispute's contracts and witnesses, each one-step holder and yield
 * (found by the script the scene deployed for it) and the script-decode
 * deployment.
 */
export const sceneRoster = ({ scene, oneStep }: InstalledScene) => {
  const { h, chain } = scene;
  const byHash = new Map(
    utxosIn(oneStep).map((utxo) => [scriptHashOf(utxo), utxo]),
  );
  const published = (member: string, hash: string): [string, UTxO] => {
    const utxo = byHash.get(hash);
    if (utxo === undefined) throw new Error(`no published script ${member}`);
    return [member, utxo];
  };
  const scripts = oneStepScripts(scene);
  const yields = chain.yields as Readonly<
    Record<
      string,
      { readonly withdrawalScript: Parameters<typeof validatorToScriptHash>[0] }
    >
  >;
  const yieldMembers = Object.entries(DEPLOYMENT_MEMBERS).flatMap(
    ([member, { family, familyPaths }]) =>
      family !== CATEGORY
        ? []
        : familyPaths.flatMap(([kind, path]) => {
            const script = kind === "yields" ? yields[path] : undefined;
            const hash =
              script === undefined
                ? undefined
                : validatorToScriptHash(script.withdrawalScript);
            return hash === undefined || !byHash.has(hash)
              ? []
              : [published(member, hash)];
          }),
  );
  const scriptDecode = scriptDecodeDeploymentOf(h.emulator);
  const witnesses = h.witnessReferenceScripts as Readonly<
    Record<string, UTxO | undefined>
  >;
  return new Map<string, UTxO>([
    ...Object.entries(VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS).map(
      ([role, member]): [string, UTxO] => [
        member,
        scene.references[role as keyof Scene["references"]],
      ],
    ),
    ...[
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
    ].map((role): [string, UTxO] => [role, witnesses[role]!]),
    ...Object.entries(VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES).map(
      ([holder, member]) =>
        published(
          member,
          scripts[holder as keyof typeof scripts].spendingScriptHash,
        ),
    ),
    ...yieldMembers,
    [SCRIPT_DECODE_SPEND_ROLE, scriptDecode.reference],
    ...SDK.SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name): [string, UTxO] => [
      SDK.SCRIPT_DECODE_STAKER_MEMBERS[name],
      scriptDecode.stakerReferences[name],
    ]),
  ]);
};

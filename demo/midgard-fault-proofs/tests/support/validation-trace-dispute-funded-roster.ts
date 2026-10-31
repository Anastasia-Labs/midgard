/** The roster a funded validation-trace dispute's watcher governs: the
 * dispute's contracts by their manifest names, the reference scripts its
 * transactions read, and the fixture's signed deployment carrying the
 * scene's dispute contracts and one-step holders. */
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Script, UTxO } from "@lucid-evolution/lucid";
import {
  makeWatcherAuthorityContracts,
  makeWatcherDeploymentAuthorityFixture,
  type WatcherAuthorityContractSet,
  type WatcherDeploymentAuthorityFixtureOptions,
} from "midgard-watcher/tests/support/deployment-authority-fixture";

import { VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES } from "../../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceDisputeDeployment } from "../../src/validation-trace-dispute/transactions.js";
import { SCRIPT_DECODE_SPEND_ROLE } from "../../src/workflow/script-decode-prerequisite.js";
import { oneStepScripts } from "./validation-trace-dispute-one-step.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** The published reference scripts a transaction of the dispute on
 * `deployment` may read, a script-decode record's among them. */
export const referenceUtxos = (
  scene: Scene,
  deployment: ValidationTraceDisputeDeployment,
): readonly UTxO[] => {
  const { h } = scene;
  const { oneStep } = deployment.references;
  const { scriptDecode } = deployment;
  const byOutRef = new Map<string, UTxO>();
  for (const utxo of [
    ...Object.values(h.faultProofReferenceScripts).map(({ utxo }) => utxo),
    ...Object.values(
      h.witnessReferenceScripts as Readonly<Record<string, UTxO | undefined>>,
    ).filter((utxo): utxo is UTxO => utxo !== undefined),
    ...Object.values(scene.references),
    ...(oneStep === undefined
      ? []
      : [
          oneStep.boundary,
          oneStep.cekPrepare,
          oneStep.cekCore,
          ...Object.values(oneStep.arms),
          ...Object.values(oneStep.cekBuiltins ?? {}),
          oneStep.settle,
          ...Object.values(oneStep.cekSelection ?? {}),
          ...(oneStep.cekContext === undefined
            ? []
            : [
                oneStep.cekContext.semantic,
                oneStep.cekContext.control,
                oneStep.cekContext.settle,
                ...Object.values(oneStep.cekContext.stages),
                ...Object.values(oneStep.cekContext.items),
                ...Object.values(oneStep.cekContext.followers),
              ]),
          ...Object.values(oneStep.canonicalDecode ?? {}),
        ]),
    ...(scriptDecode === undefined
      ? []
      : [
          scriptDecode.reference,
          ...Object.values(scriptDecode.stakerReferences),
        ]),
  ])
    byOutRef.set(`${utxo.txHash}#${utxo.outputIndex.toString()}`, utxo);
  return [...byOutRef.values()];
};

/** The scene's one-step holders' scripts by the names a manifest gives
 * them. */
const oneStepContracts = (scene: Scene) => {
  const scripts = oneStepScripts(scene);
  return Object.entries(VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES).map(
    ([holder, name]) =>
      [name, scripts[holder as keyof typeof scripts]] as const,
  );
};

/** The dispute contracts by the names a manifest gives them, and the
 * script-decode validator a deployment that drives selections names. */
export const scriptHashes = (
  scene: Scene,
  deployment: ValidationTraceDisputeDeployment,
): Readonly<Record<string, string>> => {
  const { chain, h } = scene;
  return {
    ...(deployment.scriptDecode === undefined
      ? {}
      : {
          [SCRIPT_DECODE_SPEND_ROLE]:
            deployment.scriptDecode.validator.spendingScriptHash,
        }),
    ...Object.fromEntries(
      oneStepContracts(scene).map(([name, script]) => [
        name,
        script.spendingScriptHash,
      ]),
    ),
    fraudProofValidationTraceDispute:
      h.contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
    validationTraceDisputeOpener: chain.opener.spendingScriptHash,
    validationTraceDisputeSource: chain.source.spendingScriptHash,
    validationTraceDisputeCase: chain.case.spendingScriptHash,
    validationTraceDisputeBond: chain.disputeBond.spendingScriptHash,
    validationTraceDisputeGame: chain.game.spendingScriptHash,
    validationTraceDisputeTimeout: chain.timeout.spendingScriptHash,
    validationTraceDisputeAward: chain.award.spendingScriptHash,
    // The hub's fraud-proof record, where a case close locks its record.
    fraudProofSpend: h.contracts.fraudProof.spendingScriptHash,
    // A protocol-state contract, as every deployment's roster governs.
    stateQueueSpend: h.contracts.stateQueue.spendingScriptHash,
  };
};

/** Deploys `script` as the manifest's contract `name`, published at
 * `published` when given. */
export type DeployContract = (
  name: string,
  script: Script,
  scriptHash: string,
  published?: Readonly<{ txHash: string; outputIndex: number }>,
) => void;

/** Further contracts of the scene the signed deployment carries, and its
 * other fixture options. */
export type SignedDeploymentOverlay = Readonly<{
  overlay: (
    contractSet: WatcherAuthorityContractSet,
    deployed: DeployContract,
  ) => WatcherAuthorityContractSet;
  options: Omit<WatcherDeploymentAuthorityFixtureOptions, "contractSet">;
}>;

/**
 * The fixture's signed deployment authority, its dispute bond, case, game,
 * timeout and award contracts and its one-step holders (and their
 * reference-script entries) the scene's, and whatever `extended` adds.
 */
export const signedDeploymentFixture = (
  scene: Scene,
  extended?: SignedDeploymentOverlay,
) => {
  const { chain } = scene;
  const contractSet = makeWatcherAuthorityContracts();
  const deployed: DeployContract = (name, script, scriptHash, published) => {
    contractSet.contracts[name] = {
      ...contractSet.contracts[name]!,
      ...(published === undefined
        ? {}
        : {
            refScriptUTxO: {
              txHash: published.txHash,
              outputIndex: published.outputIndex,
            },
          }),
      contract: { type: script.type, cborHex: script.script },
      scriptHash,
    };
    for (const [role, contract] of Object.entries(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ))
      if (contract === name)
        contractSet.referenceScripts[role] = {
          ...contractSet.referenceScripts[role]!,
          scriptHash,
          ...(published === undefined
            ? {}
            : {
                outRef: `${published.txHash}#${published.outputIndex.toString()}`,
              }),
        };
  };
  deployed(
    "validationTraceDisputeBond",
    chain.disputeBond.spendingScript,
    chain.disputeBond.spendingScriptHash,
  );
  deployed(
    "validationTraceDisputeCase",
    chain.case.spendingScript,
    chain.case.spendingScriptHash,
  );
  deployed(
    "validationTraceDisputeCaseMint",
    chain.case.mintingScript,
    chain.case.policyId,
  );
  for (const [name, script] of [
    ["validationTraceDisputeGame", chain.game],
    ["validationTraceDisputeTimeout", chain.timeout],
    ["validationTraceDisputeAward", chain.award],
  ] as const)
    deployed(name, script.spendingScript, script.spendingScriptHash);
  deployed(
    "validationTraceDisputeAwardMint",
    chain.award.mintingScript,
    chain.award.policyId,
  );
  for (const [name, script] of oneStepContracts(scene))
    deployed(name, script.spendingScript, script.spendingScriptHash);
  return makeWatcherDeploymentAuthorityFixture({
    ...extended?.options,
    contractSet: extended?.overlay(contractSet, deployed) ?? contractSet,
  });
};

/** The signed deployment of `signedDeploymentFixture`. */
export const signedDeployment = (scene: Scene) =>
  signedDeploymentFixture(scene).result;

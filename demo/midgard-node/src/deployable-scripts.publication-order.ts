import * as SDK from "@al-ft/midgard-sdk";
import { type ReferenceScriptAuthTokenTarget } from "@al-ft/midgard-sdk";

import {
  type DeployableScriptSpec,
  REFERENCE_SCRIPT_ROLE_BY_CONTRACT,
} from "./deployable-scripts.cek-core-stage-order.js";
import { DEPLOYABLE_SCRIPT_CATALOGUE } from "./deployable-scripts.deployable-script-catalogue.js";
import {
  type DeployableScript,
  type PublishedDeployableScript,
} from "./deployable-scripts.fault-proof-step-contract-names.js";

export type DeployableScriptSectionId =
  keyof typeof DEPLOYABLE_SCRIPT_CATALOGUE;

export const MANIFEST_ORDER = Object.keys(
  DEPLOYABLE_SCRIPT_CATALOGUE,
) as readonly DeployableScriptSectionId[];

type PublicationStep =
  | DeployableScriptSectionId
  | { readonly interleaveByChain: readonly DeployableScriptSectionId[] };

/** Reference-script publication order over the same sections. */
export const PUBLICATION_ORDER: readonly PublicationStep[] = [
  "referenceScriptAuth",
  "hubOracle",
  "daParamsGovernor",
  "daBondPool",
  "daAttestation",
  "scheduler",
  "stateQueue",
  "registeredOperators",
  "activeOperators",
  "retiredOperators",
  "fraudProofCatalogue",
  "computationThread",
  "fraudProofToken",
  "chunkedVerify",
  "pexcludes",
  "depositHistory",
  "withdrawalHistory",
  "depositMint",
  "depositSpend",
  "withdrawalMint",
  "withdrawalSpend",
  "settlement",
  "phasMembership",
  "reserve",
  "payout",
  "availabilityChallenge",
  "fieldPreimageCertificate",
  "cekProgramMaterial",
  "validationTraceDisputeControl",
  // Each legacy family publishes step 01 followed by its later steps.
  {
    interleaveByChain: [
      "legacyFaultProofFirstSteps",
      "legacyFaultProofLaterSteps",
    ],
  },
  "registeredChainsA",
  "registeredChainsB",
  "registeredChainsC",
  "registeredChainAuxiliaries",
  "validationTraceCekCore",
  "validationTraceScriptSourcesSemantics",
  "validationTraceRedeemerItem",
  "validationTraceRedeemerNormalizationSemantic",
  "validationTraceScriptSourcesYields",
  "validationTracePhaseASemantics",
  "validationTraceCekContext",
  "validationTraceCekMaterial",
  "transitionTraceYields",
  "minAdaYields",
  "correctionLock",
  // Sections with no published script, listed so this order covers every
  // section of the catalogue.
  "escapeHatch",
  "txOrder",
];

const interleaveByChain = (
  specs: readonly DeployableScriptSpec[],
): readonly DeployableScriptSpec[] => {
  const chains: string[] = [];
  const byChain = new Map<string, DeployableScriptSpec[]>();
  for (const spec of specs) {
    const chain = spec.chain ?? "";
    let members = byChain.get(chain);
    if (members === undefined) {
      members = [];
      byChain.set(chain, members);
      chains.push(chain);
    }
    members.push(spec);
  }
  return chains.flatMap((chain) => byChain.get(chain) ?? []);
};

const sectionSpecs = (
  contracts: SDK.MidgardValidators,
  step: PublicationStep,
): readonly DeployableScriptSpec[] =>
  typeof step === "string"
    ? DEPLOYABLE_SCRIPT_CATALOGUE[step](contracts)
    : interleaveByChain(
        step.interleaveByChain.flatMap((id) =>
          DEPLOYABLE_SCRIPT_CATALOGUE[id](contracts),
        ),
      );

const referenceScriptRole = (
  spec: DeployableScriptSpec,
): ReferenceScriptAuthTokenTarget | undefined => {
  const role = REFERENCE_SCRIPT_ROLE_BY_CONTRACT.get(spec.contract);
  if (spec.referenceScript && role === undefined) {
    throw new Error(
      `Contract is missing a canonical reference-script role: ${spec.contract}`,
    );
  }
  if (!spec.referenceScript && role !== undefined) {
    throw new Error(
      `Contract ${spec.contract} is not published but carries reference-script role ${role}`,
    );
  }
  return role;
};

const resolveSpec = (spec: DeployableScriptSpec): DeployableScript => ({
  contract: spec.contract,
  role: referenceScriptRole(spec),
  purpose: spec.purpose,
  commands: spec.commands,
  ...spec.resolve(),
});

/** Every script the deployment manifest records, in manifest order. */
export const manifestDeployableScripts = (
  contracts: SDK.MidgardValidators,
): readonly DeployableScript[] =>
  MANIFEST_ORDER.flatMap((id) =>
    DEPLOYABLE_SCRIPT_CATALOGUE[id](contracts).map(resolveSpec),
  );

/** Every script published as a reference script, in publication order. */
export const publishedDeployableScripts = (
  contracts: SDK.MidgardValidators,
): readonly PublishedDeployableScript[] =>
  PUBLICATION_ORDER.flatMap((step) =>
    sectionSpecs(contracts, step).flatMap((spec) => {
      if (!spec.referenceScript || !spec.publishable()) {
        return [];
      }
      const resolved = resolveSpec(spec);
      return resolved.role === undefined
        ? []
        : [{ ...resolved, role: resolved.role }];
    }),
  );

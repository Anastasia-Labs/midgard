import {
  type DeploymentContract,
  deploymentFamilyStepNames,
  deploymentValidatorTitle as deployedTitle,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import { buildDirectBindingContracts } from "../direct-binding.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofDistinctAssetAccumulationLimit"),
  familyEntry: deployedTitle(
    "fraudProofDistinctAssetAccumulationLimitFamilyEntry",
  ),
  inputs: deployedTitle("fraudProofDistinctAssetAccumulationLimitInputs"),
  inputsResolve: deployedTitle(
    "fraudProofDistinctAssetAccumulationLimitInputsResolve",
  ),
  inputRank: deployedTitle("fraudProofDistinctAssetAccumulationLimitInputRank"),
  outputsGrammar: deployedTitle(
    "fraudProofDistinctAssetAccumulationLimitOutputsGrammar",
  ),
  outputs: deployedTitle("fraudProofDistinctAssetAccumulationLimitOutputs"),
  mintGrammar: deployedTitle(
    "fraudProofDistinctAssetAccumulationLimitMintGrammar",
  ),
  mint: deployedTitle("fraudProofDistinctAssetAccumulationLimitMint"),
  terminal: deployedTitle("fraudProofDistinctAssetAccumulationLimitTerminal"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through
 * `distinctAssetAccumulationLimitRoles`, so a manifest-restored chain carries
 * them. */
const DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_MEMBERS = [
  ["step01", "fraudProofDistinctAssetAccumulationLimit"],
  ["bindEvent", "fraudProofDistinctAssetAccumulationLimitBindEvent"],
  ["bindPreState", "fraudProofDistinctAssetAccumulationLimitBindPreState"],
  ["familyEntry", "fraudProofDistinctAssetAccumulationLimitFamilyEntry"],
  ["inputs", "fraudProofDistinctAssetAccumulationLimitInputs"],
  ["inputsResolve", "fraudProofDistinctAssetAccumulationLimitInputsResolve"],
  ["inputRank", "fraudProofDistinctAssetAccumulationLimitInputRank"],
  ["outputsGrammar", "fraudProofDistinctAssetAccumulationLimitOutputsGrammar"],
  ["outputs", "fraudProofDistinctAssetAccumulationLimitOutputs"],
  ["mintGrammar", "fraudProofDistinctAssetAccumulationLimitMintGrammar"],
  ["mint", "fraudProofDistinctAssetAccumulationLimitMint"],
  ["terminal", "fraudProofDistinctAssetAccumulationLimitTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type DistinctAssetAccumulationLimitRole =
  (typeof DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_MEMBERS)[number][0];

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER: readonly DistinctAssetAccumulationLimitRole[] =
  Object.freeze(
    DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_MEMBERS.map(([role]) => role),
  );

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `distinctAssetAccumulationLimit` family. */
export const checkDistinctAssetAccumulationLimitRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_MEMBERS.length)
    throw new Error(
      `distinct-asset-accumulation-limit has ${DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_MEMBERS.forEach(
    ([role, member], index) => {
      if (steps[index] !== member)
        throw new Error(
          `distinct-asset-accumulation-limit role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
        );
    },
  );
};

export type DistinctAssetAccumulationLimitChain = FraudProofChain;

export type DistinctAssetAccumulationLimitRoles = Readonly<
  Record<DistinctAssetAccumulationLimitRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const distinctAssetAccumulationLimitRoles = (
  chain: FraudProofChain,
): DistinctAssetAccumulationLimitRoles => {
  checkDistinctAssetAccumulationLimitRoleOrder(
    deploymentFamilyStepNames("distinctAssetAccumulationLimit"),
  );
  if (
    chain.steps.length !== DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.length
  )
    throw new Error(
      `distinct-asset-accumulation-limit chain has ${chain.steps.length} steps; expected ${DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<DistinctAssetAccumulationLimitRole, SpendingValidator>,
  );
};

export type DistinctAssetAccumulationLimitFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly distinctAssetAccumulationLimit: DistinctAssetAccumulationLimitChain;
};

export type BuildDistinctAssetAccumulationLimitFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared binding, the family's own fold over spent inputs,
 * outputs and mint, and the terminal decision. */
export const buildDistinctAssetAccumulationLimitChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  DistinctAssetAccumulationLimitChain,
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const thread = computationThread.policyId;
    const certificate = fieldPreimageCertificatePolicyId;
    const titles = DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES;
    const step = (title: string, params: readonly Data[], role: string) =>
      buildFaultProofSpendingStep(
        context,
        title,
        params,
        `Failed to build distinct-asset-accumulation-limit ${role}`,
      );
    const terminal = yield* step(
      titles.terminal,
      [fraudProof.policyId, fraudProofTokenAddressData, thread],
      "terminal",
    );
    const terminalHash = terminal.spendingScriptHash;
    const mint = yield* step(
      titles.mint,
      [terminalHash, thread, certificate],
      "mint",
    );
    const mintGrammar = yield* step(
      titles.mintGrammar,
      [mint.spendingScriptHash, terminalHash, thread, certificate],
      "mint grammar",
    );
    const outputs = yield* step(
      titles.outputs,
      [mintGrammar.spendingScriptHash, terminalHash, thread, certificate],
      "outputs",
    );
    const outputsGrammar = yield* step(
      titles.outputsGrammar,
      [
        outputs.spendingScriptHash,
        mintGrammar.spendingScriptHash,
        thread,
        certificate,
      ],
      "outputs grammar",
    );
    const inputRank = yield* step(
      titles.inputRank,
      [terminalHash, thread, certificate],
      "input rank",
    );
    const inputsResolve = yield* step(
      titles.inputsResolve,
      [inputRank.spendingScriptHash, outputsGrammar.spendingScriptHash, thread],
      "inputs resolve",
    );
    const inputs = yield* step(
      titles.inputs,
      [inputsResolve.spendingScriptHash, thread, certificate],
      "inputs",
    );
    const familyEntry = yield* step(
      titles.familyEntry,
      [
        inputs.spendingScriptHash,
        outputsGrammar.spendingScriptHash,
        thread,
        certificate,
      ],
      "family entry",
    );
    const binding = yield* buildDirectBindingContracts({
      blueprint,
      network,
      computationThreadPolicyId: thread,
      nextStepScriptHash: familyEntry.spendingScriptHash,
    });
    const step01 = yield* step(
      titles.step01,
      [binding.event.spendingScriptHash, thread, hubOraclePolicyId],
      "step 01",
    );
    const steps: [SpendingValidator, ...SpendingValidator[]] = [
      step01,
      binding.event,
      binding.preState,
      familyEntry,
      inputs,
      inputsResolve,
      inputRank,
      outputsGrammar,
      outputs,
      mintGrammar,
      mint,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    distinctAssetAccumulationLimitRoles(chain);
    return chain;
  });

export const buildDistinctAssetAccumulationLimitFaultProofContracts = (
  params: BuildDistinctAssetAccumulationLimitFaultProofContractsParams,
): Effect.Effect<DistinctAssetAccumulationLimitFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const distinctAssetAccumulationLimit =
      yield* buildDistinctAssetAccumulationLimitChain({ ...params, ...shared });
    return { ...shared, distinctAssetAccumulationLimit };
  });

import {
  type DeploymentContract,
  deploymentFamilyStepNames,
  deploymentValidatorTitle as deployedTitle,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { FaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofExecutionSourceScriptDecoding"),
  bindEvent: deployedTitle("fraudProofExecutionSourceScriptDecodingBindEvent"),
  terminal: deployedTitle("fraudProofExecutionSourceScriptDecodingTerminal"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through
 * `executionSourceScriptDecodingRoles`, so a manifest-restored chain carries
 * them. */
const EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_MEMBERS = [
  ["step01", "fraudProofExecutionSourceScriptDecoding"],
  ["bindEvent", "fraudProofExecutionSourceScriptDecodingBindEvent"],
  ["terminal", "fraudProofExecutionSourceScriptDecodingTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type ExecutionSourceScriptDecodingRole =
  (typeof EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_MEMBERS)[number][0];

export const EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER: readonly ExecutionSourceScriptDecodingRole[] =
  Object.freeze(
    EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_MEMBERS.map(([role]) => role),
  );

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `executionSourceScriptDecoding` family. */
export const checkExecutionSourceScriptDecodingRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_MEMBERS.length)
    throw new Error(
      `execution-source-script-decoding has ${EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_MEMBERS.forEach(
    ([role, member], index) => {
      if (steps[index] !== member)
        throw new Error(
          `execution-source-script-decoding role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
        );
    },
  );
};

export type ExecutionSourceScriptDecodingChain = FraudProofChain;

export type ExecutionSourceScriptDecodingRoles = Readonly<
  Record<ExecutionSourceScriptDecodingRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const executionSourceScriptDecodingRoles = (
  chain: FraudProofChain,
): ExecutionSourceScriptDecodingRoles => {
  checkExecutionSourceScriptDecodingRoleOrder(
    deploymentFamilyStepNames("executionSourceScriptDecoding"),
  );
  if (chain.steps.length !== EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER.length)
    throw new Error(
      `execution-source-script-decoding chain has ${chain.steps.length} steps; expected ${EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<ExecutionSourceScriptDecodingRole, SpendingValidator>,
  );
};

export type ExecutionSourceScriptDecodingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly executionSourceScriptDecoding: ExecutionSourceScriptDecodingChain;
};

export type BuildExecutionSourceScriptDecodingFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared step 01 and event selection, then the terminal, which
 * decides from the selected event's recorded reason alone. */
export const buildExecutionSourceScriptDecodingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: FaultProofChainInputs): Effect.Effect<
  ExecutionSourceScriptDecodingFaultProofContracts["executionSourceScriptDecoding"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const terminal = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.terminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
      "Failed to build execution-source-script-decoding terminal",
    );
    const bindEvent = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.bindEvent,
      [terminal.spendingScriptHash, computationThreadPolicyId],
      "Failed to build execution-source-script-decoding event selection",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step01,
      [
        bindEvent.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build execution-source-script-decoding step 01",
    );
    const steps: [SpendingValidator, ...SpendingValidator[]] = [
      step01,
      bindEvent,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    executionSourceScriptDecodingRoles(chain);
    return chain;
  });

export const buildExecutionSourceScriptDecodingFaultProofContracts = (
  params: BuildExecutionSourceScriptDecodingFaultProofContractsParams,
): Effect.Effect<ExecutionSourceScriptDecodingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const executionSourceScriptDecoding =
      yield* buildExecutionSourceScriptDecodingChain({ ...params, ...shared });
    return { ...shared, executionSourceScriptDecoding };
  });

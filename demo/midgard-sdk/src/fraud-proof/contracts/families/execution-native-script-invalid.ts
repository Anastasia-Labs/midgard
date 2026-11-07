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
import { buildDirectBindingContracts } from "../direct-binding.js";
import { DIRECT_PURPOSE_TITLES } from "../direct-purpose.js";
import { buildDirectReceiveContracts } from "../direct-receive.js";
import { buildDirectSourceContracts } from "../direct-source.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofExecutionNativeScriptInvalid"),
  familyEntry: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidFamilyEntry",
  ),
  sourceLookup: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidSourceLookup",
  ),
  signers: deployedTitle("fraudProofExecutionNativeScriptInvalidSigners"),
  evaluate: deployedTitle("fraudProofExecutionNativeScriptInvalidEvaluate"),
  spendKey: deployedTitle("fraudProofExecutionNativeScriptInvalidSpendKey"),
  spendRank: deployedTitle("fraudProofExecutionNativeScriptInvalidSpendRank"),
  terminal: deployedTitle("fraudProofExecutionNativeScriptInvalidTerminal"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through
 * `executionNativeScriptInvalidRoles`, so a manifest-restored chain carries
 * them. */
const EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_MEMBERS = [
  ["step01", "fraudProofExecutionNativeScriptInvalid"],
  ["bindEvent", "fraudProofExecutionNativeScriptInvalidBindEvent"],
  ["bindPreState", "fraudProofExecutionNativeScriptInvalidBindPreState"],
  ["sourceStart", "fraudProofExecutionNativeScriptInvalidSourceStart"],
  ["sourceResolved", "fraudProofExecutionNativeScriptInvalidSourceResolved"],
  [
    "sourceAuthenticate",
    "fraudProofExecutionNativeScriptInvalidSourceAuthenticate",
  ],
  ["sourceMap", "fraudProofExecutionNativeScriptInvalidSourceMap"],
  [
    "sourceInputFinish",
    "fraudProofExecutionNativeScriptInvalidSourceInputFinish",
  ],
  ["sourceGrammar", "fraudProofExecutionNativeScriptInvalidSourceGrammar"],
  ["sourceWitnesses", "fraudProofExecutionNativeScriptInvalidSourceWitnesses"],
  ["sourceFinish", "fraudProofExecutionNativeScriptInvalidSourceFinish"],
  ["familyEntry", "fraudProofExecutionNativeScriptInvalidFamilyEntry"],
  ["purposeSelect", "fraudProofExecutionNativeScriptInvalidPurposeSelect"],
  ["spendSelect", "fraudProofExecutionNativeScriptInvalidSpendSelect"],
  ["spendScan", "fraudProofExecutionNativeScriptInvalidSpendScan"],
  ["mintGrammar", "fraudProofExecutionNativeScriptInvalidMintGrammar"],
  ["mintScan", "fraudProofExecutionNativeScriptInvalidMintScan"],
  ["receiveStart", "fraudProofExecutionNativeScriptInvalidReceiveStart"],
  ["receiveGrammar", "fraudProofExecutionNativeScriptInvalidReceiveGrammar"],
  ["receiveOutputs", "fraudProofExecutionNativeScriptInvalidReceiveOutputs"],
  ["receiveFinish", "fraudProofExecutionNativeScriptInvalidReceiveFinish"],
  [
    "receiveDeclaration",
    "fraudProofExecutionNativeScriptInvalidReceiveDeclaration",
  ],
  ["sourceLookup", "fraudProofExecutionNativeScriptInvalidSourceLookup"],
  ["signers", "fraudProofExecutionNativeScriptInvalidSigners"],
  ["evaluate", "fraudProofExecutionNativeScriptInvalidEvaluate"],
  ["spendKey", "fraudProofExecutionNativeScriptInvalidSpendKey"],
  ["spendRank", "fraudProofExecutionNativeScriptInvalidSpendRank"],
  ["terminal", "fraudProofExecutionNativeScriptInvalidTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type ExecutionNativeScriptInvalidRole =
  (typeof EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_MEMBERS)[number][0];

export const EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER: readonly ExecutionNativeScriptInvalidRole[] =
  Object.freeze(
    EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_MEMBERS.map(([role]) => role),
  );

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `executionNativeScriptInvalid` family. */
export const checkExecutionNativeScriptInvalidRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_MEMBERS.length)
    throw new Error(
      `execution-native-script-invalid has ${EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_MEMBERS.forEach(
    ([role, member], index) => {
      if (steps[index] !== member)
        throw new Error(
          `execution-native-script-invalid role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
        );
    },
  );
};

export type ExecutionNativeScriptInvalidChain = FraudProofChain;

export type ExecutionNativeScriptInvalidRoles = Readonly<
  Record<ExecutionNativeScriptInvalidRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const executionNativeScriptInvalidRoles = (
  chain: FraudProofChain,
): ExecutionNativeScriptInvalidRoles => {
  checkExecutionNativeScriptInvalidRoleOrder(
    deploymentFamilyStepNames("executionNativeScriptInvalid"),
  );
  if (chain.steps.length !== EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.length)
    throw new Error(
      `execution-native-script-invalid chain has ${chain.steps.length} steps; expected ${EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<ExecutionNativeScriptInvalidRole, SpendingValidator>,
  );
};

export type ExecutionNativeScriptInvalidFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly executionNativeScriptInvalid: ExecutionNativeScriptInvalidChain;
};

export type BuildExecutionNativeScriptInvalidFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared bind and source graph, the family entry, the shared
 * purpose route ending at the family lookup, the paged signer and native
 * evaluation roles, the key-spend rank and the terminal decision. */
export const buildExecutionNativeScriptInvalidChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  ExecutionNativeScriptInvalidFaultProofContracts["executionNativeScriptInvalid"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ] as const;
    const titles = EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES;
    const step = (
      title: string,
      params: Parameters<typeof buildFaultProofSpendingStep>[2],
      role: string,
    ) =>
      buildFaultProofSpendingStep(
        context,
        title,
        params,
        `Failed to build execution-native-script-invalid ${role}`,
      );
    const terminal = yield* step(
      titles.terminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
      "terminal",
    );
    const evaluate = yield* step(
      titles.evaluate,
      [terminal.spendingScriptHash, computationThreadPolicyId],
      "evaluation",
    );
    const signers = yield* step(
      titles.signers,
      [evaluate.spendingScriptHash, ...certified],
      "signer frontier",
    );
    const sourceLookup = yield* step(
      titles.sourceLookup,
      [
        signers.spendingScriptHash,
        terminal.spendingScriptHash,
        computationThreadPolicyId,
      ],
      "source lookup",
    );
    const spendRank = yield* step(
      titles.spendRank,
      [terminal.spendingScriptHash, ...certified],
      "key-spend rank",
    );
    const spendKey = yield* step(
      titles.spendKey,
      [spendRank.spendingScriptHash, ...certified],
      "key-spend opening",
    );
    const receive = yield* buildDirectReceiveContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: sourceLookup.spendingScriptHash,
    });
    const spendScan = yield* step(
      DIRECT_PURPOSE_TITLES.spendScan,
      [sourceLookup.spendingScriptHash, ...certified],
      "spend pointer rank",
    );
    const spendSelect = yield* step(
      DIRECT_PURPOSE_TITLES.spendSelect,
      [spendScan.spendingScriptHash, ...certified],
      "spend selection",
    );
    const mintScan = yield* step(
      DIRECT_PURPOSE_TITLES.mintScan,
      [sourceLookup.spendingScriptHash, ...certified],
      "mint selection",
    );
    const mintGrammar = yield* step(
      DIRECT_PURPOSE_TITLES.mintGrammar,
      [mintScan.spendingScriptHash, ...certified],
      "mint grammar",
    );
    const purposeSelect = yield* step(
      DIRECT_PURPOSE_TITLES.select,
      [
        sourceLookup.spendingScriptHash,
        spendSelect.spendingScriptHash,
        mintGrammar.spendingScriptHash,
        receive.start.spendingScriptHash,
        ...certified,
      ],
      "purpose selection",
    );
    const familyEntry = yield* step(
      titles.familyEntry,
      [
        purposeSelect.spendingScriptHash,
        spendKey.spendingScriptHash,
        terminal.spendingScriptHash,
        ...certified,
      ],
      "family entry",
    );
    const source = yield* buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: familyEntry.spendingScriptHash,
    });
    const binding = yield* buildDirectBindingContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      nextStepScriptHash: source.start.spendingScriptHash,
    });
    const step01 = yield* step(
      titles.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "step 01",
    );
    const steps: [SpendingValidator, ...SpendingValidator[]] = [
      step01,
      binding.event,
      binding.preState,
      source.start,
      source.resolved,
      source.authenticate,
      source.map,
      source.inputFinish,
      source.grammar,
      source.witnesses,
      source.finish,
      familyEntry,
      purposeSelect,
      spendSelect,
      spendScan,
      mintGrammar,
      mintScan,
      receive.start,
      receive.grammar,
      receive.outputs,
      receive.finish,
      receive.declaration,
      sourceLookup,
      signers,
      evaluate,
      spendKey,
      spendRank,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    executionNativeScriptInvalidRoles(chain);
    return chain;
  });

export const buildExecutionNativeScriptInvalidFaultProofContracts = (
  params: BuildExecutionNativeScriptInvalidFaultProofContractsParams,
): Effect.Effect<ExecutionNativeScriptInvalidFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const executionNativeScriptInvalid =
      yield* buildExecutionNativeScriptInvalidChain({ ...params, ...shared });
    return { ...shared, executionNativeScriptInvalid };
  });

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
import { buildDirectAllPurposesContracts } from "../direct-all-purposes.js";
import { buildDirectBindingContracts } from "../direct-binding.js";
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

export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofScriptIntegrityHashMismatch"),
  familyEntry: deployedTitle(
    "fraudProofScriptIntegrityHashMismatchFamilyEntry",
  ),
  terminal: deployedTitle("fraudProofScriptIntegrityHashMismatchTerminal"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through
 * `scriptIntegrityHashMismatchRoles`, so a manifest-restored chain carries
 * them. */
const SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_MEMBERS = [
  ["step01", "fraudProofScriptIntegrityHashMismatch"],
  ["bindEvent", "fraudProofScriptIntegrityHashMismatchBindEvent"],
  ["bindPreState", "fraudProofScriptIntegrityHashMismatchBindPreState"],
  ["sourceStart", "fraudProofScriptIntegrityHashMismatchSourceStart"],
  ["sourceResolved", "fraudProofScriptIntegrityHashMismatchSourceResolved"],
  [
    "sourceAuthenticate",
    "fraudProofScriptIntegrityHashMismatchSourceAuthenticate",
  ],
  ["sourceMap", "fraudProofScriptIntegrityHashMismatchSourceMap"],
  [
    "sourceInputFinish",
    "fraudProofScriptIntegrityHashMismatchSourceInputFinish",
  ],
  ["sourceGrammar", "fraudProofScriptIntegrityHashMismatchSourceGrammar"],
  ["sourceWitnesses", "fraudProofScriptIntegrityHashMismatchSourceWitnesses"],
  ["sourceFinish", "fraudProofScriptIntegrityHashMismatchSourceFinish"],
  ["familyEntry", "fraudProofScriptIntegrityHashMismatchFamilyEntry"],
  ["allPurposesSpend", "fraudProofScriptIntegrityHashMismatchAllPurposesSpend"],
  [
    "allPurposesSpendResolve",
    "fraudProofScriptIntegrityHashMismatchAllPurposesSpendResolve",
  ],
  [
    "allPurposesMintGrammar",
    "fraudProofScriptIntegrityHashMismatchAllPurposesMintGrammar",
  ],
  ["allPurposesMint", "fraudProofScriptIntegrityHashMismatchAllPurposesMint"],
  [
    "allPurposesObserveOpen",
    "fraudProofScriptIntegrityHashMismatchAllPurposesObserveOpen",
  ],
  [
    "allPurposesObserve",
    "fraudProofScriptIntegrityHashMismatchAllPurposesObserve",
  ],
  [
    "allPurposesReceiveGrammar",
    "fraudProofScriptIntegrityHashMismatchAllPurposesReceiveGrammar",
  ],
  [
    "allPurposesReceive",
    "fraudProofScriptIntegrityHashMismatchAllPurposesReceive",
  ],
  ["terminal", "fraudProofScriptIntegrityHashMismatchTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type ScriptIntegrityHashMismatchRole =
  (typeof SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_MEMBERS)[number][0];

export const SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER: readonly ScriptIntegrityHashMismatchRole[] =
  Object.freeze(
    SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_MEMBERS.map(([role]) => role),
  );

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `scriptIntegrityHashMismatch` family. */
export const checkScriptIntegrityHashMismatchRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_MEMBERS.length)
    throw new Error(
      `script-integrity-hash-mismatch has ${SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_MEMBERS.forEach(
    ([role, member], index) => {
      if (steps[index] !== member)
        throw new Error(
          `script-integrity-hash-mismatch role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
        );
    },
  );
};

export type ScriptIntegrityHashMismatchChain = FraudProofChain;

export type ScriptIntegrityHashMismatchRoles = Readonly<
  Record<ScriptIntegrityHashMismatchRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const scriptIntegrityHashMismatchRoles = (
  chain: FraudProofChain,
): ScriptIntegrityHashMismatchRoles => {
  checkScriptIntegrityHashMismatchRoleOrder(
    deploymentFamilyStepNames("scriptIntegrityHashMismatch"),
  );
  if (chain.steps.length !== SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.length)
    throw new Error(
      `script-integrity-hash-mismatch chain has ${chain.steps.length} steps; expected ${SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<ScriptIntegrityHashMismatchRole, SpendingValidator>,
  );
};

export type ScriptIntegrityHashMismatchFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly scriptIntegrityHashMismatch: ScriptIntegrityHashMismatchChain;
};

export type BuildScriptIntegrityHashMismatchFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared bind and source graph, the family entry, the shared
 * all-purposes walk and the terminal decision. */
export const buildScriptIntegrityHashMismatchChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  ScriptIntegrityHashMismatchFaultProofContracts["scriptIntegrityHashMismatch"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const terminal = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.terminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
      "Failed to build script-integrity-hash-mismatch terminal",
    );
    const walk = yield* buildDirectAllPurposesContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: terminal.spendingScriptHash,
    });
    const familyEntry = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.familyEntry,
      [
        walk.spend.spendingScriptHash,
        walk.mintGrammar.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build script-integrity-hash-mismatch family entry",
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
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build script-integrity-hash-mismatch step 01",
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
      walk.spend,
      walk.spendResolve,
      walk.mintGrammar,
      walk.mint,
      walk.observeOpen,
      walk.observe,
      walk.receiveGrammar,
      walk.receive,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    scriptIntegrityHashMismatchRoles(chain);
    return chain;
  });

export const buildScriptIntegrityHashMismatchFaultProofContracts = (
  params: BuildScriptIntegrityHashMismatchFaultProofContractsParams,
): Effect.Effect<ScriptIntegrityHashMismatchFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const scriptIntegrityHashMismatch =
      yield* buildScriptIntegrityHashMismatchChain({ ...params, ...shared });
    return { ...shared, scriptIntegrityHashMismatch };
  });

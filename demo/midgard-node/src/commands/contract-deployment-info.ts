/**
 * Builds and writes a deployment manifest for the currently configured Midgard
 * validator bundle.
 *
 * The manifest is keyed by explicit script names such as `depositMint` and
 * `depositSpend`, because many logical contracts compile to distinct scripts for
 * different purposes. Each entry records the compiled script bytes, its
 * corresponding script hash/policy id, and any matching reference-script UTxO
 * currently published in the dedicated reference-script wallet.
 */
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_RELEASE_EVIDENCE_DIGEST,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestCanonicalRational,
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentManifestEconomics,
  type DeploymentManifestEconomicsProfile,
  deriveDeploymentManifestCardanoProtocolParametersFromOgmios,
  makeDeploymentMarker,
  parseDeploymentManifestAvailabilityChallenge,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  GENESIS_HEADER_HASH,
  type ReferenceScriptAuthPolicyDeploymentInfo,
  type ReferenceScriptAuthPolicyRef,
  type ReferenceScriptAuthTokenTarget,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  type DeploymentManifestValue,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestValue,
} from "../deployment-manifest.js";
import {
  bindDeploymentRunStateToMarker,
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  sha256File,
} from "../e2e/run-state.js";
import {
  contractDeploymentInfoPathOverride,
  daAvailabilityChallengeEnvironmentInput,
  deploymentEconomicsProfileFromEnvironment,
} from "../environment.js";
import { writeJsonFileAtomic } from "../files/atomic-write.js";
import { normalizeOgmiosHttpUrl } from "../local-ledger-slot.js";
import { loadPhasMembershipWithdrawalScript } from "../phas-membership.js";
import {
  loadRealBlueprintSha256,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  deriveOperatorDaParams,
  fetchProtocolDeploymentStatus,
  fraudProofsToIndexedValidators,
} from "../transactions/initialization.js";
import { fetchReferenceScriptUtxosAt } from "../transactions/reference-scripts.js";
import { compareOutRefs } from "../tx-context.js";

export type ContractDeploymentInfoRefScriptUTxO = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type ContractDeploymentInfoEntry = {
  readonly refScriptUTxO: ContractDeploymentInfoRefScriptUTxO | null;
  readonly contract: {
    readonly type: Script["type"];
    readonly cborHex: string;
  };
  readonly scriptHash: string;
  readonly fraudProofCatalogue?: SDK.FraudProofCatalogueDeploymentInfo;
};

export type ContractDeploymentInfo = {
  readonly referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo;
  readonly contracts: Readonly<Record<string, ContractDeploymentInfoEntry>>;
};

export { computeDeploymentManifestId, DEPLOYMENT_MANIFEST_SCHEMA_VERSION };

export type DeploymentManifestStepStatus =
  | "pending"
  | "in_progress"
  | "submitted"
  | "complete"
  | "attached"
  | "failed"
  | "blocked_requires_fresh_redeploy";

export type DeploymentManifest = ContractDeploymentInfo & {
  readonly schemaVersion: typeof DEPLOYMENT_MANIFEST_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly consensusProfileDigest: string;
  readonly network: string;
  readonly cardanoProtocolParameters: DeploymentManifestValue["cardanoProtocolParameters"];
  readonly genesis: DeploymentManifestValue["genesis"];
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShot: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly outRef: string;
    readonly status: "consumed_by_init";
  };
  readonly referenceScripts: Readonly<
    Record<
      string,
      {
        readonly status: "confirmed";
        readonly roleUnit: string;
        readonly scriptHash: string;
        readonly outRef: string;
      }
    >
  >;
  readonly da: DeploymentManifestValue["da"];
  readonly proofEvidence: DeploymentManifestValue["proofEvidence"];
  readonly steps: Readonly<
    Record<
      | "prepareHubOracleNonce"
      | "deployNodeRuntimeReferenceScripts"
      | "initProtocol"
      | "phasRegistration"
      | "operatorRegistration"
      | "operatorActivation",
      {
        readonly status: DeploymentManifestStepStatus;
        readonly txHash?: string;
      }
    >
  >;
  readonly validationDispute: {
    readonly version: number;
    readonly responseWindowMs: number;
    readonly maxBisectionRounds: number;
    readonly maturityMs: number;
  };
  readonly l1Finality: DeploymentManifestValue["l1Finality"];
  readonly economics: DeploymentManifestValue["economics"];
  readonly availabilityChallenge: DeploymentManifestValue["availabilityChallenge"];
};

export type DeploymentManifestVerificationReport = {
  readonly ok: boolean;
  readonly manifestId?: string;
  readonly path?: string;
  readonly mismatches: readonly string[];
  readonly recommendation:
    | "attach"
    | "correct_attach_config"
    | "fresh_redeploy_required";
};

export type FinalizedDeploymentIdentity = {
  readonly path: string;
  readonly manifestId: string;
  readonly contractDeploymentInfoSha256: string;
  readonly manifest: DeploymentManifest;
};

const DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME =
  "contract-deployment-info.json";
const DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME = "deploymentInfo";

const resolvePackageRootFromModuleUrl = (moduleUrl: string): string => {
  let currentDir = dirname(fileURLToPath(moduleUrl));
  while (true) {
    if (existsSync(resolvePath(currentDir, "package.json"))) {
      return currentDir;
    }
    const parentDir = resolvePath(currentDir, "..");
    if (parentDir === currentDir) {
      return resolvePath(process.cwd());
    }
    currentDir = parentDir;
  }
};

type ScriptDescriptor = {
  readonly name: string;
  readonly script: Script;
  readonly scriptHash: string;
  readonly contract: ContractDeploymentInfoEntry["contract"];
  readonly referenceScriptTargetName?: ReferenceScriptAuthTokenTarget;
};

const REFERENCE_SCRIPT_TARGET_BY_CONTRACT_NAME = Object.freeze(
  Object.fromEntries(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([targetName, contractName]) => {
        if (!(targetName in SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)) {
          throw new Error(
            `Deployment manifest reference-script role is not registered by the SDK: ${targetName}`,
          );
        }
        return [contractName, targetName as ReferenceScriptAuthTokenTarget];
      },
    ),
  ) as Readonly<Record<string, ReferenceScriptAuthTokenTarget>>,
);

const REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES = [
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "valueNotPreserved",
  "inputSetUniqueness",
  "mintAuthorization",
  "networkId",
  "missingNativeScriptUtxo",
  "nativeScriptInvalid",
  "minAda",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "witnessScriptDecoding",
  "scriptIntegrityHashMissing",
  "transactionOutputNonCanonical",
  "resolvedOutputNonCanonical",
  "mintDeclaredAssetLimit",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "outputReferenceScriptDecoding",
  "executionSourceScriptDecoding",
  "receivePurposeLanguage",
  "unusedScriptWitness",
  "unusedRedeemer",
  "executionNativeScriptInvalid",
  "scriptIntegrityHashMismatch",
  "distinctAssetAccumulationLimit",
  "missingScriptSource",
  "missingRedeemer",
] as const satisfies readonly (keyof SDK.FaultProofContracts)[];

const upperFirst = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`;

const faultProofStepContractName = (
  category: (typeof REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES)[number],
  stepIndex: number,
): string => {
  if (category === "nativeScriptDecoding") {
    const names = [
      "fraudProofNativeScriptDecoding",
      "fraudProofNativeScriptDecodingStep02",
      "fraudProofNativeScriptDecodingStep03OpenSubject",
      "fraudProofNativeScriptDecodingStep03BindDescriptor",
      "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
      "fraudProofNativeScriptDecodingStep04",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined) {
      throw new Error(
        `native-script-decoding exposes an unexpected step index ${stepIndex.toString()}`,
      );
    }
    return name;
  }
  if (category === "fieldPreimageLengthMismatch") {
    const names = [
      "fraudProofFieldPreimageLengthMismatch",
      "fraudProofFieldPreimageLengthMismatchStep02Accepted",
      "fraudProofFieldPreimageLengthMismatchStep02Forced",
      "fraudProofFieldPreimageLengthMismatchStep03",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `field-preimage-length-mismatch exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "scriptIntegrityHashMissing") {
    const names = [
      "fraudProofScriptIntegrityHashMissing",
      "fraudProofScriptIntegrityHashMissingStep02",
      "fraudProofScriptIntegrityHashMissingStep03",
      "fraudProofScriptIntegrityHashMissingScriptGrammar",
      "fraudProofScriptIntegrityHashMissingScriptScan",
      "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
      "fraudProofScriptIntegrityHashMissingStep04",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `script-integrity-hash-missing exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "missingRedeemer") {
    const names = [
      "fraudProofMissingRedeemer",
      "fraudProofMissingRedeemerStep02",
      "fraudProofMissingRedeemerStep02a",
      "fraudProofMissingRedeemerStep02b",
      "fraudProofMissingRedeemerStep03",
      "fraudProofMissingRedeemerStep04",
      "fraudProofMissingRedeemerStep05",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `missing-redeemer exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "unusedRedeemer") {
    const names = [
      "fraudProofUnusedRedeemer",
      "fraudProofUnusedRedeemerStep02",
      "fraudProofUnusedRedeemerStep02a",
      "fraudProofUnusedRedeemerStep02b",
      "fraudProofUnusedRedeemerStep02c",
      "fraudProofUnusedRedeemerStep03",
      "fraudProofUnusedRedeemerStep04",
      "fraudProofUnusedRedeemerStep05",
      "fraudProofUnusedRedeemerStep06",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `unused-redeemer exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "executionNativeScriptInvalid") {
    const names = [
      "fraudProofExecutionNativeScriptInvalid",
      "fraudProofExecutionNativeScriptInvalidStep02",
      "fraudProofExecutionNativeScriptInvalidStep03",
      "fraudProofExecutionNativeScriptInvalidStep04",
      "fraudProofExecutionNativeScriptInvalidStep05",
      "fraudProofExecutionNativeScriptInvalidStep06",
      "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
      "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
      "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `execution-native-script-invalid exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  return `fraudProof${upperFirst(category)}${
    stepIndex === 0 ? "" : `Step${(stepIndex + 1).toString().padStart(2, "0")}`
  }`;
};

const referenceScriptTargetForContract = (
  contractName: string,
): ReferenceScriptAuthTokenTarget => {
  const targetName = REFERENCE_SCRIPT_TARGET_BY_CONTRACT_NAME[contractName];
  if (targetName === undefined) {
    throw new Error(
      `Contract is missing a canonical reference-script role: ${contractName}`,
    );
  }
  return targetName;
};

const mintDescriptor = (
  name: string,
  validator: SDK.MintingValidator,
  referenceScriptTargetName?: ReferenceScriptAuthTokenTarget,
): ScriptDescriptor => ({
  name,
  script: validator.mintingScript,
  scriptHash: validator.policyId,
  contract: {
    type: validator.mintingScript.type,
    cborHex: validator.mintingScript.script,
  },
  ...(referenceScriptTargetName === undefined
    ? {}
    : { referenceScriptTargetName }),
});

const spendDescriptor = (
  name: string,
  validator: SDK.SpendingValidator,
  referenceScriptTargetName?: ReferenceScriptAuthTokenTarget,
): ScriptDescriptor => ({
  name,
  script: validator.spendingScript,
  scriptHash: validator.spendingScriptHash,
  contract: {
    type: validator.spendingScript.type,
    cborHex: validator.spendingScript.script,
  },
  ...(referenceScriptTargetName === undefined
    ? {}
    : { referenceScriptTargetName }),
});

const withdrawalDescriptor = (
  name: string,
  validator: SDK.WithdrawalValidator,
  referenceScriptTargetName?: ReferenceScriptAuthTokenTarget,
): ScriptDescriptor => ({
  name,
  script: validator.withdrawalScript,
  scriptHash: validator.withdrawalScriptHash,
  contract: {
    type: validator.withdrawalScript.type,
    cborHex: validator.withdrawalScript.script,
  },
  ...(referenceScriptTargetName === undefined
    ? {}
    : { referenceScriptTargetName }),
});

const phasMembershipDescriptor = (
  referenceScriptTargetName?: ReferenceScriptAuthTokenTarget,
): ScriptDescriptor => {
  const script = loadPhasMembershipWithdrawalScript();
  return {
    name: "phasMembershipWithdraw",
    script,
    scriptHash: validatorToScriptHash(script),
    contract: {
      type: script.type,
      cborHex: script.script,
    },
    ...(referenceScriptTargetName === undefined
      ? {}
      : { referenceScriptTargetName }),
  };
};

const TRANSITION_TRACE_FINAL_CONTRACT_NAMES = [
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
] as const;

/**
 * The prefix of a compiled fault-proof chain that the canonical deployment ABI
 * actually registers.
 *
 * The canonical ABI registers all five `nativeScriptInvalid` steps. The
 * separate `missingNativeScriptUtxo` chain still compiles seven steps while
 * its manifest names five. The exact manifest key set therefore bounds that
 * chain until its own ABI extension is implemented across consumers.
 *
 * This is a BOUND, not a filter. It only ever drops a TAIL of unregistered
 * steps: an unregistered step with a registered step after it, or a chain whose
 * very first step is unregistered, still fails closed here.
 */
const abiRegisteredChainSteps = <T>(
  category: (typeof REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES)[number],
  steps: readonly T[],
): readonly T[] => {
  const registered = steps.map(
    (_, stepIndex) =>
      REFERENCE_SCRIPT_TARGET_BY_CONTRACT_NAME[
        faultProofStepContractName(category, stepIndex)
      ] !== undefined,
  );
  const firstUnregistered = registered.indexOf(false);
  if (firstUnregistered === -1) {
    return steps;
  }
  if (firstUnregistered === 0) {
    throw new Error(
      `Fault-proof category ${category} has no canonical reference-script role for its first step`,
    );
  }
  if (registered.lastIndexOf(true) > firstUnregistered) {
    throw new Error(
      `Fault-proof category ${category} registers a step after unregistered step ${(firstUnregistered + 1).toString()}`,
    );
  }
  return steps.slice(0, firstUnregistered);
};

const registeredFaultProofScriptDescriptors = (
  contracts: SDK.MidgardValidators,
): readonly ScriptDescriptor[] => [
  ...REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.flatMap((category) =>
    abiRegisteredChainSteps(
      category,
      contracts.fraudProofContracts[category].steps,
    ).map((validator, stepIndex) => {
      const contractName = faultProofStepContractName(category, stepIndex);
      return spendDescriptor(
        contractName,
        validator,
        referenceScriptTargetForContract(contractName),
      );
    }),
  ),
  spendDescriptor(
    "fraudProofTransitionTrace",
    contracts.fraudProofContracts.transitionTrace.route,
    referenceScriptTargetForContract("fraudProofTransitionTrace"),
  ),
  ...contracts.fraudProofContracts.transitionTrace.finals.map(
    (validator, index) => {
      const contractName = TRANSITION_TRACE_FINAL_CONTRACT_NAMES[index];
      return spendDescriptor(
        contractName,
        validator,
        referenceScriptTargetForContract(contractName),
      );
    },
  ),
  // `buildNetworkIdChain` keeps the forced (wrongful-rejection) door and the
  // resumable output scan it hands off to out of `steps`, so the chain walk
  // above never emits them. Each carries its own manifest name and
  // reference-script role and is emitted here by name.
  spendDescriptor(
    "fraudProofValueNotPreservedUnionAcceptedSource",
    contracts.fraudProofContracts.valueNotPreserved.unionAcceptedSource,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionAcceptedSource",
    ),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionForcedSource",
    contracts.fraudProofContracts.valueNotPreserved.unionForcedSource,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionForcedSource",
    ),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionEvent",
    contracts.fraudProofContracts.valueNotPreserved.unionEvent,
    referenceScriptTargetForContract("fraudProofValueNotPreservedUnionEvent"),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionPreState",
    contracts.fraudProofContracts.valueNotPreserved.unionPreState,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionPreState",
    ),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionInputs",
    contracts.fraudProofContracts.valueNotPreserved.unionInputs,
    referenceScriptTargetForContract("fraudProofValueNotPreservedUnionInputs"),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionInputValue",
    contracts.fraudProofContracts.valueNotPreserved.unionInputValue,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionInputValue",
    ),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionAssets",
    contracts.fraudProofContracts.valueNotPreserved.unionAssets,
    referenceScriptTargetForContract("fraudProofValueNotPreservedUnionAssets"),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionFieldGrammar",
    contracts.fraudProofContracts.valueNotPreserved.unionFieldGrammar,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionFieldGrammar",
    ),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionOutputs",
    contracts.fraudProofContracts.valueNotPreserved.unionOutputs,
    referenceScriptTargetForContract("fraudProofValueNotPreservedUnionOutputs"),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionOutputScan",
    contracts.fraudProofContracts.valueNotPreserved.unionOutputScan,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionOutputScan",
    ),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionMint",
    contracts.fraudProofContracts.valueNotPreserved.unionMint,
    referenceScriptTargetForContract("fraudProofValueNotPreservedUnionMint"),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionUpdate",
    contracts.fraudProofContracts.valueNotPreserved.unionUpdate,
    referenceScriptTargetForContract("fraudProofValueNotPreservedUnionUpdate"),
  ),
  spendDescriptor(
    "fraudProofValueNotPreservedUnionTerminal",
    contracts.fraudProofContracts.valueNotPreserved.unionTerminal,
    referenceScriptTargetForContract(
      "fraudProofValueNotPreservedUnionTerminal",
    ),
  ),
  spendDescriptor(
    "fraudProofMissingSignatureForcedStep",
    contracts.fraudProofContracts.missingSignature.forcedStep,
    referenceScriptTargetForContract("fraudProofMissingSignatureForcedStep"),
  ),
  spendDescriptor(
    "fraudProofMissingSignatureForcedSigner",
    contracts.fraudProofContracts.missingSignature.forcedSigner,
    referenceScriptTargetForContract("fraudProofMissingSignatureForcedSigner"),
  ),
  spendDescriptor(
    "fraudProofMissingSignatureForcedWitness",
    contracts.fraudProofContracts.missingSignature.forcedWitness,
    referenceScriptTargetForContract("fraudProofMissingSignatureForcedWitness"),
  ),
  spendDescriptor(
    "fraudProofNetworkIdForcedStep",
    contracts.fraudProofContracts.networkId.forcedStep,
    referenceScriptTargetForContract("fraudProofNetworkIdForcedStep"),
  ),
  spendDescriptor(
    "fraudProofNetworkIdForcedScan",
    contracts.fraudProofContracts.networkId.forcedScan,
    referenceScriptTargetForContract("fraudProofNetworkIdForcedScan"),
  ),
];

const legacyFaultProofMissingStepDescriptors = (
  contracts: SDK.MidgardValidators,
): readonly ScriptDescriptor[] => {
  const families = [
    ["fraudProofDoubleSpend", contracts.fraudProofContracts.doubleSpend],
    [
      "fraudProofNonExistentInput",
      contracts.fraudProofContracts.nonExistentInput,
    ],
    [
      "fraudProofNonExistentInputNoIndex",
      contracts.fraudProofContracts.nonExistentInputNoIndex,
    ],
    ["fraudProofInvalidRange", contracts.fraudProofContracts.invalidRange],
    ["fraudProofZeroInput", contracts.fraudProofContracts.zeroInput],
    ["fraudProofDaHashPreimage", contracts.fraudProofContracts.daHashPreimage],
    [
      "fraudProofNoReferenceInput",
      contracts.fraudProofContracts.noReferenceInput,
    ],
    [
      "fraudProofReferenceInputNoIdx",
      contracts.fraudProofContracts.referenceInputNoIdx,
    ],
    [
      "fraudProofInvalidSignature",
      contracts.fraudProofContracts.invalidSignature,
    ],
  ] as const;
  return families.flatMap(([firstStepContractName, chain]) =>
    chain.steps.slice(1).map((validator, index) => {
      const contractName = `${firstStepContractName}Step${(index + 2)
        .toString()
        .padStart(2, "0")}`;
      return spendDescriptor(
        contractName,
        validator,
        referenceScriptTargetForContract(contractName),
      );
    }),
  );
};

const fetchLiveReferenceScriptUtxos = (): Effect.Effect<
  readonly UTxO[],
  Error,
  Lucid
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const referenceScriptsLucid = lucidService.referenceScriptsApi;
    const referenceScriptsAddress = lucidService.referenceScriptsAddress;
    return yield* fetchReferenceScriptUtxosAt(
      referenceScriptsLucid,
      referenceScriptsAddress,
      "contract deployment info reference-script UTxO fetch",
      `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}`,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error("Failed to resolve contract deployment reference scripts", {
            cause,
          }),
      ),
    );
  });

export const buildReferenceScriptOutRefMap = (
  utxos: readonly UTxO[],
  descriptors: readonly ScriptDescriptor[],
  authPolicy: ReferenceScriptAuthPolicyRef,
): ReadonlyMap<string, ContractDeploymentInfoRefScriptUTxO> => {
  const byDescriptorName = new Map<
    string,
    ContractDeploymentInfoRefScriptUTxO
  >();
  for (const descriptor of descriptors) {
    if (descriptor.referenceScriptTargetName === undefined) {
      continue;
    }
    const roleUnit = referenceScriptAuthUnit(
      authPolicy.policyId,
      descriptor.referenceScriptTargetName,
    );
    const candidates = utxos.filter(
      (utxo) => (utxo.assets[roleUnit] ?? 0n) !== 0n,
    );
    if (candidates.length === 0) {
      continue;
    }
    if (candidates.length !== 1) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} is ambiguous: expected exactly one live ${roleUnit} UTxO, found ${candidates.length.toString()}`,
      );
    }
    const candidate = candidates[0]!;
    if (candidate.assets[roleUnit] !== 1n) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} must carry exactly one ${roleUnit} token`,
      );
    }
    const authPolicyUnits = Object.entries(candidate.assets).filter(
      ([unit, quantity]) =>
        unit.startsWith(authPolicy.policyId) && quantity !== 0n,
    );
    if (authPolicyUnits.length !== 1 || authPolicyUnits[0]?.[0] !== roleUnit) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} UTxO must carry no other ${authPolicy.policyId} role token`,
      );
    }
    if (candidate.scriptRef == null) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} token is not attached to a reference script`,
      );
    }
    const observedScriptHash = validatorToScriptHash(candidate.scriptRef);
    if (observedScriptHash !== descriptor.scriptHash) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} script hash mismatch: expected ${descriptor.scriptHash}, found ${observedScriptHash}`,
      );
    }
    byDescriptorName.set(descriptor.name, {
      txHash: candidate.txHash,
      outputIndex: candidate.outputIndex,
    });
  }
  return byDescriptorName;
};

const collectScriptDescriptors = (
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicy?: ReferenceScriptAuthPolicyDeploymentInfo,
): readonly ScriptDescriptor[] => [
  mintDescriptor(
    "referenceScriptAuthMint",
    referenceScriptAuthPolicy === undefined
      ? contracts.referenceScriptAuth
      : {
          mintingScriptCBOR: referenceScriptAuthPolicy.nativeScript.cborHex,
          mintingScript: {
            type: "Native",
            script: referenceScriptAuthPolicy.nativeScript.cborHex,
          },
          policyId: referenceScriptAuthPolicy.policyId,
        },
    "reference-script-auth minting",
  ),
  mintDescriptor("hubOracleMint", contracts.hubOracle, "hub-oracle minting"),
  spendDescriptor(
    "daParamsGovernorSpend",
    contracts.daParamsGovernor,
    "da-params-governor spending",
  ),
  mintDescriptor(
    "daParamsGovernorMint",
    contracts.daParamsGovernor,
    "da-params-governor minting",
  ),
  spendDescriptor(
    "daAttestationSpend",
    contracts.daAttestation,
    "da-attestation spending",
  ),
  mintDescriptor(
    "daAttestationMint",
    contracts.daAttestation,
    "da-attestation minting",
  ),
  spendDescriptor(
    "stateQueueSpend",
    contracts.stateQueue,
    "state-queue spending",
  ),
  mintDescriptor("stateQueueMint", contracts.stateQueue, "state-queue minting"),
  withdrawalDescriptor(
    "stateQueueCommitWithdraw",
    contracts.stateQueue.yields.commit,
    "state-queue commit withdrawal",
  ),
  withdrawalDescriptor(
    "stateQueueUnattestedTimeoutWithdraw",
    contracts.stateQueue.yields.unattestedTimeout,
    "state-queue unattested-timeout withdrawal",
  ),
  withdrawalDescriptor(
    "stateQueueUnavailableTimeoutWithdraw",
    contracts.stateQueue.yields.unavailableTimeout,
    "state-queue unavailable-timeout withdrawal",
  ),
  withdrawalDescriptor(
    "stateQueueFraudRemovalWithdraw",
    contracts.stateQueue.yields.fraudRemoval,
    "state-queue fraud-removal withdrawal",
  ),
  withdrawalDescriptor(
    "stateQueueMergeWithdraw",
    contracts.stateQueue.yields.merge,
    "state-queue merge withdrawal",
  ),
  spendDescriptor("schedulerSpend", contracts.scheduler, "scheduler spending"),
  mintDescriptor("schedulerMint", contracts.scheduler, "scheduler minting"),
  spendDescriptor(
    "registeredOperatorsSpend",
    contracts.registeredOperators,
    "registered-operators spending",
  ),
  mintDescriptor(
    "registeredOperatorsMint",
    contracts.registeredOperators,
    "registered-operators minting",
  ),
  spendDescriptor(
    "activeOperatorsSpend",
    contracts.activeOperators,
    "active-operators spending",
  ),
  mintDescriptor(
    "activeOperatorsMint",
    contracts.activeOperators,
    "active-operators minting",
  ),
  spendDescriptor(
    "retiredOperatorsSpend",
    contracts.retiredOperators,
    "retired-operators spending",
  ),
  mintDescriptor(
    "retiredOperatorsMint",
    contracts.retiredOperators,
    "retired-operators minting",
  ),
  spendDescriptor("escapeHatchSpend", contracts.escapeHatch),
  mintDescriptor("escapeHatchMint", contracts.escapeHatch),
  spendDescriptor("fraudProofCatalogueSpend", contracts.fraudProofCatalogue),
  mintDescriptor(
    "fraudProofCatalogueMint",
    contracts.fraudProofCatalogue,
    "fraud-proof-catalogue minting",
  ),
  spendDescriptor("fraudProofSpend", contracts.fraudProof),
  mintDescriptor(
    "fraudProofMint",
    contracts.fraudProof,
    "V1 fraud-proof token minting",
  ),
  spendDescriptor("depositSpend", contracts.deposit, "deposit spending"),
  mintDescriptor("depositMint", contracts.deposit, "deposit minting"),
  spendDescriptor(
    "withdrawalSpend",
    contracts.withdrawal,
    "withdrawal spending",
  ),
  mintDescriptor("withdrawalMint", contracts.withdrawal, "withdrawal minting"),
  spendDescriptor("txOrderSpend", contracts.txOrder),
  mintDescriptor("txOrderMint", contracts.txOrder),
  spendDescriptor(
    "fieldPreimageCertificateSpend",
    contracts.fieldPreimageCertificate,
    "V1 field-preimage certificate",
  ),
  mintDescriptor(
    "fieldPreimageCertificateMint",
    contracts.fieldPreimageCertificate,
    "V1 field-preimage certificate minting",
  ),
  spendDescriptor(
    "cekProgramMaterialSpend",
    contracts.cekProgramMaterial,
    "V1 immutable CEK program-material publication",
  ),
  spendDescriptor("settlementSpend", contracts.settlement),
  mintDescriptor("settlementMint", contracts.settlement, "settlement minting"),
  spendDescriptor("payoutSpend", contracts.payout, "payout spending"),
  mintDescriptor("payoutMint", contracts.payout, "payout minting"),
  spendDescriptor("reserveSpend", contracts.reserve, "reserve spending"),
  withdrawalDescriptor(
    "reserveWithdraw",
    contracts.reserve,
    "reserve observer",
  ),
  phasMembershipDescriptor("membership proof withdrawal"),
  spendDescriptor(
    "fraudProofDoubleSpend",
    contracts.fraudProofs.doubleSpend,
    "V1 fraud-proof double-spend step-01",
  ),
  spendDescriptor(
    "fraudProofNonExistentInput",
    contracts.fraudProofs.nonExistentInput,
    "V1 fraud-proof non-existent-input step-01",
  ),
  spendDescriptor(
    "fraudProofNonExistentInputNoIndex",
    contracts.fraudProofs.nonExistentInputNoIndex,
    "V1 fraud-proof non-existent-input-no-index step-01",
  ),
  spendDescriptor(
    "fraudProofInvalidRange",
    contracts.fraudProofs.invalidRange,
    "V1 fraud-proof invalid-range step-01",
  ),
  spendDescriptor(
    "fraudProofZeroInput",
    contracts.fraudProofs.zeroInput,
    "V1 fraud-proof zero-input step-01",
  ),
  spendDescriptor(
    "fraudProofDaHashPreimage",
    contracts.fraudProofs.daHashPreimage,
    "V1 fraud-proof da-hash-preimage step-01",
  ),
  spendDescriptor(
    "fraudProofNoReferenceInput",
    contracts.fraudProofs.noReferenceInput,
    "V1 fraud-proof no-reference-input step-01",
  ),
  spendDescriptor(
    "fraudProofReferenceInputNoIdx",
    contracts.fraudProofs.referenceInputNoIdx,
    "V1 fraud-proof reference-input-no-idx step-01",
  ),
  spendDescriptor(
    "fraudProofInvalidSignature",
    contracts.fraudProofs.invalidSignature,
    "V1 fraud-proof invalid-signature step-01",
  ),
  spendDescriptor(
    "validationTraceDispute",
    contracts.fraudProofs.validationTraceDispute,
    "V1 validation-trace dispute",
  ),
  spendDescriptor(
    "validationTraceDisputeSource",
    contracts.fraudProofs.validationTraceDispute.source,
    "V1 validation-trace source",
  ),
  spendDescriptor(
    "validationTraceDisputeGame",
    contracts.fraudProofs.validationTraceDispute.game,
    "V1 validation-trace game",
  ),
  spendDescriptor(
    "validationTraceDisputeBoundary",
    contracts.fraudProofs.validationTraceDispute.boundary,
    "V1 validation-trace boundary",
  ),
  spendDescriptor(
    "validationTraceDisputeTimeout",
    contracts.fraudProofs.validationTraceDispute.timeout,
    "V1 validation-trace timeout",
  ),
  spendDescriptor(
    "validationTraceDisputeAward",
    contracts.fraudProofs.validationTraceDispute.award,
    "V1 validation-trace award",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesNonOutputSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[32],
    "V1 validation-trace script-sources NonOutput semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[33],
    "V1 validation-trace script-sources OutputProofBegin semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[34],
    "V1 validation-trace script-sources OutputProofStep semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[35],
    "V1 validation-trace script-sources OutputProofFinalize semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[36],
    "V1 validation-trace script-sources OutputProofFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[37],
    "V1 validation-trace script-sources StageZeroBegin semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[38],
    "V1 validation-trace script-sources StageZeroFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[39],
    "V1 validation-trace script-sources StageZeroHashBlock semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[40],
    "V1 validation-trace script-sources StageZeroHashAdvance semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[41],
    "V1 validation-trace script-sources StageZeroHashTerminal semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageNineMismatchSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[42],
    "V1 validation-trace script-sources StageNineMismatch semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[43],
    "V1 validation-trace script-sources StageNineNativeMatch semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[44],
    "V1 validation-trace script-sources StageNineEffectfulMatch semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageNineMissingSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[45],
    "V1 validation-trace script-sources StageNineMissing semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[46],
    "V1 validation-trace script-sources StageOneFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[47],
    "V1 validation-trace script-sources StageOneRedeemer semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[48],
    "V1 validation-trace script-sources StageElevenFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[49],
    "V1 validation-trace script-sources StageElevenSource semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[50],
    "V1 validation-trace script-sources StageTwelveFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[51],
    "V1 validation-trace script-sources StageTwelveRedeemer semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageTenMissingSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[52],
    "V1 validation-trace script-sources StageTenMissing semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageTenMismatchSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[53],
    "V1 validation-trace script-sources StageTenMismatch semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageTenMatchSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[54],
    "V1 validation-trace script-sources StageTenMatch semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[55],
    "V1 validation-trace script-sources StageEightFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[56],
    "V1 validation-trace script-sources StageEightPurpose semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageSevenObserverSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[57],
    "V1 validation-trace script-sources StageSevenObserver semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageSevenReceiveSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[58],
    "V1 validation-trace script-sources StageSevenReceive semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[59],
    "V1 validation-trace script-sources StageSevenFinish semantic",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemTraversalNormalizer",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.traversalNormalizer,
    "V1 validation-trace redeemer item traversal normalizer",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemOuterNormalizer",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.outerNormalizer,
    "V1 validation-trace redeemer item outer normalizer",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemSourceAuthenticator",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.sourceAuthenticator,
    "V1 validation-trace redeemer item source authenticator",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemFoldMapExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[0],
    "V1 validation-trace redeemer item fold map executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemFinalizeFrameExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[1],
    "V1 validation-trace redeemer item finalize frame executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemOpenHeaderExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[2],
    "V1 validation-trace redeemer item open header executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemOpenTailExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[3],
    "V1 validation-trace redeemer item open tail executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemHeadScalarExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[4],
    "V1 validation-trace redeemer item head scalar executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemHeadSequenceExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[5],
    "V1 validation-trace redeemer item head sequence executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemHeadMapExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[6],
    "V1 validation-trace redeemer item head map executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[7],
    "V1 validation-trace redeemer item head large constructor executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemAttachIntegerExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[8],
    "V1 validation-trace redeemer item attach integer executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemAttachBytesExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[9],
    "V1 validation-trace redeemer item attach bytes executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemFoldListExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[10],
    "V1 validation-trace redeemer item fold list executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemAdvanceIntegerExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[11],
    "V1 validation-trace redeemer item advance integer executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemAdvanceBytesExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[12],
    "V1 validation-trace redeemer item advance bytes executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[13],
    "V1 validation-trace redeemer item advance large constructor executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[14],
    "V1 validation-trace redeemer item advance large fields executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemCloseExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[15],
    "V1 validation-trace redeemer item close executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemFinishDataExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[16],
    "V1 validation-trace redeemer item finish data executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemInvalidHeaderExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[17],
    "V1 validation-trace redeemer item invalid header executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemInvalidTailExecutor",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.executors[18],
    "V1 validation-trace redeemer item invalid tail executor",
  ),
  spendDescriptor(
    "validationTraceDisputeRedeemerItemSettlement",
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages.settlement,
    "V1 validation-trace redeemer item settlement",
  ),
  spendDescriptor(
    "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[90],
    "V1 validation-trace script-sources RedeemerNormalization semantic",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageTwoAdvanceWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageTwoAdvance,
    "V1 validation-trace script-sources StageTwoAdvance yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageThreeReplayWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageThreeReplay,
    "V1 validation-trace script-sources StageThreeReplay yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageThreeFinishWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageThreeFinish,
    "V1 validation-trace script-sources StageThreeFinish yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageFourBeginWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageFourBegin,
    "V1 validation-trace script-sources StageFourBegin yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageFourFinishWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageFourFinish,
    "V1 validation-trace script-sources StageFourFinish yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageSixBeginPolicyWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageSixBeginPolicy,
    "V1 validation-trace script-sources StageSixBeginPolicy yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageSixFoldAssetWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageSixFoldAsset,
    "V1 validation-trace script-sources StageSixFoldAsset yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesStageSixFinishWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesStageSixFinish,
    "V1 validation-trace script-sources StageSixFinish yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesObserverItemWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesObserverItem,
    "V1 validation-trace script-sources observer item yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesObserverBoundWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesObserverBound,
    "V1 validation-trace script-sources observer bound yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeScriptSourcesRedeemerDescriptorWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .scriptSourcesRedeemerDescriptor,
    "V1 validation-trace script-sources redeemer descriptor yield",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsAdvanceSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[10],
    "V1 validation-trace phase-A native Advance semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsItemSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[11],
    "V1 validation-trace phase-A native Item semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsTokenHeadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[12],
    "V1 validation-trace phase-A native TokenHead semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[13],
    "V1 validation-trace phase-A native AllOrAnyContainerFramePayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[14],
    "V1 validation-trace phase-A native AllOrAnyEmptyContainerPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[15],
    "V1 validation-trace phase-A native AtLeastContainerFramePayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[16],
    "V1 validation-trace phase-A native AtLeastEmptyContainerPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[17],
    "V1 validation-trace phase-A native TimelockPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[18],
    "V1 validation-trace phase-A native SignatureMembershipPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[19],
    "V1 validation-trace phase-A native SignatureEmptyPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[20],
    "V1 validation-trace phase-A native SignatureBelowFirstPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[21],
    "V1 validation-trace phase-A native SignatureAboveLastPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[22],
    "V1 validation-trace phase-A native SignatureBetweenPayload semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseANativeScriptsFrameSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[23],
    "V1 validation-trace phase-A native Frame semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[24],
    "V1 validation-trace phase-A preconditions Finalize semantic",
  ),
  spendDescriptor(
    "validationTraceDisputePhaseAScriptPreconditionsItemSemantic",
    contracts.fraudProofContracts.validationTraceDispute.semanticResolvers[25],
    "V1 validation-trace phase-A preconditions Item semantic",
  ),
  ...registeredFaultProofScriptDescriptors(contracts),
  mintDescriptor(
    "computationThreadMint",
    contracts.computationThread,
    "V1 fraud-proof computation-thread minting",
  ),
  withdrawalDescriptor(
    "chunkedVerifyWithdraw",
    contracts.chunkedVerify,
    "V1 MPF chunked-verify withdrawal",
  ),
  withdrawalDescriptor(
    "pexcludesWithdraw",
    contracts.pexcludes,
    "V1 MPF pexcludes withdrawal",
  ),
  ...legacyFaultProofMissingStepDescriptors(contracts),
  spendDescriptor(
    "validationTraceDisputeCekCoreSettle",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages.settle,
    "V1 validation-trace CEK core settle",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreCompute",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages.compute,
    "V1 validation-trace CEK core compute",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreMachine",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages.machine,
    "V1 validation-trace CEK core machine",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreMapConversion",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .mapConversion,
    "V1 validation-trace CEK core map conversion",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreDirectScalar",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .directScalar,
    "V1 validation-trace CEK core direct scalar",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreDirectStructured",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .directStructured,
    "V1 validation-trace CEK core direct structured",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreDirectScalarBudget",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .directScalarBudget,
    "V1 validation-trace CEK core direct scalar budget",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreDirectStructuredBudget",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .directStructuredBudget,
    "V1 validation-trace CEK core direct structured budget",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreDirectScalarRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .directScalarRoots,
    "V1 validation-trace CEK core direct scalar roots",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreDirectStructuredRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .directStructuredRoots,
    "V1 validation-trace CEK core direct structured roots",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticPair",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticPair,
    "V1 validation-trace CEK core semantic pair",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticListConstruct",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticListConstruct,
    "V1 validation-trace CEK core semantic list construct",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticListSelect",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticListSelect,
    "V1 validation-trace CEK core semantic list select",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticChoose",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticChoose,
    "V1 validation-trace CEK core semantic choose",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticDataConstruct",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticDataConstruct,
    "V1 validation-trace CEK core semantic data construct",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticDataScalar",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticDataScalar,
    "V1 validation-trace CEK core semantic data scalar",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticDataMisc",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticDataMisc,
    "V1 validation-trace CEK core semantic data misc",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticBudget",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticBudget,
    "V1 validation-trace CEK core semantic budget",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticRoots,
    "V1 validation-trace CEK core semantic roots",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticResult",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticResult,
    "V1 validation-trace CEK core semantic result",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreMapStartNodes",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .mapStartNodes,
    "V1 validation-trace CEK core map start nodes",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreMapStartBudget",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .mapStartBudget,
    "V1 validation-trace CEK core map start budget",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreMapStartRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .mapStartRoots,
    "V1 validation-trace CEK core map start roots",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticFailureMaterial",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticFailureMaterial,
    "V1 validation-trace CEK core semantic failure material",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreSemanticFailureRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .semanticFailureRoots,
    "V1 validation-trace CEK core semantic failure roots",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreBlsFinal",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages.blsFinal,
    "V1 validation-trace CEK core bls final",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreBlsBudget",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .blsBudget,
    "V1 validation-trace CEK core bls budget",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreBlsRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages.blsRoots,
    "V1 validation-trace CEK core bls roots",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreFailureBudget",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .failureBudget,
    "V1 validation-trace CEK core failure budget",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreFailureKnown",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .failureKnown,
    "V1 validation-trace CEK core failure known",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreTypeFailureKinds",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .typeFailureKinds,
    "V1 validation-trace CEK core type failure kinds",
  ),
  spendDescriptor(
    "validationTraceDisputeCekCoreTypeFailureRoots",
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
      .typeFailureRoots,
    "V1 validation-trace CEK core type failure roots",
  ),

  spendDescriptor(
    "validationTraceDisputeCekMaterialTraversal",
    contracts.fraudProofContracts.validationTraceDispute.cekMaterialTraversal,
    "V1 validation-trace CEK material traversal",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeCekMaterialProgramTaskWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .cekMaterialProgramTask,
    "V1 validation-trace CEK material program task yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeCekMaterialDataTaskWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .cekMaterialDataTask,
    "V1 validation-trace CEK material Data task yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeCekSelectionAuthenticateWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .cekSelectionAuthenticate,
    "V1 validation-trace CEK selection authenticate yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeCekSelectionSuccessorWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .cekSelectionSuccessor,
    "V1 validation-trace CEK selection successor yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeCekSelectionMaterialProgramWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .cekSelectionMaterialProgram,
    "V1 validation-trace CEK selection material program yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeCekSelectionMaterialDataWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .cekSelectionMaterialData,
    "V1 validation-trace CEK selection material data yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputePhaseANativeItemNativeWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .phaseANativeItemNative,
    "V1 validation-trace phase-A native item native yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputePhaseANativeItemForeignWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .phaseANativeItemForeign,
    "V1 validation-trace phase-A native item foreign yield",
  ),
  withdrawalDescriptor(
    "validationTraceDisputeValueAndMintAssetFoldWithdraw",
    contracts.fraudProofContracts.validationTraceDispute.yields
      .valueAndMintAssetFold,
    "V1 validation-trace value-and-mint asset-fold yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.l2Open,
    "V1 fraud-proof transition-trace final-4 L2 open yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.l2Summaries,
    "V1 fraud-proof transition-trace final-4 L2 summaries yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.l2Replay,
    "V1 fraud-proof transition-trace final-4 L2 replay yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.claimStructure,
    "V1 fraud-proof transition-trace final-4 claim structure yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.claimSource,
    "V1 fraud-proof transition-trace final-4 claim source yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.claimEndpoints,
    "V1 fraud-proof transition-trace final-4 claim endpoints yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceDepositProjectionWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.depositProjection,
    "V1 fraud-proof transition-trace final-5 projection yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceDepositSummariesWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.depositSummaries,
    "V1 fraud-proof transition-trace final-5 summaries yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.l2Assembly,
    "V1 fraud-proof transition-trace final-4 L2 assembly yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.l2Scan,
    "V1 fraud-proof transition-trace final-4 L2 scan yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.l2Value,
    "V1 fraud-proof transition-trace final-4 L2 value yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceDepositAssemblyWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.depositAssembly,
    "V1 fraud-proof transition-trace final-5 assembly yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceDepositScanWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.depositScan,
    "V1 fraud-proof transition-trace final-5 scan yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceDepositValueWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.depositValue,
    "V1 fraud-proof transition-trace final-5 value yield",
  ),
  withdrawalDescriptor(
    "fraudProofTransitionTraceDepositReplayWithdraw",
    contracts.fraudProofContracts.transitionTrace.yields.depositReplay,
    "V1 fraud-proof transition-trace final-5 replay yield",
  ),
  withdrawalDescriptor(
    "fraudProofMinAdaStep02TxWithdraw",
    contracts.fraudProofContracts.minAda.yields.tx,
    "V1 fraud-proof min-ada step-02 tx yield",
  ),
  withdrawalDescriptor(
    "fraudProofMinAdaStep02UtxoWithdraw",
    contracts.fraudProofContracts.minAda.yields.utxo,
    "V1 fraud-proof min-ada step-02 UTxO yield",
  ),
  spendDescriptor(
    "correctionLockSpend",
    contracts.correctionLock,
    "correction-lock spending",
  ),
  spendDescriptor(
    "availabilityChallengeSpend",
    contracts.availabilityChallenge,
    "availability-challenge spending",
  ),
  mintDescriptor(
    "availabilityChallengeMint",
    contracts.availabilityChallenge,
    "availability-challenge minting",
  ),
];

const defaultSteps = (): DeploymentManifest["steps"] => ({
  prepareHubOracleNonce: { status: "pending" },
  deployNodeRuntimeReferenceScripts: { status: "pending" },
  initProtocol: { status: "pending" },
  phasRegistration: { status: "pending" },
  operatorRegistration: { status: "pending" },
  operatorActivation: { status: "pending" },
});

const buildReferenceScriptRecords = (
  deploymentInfo: ContractDeploymentInfo,
): DeploymentManifest["referenceScripts"] => {
  const entries: [string, DeploymentManifest["referenceScripts"][string]][] =
    [];
  for (const [contractName, entry] of Object.entries(
    deploymentInfo.contracts,
  )) {
    const targetName = REFERENCE_SCRIPT_TARGET_BY_CONTRACT_NAME[contractName];
    if (targetName === undefined) {
      continue;
    }
    const refScript = entry.refScriptUTxO;
    if (refScript === null) {
      throw new Error(
        `Cannot finalize DeploymentManifestV1 without reference script ${targetName}`,
      );
    }
    entries.push([
      targetName,
      {
        status: "confirmed",
        roleUnit: referenceScriptAuthUnit(
          deploymentInfo.referenceScriptAuthPolicy.policyId,
          targetName,
        ),
        scriptHash: entry.scriptHash,
        outRef: `${refScript.txHash}#${refScript.outputIndex.toString()}`,
      },
    ]);
  }
  return Object.fromEntries(
    entries.sort(([left], [right]) => left.localeCompare(right)),
  );
};

export type DeploymentManifestBuildContext = {
  readonly network: string;
  readonly cardanoProtocolParameters: DeploymentManifestValue["cardanoProtocolParameters"];
  readonly genesis: DeploymentManifestValue["genesis"];
  readonly da: DeploymentManifestValue["da"];
  readonly proofEvidence: DeploymentManifestValue["proofEvidence"];
  readonly economics: DeploymentManifestEconomics;
  readonly availabilityChallenge: DeploymentManifestAvailabilityChallenge;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShotTxHash: string;
  readonly hubOracleOneShotOutputIndex: number;
  readonly hubOracleOneShotStatus?: DeploymentManifest["hubOracleOneShot"]["status"];
  readonly now?: Date;
  readonly existingManifest?: DeploymentManifest;
  readonly steps?: Partial<DeploymentManifest["steps"]>;
};

export type DeploymentManifestIdentityContext = Pick<
  DeploymentManifestBuildContext,
  | "cardanoProtocolParameters"
  | "genesis"
  | "da"
  | "proofEvidence"
  | "economics"
  | "availabilityChallenge"
>;

const configuredDeploymentEconomics = (): DeploymentManifestEconomics =>
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[
    deploymentEconomicsProfileFromEnvironment()
  ];

const configuredAvailabilityChallenge =
  (): DeploymentManifestAvailabilityChallenge =>
    parseDeploymentManifestAvailabilityChallenge(
      daAvailabilityChallengeEnvironmentInput(
        (name) => `${name} must be an explicit positive decimal integer`,
      ),
    );

const protocolRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Record<string, unknown>;
};

const protocolNatural = (value: unknown, field: string): string => {
  if (typeof value === "bigint" && value >= 0n) return value.toString(10);
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString(10);
  }
  if (typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value)) {
    return value;
  }
  throw new Error(`${field} must be a canonical natural`);
};

const protocolGcd = (left: bigint, right: bigint): bigint => {
  let a = left < 0n ? -left : left;
  let b = right < 0n ? -right : right;
  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }
  return a;
};

const protocolRational = (
  value: unknown,
  field: string,
): DeploymentManifestCanonicalRational => {
  let numerator: bigint;
  let denominator: bigint;
  if (typeof value === "string" && /^[0-9]+\/[1-9][0-9]*$/u.test(value)) {
    const [rawNumerator, rawDenominator] = value.split("/") as [string, string];
    numerator = BigInt(rawNumerator);
    denominator = BigInt(rawDenominator);
  } else if (
    (typeof value === "number" && Number.isFinite(value) && value >= 0) ||
    (typeof value === "string" &&
      /^(?:0|[1-9][0-9]*)(?:\.[0-9]+)?$/u.test(value))
  ) {
    const decimal = typeof value === "number" ? value.toString() : value;
    if (/e/i.test(decimal)) {
      throw new Error(`${field} must not use exponent notation`);
    }
    const [whole, fractional = ""] = decimal.split(".") as [string, string?];
    denominator = 10n ** BigInt(fractional.length);
    numerator = BigInt(`${whole}${fractional}`);
  } else {
    throw new Error(`${field} must be a nonnegative exact rational`);
  }
  if (denominator <= 0n)
    throw new Error(`${field} denominator must be positive`);
  const divisor = protocolGcd(numerator, denominator);
  return Object.freeze({
    numerator: (numerator / divisor).toString(10),
    denominator: (denominator / divisor).toString(10),
  });
};

const sameRational = (
  left: DeploymentManifestCanonicalRational,
  right: DeploymentManifestCanonicalRational,
): boolean =>
  left.numerator === right.numerator && left.denominator === right.denominator;

const exactProtocolParameterSnapshot = (
  providerValue: unknown,
  rawOgmiosValue: unknown,
): DeploymentManifestCardanoProtocolParameters => {
  const provider = protocolRecord(providerValue, "Lucid protocol parameters");
  const snapshot =
    deriveDeploymentManifestCardanoProtocolParametersFromOgmios(rawOgmiosValue);
  const providerChecks: readonly [string, string][] = Object.freeze([
    [protocolNatural(provider.minFeeA, "provider.minFeeA"), snapshot.minFeeA],
    [protocolNatural(provider.minFeeB, "provider.minFeeB"), snapshot.minFeeB],
    [
      protocolNatural(provider.maxTxSize, "provider.maxTxSize"),
      snapshot.maxTxSize,
    ],
    [
      protocolNatural(provider.maxValSize, "provider.maxValSize"),
      snapshot.maxValueSize,
    ],
    [
      protocolNatural(provider.maxTxExMem, "provider.maxTxExMem"),
      snapshot.maxTxExUnits.memory,
    ],
    [
      protocolNatural(provider.maxTxExSteps, "provider.maxTxExSteps"),
      snapshot.maxTxExUnits.steps,
    ],
    [
      protocolNatural(provider.coinsPerUtxoByte, "provider.coinsPerUtxoByte"),
      snapshot.coinsPerUtxoByte,
    ],
    [
      protocolNatural(
        provider.collateralPercentage,
        "provider.collateralPercentage",
      ),
      snapshot.collateralPercentage,
    ],
    [
      protocolNatural(
        provider.maxCollateralInputs,
        "provider.maxCollateralInputs",
      ),
      snapshot.maxCollateralInputs,
    ],
  ]);
  if (providerChecks.some(([observed, expected]) => observed !== expected)) {
    throw new Error("Lucid and raw Ogmios protocol parameters disagree");
  }
  if (
    !sameRational(
      protocolRational(provider.priceMem, "provider.priceMem"),
      snapshot.priceMemory,
    ) ||
    !sameRational(
      protocolRational(provider.priceStep, "provider.priceStep"),
      snapshot.priceSteps,
    ) ||
    !sameRational(
      protocolRational(
        provider.minFeeRefScriptCostPerByte,
        "provider.minFeeRefScriptCostPerByte",
      ),
      snapshot.referenceScriptFee.base,
    )
  ) {
    throw new Error(
      "Lucid and raw Ogmios rational protocol parameters disagree",
    );
  }
  return snapshot;
};

export const cardanoProtocolParametersIdentityFromProvider = async (
  provider: {
    readonly getProtocolParameters: () => Promise<unknown>;
  },
  rawOgmiosProtocolParameters: unknown,
): Promise<DeploymentManifestValue["cardanoProtocolParameters"]> => {
  const snapshot = exactProtocolParameterSnapshot(
    await provider.getProtocolParameters(),
    rawOgmiosProtocolParameters,
  );
  return {
    snapshot,
    digest: computeDeploymentManifestJsonDigest(snapshot),
  };
};

export const queryLocalOgmiosProtocolParameters = async (
  ogmiosUrl: string,
  fetchImpl: typeof fetch = fetch,
): Promise<unknown> => {
  const response = await fetchImpl(normalizeOgmiosHttpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/protocolParameters",
      id: "midgard-deployment-protocol-parameters-v1",
    }),
    signal: AbortSignal.timeout(30_000),
  });
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Ogmios protocol-parameter query failed with HTTP ${response.status.toString()}`,
    );
  }
  let payload: unknown;
  try {
    payload = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("Ogmios protocol-parameter response is not JSON", {
      cause,
    });
  }
  const envelope = protocolRecord(
    payload,
    "Ogmios protocol parameters response",
  );
  if (
    envelope.jsonrpc !== "2.0" ||
    envelope.id !== "midgard-deployment-protocol-parameters-v1" ||
    Object.prototype.hasOwnProperty.call(envelope, "error") ||
    !Object.prototype.hasOwnProperty.call(envelope, "result")
  ) {
    throw new Error("Ogmios protocol-parameter response identity is invalid");
  }
  return payload;
};

const genesisUtxoIdentitySnapshot = (
  utxos: readonly UTxO[],
): ReturnType<typeof normalizeDeploymentManifestJsonValue> =>
  normalizeDeploymentManifestJsonValue(
    [...utxos].sort(compareOutRefs).map((utxo) => ({
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
      address: utxo.address,
      assets: Object.fromEntries(
        Object.entries(utxo.assets)
          .sort(([left], [right]) => left.localeCompare(right))
          .map(([unit, amount]) => [unit, amount.toString(10)]),
      ),
      datumHash: utxo.datumHash ?? null,
      datum: utxo.datum ?? null,
      scriptRef:
        utxo.scriptRef == null
          ? null
          : {
              type: utxo.scriptRef.type,
              script: utxo.scriptRef.script,
            },
    })),
    "genesisUtxos",
  );

export const buildDeploymentManifestIdentityContextProgram: Effect.Effect<
  DeploymentManifestIdentityContext,
  Error,
  Lucid | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucidService = yield* Lucid;
  const cardanoProtocolParameters = yield* Effect.tryPromise({
    try: async () => {
      const provider = lucidService.api.config().provider;
      if (provider === undefined) {
        throw new Error("Lucid has no configured Cardano provider");
      }
      const rawOgmiosProtocolParameters =
        await queryLocalOgmiosProtocolParameters(nodeConfig.L1_OGMIOS_KEY);
      return cardanoProtocolParametersIdentityFromProvider(
        provider,
        rawOgmiosProtocolParameters,
      );
    },
    catch: (cause) =>
      new Error(
        `Failed to obtain the trusted Cardano protocol-parameter snapshot: ${String(cause)}`,
      ),
  });
  const daParams = yield* deriveOperatorDaParams(nodeConfig).pipe(
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to derive deployment-manifest DA identity: ${String(cause)}`,
        ),
    ),
  );
  const committeeVkeys = daParams.committee.match(/[0-9a-f]{64}/gu) ?? [];
  if (committeeVkeys.join("") !== daParams.committee) {
    return yield* Effect.fail(
      new Error(
        "Failed to split the packed DA committee into exact 32-byte verification keys",
      ),
    );
  }
  const threshold = Number(daParams.da_threshold);
  if (!Number.isSafeInteger(threshold) || threshold <= 0) {
    return yield* Effect.fail(
      new Error("DA threshold does not fit the V1 manifest integer envelope"),
    );
  }
  const blueprintHash = yield* loadRealBlueprintSha256();
  const genesisSnapshot = genesisUtxoIdentitySnapshot(nodeConfig.GENESIS_UTXOS);
  return {
    economics: configuredDeploymentEconomics(),
    availabilityChallenge: configuredAvailabilityChallenge(),
    cardanoProtocolParameters,
    genesis: {
      headerHash: GENESIS_HEADER_HASH,
      utxoSetDigest: computeDeploymentManifestJsonDigest(genesisSnapshot),
    },
    da: {
      committeeVkeys,
      committeeSignersHash:
        computeDeploymentManifestDaCommitteeSignersHash(committeeVkeys),
      threshold,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
        envelopeEncoding: nodeConfig.MIDGARD_DA_PAYLOAD_ENVELOPE,
        zstdLevel: nodeConfig.MIDGARD_DA_ZSTD_LEVEL,
        limits: DA_TRANSPORT_LIMITS,
        retentionDays: nodeConfig.RETENTION_DAYS,
      },
    },
    proofEvidence: {
      digest: MIDGARD_RELEASE_EVIDENCE_DIGEST,
      blueprintHash,
    },
  };
});

const assertOutRefFields = (txHash: string, outputIndex: number): void => {
  if (!/^[0-9a-fA-F]{64}$/.test(txHash)) {
    throw new Error("hubOracleOneShot.txHash must be 32 bytes of hex");
  }
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new Error(
      "hubOracleOneShot.outputIndex must be a safe non-negative integer",
    );
  }
};

const withManifestId = (
  manifest: Omit<DeploymentManifest, "manifestId">,
): DeploymentManifest => ({
  ...manifest,
  manifestId: computeDeploymentManifestId(manifest),
});

/**
 * Builds the sole canonical V1 manifest and re-parses it before return so
 * missing contracts, tuple drift, and dispute-schedule drift fail closed.
 */
export const buildDeploymentManifest = (
  deploymentInfo: ContractDeploymentInfo,
  context: DeploymentManifestBuildContext,
): DeploymentManifest => {
  assertOutRefFields(
    context.hubOracleOneShotTxHash,
    context.hubOracleOneShotOutputIndex,
  );
  const nowIso = (context.now ?? new Date()).toISOString();
  const referenceScripts = buildReferenceScriptRecords(deploymentInfo);
  const hubOracleOneShotStatus =
    context.hubOracleOneShotStatus ??
    context.existingManifest?.hubOracleOneShot.status;
  if (hubOracleOneShotStatus !== "consumed_by_init") {
    throw new Error(
      "Cannot finalize DeploymentManifestV1 before the hub-oracle one-shot is consumed by initialization",
    );
  }
  const baseSteps = {
    ...defaultSteps(),
    prepareHubOracleNonce: { status: "complete" as const },
    deployNodeRuntimeReferenceScripts: {
      status: "complete" as const,
    },
    ...(context.existingManifest?.steps ?? {}),
    ...(context.steps ?? {}),
  };
  const manifest = withManifestId({
    schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    network: context.network,
    cardanoProtocolParameters: context.cardanoProtocolParameters,
    genesis: context.genesis,
    createdAt: context.existingManifest?.createdAt ?? nowIso,
    updatedAt: context.existingManifest?.updatedAt ?? nowIso,
    referenceScriptDeployAddress: context.referenceScriptDeployAddress,
    hubOracleOneShot: {
      txHash: context.hubOracleOneShotTxHash.toLowerCase(),
      outputIndex: context.hubOracleOneShotOutputIndex,
      outRef: `${context.hubOracleOneShotTxHash.toLowerCase()}#${context.hubOracleOneShotOutputIndex.toString()}`,
      status: hubOracleOneShotStatus,
    },
    referenceScriptAuthPolicy: deploymentInfo.referenceScriptAuthPolicy,
    contracts: deploymentInfo.contracts,
    referenceScripts,
    da: context.da,
    proofEvidence: context.proofEvidence,
    steps: baseSteps,
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics: context.economics,
    availabilityChallenge: context.availabilityChallenge,
  }) as DeploymentManifest;
  return parseDeploymentManifestValue(manifest) as DeploymentManifest;
};

export const parseDeploymentManifest = (value: unknown): DeploymentManifest =>
  parseDeploymentManifestValue(value) as DeploymentManifest;

export const readDeploymentManifestFile = (
  outputPath: string,
): DeploymentManifest => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const parsed = JSON.parse(readFileSync(resolvedOutputPath, "utf8"));
  return parseDeploymentManifest(parsed);
};

export const readFinalizedDeploymentIdentity = (
  outputPath: string,
): FinalizedDeploymentIdentity => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const raw = readFileSync(resolvedOutputPath);
  const parsed = JSON.parse(raw.toString("utf8"));
  const manifest = parseDeploymentManifest(parsed);
  return {
    path: resolvedOutputPath,
    manifestId: manifest.manifestId,
    contractDeploymentInfoSha256: createHash("sha256")
      .update(raw)
      .digest("hex"),
    manifest,
  };
};

export const verifyDeploymentManifestAgainstConfig = (
  manifest: DeploymentManifest,
  context: {
    readonly network: string;
    readonly referenceScriptDeployAddress: string;
    readonly hubOracleOneShotTxHash: string;
    readonly hubOracleOneShotOutputIndex: number;
    readonly economicsProfile: DeploymentManifestEconomicsProfile;
    readonly path?: string;
  },
): DeploymentManifestVerificationReport => {
  const mismatches: string[] = [];
  if (manifest.network !== context.network) {
    mismatches.push(
      `network manifest=${manifest.network} config=${context.network}`,
    );
  }
  if (
    manifest.referenceScriptDeployAddress !==
    context.referenceScriptDeployAddress
  ) {
    mismatches.push(
      `referenceScriptDeployAddress manifest=${manifest.referenceScriptDeployAddress} config=${context.referenceScriptDeployAddress}`,
    );
  }
  if (
    manifest.hubOracleOneShot.txHash !==
    context.hubOracleOneShotTxHash.toLowerCase()
  ) {
    mismatches.push(
      `hubOracleOneShot.txHash manifest=${manifest.hubOracleOneShot.txHash} config=${context.hubOracleOneShotTxHash}`,
    );
  }
  if (
    manifest.hubOracleOneShot.outputIndex !==
    context.hubOracleOneShotOutputIndex
  ) {
    mismatches.push(
      `hubOracleOneShot.outputIndex manifest=${manifest.hubOracleOneShot.outputIndex.toString()} config=${context.hubOracleOneShotOutputIndex.toString()}`,
    );
  }
  if (manifest.economics.profile !== context.economicsProfile) {
    mismatches.push(
      `economics.profile manifest=${manifest.economics.profile} config=${context.economicsProfile}`,
    );
  }
  return {
    ok: mismatches.length === 0,
    manifestId: manifest.manifestId,
    path: context.path,
    mismatches,
    recommendation:
      mismatches.length === 0 ? "attach" : "correct_attach_config",
  };
};

export const configuredContractDeploymentInfoPath = (): string => {
  const configuredPath = contractDeploymentInfoPathOverride();
  return configuredPath === undefined
    ? defaultContractDeploymentInfoOutputPath()
    : normalizeOutputPath(configuredPath);
};

export const verifyConfiguredDeploymentManifestProgram: Effect.Effect<
  DeploymentManifestVerificationReport,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const path = configuredContractDeploymentInfoPath();
  const manifest = yield* Effect.try({
    try: () => readDeploymentManifestFile(path),
    catch: (cause) =>
      new Error(
        `Failed to read V1 deployment manifest at ${path}: ${String(cause)}`,
      ),
  });
  return verifyDeploymentManifestAgainstConfig(manifest, {
    network: nodeConfig.NETWORK,
    referenceScriptDeployAddress: nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
    hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    economicsProfile: nodeConfig.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE,
    path,
  });
});

export const verifyConfiguredDeploymentManifestIfPresentProgram: Effect.Effect<
  DeploymentManifestVerificationReport | null,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const path = configuredContractDeploymentInfoPath();
  if (!existsSync(path)) {
    return null;
  }
  return yield* verifyConfiguredDeploymentManifestProgram;
});

export const buildContractDeploymentInfoFromContracts = (
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
  referenceScriptOutRefs: ReadonlyMap<
    string,
    ContractDeploymentInfoRefScriptUTxO
  > = new Map(),
  fraudProofCatalogue?: SDK.FraudProofCatalogueDeploymentInfo,
): ContractDeploymentInfo =>
  Object.freeze({
    referenceScriptAuthPolicy,
    contracts: Object.fromEntries(
      collectScriptDescriptors(contracts, referenceScriptAuthPolicy).map(
        (descriptor) => [
          descriptor.name,
          {
            refScriptUTxO: referenceScriptOutRefs.get(descriptor.name) ?? null,
            contract: descriptor.contract,
            scriptHash: descriptor.scriptHash,
            ...(descriptor.name === "fraudProofCatalogueMint" &&
            fraudProofCatalogue !== undefined
              ? { fraudProofCatalogue }
              : {}),
          } satisfies ContractDeploymentInfoEntry,
        ],
      ),
    ),
  });

const resolveLiveContractDeploymentInfoProgram = (
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error, Lucid | MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const descriptors = collectScriptDescriptors(
      contracts,
      referenceScriptAuthPolicy,
    );
    const referenceScriptWalletUtxos = yield* fetchLiveReferenceScriptUtxos();
    const referenceScriptOutRefs = buildReferenceScriptOutRefMap(
      referenceScriptWalletUtxos,
      descriptors,
      referenceScriptAuthPolicy,
    );
    const fraudProofCatalogue = yield* buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    );
    return buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    );
  });

export const buildContractDeploymentInfoProgram = (
  contracts: SDK.MidgardValidators,
  referenceScriptUtxos: readonly UTxO[],
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error> =>
  Effect.gen(function* () {
    const descriptors = collectScriptDescriptors(
      contracts,
      referenceScriptAuthPolicy,
    );
    const referenceScriptOutRefs = buildReferenceScriptOutRefMap(
      referenceScriptUtxos,
      descriptors,
      referenceScriptAuthPolicy,
    );
    const fraudProofCatalogue = yield* buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    );
    return buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    );
  });

export const defaultContractDeploymentInfoOutputPath = (): string =>
  resolvePath(
    resolvePackageRootFromModuleUrl(import.meta.url),
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME,
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME,
  );

const normalizeOutputPath = (outputPath: string): string => {
  const normalized = outputPath.trim();
  if (normalized.length === 0) {
    throw new Error("Contract deployment info output path must not be empty.");
  }
  return resolvePath(normalized);
};

const readReferenceScriptAuthPolicyForLiveWrite = async (
  outputPath: string,
): Promise<ReferenceScriptAuthPolicyDeploymentInfo> => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  if (existsSync(resolvedOutputPath)) {
    return readDeploymentManifestFile(resolvedOutputPath)
      .referenceScriptAuthPolicy;
  }
  const runStatePath = defaultDeploymentRunStatePath();
  const runState = await loadDeploymentRunState(runStatePath);
  const policy = runState?.identity.referenceScriptAuthPolicy;
  if (policy === undefined) {
    throw new Error(
      `Deployment run state at "${runStatePath}" is missing identity.referenceScriptAuthPolicy`,
    );
  }
  return {
    policyId: policy.policyId,
    nativeScript: policy.nativeScript,
    tokenNames: SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: {
      required: true,
      rule: "After the timelock expires, verify there is exactly one role token under this policy for every listed token name before treating the deployment as production-ready.",
    },
  };
};

export const writeContractDeploymentInfoFileProgram = (
  outputPath: string,
  deploymentInfo: ContractDeploymentInfo,
): Effect.Effect<string, Error> =>
  Effect.tryPromise({
    try: async () => {
      const resolvedOutputPath = normalizeOutputPath(outputPath);
      await writeJsonFileAtomic(resolvedOutputPath, deploymentInfo);
      return resolvedOutputPath;
    },
    catch: (cause) =>
      new Error(
        `Failed to write contract deployment info file: ${String(cause)}`,
      ),
  });

export type LiveContractDeploymentInfoWriteOptions = {
  readonly steps?: Partial<DeploymentManifest["steps"]>;
  readonly hubOracleOneShotStatus?: DeploymentManifest["hubOracleOneShot"]["status"];
};

const formatDeploymentManifestVerificationReport = (
  report: DeploymentManifestVerificationReport,
): string =>
  `recommendation=${report.recommendation}; mismatches=[${report.mismatches.join(
    "; ",
  )}]`;

const buildLiveDeploymentManifestProgram = (
  outputPath: string,
  options: LiveContractDeploymentInfoWriteOptions = {},
): Effect.Effect<
  DeploymentManifest,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const identityContext =
      yield* buildDeploymentManifestIdentityContextProgram;
    const referenceScriptAuthPolicy = yield* Effect.tryPromise({
      try: () => readReferenceScriptAuthPolicyForLiveWrite(outputPath),
      catch: (cause) =>
        new Error(
          `Failed to read existing reference-script auth policy metadata: ${String(cause)}`,
        ),
    });
    const deploymentInfo = yield* resolveLiveContractDeploymentInfoProgram(
      referenceScriptAuthPolicy,
    );
    const existingManifest = yield* Effect.sync(() => {
      try {
        return readDeploymentManifestFile(outputPath);
      } catch {
        return undefined;
      }
    });
    const finalizationRequested =
      options.hubOracleOneShotStatus === "consumed_by_init" &&
      options.steps?.initProtocol?.status === "complete";
    if (existingManifest === undefined && !finalizationRequested) {
      return yield* Effect.fail(
        new Error(
          "A first DeploymentManifestV1 may be created only after initialization and reference-script publication are complete",
        ),
      );
    }
    const requestedSteps = finalizationRequested
      ? {
          prepareHubOracleNonce: {
            status: "complete" as const,
            txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH.toLowerCase(),
          },
          deployNodeRuntimeReferenceScripts: {
            status: "complete" as const,
          },
          ...options.steps,
        }
      : options.steps;
    const deploymentManifest = buildDeploymentManifest(deploymentInfo, {
      network: nodeConfig.NETWORK,
      ...identityContext,
      referenceScriptDeployAddress:
        nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
      hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
      hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
      existingManifest,
      steps: requestedSteps,
      hubOracleOneShotStatus: options.hubOracleOneShotStatus,
    });
    const verification = verifyDeploymentManifestAgainstConfig(
      deploymentManifest,
      {
        network: nodeConfig.NETWORK,
        referenceScriptDeployAddress:
          nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
        hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
        hubOracleOneShotOutputIndex:
          nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
        economicsProfile: nodeConfig.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE,
        path: outputPath,
      },
    );
    if (!verification.ok) {
      return yield* Effect.fail(
        new Error(
          `Refusing to write deployment manifest with configuration drift: ${formatDeploymentManifestVerificationReport(
            verification,
          )}`,
        ),
      );
    }
    return deploymentManifest;
  });

export const writeLiveContractDeploymentInfoProgram = (
  outputPath: string,
  options: LiveContractDeploymentInfoWriteOptions = {},
): Effect.Effect<string, Error, Lucid | MidgardContracts | NodeConfig> =>
  Effect.gen(function* () {
    const deploymentManifest = yield* buildLiveDeploymentManifestProgram(
      outputPath,
      options,
    );
    const marker = makeDeploymentMarker(deploymentManifest.manifestId);
    const runStatePath = defaultDeploymentRunStatePath();
    const runState = yield* Effect.tryPromise({
      try: () => loadDeploymentRunState(runStatePath),
      catch: (cause) =>
        new Error(`Failed to inspect deployment run-state identity`, {
          cause,
        }),
    });
    if (
      runState?.identity.deploymentMarker !== undefined &&
      runState.identity.deploymentMarker.manifestId !== marker.manifestId
    ) {
      return yield* Effect.fail(
        new Error(
          `Refusing to replace final deployment manifest ${runState.identity.deploymentMarker.manifestId} with ${marker.manifestId}; start an explicit fresh deployment run instead`,
        ),
      );
    }
    const manifestPath = yield* writeContractDeploymentInfoFileProgram(
      outputPath,
      deploymentManifest,
    );
    if (runState !== null) {
      const manifestSha256 = yield* Effect.tryPromise({
        try: () => sha256File(manifestPath),
        catch: (cause) =>
          new Error(`Failed to hash final deployment manifest`, { cause }),
      });
      yield* Effect.tryPromise({
        try: () =>
          mutateDeploymentRunState(
            runStatePath,
            () => {
              throw new Error(
                "Deployment run state disappeared before final marker binding",
              );
            },
            (current) =>
              bindDeploymentRunStateToMarker(current, {
                marker,
                manifestPath,
                manifestSha256,
              }),
          ),
        catch: (cause) =>
          new Error(`Failed to bind deployment run state to final manifest`, {
            cause,
          }),
      });
    }
    return manifestPath;
  });

export type ReconcileInitializedDeploymentManifestOptions = {
  readonly outputPath: string;
  readonly initTxHash: string;
};

export type ReconcileInitializedDeploymentManifestSummary = {
  readonly status: "complete";
  readonly path: string;
  readonly manifestId: string;
  readonly initTxHash: string;
  readonly hubOracleOutRef: string;
  readonly referenceScriptsConfirmed: number;
};

export const reconcileInitializedDeploymentManifestProgram = ({
  outputPath,
  initTxHash,
}: ReconcileInitializedDeploymentManifestOptions): Effect.Effect<
  ReconcileInitializedDeploymentManifestSummary,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const normalizedInitTxHash = initTxHash.toLowerCase();
    const deploymentStatus = yield* fetchProtocolDeploymentStatus(
      lucidService.api,
      contracts,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error("Failed to inspect live protocol deployment status", {
            cause,
          }),
      ),
    );
    if (!deploymentStatus.complete) {
      return yield* Effect.fail(
        new Error(
          `Cannot reconcile deployment manifest for an incomplete protocol deployment: missing_components=[${deploymentStatus.missingComponents.join(
            ",",
          )}],state_queue_healthy=${deploymentStatus.stateQueueTopology.healthy.toString()}`,
        ),
      );
    }
    const hubOracleWitness = deploymentStatus.hubOracleWitness;
    if (hubOracleWitness === null) {
      return yield* Effect.fail(
        new Error(
          "Cannot reconcile deployment manifest without hub-oracle witness",
        ),
      );
    }
    const liveInitTxHash = hubOracleWitness.txHash.toLowerCase();
    if (liveInitTxHash !== normalizedInitTxHash) {
      return yield* Effect.fail(
        new Error(
          `Init transaction mismatch: live hub-oracle witness was created by ${liveInitTxHash}, expected ${normalizedInitTxHash}`,
        ),
      );
    }

    const path = yield* writeLiveContractDeploymentInfoProgram(outputPath, {
      hubOracleOneShotStatus: "consumed_by_init",
      steps: {
        initProtocol: {
          status: "complete",
          txHash: normalizedInitTxHash,
        },
      },
    });
    const manifest = yield* Effect.try({
      try: () => readDeploymentManifestFile(path),
      catch: (cause) =>
        new Error(
          `Failed to read reconciled deployment manifest: ${String(cause)}`,
        ),
    });
    return {
      status: "complete" as const,
      path,
      manifestId: manifest.manifestId,
      initTxHash: normalizedInitTxHash,
      hubOracleOutRef: `${hubOracleWitness.txHash}#${hubOracleWitness.outputIndex.toString()}`,
      referenceScriptsConfirmed: Object.values(
        manifest.referenceScripts,
      ).filter((record) => record.status === "confirmed").length,
    };
  });

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
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
  type DeploymentManifestV1AvailabilityChallenge,
  type DeploymentManifestV1CanonicalRational,
  type DeploymentManifestV1CardanoProtocolParameters,
  type DeploymentManifestV1Economics,
  type DeploymentManifestV1EconomicsProfile,
  deriveDeploymentManifestV1CardanoProtocolParametersFromOgmios,
  makeDeploymentMarkerV1,
  parseDeploymentManifestV1AvailabilityChallenge,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
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
  computeDeploymentManifestId,
  computeDeploymentManifestV1DaCommitteeSignersHash,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  type DeploymentManifestV1Value,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentManifestV1Value,
} from "@/deployment-manifest-v1.js";
import {
  bindDeploymentRunStateToMarkerV1,
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  sha256File,
} from "@/e2e/run-state.js";
import { writeJsonFileAtomic } from "@/files/atomic-write.js";
import { normalizeOgmiosHttpUrl } from "@/local-ledger-slot.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  loadRealBlueprintSha256,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  deriveOperatorDaParams,
  fetchProtocolDeploymentStatus,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";
import { fetchReferenceScriptUtxosAt } from "@/transactions/reference-scripts.js";
import { compareOutRefs } from "@/tx-context.js";

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

export { computeDeploymentManifestId, DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION };

export type DeploymentManifestStepStatus =
  | "pending"
  | "in_progress"
  | "submitted"
  | "complete"
  | "attached"
  | "failed"
  | "blocked_requires_fresh_redeploy";

export type DeploymentManifestV1 = ContractDeploymentInfo & {
  readonly schemaVersion: typeof DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly consensusProfileDigest: string;
  readonly network: string;
  readonly cardanoProtocolParameters: DeploymentManifestV1Value["cardanoProtocolParameters"];
  readonly genesis: DeploymentManifestV1Value["genesis"];
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
  readonly da: DeploymentManifestV1Value["da"];
  readonly proofEvidence: DeploymentManifestV1Value["proofEvidence"];
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
  readonly l1Finality: DeploymentManifestV1Value["l1Finality"];
  readonly economics: DeploymentManifestV1Value["economics"];
  readonly availabilityChallenge: DeploymentManifestV1Value["availabilityChallenge"];
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
  readonly manifest: DeploymentManifestV1;
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
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([targetName, contractName]) => {
      if (!(targetName in SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)) {
        throw new Error(
          `Deployment manifest reference-script role is not registered by the SDK: ${targetName}`,
        );
      }
      return [contractName, targetName as ReferenceScriptAuthTokenTarget];
    }),
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
 * `midgard-core`'s `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES` names five
 * `missingNativeScriptUtxo` steps and three `nativeScriptInvalid` steps, while
 * `onchain/aiken` compiles seven and five and `midgard-fault-proofs`'s runtime
 * submits against `fraudProofMissingNativeScriptUtxoStep06`/`Step07` and
 * `fraudProofNativeScriptInvalidStep04`/`Step05`. `validateContracts` takes an
 * EXACT key set, so a manifest carrying those four surplus steps is rejected
 * outright and one omitting the five/three is rejected too — this builder can
 * only emit what the ABI names. Extending the ABI moves the manifest identity
 * for `midgard-core`, `midgard-watcher`, `da-committee-node` and this package,
 * so it is not a change this module can make on its own.
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

const defaultSteps = (): DeploymentManifestV1["steps"] => ({
  prepareHubOracleNonce: { status: "pending" },
  deployNodeRuntimeReferenceScripts: { status: "pending" },
  initProtocol: { status: "pending" },
  phasRegistration: { status: "pending" },
  operatorRegistration: { status: "pending" },
  operatorActivation: { status: "pending" },
});

const buildReferenceScriptRecords = (
  deploymentInfo: ContractDeploymentInfo,
): DeploymentManifestV1["referenceScripts"] => {
  const entries: [string, DeploymentManifestV1["referenceScripts"][string]][] =
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
  readonly cardanoProtocolParameters: DeploymentManifestV1Value["cardanoProtocolParameters"];
  readonly genesis: DeploymentManifestV1Value["genesis"];
  readonly da: DeploymentManifestV1Value["da"];
  readonly proofEvidence: DeploymentManifestV1Value["proofEvidence"];
  readonly economics: DeploymentManifestV1Economics;
  readonly availabilityChallenge: DeploymentManifestV1AvailabilityChallenge;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShotTxHash: string;
  readonly hubOracleOneShotOutputIndex: number;
  readonly hubOracleOneShotStatus?: DeploymentManifestV1["hubOracleOneShot"]["status"];
  readonly now?: Date;
  readonly existingManifest?: DeploymentManifestV1;
  readonly steps?: Partial<DeploymentManifestV1["steps"]>;
};

export type DeploymentManifestV1IdentityContext = Pick<
  DeploymentManifestBuildContext,
  | "cardanoProtocolParameters"
  | "genesis"
  | "da"
  | "proofEvidence"
  | "economics"
  | "availabilityChallenge"
>;

const configuredDeploymentEconomics = (): DeploymentManifestV1Economics => {
  const profile = process.env.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE?.trim();
  if (
    profile !== "public-preprod-launch-v1" &&
    profile !== "bounded-acceptance-v1"
  ) {
    throw new Error(
      "MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE must explicitly equal public-preprod-launch-v1 or bounded-acceptance-v1",
    );
  }
  return DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE[
    profile as DeploymentManifestV1EconomicsProfile
  ];
};

const configuredAvailabilityChallenge =
  (): DeploymentManifestV1AvailabilityChallenge => {
    const requiredInteger = (name: string): number => {
      const raw = process.env[name]?.trim();
      if (raw === undefined || !/^[1-9][0-9]*$/u.test(raw)) {
        throw new Error(`${name} must be an explicit positive decimal integer`);
      }
      const value = Number(raw);
      if (!Number.isSafeInteger(value)) {
        throw new Error(`${name} must fit a JavaScript safe integer`);
      }
      return value;
    };
    return parseDeploymentManifestV1AvailabilityChallenge({
      responseClasses: {
        smallPayloadMaxBytes: 65_536,
        smallResponseWindowMs: 3_600_000,
        fullPayloadMaxBytes: 67_108_864,
        fullResponseWindowMs: 172_800_000,
      },
      responseGeometry: {
        chunkByteLength: requiredInteger(
          "MIDGARD_DA_AVAILABILITY_CHUNK_BYTE_LENGTH",
        ),
        trancheByteLength: requiredInteger(
          "MIDGARD_DA_AVAILABILITY_TRANCHE_BYTE_LENGTH",
        ),
        maxTrancheCount: requiredInteger(
          "MIDGARD_DA_AVAILABILITY_MAX_TRANCHE_COUNT",
        ),
      },
      daBondLovelace: requiredInteger("MIDGARD_DA_AVAILABILITY_BOND_LOVELACE"),
      challengerBondLovelace: requiredInteger(
        "MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE",
      ),
      maxOpenFeeLovelace: requiredInteger(
        "MIDGARD_DA_AVAILABILITY_MAX_OPEN_FEE_LOVELACE",
      ),
      maxPublicationFeeLovelace: requiredInteger(
        "MIDGARD_DA_AVAILABILITY_MAX_PUBLICATION_FEE_LOVELACE",
      ),
      maxSettlementFeeLovelace: requiredInteger(
        "MIDGARD_DA_AVAILABILITY_MAX_SETTLEMENT_FEE_LOVELACE",
      ),
      maxCloseFeeLovelace: requiredInteger(
        "MIDGARD_DA_AVAILABILITY_MAX_CLOSE_FEE_LOVELACE",
      ),
      maxTimeoutFeeLovelace: requiredInteger(
        "MIDGARD_DA_AVAILABILITY_MAX_TIMEOUT_FEE_LOVELACE",
      ),
      bondOwnerCredential: (() => {
        const value =
          process.env.MIDGARD_DA_AVAILABILITY_BOND_OWNER_CREDENTIAL?.trim();
        if (value === undefined || !/^[0-9a-f]{56}$/u.test(value)) {
          throw new Error(
            "MIDGARD_DA_AVAILABILITY_BOND_OWNER_CREDENTIAL must be exactly 28 lowercase hex bytes",
          );
        }
        return value;
      })(),
    });
  };

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
): DeploymentManifestV1CanonicalRational => {
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
  left: DeploymentManifestV1CanonicalRational,
  right: DeploymentManifestV1CanonicalRational,
): boolean =>
  left.numerator === right.numerator && left.denominator === right.denominator;

const exactProtocolParameterSnapshotV1 = (
  providerValue: unknown,
  rawOgmiosValue: unknown,
): DeploymentManifestV1CardanoProtocolParameters => {
  const provider = protocolRecord(providerValue, "Lucid protocol parameters");
  const snapshot =
    deriveDeploymentManifestV1CardanoProtocolParametersFromOgmios(
      rawOgmiosValue,
    );
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

export const cardanoProtocolParametersIdentityV1FromProvider = async (
  provider: {
    readonly getProtocolParameters: () => Promise<unknown>;
  },
  rawOgmiosProtocolParameters: unknown,
): Promise<DeploymentManifestV1Value["cardanoProtocolParameters"]> => {
  const snapshot = exactProtocolParameterSnapshotV1(
    await provider.getProtocolParameters(),
    rawOgmiosProtocolParameters,
  );
  return {
    snapshot,
    digest: computeDeploymentManifestV1JsonDigest(snapshot),
  };
};

export const queryLocalOgmiosProtocolParametersV1 = async (
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
): ReturnType<typeof normalizeDeploymentManifestV1JsonValue> =>
  normalizeDeploymentManifestV1JsonValue(
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

export const buildDeploymentManifestV1IdentityContextProgram: Effect.Effect<
  DeploymentManifestV1IdentityContext,
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
        await queryLocalOgmiosProtocolParametersV1(nodeConfig.L1_OGMIOS_KEY);
      return cardanoProtocolParametersIdentityV1FromProvider(
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
      utxoSetDigest: computeDeploymentManifestV1JsonDigest(genesisSnapshot),
    },
    da: {
      committeeVkeys,
      committeeSignersHash:
        computeDeploymentManifestV1DaCommitteeSignersHash(committeeVkeys),
      threshold,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        envelopeEncoding: nodeConfig.MIDGARD_DA_PAYLOAD_ENVELOPE,
        zstdLevel: nodeConfig.MIDGARD_DA_ZSTD_LEVEL,
        limits: DA_TRANSPORT_LIMITS_V1,
        retentionDays: nodeConfig.RETENTION_DAYS,
      },
    },
    proofEvidence: {
      digest: MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
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
  manifest: Omit<DeploymentManifestV1, "manifestId">,
): DeploymentManifestV1 => ({
  ...manifest,
  manifestId: computeDeploymentManifestId(manifest),
});

/**
 * Builds the sole canonical V1 manifest and re-parses it before return so
 * missing contracts, tuple drift, and dispute-schedule drift fail closed.
 */
export const buildDeploymentManifestV1 = (
  deploymentInfo: ContractDeploymentInfo,
  context: DeploymentManifestBuildContext,
): DeploymentManifestV1 => {
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
    schemaVersion: DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
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
      version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
    },
    l1Finality: DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
    economics: context.economics,
    availabilityChallenge: context.availabilityChallenge,
  }) as DeploymentManifestV1;
  return parseDeploymentManifestV1Value(manifest) as DeploymentManifestV1;
};

export const parseDeploymentManifestV1 = (
  value: unknown,
): DeploymentManifestV1 =>
  parseDeploymentManifestV1Value(value) as DeploymentManifestV1;

export const readDeploymentManifestV1File = (
  outputPath: string,
): DeploymentManifestV1 => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const parsed = JSON.parse(readFileSync(resolvedOutputPath, "utf8"));
  return parseDeploymentManifestV1(parsed);
};

export const readFinalizedDeploymentIdentity = (
  outputPath: string,
): FinalizedDeploymentIdentity => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const raw = readFileSync(resolvedOutputPath);
  const parsed = JSON.parse(raw.toString("utf8"));
  const manifest = parseDeploymentManifestV1(parsed);
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
  manifest: DeploymentManifestV1,
  context: {
    readonly network: string;
    readonly referenceScriptDeployAddress: string;
    readonly hubOracleOneShotTxHash: string;
    readonly hubOracleOneShotOutputIndex: number;
    readonly economicsProfile: DeploymentManifestV1EconomicsProfile;
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
  const configuredPath =
    process.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH?.trim();
  return configuredPath === undefined || configuredPath.length === 0
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
    try: () => readDeploymentManifestV1File(path),
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
    return readDeploymentManifestV1File(resolvedOutputPath)
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
  readonly steps?: Partial<DeploymentManifestV1["steps"]>;
  readonly hubOracleOneShotStatus?: DeploymentManifestV1["hubOracleOneShot"]["status"];
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
  DeploymentManifestV1,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const identityContext =
      yield* buildDeploymentManifestV1IdentityContextProgram;
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
        return readDeploymentManifestV1File(outputPath);
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
    const deploymentManifest = buildDeploymentManifestV1(deploymentInfo, {
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
    const marker = makeDeploymentMarkerV1(deploymentManifest.manifestId);
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
              bindDeploymentRunStateToMarkerV1(current, {
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
      try: () => readDeploymentManifestV1File(path),
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

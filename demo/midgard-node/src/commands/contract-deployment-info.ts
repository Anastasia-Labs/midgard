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

import * as SDK from "@al-ft/midgard-sdk";
import {
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
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  parseDeploymentManifestV2Value,
} from "@/deployment-manifest-v2.js";
import { writeJsonFileAtomic } from "@/files/atomic-write.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import { Lucid, MidgardContracts, NodeConfig } from "@/services/index.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
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

export { computeDeploymentManifestId, DEPLOYMENT_MANIFEST_SCHEMA_VERSION };

export type DeploymentManifestStepStatus =
  | "pending"
  | "in_progress"
  | "submitted"
  | "complete"
  | "attached"
  | "failed"
  | "blocked_requires_fresh_redeploy";

export type DeploymentManifestV2 = ContractDeploymentInfo & {
  readonly schemaVersion: typeof DEPLOYMENT_MANIFEST_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly network: string;
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShot: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly outRef: string;
    readonly status: "prepared" | "consumed_by_init" | "unknown";
  };
  readonly referenceScripts: Readonly<
    Record<
      string,
      {
        readonly status: "confirmed" | "missing";
        readonly roleUnit: string;
        readonly scriptHash: string;
        readonly outRef: string | null;
      }
    >
  >;
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
  readonly manifest: DeploymentManifestV2;
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

const REFERENCE_SCRIPT_TARGET_BY_CONTRACT_NAME: Readonly<
  Record<string, ReferenceScriptAuthTokenTarget>
> = {
  referenceScriptAuthMint: "reference-script-auth minting",
  hubOracleMint: "hub-oracle minting",
  daParamsGovernorSpend: "da-params-governor spending",
  daParamsGovernorMint: "da-params-governor minting",
  daAttestationSpend: "da-attestation spending",
  daAttestationMint: "da-attestation minting",
  stateQueueSpend: "state-queue spending",
  stateQueueMint: "state-queue minting",
  schedulerSpend: "scheduler spending",
  schedulerMint: "scheduler minting",
  registeredOperatorsSpend: "registered-operators spending",
  registeredOperatorsMint: "registered-operators minting",
  activeOperatorsSpend: "active-operators spending",
  activeOperatorsMint: "active-operators minting",
  retiredOperatorsSpend: "retired-operators spending",
  retiredOperatorsMint: "retired-operators minting",
  fraudProofCatalogueMint: "fraud-proof-catalogue minting",
  depositSpend: "deposit spending",
  depositMint: "deposit minting",
  withdrawalSpend: "withdrawal spending",
  withdrawalMint: "withdrawal minting",
  settlementMint: "settlement minting",
  payoutSpend: "payout spending",
  payoutMint: "payout minting",
  reserveSpend: "reserve spending",
  reserveWithdraw: "reserve observer",
  phasMembershipWithdraw: "membership proof withdrawal",
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
    cborHex: validator.mintingScriptCBOR,
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
    cborHex: validator.spendingScriptCBOR,
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
    cborHex: validator.withdrawalScriptCBOR,
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
  const sorted = [...utxos].sort(compareOutRefs).reverse();
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
    for (const utxo of sorted) {
      if (utxo.scriptRef == null) {
        continue;
      }
      if (utxo.assets[roleUnit] !== 1n) {
        continue;
      }
      if (validatorToScriptHash(utxo.scriptRef) !== descriptor.scriptHash) {
        continue;
      }
      byDescriptorName.set(descriptor.name, {
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
      });
      break;
    }
  }
  return byDescriptorName;
};

const collectScriptDescriptors = (
  contracts: SDK.MidgardValidators,
): readonly ScriptDescriptor[] => [
  mintDescriptor(
    "referenceScriptAuthMint",
    contracts.referenceScriptAuth,
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
  mintDescriptor("fraudProofMint", contracts.fraudProof),
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
  spendDescriptor("fraudProofDoubleSpend", contracts.fraudProofs.doubleSpend),
  spendDescriptor(
    "fraudProofNonExistentInput",
    contracts.fraudProofs.nonExistentInput,
  ),
  spendDescriptor(
    "fraudProofNonExistentInputNoIndex",
    contracts.fraudProofs.nonExistentInputNoIndex,
  ),
  spendDescriptor("fraudProofInvalidRange", contracts.fraudProofs.invalidRange),
  spendDescriptor(
    "fraudProofTransitionTrace",
    contracts.fraudProofs.transitionTrace,
  ),
  spendDescriptor("fraudProofZeroInput", contracts.fraudProofs.zeroInput),
  spendDescriptor(
    "fraudProofNoReferenceInput",
    contracts.fraudProofs.noReferenceInput,
  ),
  spendDescriptor(
    "fraudProofNoReferenceInputNoIndex",
    contracts.fraudProofs.noReferenceInputNoIndex,
  ),
];

const defaultSteps = (): DeploymentManifestV2["steps"] => ({
  prepareHubOracleNonce: { status: "pending" },
  deployNodeRuntimeReferenceScripts: { status: "pending" },
  initProtocol: { status: "pending" },
  phasRegistration: { status: "pending" },
  operatorRegistration: { status: "pending" },
  operatorActivation: { status: "pending" },
});

const buildReferenceScriptRecords = (
  deploymentInfo: ContractDeploymentInfo,
): DeploymentManifestV2["referenceScripts"] => {
  const entries: [string, DeploymentManifestV2["referenceScripts"][string]][] =
    [];
  for (const [contractName, entry] of Object.entries(
    deploymentInfo.contracts,
  )) {
    const targetName = REFERENCE_SCRIPT_TARGET_BY_CONTRACT_NAME[contractName];
    if (targetName === undefined) {
      continue;
    }
    const refScript = entry.refScriptUTxO;
    entries.push([
      targetName,
      {
        status: refScript === null ? "missing" : "confirmed",
        roleUnit: referenceScriptAuthUnit(
          deploymentInfo.referenceScriptAuthPolicy.policyId,
          targetName,
        ),
        scriptHash: entry.scriptHash,
        outRef:
          refScript === null
            ? null
            : `${refScript.txHash}#${refScript.outputIndex.toString()}`,
      },
    ]);
  }
  return Object.fromEntries(
    entries.sort(([left], [right]) => left.localeCompare(right)),
  );
};

export type DeploymentManifestBuildContext = {
  readonly network: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShotTxHash: string;
  readonly hubOracleOneShotOutputIndex: number;
  readonly hubOracleOneShotStatus?: DeploymentManifestV2["hubOracleOneShot"]["status"];
  readonly now?: Date;
  readonly existingManifest?: DeploymentManifestV2;
  readonly steps?: Partial<DeploymentManifestV2["steps"]>;
};

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
  manifest: Omit<DeploymentManifestV2, "manifestId">,
): DeploymentManifestV2 => ({
  ...manifest,
  manifestId: computeDeploymentManifestId(manifest),
});

export const buildDeploymentManifestV2 = (
  deploymentInfo: ContractDeploymentInfo,
  context: DeploymentManifestBuildContext,
): DeploymentManifestV2 => {
  assertOutRefFields(
    context.hubOracleOneShotTxHash,
    context.hubOracleOneShotOutputIndex,
  );
  const nowIso = (context.now ?? new Date()).toISOString();
  const referenceScripts = buildReferenceScriptRecords(deploymentInfo);
  const allReferenceScriptsConfirmed = Object.values(referenceScripts).every(
    (record) => record.status === "confirmed",
  );
  const baseSteps = {
    ...defaultSteps(),
    prepareHubOracleNonce: { status: "complete" as const },
    deployNodeRuntimeReferenceScripts: {
      status: allReferenceScriptsConfirmed
        ? ("complete" as const)
        : ("pending" as const),
    },
    ...(context.existingManifest?.steps ?? {}),
    ...(context.steps ?? {}),
  };
  return withManifestId({
    schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    network: context.network,
    createdAt: context.existingManifest?.createdAt ?? nowIso,
    updatedAt: nowIso,
    referenceScriptDeployAddress: context.referenceScriptDeployAddress,
    hubOracleOneShot: {
      txHash: context.hubOracleOneShotTxHash.toLowerCase(),
      outputIndex: context.hubOracleOneShotOutputIndex,
      outRef: `${context.hubOracleOneShotTxHash.toLowerCase()}#${context.hubOracleOneShotOutputIndex.toString()}`,
      status:
        context.hubOracleOneShotStatus ??
        context.existingManifest?.hubOracleOneShot.status ??
        ("prepared" as const),
    },
    referenceScriptAuthPolicy: deploymentInfo.referenceScriptAuthPolicy,
    contracts: deploymentInfo.contracts,
    referenceScripts,
    steps: baseSteps,
  });
};

export const parseDeploymentManifestV2 = (
  value: unknown,
): DeploymentManifestV2 =>
  parseDeploymentManifestV2Value(value) as DeploymentManifestV2;

export const readDeploymentManifestV2File = (
  outputPath: string,
): DeploymentManifestV2 => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const parsed = JSON.parse(readFileSync(resolvedOutputPath, "utf8"));
  return parseDeploymentManifestV2(parsed);
};

const requiredFinalizedDeploymentSteps = [
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
] as const satisfies readonly (keyof DeploymentManifestV2["steps"])[];

export const finalizedDeploymentReadinessMismatches = (
  manifest: DeploymentManifestV2,
): readonly string[] => {
  const mismatches: string[] = [];
  const missingReferenceScripts = Object.entries(manifest.referenceScripts)
    .filter(([, record]) => record.status !== "confirmed")
    .map(([name]) => name);
  if (missingReferenceScripts.length > 0) {
    mismatches.push(
      `missing reference scripts: ${missingReferenceScripts.join(",")}`,
    );
  }
  for (const stepName of requiredFinalizedDeploymentSteps) {
    const step = manifest.steps[stepName];
    if (step?.status !== "complete") {
      mismatches.push(`steps.${stepName}.status=${step?.status ?? "missing"}`);
    }
  }
  if (manifest.hubOracleOneShot.status !== "consumed_by_init") {
    mismatches.push(
      `hubOracleOneShot.status=${manifest.hubOracleOneShot.status}`,
    );
  }
  return mismatches;
};

export const readFinalizedDeploymentIdentity = (
  outputPath: string,
): FinalizedDeploymentIdentity => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const raw = readFileSync(resolvedOutputPath);
  const parsed = JSON.parse(raw.toString("utf8"));
  const manifest = parseDeploymentManifestV2(parsed);
  const readinessMismatches = finalizedDeploymentReadinessMismatches(manifest);
  if (readinessMismatches.length > 0) {
    throw new Error(
      `Contract deployment manifest is not finalized for DA runtime generation: ${readinessMismatches.join(
        "; ",
      )}`,
    );
  }
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
  manifest: DeploymentManifestV2,
  context: {
    readonly network: string;
    readonly referenceScriptDeployAddress: string;
    readonly hubOracleOneShotTxHash: string;
    readonly hubOracleOneShotOutputIndex: number;
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
  const missingReferenceScripts = Object.entries(manifest.referenceScripts)
    .filter(([, record]) => record.status !== "confirmed")
    .map(([name]) => name);
  if (missingReferenceScripts.length > 0) {
    mismatches.push(
      `missing reference scripts: ${missingReferenceScripts.join(",")}`,
    );
  }
  return {
    ok: mismatches.length === 0,
    manifestId: manifest.manifestId,
    path: context.path,
    mismatches,
    recommendation:
      mismatches.length === 0
        ? "attach"
        : missingReferenceScripts.length > 0
          ? "fresh_redeploy_required"
          : "correct_attach_config",
  };
};

const configuredContractDeploymentInfoPath = (): string => {
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
    try: () => readDeploymentManifestV2File(path),
    catch: (cause) =>
      new Error(
        `Failed to read v2 deployment manifest at ${path}: ${String(cause)}`,
      ),
  });
  return verifyDeploymentManifestAgainstConfig(manifest, {
    network: nodeConfig.NETWORK,
    referenceScriptDeployAddress: nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
    hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
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
      collectScriptDescriptors(contracts).map((descriptor) => [
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
      ]),
    ),
  });

const resolveLiveContractDeploymentInfoProgram = (
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error, Lucid | MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const descriptors = collectScriptDescriptors(contracts);
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
    const descriptors = collectScriptDescriptors(contracts);
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

const readExistingReferenceScriptAuthPolicy = (
  outputPath: string,
): ReferenceScriptAuthPolicyDeploymentInfo => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  if (!existsSync(resolvedOutputPath)) {
    throw new Error(
      `Contract deployment info at "${resolvedOutputPath}" does not exist; deploy reference scripts first so the reference-script auth policy is recorded.`,
    );
  }
  const parsed = JSON.parse(readFileSync(resolvedOutputPath, "utf8")) as {
    referenceScriptAuthPolicy?: ReferenceScriptAuthPolicyDeploymentInfo;
  };
  if (parsed.referenceScriptAuthPolicy === undefined) {
    throw new Error(
      `Contract deployment info at "${resolvedOutputPath}" is missing referenceScriptAuthPolicy; redeploy reference scripts to create a complete manifest.`,
    );
  }
  return parsed.referenceScriptAuthPolicy;
};

const referenceScriptAuthPolicyInputPaths = (outputPath: string): string[] => {
  const configuredPath =
    process.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH?.trim();
  return Array.from(
    new Set(
      [
        outputPath,
        ...(configuredPath === undefined || configuredPath.length === 0
          ? []
          : [configuredPath]),
        defaultContractDeploymentInfoOutputPath(),
      ].map((candidate) => normalizeOutputPath(candidate)),
    ),
  );
};

const readReferenceScriptAuthPolicyForLiveWrite = (
  outputPath: string,
): ReferenceScriptAuthPolicyDeploymentInfo => {
  const errors: string[] = [];
  for (const candidate of referenceScriptAuthPolicyInputPaths(outputPath)) {
    try {
      return readExistingReferenceScriptAuthPolicy(candidate);
    } catch (cause) {
      errors.push(`${candidate}: ${String(cause)}`);
    }
  }
  throw new Error(
    `Failed to find required referenceScriptAuthPolicy metadata. Tried: ${errors.join(
      "; ",
    )}`,
  );
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
  readonly steps?: Partial<DeploymentManifestV2["steps"]>;
  readonly hubOracleOneShotStatus?: DeploymentManifestV2["hubOracleOneShot"]["status"];
  readonly requireReadyManifest?: boolean;
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
  DeploymentManifestV2,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const referenceScriptAuthPolicy = yield* Effect.try({
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
        return readDeploymentManifestV2File(outputPath);
      } catch {
        return undefined;
      }
    });
    const deploymentManifest = buildDeploymentManifestV2(deploymentInfo, {
      network: nodeConfig.NETWORK,
      referenceScriptDeployAddress:
        nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
      hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
      hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
      existingManifest,
      steps: options.steps,
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
        path: outputPath,
      },
    );
    if (options.requireReadyManifest === true && !verification.ok) {
      return yield* Effect.fail(
        new Error(
          `Refusing to write incomplete deployment manifest: ${formatDeploymentManifestVerificationReport(
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
    return yield* writeContractDeploymentInfoFileProgram(
      outputPath,
      deploymentManifest,
    );
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
      requireReadyManifest: true,
    });
    const manifest = yield* Effect.try({
      try: () => readDeploymentManifestV2File(path),
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

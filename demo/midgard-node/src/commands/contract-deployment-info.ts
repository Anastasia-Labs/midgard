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
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
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
  txOrderFieldPreimageSpend: "V1 transaction-field preimage publication",
  txOrderFieldReceiptSpend: "V1 transaction-field receipt",
  txOrderFieldReceiptMint: "V1 transaction-field receipt minting",
  cekProgramMaterialSpend: "V1 immutable CEK program-material publication",
  validationTraceDispute: "V1 validation-trace dispute",
  validationTraceDisputeSource: "V1 validation-trace source",
  validationTraceDisputeGame: "V1 validation-trace game",
  validationTraceDisputeBoundary: "V1 validation-trace boundary",
  validationTraceDisputeTimeout: "V1 validation-trace timeout",
  validationTraceDisputeAward: "V1 validation-trace award",
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
  spendDescriptor(
    "txOrderFieldPreimageSpend",
    contracts.txOrderFieldPreimage,
    "V1 transaction-field preimage publication",
  ),
  spendDescriptor(
    "txOrderFieldReceiptSpend",
    contracts.txOrderFieldReceipt,
    "V1 transaction-field receipt",
  ),
  mintDescriptor(
    "txOrderFieldReceiptMint",
    contracts.txOrderFieldReceipt,
    "V1 transaction-field receipt minting",
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
  "cardanoProtocolParameters" | "genesis" | "da" | "proofEvidence"
>;

export const cardanoProtocolParametersIdentityV1FromProvider =
  async (provider: {
    readonly getProtocolParameters: () => Promise<unknown>;
  }): Promise<DeploymentManifestV1Value["cardanoProtocolParameters"]> => {
    const snapshot = normalizeDeploymentManifestV1JsonValue(
      await provider.getProtocolParameters(),
      "cardanoProtocolParameters.snapshot",
    );
    return {
      snapshot,
      digest: computeDeploymentManifestV1JsonDigest(snapshot),
    };
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
      return cardanoProtocolParametersIdentityV1FromProvider(provider);
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
  return {
    ok: mismatches.length === 0,
    manifestId: manifest.manifestId,
    path: context.path,
    mismatches,
    recommendation:
      mismatches.length === 0 ? "attach" : "correct_attach_config",
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

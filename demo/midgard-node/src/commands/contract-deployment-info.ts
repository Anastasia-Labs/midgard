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
import { existsSync, readFileSync } from "node:fs";
import { mkdir, rename, writeFile } from "node:fs/promises";
import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ReferenceScriptAuthPolicyRef,
  type ReferenceScriptAuthPolicyDeploymentInfo,
  type ReferenceScriptAuthTokenTarget,
  referenceScriptAuthUnit,
} from "@/deployment/reference-script-auth.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import { Lucid, MidgardContracts } from "@/services/index.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";
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

const mergeWalletUtxosPreservingScriptRefs = (
  liveUtxos: readonly UTxO[],
  cachedUtxos: readonly UTxO[],
): readonly UTxO[] => {
  const cachedByOutRef = new Map(
    cachedUtxos.map((utxo) => [
      `${utxo.txHash}#${utxo.outputIndex.toString()}`,
      utxo,
    ]),
  );
  return liveUtxos.map((utxo) => {
    if (utxo.scriptRef !== undefined) {
      return utxo;
    }
    const cached = cachedByOutRef.get(
      `${utxo.txHash}#${utxo.outputIndex.toString()}`,
    );
    if (cached?.scriptRef === undefined) {
      return utxo;
    }
    return {
      ...utxo,
      scriptRef: cached.scriptRef,
    };
  });
};

const fetchReferenceScriptWalletUtxos = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const referenceScriptsLucid = lucidService.referenceScriptsApi;
  const referenceScriptsAddress = lucidService.referenceScriptsAddress;
  const cachedWalletUtxos = yield* Effect.tryPromise(() =>
    referenceScriptsLucid.wallet().getUtxos(),
  ).pipe(Effect.catchAll(() => Effect.succeed([] as readonly UTxO[])));
  const liveWalletUtxos = yield* Effect.tryPromise({
    try: () => referenceScriptsLucid.utxosAt(referenceScriptsAddress),
    catch: (cause) =>
      new Error(
        `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}: ${String(cause)}`,
      ),
  });
  return mergeWalletUtxosPreservingScriptRefs(
    liveWalletUtxos,
    cachedWalletUtxos,
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
];

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
    const referenceScriptWalletUtxos = yield* fetchReferenceScriptWalletUtxos;
    const descriptors = collectScriptDescriptors(contracts);
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
      await mkdir(dirname(resolvedOutputPath), { recursive: true });
      const tempPath = `${resolvedOutputPath}.tmp-${process.pid.toString()}-${Date.now().toString()}`;
      await writeFile(
        tempPath,
        `${JSON.stringify(deploymentInfo, null, 2)}\n`,
        "utf8",
      );
      await rename(tempPath, resolvedOutputPath);
      return resolvedOutputPath;
    },
    catch: (cause) =>
      new Error(
        `Failed to write contract deployment info file: ${String(cause)}`,
      ),
  });

export const writeLiveContractDeploymentInfoProgram = (
  outputPath: string,
): Effect.Effect<string, Error, Lucid | MidgardContracts> =>
  Effect.gen(function* () {
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
    return yield* writeContractDeploymentInfoFileProgram(
      outputPath,
      deploymentInfo,
    );
  });

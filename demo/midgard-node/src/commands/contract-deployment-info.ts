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
import { Effect } from "effect";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { dirname, resolve as resolvePath } from "node:path";
import { existsSync } from "node:fs";
import { mkdir, rename, writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { Lucid, MidgardContracts } from "@/services/index.js";
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
};

export type ContractDeploymentInfo = Readonly<
  Record<string, ContractDeploymentInfoEntry>
>;

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
};

const mintDescriptor = (
  name: string,
  validator: SDK.MintingValidator,
): ScriptDescriptor => ({
  name,
  script: validator.mintingScript,
  scriptHash: validator.policyId,
  contract: {
    type: validator.mintingScript.type,
    cborHex: validator.mintingScriptCBOR,
  },
});

const spendDescriptor = (
  name: string,
  validator: SDK.SpendingValidator,
): ScriptDescriptor => ({
  name,
  script: validator.spendingScript,
  scriptHash: validator.spendingScriptHash,
  contract: {
    type: validator.spendingScript.type,
    cborHex: validator.spendingScriptCBOR,
  },
});

const withdrawalDescriptor = (
  name: string,
  validator: SDK.WithdrawalValidator,
): ScriptDescriptor => ({
  name,
  script: validator.withdrawalScript,
  scriptHash: validator.withdrawalScriptHash,
  contract: {
    type: validator.withdrawalScript.type,
    cborHex: validator.withdrawalScriptCBOR,
  },
});

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
  const walletAddress = yield* Effect.tryPromise({
    try: () => referenceScriptsLucid.wallet().address(),
    catch: (cause) =>
      new Error(
        `Failed to resolve reference-script wallet address: ${String(cause)}`,
      ),
  });
  const cachedWalletUtxos = yield* Effect.tryPromise({
    try: () => referenceScriptsLucid.wallet().getUtxos(),
    catch: () => [] as readonly UTxO[],
  }).pipe(Effect.catchAll(() => Effect.succeed([] as readonly UTxO[])));
  const liveWalletUtxos = yield* Effect.tryPromise({
    try: () => referenceScriptsLucid.utxosAt(walletAddress),
    catch: (cause) =>
      new Error(
        `Failed to fetch reference-script wallet UTxOs at ${walletAddress}: ${String(cause)}`,
      ),
  });
  return mergeWalletUtxosPreservingScriptRefs(
    liveWalletUtxos,
    cachedWalletUtxos,
  );
});

const buildReferenceScriptOutRefMap = (
  utxos: readonly UTxO[],
): ReadonlyMap<string, ContractDeploymentInfoRefScriptUTxO> => {
  const sorted = [...utxos].sort(compareOutRefs).reverse();
  const byScriptHash = new Map<string, ContractDeploymentInfoRefScriptUTxO>();
  for (const utxo of sorted) {
    if (utxo.scriptRef === undefined) {
      continue;
    }
    try {
      const scriptHash = validatorToScriptHash(utxo.scriptRef);
      if (!byScriptHash.has(scriptHash)) {
        byScriptHash.set(scriptHash, {
          txHash: utxo.txHash,
          outputIndex: utxo.outputIndex,
        });
      }
    } catch {
      continue;
    }
  }
  return byScriptHash;
};

const collectScriptDescriptors = (
  contracts: SDK.MidgardValidators,
): readonly ScriptDescriptor[] => [
  mintDescriptor("hubOracleMint", contracts.hubOracle),
  spendDescriptor("stateQueueSpend", contracts.stateQueue),
  mintDescriptor("stateQueueMint", contracts.stateQueue),
  spendDescriptor("schedulerSpend", contracts.scheduler),
  mintDescriptor("schedulerMint", contracts.scheduler),
  spendDescriptor("registeredOperatorsSpend", contracts.registeredOperators),
  mintDescriptor("registeredOperatorsMint", contracts.registeredOperators),
  spendDescriptor("activeOperatorsSpend", contracts.activeOperators),
  mintDescriptor("activeOperatorsMint", contracts.activeOperators),
  spendDescriptor("retiredOperatorsSpend", contracts.retiredOperators),
  mintDescriptor("retiredOperatorsMint", contracts.retiredOperators),
  spendDescriptor("escapeHatchSpend", contracts.escapeHatch),
  mintDescriptor("escapeHatchMint", contracts.escapeHatch),
  spendDescriptor("fraudProofCatalogueSpend", contracts.fraudProofCatalogue),
  mintDescriptor("fraudProofCatalogueMint", contracts.fraudProofCatalogue),
  spendDescriptor("fraudProofSpend", contracts.fraudProof),
  mintDescriptor("fraudProofMint", contracts.fraudProof),
  spendDescriptor("depositSpend", contracts.deposit),
  mintDescriptor("depositMint", contracts.deposit),
  spendDescriptor("withdrawalSpend", contracts.withdrawal),
  mintDescriptor("withdrawalMint", contracts.withdrawal),
  spendDescriptor("txOrderSpend", contracts.txOrder),
  mintDescriptor("txOrderMint", contracts.txOrder),
  spendDescriptor("settlementSpend", contracts.settlement),
  mintDescriptor("settlementMint", contracts.settlement),
  spendDescriptor("payoutSpend", contracts.payout),
  mintDescriptor("payoutMint", contracts.payout),
  spendDescriptor("reserveSpend", contracts.reserve),
  withdrawalDescriptor("reserveWithdraw", contracts.reserve),
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
  referenceScriptOutRefs: ReadonlyMap<
    string,
    ContractDeploymentInfoRefScriptUTxO
  > = new Map(),
): ContractDeploymentInfo =>
  Object.freeze(
    Object.fromEntries(
      collectScriptDescriptors(contracts).map((descriptor) => [
        descriptor.name,
        {
          refScriptUTxO:
            referenceScriptOutRefs.get(descriptor.scriptHash) ?? null,
          contract: descriptor.contract,
          scriptHash: descriptor.scriptHash,
        } satisfies ContractDeploymentInfoEntry,
      ]),
    ),
  );

export const resolveLiveContractDeploymentInfoProgram: Effect.Effect<
  ContractDeploymentInfo,
  Error,
  Lucid | MidgardContracts
> = Effect.gen(function* () {
  const contracts = yield* MidgardContracts;
  const referenceScriptWalletUtxos = yield* fetchReferenceScriptWalletUtxos;
  const referenceScriptOutRefs =
    buildReferenceScriptOutRefMap(referenceScriptWalletUtxos);
  return buildContractDeploymentInfoFromContracts(
    contracts,
    referenceScriptOutRefs,
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
    throw new Error(
      "Contract deployment info output path must not be empty.",
    );
  }
  return resolvePath(normalized);
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
  resolveLiveContractDeploymentInfoProgram.pipe(
    Effect.flatMap((deploymentInfo) =>
      writeContractDeploymentInfoFileProgram(outputPath, deploymentInfo),
    ),
  );

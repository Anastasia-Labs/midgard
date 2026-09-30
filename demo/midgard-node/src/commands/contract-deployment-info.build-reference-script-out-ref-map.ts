import { existsSync } from "node:fs";
import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import {
  type DeploymentManifest,
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestContractEntry,
  type DeploymentManifestEconomics,
  parseDeploymentManifestAvailabilityChallenge,
} from "@al-ft/midgard-core/deployment-manifest-identity";
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
  type DeployableScript,
  manifestDeployableScripts,
  referenceScriptRoleForContract,
} from "../deployable-scripts.js";
import { daAvailabilityChallengeEnvironmentInput } from "../environment.js";
import { Lucid } from "../services/index.js";
import { fetchReferenceScriptUtxosAt } from "../transactions/reference-scripts.js";

export type ContractDeploymentInfoRefScriptUTxO = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type ContractDeploymentInfoEntry = DeploymentManifestContractEntry;

export type ContractDeploymentInfo = {
  readonly referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo;
  readonly contracts: Readonly<Record<string, ContractDeploymentInfoEntry>>;
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

export const DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME =
  "contract-deployment-info.json";

export const DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME = "deploymentInfo";

export const resolvePackageRootFromModuleUrl = (moduleUrl: string): string => {
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

export const fetchLiveReferenceScriptUtxos = (): Effect.Effect<
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

const scriptDescriptor = ({
  contract,
  script,
  scriptHash,
  role,
}: Pick<
  DeployableScript,
  "contract" | "script" | "scriptHash" | "role"
>): ScriptDescriptor => ({
  name: contract,
  script,
  scriptHash,
  contract: {
    type: script.type,
    cborHex: script.script,
  },
  ...(role === undefined ? {} : { referenceScriptTargetName: role }),
});

/**
 * Manifest descriptors for every deployable script, in manifest order. When
 * the reference-script auth policy is known, its native script replaces the
 * bundle's placeholder as `referenceScriptAuthMint`.
 */
export const collectScriptDescriptors = (
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicy?: ReferenceScriptAuthPolicyDeploymentInfo,
): readonly ScriptDescriptor[] =>
  manifestDeployableScripts(contracts).map((deployable) =>
    deployable.contract === "referenceScriptAuthMint" &&
    referenceScriptAuthPolicy !== undefined
      ? scriptDescriptor({
          ...deployable,
          script: {
            type: "Native",
            script: referenceScriptAuthPolicy.nativeScript.cborHex,
          },
          scriptHash: referenceScriptAuthPolicy.policyId,
        })
      : scriptDescriptor(deployable),
  );

export const defaultSteps = (): DeploymentManifest["steps"] => ({
  prepareHubOracleNonce: { status: "pending" },
  deployNodeRuntimeReferenceScripts: { status: "pending" },
  initProtocol: { status: "pending" },
  phasRegistration: { status: "pending" },
  availabilityRegistration: { status: "pending" },
  operatorRegistration: { status: "pending" },
  operatorActivation: { status: "pending" },
});

export const buildReferenceScriptRecords = (
  deploymentInfo: ContractDeploymentInfo,
): DeploymentManifest["referenceScripts"] => {
  const entries: [string, DeploymentManifest["referenceScripts"][string]][] =
    [];
  for (const [contractName, entry] of Object.entries(
    deploymentInfo.contracts,
  )) {
    const targetName = referenceScriptRoleForContract(contractName);
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
  readonly network: DeploymentManifest["network"];
  readonly cardanoProtocolParameters: DeploymentManifest["cardanoProtocolParameters"];
  readonly genesis: DeploymentManifest["genesis"];
  readonly da: DeploymentManifest["da"];
  readonly artifacts: DeploymentManifest["artifacts"];
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
  | "artifacts"
  | "economics"
  | "availabilityChallenge"
>;

export const configuredAvailabilityChallenge =
  (): DeploymentManifestAvailabilityChallenge =>
    parseDeploymentManifestAvailabilityChallenge(
      daAvailabilityChallengeEnvironmentInput(
        (name) => `${name} must be an explicit positive decimal integer`,
      ),
    );

export const protocolRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Record<string, unknown>;
};

export const protocolNatural = (value: unknown, field: string): string => {
  if (typeof value === "bigint" && value >= 0n) return value.toString(10);
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString(10);
  }
  if (typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value)) {
    return value;
  }
  throw new Error(`${field} must be a canonical natural`);
};

export const protocolGcd = (left: bigint, right: bigint): bigint => {
  let a = left < 0n ? -left : left;
  let b = right < 0n ? -right : right;
  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }
  return a;
};

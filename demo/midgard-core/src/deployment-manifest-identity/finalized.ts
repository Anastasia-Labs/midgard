import {
  getAddressDetails,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import { decodeMidgardNativeScript } from ".././codec/native-script.js";
import { MIDGARD_CONSENSUS_PROFILE } from ".././consensus-profile.js";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from ".././da-transport.js";
import {
  MIDGARD_RETENTION_WINDOW,
  retentionDaysCoverWindow,
} from ".././retention-window.js";
import { verifyDeploymentManifestFraudProofCatalogueIdentity } from "./catalogue-proof.js";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY,
  type DeploymentManifestFraudProofCatalogueCategory,
  type DeploymentManifestFraudProofCatalogueCategoryIdentity,
} from "./catalogue-roles.js";
import {
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRecipe,
  parseDeploymentManifestEventHistoryRetentionAddress,
  parseDeploymentManifestEventHistoryRetentionAddresses,
} from "./event-history.js";
import {
  computeDeploymentManifestJsonDigest,
  normalizeDeploymentManifestJsonValueInternal,
  stableJson,
  verifyDeploymentManifestIdentity,
} from "./identity.js";
import {
  requireExactKeys,
  requireFinalOutRef,
  requireHex,
  requireInteger,
  requireIsoTimestamp,
  requireRecord,
  requireString,
} from "./primitives.js";
import { parseDeploymentManifestCardanoProtocolParameters } from "./protocol-parameters.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./reference-script-contracts.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES } from "./reference-script-tokens.js";
import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_STEP_NAMES,
  type DeploymentManifest,
} from "./types.js";

// Script-hash derivation is a pure function of (type, cborHex), but each
// derivation pays a full CBOR decode of the compiled script. A manifest
// carries every deployment contract, and callers re-verify the manifest on
// every authority check, so uncached derivation is quadratic in practice.
// A cache hit returns exactly what re-derivation would, including for
// tampered manifests: a changed script changes the key.
const SCRIPT_HASH_DERIVATION_CACHE_LIMIT = 4096;
const scriptHashDerivationCache = new Map<string, string>();
const deriveScriptHashCached = (
  type: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3",
  cborHex: string,
): string => {
  const key = `${type}:${cborHex}`;
  const cached = scriptHashDerivationCache.get(key);
  if (cached !== undefined) {
    return cached;
  }
  const derived = validatorToScriptHash({ type, script: cborHex });
  if (scriptHashDerivationCache.size >= SCRIPT_HASH_DERIVATION_CACHE_LIMIT) {
    const oldest = scriptHashDerivationCache.keys().next().value;
    if (oldest !== undefined) {
      scriptHashDerivationCache.delete(oldest);
    }
  }
  scriptHashDerivationCache.set(key, derived);
  return derived;
};

const validateFinalizedContracts = (
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    [],
    "contracts",
  );
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const scriptHashByName = new Map<string, string>();
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireRecord(contracts[contractName], field);
    const historyFamily =
      contractName === "fraudProofFabricatedDeposit" ||
      contractName === "fraudProofFabricatedWithdrawal";
    const transitionHistory = contractName === "fraudProofTransitionTrace";
    const historyList =
      contractName === "depositMint" || contractName === "withdrawalMint";
    requireExactKeys(
      entry,
      [
        "refScriptUTxO",
        "contract",
        "scriptHash",
        ...(historyList ? ["eventHistoryRecipe"] : []),
        ...(historyFamily
          ? ["eventHistoryBounds", "eventHistoryRetentionAddress"]
          : transitionHistory
            ? ["eventHistoryBounds", "eventHistoryRetentionAddresses"]
            : []),
      ],
      contractName === "fraudProofCatalogueMint" ? ["fraudProofCatalogue"] : [],
      field,
    );
    if (historyList)
      parseDeploymentManifestEventHistoryRecipe(
        entry.eventHistoryRecipe,
        `${field}.eventHistoryRecipe`,
      );
    if (historyFamily || transitionHistory) {
      parseDeploymentManifestEventHistoryBounds(
        entry.eventHistoryBounds,
        `${field}.eventHistoryBounds`,
      );
      if (transitionHistory)
        parseDeploymentManifestEventHistoryRetentionAddresses(
          entry.eventHistoryRetentionAddresses,
        );
      else
        parseDeploymentManifestEventHistoryRetentionAddress(
          entry.eventHistoryRetentionAddress,
        );
    }
    if (referenceScriptContractNames.has(contractName)) {
      requireFinalOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    } else if (entry.refScriptUTxO !== null) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be null because the contract has no reference-script role`,
      );
    }
    const contract = requireRecord(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    if (
      contract.type !== "Native" &&
      contract.type !== "PlutusV1" &&
      contract.type !== "PlutusV2" &&
      contract.type !== "PlutusV3"
    ) {
      throw new Error(
        `Deployment manifest ${field}.contract.type is unsupported`,
      );
    }
    const cborHex = requireHex(
      contract.cborHex,
      undefined,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireHex(entry.scriptHash, 28, `${field}.scriptHash`);
    let derivedScriptHash: string;
    try {
      derivedScriptHash = deriveScriptHashCached(contract.type, cborHex);
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.contract.cborHex is invalid: ${String(cause)}`,
      );
    }
    if (derivedScriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash mismatch: expected ${derivedScriptHash}`,
      );
    }
    scriptHashByName.set(contractName, scriptHash);
  }

  const catalogueMint = requireRecord(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  const catalogue = requireRecord(
    catalogueMint.fraudProofCatalogue,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireHex(
    catalogue.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireRecord(
    catalogue.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  requireExactKeys(
    categories,
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;
  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const contractName =
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[categoryName];
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireRecord(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    const categoryId = requireHex(
      category.categoryId,
      4,
      `${field}.categoryId`,
    );
    const scriptHash = requireHex(
      category.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    const membershipProofCbor = requireHex(
      category.membershipProofCbor,
      undefined,
      `${field}.membershipProofCbor`,
    );
    if (scriptHash !== scriptHashByName.get(contractName)) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    parsedCategories[categoryName] = {
      categoryId,
      scriptHash,
      membershipProofCbor,
    };
  }
  verifyDeploymentManifestFraudProofCatalogueIdentity({
    root: requireHex(
      catalogue.root,
      32,
      "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    ),
    categories: parsedCategories,
  });
};

const validateFinalizedReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  const roles = Object.keys(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  );
  requireExactKeys(referenceScripts, roles, [], "referenceScripts");
  const policyId = requireHex(
    referenceScriptAuthPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of roles) {
    const field = `referenceScripts.${role}`;
    const reference = requireRecord(referenceScripts[role], field);
    requireExactKeys(
      reference,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (reference.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const tokenName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    const expectedRoleUnit =
      policyId + bytesToHex(new TextEncoder().encode(tokenName));
    if (reference.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireRecord(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    const scriptHash = requireHex(
      reference.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    if (scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireFinalOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const expectedOutRef = `${contractOutRef.txHash}#${contractOutRef.outputIndex.toString()}`;
    if (reference.outRef !== expectedOutRef) {
      throw new Error(
        `Deployment manifest ${field}.outRef must equal ${expectedOutRef}`,
      );
    }
  }
};

const validateFinalizedDa = (value: unknown): void => {
  const da = requireRecord(value, "Deployment manifest da");
  requireExactKeys(
    da,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (!Array.isArray(da.committeeVkeys) || da.committeeVkeys.length === 0) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = da.committeeVkeys.map((entry, index) =>
    requireHex(entry, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error("Deployment manifest da.committeeVkeys must be unique");
  }
  const committeeSignersHash = requireHex(
    da.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash = bytesToHex(
    blake2b(hexToBytes(committeeVkeys.join("")), { dkLen: 32 }),
  );
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requireInteger(da.threshold, "da.threshold", 1);
  if (threshold > committeeVkeys.length) {
    throw new Error("Deployment manifest da.threshold exceeds committee size");
  }
  const transport = requireRecord(
    da.transportProfile,
    "Deployment manifest da.transportProfile",
  );
  requireExactKeys(
    transport,
    [
      "protocolVersion",
      "runtimeManifestSchemaVersion",
      "envelopeEncoding",
      "zstdLevel",
      "limits",
      "retentionDays",
    ],
    [],
    "da.transportProfile",
  );
  if (transport.protocolVersion !== DA_TRANSPORT_PROTOCOL_VERSION) {
    throw new Error(
      "Deployment manifest da.transportProfile.protocolVersion is unsupported",
    );
  }
  if (
    transport.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_SCHEMA_VERSION
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.runtimeManifestSchemaVersion is unsupported",
    );
  }
  if (
    transport.envelopeEncoding !== "identity" &&
    transport.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding is unsupported",
    );
  }
  requireInteger(transport.zstdLevel, "da.transportProfile.zstdLevel", 1);
  if (
    stableJson(
      normalizeDeploymentManifestJsonValueInternal(
        transport.limits,
        "Deployment manifest da.transportProfile.limits",
        false,
      ),
    ) !== stableJson(DA_TRANSPORT_LIMITS)
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
    );
  }
  const retentionDays = requireInteger(
    transport.retentionDays,
    "da.transportProfile.retentionDays",
    1,
  );
  // Existing >= 15-day transport-profile floor: never weakened.
  if (retentionDays < DA_TRANSPORT_LIMITS.minimumRetentionDays) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays is too short",
    );
  }
  // Q54: additionally bind the window to the derived challengeability horizon
  // (block maturity + worst-case proof-time bound), so deployment identity -
  // not a literal - is what the DA and proof stores enforce against.
  if (
    !retentionDaysCoverWindow(
      retentionDays,
      "Deployment manifest da.transportProfile.retentionDays",
    )
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must cover the canonical V1 retention window (requiredRetentionMs=${String(
        MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
      )})`,
    );
  }
};

// manifestIds whose deep finalized verification already succeeded in this
// process. Reusing one is sound only because verifyDeploymentManifestIdentity
// runs uncached on every call: it re-hashes the manifest's full normalized
// content and requires manifestId to equal that hash, so a mutated manifest
// either fails identity verification outright or arrives under a new
// manifestId and misses this cache. Everything the deep pass checks is a pure
// function of that same content plus module constants.
const VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT = 64;
const verifiedFinalizedManifestIds = new Set<string>();

export type ReferenceScriptPublicationAuthority =
  | {
      readonly kind: "publisher-signature";
      readonly expiresAtSlot: number;
      readonly publisherKeyHash: string;
    }
  | {
      readonly kind: "time-only";
      readonly expiresAtSlot: number;
    };

/**
 * Immediate publication audits trust the named publisher while its minting
 * window remains open. Historical time-only policies have no signer and still
 * require expiry before their role tokens can be treated as unique. The policy
 * identifies the publisher independently of the reference-output recipient;
 * callers publishing with a wallet can additionally check that wallet here.
 */
export const verifyReferenceScriptPublicationAuthority = (input: {
  readonly cborHex: string;
  readonly expiresAtSlot: number;
  readonly publisherAddress?: string;
  readonly postTimelockAuditRequired: boolean;
}): ReferenceScriptPublicationAuthority => {
  const cborHex = requireHex(
    input.cborHex,
    undefined,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  const expiresAtSlot = requireInteger(
    input.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  const { script } = decodeMidgardNativeScript(hexToBytes(cborHex));
  const signature = script.type === "all" ? script.scripts[0] : undefined;
  const signed =
    script.type === "all" &&
    script.scripts.length === 2 &&
    signature?.type === "sig" &&
    script.scripts[1]?.type === "before";
  const deadline = signed ? script.scripts[1] : script;
  if (deadline.type !== "before") {
    throw new Error(
      "Deployment manifest reference-script authority must be an exact publisher signature AND expiry policy, or a historical time-only policy",
    );
  }
  if (deadline.slot !== BigInt(expiresAtSlot)) {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.expiresAtSlot must match the native policy CBOR",
    );
  }
  if (!signed) {
    if (input.postTimelockAuditRequired !== true) {
      throw new Error(
        "Deployment manifest time-only reference-script authority requires postTimelockAudit.required to be true",
      );
    }
    return { kind: "time-only", expiresAtSlot };
  }
  const publisherKeyHash = signature.keyHash.toString("hex");
  if (input.publisherAddress !== undefined) {
    const paymentCredential = getAddressDetails(
      input.publisherAddress,
    ).paymentCredential;
    if (
      paymentCredential?.type !== "Key" ||
      paymentCredential.hash !== publisherKeyHash
    ) {
      throw new Error(
        "Deployment manifest reference-script authority signer must match the publisherAddress payment key",
      );
    }
  }
  if (input.postTimelockAuditRequired !== false) {
    throw new Error(
      "Deployment manifest publisher-signed reference-script authority requires postTimelockAudit.required to be false; audit immediately after publication",
    );
  }
  return { kind: "publisher-signature", expiresAtSlot, publisherKeyHash };
};

export const verifyFinalizedDeploymentManifest = (
  value: unknown,
): DeploymentManifest => {
  const candidate = verifyDeploymentManifestIdentity(value);
  const verifiedManifestId = candidate.manifestId as string;
  if (verifiedFinalizedManifestIds.has(verifiedManifestId)) {
    return candidate as DeploymentManifest;
  }
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );

  const cardano = requireRecord(
    candidate.cardanoProtocolParameters,
    "Deployment manifest cardanoProtocolParameters",
  );
  requireExactKeys(
    cardano,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoDigest = requireHex(
    cardano.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  parseDeploymentManifestCardanoProtocolParameters(cardano.snapshot);
  const expectedCardanoDigest = computeDeploymentManifestJsonDigest(
    cardano.snapshot,
  );
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }

  const genesis = requireRecord(
    candidate.genesis,
    "Deployment manifest genesis",
  );
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireHex(genesis.headerHash, 28, "genesis.headerHash");
  requireHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const oneShot = requireRecord(
    candidate.hubOracleOneShot,
    "Deployment manifest hubOracleOneShot",
  );
  requireExactKeys(
    oneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const oneShotTxHash = requireHex(
    oneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const oneShotOutputIndex = requireInteger(
    oneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOneShotOutRef = `${oneShotTxHash}#${oneShotOutputIndex.toString()}`;
  if (oneShot.outRef !== expectedOneShotOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef must equal ${expectedOneShotOutRef}`,
    );
  }
  if (oneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }

  const authPolicy = requireRecord(
    candidate.referenceScriptAuthPolicy,
    "Deployment manifest referenceScriptAuthPolicy",
  );
  requireExactKeys(
    authPolicy,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireHex(
    authPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireRecord(
    authPolicy.nativeScript,
    "Deployment manifest referenceScriptAuthPolicy.nativeScript",
  );
  requireExactKeys(
    nativeScript,
    [
      "type",
      "cborHex",
      "expiresAtSlot",
      "expiresAtUnixTime",
      "timelockDurationMs",
    ],
    [],
    "referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const nativeScriptCbor = requireHex(
    nativeScript.cborHex,
    undefined,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  const expiresAtSlot = requireInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requireInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
    1,
  );
  const derivedPolicyId = deriveScriptHashCached("Native", nativeScriptCbor);
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    authPolicy.tokenNames,
    "Deployment manifest referenceScriptAuthPolicy.tokenNames",
  );
  const roles = Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES);
  requireExactKeys(
    tokenNames,
    roles,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const role of roles) {
    const expected =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    if (tokenNames[role] !== expected) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expected}`,
      );
    }
  }
  const audit = requireRecord(
    authPolicy.postTimelockAudit,
    "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    audit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (typeof audit.required !== "boolean") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be a boolean",
    );
  }
  verifyReferenceScriptPublicationAuthority({
    cborHex: nativeScriptCbor,
    expiresAtSlot,
    postTimelockAuditRequired: audit.required,
  });
  requireString(audit.rule, "referenceScriptAuthPolicy.postTimelockAudit.rule");

  const contracts = requireRecord(
    candidate.contracts,
    "Deployment manifest contracts",
  );
  validateFinalizedContracts(contracts);
  for (const [name, kind] of [
    ["deposit", "Deposit"],
    ["withdrawal", "Withdrawal"],
  ] as const) {
    const mint = requireRecord(
      contracts[`${name}Mint`],
      `contracts.${name}Mint`,
    );
    const spend = requireRecord(
      contracts[`${name}Spend`],
      `contracts.${name}Spend`,
    );
    const recipe = parseDeploymentManifestEventHistoryRecipe(
      mint.eventHistoryRecipe,
    );
    const hub = requireRecord(
      contracts.hubOracleMint,
      "contracts.hubOracleMint",
    );
    if (
      recipe.kind !== kind ||
      recipe.hubPolicyId !== hub.scriptHash ||
      recipe.initializationNonce.txHash !== oneShotTxHash ||
      recipe.initializationNonce.outputIndex !== oneShotOutputIndex ||
      mint.scriptHash !== spend.scriptHash
    )
      throw new Error(
        `Deployment manifest ${name} history recipe or list roles differ from its deployment`,
      );
  }
  const authContract = requireRecord(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (authContract.scriptHash !== policyId) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateFinalizedReferenceScripts(
    requireRecord(
      candidate.referenceScripts,
      "Deployment manifest referenceScripts",
    ),
    authPolicy,
    contracts,
  );
  validateFinalizedDa(candidate.da);

  const artifacts = requireRecord(
    candidate.artifacts,
    "Deployment manifest artifacts",
  );
  requireExactKeys(artifacts, ["blueprintHash"], [], "artifacts");
  requireHex(artifacts.blueprintHash, 32, "artifacts.blueprintHash");

  const steps = requireRecord(candidate.steps, "Deployment manifest steps");
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_STEP_NAMES, [], "steps");
  const supportedStepStatuses = new Set([
    "pending",
    "in_progress",
    "submitted",
    "complete",
    "attached",
    "failed",
    "blocked_requires_fresh_redeploy",
  ]);
  for (const stepName of DEPLOYMENT_MANIFEST_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireRecord(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (!supportedStepStatuses.has(String(step.status))) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireHex(step.txHash, 32, `${field}.txHash`);
    }
  }
  for (const requiredStep of [
    "prepareHubOracleNonce",
    "deployNodeRuntimeReferenceScripts",
    "initProtocol",
    "availabilityRegistration",
  ]) {
    const step = requireRecord(steps[requiredStep], `steps.${requiredStep}`);
    if (step.status !== "complete") {
      throw new Error(
        `Deployment manifest steps.${requiredStep}.status must be complete`,
      );
    }
  }

  const dispute = requireRecord(
    candidate.validationDispute,
    "Deployment manifest validationDispute",
  );
  requireExactKeys(
    dispute,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  const expectedDispute = {
    version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
    responseWindowMs:
      MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
    maxBisectionRounds:
      MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
    maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedDispute)) {
    if (dispute[key] !== expected) {
      throw new Error(
        `Deployment manifest validationDispute.${key} must equal ${expected.toString()}`,
      );
    }
  }

  const l1Finality = requireRecord(
    candidate.l1Finality,
    "Deployment manifest l1Finality",
  );
  requireExactKeys(
    l1Finality,
    ["confirmationDepth", "automaticRecoveryMaxDepth", "deepRollbackPolicy"],
    [],
    "l1Finality",
  );
  for (const [key, expected] of Object.entries(
    DEPLOYMENT_MANIFEST_L1_FINALITY,
  )) {
    if (l1Finality[key] !== expected) {
      throw new Error(
        `Deployment manifest l1Finality.${key} must equal ${String(expected)}`,
      );
    }
  }
  if (
    verifiedFinalizedManifestIds.size >=
    VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT
  ) {
    const oldest = verifiedFinalizedManifestIds.values().next().value;
    if (oldest !== undefined) {
      verifiedFinalizedManifestIds.delete(oldest);
    }
  }
  verifiedFinalizedManifestIds.add(verifiedManifestId);
  return candidate as DeploymentManifest;
};

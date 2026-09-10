import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { inspect } from "node:util";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
} from "../../src/commands/contract-deployment-info.js";
import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "../../src/transactions/availability-challenge-registration.js";
import {
  atomicProtocolInitReferenceScriptsFromPublications,
  buildAtomicProtocolInitTxProgram,
  buildFraudProofCatalogueDeploymentInfo,
  deriveOperatorDaParams,
  fraudProofsToIndexedValidators,
} from "../../src/transactions/initialization.js";
import { ensurePhasMembershipRewardAccountRegisteredProgram } from "../../src/transactions/phas-membership-registration.js";
import { nodeRuntimeReferenceScriptTargets } from "../../src/transactions/reference-scripts.js";
import { ensureRuntimeRewardAccountsRegisteredProgram } from "../../src/transactions/script-reward-registration.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./availability-challenge.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./cardano-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

// The user accepted the hard limit without reserve for these five roles only.
const hardLimitAcceptedContracts = new Set<string>([
  "fraudProofValueNotPreservedStep02",
  "fraudProofResolvedOutputNonCanonicalStep04",
  "fraudProofExecutionSourceScriptDecodingStep02",
  "fraudProofReceivePurposeLanguageStep02",
  "fraudProofExecutionNativeScriptInvalidStep02",
]);

export type PublishedWorkflowDeploymentAccounts = Readonly<{
  operator: ReturnType<typeof generateEmulatorAccount>;
  publisher: ReturnType<typeof generateEmulatorAccount>;
  cosigner: ReturnType<typeof generateEmulatorAccount>;
}>;

/** Prepare test signers before producing release-bound funding profiles. */
export const createPublishedWorkflowDeploymentAccounts =
  (): PublishedWorkflowDeploymentAccounts => ({
    operator: generateEmulatorAccount({ lovelace: 200_000_000_000n }),
    publisher: generateEmulatorAccount({ lovelace: 4_000_000_000_000n }),
    cosigner: generateEmulatorAccount({ lovelace: 0n }),
  });

/**
 * Ordinary deployment only: every reference is published, the real atomic
 * initialization consumes its nonce, and every availability yield is registered.
 * Callers get a complete deployment for subsequent authenticated workflow tests.
 */
export const publishWorkflowDeployment = async (
  options: Readonly<{
    accounts?: PublishedWorkflowDeploymentAccounts;
    /** Align the emulator's real slot clock with watcher network admission. */
    network?: "Custom" | "Preprod";
  }> = {},
) => {
  const network = options.network ?? "Custom";
  const { operator, publisher, cosigner } =
    options.accounts ?? createPublishedWorkflowDeploymentAccounts();
  const blueprintPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH;
  if (blueprintPath === undefined) {
    throw new Error(
      "Set MIDGARD_REAL_BLUEPRINT_PATH to the frozen testnet blueprint",
    );
  }
  const blueprintJson = await readFile(blueprintPath, "utf8");

  const emulator = new Emulator([operator, publisher], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 16_384,
    maxTxExMem: 16_500_000n,
    maxTxExSteps: 10_000_000_000n,
  });
  emulator.time = 1_788_739_200_000;
  if (network === "Preprod") {
    emulator.slot = unixTimeToEnclosingSlot(
      emulator.time,
      SLOT_CONFIG_NETWORK.Preprod,
    );
    emulator.blockHeight = Math.floor(emulator.slot / 20);
  }
  const operatorLucid = await Lucid(emulator, network);
  const publisherLucid = await Lucid(emulator, network);
  operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  publisherLucid.selectWallet.fromSeed(publisher.seedPhrase);
  // Give initialization a real creating transaction. Emulator genesis outputs
  // have no transaction CBOR for the watcher's historical input resolver.
  const bootstrap = await operatorLucid
    .newTx()
    .collectFrom(await operatorLucid.wallet().getUtxos())
    .pay.ToAddress(await operatorLucid.wallet().address(), {
      lovelace: 10_000_000n,
    })
    .complete({ localUPLCEval: true });
  const bootstrapTxHash = await (
    await bootstrap.sign.withWallet().complete()
  ).submit();
  await operatorLucid.awaitTx(bootstrapTxHash);
  const [nonce] = await operatorLucid.utxosByOutRef([
    { txHash: bootstrapTxHash, outputIndex: 0 },
  ]);
  if (nonce === undefined) {
    throw new Error("Confirmed deployment nonce output is not visible");
  }
  const authPolicy = SDK.createReferenceScriptAuthPolicy(
    publisherLucid,
    emulator.now(),
  );
  const contracts = await loadRealMidgardContractsForTest(nonce, authPolicy);
  const catalogue = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    ),
  );
  const targets = nodeRuntimeReferenceScriptTargets(contracts);
  const expectedRoles = Object.keys(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  );
  if (
    targets.length !== expectedRoles.length ||
    new Set(targets.map(({ name }) => name)).size !== targets.length ||
    targets.some(({ name }) => !expectedRoles.includes(name))
  ) {
    throw new Error(
      `Node reference targets differ from finalized manifest roles: targets=${targets.length} expected=${expectedRoles.length} missing=${expectedRoles.filter((role) => !targets.some(({ name }) => name === role)).join(",")} extra=${targets
        .filter(({ name }) => !expectedRoles.includes(name))
        .map(({ name }) => name)
        .join(",")}`,
    );
  }
  const references = new Map<string, UTxO>();
  const publications: { name: string; utxo: UTxO }[] = [];
  const receipts: {
    role: string;
    contractName: string;
    signedBytes: number;
    publicationLimit: number;
    scriptHash: string;
    signedCborSha256: string;
    outRef: { txHash: string; outputIndex: number };
  }[] = [];
  const walletAddress = await publisherLucid.wallet().address();
  for (const target of targets) {
    const name =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        target.name as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const selectedFundingInputs = SDK.selectReferenceScriptFundingUtxos(
      await publisherLucid.wallet().getUtxos(),
      SDK.referenceScriptPublicationFundingTarget(1),
    );
    const { tx, layout } = await Effect.runPromise(
      SDK.completeReferenceScriptPublicationTxProgram({
        lucid: publisherLucid,
        selectedFundingInputs,
        walletAddress,
        referenceScriptsAddress: walletAddress,
        missingTargets: [target],
        authPolicy,
      }),
    );
    const signed = await tx.sign.withWallet().complete();
    const signedBytes = signed.toCBOR().length / 2;
    const publicationLimit = hardLimitAcceptedContracts.has(name)
      ? 16_384
      : 15_872;
    if (signedBytes > publicationLimit)
      throw new Error(
        `${target.name} exceeds its publication acceptance limit (${publicationLimit})`,
      );
    const txHash = await signed.submit().catch((cause: unknown) => {
      throw new Error(
        `Reference publication failed for ${target.name}: ${inspect(cause, { depth: 8 })}`,
      );
    });
    emulator.awaitBlock();
    const outputIndex = layout.localReferenceOutputs.get(
      target.name,
    )!.outputIndex;
    const [utxo] = await publisherLucid.utxosByOutRef([
      { txHash, outputIndex },
    ]);
    if (
      utxo?.scriptRef == null ||
      validatorToScriptHash(utxo.scriptRef) !==
        validatorToScriptHash(target.script) ||
      utxo.assets[
        SDK.referenceScriptAuthUnit(authPolicy.policyId, target.name)
      ] !== 1n
    ) {
      throw new Error(
        `Published reference identity not visible for ${target.name}`,
      );
    }
    references.set(name, utxo);
    publications.push({ name: target.name, utxo });
    receipts.push({
      role: target.name,
      contractName: name,
      signedBytes,
      publicationLimit,
      scriptHash: validatorToScriptHash(target.script),
      signedCborSha256: createHash("sha256")
        .update(Buffer.from(signed.toCBOR(), "hex"))
        .digest("hex"),
      outRef: { txHash, outputIndex },
    });
  }
  const nodeConfig = {
    HUB_ORACLE_ONE_SHOT_TX_HASH: nonce.txHash,
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonce.outputIndex,
    L1_OPERATOR_SEED_PHRASE: operator.seedPhrase,
    DA_COSIGNER_SEED_PHRASE: cosigner.seedPhrase,
    NETWORK: "Preprod" as const,
  };
  const initBuilder = await Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      operatorLucid,
      contracts,
      nodeConfig,
      catalogue.root,
      undefined,
      atomicProtocolInitReferenceScriptsFromPublications(publications),
    ),
  );
  const init = await initBuilder.complete();
  const initSigned = await init.sign.withWallet().complete();
  const initTxHash = await initSigned.submit().catch((cause: unknown) => {
    throw new Error(
      `Atomic initialization submission failed: ${inspect(cause, { depth: 8 })}`,
    );
  });
  emulator.awaitBlock();
  if ((await operatorLucid.utxosByOutRef([nonce])).length !== 0) {
    throw new Error("Initialization did not consume the deployment nonce");
  }
  const availabilityRegistrations = await Effect.runPromise(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      operatorLucid,
      contracts,
    ),
  );
  if (availabilityRegistrations.some(({ txHash }) => txHash !== null)) {
    throw new Error(
      "Atomic initialization must register all availability reward accounts",
    );
  }
  const rootUnit =
    contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME;
  await Effect.runPromise(
    ensurePhasMembershipRewardAccountRegisteredProgram(operatorLucid),
  );
  await Effect.runPromise(
    ensureRuntimeRewardAccountsRegisteredProgram(operatorLucid, contracts),
  );
  const roots = (
    await operatorLucid.utxosAt(contracts.stateQueue.spendingScriptAddress)
  ).filter((utxo) => utxo.assets[rootUnit] === 1n);
  if (roots.length !== 1 || roots[0]!.txHash !== initTxHash) {
    throw new Error(
      "Atomic initialization did not create one authenticated confirmed-state root",
    );
  }
  const rootDatum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(roots[0]!),
  );
  const genesis = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(rootDatum),
  );
  const expectedGenesis = SDK.makeGenesisConfirmedState(genesis.data.startTime);
  if (
    genesis.link !== "Empty" ||
    Object.entries(expectedGenesis).some(
      ([key, value]) =>
        genesis.data[key as keyof typeof expectedGenesis] !== value,
    )
  ) {
    throw new Error(
      "Initialized confirmed-state root differs from canonical genesis",
    );
  }
  // Close the publication authority and verify uniqueness before recording
  // the finalized installation. Advancing emulator slots changes no L1 limits.
  emulator.awaitSlot(Math.max(1, authPolicy.expiresAtSlot - emulator.slot + 1));
  const confirmedReferences = await publisherLucid.utxosAt(walletAddress);
  for (const role of expectedRoles) {
    const unit = SDK.referenceScriptAuthUnit(authPolicy.policyId, role);
    const quantity = confirmedReferences.reduce(
      (sum, utxo) => sum + (utxo.assets[unit] ?? 0n),
      0n,
    );
    if (quantity !== 1n)
      throw new Error(`Finalized publication role ${role} is not unique`);
  }
  const deploymentInfo = buildContractDeploymentInfoFromContracts(
    contracts,
    SDK.referenceScriptAuthPolicyDeploymentInfo(authPolicy),
    new Map(
      [...references].map(([name, { txHash, outputIndex }]) => [
        name,
        { txHash, outputIndex },
      ]),
    ),
    catalogue,
  );
  const daParams = await Effect.runPromise(deriveOperatorDaParams(nodeConfig));
  const manifest = buildDeploymentManifest(deploymentInfo, {
    network,
    referenceScriptDeployAddress: walletAddress,
    hubOracleOneShotTxHash: nonce.txHash,
    hubOracleOneShotOutputIndex: nonce.outputIndex,
    hubOracleOneShotStatus: "consumed_by_init",
    steps: {
      initProtocol: { status: "complete", txHash: initTxHash },
      availabilityRegistration: { status: "complete" },
    },
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
    cardanoProtocolParameters: {
      snapshot: TEST_CARDANO_PROTOCOL_PARAMETERS,
      digest: computeDeploymentManifestJsonDigest(
        TEST_CARDANO_PROTOCOL_PARAMETERS,
      ),
    },
    genesis: {
      headerHash: genesis.data.headerHash,
      utxoSetDigest: computeDeploymentManifestJsonDigest([]),
    },
    da: {
      committeeVkeys: daParams.committee.match(/.{64}/gu)!,
      committeeSignersHash: daParams.committee_signers_hash,
      threshold: Number(daParams.da_threshold),
      transportProfile: {
        protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS,
        retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      },
    },
    artifacts: {
      blueprintHash: createHash("sha256").update(blueprintJson).digest("hex"),
    },
  });
  verifyFinalizedDeploymentManifest(manifest);
  return {
    initialization: {
      txHash: initTxHash,
      signedBytes: initSigned.toCBOR().length / 2,
      rootOutRef: {
        txHash: roots[0]!.txHash,
        outputIndex: roots[0]!.outputIndex,
      },
      nonceConsumed: true,
      genesisConfirmed: true,
    },
    availabilityRegistrations,
    emulator,
    operatorLucid,
    publisherLucid,
    contracts,
    references,
    receipts,
    blueprintJson,
    deploymentInfo,
    manifest,
  };
};

import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifestCardanoProtocolParameters,
  verifyFinalizedDeploymentManifest,
  verifyReferenceScriptPublicationAuthority,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
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
import {
  ensureEventHistoryRewardAccountsRegisteredProgram,
  ensureRuntimeRewardAccountsRegisteredProgram,
} from "../../src/transactions/script-reward-registration.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./availability-challenge.js";
import { runWithoutFollower } from "./intent-journal.js";
import {
  awaitReferenceScriptPublicationReadiness,
  hardLimitAcceptedContracts,
  type PublishedWorkflowChain,
  type PublishedWorkflowDeploymentResume,
  submitPublishedInitialization,
} from "./published-workflow-deployment.submit-published-initialization.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";
import {
  publicationAuthorityLifetime,
  type PublicationSchedule,
  publishReferenceChain,
} from "./reference-publication-chain.js";

export const publishWorkflowDeploymentOnChain = async ({
  network,
  accounts,
  operatorLucid,
  publisherLucid,
  chain,
  protocolParameters,
  publicationMaxTargetsPerBatch = 8,
  eventHistoryProtectionDurationMs,
  publicationJournalPath,
  publicationSchedule,
  publicationSynchronize,
  onPublication = () => {},
  onPrepared = () => {},
  onInitialization = () => {},
  resume,
}: Readonly<{
  network: "Custom" | "Preprod";
  accounts: Readonly<
    Record<
      "operator" | "publisher" | "cosigner",
      { readonly seedPhrase: string }
    >
  >;
  operatorLucid: LucidEvolution;
  publisherLucid: LucidEvolution;
  chain: PublishedWorkflowChain;
  protocolParameters: DeploymentManifestCardanoProtocolParameters;
  publicationJournalPath: string;
  publicationSchedule: PublicationSchedule;
  publicationSynchronize: () => Promise<number>;
  publicationMaxTargetsPerBatch?: number;
  eventHistoryProtectionDurationMs?: bigint;
  resume?: PublishedWorkflowDeploymentResume;
  onPrepared?: (
    context: Pick<PublishedWorkflowDeploymentResume, "nonce" | "authPolicy">,
  ) => void | Promise<void>;
  onInitialization?: (signedCbor: string) => void | Promise<void>;
  onPublication?: (receipt: {
    role: string;
    signedCbor: string;
    outRef: { txHash: string; outputIndex: number };
  }) => void | Promise<void>;
}>) => {
  const { operator, cosigner } = accounts;
  // Same resolution as the package's other real-contract suites: an explicit
  // frozen blueprint wins; otherwise the locally built testnet blueprint.
  const blueprintPath =
    process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
    fileURLToPath(
      new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
    );
  const blueprintJson = await readFile(blueprintPath, "utf8").catch(
    (cause: unknown) => {
      throw new Error(
        `Missing Aiken blueprint at ${blueprintPath}. Run \`aiken build --env testnet\` in onchain/aiken, or point MIDGARD_REAL_BLUEPRINT_PATH at one.`,
        { cause },
      );
    },
  );
  // Give initialization a real creating transaction. Emulator genesis outputs
  // have no transaction CBOR for the watcher's historical input resolver.
  let nonce = resume?.nonce;
  if (nonce === undefined) {
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
    await operatorLucid.awaitTx(bootstrapTxHash, 500);
    [nonce] = await operatorLucid.utxosByOutRef([
      { txHash: bootstrapTxHash, outputIndex: 0 },
    ]);
    if (nonce === undefined) {
      throw new Error("Confirmed deployment nonce output is not visible");
    }
  }
  const authPolicy =
    resume?.authPolicy ??
    (await SDK.createReferenceScriptAuthPolicy(
      publisherLucid,
      chain.now(),
      publicationAuthorityLifetime(
        Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE)
          .length,
        publicationSchedule,
      ),
    ));
  const walletAddress = await publisherLucid.wallet().address();
  const authPolicyMetadata =
    SDK.referenceScriptAuthPolicyDeploymentInfo(authPolicy);
  verifyReferenceScriptPublicationAuthority({
    cborHex: authPolicyMetadata.nativeScript.cborHex,
    expiresAtSlot: authPolicyMetadata.nativeScript.expiresAtSlot,
    publisherAddress: walletAddress,
    postTimelockAuditRequired: authPolicyMetadata.postTimelockAudit.required,
  });
  await onPrepared({ nonce, authPolicy });
  const contracts = await loadRealMidgardContractsForTest(
    nonce,
    authPolicy,
    eventHistoryProtectionDurationMs,
  );
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
  if (
    !Number.isSafeInteger(publicationMaxTargetsPerBatch) ||
    publicationMaxTargetsPerBatch < 1 ||
    publicationMaxTargetsPerBatch > 32
  )
    throw new Error(
      "Reference publication batch size must be between 1 and 32",
    );
  const publicationResult = await publishReferenceChain({
    lucid: publisherLucid,
    targets,
    authPolicy,
    journalPath: publicationJournalPath,
    schedule: publicationSchedule,
    maxTargetsPerBatch: publicationMaxTargetsPerBatch,
    publicationLimit: (role) =>
      hardLimitAcceptedContracts.has(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ],
      )
        ? 16_384
        : 15_872,
    synchronize: publicationSynchronize,
    wait: async () => {
      await chain.delaySlots(1);
    },
    now: chain.now,
    priorPublications: resume?.publications,
  });
  for (const transaction of publicationResult.transactions) {
    for (const { role, outputIndex } of transaction.roles) {
      const target = targets.find(({ name }) => name === role)!;
      const name =
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ];
      const [utxo] = await publisherLucid.utxosByOutRef([
        { txHash: transaction.hash, outputIndex },
      ]);
      if (
        utxo?.scriptRef == null ||
        validatorToScriptHash(utxo.scriptRef) !==
          validatorToScriptHash(target.script) ||
        utxo.assets[SDK.referenceScriptAuthUnit(authPolicy.policyId, role)] !==
          1n
      )
        throw new Error(`Confirmed reference identity differs for ${role}`);
      references.set(name, utxo);
      publications.push({ name: role, utxo });
      if (!resume?.publications.some((receipt) => receipt.role === role)) {
        await onPublication({
          role,
          signedCbor: transaction.signedCbor,
          outRef: { txHash: transaction.hash, outputIndex },
        });
      }
      receipts.push({
        role,
        contractName: name,
        signedBytes: transaction.signedBytes,
        publicationLimit: hardLimitAcceptedContracts.has(name)
          ? 16_384
          : 15_872,
        scriptHash: validatorToScriptHash(target.script),
        signedCborSha256: createHash("sha256")
          .update(Buffer.from(transaction.signedCbor, "hex"))
          .digest("hex"),
        outRef: { txHash: transaction.hash, outputIndex },
      });
    }
  }
  const nodeConfig = {
    HUB_ORACLE_ONE_SHOT_TX_HASH: nonce.txHash,
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonce.outputIndex,
    L1_OPERATOR_SEED_PHRASE: operator.seedPhrase,
    DA_COSIGNER_SEED_PHRASE: cosigner.seedPhrase,
    NETWORK: "Preprod" as const,
  };
  await runWithoutFollower(
    ensureEventHistoryRewardAccountsRegisteredProgram(
      publisherLucid,
      contracts,
    ),
  );
  let initCbor = resume?.initializationCbor;
  if (initCbor === undefined) {
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
    const init = await initBuilder.complete({ localUPLCEval: true });
    const initSigned = await init.sign.withWallet().complete();
    initCbor = initSigned.toCBOR();
  }
  const initTxHash = await submitPublishedInitialization({
    lucid: operatorLucid,
    nonce,
    signedCbor: initCbor,
    onPrepared: onInitialization,
    synchronize: publicationSynchronize,
    now: chain.now,
  });
  if ((await operatorLucid.utxosByOutRef([nonce])).length !== 0) {
    throw new Error("Initialization did not consume the deployment nonce");
  }
  const availabilityRegistrations = await runWithoutFollower(
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
  await runWithoutFollower(
    ensurePhasMembershipRewardAccountRegisteredProgram(operatorLucid),
  );
  await runWithoutFollower(
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
  // New policies require the publisher's signature. Audit confirmed references
  // immediately; only historical time-only policies need an expiry barrier.
  await awaitReferenceScriptPublicationReadiness({
    authPolicy,
    publisherAddress: walletAddress,
    synchronize: publicationSynchronize,
    awaitSlot: chain.delaySlots,
  });
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
      snapshot: protocolParameters,
      digest: computeDeploymentManifestJsonDigest(protocolParameters),
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
      signedBytes: initCbor.length / 2,
      rootOutRef: {
        txHash: roots[0]!.txHash,
        outputIndex: roots[0]!.outputIndex,
      },
      nonceConsumed: true,
      genesisConfirmed: true,
    },
    availabilityRegistrations,
    chain,
    operatorLucid,
    publisherLucid,
    contracts,
    references,
    receipts,
    publicationMetrics: publicationResult.metrics,
    publicationJournalPath,
    blueprintJson,
    deploymentInfo,
    manifest,
  };
};

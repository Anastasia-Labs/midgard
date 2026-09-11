import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  computeFraudProofReleaseFinalityPolicyDigest,
  createCatalogueCompleteCanonicalReplay,
  createHeaderClassifier,
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1SnapshotAuthority,
  headerDecisionCanonicalEvidence,
  resolveHistoricalNativeScriptCorpus,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { unsafeCreateCrossBlockSettlementAuthorityFromRawForTest } from "@al-ft/midgard-fault-proofs/test-support/cross-block-settlement-authority";
import { captureRetainedPlutusIdentityOrigins } from "@al-ft/midgard-fault-proofs/test-support/retained-reason-classifier";
import {
  requireTransitionTraceL1Events,
  unsafeCreateTransitionTraceEventAuthorityFromRawForTest,
} from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";

import type { VerifiableJourneyBlock } from "./fixture-verification.js";

/**
 * Exercise the unmodified installed selector against real retained transaction
 * material. Only local raw transport/finality is synthetic. This helper is for
 * transaction-only blocks: it never substitutes fabricated L1 event provenance
 * for a deposit, withdrawal or forced order in the challenged block.
 */
export const classifyFullCatalogueTransactionFixture = async (input: {
  block: VerifiableJourneyBlock;
  predecessor: VerifiableJourneyBlock;
  history: readonly VerifiableJourneyBlock[];
  /** Only the synthetic empty-event hub seed uses this valid control transaction. */
  originTransaction?: Parameters<
    typeof captureRetainedPlutusIdentityOrigins
  >[0]["transaction"];
  forced?: Pick<
    Parameters<typeof captureRetainedPlutusIdentityOrigins>[0],
    "transaction" | "orderKey"
  >;
}) => {
  const envelope = await unwrapDaPayload(
    Buffer.from(input.block.payloadEnvelopeCbor),
    { maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes },
  );
  const payload = SDK.decodeDaPayload(Buffer.from(envelope.innerBytes));
  const body = payload.block_body;
  if (
    body.deposits.length + body.withdrawals.length > 0 ||
    (body.forced_transactions.length > 0 && input.forced === undefined)
  )
    throw new Error(
      "Full local transaction verifier requires exact raw event authority for event-bearing blocks",
    );
  const first = body.transaction_preimages[0];
  if (
    first === undefined &&
    input.forced === undefined &&
    input.originTransaction === undefined
  )
    throw new Error(
      "Full local transaction verifier requires a retained transaction",
    );
  const canonicalCbor =
    input.forced?.transaction.canonicalCbor ??
    input.originTransaction?.canonicalCbor ??
    Buffer.from(first![1], "hex");
  const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const proof =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const source: SDK.L2TransactionSource = {
    tx_id: txId,
    source: {
      compact_cbor: proof.compactCbor.toString("hex"),
      witness_set_compact_cbor: proof.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proof.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  const seedHandle = await captureRetainedPlutusIdentityOrigins(
    {
      block: {
        ...input.block,
        payloadEnvelopeCbor: Buffer.from(input.block.payloadEnvelopeCbor),
      },
      transaction: {
        txId,
        canonicalCbor,
        compactCbor: Buffer.from(proof.compactCbor),
        source,
        sourceValueBytes: Buffer.from(
          Data.to(source, SDK.L2TransactionSource),
          "hex",
        ),
      },
      orderKey: input.forced?.orderKey ?? {
        transactionId: txId,
        outputIndex: 0n,
      },
    },
    {
      omitEvent: input.forced === undefined,
      inclusionTime: input.block.header.endTime - 1n,
    },
  );
  const seed = requireTransitionTraceL1Events(seedHandle).snapshot;
  const hub = seed.scopes.find((scope) => scope.role === "hub_oracle");
  if (hub === undefined) throw new Error("Local raw authority has no hub");
  const hubOraclePolicyId = getAddressDetails(hub.address).paymentCredential!
    .hash;
  const raw: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) => ({
      ...seed,
      deploymentIdentityDigest: request.deploymentIdentityDigest,
      blueprintHash: request.blueprintHash,
      finalityPolicyDigest: request.finalityPolicyDigest,
      headerHash: request.headerHash,
      scopes: request.scopes.map((scope) => ({
        ...scope,
        utxos:
          seed.scopes.find((existing) => existing.address === scope.address)
            ?.utxos ?? [],
      })),
      historyUnits: request.historyUnits,
      history: request.historyUnits.map((unit) => {
        const value = seed.history.find((existing) => existing.unit === unit);
        if (value === undefined)
          throw new Error(
            "Local raw authority has no coverage for requested event unit",
          );
        return value;
      }),
    }),
  };
  const deploymentFingerprint = "d1".repeat(32);
  const policy = {
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  } as const;
  const releaseFinality = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
  const directory = await mkdtemp("/var/tmp/midgard-full-catalogue-local-");
  try {
    const historySource = createHistoricalNativeScriptHistorySource({
      providerRoster: createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint,
        providers: [
          {
            sourceId: "retained-local-a",
            authorityEndpoint: "https://retained-a.example.test",
            operatorIdentitySha256: "aa".repeat(32),
          },
          {
            sourceId: "retained-local-b",
            authorityEndpoint: "https://retained-b.example.test",
            operatorIdentitySha256: "bb".repeat(32),
          },
        ],
      }),
    });
    const checkpointStore = createSqliteHistoricalNativeScriptCheckpointStore({
      path: join(directory, "history.sqlite"),
      rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
    });
    // As in the established catalogue-retained classifier unit test, these are
    // only the fields consumed by raw authority admission; no deployment claim.
    const bindingFields = {
      deploymentFingerprint,
      blueprintHash: releaseFinality.blueprintHash,
      network: "Preprod" as const,
      releaseFinality,
      resolvedContracts: { hubOraclePolicyId },
      definition: { headerHash: input.block.headerHash },
    };
    const transitionTraceEventAuthority =
      unsafeCreateTransitionTraceEventAuthorityFromRawForTest({
        binding: bindingFields as Parameters<
          typeof unsafeCreateTransitionTraceEventAuthorityFromRawForTest
        >[0]["binding"],
        authority: raw,
      });
    const settlementAuthority =
      unsafeCreateCrossBlockSettlementAuthorityFromRawForTest({
        binding: bindingFields as Parameters<
          typeof unsafeCreateCrossBlockSettlementAuthorityFromRawForTest
        >[0]["binding"],
        raw,
        historySource,
      });
    const lucid = await Lucid(new Emulator([]), "Preprod");
    const replayer = createCatalogueCompleteCanonicalReplay({
      lucid,
      network: "Preprod",
      hubOraclePolicyId,
      minimumConfirmationDepth: policy.confirmationDepth,
      owner: "b1".repeat(28),
    });
    const classifier = await createHeaderClassifier({
      deploymentFingerprint,
      replayer,
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
      historicalReplayAuthority: { checkpointStore, historySource },
      settlementAuthority,
      transitionTraceEventAuthority,
    });
    const sources: RetainedDaPayloadSource[] = [
      {
        sourceId: "retained-local",
        fetchPayloadByHeaderHash: async (headerHash) => {
          const retained = [
            input.block,
            input.predecessor,
            ...input.history,
          ].find((block) => block.headerHash === headerHash);
          if (retained === undefined)
            throw new Error(`Missing actual retained ancestor ${headerHash}`);
          return {
            ok: true,
            sourceId: "retained-local",
            sourcePeerId: "local-test",
            attempts: [],
            payloadEnvelopeCbor: Buffer.from(retained.payloadEnvelopeCbor),
            provenance: {
              trustClass: "public_or_permissionless_da",
              sourceId: "retained-local/local-test",
              grade: "security",
            },
          };
        },
      },
    ];
    const observation = authenticatedHeaderObservation(input.block);
    const decision = await classifyHeader({
      classifier,
      observation,
      predecessorObservation: authenticatedHeaderObservation(input.predecessor),
      sources,
      authenticatedObservationDigest:
        await authenticatedStateQueueObservationDigest({
          observation,
          minimumConfirmationDepth: policy.confirmationDepth,
        }),
    });
    const evidence = await headerDecisionCanonicalEvidence(decision);
    const historicalNativeScriptCorpus =
      evidence === undefined
        ? undefined
        : await resolveHistoricalNativeScriptCorpus({
            deploymentFingerprint,
            checkpointStore,
            historySource,
            currentEvidence: evidence,
            sources,
          });
    return {
      decision,
      evidence,
      historicalNativeScriptCorpus,
      verification: "full-installed-local-classification" as const,
    };
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
};

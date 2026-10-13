/**
 * The installed-catalogue classifier over retained fixture payloads, shared by
 * the catalogue classification tests.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  credentialToAddress,
  getAddressDetails,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import type { RetainedDaPayloadSource } from "../../src/transition-trace/fetch.js";
import {
  createTransitionTraceEventAuthority,
  requireTransitionTraceL1Events,
} from "../../src/transition-trace/l1-events.js";
import { createCatalogueCompleteCanonicalReplay } from "../../src/workflow/catalogue-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  createHeaderClassifier,
} from "../../src/workflow/header-classifier.js";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "../../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  RELEASE_L1_FINALITY_POLICY,
} from "../../src/workflow/release-finality-policy.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "../helpers/transition-history-fixture.js";
import { fixtureLedgerAt } from "./fixture-ledger.js";
import { fraudProofL1SourceForTest } from "./fraud-proof-l1-source.js";
import {
  type buildRetainedPlutusIdentityFixture,
  captureRetainedPlutusIdentityOrigins,
} from "./retained-reason-classifier.js";

/** The raw L1 snapshot authority the current case installed; reset after each. */
export const transport = { raw: undefined as unknown };

const DEPLOYMENT = "d1".repeat(32);
const RELEASE = "e1".repeat(32);
const policy = { ...RELEASE_L1_FINALITY_POLICY };
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};
const releaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => releaseFinality,
};

// These transaction-only cases must never query a deposit/withdrawal provider.
// Their real evidence-authority factories are still included in the catalogue.
const unusedEventProvider = {
  utxosByOutRef: async () => {
    throw new Error(
      "Unexpected fabricated-event lookup in transaction-only fixture",
    );
  },
  utxosAtWithUnit: async () => {
    throw new Error(
      "Unexpected fabricated-event lookup in transaction-only fixture",
    );
  },
} as unknown as LucidEvolution;

export const setup = async (
  fixture: Pick<
    Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>,
    "block" | "predecessor" | "transaction" | "orderKey"
  >,
  sourceKind: "normal" | "forced",
) => {
  const seedHandle = await captureRetainedPlutusIdentityOrigins(fixture, {
    omitEvent: sourceKind === "normal",
  });
  const seed = requireTransitionTraceL1Events(seedHandle).snapshot;
  const hubScope = seed.scopes.find(({ role }) => role === "hub_oracle")!;
  const hubOraclePolicyId = getAddressDetails(hubScope.address)
    .paymentCredential!.hash;
  // Adapt only the requested raw address/history coverage; the same raw body
  // authenticates hub and forced NFT.
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
          seed.scopes.find(({ address }) => address === scope.address)?.utxos ??
          [],
      })),
      historyUnits: request.historyUnits,
      history: request.historyUnits.map((unit) => {
        const history = seed.history.find((item) => item.unit === unit);
        if (history === undefined)
          throw new Error("Fixture cannot supply unknown raw unit history");
        return history;
      }),
    }),
  };
  transport.raw = raw;
  const bindingFields = {
    deploymentFingerprint: DEPLOYMENT,
    blueprintHash: RELEASE,
    network: "Preprod" as const,
    releaseFinality,
    resolvedContracts: {
      hubOraclePolicyId,
      contracts: {
        transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
      },
    },
    definition: { headerHash: fixture.block.headerHash },
  };
  // Raw transport unit fixtures only: no deployment-manifest or chain-lifecycle
  // assurance is inferred from these minimal consumed binding fields.
  const transitionBinding = bindingFields as Parameters<
    typeof createTransitionTraceEventAuthority
  >[0]["binding"];
  const transitionTraceEventAuthority = createTransitionTraceEventAuthority({
    binding: transitionBinding,
    l1: fraudProofL1SourceForTest({ authority: () => transport.raw }),
  });
  const history = {
    inlineLimitBytes: 512n,
    maxPayloadBytes: 5000n,
    maxPayloadNodes: 512n,
    retentionAddress: credentialToAddress("Preprod", {
      type: "Script",
      hash: "ee".repeat(28),
    }),
  };
  const replayer = createCatalogueCompleteCanonicalReplay({
    history: { deposit: history, withdrawal: history },
    lucid: unusedEventProvider,
    network: "Preprod",
    hubOraclePolicyId,
    minimumConfirmationDepth: policy.confirmationDepth,
    owner: "b1".repeat(28),
  });
  const classifierInput = {
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    deploymentFingerprint: DEPLOYMENT,
    replayer,
    releaseFinalityAuthority,
    transitionTraceEventAuthority,
  };
  const classifier = await createHeaderClassifier(classifierInput);
  const sources: readonly RetainedDaPayloadSource[] = [
    {
      sourceId: "retained-fixture",
      fetchPayloadByHeaderHash: async (headerHash) => {
        const retained = [fixture.block, fixture.predecessor].find(
          (block) => block.headerHash === headerHash,
        );
        if (retained === undefined)
          throw new Error(
            "Retained catalogue fixture requested another header",
          );
        return {
          ok: true,
          sourceId: "retained-fixture",
          sourcePeerId: "unit",
          attempts: [],
          payloadEnvelopeCbor: retained.payloadEnvelopeCbor,
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "retained-fixture/unit",
            grade: "security",
          },
        };
      },
    },
  ];
  const observation = authenticatedHeaderObservation(fixture.block);
  const classifyInput = {
    classifier,
    observation,
    sources,
    predecessorLedgerAt: fixtureLedgerAt({ sources }),
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.confirmationDepth,
      }),
  };
  return { replayer, classifierInput, classifyInput, transitionBinding };
};

// These cases construct retained evidence and replay the complete catalogue.
// The five-second unit budget timed out in 3/20 runs under concurrent load.
export const replayBudget = { timeout: 30_000 };

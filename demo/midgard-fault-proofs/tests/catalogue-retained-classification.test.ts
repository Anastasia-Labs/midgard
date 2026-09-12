import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";

import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  GENESIS_HEADER_HASH,
  hashBlockHeader,
  MIN_FEE_VIOLATION_ID,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { getAddressDetails, type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

const transport = vi.hoisted(() => ({ raw: undefined as unknown }));
// Same raw transport seam as transition-trace-installed-lifecycle.test.ts.
// Every snapshot, origin, replay, detector and classifier admission remains real.
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyLocalKupmiosL1ObservationPort: () => ({
      rawL1: transport.raw,
    }),
  };
});

import { unsafeCreateCrossBlockSettlementAuthorityFromRawForTest } from "../src/cross-block-duplicate-event/settlement-authority.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import {
  createTransitionTraceEventAuthority,
  requireTransitionTraceL1Events,
} from "../src/transition-trace/l1-events.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { UNUSED_REDEEMER_VIOLATION_ID } from "../src/unused-redeemer/family.js";
import { buildUnusedRedeemerObservationFromRetainedDa } from "../src/unused-redeemer/replay.js";
import { readValidationTraceReplaySelection } from "../src/validation-dispute/replay.js";
import { createCatalogueCompleteCanonicalReplay } from "../src/workflow/catalogue-replay.js";
import {
  MIN_FEE_COMPLETE_CANONICAL_REPLAY,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayBundle,
  requireCompleteCanonicalReplayDecision,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  headerDecisionCanonicalEvidence,
  headerDecisionReplayContext,
} from "../src/workflow/header-classifier.js";
import {
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
} from "../src/workflow/historical-native-script-corpus.js";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedPlutusIdentityFixture,
  buildRetainedPlutusUnboundVariableFixture,
  captureRetainedPlutusIdentityOrigins,
} from "./support/retained-reason-classifier.js";

const DEPLOYMENT = "d1".repeat(32);
const RELEASE = "e1".repeat(32);
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
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
const directories: string[] = [];
afterEach(() => {
  for (const directory of directories.splice(0))
    rmSync(directory, { recursive: true, force: true });
  transport.raw = undefined;
});

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

const setup = async (
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
  // Adapt only the requested raw address/history coverage. Empty settlement
  // coverage is explicit; the same raw body authenticates hub and forced NFT.
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
  const historySource = createHistoricalNativeScriptHistorySource({
    providerRoster: createHistoricalNativeScriptProviderRoster({
      deploymentFingerprint: DEPLOYMENT,
      providers: [
        {
          sourceId: "archive-a",
          authorityEndpoint: "https://archive-a.example.test",
          operatorIdentitySha256: "aa".repeat(32),
        },
        {
          sourceId: "archive-b",
          authorityEndpoint: "https://archive-b.example.test",
          operatorIdentitySha256: "bb".repeat(32),
        },
      ],
    }),
  });
  const directory = mkdtempSync("/var/tmp/midgard-catalogue-retained-");
  directories.push(directory);
  const checkpointStore = createSqliteHistoricalNativeScriptCheckpointStore({
    path: join(directory, "history.sqlite"),
    rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
  });
  const bindingFields = {
    deploymentFingerprint: DEPLOYMENT,
    blueprintHash: RELEASE,
    network: "Preprod" as const,
    releaseFinality,
    resolvedContracts: { hubOraclePolicyId },
    definition: { headerHash: fixture.block.headerHash },
  };
  // Raw transport unit fixtures only: no deployment-manifest or chain-lifecycle
  // assurance is inferred from these minimal consumed binding fields.
  const transitionBinding = bindingFields as Parameters<
    typeof createTransitionTraceEventAuthority
  >[0]["binding"];
  const transitionTraceEventAuthority = createTransitionTraceEventAuthority({
    binding: transitionBinding,
    source: {} as never,
  });
  const settlementAuthority =
    unsafeCreateCrossBlockSettlementAuthorityFromRawForTest({
      binding: bindingFields as Parameters<
        typeof unsafeCreateCrossBlockSettlementAuthorityFromRawForTest
      >[0]["binding"],
      raw,
      historySource,
    });
  const replayer = createCatalogueCompleteCanonicalReplay({
    lucid: unusedEventProvider,
    network: "Preprod",
    hubOraclePolicyId,
    minimumConfirmationDepth: policy.confirmationDepth,
    owner: "b1".repeat(28),
  });
  const classifierInput = {
    deploymentFingerprint: DEPLOYMENT,
    replayer,
    releaseFinalityAuthority,
    historicalReplayAuthority: { checkpointStore, historySource },
    settlementAuthority,
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
    predecessorObservation: authenticatedHeaderObservation(fixture.predecessor),
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.confirmationDepth,
      }),
  };
  return { replayer, classifierInput, classifyInput, transitionBinding };
};

describe("installed catalogue retained classification", () => {
  it("uses exactly all admitted catalogue members and derives healthy context inside classifyHeader", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const { replayer, classifyInput } = await setup(fixture, "normal");
    expect(replayer.launchScope).toEqual(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    expect(replayer.launchScope).toHaveLength(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    );
    expect(requireCompleteCanonicalReplayBundle(replayer)).toBe(
      replayer.launchScope,
    );
    expect(() => requireCompleteCanonicalReplayBundle({ ...replayer })).toThrow(
      /closed canonical replay/u,
    );
    const decision = await classifyHeader(classifyInput);
    expect(decision).toMatchObject({
      decision: "healthy",
      launchScope: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    });
    const context = headerDecisionReplayContext(decision);
    expect(context?.validationTraceReplay?.headerHash).toBe(
      fixture.block.headerHash,
    );
    expect(context?.validationTraceReplay?.eventSnapshotDigest).toBe(
      context?.transitionTraceEvents?.snapshotDigest,
    );
    expect(context?.historicalCorpus).toBeDefined();
    expect(context?.settlements).toBeDefined();
    const copy = await headerDecisionCanonicalEvidence(decision);
    expect(copy?.headerHash).toBe(fixture.block.headerHash);
    copy!.reconstruction.payloadEnvelopeCbor.fill(0);
    Object.assign(copy!.observation.header, { endTime: 0n });
    const fresh = await headerDecisionCanonicalEvidence(decision);
    expect(
      fresh!.reconstruction.payloadEnvelopeCbor.equals(
        fixture.block.payloadEnvelopeCbor,
      ),
    ).toBe(true);
    expect(fresh!.observation.header.endTime).toBe(
      fixture.block.header.endTime,
    );
    await expect(
      headerDecisionCanonicalEvidence({ ...decision }),
    ).rejects.toThrow(/module-admitted/u);
  });

  it("classifies the existing accepted forced identity through all catalogue members", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture(
      { verdict: "accepted" },
      { sourceKind: "forced" },
    );
    const { replayer, classifyInput } = await setup(fixture, "forced");
    expect(replayer.launchScope).toEqual(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    expect(replayer.launchScope).toHaveLength(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    );
    const decision = await classifyHeader(classifyInput);
    expect(decision).toMatchObject({
      decision: "healthy",
      headerHash: fixture.block.headerHash,
      launchScope: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    });
    const context = headerDecisionReplayContext(decision);
    expect(context?.validationTraceReplay?.eventSnapshotDigest).toBe(
      context?.transitionTraceEvents?.snapshotDigest,
    );
    expect(context?.transitionTraceEvents).toBeDefined();
    const evidence = await headerDecisionCanonicalEvidence(decision);
    expect(evidence?.reconstruction.forcedTransactions).toHaveLength(1);
    expect(evidence?.reconstruction.transactions).toHaveLength(0);
  });

  it("routes an accepted ordinary transaction below the committed fee through the full catalogue", async () => {
    // Keep the existing complete accepted trace, transaction and ledger material.
    // Only the header fee context changes to the small value already used by
    // retained-classification-polarities.test.ts. Canonical replay recomputes it.
    const retained = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const reconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      expectedHeaderHash: retained.block.headerHash,
      committedHeader: retained.block.header,
    });
    const header = { ...retained.block.header, minFeeB: 8n };
    const headerHash = await Effect.runPromise(hashBlockHeader(header));
    const payloadEnvelopeCbor = await reencodeFixturePayload({
      ...reconstruction.payload,
      block_body: {
        ...reconstruction.payload.block_body,
        header,
        header_hash: headerHash,
      },
    });
    const fixture = {
      block: { header, headerHash, payloadEnvelopeCbor },
      predecessor: retained.predecessor,
      transaction: retained.transaction,
      orderKey: retained.orderKey,
    };
    const { replayer, classifyInput } = await setup(fixture, "normal");
    expect(replayer.launchScope).toEqual(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    const decision = await classifyHeader(classifyInput);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "minFee",
      violationId: MIN_FEE_VIOLATION_ID,
      headerHash,
      position: "0",
      detectionId: `${MIN_FEE_VIOLATION_ID}:0:${fixture.transaction.txId}:0:8`,
    });
    const evidence = await headerDecisionCanonicalEvidence(decision);
    expect(evidence?.header.minFeeB).toBe(8n);
    expect(
      evidence?.reconstruction.transactions[0]?.fullTransactionCbor,
    ).toEqual(retained.transaction.canonicalCbor);
    expect(
      headerDecisionReplayContext(decision)?.validationTraceReplay,
    ).toBeDefined();
  });

  it.each([
    {
      label: "network alone",
      minFeeB: 0n,
      expectedNetworkId: 1n,
      category: "networkId",
      feeFaults: 0,
      networkFaults: 2,
    },
    {
      label: "fee and network together",
      minFeeB: 8n,
      expectedNetworkId: 1n,
      category: "minFee",
      feeFaults: 1,
      networkFaults: 2,
    },
  ] as const)(
    "selects catalogue priority with $label at the same transaction position",
    async ({
      minFeeB,
      expectedNetworkId,
      category,
      feeFaults,
      networkFaults,
    }) => {
      // Reuse the complete accepted identity unchanged. Only ordinary committed
      // header parameters vary; no execution program or transaction field changes.
      const retained = await buildRetainedPlutusIdentityFixture({
        verdict: "accepted",
      });
      const reconstruction = await reconstructDaPayload({
        payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
        expectedHeaderHash: retained.block.headerHash,
        committedHeader: retained.block.header,
      });
      const header = { ...retained.block.header, minFeeB, expectedNetworkId };
      const headerHash = await Effect.runPromise(hashBlockHeader(header));
      const payloadEnvelopeCbor = await reencodeFixturePayload({
        ...reconstruction.payload,
        block_body: {
          ...reconstruction.payload.block_body,
          header,
          header_hash: headerHash,
        },
      });
      const fixture = {
        ...retained,
        block: { header, headerHash, payloadEnvelopeCbor },
      };
      const { replayer, classifyInput } = await setup(fixture, "normal");
      expect(replayer.launchScope).toEqual(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      );
      expect(replayer.launchScope).toHaveLength(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      );
      const decision = await classifyHeader(classifyInput);
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category,
        headerHash,
        position: "0",
      });
      const evidence = await headerDecisionCanonicalEvidence(decision);
      if (evidence === undefined)
        throw new Error("Expected admitted canonical priority evidence");
      expect(
        evidence.reconstruction.transactions[0]?.fullTransactionCbor,
      ).toEqual(retained.transaction.canonicalCbor);
      // Independent real family replays establish that the lower-priority fault
      // still exists in the selected block, rather than inferring overlap solely
      // from changed header values or injecting synthetic detector results.
      const fee = requireCompleteCanonicalReplayDecision({
        evidence,
        replayer: MIN_FEE_COMPLETE_CANONICAL_REPLAY,
        decision: await MIN_FEE_COMPLETE_CANONICAL_REPLAY.replay(evidence),
      });
      const network = requireCompleteCanonicalReplayDecision({
        evidence,
        replayer: NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
        decision: await NETWORK_ID_COMPLETE_CANONICAL_REPLAY.replay(evidence),
      });
      expect(fee).toHaveLength(feeFaults);
      expect(network).toHaveLength(networkFaults);
      for (const detection of [...fee, ...network])
        expect(detection).toMatchObject({ headerHash, position: 0n });
      expect(
        decision.decision === "fault_detected" && decision.detectionId,
      ).toBe(
        category === "minFee" ? fee[0]!.detectionId : "network-id:0:output:0",
      );
      expect(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf("minFee"),
      ).toBeLessThan(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf("networkId"));
      expect(
        headerDecisionReplayContext(decision)?.validationTraceReplay,
      ).toBeDefined();
    },
  );

  it("routes an exact forced Plutus wrongful rejection through the same full catalogue", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture(
      {
        verdict: "rejected",
        reason: { PlutusExecutionFailed: { execution_index: 0n } },
      },
      { sourceKind: "forced" },
    );
    const { classifyInput } = await setup(fixture, "forced");
    const decision = await classifyHeader(classifyInput);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "validationTraceDispute",
      violationId: "validation-trace",
    });
    expect(
      headerDecisionReplayContext(decision)?.validationTraceReplay,
    ).toBeDefined();
    if (decision.decision !== "fault_detected")
      throw new Error("Expected selected Plutus fault");
    const context = headerDecisionReplayContext(decision)!;
    const copiedEvidence = await headerDecisionCanonicalEvidence(decision);
    const selection = readValidationTraceReplaySelection({
      evidence: copiedEvidence!,
      context: context.validationTraceReplay!,
      predecessor: context.predecessor,
      transitionTraceEvents: context.transitionTraceEvents,
      detectionId: decision.detectionId,
    });
    expect(selection.headerHash).toBe(fixture.block.headerHash);
  });

  it("keeps the direct minimum-fee route for its exact typed wrongful rejection", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture(
      { verdict: "rejected", reason: "FeeBelowMinimum" },
      { sourceKind: "forced" },
    );
    const { classifyInput } = await setup(fixture, "forced");
    const decision = await classifyHeader(classifyInput);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "minFee",
    });
  });

  it("routes the selected identity redeemer's wrongful forced rejection through the full catalogue", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture(
      {
        verdict: "rejected",
        reason: { UnusedRedeemer: { redeemer_index: 0n } },
      },
      { sourceKind: "forced" },
    );
    expect(fixture.replay.trace.verdict).toBe("accepted");
    const { replayer, classifyInput } = await setup(fixture, "forced");
    expect(replayer.launchScope).toEqual(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    expect(replayer.launchScope).toHaveLength(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    );
    const decision = await classifyHeader(classifyInput);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "unusedRedeemer",
      violationId: UNUSED_REDEEMER_VIOLATION_ID,
      headerHash: fixture.block.headerHash,
      position: "0",
      detectionId: `${UNUSED_REDEEMER_VIOLATION_ID}:forced:0:${fixture.transaction.txId}:0`,
    });
    const evidence = await headerDecisionCanonicalEvidence(decision);
    if (evidence === undefined)
      throw new Error("Expected retained evidence for the selected redeemer");
    const { observation, base } =
      await buildUnusedRedeemerObservationFromRetainedDa({
        block: evidence,
        eventKey: {
          ForcedTransactionEventKey: { tx_order_id: fixture.orderKey },
        },
        transactionId: fixture.transaction.txId,
        redeemerIndex: 0,
        txCbor: fixture.transaction.canonicalCbor,
      });
    expect(observation.unused).toBe(false);
    expect(base.selectedBit).toBe(1n);
    const context = headerDecisionReplayContext(decision);
    expect(context?.transitionTraceEvents).toBeDefined();
    expect(context?.validationTraceReplay?.eventSnapshotDigest).toBe(
      context?.transitionTraceEvents?.snapshotDigest,
    );
  });

  it.each([
    {
      arm: "FieldPreimageLengthMismatch",
      reason: { FieldPreimageLengthMismatch: { field_index: 0n } },
      category: "fieldPreimageLengthMismatch",
    },
    {
      arm: "FieldItemWidthIllegal",
      reason: {
        FieldItemWidthIllegal: { field_index: 2n, item_index: 0n },
      },
      category: "fieldItemWidthIllegal",
    },
    { arm: "EmptyInputs", reason: "EmptyInputs", category: "zeroInput" },
    {
      arm: "NetworkIdMismatch",
      reason: "NetworkIdMismatch",
      category: "networkId",
    },
    {
      arm: "InputNotFound",
      reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
      category: "nonExistentInput",
    },
    {
      arm: "InputSpentOutputNonCanonical",
      reason: {
        InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
      },
      category: "resolvedOutputNonCanonical",
    },
    {
      arm: "OutputNonCanonical",
      reason: { OutputNonCanonical: { output_index: 0n } },
      category: "transactionOutputNonCanonical",
    },
    {
      arm: "OutputBelowMinAda",
      reason: { OutputBelowMinAda: { output_index: 0n } },
      category: "minAda",
    },
  ] as const satisfies readonly {
    arm: string;
    reason: RejectionReason;
    category: FraudProofCatalogueCategoryName;
  }[])(
    "$arm: routes the unchanged accepted identity's wrongful rejection through the full catalogue",
    async ({ reason, category }) => {
      // Every indexed row refers to the existing spend input or output at zero.
      // The table changes only the rejection claim and its retained commitments.
      const accepted = await buildRetainedPlutusIdentityFixture(
        { verdict: "accepted" },
        { sourceKind: "forced" },
      );
      const fixture = await buildRetainedPlutusIdentityFixture(
        { verdict: "rejected", reason },
        { sourceKind: "forced" },
      );
      expect(fixture.transaction.canonicalCbor).toEqual(
        accepted.transaction.canonicalCbor,
      );
      expect(fixture.predecessor.payloadEnvelopeCbor).toEqual(
        accepted.predecessor.payloadEnvelopeCbor,
      );
      expect(fixture.replay.trace.verdict).toBe("accepted");
      const { replayer, classifyInput } = await setup(fixture, "forced");
      expect(replayer.launchScope).toEqual(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      );
      expect(replayer.launchScope).toHaveLength(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      );
      const decision = await classifyHeader(classifyInput);
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category,
        headerHash: fixture.block.headerHash,
        position: "0",
      });
      const evidence = await headerDecisionCanonicalEvidence(decision);
      expect(evidence?.reconstruction.forcedTransactions).toHaveLength(1);
      expect(
        evidence?.reconstruction.forcedTransactions[0]?.value,
      ).toMatchObject({
        tx_id: fixture.transaction.txId,
        verdict: { ForcedTxInvalid: { reason } },
      });
      const context = headerDecisionReplayContext(decision);
      expect(context?.transitionTraceEvents).toBeDefined();
      expect(context?.validationTraceReplay?.eventSnapshotDigest).toBe(
        context?.transitionTraceEvents?.snapshotDigest,
      );
    },
  );

  it("proves an accepted descriptor whose terminal bytes reject the transaction", async () => {
    const fixture = await buildRetainedPlutusUnboundVariableFixture({
      verdict: "accepted",
    });
    const { classifyInput } = await setup(fixture, "normal");
    // The retained terminal witness rejects while the descriptor claims
    // Accepted, so the operator applied a transaction its own authenticated
    // replay refused. Accepted-mismatch evidence is now assembled only from a
    // genuine acceptance witness, which leaves this shape as the buildable
    // one-step transition fault it always was.
    expect(await classifyHeader(classifyInput)).toMatchObject({
      decision: "fault_detected",
      category: "transitionTrace",
      detectionId: "transition-trace:0:invalidOneStepTransition",
      headerHash: fixture.block.headerHash,
      position: "0",
    });
  });

  it("refuses missing predecessor coverage without calling replay", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const { classifyInput } = await setup(fixture, "normal");
    const decision = await classifyHeader({
      ...classifyInput,
      predecessorObservation: undefined,
    });
    expect(decision).toMatchObject({
      decision: "unprovable",
      reason: "predecessor_context_unavailable",
    });
    await expect(
      headerDecisionCanonicalEvidence(decision),
    ).resolves.toBeUndefined();
  });

  it("refuses absent and copied raw event authority at catalogue installation", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const { classifierInput } = await setup(fixture, "normal");
    await expect(
      createHeaderClassifier({
        ...classifierInput,
        transitionTraceEventAuthority: undefined,
      }),
    ).rejects.toThrow(/raw L1 event authority/u);
    await expect(
      createHeaderClassifier({
        ...classifierInput,
        transitionTraceEventAuthority: {
          ...classifierInput.transitionTraceEventAuthority,
        },
      }),
    ).rejects.toThrow(/admitted/u);
  });
  it("requires the non-genesis predecessor even when its ledger is empty", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const predecessor = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: GENESIS_HEADER_HASH,
    });
    const block = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
    });
    const { classifyInput } = await setup(
      { ...fixture, block, predecessor },
      "normal",
    );
    const decision = await classifyHeader({
      ...classifyInput,
      predecessorObservation: undefined,
    });
    expect(decision).toMatchObject({
      decision: "unprovable",
      reason: "predecessor_context_unavailable",
    });
  });

  it("binds optional originating authority to a validation-only classifier's deployment", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const { classifierInput, transitionBinding } = await setup(
      fixture,
      "normal",
    );
    const foreign = createTransitionTraceEventAuthority({
      binding: { ...transitionBinding, deploymentFingerprint: "f1".repeat(32) },
      source: {} as never,
    });
    await expect(
      createHeaderClassifier({
        ...classifierInput,
        replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
        transitionTraceEventAuthority: foreign,
      }),
    ).rejects.toThrow(/raw L1 event authority/u);
    await expect(
      createHeaderClassifier({
        ...classifierInput,
        replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
      }),
    ).resolves.toMatchObject({ launchScope: ["validationTraceDispute"] });
  });
});

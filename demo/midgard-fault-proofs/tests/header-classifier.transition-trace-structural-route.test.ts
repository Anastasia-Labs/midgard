import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  getAddressDetails,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { unsafeCreateCrossBlockSettlementAuthorityFromRawForTest } from "../src/cross-block-duplicate-event/settlement-authority.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import {
  requireTransitionTraceL1Events,
  unsafeCreateTransitionTraceEventAuthorityFromRawForTest,
} from "../src/transition-trace/l1-events.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  encodeData,
  reconstructDaPayload,
} from "../src/transition-trace/reconstruct.js";
import { createCatalogueCompleteCanonicalReplay } from "../src/workflow/catalogue-replay.js";
import { NETWORK_ID_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { digest } from "../src/workflow/header-classifier.authenticated-state-queue-observation-digest.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  headerDecisionReplayContext,
} from "../src/workflow/header-classifier.js";
import { TRANSITION_TRACE_STRUCTURAL_ROUTE } from "../src/workflow/header-classifier.transition-trace-structural-route.js";
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
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "./helpers/transition-history-fixture.js";
import {
  buildRetainedPlutusIdentityFixture,
  captureRetainedPlutusIdentityOrigins,
} from "./support/retained-reason-classifier.js";

const DEPLOYMENT = "d1".repeat(32);
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: "e1".repeat(32),
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
});

type Fixture = Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>;
type Block = Pick<
  Fixture["block"],
  "header" | "headerHash" | "payloadEnvelopeCbor"
>;
type Body = SDK.DaPayload["block_body"];

const entries = (values: readonly SDK.DaPayloadEntry[]) =>
  values.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

/** Re-commits a mutated body under a new header, so the roots authenticate. */
const recommit = async (
  block: Block,
  mutate: (body: Body) => Partial<Body>,
  headerOverrides: Partial<SDK.Header> = {},
): Promise<Block> => {
  const { payload } = await reconstructDaPayload({
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
  });
  const body = { ...payload.block_body, ...mutate(payload.block_body) };
  const sorted = (values: readonly SDK.DaPayloadEntry[]) =>
    [...values].sort(([left], [right]) =>
      left < right ? -1 : left > right ? 1 : 0,
    );
  const eventToStep = sorted(body.event_to_step);
  const header: SDK.Header = {
    ...body.header,
    eventToStepRoot: (
      await buildCountedRoot(SDK.ROOT_DOMAINS.eventToStep, entries(eventToStep))
    ).root,
    ...headerOverrides,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    header,
    headerHash,
    payloadEnvelopeCbor: await reencodeFixturePayload({
      ...payload,
      block_body: {
        ...body,
        event_to_step: eventToStep,
        header,
        header_hash: headerHash,
      },
    }),
  };
};

const eventToStepValue = (value: SDK.EventToStepValue) =>
  encodeData(value, SDK.EventToStepValueSchema).toString("hex");

const classify = async ({
  fixture,
  block,
  installed,
}: {
  readonly fixture: Fixture;
  readonly block: Block;
  readonly installed: boolean;
}) => {
  const seed = requireTransitionTraceL1Events(
    await captureRetainedPlutusIdentityOrigins(fixture, { omitEvent: true }),
  ).snapshot;
  const hubScope = seed.scopes.find(({ role }) => role === "hub_oracle")!;
  const hubOraclePolicyId = getAddressDetails(hubScope.address)
    .paymentCredential!.hash;
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
  const historySource = createHistoricalNativeScriptHistorySource({
    providerRoster: createHistoricalNativeScriptProviderRoster({
      deploymentFingerprint: DEPLOYMENT,
      providers: ["a", "b"].map((name) => ({
        sourceId: `archive-${name}`,
        authorityEndpoint: `https://archive-${name}.example.test`,
        operatorIdentitySha256: name.repeat(64),
      })),
    }),
  });
  const directory = mkdtempSync("/var/tmp/midgard-structural-route-");
  directories.push(directory);
  const bindingFields = {
    deploymentFingerprint: DEPLOYMENT,
    blueprintHash: releaseFinality.blueprintHash,
    network: "Preprod" as const,
    releaseFinality,
    resolvedContracts: {
      hubOraclePolicyId,
      contracts: {
        transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
      },
    },
    definition: { headerHash: block.headerHash },
  };
  const history = {
    inlineLimitBytes: 512n,
    maxPayloadBytes: 5000n,
    maxPayloadNodes: 512n,
    retentionAddress: credentialToAddress("Preprod", {
      type: "Script",
      hash: "ee".repeat(28),
    }),
  };
  const refuse = async () => {
    throw new Error("unexpected fabricated-event lookup");
  };
  const classifier = await createHeaderClassifier(
    installed
      ? {
          deploymentFingerprint: DEPLOYMENT,
          replayer: createCatalogueCompleteCanonicalReplay({
            history: { deposit: history, withdrawal: history },
            lucid: {
              utxosByOutRef: refuse,
              utxosAtWithUnit: refuse,
            } as unknown as LucidEvolution,
            network: "Preprod",
            hubOraclePolicyId,
            minimumConfirmationDepth: policy.confirmationDepth,
            owner: "b1".repeat(28),
          }),
          releaseFinalityAuthority,
          historicalReplayAuthority: {
            checkpointStore: createSqliteHistoricalNativeScriptCheckpointStore({
              path: join(directory, "history.sqlite"),
              rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
            }),
            historySource,
          },
          settlementAuthority:
            unsafeCreateCrossBlockSettlementAuthorityFromRawForTest({
              binding: bindingFields as Parameters<
                typeof unsafeCreateCrossBlockSettlementAuthorityFromRawForTest
              >[0]["binding"],
              raw,
              historySource,
            }),
          transitionTraceEventAuthority:
            unsafeCreateTransitionTraceEventAuthorityFromRawForTest({
              binding: bindingFields as Parameters<
                typeof unsafeCreateTransitionTraceEventAuthorityFromRawForTest
              >[0]["binding"],
              authority: raw,
            }),
        }
      : {
          deploymentFingerprint: DEPLOYMENT,
          replayer: NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
          releaseFinalityAuthority,
        },
  );
  // Every retained-DA read is recorded: a predecessor fetch or a historical
  // corpus walk would request a header other than the classified block.
  const requested: string[] = [];
  const sources: readonly RetainedDaPayloadSource[] = [
    {
      sourceId: "retained-fixture",
      fetchPayloadByHeaderHash: async (headerHash) => {
        requested.push(headerHash);
        const retained = [block, fixture.predecessor].find(
          (candidate) => candidate.headerHash === headerHash,
        );
        if (retained === undefined)
          throw new Error("Structural route fixture requested another header");
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
  const observation = authenticatedHeaderObservation(block);
  const decision = await classifyHeader({
    classifier,
    observation,
    sources,
    ...(installed
      ? {
          predecessorObservation: authenticatedHeaderObservation(
            fixture.predecessor,
          ),
        }
      : {}),
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.confirmationDepth,
      }),
  });
  return { decision, requested, classifier };
};

const structuralReplayDigest = (
  launchScope: readonly string[],
  decision: { detectionId: string; position: string; headerHash: string },
) =>
  digest({
    route: TRANSITION_TRACE_STRUCTURAL_ROUTE,
    launchScope,
    selected: {
      detectionId: decision.detectionId,
      headerHash: decision.headerHash,
      violationId: "transition-trace",
      position: decision.position,
      diagnostic: null,
    },
  });

// Each case builds retained evidence and, when installed, the full catalogue.
const replayBudget = { timeout: 60_000 };
describe("pre-union transition trace structural route", replayBudget, () => {
  const tampers: readonly [string, (body: Body) => Partial<Body>][] = [
    [
      "an event_to_step entry naming another step",
      (body) => ({
        event_to_step: body.event_to_step.map(([key]) => [
          key,
          eventToStepValue({ step_index: 1n, phase: "L2Transaction" }),
        ]),
      }),
    ],
    [
      "an event_to_step entry naming another phase",
      (body) => ({
        event_to_step: body.event_to_step.map(([key]) => [
          key,
          eventToStepValue({ step_index: 0n, phase: "Deposit" }),
        ]),
      }),
    ],
    [
      "an event_to_step entry re-keyed to an uncommitted event",
      (body) => ({
        event_to_step: body.event_to_step.map(([, value]) => [
          encodeData(
            { L2TransactionEventKey: { tx_id: "ab".repeat(32) } },
            SDK.EventKeySchema,
          ).toString("hex"),
          value,
        ]),
      }),
    ],
  ];

  for (const [name, tamper] of tampers) {
    it(`seals ${name} before predecessor and corpus admission`, async () => {
      const fixture = await buildRetainedPlutusIdentityFixture({
        verdict: "accepted",
      });
      const block = await recommit(fixture.block, tamper);
      const { decision, requested, classifier } = await classify({
        fixture,
        block,
        installed: true,
      });
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "transitionTrace",
        violationId: "transition-trace",
        headerHash: block.headerHash,
      });
      if (decision.decision !== "fault_detected") throw new Error("narrowed");
      expect(decision.detectionId).toMatch(
        /^transition-trace:\d+:(eventToStepMismatch|sourceMembershipMismatch)$/u,
      );
      expect(decision.replayDigest).toBe(
        structuralReplayDigest(classifier.launchScope, decision),
      );
      expect(headerDecisionReplayContext(decision)).toBeUndefined();
      expect(requested).toEqual([block.headerHash]);
    });
  }

  it("seals a header count its authenticated root disagrees with", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const { header } = fixture.block;
    // Header-valid: the source counts still sum to the total and the step
    // count still equals it, but no counted root embeds the new counts.
    const block = await recommit(fixture.block, () => ({}), {
      depositCount: header.depositCount + 1n,
      totalEventCount: header.totalEventCount + 1n,
      transitionStepCount: header.transitionStepCount + 1n,
    });
    const { decision, requested, classifier } = await classify({
      fixture,
      block,
      installed: true,
    });
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "transitionTrace",
      violationId: "transition-trace",
      detectionId: "transition-trace:0:countFault",
    });
    if (decision.decision !== "fault_detected") throw new Error("narrowed");
    expect(decision.replayDigest).toBe(
      structuralReplayDigest(classifier.launchScope, decision),
    );
    expect(requested).toEqual([block.headerHash]);
  });

  it("leaves an uninstalled scope to its installed families", async () => {
    // The route never replaces an installed family's result: networkId has
    // nothing to prove on this block, so the union's verdict stands.
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const block = await recommit(fixture.block, tampers[0]![1]);
    const { decision } = await classify({ fixture, block, installed: false });
    expect(decision).toMatchObject({
      decision: "healthy",
      launchScope: ["networkId"],
    });
  });

  it("leaves a consistent block to the replay union", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const installed = await classify({
      fixture,
      block: fixture.block,
      installed: true,
    });
    expect(installed.decision.decision).toBe("healthy");
    expect(
      headerDecisionReplayContext(installed.decision)?.historicalCorpus,
    ).toBeDefined();
    const uninstalled = await classify({
      fixture,
      block: fixture.block,
      installed: false,
    });
    expect(uninstalled.decision.decision).toBe("healthy");
  });
});

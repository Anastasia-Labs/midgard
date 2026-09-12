import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  computeFraudProofRawL1RollbackCursor,
  createHeaderClassifier,
  createTransitionTraceEventAuthority,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  buildRetainedPlutusIdentityFixture,
  buildRetainedPlutusUnboundVariableFixture,
  captureRetainedPlutusIdentityOrigins,
} from "@al-ft/midgard-fault-proofs/test-support/retained-reason-classifier";
import { requireTransitionTraceL1Events } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  getAddressDetails,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it, vi } from "vitest";

import {
  assertWatcherValidationReplayCaptureCurrent,
  captureWatcherValidationReplayTranscript,
  refreshWatcherValidationReplayCapture,
} from "../../src/fault-proofs/replay-transcript-capture.js";
import {
  admitWatcherUserEventOrigin,
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginFacts,
} from "../../src/indexers/user-event-origin.js";
import { makeWatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  createWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  type WatcherDurableAtomicBackend,
  watcherSameCanonicalJson,
} from "../../src/storage/durable-store.js";
import { watcherUserEventArchiveDigest } from "../../src/storage/user-event-checkpoint.js";
import { watcherAuthenticatedReplayTranscriptCborHex } from "../../src/verification/authenticated-replay-transcript.js";
import { readWatcherReplayTranscriptRecords } from "../../src/verification/replay-transcript-records.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import {
  createSyntheticStateQueueObservationFixture,
  type SyntheticStateQueueObservationCapture,
} from "../support/state-queue-observation-fixture.js";
import { genuineUserEventForcedPayloadForCanonicalTx } from "../support/user-event-authority-scenarios.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

const legacyTransport = vi.hoisted(() => ({ raw: undefined as unknown }));
// The existing retained-classifier raw-port seam supplies its legacy originating
// event snapshot. Its authority/classifier factories stay real. SQ observations,
// the new user-event service, its local capabilities and replay capture are never
// mocked; those independently admit the same order/native bytes below.
vi.mock(
  "@al-ft/midgard-fault-proofs/test-support/family-l1-observation",
  async (load) => {
    const actual =
      await load<
        typeof import("@al-ft/midgard-fault-proofs/test-support/family-l1-observation")
      >();
    return {
      ...actual,
      createFraudProofFamilyLocalKupmiosL1ObservationPort: () => ({
        rawL1: legacyTransport.raw,
      }),
    };
  },
);

const h32 = (byte: string) => byte.repeat(32);
const sha256 = (bytes: Uint8Array) =>
  createHash("sha256").update(bytes).digest("hex");
type Retained = Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>;
const transactionInput = (outRef: string) => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};

/** Ordinary forced-order mint shape from user-event-history.test.ts, with the
 * existing retained fixture's nonce/order id and exact native payload. */
const ordinaryForcedOrder = (
  facts: WatcherUserEventOriginFacts,
  retained: Retained,
) => {
  const eventId = retained.orderKey;
  const idCbor = SDK.outputReferenceToPlutusDataCbor({
    txHash: eventId.transactionId,
    outputIndex: Number(eventId.outputIndex),
  });
  const assetName = Buffer.from(
    blake2b(Buffer.from(idCbor, "hex"), { dkLen: 32 }),
  ).toString("hex");
  const witness = SDK.userEventWitnessScriptHash(assetName);
  const scripts = facts.scripts.forcedOrder;
  const payload = genuineUserEventForcedPayloadForCanonicalTx(
    encodeMidgardForcedTxCanonical(
      decodeMidgardNativeTxFullFromCanonicalCbor(
        retained.transaction.canonicalCbor,
      ),
    ),
  );
  const datum = Data.to(
    {
      inclusion_time: BigInt(
        SDK.resolveEventInclusionTime(
          slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
          "Preprod",
        ),
      ),
      witness,
      refund_address: {
        paymentCredential: { PublicKeyCredential: ["88".repeat(28)] },
        stakeCredential: null,
      },
      refund_datum: "NoDatum",
      event: {
        id: eventId,
        tx: {
          tx_id: payload.tx_id,
          transaction_commitment: payload.transaction_commitment,
          submitted_source: payload.submitted_source,
        },
      },
    },
    SDK.TxOrderDatum,
  );
  const policy = CML.ScriptHash.from_hex(scripts.policyId);
  const assets = CML.MultiAsset.new();
  assets.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(scripts.addressHex),
      CML.Value.new(3_000_000n, assets),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    transactionInput(`${eventId.transactionId}#${eventId.outputIndex}`),
  );
  const references = CML.TransactionInputList.new();
  references.add(transactionInput(facts.activation.hubOutRef));
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_reg_cert(
      CML.Credential.new_script(CML.ScriptHash.from_hex(witness)),
      0n,
    ),
  );
  const mint = CML.Mint.new();
  mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(references);
  body.set_certs(certificates);
  body.set_mint(mint);
  body.set_ttl(1_000n);
  const material = payload.carriage.map((entry) => {
    if (
      typeof entry !== "object" ||
      entry === null ||
      !("Inline" in entry) ||
      typeof entry.Inline !== "object" ||
      entry.Inline === null ||
      !("preimage" in entry.Inline) ||
      typeof entry.Inline.preimage !== "string"
    )
      throw new Error("Ordinary forced fixture requires inline carriage");
    return { Inline: { preimage: entry.Inline.preimage } };
  });
  const witnessSet = CML.TransactionWitnessSet.new();
  const redeemers = CML.LegacyRedeemerList.new();
  const mintRedeemer = Data.to(
    {
      event: {
        AuthenticateEvent: {
          nonce_input_index: 0n,
          event_output_index: 0n,
          hub_ref_input_index: 0n,
          witness_registration_redeemer_index: 1n,
        },
      },
      material_carriage: material,
    },
    SDK.TxOrderMintRedeemer,
  );
  const certRedeemer = Data.to(
    { MintOrBurn: { targetPolicy: scripts.policyId } },
    SDK.UserEventWitnessPublishRedeemer,
  );
  for (const [tag, cbor] of [
    [CML.RedeemerTag.Mint, mintRedeemer],
    [CML.RedeemerTag.Cert, certRedeemer],
  ] as const)
    redeemers.add(
      CML.LegacyRedeemer.new(
        tag,
        0n,
        CML.PlutusData.from_cbor_hex(cbor),
        CML.ExUnits.new(0n, 0n),
      ),
    );
  witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  body.set_script_data_hash(
    CML.ScriptDataHash.from_raw_bytes(Buffer.alloc(32, 0x6a)),
  );
  return CML.Transaction.new(body, witnessSet, true).to_canonical_cbor_hex();
};

const setup = async (kind: "normal" | "forced") => {
  const retained =
    kind === "normal"
      ? await buildRetainedPlutusUnboundVariableFixture({ verdict: "accepted" })
      : await buildRetainedPlutusIdentityFixture(
          {
            verdict: "rejected",
            reason: { PlutusExecutionFailed: { execution_index: 0n } },
          },
          { sourceKind: "forced" },
        );
  const construction = await createSyntheticUserEventOriginFixture();
  const identity = construction.deploymentIdentity;
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: identity.manifestId,
      network: identity.network,
      blueprintHash: identity.blueprintHash,
      programCommitments: identity.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  await construction.close();
  const queue = await createSyntheticStateQueueObservationFixture({
    header: retained.block.header,
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
    ...(kind === "normal"
      ? {}
      : {
          composeCommitBlock: async ({
            transport,
            commitTransactionCbor,
          }: Parameters<
            NonNullable<
              NonNullable<
                Parameters<
                  typeof createSyntheticStateQueueObservationFixture
                >[0]
              >["composeCommitBlock"]
            >
          >[0]) => {
            const pair = await transport.openFinalizedBlock(
              transport.activationBlock,
            );
            try {
              const input = {
                deploymentIdentity: transport.deploymentIdentity,
                scriptBinding: transport.scriptBinding,
                finality: pair.finality,
                observation: pair.observation,
              };
              const origin = admitWatcherUserEventOrigin(input);
              const facts = readWatcherUserEventOrigin({ ...input, origin });
              return {
                transactions: [
                  ordinaryForcedOrder(facts, retained),
                  commitTransactionCbor,
                ],
                creatingBodies: [transport.initializationBodyCbor],
              };
            } finally {
              await pair.close();
            }
          },
        }),
  });
  const fixture = queue.transport;
  const directory = await mkdtemp("/var/tmp/replay-capture-release-");
  const authorityPath = join(directory, "authority.json");
  const ruleBundlePath = join(directory, "rules.json");
  await writeFile(
    authorityPath,
    JSON.stringify({
      signedIdentity: fixture.deployment.signedIdentity,
      policy: fixture.deployment.policy,
      trustRoots: fixture.deployment.trustRoots,
      durableMarker: fixture.deployment.marker,
    }),
  );
  await writeFile(ruleBundlePath, JSON.stringify(ruleBundle));
  const loadAuthority = () =>
    loadWatcherVerifiedDeploymentAuthority({
      path: authorityPath,
      ruleBundlePath,
    });
  const deploymentAuthority = await loadAuthority();
  let bytes: Uint8Array | null = null;
  let currentHead: WatcherRollbackDurableTrustedHead | null = null;
  const backend: WatcherDurableAtomicBackend = {
    read: async () => (bytes === null ? null : Uint8Array.from(bytes)),
    compareAndSwap: async (expected, next) => {
      if ((bytes === null ? null : sha256(bytes)) !== expected) return false;
      bytes = Uint8Array.from(next);
      return true;
    },
  };
  const client: WatcherTrustedHeadAuthorityClient = {
    readRecordAuthenticationKeyId: async () => h32("99"),
    readCurrent: async () => currentHead,
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      if (!watcherSameCanonicalJson(expectedTrustedHead, currentHead))
        return false;
      currentHead = nextTrustedHead;
      return true;
    },
  };
  const objects = new Map<string, Uint8Array>();
  const archive = {
    put: async (value: Uint8Array) => {
      const digest = watcherUserEventArchiveDigest(value);
      objects.set(digest, Uint8Array.from(value));
      return digest;
    },
    read: async (digest: string) => {
      const value = objects.get(digest);
      return value === undefined ? null : Uint8Array.from(value);
    },
  };
  const durableInput = {
    backend,
    policy: makeWatcherFinalityPolicy(
      fixture.watcherConfig,
      deploymentAuthority.deploymentIdentity,
    )!,
    authenticationKey: Uint8Array.from({ length: 32 }, (_, index) => index + 1),
    client,
    userEventArchive: archive,
  };
  const runtime = await createWatcherDurableRuntime(durableInput);
  const blueprintBytes = await readFile(
    process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
      fileURLToPath(
        new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
      ),
  );
  const input = {
    watcherConfig: fixture.watcherConfig,
    deploymentAuthority,
    blueprintBytes,
    nativeChainSyncBinaryPath: fixture.nativeChainSyncBinaryPath,
    runtime,
    archive,
  };
  return {
    retained,
    queue,
    fixture,
    input,
    durableInput,
    loadAuthority,
    close: async () => {
      await queue.close();
      await rm(directory, { recursive: true, force: true });
      legacyTransport.raw = undefined;
    },
  };
};

const classify = async (
  context: Awaited<ReturnType<typeof setup>>,
  captured: SyntheticStateQueueObservationCapture,
  deploymentAuthority = context.input.deploymentAuthority,
) => {
  const observation = authenticatedHeaderObservation(context.retained.block, {
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: captured.observation.sourceId,
      grade: "security",
    },
    chainPoint: {
      blockHash: captured.header.observedBlockHash,
      slot: BigInt(captured.header.observedSlot),
    },
    confirmationDepth: Number(captured.header.finalityDepth),
  });
  const releaseFinalityAuthority = watcherDeploymentReleaseFinalityAuthority(
    deploymentAuthority.deploymentIdentity,
  );
  let transitionTraceEventAuthority;
  if (context.retained.block.header.forcedTransactionCount > 0n) {
    const seedHandle = await captureRetainedPlutusIdentityOrigins(
      context.retained,
    );
    const seed = requireTransitionTraceL1Events(seedHandle).snapshot;
    const hub = seed.scopes.find(({ role }) => role === "hub_oracle")!;
    const raw: FraudProofRawL1SnapshotAuthority = {
      authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
      capture: async (request) => ({
        ...seed,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        blueprintHash: request.blueprintHash,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        cursor: {
          ...seed.cursor,
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            ...request,
            sourceId: seed.provenance.sourceId,
            pointId: seed.cursor.point.pointId,
          }),
        },
        scopes: request.scopes.map((scope) => ({
          ...scope,
          utxos:
            seed.scopes.find(({ address }) => address === scope.address)
              ?.utxos ?? [],
        })),
        historyUnits: request.historyUnits,
        history: request.historyUnits.map((unit) => {
          const history = seed.history.find((entry) => entry.unit === unit);
          if (history === undefined)
            throw new Error(
              "Ordinary retained fixture cannot supply another unit",
            );
          return history;
        }),
      }),
    };
    legacyTransport.raw = raw;
    const releaseFinality = await releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: deploymentAuthority.deploymentIdentity.manifestId,
    });
    // Only configuration fields consumed by the existing raw-port fixture are
    // supplied here. This is not a claimed deployed workflow binding.
    const binding = {
      deploymentFingerprint: deploymentAuthority.deploymentIdentity.manifestId,
      blueprintHash: deploymentAuthority.deploymentIdentity.blueprintHash,
      network: "Preprod",
      releaseFinality,
      resolvedContracts: {
        hubOraclePolicyId: getAddressDetails(hub.address).paymentCredential!
          .hash,
      },
      definition: { headerHash: context.retained.block.headerHash },
    } as Parameters<typeof createTransitionTraceEventAuthority>[0]["binding"];
    transitionTraceEventAuthority = createTransitionTraceEventAuthority({
      binding,
      source: {} as never,
    });
  }
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentAuthority.deploymentIdentity.manifestId,
    replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority,
    ...(transitionTraceEventAuthority === undefined
      ? {}
      : { transitionTraceEventAuthority }),
  });
  const sources = [
    {
      sourceId: "ordinary-retained-capture",
      fetchPayloadByHeaderHash: async (headerHash: string) => {
        const block = [
          context.retained.block,
          context.retained.predecessor,
        ].find((entry) => entry.headerHash === headerHash);
        if (block === undefined)
          throw new Error("Ordinary retained fixture requested another header");
        return {
          ok: true as const,
          sourceId: "ordinary-retained-capture",
          sourcePeerId: "unit",
          attempts: [],
          payloadEnvelopeCbor: block.payloadEnvelopeCbor,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "ordinary-retained-capture/unit",
            grade: "security" as const,
          },
        };
      },
    },
  ];
  const decision = await classifyHeader({
    classifier,
    observation,
    sources,
    predecessorObservation: authenticatedHeaderObservation(
      context.retained.predecessor,
    ),
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
  });
  expect(decision).toMatchObject({
    decision: "fault_detected",
    category: "validationTraceDispute",
    headerHash: captured.header.headerHash,
  });
  return decision;
};

// Retained predecessor/classifier-origin context remains the existing ordinary
// classifier fixture. The selected header is independently admitted by the real
// SQ source, and local event history is independently acquired by the real service.
describe("validation transcript capture with an owned user-event runtime", () => {
  it.each(["normal", "forced"] as const)(
    "captures and cold re-admits the existing %s retained Plutus decision",
    async (kind) => {
      const context = await setup(kind);
      let service: WatcherUserEventRuntime | undefined;
      try {
        service = await createWatcherUserEventRuntime(context.input);
        await service.advanceThrough(context.queue.commitBlock.point);
        const observed = await context.queue.observeFresh();
        const decision = await classify(context, observed);
        const input = {
          deploymentAuthority: context.input.deploymentAuthority,
          stateQueueObservation: observed.observation,
          header: observed.header,
          decision,
          userEventRuntime: service,
        };
        const capture = await captureWatcherValidationReplayTranscript(input);
        expect(capture.decisionDigest).toBe(decision.decisionDigest);
        expect(capture.transcript.eventAuthorityRecordsCborHex).toHaveLength(
          kind === "forced" ? 1 : 0,
        );
        const persisted = watcherAuthenticatedReplayTranscriptCborHex(
          capture.transcript,
        );
        const records = await readWatcherReplayTranscriptRecords(persisted, 30);
        if (kind === "forced") {
          expect(records.events[0]!.origin).toMatchObject({
            source: "local_publication",
            throughHeader: {
              headerHash: observed.header.headerHash,
              transactionIndex: "1",
            },
          });
        }
        await refreshWatcherValidationReplayCapture(capture);
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(capture),
        ).not.toThrow();
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent({ ...capture }),
        ).toThrow();
        await expect(
          captureWatcherValidationReplayTranscript({
            ...input,
            userEventRuntime: { ...service },
          }),
        ).rejects.toThrow();
        await expect(
          captureWatcherValidationReplayTranscript({
            ...input,
            header: { ...observed.header },
          }),
        ).rejects.toThrow();
        await expect(
          captureWatcherValidationReplayTranscript({
            ...input,
            decision: { ...decision },
          }),
        ).rejects.toThrow();
        const rollback = service.handleRollback({
          kind: "point",
          blockHash: context.queue.commitBlock.point.blockHash,
          slot: context.queue.commitBlock.point.slot,
        });
        expect(service.read().status).toBe("suspended");
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(capture),
        ).toThrow();
        await expect(
          refreshWatcherValidationReplayCapture(capture),
        ).rejects.toThrow();
        await rollback;
        expect(service.read().status).toBe("ready");
        // The runtime's native generation fence keeps even zero-event captures
        // retired after the same service resumes its protected head.
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(capture),
        ).toThrow();
        await service.close();
        await expect(
          refreshWatcherValidationReplayCapture(capture),
        ).rejects.toThrow();
        await observed.close();
        const deploymentAuthority = await context.loadAuthority();
        const runtime = await createWatcherDurableRuntime(context.durableInput);
        service = await createWatcherUserEventRuntime({
          ...context.input,
          runtime,
          deploymentAuthority,
        });
        const fresh = await context.queue.observeFresh();
        const renewedDecision = await classify(
          context,
          fresh,
          deploymentAuthority,
        );
        const renewed = await captureWatcherValidationReplayTranscript({
          deploymentAuthority,
          stateQueueObservation: fresh.observation,
          header: fresh.header,
          decision: renewedDecision,
          userEventRuntime: service,
          persistedTranscriptCborHex: persisted,
        });
        expect(renewed.transcript.coordinate).toEqual(
          capture.transcript.coordinate,
        );
        expect(renewed.transcript.headerHash).toBe(
          capture.transcript.headerHash,
        );
        await refreshWatcherValidationReplayCapture(renewed);
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(renewed),
        ).not.toThrow();
        await service.close();
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(renewed),
        ).toThrow();
      } finally {
        await service?.close();
        await context.close();
      }
    },
    180_000,
  );
});

import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { compareOutRefs, parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  AddressData,
  DepositDatum,
  DepositEvent,
  DepositInfo,
  DepositSpendRedeemer,
  ForcedInclusionTxV1,
  HubOracleDatum,
  MerkleRoot,
  outputReferenceToPlutusDataCbor,
  Proof,
  resolveEventInclusionTime,
  RootDomain,
  SettlementDatum,
  TxOrderDatum,
  TxOrderMintRedeemer,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  WithdrawalEvent,
  WithdrawalInfo,
  WithdrawalOrderDatum,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it, vi } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import {
  acceptWatcherLocalUserEventPublication,
  assertWatcherLocalUserEventAuthorityCurrent,
  createWatcherLocalUserEventHistory,
  prepareWatcherLocalUserEventTransition,
  readWatcherLocalUserEventAuthority,
  readWatcherLocalUserEventHistory,
  readWatcherLocalUserEventTransition,
  type WatcherLocalUserEventEntry,
  type WatcherLocalUserEventHeaderCutoff,
  type WatcherUserEventObservation,
  type WatcherUserEventSnapshot,
} from "../../src/indexers/user-event-indexer.js";
import {
  admitWatcherUserEventOrigin,
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginFacts,
} from "../../src/indexers/user-event-origin.js";
import type { WatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import {
  makeWatcherFinalityBootstrapState,
  readWatcherLocalBackfillFinality,
} from "../../src/l1/finality-engine.js";
import {
  initializeWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  assertWatcherUserEventRuntime,
  createWatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import type {
  WatcherDurableAtomicBackend,
  WatcherDurableStore,
} from "../../src/storage/durable-store.js";
import {
  makeEmptyWatcherDurableStore,
  makeWatcherDurableStore,
  watcherCanonicalJson,
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../../src/storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  watcherUserEventArchiveDigest,
} from "../../src/storage/user-event-checkpoint.js";
import {
  assertWatcherAuthenticatedReplayTranscript,
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
  watcherReplayRawRecordCborHex,
} from "../../src/verification/authenticated-replay-transcript.js";
import {
  evaluateWatcherBlockReplay,
  readWatcherBlockReplayEventAuthorityRecords,
  watcherBlockReplayDownstreamInputDigest,
  watcherBlockReplayEventAuthorityManifest,
} from "../../src/verification/block-replay.js";
import type { WatcherCommittedEventClaim } from "../../src/verification/event-claims.js";
import { deriveWatcherLocalEventReplayAuthority } from "../../src/verification/local-event-replay-authority.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import { readWatcherReplayTranscriptRecords } from "../../src/verification/replay-transcript-records.js";
import type { WatcherRuleBundle } from "../../src/verification/rule-bundle.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import { genuineUserEventForcedPayloadForCanonicalTx } from "../support/user-event-authority-scenarios.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticUserEventBlock,
} from "../support/user-event-origin-fixture.js";

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const h32 = (byte: string): string => byte.repeat(32);
const addressHex = (address: AddressData): string => {
  const credential = address.paymentCredential;
  if (address.stakeCredential !== null || !("ScriptCredential" in credential))
    throw new Error("fixture needs an enterprise script address");
  return `70${credential.ScriptCredential[0]}`;
};
const transactionInput = (outRef: string) => {
  const [transactionId, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(transactionId!),
    BigInt(index!),
  );
};
const ledgerReferenceIndex = (
  inputs: CML.TransactionInputList,
  outRef: string,
): bigint => {
  const ordered = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  const target = parseOutRefLabel(outRef);
  const index = ordered.findIndex(
    (input) => compareOutRefs(input, target) === 0,
  );
  if (index < 0) throw new Error("Missing fixture reference input");
  return BigInt(index);
};
const transaction = (
  body: CML.TransactionBody,
  values: readonly Readonly<{
    tag: CML.RedeemerTag;
    index: bigint;
    cbor: string;
  }>[],
  preserveDataEncoding = false,
): string => {
  const witness = CML.TransactionWitnessSet.new();
  const redeemers = CML.LegacyRedeemerList.new();
  for (const value of values)
    redeemers.add(
      CML.LegacyRedeemer.new(
        value.tag,
        value.index,
        CML.PlutusData.from_cbor_hex(value.cbor),
        CML.ExUnits.new(0n, 0n),
      ),
    );
  if (values.length > 0) {
    witness.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(Buffer.alloc(32, 0x6a)),
    );
  }
  const complete = CML.Transaction.new(body, witness, true, undefined);
  return preserveDataEncoding
    ? complete.to_cbor_hex()
    : complete.to_canonical_cbor_hex();
};

/** Ordinary semantic unit transaction bytes for a synthetic local block.
 * These do not claim ledger acceptance or public-chain transaction inclusion.
 * The checks mirror the existing deposit lifecycle fixture with this deployment's
 * real hub datum and parameter-derived scripts.
 */
const depositLifecycle = (
  facts: WatcherUserEventOriginFacts,
  preserveDataEncoding = false,
) => {
  const hub = Data.from(facts.activation.hubDatumCbor, HubOracleDatum);
  const nonceOutRef = `${h32("b2")}#0`;
  const eventId = { transactionId: h32("b2"), outputIndex: 0n };
  const assetName = Buffer.from(
    blake2b(
      Buffer.from(
        outputReferenceToPlutusDataCbor({
          txHash: eventId.transactionId,
          outputIndex: 0,
        }),
        "hex",
      ),
      { dkLen: 32 },
    ),
  ).toString("hex");
  const witnessHash = userEventWitnessScriptHash(assetName);
  const l2Address: AddressData = {
    paymentCredential: { PublicKeyCredential: ["88".repeat(28)] },
    stakeCredential: null,
  };
  const event = {
    id: eventId,
    info: { l2_address: l2Address, l2_network_id: 0n, l2_datum: null },
  };
  const datum = Data.to(
    {
      event,
      inclusion_time: BigInt(
        resolveEventInclusionTime(
          slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
          "Preprod",
        ),
      ),
      witness: witnessHash,
    },
    DepositDatum,
  );
  const mint = CML.Mint.new();
  const policy = CML.ScriptHash.from_hex(facts.scripts.deposit.policyId);
  mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const assets = CML.MultiAsset.new();
  assets.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(facts.scripts.deposit.addressHex),
      CML.Value.new(3_000_000n, assets),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(nonceOutRef));
  const refs = CML.TransactionInputList.new();
  const createReferences = preserveDataEncoding
    ? [
        facts.activation.hubOutRef,
        `${facts.activation.transactionId}#${facts.activation.hubOutputIndex === 0 ? 1 : 0}`,
      ].sort((left, right) =>
        compareOutRefs(parseOutRefLabel(right), parseOutRefLabel(left)),
      )
    : [facts.activation.hubOutRef];
  for (const outRef of createReferences) refs.add(transactionInput(outRef));
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_reg_cert(
      CML.Credential.new_script(CML.ScriptHash.from_hex(witnessHash)),
      0n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_mint(mint);
  body.set_certs(certificates);
  body.set_reference_inputs(refs);
  body.set_ttl(1_000n);
  const create = transaction(
    body,
    [
      {
        tag: CML.RedeemerTag.Mint,
        index: 0n,
        cbor: Data.to(
          {
            AuthenticateEvent: {
              nonce_input_index: 0n,
              event_output_index: 0n,
              hub_ref_input_index: ledgerReferenceIndex(
                refs,
                facts.activation.hubOutRef,
              ),
              witness_registration_redeemer_index: 1n,
            },
          },
          UserEventMintRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Cert,
        index: 0n,
        cbor: Data.to(
          { MintOrBurn: { targetPolicy: facts.scripts.deposit.policyId } },
          UserEventWitnessPublishRedeemer,
        ),
      },
    ],
    preserveDataEncoding,
  );
  const createId = CML.hash_transaction(
    CML.Transaction.from_cbor_hex(create).body(),
  ).to_hex();
  const phasRoot = h32("a4");
  const root = Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardRootCountV1"),
        Buffer.from(Data.to("DepositsRootDomain", RootDomain), "hex"),
        Buffer.from(phasRoot, "hex"),
        Buffer.from(Data.to(1n), "hex"),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");
  const settlementAssets = CML.MultiAsset.new();
  settlementAssets.set(
    CML.ScriptHash.from_hex(hub.settlement),
    CML.AssetName.from_hex(""),
    1n,
  );
  const settlementOutputs = CML.TransactionOutputList.new();
  settlementOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(addressHex(hub.settlement_addr)),
      CML.Value.new(5_000_000n, settlementAssets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              deposits_root: root,
              withdrawals_root: h32("a5"),
              forced_transactions_root: h32("a6"),
              transactions_root: h32("a7"),
              resolution_claim: null,
            },
            SettlementDatum,
          ),
        ),
      ),
    ),
  );
  const settlementInputs = CML.TransactionInputList.new();
  settlementInputs.add(transactionInput(`${h32("f0")}#0`));
  const settlementBody = CML.TransactionBody.new(
    settlementInputs,
    settlementOutputs,
    200_000n,
  ).to_cbor_hex();
  const settlementId = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(settlementBody),
  ).to_hex();
  const eventFields = CML.PlutusData.from_cbor_hex(Data.to(event, DepositEvent))
    .as_constr_plutus_data()!
    .fields();
  const membership = {
    domain: "DepositsRootDomain" as const,
    root,
    phas_root: phasRoot,
    count: 1n,
    key: eventFields.get(0).to_cbor_hex(),
    value: eventFields.get(1).to_cbor_hex(),
    proof: [],
  };
  const consumeInputs = CML.TransactionInputList.new();
  consumeInputs.add(transactionInput(`${createId}#0`));
  const consumeOutputs = CML.TransactionOutputList.new();
  consumeOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(addressHex(hub.reserve_addr)),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const burn = CML.Mint.new();
  burn.set(policy, CML.AssetName.from_hex(assetName), -1n);
  const unregister = CML.CertificateList.new();
  unregister.add(
    CML.Certificate.new_unreg_cert(
      CML.Credential.new_script(CML.ScriptHash.from_hex(witnessHash)),
      0n,
    ),
  );
  const consumeRefs = CML.TransactionInputList.new();
  consumeRefs.add(transactionInput(facts.activation.hubOutRef));
  consumeRefs.add(transactionInput(`${settlementId}#0`));
  const consumeBody = CML.TransactionBody.new(
    consumeInputs,
    consumeOutputs,
    200_000n,
  );
  consumeBody.set_mint(burn);
  consumeBody.set_certs(unregister);
  consumeBody.set_reference_inputs(consumeRefs);
  const membershipItems = CML.PlutusDataList.new();
  membershipItems.add(
    CML.PlutusData.from_cbor_hex(Data.to(phasRoot, MerkleRoot)),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(membership.key, "hex")),
  );
  membershipItems.add(
    CML.PlutusData.new_bytes(Buffer.from(membership.value, "hex")),
  );
  membershipItems.add(CML.PlutusData.from_cbor_hex(Data.to([], Proof)));
  const consume = transaction(consumeBody, [
    {
      tag: CML.RedeemerTag.Spend,
      index: 0n,
      cbor: Data.to(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: ledgerReferenceIndex(
            consumeRefs,
            facts.activation.hubOutRef,
          ),
          settlement_ref_input_index: ledgerReferenceIndex(
            consumeRefs,
            `${settlementId}#0`,
          ),
          mint_redeemer_index: 1n,
          membership_proof: membership,
          inclusion_proof_script_withdraw_redeemer_index: 3n,
        },
        DepositSpendRedeemer,
      ),
    },
    {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
      cbor: Data.to(
        {
          BurnEventNFT: {
            nonce_asset_name: assetName,
            witness_unregistration_redeemer_index: 2n,
          },
        },
        UserEventMintRedeemer,
      ),
    },
    {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
      cbor: Data.to(
        { MintOrBurn: { targetPolicy: facts.scripts.deposit.policyId } },
        UserEventWitnessPublishRedeemer,
      ),
    },
    {
      tag: CML.RedeemerTag.Reward,
      index: 0n,
      cbor: CML.PlutusData.new_list(membershipItems).to_cbor_hex(),
    },
  ]);
  return Object.freeze({
    create,
    consume,
    createId,
    settlementBody,
    expectedEventId: outputReferenceToPlutusDataCbor({
      txHash: eventId.transactionId,
      outputIndex: 0,
    }),
  });
};

const durableFixture = async (
  policy: WatcherFinalityPolicy,
  bootstrapStore?: WatcherDurableStore,
) => {
  let bytes: Uint8Array | null = null;
  let currentHead: WatcherRollbackDurableTrustedHead | null = null;
  let failAfterCas = false;
  let failNextRead = false;
  let beforePut: (() => Promise<void>) | null = null;
  let casCount = 0;
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
    readCurrent: async () => {
      if (failNextRead) {
        failNextRead = false;
        throw new Error("fixture read-back interruption");
      }
      return currentHead;
    },
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      if (!watcherSameCanonicalJson(expectedTrustedHead, currentHead))
        return false;
      currentHead = nextTrustedHead;
      casCount += 1;
      if (failAfterCas) {
        failAfterCas = false;
        failNextRead = true;
      }
      return true;
    },
  };
  const objects = new Map<string, Uint8Array>();
  const archive = {
    put: async (value: Uint8Array) => {
      await beforePut?.();
      const digest = watcherUserEventArchiveDigest(value);
      objects.set(digest, Uint8Array.from(value));
      return digest;
    },
    read: async (digest: string) => {
      const value = objects.get(digest);
      return value === undefined ? null : Uint8Array.from(value);
    },
  };
  const runtimeInput = {
    backend,
    policy,
    authenticationKey: Uint8Array.from({ length: 32 }, (_, index) => index + 1),
    client,
    userEventArchive: archive,
  };
  if (bootstrapStore !== undefined)
    await initializeWatcherRollbackDurableAuthority({
      backend,
      policy,
      authenticationKey: runtimeInput.authenticationKey,
      trustedHead: null,
      bootstrapStore,
      bootstrapFinalityState: makeWatcherFinalityBootstrapState(policy)!,
    });
  const runtime = await createWatcherDurableRuntime(runtimeInput);
  return {
    runtime,
    runtimeInput,
    archive,
    objects,
    casCount: () => casCount,
    interruptNextReadBack: () => {
      failAfterCas = true;
    },
    setBeforePut: (callback: (() => Promise<void>) | null) => {
      beforePut = callback;
    },
  };
};

const openOrigin = async (
  fixture: Awaited<ReturnType<typeof createSyntheticUserEventOriginFixture>>,
) => {
  const pair = await fixture.openFinalizedBlock(fixture.activationBlock);
  const input = {
    deploymentIdentity: fixture.deploymentIdentity,
    scriptBinding: fixture.scriptBinding,
    finality: pair.finality,
    observation: pair.observation,
  };
  const origin = admitWatcherUserEventOrigin(input);
  const facts = readWatcherUserEventOrigin({ ...input, origin });
  return { pair, input, origin, facts };
};

describe("bounded local user-event semantic publication (synthetic local blocks)", () => {
  it.each([false, true])(
    "publishes the whole activation block, empty successor and ordered same-block deposit lifecycle only after archive/CAS (original data encoding: %s)",
    async (preserveDataEncoding) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      try {
        const { pair, input, origin, facts } = await openOrigin(fixture);
        const globalStore = makeWatcherDurableStore({
          deploymentMarker: fixture.deploymentIdentity.durableMarker,
          revision: "0",
          records: {
            ...makeEmptyWatcherDurableStore(
              fixture.deploymentIdentity.durableMarker,
            ),
            chainPoints: [
              {
                chainPointId: facts.block.chainPoint.chainPointId,
                providerId: facts.block.provider.providerId,
                blockHash: facts.block.chainPoint.blockHash,
                slot: facts.block.chainPoint.slot,
                blockNo: facts.block.chainPoint.blockNo,
                depth: facts.block.chainPoint.depth,
              },
            ],
          },
        });
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(pair.finality).policy,
          globalStore,
        );
        const publisher = await createWatcherLocalUserEventPublisher({
          ...input,
          origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        const bootstrap = publisher.read();
        expect(bootstrap.store.revision).toBe("0");
        expect(bootstrap.store.l1Observations).toEqual([]);
        expect(Reflect.set(bootstrap.store, "revision", "99")).toBe(false);
        expect(
          Reflect.set(
            bootstrap.store.deploymentMarker,
            "manifestId",
            h32("ff"),
          ),
        ).toBe(false);
        expect(Reflect.set(bootstrap.store.chainPoints, "0", {})).toBe(false);
        expect(
          Reflect.set(bootstrap.policy.deposit, "policyId", "ff".repeat(28)),
        ).toBe(false);
        let releaseWrite!: () => void;
        let enteredWrite!: () => void;
        const released = new Promise<void>((resolve) => {
          releaseWrite = resolve;
        });
        const entered = new Promise<void>((resolve) => {
          enteredWrite = resolve;
        });
        durable.setBeforePut(async () => {
          durable.setBeforePut(null);
          enteredWrite();
          await released;
        });
        const publishing = publisher.publish(pair);
        await entered;
        expect(publisher.read()).toMatchObject({
          cursor: null,
          retainedEntries: 0,
          status: "publication_pending",
          store: { revision: "0" },
        });
        await expect(publisher.publish(pair)).rejects.toThrow(
          "already in flight",
        );
        releaseWrite();
        const accepted = await publishing;
        expect(accepted.cursor).toEqual(fixture.activationBlock.point);
        const activated = publisher.read();
        expect(activated.store.revision).toBe("1");
        expect(activated.checkpoint?.checkpointSequence).toBe("0");
        expect(activated.store.l1Observations).toHaveLength(1);
        const recordedBlock = JSON.parse(
          Buffer.from(
            activated.store.l1Observations[0]!.payload.cborHex,
            "hex",
          ).toString("utf8"),
        ) as { transactions: readonly unknown[] };
        expect(recordedBlock.transactions).toHaveLength(
          facts.block.transactions.length,
        );
        expect(
          Reflect.set(
            activated.store.l1Observations[0]!.payload,
            "cborHex",
            "00",
          ),
        ).toBe(false);
        const countAfterActivation = durable.casCount();
        expect(await publisher.publish(pair)).toEqual(accepted);
        expect(durable.casCount()).toBe(countAfterActivation);
        expect(publisher.read().store.revision).toBe("1");
        const archivedEvidence = [...durable.objects.values()]
          .map(
            (bytes) =>
              JSON.parse(Buffer.from(bytes).toString("utf8")) as Record<
                string,
                unknown
              >,
          )
          .find(
            (value) =>
              value.schemaVersion ===
              "midgard-watcher-local-user-event-block-evidence-v1",
          );
        expect(archivedEvidence?.numericEncoding).toBe("exact-decimal-strings");
        const archivedWitnesses = archivedEvidence!.witnesses as {
          first: { finality: { startedAtMonotonicMs: string } };
          current: { finality: { admittedAtMonotonicMs: string } };
        };
        expect(
          Number(archivedWitnesses.first.finality.startedAtMonotonicMs),
        ).toBe(facts.originalWitness.first.finality.startedAtMonotonicMs);
        expect(
          Number(archivedWitnesses.current.finality.admittedAtMonotonicMs),
        ).toBe(facts.originalWitness.current.finality.admittedAtMonotonicMs);
        await pair.close();
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        await publisher.publish(empty);
        expect(publisher.read()).toMatchObject({
          cursor: fixture.emptySuccessorBlock.point,
          retainedEntries: 2,
          store: { revision: "2" },
        });
        expect(publisher.read().store.l1Observations).toHaveLength(2);
        const lifecycle = depositLifecycle(facts, preserveDataEncoding);
        const eventBlock = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create, lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const events = await fixture.openFinalizedBlock(eventBlock);
        await empty.close();
        await publisher.publish(events);
        const completed = publisher.read();
        expect(completed).toMatchObject({
          cursor: eventBlock.point,
          retainedEntries: 3,
          store: { revision: "3" },
          snapshot: { activeEvents: [] },
        });
        expect(completed.snapshot.terminalEvents).toHaveLength(1);
        const originalOutput = CML.Transaction.from_cbor_hex(lifecycle.create)
          .body()
          .outputs()
          .get(0);
        const originalDatum = originalOutput.datum()!.as_datum()!;
        expect(
          originalDatum.to_cbor_hex() !== originalDatum.to_canonical_cbor_hex(),
        ).toBe(preserveDataEncoding);
        expect(completed.snapshot.terminalEvents[0]).toMatchObject({
          eventId: lifecycle.expectedEventId,
          transactionHash: lifecycle.createId,
          originBlockHash: eventBlock.point.blockHash,
          terminalBlockHash: eventBlock.point.blockHash,
          terminalStatus: "absorbed",
          finalityStatus: "final",
          terminalFinalityStatus: "final",
          datumCborHex: originalDatum.to_cbor_hex(),
          datumDigest: sha256(originalDatum.to_cbor_bytes()),
          outputCborHex: originalOutput.to_cbor_hex(),
        });
        expect(completed.store.protocolUtxos).toEqual([]);
        expect(completed.store.spentProtocolUtxos).toEqual([]);
        expect(
          Reflect.set(completed.snapshot.terminalEvents[0]!, "eventId", "00"),
        ).toBe(false);
        expect(durable.runtime.read().currentStore).toEqual(globalStore);
        await expect(
          publisher.eventAuthority({
            ...events,
            eventId: lifecycle.expectedEventId,
            kind: "deposit",
          }),
        ).rejects.toThrow("fresh post-publication capture");
        const fresh = await fixture.openFinalizedBlock(eventBlock);
        await expect(
          publisher.eventAuthority({
            ...fresh,
            eventId: lifecycle.expectedEventId,
            kind: "withdrawal",
          }),
        ).rejects.toThrow("event is not retained");
        const authority = await publisher.eventAuthority({
          ...fresh,
          eventId: lifecycle.expectedEventId,
          kind: "deposit",
        });
        const authorityRead =
          await readWatcherLocalUserEventAuthority(authority);
        expect(authorityRead).toMatchObject({
          deploymentManifestId: fixture.deploymentIdentity.manifestId,
          blueprintHash: completed.policy.blueprintHash,
          event: completed.snapshot.terminalEvents[0],
          checkpointDigest: completed.checkpoint!.checkpointDigest,
          checkpointPayloadDigest: completed.checkpoint!.payloadDigest,
          snapshotDigest: completed.snapshot.snapshotDigest,
          historyEntryDigests: [completed.entryDigest],
        });
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(authority),
        ).not.toThrow();
        const replayInput = await makeLocalDepositReplayFixture(
          authority,
          fixture.deploymentIdentity.programCommitments,
        );
        const replayEventAuthority = replayInput.eventAuthorities![0]!;
        if (replayEventAuthority.localUserEvent === undefined)
          throw new Error("local replay fixture has parser authority");
        const replayResult = await evaluateWatcherBlockReplay(replayInput);
        expect(replayResult).toMatchObject({
          action: "accept",
          reasonCodes: [],
          eventRoots: [{ stepIndex: 0, phase: "Deposit", mutationCount: 1 }],
        });
        expect(replayResult.postStateRoot).not.toBe(
          replayResult.priorStateRoot,
        );
        expect(
          readWatcherBlockReplayEventAuthorityRecords(replayResult),
        ).toMatchObject([
          {
            phase: "Deposit",
            event: authorityRead.event,
            origin: {
              source: "local_publication",
              deploymentManifestId: fixture.deploymentIdentity.manifestId,
              checkpointDigest: authorityRead.checkpointDigest,
            },
          },
        ]);
        const mismatchedBundles: readonly WatcherRuleBundle[] = [
          { ...replayInput.ruleBundle, deploymentManifestId: "f1".repeat(32) },
          { ...replayInput.ruleBundle, blueprintHash: "f2".repeat(32) },
          { ...replayInput.ruleBundle, network: "Preview" },
        ];
        for (const ruleBundle of mismatchedBundles) {
          const ruleBundleCommitment =
            computeWatcherRuleBundleCommitment(ruleBundle);
          const phaseA = await evaluateWatcherPhaseABlock({
            ...replayInput,
            ruleBundle,
            ruleBundleCommitment,
          });
          expect(phaseA.action).toBe("accept");
          expect(
            await evaluateWatcherBlockReplay({
              ...replayInput,
              phaseA,
              ruleBundle,
              ruleBundleCommitment,
            }),
          ).toMatchObject({
            action: "error",
            reasonCodes: ["user_event_authority_identity_mismatch"],
          });
        }
        const copiedAuthority = { ...authority };
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(copiedAuthority),
        ).toThrow("not privately admitted");
        expect(
          await evaluateWatcherBlockReplay({
            ...replayInput,
            eventAuthorities: [
              {
                ...replayEventAuthority,
                localUserEvent: copiedAuthority,
              },
            ],
          }),
        ).toMatchObject({
          action: "error",
          reasonCodes: ["user_event_authority_invalid"],
        });
        await expect(
          readWatcherLocalUserEventAuthority({ ...authority }),
        ).rejects.toThrow("not privately admitted");
        await expect(
          Reflect.apply(readWatcherLocalUserEventAuthority, undefined, [
            authorityRead,
          ]),
        ).rejects.toThrow("not privately admitted");
        const successorBlock = await fixture.makeBlock({
          parent: eventBlock,
          transactions: [],
        });
        const successor = await fixture.openFinalizedBlock(successorBlock);
        await publisher.publish(successor);
        await expect(
          readWatcherLocalUserEventAuthority(authority),
        ).rejects.toThrow("no longer matches");
        const freshSuccessor = await fixture.openFinalizedBlock(successorBlock);
        const nextAuthority = await publisher.eventAuthority({
          ...freshSuccessor,
          eventId: lifecycle.expectedEventId,
          kind: "deposit",
        });
        expect(
          (await readWatcherLocalUserEventAuthority(nextAuthority)).event,
        ).toEqual(authorityRead.event);
        expect(durable.runtime.read().currentStore).toEqual(globalStore);
        await freshSuccessor.close();
        await expect(
          readWatcherLocalUserEventAuthority(nextAuthority),
        ).rejects.toThrow();
        const finalPair = await fixture.openFinalizedBlock(successorBlock);
        const finalAuthority = await publisher.eventAuthority({
          ...finalPair,
          eventId: lifecycle.expectedEventId,
          kind: "deposit",
        });
        publisher.close();
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(finalAuthority),
        ).toThrow("closed");
        expect(
          await evaluateWatcherBlockReplay({
            ...replayInput,
            eventAuthorities: [
              {
                ...replayEventAuthority,
                localUserEvent: finalAuthority,
              },
            ],
          }),
        ).toMatchObject({
          action: "error",
          reasonCodes: ["user_event_authority_invalid"],
        });
        await expect(
          readWatcherLocalUserEventAuthority(finalAuthority),
        ).rejects.toThrow("closed");
        expect(() => publisher.read()).toThrow("closed");
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("retains an unpublished cursor after uncertain CAS and accepts only the exact freshly protected candidate once", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publication = await readWatcherProtectedUserEventCheckpoint(
        durable.runtime,
      );
      const history = createWatcherLocalUserEventHistory({
        ...input,
        origin,
        publication,
      });
      expect(() => readWatcherLocalUserEventHistory({ ...history })).toThrow(
        "not privately admitted",
      );
      expect(() =>
        prepareWatcherLocalUserEventTransition({
          history,
          ...pair,
          referenceAuthority: { ...pair.referenceAuthority },
          publication,
        }),
      ).toThrow();
      const transition = prepareWatcherLocalUserEventTransition({
        history,
        ...pair,
        publication,
      });
      expect(() =>
        readWatcherLocalUserEventTransition({ ...transition }),
      ).toThrow("not privately admitted");
      const prepared = readWatcherLocalUserEventTransition(transition);
      expect(readWatcherLocalUserEventHistory(history).cursor).toBeNull();
      expect(() =>
        acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication,
        }),
      ).toThrow("exact prepared frame");
      for (const object of prepared.archiveObjects)
        expect(
          await durable.archive.put(Buffer.from(object.bytesHex, "hex")),
        ).toBe(object.digest);
      durable.interruptNextReadBack();
      await expect(
        persistWatcherUserEventCheckpoint(durable.runtime, prepared),
      ).rejects.toThrow("fixture read-back interruption");
      expect(readWatcherLocalUserEventHistory(history)).toMatchObject({
        cursor: null,
        retainedEntries: 0,
        store: { revision: "0" },
      });
      await expect(
        readWatcherProtectedUserEventCheckpoint(durable.runtime),
      ).rejects.toThrow();
      const restartedRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const committed =
        await readWatcherProtectedUserEventCheckpoint(restartedRuntime);
      expect(
        readWatcherProtectedUserEventCheckpointReceipt(committed).checkpoint,
      ).toEqual(prepared.nextCheckpoint);
      expect(() =>
        acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication: { ...committed },
        }),
      ).toThrow("not admitted");
      const accepted = acceptWatcherLocalUserEventPublication({
        history,
        transition,
        publication: committed,
      });
      expect(accepted.cursor).toEqual(fixture.activationBlock.point);
      expect(
        acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication: committed,
        }),
      ).toEqual(accepted);
      expect(readWatcherLocalUserEventHistory(history)).toMatchObject({
        retainedEntries: 1,
        store: { revision: "1" },
      });
      expect(() =>
        createWatcherLocalUserEventHistory({
          ...input,
          origin,
          publication: committed,
        }),
      ).toThrow("absent matching protected checkpoint");
    } finally {
      await fixture.close();
    }
  }, 120_000);

  it("writes only newly required archive objects while retaining the full protected closure", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      const put = vi.spyOn(durable.archive, "put");
      await publisher.publish(pair);
      const previous = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      ).checkpoint!;
      const protectedDigests = new Set(previous.requiredArchiveDigests);
      expect(put).toHaveBeenCalledTimes(protectedDigests.size);
      put.mockClear();
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      const next = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      ).checkpoint!;
      const written = put.mock.calls.map(([bytes]) =>
        watcherUserEventArchiveDigest(bytes),
      );
      expect(written.sort()).toEqual(
        next.requiredArchiveDigests.filter(
          (digest) => !protectedDigests.has(digest),
        ),
      );
      expect(written).toHaveLength(4);
      expect(next.requiredArchiveDigests).toHaveLength(
        protectedDigests.size + written.length,
      );
      expect(
        previous.requiredArchiveDigests.every((digest) =>
          next.requiredArchiveDigests.includes(digest),
        ),
      ).toBe(true);
      expect(publisher.read().store.revision).toBe("2");
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);

  it.each(["missing", "tampered"] as const)(
    "refuses %s protected archive bytes before publication and after the initial fresh read",
    async (mode) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      try {
        const { pair, input, origin } = await openOrigin(fixture);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(pair.finality).policy,
        );
        const publisher = await createWatcherLocalUserEventPublisher({
          ...input,
          origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(pair);
        const previous = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        ).checkpoint!;
        const digest = previous.requiredArchiveDigests[0]!;
        const original = Uint8Array.from(durable.objects.get(digest)!);
        const damage = () => {
          if (mode === "missing") durable.objects.delete(digest);
          else {
            const changed = Uint8Array.from(original);
            changed[0] = changed[0]! ^ 1;
            durable.objects.set(digest, changed);
          }
        };
        const expectedError =
          mode === "missing"
            ? "archive object is missing"
            : "archive digest differs";
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        const casBefore = durable.casCount();
        const put = vi.spyOn(durable.archive, "put");

        damage();
        await expect(publisher.publish(empty)).rejects.toThrow(expectedError);
        expect(put).not.toHaveBeenCalled();
        expect(durable.casCount()).toBe(casBefore);
        durable.objects.set(digest, Uint8Array.from(original));

        // The first new write happens after the initial protected read. The
        // refresh must still inspect every old object and refuse this change.
        durable.setBeforePut(async () => {
          durable.setBeforePut(null);
          damage();
        });
        await expect(publisher.publish(empty)).rejects.toThrow(expectedError);
        expect(put).toHaveBeenCalledTimes(4);
        expect(
          put.mock.calls.every(
            ([bytes]) =>
              !previous.requiredArchiveDigests.includes(
                watcherUserEventArchiveDigest(bytes),
              ),
          ),
        ).toBe(true);
        expect(durable.casCount()).toBe(casBefore);
        expect(durable.objects.get(digest)).not.toEqual(original);
        expect(publisher.read()).toMatchObject({
          cursor: fixture.activationBlock.point,
          status: "publication_pending",
          store: { revision: "1" },
        });

        durable.objects.set(digest, Uint8Array.from(original));
        await publisher.publish(empty);
        expect(publisher.read().store.revision).toBe("2");
        expect(durable.casCount()).toBe(casBefore + 1);
        publisher.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("retries the same archive candidate and refuses first-block substitution and skipped event coverage", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await expect(publisher.publish(empty)).rejects.toThrow(
        "exact activation pair",
      );
      durable.setBeforePut(async () => {
        durable.setBeforePut(null);
        throw new Error("fixture archive interruption");
      });
      await expect(publisher.publish(pair)).rejects.toThrow(
        "fixture archive interruption",
      );
      expect(publisher.read()).toMatchObject({
        cursor: null,
        retainedEntries: 0,
        status: "publication_pending",
      });
      await expect(publisher.publish(empty)).rejects.toThrow(
        "reconcile the original candidate",
      );
      await publisher.publish(pair);
      const skippedBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [],
      });
      const skipped = await fixture.openFinalizedBlock(skippedBlock);
      await expect(publisher.publish(skipped)).rejects.toThrow(
        "strict full-point successor",
      );
      expect(publisher.read()).toMatchObject({
        cursor: fixture.activationBlock.point,
        retainedEntries: 1,
        store: { revision: "1" },
      });
      await publisher.publish(empty);
      expect(publisher.read().store.revision).toBe("2");
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("durable local user-event restart", () => {
  it("restores validated events using only the current head, preserves progress, and continues incrementally", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const initial = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(initial.pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...initial.input,
        origin: initial.origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(initial.pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      await empty.close();
      const lifecycle = depositLifecycle(initial.facts);
      const block = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const pair = await fixture.openFinalizedBlock(block);
      await publisher.publish(pair);
      const original = publisher.read();
      publisher.close();
      await initial.pair.close();
      await pair.close();
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      const requests: string[] = [];
      const reopened = await resumeWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        readHead: async (point) => {
          requests.push(point.blockHash);
          expect(point).toEqual(block.point);
          return fixture.openFinalizedBlock(block);
        },
      });
      expect(requests).toEqual([block.point.blockHash]);
      expect(reopened.read().checkpoint).toEqual(original.checkpoint);
      expect(reopened.read().snapshot).toEqual(original.snapshot);
      expect(reopened.read().store).toEqual(original.store);
      const head = await fixture.openFinalizedBlock(block);
      const authority = await reopened.eventAuthority({
        ...head,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(authority)).checkpointDigest,
      ).toBe(original.checkpoint!.checkpointDigest);
      await head.close();
      const nextBlock = await fixture.makeBlock({
        parent: block,
        transactions: [],
      });
      const nextPair = await fixture.openFinalizedBlock(nextBlock);
      await reopened.publish(nextPair);
      expect(reopened.read().cursor).toEqual(nextBlock.point);
      expect(reopened.read().checkpoint!.checkpointSequence).toBe(
        (BigInt(original.checkpoint!.checkpointSequence) + 1n).toString(),
      );
      reopened.close();
      await nextPair.close();
      // A valid pair for another block cannot corroborate the protected head.
      await expect(
        resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime,
          archive: durable.archive,
          readHead: async () => fixture.openFinalizedBlock(block),
        }),
      ).rejects.toThrow("saved head is no longer canonical");
      const current = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const checkpoint = current.checkpoint!;
      const nextCheckpoint = makeWatcherUserEventCheckpoint({
        ...checkpoint,
        checkpointSequence: (
          BigInt(checkpoint.checkpointSequence) + 1n
        ).toString(),
        predecessorCheckpointDigest: checkpoint.checkpointDigest,
      });
      // A caller-created object cannot stamp semantic validation.
      await expect(
        persistWatcherUserEventCheckpoint(runtime, {
          expectedCheckpointDigest: checkpoint.checkpointDigest,
          expectedCheckpointSequence: checkpoint.checkpointSequence,
          nextCheckpoint,
          validationCandidate: {},
        }),
      ).rejects.toThrow();
      await persistWatcherUserEventCheckpoint(runtime, {
        expectedCheckpointDigest: checkpoint.checkpointDigest,
        expectedCheckpointSequence: checkpoint.checkpointSequence,
        nextCheckpoint,
      });
      await expect(
        resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime,
          archive: durable.archive,
          readHead: async () => {
            throw new Error("must refuse before native reads");
          },
        }),
      ).rejects.toThrow("restart requires durable semantic validation");
      await fresh.pair.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("explicit local user-event semantic recovery (synthetic local blocks)", () => {
  it("replays actual archived history across repeated durable reopen and refuses protected semantic corruption", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const emptyPair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(emptyPair);
      const lifecycle = depositLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const eventPair = await fixture.openFinalizedBlock(eventBlock);
      await publisher.publish(eventPair);
      const original = publisher.read();
      const originalObjects = new Map(
        [...durable.objects].map(([digest, bytes]) => [
          digest,
          Uint8Array.from(bytes),
        ]),
      );
      await pair.close();
      await eventPair.close();
      await emptyPair.close();
      publisher.close();
      const blocks = new Map([
        [
          fixture.emptySuccessorBlock.point.blockHash,
          fixture.emptySuccessorBlock,
        ],
        [eventBlock.point.blockHash, eventBlock],
      ]);
      const requests: string[] = [];
      const replayBlock = async (point: typeof eventBlock.point) => {
        const block = blocks.get(point.blockHash);
        if (
          block === undefined ||
          !watcherSameCanonicalJson(point, block.point)
        )
          throw new Error("unexpected replay point");
        requests.push(point.blockHash);
        return await fixture.openFinalizedBlock(block);
      };
      const reopen = async () => {
        const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        const fresh = await openOrigin(fixture);
        try {
          const resumed = await recoverWatcherLocalUserEventPublisher({
            ...fresh.input,
            origin: fresh.origin,
            referenceAuthority: fresh.pair.referenceAuthority,
            runtime,
            archive: durable.archive,
            replayBlock,
          });
          return { resumed, runtime };
        } finally {
          await fresh.pair.close();
        }
      };
      const first = await reopen();
      expect(requests).toEqual([
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
      ]);
      expect(first.resumed.read().checkpoint!.checkpointSequence).toBe(
        (BigInt(original.checkpoint!.checkpointSequence) + 1n).toString(),
      );
      expect(first.resumed.read().snapshot.terminalEvents[0]).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
        eventCborHex: original.snapshot.terminalEvents[0]!.eventCborHex,
      });
      expect(first.resumed.read().snapshot.snapshotDigest).not.toBe(
        original.snapshot.snapshotDigest,
      );
      const firstFresh = await fixture.openFinalizedBlock(eventBlock);
      const firstAuthority = await first.resumed.eventAuthority({
        ...firstFresh,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(firstAuthority))
          .checkpointDigest,
      ).toBe(first.resumed.read().checkpoint!.checkpointDigest);
      await firstFresh.close();
      first.resumed.close();
      // A second restart reads an explicit readmission payload, with the original closure intact.
      const second = await reopen();
      expect(requests).toEqual([
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
      ]);
      const successorBlock = await fixture.makeBlock({
        parent: eventBlock,
        transactions: [],
      });
      blocks.set(successorBlock.point.blockHash, successorBlock);
      const successor = await fixture.openFinalizedBlock(successorBlock);
      await second.resumed.publish(successor);
      expect(second.resumed.read().cursor).toEqual(successorBlock.point);
      await successor.close();
      second.resumed.close();
      // An ordinary successor payload must also remain semantically restartable.
      const third = await reopen();
      expect(requests.slice(-2)).toEqual([
        eventBlock.point.blockHash,
        successorBlock.point.blockHash,
      ]);
      expect(third.resumed.read().cursor).toEqual(successorBlock.point);
      for (const [digest, bytes] of originalObjects) {
        expect(await durable.archive.read(digest)).toEqual(bytes);
        expect(
          third.resumed.read().checkpoint!.requiredArchiveDigests,
        ).toContain(digest);
      }
      const finalBlock = await fixture.makeBlock({
        parent: successorBlock,
        transactions: [],
      });
      blocks.set(finalBlock.point.blockHash, finalBlock);
      const finalPair = await fixture.openFinalizedBlock(finalBlock);
      await third.resumed.publish(finalPair);
      await finalPair.close();
      third.resumed.close();
      // The real lower structural publisher can protect bytes but cannot grant
      // event semantics. Rehash every affected final-entry binding and require
      // fresh whole-block replay to detect the changed inclusion time.
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(third.runtime),
      );
      const checkpoint = protectedHead.checkpoint!;
      const payload: {
        head: WatcherLocalUserEventEntry;
        retainedEntries: readonly WatcherLocalUserEventEntry[];
        snapshot: WatcherUserEventSnapshot;
      } = JSON.parse(Buffer.from(protectedHead.payload!).toString("utf8"));
      let priorObservation: WatcherUserEventObservation | undefined;
      for (const bytes of durable.objects.values()) {
        const entryArchive: {
          entry?: WatcherLocalUserEventEntry;
          observation?: WatcherUserEventObservation;
        } = JSON.parse(Buffer.from(bytes).toString("utf8"));
        if (entryArchive.entry?.entryDigest === payload.head.entryDigest)
          priorObservation = entryArchive.observation;
      }
      if (priorObservation === undefined)
        throw new Error("archived final observation is absent");
      const { snapshotDigest: _snapshotDigest, ...snapshotFields } =
        payload.snapshot;
      const terminal = snapshotFields.terminalEvents[0]!;
      const badSnapshotFields = {
        ...snapshotFields,
        terminalEvents: [
          {
            ...terminal,
            inclusionTime: (BigInt(terminal.inclusionTime) + 1n).toString(),
          },
        ],
      };
      const badSnapshot = {
        ...badSnapshotFields,
        snapshotDigest: watcherSha256CanonicalJson(badSnapshotFields),
      };
      const { observationDigest: _observationDigest, ...observationFields } =
        priorObservation;
      const badObservationFields = {
        ...observationFields,
        snapshot: badSnapshot,
      };
      const badObservation = {
        ...badObservationFields,
        observationDigest: watcherSha256CanonicalJson(badObservationFields),
      };
      const { entryDigest: _entryDigest, ...entryFields } = payload.head;
      const badEntryFields = {
        ...entryFields,
        snapshotDigest: badSnapshot.snapshotDigest,
        observationDigest: badObservation.observationDigest,
      };
      const badEntry = {
        ...badEntryFields,
        entryDigest: watcherSha256CanonicalJson(badEntryFields),
      };
      const badEntryDigest = await durable.archive.put(
        Buffer.from(
          watcherCanonicalJson({
            entry: badEntry,
            observation: badObservation,
          }),
          "utf8",
        ),
      );
      const badPayloadDigest = await durable.archive.put(
        Buffer.from(
          watcherCanonicalJson({
            ...payload,
            head: badEntry,
            snapshot: badSnapshot,
            retainedEntries: [
              ...payload.retainedEntries.slice(0, -1),
              badEntry,
            ],
          }),
          "utf8",
        ),
      );
      await persistWatcherUserEventCheckpoint(third.runtime, {
        expectedCheckpointDigest: checkpoint.checkpointDigest,
        expectedCheckpointSequence: checkpoint.checkpointSequence,
        nextCheckpoint: makeWatcherUserEventCheckpoint({
          ...checkpoint,
          checkpointSequence: (
            BigInt(checkpoint.checkpointSequence) + 1n
          ).toString(),
          predecessorCheckpointDigest: checkpoint.checkpointDigest,
          payloadDigest: badPayloadDigest,
          requiredArchiveDigests: [
            ...new Set([
              ...checkpoint.requiredArchiveDigests,
              badEntryDigest,
              badPayloadDigest,
            ]),
          ].sort(),
        }),
      });
      await expect(reopen()).rejects.toThrow(
        "fresh semantic replay differs from the archived event fold",
      );
      for (const [digest, bytes] of originalObjects)
        expect(await durable.archive.read(digest)).toEqual(bytes);
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

// Both fixture transports retain real private admission. Switching their test
// network boundaries only selects which synthetic chain answers a fresh query.
describe("local deposit transcript semantic renewal", () => {
  it("replays an ordinary deposit through fresh durable owner and actual queue admission", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    const eventNetwork = {
      fetch: globalThis.fetch,
      WebSocket: globalThis.WebSocket,
    };
    let queue:
      | Awaited<ReturnType<typeof createSyntheticStateQueueObservationFixture>>
      | undefined;
    let resumed:
      | Awaited<ReturnType<typeof recoverWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      const lifecycle = depositLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const eventPair = await fixture.openFinalizedBlock(eventBlock);
      await publisher.publish(eventPair);
      const firstFresh = await fixture.openFinalizedBlock(eventBlock);
      const firstAuthority = await publisher.eventAuthority({
        ...firstFresh,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      const before = await readWatcherLocalUserEventAuthority(firstAuthority);
      const replayInput = await makeLocalDepositReplayFixture(
        firstAuthority,
        fixture.deploymentIdentity.programCommitments,
      );
      queue = await createSyntheticStateQueueObservationFixture({
        header: replayInput.observation.header,
        ruleBundleCommitment: replayInput.ruleBundleCommitment,
      });
      const queueNetwork = {
        fetch: globalThis.fetch,
        WebSocket: globalThis.WebSocket,
      };
      const initialQueue = await queue.observeFresh();
      expect(queue.transport.deploymentIdentity.manifestId).toBe(
        fixture.deploymentIdentity.manifestId,
      );
      const transcriptInput = {
        deploymentIdentity: queue.transport.deploymentIdentity,
        stateQueueObservation: initialQueue.observation,
        header: initialQueue.header,
        payloadEnvelopeCbor: replayInput.payloadEnvelopeCbor,
        daProvenance: replayInput.daProvenance,
        priorState: replayInput.priorState,
        eventAuthorities: replayInput.eventAuthorities,
        ruleBundle: replayInput.ruleBundle,
        ruleBundleCommitment: replayInput.ruleBundleCommitment,
      };
      const original = await createWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        coordinate: { domain: "transition_step", index: "0" },
      });
      assertWatcherAuthenticatedReplayTranscript(original);
      const persistedTranscriptCborHex =
        watcherAuthenticatedReplayTranscriptCborHex(original);
      await initialQueue.close();
      publisher.close();
      await pair.close();
      await empty.close();
      await eventPair.close();
      await firstFresh.close();

      vi.stubGlobal("fetch", eventNetwork.fetch);
      vi.stubGlobal("WebSocket", eventNetwork.WebSocket);
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const freshOrigin = await openOrigin(fixture);
      const blocks = [fixture.emptySuccessorBlock, eventBlock];
      resumed = await recoverWatcherLocalUserEventPublisher({
        ...freshOrigin.input,
        origin: freshOrigin.origin,
        referenceAuthority: freshOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = blocks.find((candidate) =>
            watcherSameCanonicalJson(candidate.point, point),
          );
          if (block === undefined)
            throw new Error("unexpected transcript replay point");
          return await fixture.openFinalizedBlock(block);
        },
      });
      const renewedPair = await fixture.openFinalizedBlock(eventBlock);
      const renewedAuthority = await resumed.eventAuthority({
        ...renewedPair,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      const after = await readWatcherLocalUserEventAuthority(renewedAuthority);
      expect(after.checkpointDigest).not.toBe(before.checkpointDigest);
      expect(after.snapshotDigest).not.toBe(before.snapshotDigest);
      expect(after.event).toMatchObject({
        eventCborHex: before.event.eventCborHex,
        outputCborHex: before.event.outputCborHex,
        originBlockHash: before.event.originBlockHash,
        originSlot: before.event.originSlot,
        originBlockNo: before.event.originBlockNo,
      });
      vi.stubGlobal("fetch", queueNetwork.fetch);
      vi.stubGlobal("WebSocket", queueNetwork.WebSocket);
      const renewedQueue = await queue.observeFresh();
      expect(renewedQueue.observation).not.toBe(initialQueue.observation);
      const authority = replayInput.eventAuthorities![0]!;
      if (authority.localUserEvent === undefined)
        throw new Error("local deposit transcript requires local authority");
      const renewedInput = {
        ...transcriptInput,
        stateQueueObservation: renewedQueue.observation,
        header: renewedQueue.header,
        daProvenance: {
          ...replayInput.daProvenance,
          sourceId: "fresh-permissionless-da-peer",
        },
        eventAuthorities: [{ ...authority, localUserEvent: renewedAuthority }],
        persistedTranscriptCborHex,
      };
      const renewed =
        await replayWatcherAuthenticatedReplayTranscript(renewedInput);
      assertWatcherAuthenticatedReplayTranscript(renewed);
      expect(renewed.transcriptDigest).not.toBe(original.transcriptDigest);
      expect(renewed.eventAuthorityRecordsCborHex).not.toEqual(
        original.eventAuthorityRecordsCborHex,
      );
      expect(renewed.blockReplayResultDigest).not.toBe(
        original.blockReplayResultDigest,
      );
      expect(renewed.payloadEnvelopeSha256).toBe(
        original.payloadEnvelopeSha256,
      );
      expect(renewed.coordinate).toEqual(original.coordinate);
      const originalRecords = await readWatcherReplayTranscriptRecords(
        persistedTranscriptCborHex,
        30,
      );
      const renewedRecords = await readWatcherReplayTranscriptRecords(
        watcherAuthenticatedReplayTranscriptCborHex(renewed),
        30,
      );
      expect(renewedRecords.blockReplay.priorStateRoot).toBe(
        originalRecords.blockReplay.priorStateRoot,
      );
      expect(renewedRecords.blockReplay.postStateRoot).toBe(
        originalRecords.blockReplay.postStateRoot,
      );
      expect(renewedRecords.blockReplay.action).toBe("accept");
      expect(renewedRecords.blockReplay.eventRoots).toMatchObject([
        { stepIndex: 0, phase: "Deposit", mutationCount: 1 },
      ]);
      expect(watcherAuthenticatedReplayTranscriptCborHex(original)).toBe(
        persistedTranscriptCborHex,
      );
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...renewedInput,
          eventAuthorities: replayInput.eventAuthorities,
        }),
      ).rejects.toThrow();
      resumed.close();
      await renewedPair.close();
      await freshOrigin.pair.close();
    } finally {
      resumed?.close();
      await queue?.close();
      await fixture.close();
    }
  }, 120_000);
});

describe("local user-event materialized history (synthetic local blocks)", () => {
  it("rotates protected anchors beyond 128 blocks, retains old event provenance and cold replays each indexed segment once", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      let runtime = durable.runtime;
      let publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime,
        archive: durable.archive,
      });
      const blocks = [fixture.activationBlock, fixture.emptySuccessorBlock];
      await publisher.publish(pair);
      await pair.close();
      const emptyPair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(emptyPair);
      await emptyPair.close();
      const lifecycle = depositLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      blocks.push(eventBlock);
      let head = eventBlock;
      while (blocks.length < 128) {
        head = await fixture.makeBlock({ parent: head, transactions: [] });
        blocks.push(head);
      }
      for (const block of blocks.slice(2)) {
        const finalized = await fixture.openFinalizedBlock(block);
        try {
          await publisher.publish(finalized);
        } finally {
          await finalized.close();
        }
      }
      const before = publisher.read();
      expect(before.retainedEntries).toBe(128);
      expect(before.store.l1Observations).toHaveLength(128);
      const originalObjects = new Map(
        [...durable.objects].map(([digest, bytes]) => [
          digest,
          Uint8Array.from(bytes),
        ]),
      );
      const successorBlock = await fixture.makeBlock({
        parent: head,
        transactions: [],
      });
      blocks.push(successorBlock);
      let successor = await fixture.openFinalizedBlock(successorBlock);
      await expect(publisher.publish(successor)).rejects.toThrow(
        "semantic anchor rotation required",
      );
      const anchorPair = await fixture.openFinalizedBlock(head);
      let releaseWrite!: () => void;
      let enteredWrite!: () => void;
      const released = new Promise<void>((resolve) => {
        releaseWrite = resolve;
      });
      const entered = new Promise<void>((resolve) => {
        enteredWrite = resolve;
      });
      durable.setBeforePut(async () => {
        durable.setBeforePut(null);
        enteredWrite();
        await released;
      });
      const rotating = publisher.rotate(anchorPair);
      await entered;
      expect(publisher.read()).toMatchObject({
        status: "publication_pending",
        retainedEntries: 128,
        store: { revision: before.store.revision },
      });
      await expect(publisher.publish(successor)).rejects.toThrow(
        "already in flight",
      );
      durable.interruptNextReadBack();
      releaseWrite();
      await expect(rotating).rejects.toThrow();
      expect(publisher.read().store).toEqual(before.store);
      await expect(publisher.rotate(anchorPair)).rejects.toThrow(
        "trusted-head read-back differs",
      );
      publisher.close();
      await anchorPair.close();
      await successor.close();
      runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const recoveryOrigin = await openOrigin(fixture);
      const recoveryRequests: string[] = [];
      const recoveryBlocks = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      publisher = await recoverWatcherLocalUserEventPublisher({
        ...recoveryOrigin.input,
        origin: recoveryOrigin.origin,
        referenceAuthority: recoveryOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = recoveryBlocks.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected recovery replay point");
          recoveryRequests.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await recoveryOrigin.pair.close();
      expect(recoveryRequests).toEqual(
        blocks.slice(1, 128).map((block) => block.point.blockHash),
      );
      const anchored = publisher.read();
      expect(anchored.retainedEntries).toBe(64);
      expect(anchored.store.l1Observations).toHaveLength(65);
      expect(anchored.snapshot.terminalEvents[0]).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
      });
      expect(anchored.snapshot.snapshotDigest).not.toBe(
        before.snapshot.snapshotDigest,
      );
      expect(anchored.store.revision).toBe(
        (BigInt(before.store.revision) + 1n).toString(),
      );
      expect(anchored.checkpoint!.requiredArchiveDigests.length).toBeLessThan(
        before.checkpoint!.requiredArchiveDigests.length,
      );
      await anchorPair.close();
      const postAnchor = await fixture.openFinalizedBlock(head);
      const authority = await publisher.eventAuthority({
        ...postAnchor,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(authority)).event,
      ).toEqual(anchored.snapshot.terminalEvents[0]);
      await postAnchor.close();
      successor = await fixture.openFinalizedBlock(successorBlock);
      await publisher.publish(successor);
      await successor.close();
      head = successorBlock;
      // Five real protected rotations exercise immediate and power-of-two ancestor links.
      for (let index = 0; index < 4; index += 1) {
        const current = await fixture.openFinalizedBlock(head);
        try {
          await publisher.rotate(current);
        } finally {
          await current.close();
        }
        if (index < 3) {
          head = await fixture.makeBlock({ parent: head, transactions: [] });
          blocks.push(head);
          const finalized = await fixture.openFinalizedBlock(head);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
      }
      const final = publisher.read();
      expect(final.retainedEntries).toBe(64);
      expect(final.store.l1Observations).toHaveLength(65);
      const savedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const savedPayload: {
        anchor: { indexDigest: string; indexSequence: string };
      } = JSON.parse(Buffer.from(savedHead.payload!).toString("utf8"));
      expect(savedPayload.anchor.indexSequence).toBe("4");
      const archivedIndex: {
        ancestorDigests: readonly string[];
        materializedStoreDigest: string;
      } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(savedPayload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      expect(archivedIndex.ancestorDigests).toHaveLength(3);
      publisher.close();
      const firstIndex: { sourcePayloadDigest: string } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(archivedIndex.ancestorDigests[2]!))!,
        ).toString("utf8"),
      );
      for (const digest of [
        savedHead.checkpoint!.payloadDigest,
        archivedIndex.materializedStoreDigest,
        firstIndex.sourcePayloadDigest,
      ]) {
        const originalBytes = durable.objects.get(digest)!;
        expect(originalBytes).toBeDefined();
        const missingOrigin = await openOrigin(fixture);
        durable.objects.delete(digest);
        try {
          await expect(
            recoverWatcherLocalUserEventPublisher({
              ...missingOrigin.input,
              origin: missingOrigin.origin,
              referenceAuthority: missingOrigin.pair.referenceAuthority,
              runtime,
              archive: durable.archive,
              replayBlock: async () => {
                throw new Error(
                  "missing archive dependency must fail before replay",
                );
              },
            }),
          ).rejects.toThrow(/absent|missing/);
        } finally {
          durable.objects.set(digest, originalBytes);
          await missingOrigin.pair.close();
        }
      }
      runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      const requested: string[] = [];
      const byHash = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      const resumed = await recoverWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected indexed replay point");
          requested.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await fresh.pair.close();
      expect(requested).toEqual(
        blocks.slice(1).map((block) => block.point.blockHash),
      );
      expect(resumed.read()).toMatchObject({
        cursor: head.point,
        retainedEntries: 64,
        store: { revision: final.store.revision },
      });
      expect(resumed.read().store.l1Observations).toHaveLength(65);
      const freshHead = await fixture.openFinalizedBlock(head);
      const retained = await resumed.eventAuthority({
        ...freshHead,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(retained)).event,
      ).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
      });
      await freshHead.close();
      for (const [digest, bytes] of originalObjects)
        expect(await durable.archive.read(digest)).toEqual(bytes);
      const nextBlock = await fixture.makeBlock({
        parent: head,
        transactions: [],
      });
      const next = await fixture.openFinalizedBlock(nextBlock);
      await resumed.publish(next);
      expect(resumed.read().cursor).toEqual(nextBlock.point);
      await next.close();
      resumed.close();
      byHash.set(nextBlock.point.blockHash, nextBlock);
      blocks.push(nextBlock);
      const secondRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const secondOrigin = await openOrigin(fixture);
      const secondRequests: string[] = [];
      const secondResumed = await resumeWatcherLocalUserEventPublisher({
        ...secondOrigin.input,
        origin: secondOrigin.origin,
        referenceAuthority: secondOrigin.pair.referenceAuthority,
        runtime: secondRuntime,
        archive: durable.archive,
        readHead: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected second indexed replay point");
          secondRequests.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await secondOrigin.pair.close();
      expect(secondRequests).toEqual([nextBlock.point.blockHash]);
      expect(secondResumed.read().cursor).toEqual(nextBlock.point);
      expect(secondResumed.read().retainedEntries).toBe(65);
      secondResumed.close();
      runtime = secondRuntime;
      // A structurally protected but incorrect ancestry link grants no replay authority.
      const currentProtected = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const currentCheckpoint = currentProtected.checkpoint!;
      const currentPayload: {
        anchor: { indexDigest: string };
        [field: string]: unknown;
      } = JSON.parse(Buffer.from(currentProtected.payload!).toString("utf8"));
      const indexValue: {
        ancestorDigests: readonly string[];
        [field: string]: unknown;
      } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(currentPayload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      const badIndex = {
        ...indexValue,
        ancestorDigests: [
          indexValue.ancestorDigests[0]!,
          indexValue.ancestorDigests[1]!,
          indexValue.ancestorDigests[0]!,
        ],
      };
      const badIndexBytes = Buffer.from(watcherCanonicalJson(badIndex), "utf8");
      const badIndexDigest = await durable.archive.put(badIndexBytes);
      const badPayloadBytes = Buffer.from(
        watcherCanonicalJson({
          ...currentPayload,
          anchor: { ...currentPayload.anchor, indexDigest: badIndexDigest },
        }),
        "utf8",
      );
      const badPayloadDigest = await durable.archive.put(badPayloadBytes);
      const badCheckpoint = makeWatcherUserEventCheckpoint({
        ...currentCheckpoint,
        checkpointSequence: (
          BigInt(currentCheckpoint.checkpointSequence) + 1n
        ).toString(),
        predecessorCheckpointDigest: currentCheckpoint.checkpointDigest,
        payloadDigest: badPayloadDigest,
        requiredArchiveDigests: [
          ...new Set([
            ...currentCheckpoint.requiredArchiveDigests,
            badIndexDigest,
            badPayloadDigest,
          ]),
        ].sort(),
      });
      await persistWatcherUserEventCheckpoint(runtime, {
        expectedCheckpointDigest: currentCheckpoint.checkpointDigest,
        expectedCheckpointSequence: currentCheckpoint.checkpointSequence,
        nextCheckpoint: badCheckpoint,
      });
      const corruptRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const corruptOrigin = await openOrigin(fixture);
      await expect(
        recoverWatcherLocalUserEventPublisher({
          ...corruptOrigin.input,
          origin: corruptOrigin.origin,
          referenceAuthority: corruptOrigin.pair.referenceAuthority,
          runtime: corruptRuntime,
          archive: durable.archive,
          replayBlock: async () => {
            throw new Error("corrupt archive must fail before replay");
          },
        }),
      ).rejects.toThrow("ancestor sequence differs");
      await corruptOrigin.pair.close();
    } finally {
      await fixture.close();
    }
  }, 600_000);
});

/** Existing ordinary order creation shapes, parameterized by this actual local
 * origin's scripts. Synthetic block admission does not assert ledger validity.
 */
const ordinaryLocalOrderCreation = (
  facts: WatcherUserEventOriginFacts,
  kind: "withdrawal" | "forced_order",
  nativeCbor: Uint8Array,
) => {
  const eventId = {
    transactionId: h32(kind === "withdrawal" ? "c2" : "c3"),
    outputIndex: 0n,
  };
  const eventIdCborHex = outputReferenceToPlutusDataCbor({
    txHash: eventId.transactionId,
    outputIndex: 0,
  });
  const assetName = Buffer.from(
    blake2b(Buffer.from(eventIdCborHex, "hex"), { dkLen: 32 }),
  ).toString("hex");
  const witness = userEventWitnessScriptHash(assetName);
  const scripts =
    kind === "withdrawal"
      ? facts.scripts.withdrawal
      : facts.scripts.forcedOrder;
  const address: {
    paymentCredential: { PublicKeyCredential: [string] };
    stakeCredential: null;
  } = {
    paymentCredential: { PublicKeyCredential: ["88".repeat(28)] },
    stakeCredential: null,
  };
  const common = {
    inclusion_time: BigInt(
      resolveEventInclusionTime(
        slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
        "Preprod",
      ),
    ),
    witness,
    refund_address: address,
    refund_datum: "NoDatum" as const,
  };
  const payload = genuineUserEventForcedPayloadForCanonicalTx(nativeCbor);
  const datum =
    kind === "withdrawal"
      ? Data.to(
          {
            ...common,
            event: {
              id: eventId,
              info: {
                body: {
                  l2_outref: eventId,
                  l2_owner: "89".repeat(28),
                  l2_value: new Map(),
                  l1_address: address,
                  l1_datum: "NoDatum",
                },
                signature: ["aa", "bb"],
                validity: "WithdrawalIsValid",
              },
            },
          },
          WithdrawalOrderDatum,
        )
      : Data.to(
          {
            ...common,
            event: {
              id: eventId,
              tx: {
                tx_id: payload.tx_id,
                transaction_commitment: payload.transaction_commitment,
                source: payload.source,
              },
            },
          },
          TxOrderDatum,
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
  inputs.add(transactionInput(`${eventId.transactionId}#0`));
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
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
  body.set_reference_inputs(refs);
  body.set_certs(certificates);
  body.set_mint(mint);
  body.set_ttl(1_000n);
  const mintEvent = {
    AuthenticateEvent: {
      nonce_input_index: 0n,
      event_output_index: 0n,
      hub_ref_input_index: 0n,
      witness_registration_redeemer_index: 1n,
    },
  };
  const materialCarriage = payload.carriage.map((entry) => {
    if (
      typeof entry !== "object" ||
      entry === null ||
      !("Inline" in entry) ||
      typeof entry.Inline !== "object" ||
      entry.Inline === null ||
      !("preimage" in entry.Inline) ||
      typeof entry.Inline.preimage !== "string"
    )
      throw new Error("ordinary forced fixture requires inline carriage");
    return { Inline: { preimage: entry.Inline.preimage } };
  });
  const cbor = transaction(body, [
    {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
      cbor:
        kind === "withdrawal"
          ? Data.to(mintEvent, UserEventMintRedeemer)
          : Data.to(
              { event: mintEvent, material_carriage: materialCarriage },
              TxOrderMintRedeemer,
            ),
    },
    {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
      cbor: Data.to(
        { MintOrBurn: { targetPolicy: scripts.policyId } },
        UserEventWitnessPublishRedeemer,
      ),
    },
  ]);
  return { cbor, eventId, eventIdCborHex, payload };
};

describe("local event replay authority derivation", () => {
  it("derives ordinary deposit, withdrawal and forced inputs from actual capabilities and snapshots awaited sources", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    let publisher:
      | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      const native = makeNativeTx();
      const deposit = depositLifecycle(facts);
      const withdrawal = ordinaryLocalOrderCreation(
        facts,
        "withdrawal",
        native.txCbor,
      );
      const forced = ordinaryLocalOrderCreation(
        facts,
        "forced_order",
        native.txCbor,
      );
      const block = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [deposit.create, withdrawal.cbor, forced.cbor],
        creatingBodies: [fixture.initializationBodyCbor],
      });
      const published = await fixture.openFinalizedBlock(block);
      await publisher.publish(published);
      const fresh = await fixture.openFinalizedBlock(block);
      const depositCap = await publisher.eventAuthority({
        ...fresh,
        kind: "deposit",
        eventId: deposit.expectedEventId,
      });
      const withdrawalCap = await publisher.eventAuthority({
        ...fresh,
        kind: "withdrawal",
        eventId: withdrawal.eventIdCborHex,
      });
      const forcedCap = await publisher.eventAuthority({
        ...fresh,
        kind: "forced_order",
        eventId: forced.eventIdCborHex,
      });
      const depositEvent = (
        await readWatcherLocalUserEventAuthority(depositCap)
      ).event;
      const depositOrigin = Data.from(depositEvent.eventCborHex, DepositEvent);
      const depositClaim: WatcherCommittedEventClaim = {
        phase: "Deposit",
        eventIdCborHex: depositEvent.eventId,
        valueCborHex: Data.to(depositOrigin.info, DepositInfo),
        canonicalNativeTxCborHex: null,
      };
      const mutableDepositClaim = { ...depositClaim };
      const pendingDeposit = deriveWatcherLocalEventReplayAuthority({
        localUserEvent: depositCap,
        committedClaim: mutableDepositClaim,
        programMaterial: [],
      });
      mutableDepositClaim.eventIdCborHex = "00";
      mutableDepositClaim.valueCborHex = "00";
      const depositAuthority = await pendingDeposit;
      const replay = await makeLocalDepositReplayFixture(
        depositCap,
        fixture.deploymentIdentity.programCommitments,
      );
      expect(
        await evaluateWatcherBlockReplay({
          ...replay,
          eventAuthorities: [depositAuthority],
        }),
      ).toMatchObject({
        action: "accept",
        reasonCodes: [],
        eventRoots: [{ phase: "Deposit", mutationCount: 1 }],
      });
      const withdrawalEvent = (
        await readWatcherLocalUserEventAuthority(withdrawalCap)
      ).event;
      const withdrawalOrigin = Data.from(
        withdrawalEvent.eventCborHex,
        WithdrawalEvent,
      );
      for (const validity of [
        "WithdrawalIsValid",
        "NonExistentWithdrawalUtxo",
      ] as const) {
        const claim: WatcherCommittedEventClaim = {
          phase: "Withdrawal",
          eventIdCborHex: withdrawalEvent.eventId,
          valueCborHex: Data.to(
            { ...withdrawalOrigin.info, validity },
            WithdrawalInfo,
          ),
          canonicalNativeTxCborHex: null,
        };
        const authority = await deriveWatcherLocalEventReplayAuthority({
          localUserEvent: withdrawalCap,
          committedClaim: claim,
          programMaterial: [],
        });
        if (authority.phase !== "Withdrawal")
          throw new Error("withdrawal authority has another phase");
        expect(authority.transitionEffect.operations).toEqual(
          validity === "WithdrawalIsValid"
            ? [
                {
                  type: "delete",
                  outRefCbor: encodeMidgardSpendInputItem({
                    txId: Buffer.from(withdrawal.eventId.transactionId, "hex"),
                    outputIndex: 0,
                  }),
                },
              ]
            : [],
        );
      }
      const forcedClaim: WatcherCommittedEventClaim = {
        phase: "ForcedTransaction",
        eventIdCborHex: forced.eventIdCborHex,
        valueCborHex: Data.to(
          {
            tx_id: forced.payload.tx_id,
            source: forced.payload.source,
            verdict: "ForcedTxValid",
          },
          ForcedInclusionTxV1,
        ),
        canonicalNativeTxCborHex: native.txCbor.toString("hex"),
      };
      const mutableForcedClaim = { ...forcedClaim };
      const material: [string, string][] = [];
      const pendingForced = deriveWatcherLocalEventReplayAuthority({
        localUserEvent: forcedCap,
        committedClaim: mutableForcedClaim,
        programMaterial: material,
      });
      mutableForcedClaim.canonicalNativeTxCborHex = "00";
      mutableForcedClaim.valueCborHex = "00";
      material.push(["00", "00"]);
      const forcedAuthority = await pendingForced;
      expect(forcedAuthority).toMatchObject({
        phase: "ForcedTransaction",
        canonicalNativeTxCbor: native.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      });
      expect("transitionEffect" in forcedAuthority).toBe(false);
      for (const claim of [
        { ...forcedClaim, canonicalNativeTxCborHex: "00" },
        { ...forcedClaim, eventIdCborHex: depositClaim.eventIdCborHex },
      ]) {
        await expect(
          deriveWatcherLocalEventReplayAuthority({
            localUserEvent: forcedCap,
            committedClaim: claim,
            programMaterial: [],
          }),
        ).rejects.toThrow();
      }
      await expect(
        deriveWatcherLocalEventReplayAuthority({
          localUserEvent: { ...depositCap },
          committedClaim: depositClaim,
          programMaterial: [],
        }),
      ).rejects.toThrow("not privately admitted");
      const inFlight = deriveWatcherLocalEventReplayAuthority({
        localUserEvent: depositCap,
        committedClaim: depositClaim,
        programMaterial: [],
      });
      publisher.close();
      await expect(inFlight).rejects.toThrow("closed");
      await expect(
        deriveWatcherLocalEventReplayAuthority({
          localUserEvent: forcedCap,
          committedClaim: forcedClaim,
          programMaterial: [],
        }),
      ).rejects.toThrow("closed");
    } finally {
      publisher?.close();
      await fixture.close();
    }
  }, 120_000);
});

describe("local user-event challenged-header cutoff (synthetic local blocks)", () => {
  it.each(["before_creation", "before_terminal", "after_terminal"] as const)(
    "uses actual same-block SQ/event order: %s",
    async (order) => {
      const state: { lifecycle: ReturnType<typeof depositLifecycle> | null } = {
        lifecycle: null,
      };
      const fixture = await createSyntheticStateQueueObservationFixture({
        composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
          const origin = await openOrigin(transport);
          state.lifecycle = depositLifecycle(origin.facts);
          await origin.pair.close();
          const { create, consume, settlementBody } = state.lifecycle;
          return {
            transactions:
              order === "before_creation"
                ? [commitTransactionCbor, create, consume]
                : order === "before_terminal"
                  ? [create, commitTransactionCbor, consume]
                  : [create, consume, commitTransactionCbor],
            creatingBodies: [transport.initializationBodyCbor, settlementBody],
          };
        },
      });
      try {
        const lifecycle = state.lifecycle!;
        const { pair, input, origin } = await openOrigin(fixture.transport);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(pair.finality).policy,
        );
        const publisher = await createWatcherLocalUserEventPublisher({
          ...input,
          origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(pair);
        await pair.close();
        for (const block of [
          fixture.transport.emptySuccessorBlock,
          fixture.initializationBlock,
          fixture.commitBlock,
        ]) {
          const finalized = await fixture.transport.openFinalizedBlock(block);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
        const captured = await fixture.observeFresh();
        const fresh = await fixture.transport.openFinalizedBlock(
          fixture.commitBlock,
        );
        const request = {
          ...fresh,
          kind: "deposit" as const,
          eventId: lifecycle.expectedEventId,
          throughHeader: captured.header,
        };
        if (order === "before_creation")
          await expect(publisher.eventAuthority(request)).rejects.toThrow(
            "origin occurs after the challenged header",
          );
        else {
          const receipt = await publisher.eventAuthority(request);
          const scoped = await readWatcherLocalUserEventAuthority(receipt);
          expect(scoped.throughHeader).toMatchObject({
            headerHash: captured.header.headerHash,
            headerCborHex: captured.header.headerCborHex,
            observedTransactionHash: captured.header.observedTransactionHash,
            observedBlockHash: fixture.commitBlock.point.blockHash,
            transactionIndex: order === "before_terminal" ? "1" : "2",
          });
          expect(scoped.event.eventId).toBe(lifecycle.expectedEventId);
          expect("terminalStatus" in scoped.event).toBe(
            order === "after_terminal",
          );
          if (order === "after_terminal")
            expect(scoped.event).toMatchObject({ terminalStatus: "absorbed" });
          expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
          await expect(
            publisher.eventAuthority({
              ...request,
              throughHeader: { ...captured.header },
            }),
          ).rejects.toThrow("not admitted by the production source");
          await fresh.close();
          await expect(
            readWatcherLocalUserEventAuthority(receipt),
          ).rejects.toThrow();
        }
        await fresh.close();
        await captured.close();
        publisher.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("scopes an older sealed header without future terminal facts and retains the same cutoff through cold replay", async () => {
    const state: {
      lifecycle: ReturnType<typeof depositLifecycle> | null;
      creation: SyntheticUserEventBlock | null;
    } = { lifecycle: null, creation: null };
    const fixture = await createSyntheticStateQueueObservationFixture({
      composeCommitBlock: async ({
        transport,
        initializationBlock,
        commitTransactionCbor,
      }) => {
        const origin = await openOrigin(transport);
        state.lifecycle = depositLifecycle(origin.facts);
        await origin.pair.close();
        state.creation = await transport.makeBlock({
          parent: initializationBlock,
          transactions: [state.lifecycle.create],
          creatingBodies: [
            transport.initializationBodyCbor,
            state.lifecycle.settlementBody,
          ],
        });
        return {
          transactions: [commitTransactionCbor],
          parent: state.creation,
        };
      },
    });
    try {
      const lifecycle = state.lifecycle!;
      const terminal = await fixture.transport.makeBlock({
        parent: fixture.commitBlock,
        transactions: [lifecycle.consume],
        creatingBodies: [
          fixture.transport.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const blocks = [
        fixture.transport.activationBlock,
        fixture.transport.emptySuccessorBlock,
        fixture.initializationBlock,
        state.creation!,
        fixture.commitBlock,
        terminal,
      ];
      let head = terminal;
      for (let index = 0; index < 67; index += 1) {
        head = await fixture.transport.makeBlock({
          parent: head,
          transactions: [],
        });
        blocks.push(head);
      }
      const captured = await fixture.observeFresh();
      const { pair, input, origin } = await openOrigin(fixture.transport);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      await pair.close();
      for (const block of blocks.slice(1)) {
        const finalized = await fixture.transport.openFinalizedBlock(block);
        try {
          await publisher.publish(finalized);
        } finally {
          await finalized.close();
        }
      }
      const toRotate = await fixture.transport.openFinalizedBlock(head);
      await publisher.rotate(toRotate);
      await toRotate.close();
      expect(publisher.read().retainedEntries).toBe(64);
      expect(
        publisher
          .read()
          .store.chainPoints.some(
            (point) => point.blockHash === fixture.commitBlock.point.blockHash,
          ),
      ).toBe(false);
      const fresh = await fixture.transport.openFinalizedBlock(head);
      const receipt = await publisher.eventAuthority({
        ...fresh,
        kind: "deposit",
        eventId: lifecycle.expectedEventId,
        throughHeader: captured.header,
      });
      const scoped = await readWatcherLocalUserEventAuthority(receipt);
      expect(scoped.throughHeader).toMatchObject({
        headerHash: captured.header.headerHash,
        observedTransactionHash: captured.header.observedTransactionHash,
        observedBlockHash: fixture.commitBlock.point.blockHash,
        transactionIndex: "0",
      });
      expect("terminalStatus" in scoped.event).toBe(false);
      expect(scoped.historyEntryDigests).toContain(
        scoped.throughHeader!.historyEntryDigest,
      );
      expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      );
      const payload: { anchor: { indexDigest: string } } = JSON.parse(
        Buffer.from(protectedHead.payload!).toString("utf8"),
      );
      const archiveIndex: { sourcePayloadDigest: string } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(payload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      const originalPayload = durable.objects.get(
        archiveIndex.sourcePayloadDigest,
      )!;
      durable.objects.delete(archiveIndex.sourcePayloadDigest);
      await expect(
        publisher.eventAuthority({
          ...fresh,
          kind: "deposit",
          eventId: lifecycle.expectedEventId,
          throughHeader: captured.header,
        }),
      ).rejects.toThrow("absent or corrupt");
      durable.objects.set(archiveIndex.sourcePayloadDigest, originalPayload);
      await fresh.close();
      publisher.close();
      await captured.close();
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const newOrigin = await openOrigin(fixture.transport);
      const byHash = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      const resumed = await recoverWatcherLocalUserEventPublisher({
        ...newOrigin.input,
        origin: newOrigin.origin,
        referenceAuthority: newOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected cutoff replay point");
          return await fixture.transport.openFinalizedBlock(block);
        },
      });
      await newOrigin.pair.close();
      const newCaptured = await fixture.observeFresh();
      const newFresh = await fixture.transport.openFinalizedBlock(head);
      const renewed = await resumed.eventAuthority({
        ...newFresh,
        kind: "deposit",
        eventId: lifecycle.expectedEventId,
        throughHeader: newCaptured.header,
      });
      const newScoped = await readWatcherLocalUserEventAuthority(renewed);
      expect("terminalStatus" in newScoped.event).toBe(false);
      const { historyEntryDigest: oldDigest, ...oldCutoff } =
        scoped.throughHeader!;
      const { historyEntryDigest: newDigest, ...newCutoff } =
        newScoped.throughHeader!;
      expect(newCutoff).toEqual(oldCutoff);
      expect(newDigest).not.toBe(oldDigest);
      expect(newScoped.event).toMatchObject({
        eventId: scoped.event.eventId,
        eventCborHex: scoped.event.eventCborHex,
        transactionHash: scoped.event.transactionHash,
      });
      await newFresh.close();
      await newCaptured.close();
      resumed.close();
    } finally {
      await fixture.close();
    }
  }, 300_000);
});

// Prepare deterministic ordinary block contents before the SQ transport builds
// its commit. This retired template capability never authorizes the later replay.
const ordinaryDepositReplayTemplate = async () => {
  const fixture = await createSyntheticUserEventOriginFixture();
  let publisher:
    | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
    | undefined;
  try {
    const { pair, input, origin, facts } = await openOrigin(fixture);
    const durable = await durableFixture(
      readWatcherLocalBackfillFinality(pair.finality).policy,
    );
    publisher = await createWatcherLocalUserEventPublisher({
      ...input,
      origin,
      runtime: durable.runtime,
      archive: durable.archive,
    });
    await publisher.publish(pair);
    await publisher.publish(
      await fixture.openFinalizedBlock(fixture.emptySuccessorBlock),
    );
    const deposit = depositLifecycle(facts);
    const block = await fixture.makeBlock({
      transactions: [deposit.create],
      creatingBodies: [fixture.initializationBodyCbor],
    });
    await publisher.publish(await fixture.openFinalizedBlock(block));
    const authority = await publisher.eventAuthority({
      ...(await fixture.openFinalizedBlock(block)),
      kind: "deposit",
      eventId: deposit.expectedEventId,
    });
    const replay = await makeLocalDepositReplayFixture(
      authority,
      fixture.deploymentIdentity.programCommitments,
    );
    return {
      header: replay.observation.header,
      ruleBundleCommitment: replay.ruleBundleCommitment,
    };
  } finally {
    publisher?.close();
    await fixture.close();
  }
};

describe("header-scoped replay authorities", () => {
  it("binds actual same-block event cutoff to W25, transcript and fresh semantic recovery", async () => {
    const template = await ordinaryDepositReplayTemplate();
    let eventId = "";
    const queue = await createSyntheticStateQueueObservationFixture({
      ...template,
      composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
        const source = await openOrigin(transport);
        try {
          const deposit = depositLifecycle(source.facts);
          eventId = deposit.expectedEventId;
          return {
            transactions: [
              deposit.create,
              commitTransactionCbor,
              deposit.consume,
            ],
            creatingBodies: [
              transport.initializationBodyCbor,
              deposit.settlementBody,
            ],
          };
        } finally {
          await source.pair.close();
        }
      },
    });
    const fixture = queue.transport;
    let publisher:
      | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
      | undefined;
    let resumed:
      | Awaited<ReturnType<typeof recoverWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const historyBlocks = [
        fixture.emptySuccessorBlock,
        queue.initializationBlock,
        queue.commitBlock,
      ];
      for (const block of historyBlocks)
        await publisher.publish(await fixture.openFinalizedBlock(block));
      expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
      const captured = await queue.observeFresh();
      const freshPair = await fixture.openFinalizedBlock(queue.commitBlock);
      const authority = await publisher.eventAuthority({
        ...freshPair,
        throughHeader: captured.header,
        kind: "deposit",
        eventId,
      });
      const local = await readWatcherLocalUserEventAuthority(authority);
      expect("terminalStatus" in local.event).toBe(false);
      expect(local.throughHeader).toMatchObject({
        headerHash: queue.headerHash,
        queueOutRef: captured.header.queueOutRef,
        transactionIndex: "1",
      });
      expect(local.historyEntryDigests).toContain(
        local.throughHeader!.historyEntryDigest,
      );
      const replay = await makeLocalDepositReplayFixture(
        authority,
        fixture.deploymentIdentity.programCommitments,
      );
      const result = await evaluateWatcherBlockReplay(replay);
      expect(result).toMatchObject({
        action: "accept",
        reasonCodes: [],
        eventRoots: [{ phase: "Deposit", mutationCount: 1 }],
      });
      expect(
        await evaluateWatcherBlockReplay({
          ...replay,
          observation: {
            ...replay.observation,
            chainPoint: {
              ...replay.observation.chainPoint,
              blockHash: h32("fe"),
            },
          },
        }),
      ).toMatchObject({
        action: "error",
        reasonCodes: ["user_event_authority_identity_mismatch"],
      });
      const transcriptInput = {
        deploymentIdentity: fixture.deploymentIdentity,
        stateQueueObservation: captured.observation,
        header: captured.header,
        payloadEnvelopeCbor: replay.payloadEnvelopeCbor,
        daProvenance: replay.daProvenance,
        priorState: replay.priorState,
        ruleBundle: replay.ruleBundle,
        ruleBundleCommitment: replay.ruleBundleCommitment,
        eventAuthorities: replay.eventAuthorities,
      };
      const original = await createWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        coordinate: { domain: "transition_step", index: "0" },
      });
      const persistedTranscriptCborHex =
        watcherAuthenticatedReplayTranscriptCborHex(original);
      const originalRecords = await readWatcherReplayTranscriptRecords(
        persistedTranscriptCborHex,
        30,
      );
      const originalEvent = originalRecords.events[0]!;
      if (
        originalEvent.origin.source !== "local_publication" ||
        originalEvent.origin.throughHeader === null
      )
        throw new Error("scoped transcript lacks a cutoff");
      const originalOrigin = originalEvent.origin;
      const cutoff = originalOrigin.throughHeader!;
      const rewriteCutoff = (
        changes: Partial<WatcherLocalUserEventHeaderCutoff>,
      ) => {
        const eventRecord = {
          ...originalEvent,
          origin: {
            ...originalOrigin,
            throughHeader: { ...cutoff, ...changes },
          },
        };
        const blockReplay = {
          ...originalRecords.blockReplay,
          authorityManifestDigest: watcherSha256CanonicalJson([
            watcherBlockReplayEventAuthorityManifest(eventRecord),
          ]),
        };
        const { resultDigest: _oldResult, ...replayMaterial } = {
          ...blockReplay,
          downstreamPrerequisite: {
            ...blockReplay.downstreamPrerequisite,
            inputDigest: watcherBlockReplayDownstreamInputDigest(blockReplay),
          },
        };
        const rewrittenReplay = {
          ...replayMaterial,
          resultDigest: watcherSha256CanonicalJson(replayMaterial),
        };
        const { transcriptDigest: _oldTranscript, ...transcriptMaterial } = {
          ...original,
          blockReplayRecordCborHex:
            watcherReplayRawRecordCborHex(rewrittenReplay),
          blockReplayResultDigest: rewrittenReplay.resultDigest,
          eventAuthorityRecordsCborHex: [
            watcherReplayRawRecordCborHex(eventRecord),
          ],
        };
        return watcherReplayRawRecordCborHex({
          ...transcriptMaterial,
          transcriptDigest:
            computeDeploymentManifestJsonDigest(transcriptMaterial),
        });
      };
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({
            historyEntryDigest: h32("fd"),
          }),
        }),
      ).rejects.toThrow("event cutoff history membership");
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({
            queueOutRef: `${h32("fc")}#0`,
          }),
        }),
      ).rejects.toThrow("event cutoff queueOutRef");
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({ transactionIndex: "2" }),
        }),
      ).rejects.toThrow("differs from fresh authenticated replay semantics");
      publisher.close();
      await captured.close();
      await freshPair.close();
      const newRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const freshOrigin = await openOrigin(fixture);
      resumed = await recoverWatcherLocalUserEventPublisher({
        ...freshOrigin.input,
        origin: freshOrigin.origin,
        referenceAuthority: freshOrigin.pair.referenceAuthority,
        runtime: newRuntime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = historyBlocks.find((candidate) =>
            watcherSameCanonicalJson(candidate.point, point),
          );
          if (block === undefined)
            throw new Error("unexpected cutoff replay point");
          return await fixture.openFinalizedBlock(block);
        },
      });
      const renewedCapture = await queue.observeFresh();
      const renewedAuthority = await resumed.eventAuthority({
        ...(await fixture.openFinalizedBlock(queue.commitBlock)),
        throughHeader: renewedCapture.header,
        kind: "deposit",
        eventId,
      });
      const renewedLocal =
        await readWatcherLocalUserEventAuthority(renewedAuthority);
      expect(renewedLocal.throughHeader).toMatchObject({
        headerHash: cutoff.headerHash,
        headerCborHex: cutoff.headerCborHex,
        queueOutRef: cutoff.queueOutRef,
        observedTransactionHash: cutoff.observedTransactionHash,
        observedBlockHash: cutoff.observedBlockHash,
        observedSlot: cutoff.observedSlot,
        observedBlockNo: cutoff.observedBlockNo,
        transactionIndex: cutoff.transactionIndex,
      });
      expect(renewedLocal.throughHeader!.historyEntryDigest).not.toBe(
        cutoff.historyEntryDigest,
      );
      const eventAuthority = replay.eventAuthorities![0]!;
      if (eventAuthority.localUserEvent === undefined)
        throw new Error("scoped replay requires local capability");
      const renewed = await replayWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        stateQueueObservation: renewedCapture.observation,
        header: renewedCapture.header,
        eventAuthorities: [
          { ...eventAuthority, localUserEvent: renewedAuthority },
        ],
        persistedTranscriptCborHex,
      });
      assertWatcherAuthenticatedReplayTranscript(renewed);
      expect(renewed.transcriptDigest).not.toBe(original.transcriptDigest);
      expect(watcherAuthenticatedReplayTranscriptCborHex(original)).toBe(
        persistedTranscriptCborHex,
      );
      // A second actual queue contains the same header at another native point.
      const network = {
        fetch: globalThis.fetch,
        WebSocket: globalThis.WebSocket,
      };
      const otherQueue =
        await createSyntheticStateQueueObservationFixture(template);
      try {
        const other = await otherQueue.observeFresh();
        expect(other.header.headerHash).toBe(renewedCapture.header.headerHash);
        expect(other.header.observedBlockHash).not.toBe(
          renewedCapture.header.observedBlockHash,
        );
        await expect(
          createWatcherAuthenticatedReplayTranscript({
            ...transcriptInput,
            stateQueueObservation: other.observation,
            header: other.header,
            eventAuthorities: [
              { ...eventAuthority, localUserEvent: renewedAuthority },
            ],
            coordinate: { domain: "transition_step", index: "0" },
          }),
        ).rejects.toThrow();
      } finally {
        await otherQueue.close();
        vi.stubGlobal("fetch", network.fetch);
        vi.stubGlobal("WebSocket", network.WebSocket);
      }
    } finally {
      publisher?.close();
      resumed?.close();
      await queue.close();
    }
  }, 120_000);
});

describe("local user-event same-process rollback suspension", () => {
  it("retires old capabilities synchronously and requires fresh protected-head W12 corroboration", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      await pair.close();
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      await empty.close();
      const lifecycle = depositLifecycle(facts);
      const block = await fixture.makeBlock({
        transactions: [lifecycle.create],
        creatingBodies: [fixture.initializationBodyCbor],
      });
      const published = await fixture.openFinalizedBlock(block);
      await publisher.publish(published);
      await published.close();
      const before = await fixture.openFinalizedBlock(block);
      const event = publisher.read().snapshot.activeEvents[0]!;
      const old = await publisher.eventAuthority({
        ...before,
        eventId: event.eventId,
        kind: "deposit",
      });
      await expect(
        readWatcherLocalUserEventAuthority(old),
      ).resolves.toBeDefined();
      publisher.suspend();
      await expect(readWatcherLocalUserEventAuthority(old)).rejects.toThrow(
        /suspended/u,
      );
      await expect(publisher.publish(before)).rejects.toThrow(/suspended/u);
      await expect(publisher.resume(before)).rejects.toThrow(/freshly/u);
      const fresh = await fixture.openFinalizedBlock(block);
      await publisher.resume(fresh);
      await fresh.close();
      await before.close();
      await expect(readWatcherLocalUserEventAuthority(old)).rejects.toThrow();
      const corroborated = await fixture.openFinalizedBlock(block);
      const authority = await publisher.eventAuthority({
        ...corroborated,
        eventId: event.eventId,
        kind: "deposit",
      });
      await expect(
        readWatcherLocalUserEventAuthority(authority),
      ).resolves.toBeDefined();
      publisher.suspend();
      const rollbackPair = await fixture.openFinalizedBlock(block);
      // A changed protected head cannot be accepted as the old same-process fold.
      const otherRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const reopenedOrigin = await openOrigin(fixture);
      const other = await recoverWatcherLocalUserEventPublisher({
        ...reopenedOrigin.input,
        origin: reopenedOrigin.origin,
        ...reopenedOrigin.pair,
        runtime: otherRuntime,
        archive: durable.archive,
        replayBlock: async (point) =>
          fixture.openFinalizedBlock(
            point.blockHash === fixture.emptySuccessorBlock.point.blockHash
              ? fixture.emptySuccessorBlock
              : block,
          ),
      });
      other.close();
      await reopenedOrigin.pair.close();
      await expect(publisher.resume(rollbackPair)).rejects.toThrow();
      await rollbackPair.close();
      await corroborated.close();
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("owned user-event runtime ordinary unavailable candidates", () => {
  it("refuses an absent claim, then issues a valid header-scoped capability and keeps indexing", async () => {
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
    const state: { lifecycle: ReturnType<typeof depositLifecycle> | null } = {
      lifecycle: null,
    };
    const fixture = await createSyntheticStateQueueObservationFixture({
      ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
      composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
        const origin = await openOrigin(transport);
        state.lifecycle = depositLifecycle(origin.facts);
        await origin.pair.close();
        return {
          transactions: [state.lifecycle.create, commitTransactionCbor],
          creatingBodies: [transport.initializationBodyCbor],
        };
      },
    });
    let service: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      const transport = fixture.transport;
      const signed = transport.deployment;
      const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
        path: "/unit/authority.json",
        ruleBundlePath: "/unit/rules.json",
        unsafeReadFileForTest: async (path) =>
          new TextEncoder().encode(
            JSON.stringify(
              path === "/unit/authority.json"
                ? {
                    signedIdentity: signed.signedIdentity,
                    policy: signed.policy,
                    trustRoots: signed.trustRoots,
                    durableMarker: signed.marker,
                  }
                : ruleBundle,
            ),
          ),
      });
      const origin = await openOrigin(transport);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(origin.pair.finality).policy,
      );
      await origin.pair.close();
      service = await createWatcherUserEventRuntime({
        watcherConfig: transport.watcherConfig,
        deploymentAuthority,
        blueprintBytes: await readFile(
          process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
            fileURLToPath(
              new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
            ),
        ),
        nativeChainSyncBinaryPath: transport.nativeChainSyncBinaryPath,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await service.advanceThrough(fixture.commitBlock.point);
      const captured = await fixture.observeFresh();
      try {
        const request = {
          kind: "deposit" as const,
          eventId: state.lifecycle!.expectedEventId,
          throughHeader: captured.header,
        };
        const before = service.read().currentPoint;
        await expect(
          service.eventAuthority({ ...request, eventId: `${h32("ab")}#0` }),
        ).rejects.toThrow(/event is not retained/u);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: before,
        });
        expect(() => assertWatcherUserEventRuntime(service!)).not.toThrow();
        const cap = await service.eventAuthority(request);
        expect(
          (await readWatcherLocalUserEventAuthority(cap)).event.eventId,
        ).toBe(request.eventId);
        const next = await transport.makeBlock({
          transactions: [],
          parent: fixture.commitBlock,
        });
        await service.advanceThrough(next.point);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: next.point,
        });
        await expect(readWatcherLocalUserEventAuthority(cap)).rejects.toThrow();
        const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        );
        durable.objects.delete(protectedHead.checkpoint!.payloadDigest);
        await expect(
          service.eventAuthority({ ...request, eventId: `${h32("ab")}#0` }),
        ).rejects.toThrow();
        expect(service.read().status).toBe("failed");
        await expect(service.done).rejects.toThrow();
      } finally {
        await captured.close();
      }
    } finally {
      await service?.close();
      await fixture.close();
    }
  }, 120_000);
});

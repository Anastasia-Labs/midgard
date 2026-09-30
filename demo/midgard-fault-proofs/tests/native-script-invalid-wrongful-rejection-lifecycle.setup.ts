import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  encodeMidgardNativeScript,
  encodeMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, getAddressDetails } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { NATIVE_SCRIPT_INVALID_FORCED_ARTIFACT } from "../src/native-script-invalid/forced-artifact.js";
import { submitNativeScriptInvalidInit } from "../src/native-script-invalid/submit-init.js";
import { submitNativeScriptInvalidStep01Forced } from "../src/native-script-invalid/submit-step-01-forced.js";
import { prepareNativeScriptInvalidWorkflowArtifact } from "../src/native-script-invalid/workflow-artifact.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  makeNativeScriptInvalidEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  network,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

export const setup = async ({
  signerCount = 1,
  falseScript = false,
  invalidSignature = false,
  depth = 0,
  maximumField = false,
  fieldBytes = 32768,
  prefixCount = 0,
  deepScript = false,
  wrongReason = false,
}: {
  signerCount?: number;
  falseScript?: boolean;
  invalidSignature?: boolean;
  depth?: number;
  maximumField?: boolean;
  fieldBytes?: number;
  prefixCount?: number;
  deepScript?: boolean;
  wrongReason?: boolean;
} = {}) => {
  const h = await makeNativeScriptInvalidEmulatorHarness();
  const keys = Array.from({ length: signerCount }, (_, index) => {
    const seed = Buffer.alloc(32);
    seed.writeUInt32BE(index + 1, 28);
    return CML.PrivateKey.from_normal_bytes(seed);
  });
  // The 32-node domain bound makes every applicable native script <1024 bytes.
  let nativeScript: Parameters<typeof encodeMidgardNativeScript>[0] = {
    type: "all" as const,
    scripts: Array.from({ length: 31 }, () => ({
      type: "sig" as const,
      keyHash: Buffer.from(
        falseScript ? "ee".repeat(28) : keys[0]!.to_public().hash().to_hex(),
        "hex",
      ),
    })),
  };
  if (deepScript) {
    nativeScript = {
      type: "all",
      scripts:
        nativeScript.type === "all" ? nativeScript.scripts.slice(0, 17) : [],
    };
    for (let i = 0; i < 14; i++)
      nativeScript = { type: "all", scripts: [nativeScript] };
  }
  const scriptItem = encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript,
    scriptBytes: encodeMidgardNativeScript(nativeScript),
  });
  const emptyNative = { type: "all" as const, scripts: [] };
  const emptyItem = encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript: emptyNative,
    scriptBytes: encodeMidgardNativeScript(emptyNative),
  });
  const prefix = Array.from({ length: prefixCount }, () => emptyItem);
  let scripts = [...prefix, scriptItem];
  if (maximumField) {
    const decoy = (size: number) =>
      encodeMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: Buffer.alloc(size, 0),
      });
    let padding = 32000;
    while (
      encodeMidgardFieldPreimage([decoy(padding), ...prefix, scriptItem])
        .length > fieldBytes
    )
      padding--;
    while (
      encodeMidgardFieldPreimage([decoy(padding + 1), ...prefix, scriptItem])
        .length <= fieldBytes
    )
      padding++;
    scripts = [decoy(padding), ...prefix, scriptItem];
  }
  const base = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage(scripts),
    addrTxWitsPreimageCbor: encodeMidgardFieldPreimage([]),
    validityIntervalStart: 0n,
    validityIntervalEnd: 100n,
  });
  const id = computeMidgardNativeTxId(base);
  const witnesses = keys
    .map((key, index) => ({
      key,
      hash: key.to_public().hash().to_hex(),
      item: Buffer.concat([
        Buffer.from("825820", "hex"),
        key.to_public().to_raw_bytes(),
        Buffer.from("5840", "hex"),
        invalidSignature && index === 0
          ? Buffer.alloc(64)
          : key.sign(id).to_raw_bytes(),
      ]),
    }))
    .sort((a, b) => a.hash.localeCompare(b.hash));
  const submitted = materializeMidgardNativeTxFromCanonical({
    ...base,
    witnessSet: {
      ...base.witnessSet,
      addrTxWitsPreimageCbor: encodeMidgardFieldPreimage(
        witnesses.map((value) => value.item),
      ),
    },
  });
  const tx = materializeMidgardForcedTxFromCanonical(submitted);
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  const proofSource = deriveMidgardForcedTxProofSource(tx);
  const credential = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const fixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120000,
      ) - 1,
  });
  const key = fixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const scriptIndex = BigInt(scripts.length - 1);
  const reason = wrongReason
    ? ("FeeBelowMinimum" as const)
    : { WitnessNativeScriptFalse: { script_index: scriptIndex } };
  const value = {
    tx_id: txId,
    submitted_source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason } },
  };
  const keyBytes = Buffer.from(Data.to(key, SDK.OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(value as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  let root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const deep =
    depth > 0
      ? syntheticDeepMembershipProof({
          key: keyBytes,
          value: valueBytes,
          branchLevels: depth,
        })
      : undefined;
  if (deep)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
      root: await commitCountedRoot({
        domain: root.domain,
        phasRoot: deep.transactionsPhasRoot,
        count: 1n,
      }),
    };
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key,
    value,
    proof: Data.from(
      deep?.proofCbor ?? (await trie.prove(keyBytes)).toCBOR().toString("hex"),
      SDK.Proof,
    ),
  };
  const header = {
    ...fixture.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const seeded = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  const source = { header, membership, direction: 1n };
  const state = {
    subject: SDK.forcedVerdictSubject({
      transactionId: txId,
      sourceKey: key,
      rejectionReason: reason,
    }),
    bad_tx_id: txId,
    bad_tx_witness_set_hash:
      tx.compact.transactionWitnessSetHash.toString("hex"),
    validity_interval_start: 0n,
    validity_interval_end: 100n,
    grammar_checkpoint_hash: "",
    grammar_complete: false,
    script_checkpoint_hash: "",
  };
  const scriptPublications: ReturnType<
    typeof import("./support/emulator/measurement.js").measureCompleteSignedTransaction
  >[] = [];
  const refs = await publishFinalFamilyReferenceScripts({
    lucid: h.proverLucid,
    family: h.family,
    label: "native-script-invalid",
    onPublication: (_index, publication) =>
      scriptPublications.push(publication.publicationMeasurement),
  });
  const witness = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  const witnessSet = {
    addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
  };
  const common = {
    lucid: h.proverLucid,
    contracts: h.family,
    categoryId: h.category.categoryId,
    signer: h.proverSigner,
    nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
    witnessSet,
  };
  const artifact = {
    schemaVersion: NATIVE_SCRIPT_INVALID_FORCED_ARTIFACT,
    headerHash: seeded.headerHash,
    forcedIndex: 0,
    detectionId: `native-script-invalid:forced:0:${txId}:${scriptIndex}`,
    forcedSourceCbor: Data.to(
      source as never,
      SDK.NativeScriptInvalidForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor:
      encodeMidgardForcedTxCanonical(submitted).toString("hex"),
  };
  const init = () =>
    submitNativeScriptInvalidInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
      network,
      signer: h.proverSigner,
      fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const bind = (threadOutRef: string, overrides = {}) =>
    submitNativeScriptInvalidStep01Forced({
      ...common,
      threadOutRef,
      state,
      forcedSource: source,
      ...overrides,
      referenceScriptUtxo: refs[0]!,
    });
  const prepareArtifact = async () => {
    const entry = {
      key,
      value,
      keyBytes,
      valueBytes,
      fullTransactionCbor: encodeMidgardForcedTxCanonical(submitted),
    };
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
    const fingerprint = eventKeyFingerprint(eventKey);
    const block = {
      headerHash: seeded.headerHash,
      header,
      transactions: [],
      reconstruction: {
        forcedTransactions: [entry],
        rootData: { forcedTransactions: root },
        sourceEventsByFingerprint: new Map([
          [
            fingerprint,
            { phase: "ForcedTransaction", eventKey, fingerprint, entry },
          ],
        ]),
      },
    } as unknown as CanonicalBlockEvidence;
    const replay =
      await NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY.replay(block);
    const selected = replay.detections.find(
      (detection) => detection.detectionId === artifact.detectionId,
    );
    if (selected === undefined)
      throw new Error(
        "complete replay missed the forced native script contradiction",
      );
    return prepareNativeScriptInvalidWorkflowArtifact({
      evidence: block,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: block.headerHash,
        category: "nativeScriptInvalid",
        selected,
        detections: replay.detections,
        unprovableGaps: [],
      },
    });
  };
  return {
    h,
    refs,
    common,
    artifact,
    prepareArtifact,
    init,
    bind,
    seeded,
    state,
    source,
    scriptItem,
    scripts,
    witnesses,
    proofSource,
    scriptPublications,
    scriptIndex: scriptIndex,
  };
};

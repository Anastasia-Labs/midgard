import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  admitMinFeeForcedArtifact,
  MIN_FEE_FORCED_ARTIFACT,
  prepareMinFeeForcedArtifact,
} from "../src/min-fee-forced-artifact.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { submitMinFeeCancel } from "../src/submit-min-fee-cancel.js";
import { submitMinFeeStep01Forced } from "../src/submit-min-fee-forced-step-01.js";
import { submitMinFeeInit } from "../src/submit-min-fee-init.js";
import {
  type MinFeeFieldItemCbors,
  submitMinFeeStep02,
} from "../src/submit-min-fee-step-02.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

const fitRows: unknown[] = [];
afterAll(async () => {
  if (process.env.MIN_FEE_FIT_LEDGER_PATH) {
    const body = {
      schemaVersion: "midgard-min-fee-wrongful-rejection-fit-ledger-v1",
      category: "minFee",
      categoryId: "00000013",
      compiler: "aiken v1.1.23+5adf783",
      environment: "testnet",
      limits: {
        signedBytes: 16_384,
        publicationBytes: 15_872,
        memory: "13200000",
        cpu: "8000000000",
      },
      shapes: fitRows,
    };
    const blueprintSha256 = createHash("sha256")
      .update(await readFile(process.env.MIDGARD_REAL_BLUEPRINT_PATH!))
      .digest("hex");
    const json = JSON.parse(
      JSON.stringify(body, (_, value) =>
        typeof value === "bigint" ? value.toString() : value,
      ),
    );
    json.blueprintSha256 = blueprintSha256;
    await writeFile(
      process.env.MIN_FEE_FIT_LEDGER_PATH,
      JSON.stringify(
        {
          ...json,
          ledgerDigest: createHash("sha256")
            .update(JSON.stringify(json))
            .digest("hex"),
        },
        null,
        2,
      ) + "\n",
    );
  }
});

const setup = async (
  minimum: bigint,
  inputCount = 1,
  allFields = false,
  depth = 0,
  scaledMinimum = false,
) => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: { realMinFee: true, alwaysFraudProofCatalogue: true },
  });
  const contracts = h.contracts.minFee!;
  const category = h.catalogue.categories.minFee!;
  expect(category.scriptHash).toBe(contracts.steps[0].spendingScriptHash);
  const credential = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const fixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1,
  });
  const inputItems = Array.from({ length: inputCount }, (_, i) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.from("77".repeat(32), "hex"),
      outputIndex: i,
    }),
  );
  const base = makeNativeTx({ spendInputCbors: inputItems, fee: 1_000n });
  const items = (count: number, width: number, seed: number) =>
    encodeMidgardFieldPreimage(
      Array.from({ length: count }, (_, i) =>
        Buffer.alloc(width, (i + seed) % 256),
      ),
    );
  const submitted = allFields
    ? materializeMidgardNativeTxFromCanonical({
        ...base,
        body: {
          ...base.body,
          referenceInputsPreimageCbor: encodeMidgardFieldPreimage(inputItems),
          outputsPreimageCbor: items(100, 32, 1),
          requiredObserversPreimageCbor: items(110, 28, 2),
          requiredSignersPreimageCbor: items(110, 28, 3),
          mintPreimageCbor: items(100, 32, 4),
        },
        witnessSet: {
          ...base.witnessSet,
          scriptTxWitsPreimageCbor: items(100, 32, 5),
          addrTxWitsPreimageCbor: items(33, 101, 6),
          redeemerTxWitsPreimageCbor: items(100, 32, 7),
        },
      })
    : base;
  const tx = adjudicateMidgardNativeTxFullValidity(submitted, "TxIsInvalid");
  const transactionId = computeMidgardNativeTxId(tx).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSource(tx);
  const sourceKey = fixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const leaf = {
    tx_id: transactionId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
  } as const;
  const key = Buffer.from(Data.to(sourceKey, SDK.OutputReference), "hex");
  const value = Buffer.from(
    Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  let root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key, value },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(key, value);
  const proof = await trie.prove(key);
  const deep =
    depth > 0
      ? syntheticDeepMembershipProof({ key, value, branchLevels: depth })
      : null;
  if (deep !== null)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
      count: 1n,
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
    key: sourceKey,
    value: leaf,
    proof: Data.from(
      deep?.proofCbor ?? proof.toCBOR().toString("hex"),
      SDK.Proof,
    ),
  };
  const minFeeA = scaledMinimum ? 1n : 0n;
  const minFeeB = scaledMinimum
    ? 1_000n -
      SDK.minimumFeeFromProofSource({
        source: proofSource,
        minFeeA: 1n,
        minFeeB: 0n,
      }).canonicalTxSize
    : minimum;
  const header = {
    ...fixture.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
    minFeeA,
    minFeeB,
  };
  const seeded = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  const forcedSource = { header: header, membership, direction: 1n };
  const state: SDK.MinFeeStep02State = {
    subject: SDK.forcedVerdictSubject({
      transactionId,
      sourceKey,
      rejectionReason: "FeeBelowMinimum",
    }),
    bad_tx: nativeTxFromCoreCompact(tx.compact),
    bad_tx_body_fee: 1_000n,
    bad_tx_id: transactionId,
    min_fee_a: minFeeA,
    min_fee_b: minFeeB,
  };
  const witness = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  const witnessSet = {
    addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
  };
  const fields = [
    tx.body.spendInputsPreimageCbor,
    tx.body.referenceInputsPreimageCbor,
    tx.body.outputsPreimageCbor,
    tx.body.requiredObserversPreimageCbor,
    tx.body.requiredSignersPreimageCbor,
    tx.body.mintPreimageCbor,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    tx.witnessSet.addrTxWitsPreimageCbor,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  ].map((field) =>
    decodeMidgardFieldPreimage(field),
  ) as unknown as MinFeeFieldItemCbors;
  const refs: UTxO[] = [];
  const scriptPublications = [];
  for (const step of contracts.steps) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: step.spendingScript,
      label: "min-fee forced step",
    });
    refs.push(publication.utxo);
    scriptPublications.push(publication.publicationMeasurement);
  }
  const init = () =>
    submitMinFeeInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      network,
      contracts,
      category,
      catalogue: {
        policyId: h.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          h.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: h.catalogue.root,
      },
      signer: h.proverSigner,
      fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const bind = (threadOutRef: string, overrides = {}) =>
    submitMinFeeStep01Forced({
      lucid: h.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: h.proverSigner,
      threadOutRef,
      state,
      forcedSource,
      referenceScriptUtxo: refs[0]!,
      ...overrides,
    });
  const certificates: UTxO[] = [];
  const chunks: UTxO[] = [];
  const prepareCarriages = async () => {
    for (const [fieldIndex, items] of fields.entries()) {
      const planned = planFaultProofFieldOpening({
        fieldIndex,
        anchorTxId: transactionId,
        nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
        itemCbors: items,
        owner: h.proverSigner.paymentKeyHash,
        publish: true,
        ...(fieldIndex < 6
          ? {}
          : {
              witnessSet,
              anchorWitnessSetHash: state.bad_tx.witness_set_hash,
            }),
        label: "min-fee maximum field",
      });
      const published = await publishFaultProofFieldCarriage({
        lucid: h.proverLucid,
        signer: h.proverSigner,
        planned,
        publisherAddress: h.proverSigner.address,
        label: "min-fee maximum field",
      });
      chunks.push(...published);
      if (planned.plan.tier === "Certified") {
        const reference = await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: h.contracts.fieldPreimageCertificate.mintingScript,
          label: "min-fee field certificate",
        });
        const certificate = await certifyFaultProofFieldCarriage({
          lucid: h.proverLucid,
          network,
          signer: h.proverSigner,
          planned,
          certificatePolicyId: h.contracts.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            h.contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo: reference.utxo,
          chunkUtxos: published,
          compactCbor: proofSource.compactCbor.toString("hex"),
          witnessSetCompactCbor:
            proofSource.witnessSetCompactCbor.toString("hex"),
        });
        certificates.push(certificate.certificateUtxo);
      }
    }
  };
  const finish = (
    threadOutRef: string,
    unsafeSkipLocalViolationCheckForTest = false,
  ) =>
    submitMinFeeStep02({
      lucid: h.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: h.proverSigner,
      threadOutRef,
      nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
      witnessSet,
      fieldItemCbors: fields,
      certificateUtxos: certificates,
      existingPublicationUtxos: chunks,
      publishCarriages: chunks.length > 0,
      publishMissingCarriages: chunks.length === 0,
      referenceScriptUtxo: refs[1]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      unsafeSkipLocalViolationCheckForTest,
    });
  const cancel = (threadOutRef: string, step: number) =>
    submitMinFeeCancel({
      lucid: h.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: h.proverSigner,
      threadOutRef,
      referenceScriptUtxo: refs[step]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const remove = async () => {
    const refs = await captureEmulatorSubmission(h.emulator, () =>
      publishRemovalReferenceScripts({
        lucid: h.proverLucid,
        contracts: h.contracts,
      }),
    );
    const now = BigInt(h.emulator.now());
    const removal = await captureEmulatorSubmission(h.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
          removalReferenceScripts: refs.result.published,
        }),
        network,
        signer: h.proverSigner,
        fraudCategory: "minFee",
        fraudulentHeaderHash: seeded.headerHash,
        requireReferenceScripts: true,
        validFrom: now - 120_000n,
        validTo: now + 300_000n,
      }),
    );
    expect(
      await h.proverLucid.utxosAtWithUnit(
        h.contracts.stateQueue.spendingScriptAddress,
        seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(0);
    return [...refs.measurements, ...removal.measurements];
  };
  const artifact = {
    schemaVersion: MIN_FEE_FORCED_ARTIFACT,
    headerHash: seeded.headerHash,
    detectionId: `min-fee:forced:0:${transactionId}`,
    forcedIndex: 0,
    forcedSourceCbor: Data.to(
      forcedSource as never,
      SDK.MinFeeForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor:
      encodeMidgardNativeTxCanonical(submitted).toString("hex"),
  };
  const prepareArtifact = () => {
    const entry = {
      key: sourceKey,
      value: leaf,
      keyBytes: key,
      valueBytes: value,
      fullTransactionCbor: encodeMidgardNativeTxCanonical(submitted),
    };
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: sourceKey } };
    const fingerprint = eventKeyFingerprint(eventKey);
    const block = {
      headerHash: seeded.headerHash,
      header,
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
    return prepareMinFeeForcedArtifact({
      block,
      detectionId: artifact.detectionId,
    });
  };
  return {
    h,
    contracts,
    category,
    seeded,
    forcedSource,
    state,
    refs,
    init,
    bind,
    finish,
    cancel,
    prepareCarriages,
    scriptPublications,
    remove,
    artifact,
    prepareArtifact,
  };
};

describe("minFee wrongful rejection registered lifecycle", () => {
  it.each([999n, 1_000n])(
    "contradicts fee rejection at minimum %s through permanent proof and removal",
    async (minimum) => {
      const s = await setup(minimum);
      const initial = await s.init();
      await expect(
        s.bind(initial.nextThreadOutRef, {
          state: { ...s.state, min_fee_b: minimum + 1n },
        }),
      ).rejects.toThrow();
      await expect(
        s.bind(initial.nextThreadOutRef, {
          forcedSource: { ...s.forcedSource, direction: 0n },
        }),
      ).rejects.toThrow();
      if (minimum === 1_000n) {
        const source = s.forcedSource;
        for (const forcedSource of [
          {
            ...source,
            header: {
              ...source.header,
              blockSlot: source.header.blockSlot + 1n,
            },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              count: source.membership.count + 1n,
            },
          },
          {
            ...source,
            membership: { ...source.membership, phas_root: "ff".repeat(32) },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              key: {
                ...source.membership.key,
                outputIndex: source.membership.key.outputIndex + 1n,
              },
            },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              value: { ...source.membership.value, tx_id: "ff".repeat(32) },
            },
          },
          {
            ...source,
            membership: {
              ...source.membership,
              value: {
                ...source.membership.value,
                source: {
                  ...source.membership.value.source,
                  field_preimage_lengths_cbor: "80",
                },
              },
            },
          },
        ])
          await expect(
            s.bind(initial.nextThreadOutRef, { forcedSource }),
          ).rejects.toThrow();
      }
      const bound = await s.bind(initial.nextThreadOutRef);
      const completed = await captureEmulatorSubmission(s.h.emulator, () =>
        s.finish(bound.nextThreadOutRef),
      );
      expect(completed.result.minimumFee).toBe(minimum);
      const publications = await publishRemovalReferenceScripts({
        lucid: s.h.proverLucid,
        contracts: s.h.contracts,
      });
      const now = BigInt(s.h.emulator.now());
      await submitRemoveFraudulentBlock({
        lucid: s.h.proverLucid,
        blueprint: s.h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          s.h.contracts,
          s.h.catalogue,
          { removalReferenceScripts: publications.published },
        ),
        network,
        signer: s.h.proverSigner,
        fraudCategory: "minFee",
        fraudulentHeaderHash: s.seeded.headerHash,
        requireReferenceScripts: true,
        validFrom: now - 120_000n,
        validTo: now + 300_000n,
      });
      expect(
        await s.h.proverLucid.utxosAtWithUnit(
          s.h.contracts.stateQueue.spendingScriptAddress,
          s.seeded.stateQueueBlockUnit,
        ),
      ).toHaveLength(0);
      expect(
        await s.h.proverLucid.utxosAtWithUnit(
          s.contracts.fraudProof.spendingScriptAddress,
          completed.result.fraudProofUnit,
        ),
      ).toHaveLength(1);
    },
    600_000,
  );

  it.each([358, 378, 379, 819, -80, -64])(
    "measures forced field boundary %s",
    async (inputCount) => {
      const s = await setup(
        1_000n,
        inputCount === -64 ? 819 : Math.abs(inputCount),
        inputCount === -80,
        inputCount === -64 ? 64 : 0,
      );
      const init = await captureEmulatorSubmission(s.h.emulator, s.init);
      const bind = await captureEmulatorSubmission(s.h.emulator, () =>
        s.bind(init.result.nextThreadOutRef),
      );
      const publication = await captureEmulatorSubmission(
        s.h.emulator,
        s.prepareCarriages,
      );
      const finished = await captureEmulatorSubmission(s.h.emulator, () =>
        s.finish(bind.result.nextThreadOutRef),
      );
      const removal = await s.remove();
      const stages = [
        ...s.scriptPublications,
        ...init.measurements,
        ...bind.measurements,
        ...publication.measurements,
        ...finished.measurements,
        ...removal,
      ];
      for (const row of stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15_872);
        expect(row.executionMemory).toBeLessThanOrEqual(13_200_000n);
        expect(row.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
      }
      fitRows.push({
        shape:
          inputCount === -64
            ? "maximum-proof-64-and-certified-field"
            : inputCount === -80
              ? "all-nine-populated"
              : `field0-${inputCount}`,
        stages,
      });
    },
    600_000,
  );

  it("proves the exact nonzero-slope fee boundary through all nine authenticated lengths", async () => {
    const s = await setup(1_000n, 1, false, 0, true);
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef);
    const result = await s.finish(bound.nextThreadOutRef);
    expect(result.minimumFee).toBe(1_000n);
    expect(result.canonicalTxSize + s.state.min_fee_b).toBe(1_000n);
    await s.remove();
  }, 600_000);

  it("reopens serialized forced evidence on restart and refuses every source substitution", async () => {
    const s = await setup(1_000n);
    const prepared = await s.prepareArtifact();
    expect(prepared).toEqual(s.artifact);
    const stored = JSON.parse(JSON.stringify(prepared));
    const admitted = await admitMinFeeForcedArtifact(stored);
    expect(admitted.evidence.state).toEqual(s.state);
    for (const mutation of [
      { ...stored, headerHash: "ff".repeat(28) },
      { ...stored, detectionId: "min-fee:forced:1:other" },
      { ...stored, fullTransactionCbor: stored.fullTransactionCbor + "00" },
      { ...stored, forcedSourceCbor: stored.forcedSourceCbor + "00" },
      { ...stored, unexpected: true },
    ])
      await expect(admitMinFeeForcedArtifact(mutation)).rejects.toThrow();
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef, {
      state: admitted.evidence.state,
      forcedSource: admitted.forcedSource,
    });
    // A fresh admission re-derives the exact same checkpoint after the bind.
    const resumed = await admitMinFeeForcedArtifact(
      JSON.parse(JSON.stringify(stored)),
    );
    const [utxo] = await s.h.proverLucid.utxosByOutRef([
      {
        txHash: bound.nextThreadOutRef.split("#")[0]!,
        outputIndex: Number(bound.nextThreadOutRef.split("#")[1]),
      },
    ]);
    expect(Data.from(utxo!.datum!, SDK.MinFeeStep02Datum).data).toEqual(
      resumed.evidence.state,
    );
    await s.finish(bound.nextThreadOutRef);
    await s.remove();
  }, 600_000);

  it("refuses the honest one-lovelace-below rejection on chain and cancels at both steps", async () => {
    const s = await setup(1_001n);
    const first = await s.init();
    await s.cancel(first.nextThreadOutRef, 0);
    const initial = await s.init();
    const bound = await s.bind(initial.nextThreadOutRef);
    await expect(s.finish(bound.nextThreadOutRef, true)).rejects.toThrow();
    expect(
      await s.h.proverLucid.utxosAtWithUnit(
        s.h.contracts.stateQueue.spendingScriptAddress,
        s.seeded.stateQueueBlockUnit,
      ),
    ).toHaveLength(1);
    await s.cancel(bound.nextThreadOutRef, 1);
  }, 600_000);
});

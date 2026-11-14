import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  midgardNativeTxProofFieldPreimageLengths,
} from "@al-ft/midgard-core";
import {
  type FieldPreimageLengthMismatchFaultProofContracts,
  type Header,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { ManifestBoundFieldPreimageLengthConfig } from "../src/field-preimage-length-mismatch/config.js";
import { prepareAcceptedFieldPreimageLengthMismatch } from "../src/field-preimage-length-mismatch/prepare-accepted.js";
import { encodeL2TransactionSourceValue } from "../src/prepare-double-spend.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/step-support.js";
import {
  registeredContracts,
  type SetupOptions,
} from "./field-preimage-length-mismatch-lifecycle.registered-contracts.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildFieldPreimageLengthForcedFixture } from "./support/field-preimage-length-mismatch-forced-fixture.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  countedTransactionsRoot,
  createRecordingLeaseCoordinator,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

export const setup = async ({
  forced = false,
  acceptedPreimageBytes,
  honestAccepted = false,
  forcedLeaf,
  forcedDaFixture,
}: SetupOptions = {}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realFieldPreimageLengthMismatch: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const { chain, category } = await registeredContracts(harness);
  const operator = async () => {
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("missing funder key");
    return {
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
    };
  };
  const forcedFixture = forced
    ? await buildInvalidForcedTransitionTraceFixture({
        ...(await operator()),
        fieldPreimageLengthMismatchIndex: 0,
        headerDurationMs: 300_000,
      })
    : undefined;
  const familyForced =
    forcedLeaf === undefined
      ? undefined
      : await buildFieldPreimageLengthForcedFixture({
          ...(await operator()),
          verdict: forcedLeaf.verdict,
          ...(forcedLeaf.mismatch
            ? {
                lengthsMutation: (lengths: number[]) => [
                  lengths[0]! + 1,
                  ...lengths.slice(1),
                ],
              }
            : {}),
        });
  const authenticatedForcedDa =
    forcedDaFixture === undefined
      ? undefined
      : await forcedDaFixture(await operator());
  const forcedHeader =
    authenticatedForcedDa?.header ??
    forcedFixture?.header ??
    familyForced?.header;
  const baseMaterial = committedFieldShapeScenarioMaterial("honest");
  if (baseMaterial.fullTx === null || baseMaterial.canonicalTx === null)
    throw new Error("missing canonical tx");
  const material =
    acceptedPreimageBytes === undefined
      ? baseMaterial
      : (() => {
          const canonical = {
            ...baseMaterial.canonicalTx,
            body: {
              ...baseMaterial.canonicalTx.body,
              spendInputsPreimageCbor:
                acceptedPreimageBytes === 32_768
                  ? encodeMidgardFieldPreimage([
                      encodeCbor(Buffer.alloc(32_761, 0xa5)),
                    ])
                  : Buffer.alloc(acceptedPreimageBytes, 0xa5),
            },
          };
          const fullTx = materializeMidgardNativeTxFromCanonical(canonical);
          return {
            ...baseMaterial,
            canonicalTx: canonical,
            fullTx,
            compact: fullTx.compact,
            committedPreimage: Buffer.from(fullTx.body.spendInputsPreimageCbor),
          };
        })();
  const materialFullTx = material.fullTx;
  if (materialFullTx === null) throw new Error("missing material full tx");
  expect(material.fieldIndex).toBe(0);
  const nativeTxId = computeMidgardNativeTxId(material.compact).toString("hex");
  const honestLengths = [
    ...midgardNativeTxProofFieldPreimageLengths({
      body: materialFullTx.body,
      witnessSet: materialFullTx.witnessSet,
    }),
  ];
  const lengths = [...honestLengths];
  if (!honestAccepted) {
    lengths[material.fieldIndex] = lengths[material.fieldIndex]! + 1;
  }
  const proofSource = (fieldLengths: readonly number[]) => ({
    compactCbor: encodeMidgardNativeTxCompact(material.compact),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(materialFullTx.witnessSet),
    ),
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
      ...fieldLengths,
    ]),
  });
  const sourceCbor = encodeL2TransactionSourceValue({
    txId: nativeTxId,
    proofSource: proofSource(lengths),
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(sourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const fraudulent =
    forcedHeader === undefined
      ? await setupFraudulentBlock({
          funderLucid: harness.funderLucid,
          emulator: harness.emulator,
          contracts: harness.contracts,
          catalogue: harness.catalogue,
          fixture: {
            transactionsRoot,
            l2TransactionCount: 1n,
            headerDurationMs: 300_000,
          },
        })
      : await submitSetupTx({
          lucid: harness.funderLucid,
          contracts: harness.contracts,
          nonceUtxo: harness.nonceUtxo,
          catalogue: harness.catalogue,
          header: forcedHeader,
        });
  const references = [];
  for (const [index, step] of chain.steps.entries()) {
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `field-preimage-length-step-${index.toString()}`,
        })
      ).utxo,
    );
  }
  const acceptedPrepared =
    forcedFixture === undefined &&
    acceptedPreimageBytes === undefined &&
    !honestAccepted
      ? await prepareAcceptedFieldPreimageLengthMismatch({
          headerHash: fraudulent.headerHash,
          committedTransactionsRoot: await countedTransactionsRoot(
            transactionsRoot,
            1n,
          ),
          l2TransactionCount: 1n,
          entries: [[nativeTxId, sourceCbor]],
          transactionId: nativeTxId,
          canonicalTransactionCbor:
            encodeMidgardNativeTxCanonical(materialFullTx),
          fieldIndex: material.fieldIndex,
        })
      : undefined;
  const scenario = {
    canonicalTx: material.canonicalTx,
    fullTx: material.fullTx,
    nativeTxId,
    fieldIndex: material.fieldIndex,
    committedPreimage: material.committedPreimage,
    referenceInputsPreimage: Buffer.from(
      materialFullTx.body.referenceInputsPreimageCbor,
    ),
    honestLengths,
    lengths,
    witnessSetCompactCbor: proofSource(lengths).witnessSetCompactCbor,
    /** The same transaction re-keyed under the honest vector: not in the PHAS. */
    substitutedSourceCbor: encodeL2TransactionSourceValue({
      txId: nativeTxId,
      proofSource: proofSource(honestLengths),
    }),
    inclusion: {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(material.compact),
      nativeTxCompactCbor: encodeMidgardNativeTxCompact(
        material.compact,
      ).toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    },
  };
  const contracts: FieldPreimageLengthMismatchFaultProofContracts = {
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    fieldPreimageCertificate: harness.contracts.fieldPreimageCertificate,
    fieldPreimageLengthMismatch: {
      ...chain,
      acceptedStep02: chain.steps[1],
      forcedStep02: chain.steps[2],
    },
  };
  const config = {
    schemaVersion:
      "midgard-field-preimage-length-mismatch-production-config-v1",
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    binding: {
      blueprint: harness.realBlueprint,
      network,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      definition: {
        headerHash: fraudulent.headerHash,
        stateQueue: { policyId: harness.contracts.stateQueue.policyId },
      },
      resolvedContracts: {
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        category,
      },
    },
    contracts,
    referenceScripts: {
      step01: references[0],
      step02Accepted: references[1],
      step02Forced: references[2],
      step03: references[3],
      witnesses: harness.witnessReferenceScripts,
    },
  } as unknown as ManifestBoundFieldPreimageLengthConfig;
  return {
    harness,
    config,
    fraudulent,
    scenario,
    forcedFixture,
    familyForced,
    authenticatedForcedDa,
    acceptedPrepared,
    sourceCbor,
    transactionsRoot,
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(materialFullTx),
    fraudulentHeader:
      forcedHeader ??
      (fraudulent as unknown as { readonly header: Header }).header,
  };
};

type Fixture = Awaited<ReturnType<typeof setup>>;

export const removeFraudulentBlock = async (
  fixture: Pick<Fixture, "harness" | "fraudulent">,
  { leased = false }: { readonly leased?: boolean } = {},
) => {
  const removalReferences = await publishRemovalReferenceScripts({
    lucid: fixture.harness.proverLucid,
    contracts: fixture.harness.contracts,
  });
  // A registered family resolves removal through the canonical catalogue:
  // the manifest's fraudProofFieldPreimageLengthMismatch entries carry the
  // registered chain the harness folded into the catalogue root.
  return await captureEmulatorSubmission(fixture.harness.emulator, () =>
    submitRemoveFraudulentBlock({
      lucid: fixture.harness.proverLucid,
      blueprint: fixture.harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        fixture.harness.contracts,
        fixture.harness.catalogue,
        { removalReferenceScripts: removalReferences.published },
      ),
      network,
      signer: fixture.harness.proverSigner,
      fraudCategory: "fieldPreimageLengthMismatch",
      fraudulentHeaderHash: fixture.fraudulent.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      ...(leased
        ? {
            stateQueueMutationLeaseCoordinator: createRecordingLeaseCoordinator(
              [],
            ),
          }
        : {}),
      validFrom: BigInt(Math.max(0, fixture.harness.emulator.now() - 120_000)),
      validTo: BigInt(fixture.harness.emulator.now() + 300_000),
    }),
  );
};

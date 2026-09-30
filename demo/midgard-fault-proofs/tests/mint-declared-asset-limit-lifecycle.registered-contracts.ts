import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardMintPolicyItem,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  applyMintDeclaredAssetLimitScripts,
  MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
  type MintDeclaredAssetLimitContracts,
} from "../src/mint-declared-asset-limit/contracts.js";
import { prepareMintDeclaredAssetLimitEvidence } from "../src/mint-declared-asset-limit/family.js";
import { nativeTxFromCoreCompact } from "../src/step-support.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
} from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";

export const network = "Custom" as const;

export const firstStepDeploymentEntry = "fraudProofMintDeclaredAssetLimit";

export const REASON_ARM = "MintDeclaredAssetLimit";

export const coverage = createLifecycleCoverageRecorder();

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

type Capture = Awaited<ReturnType<typeof captureEmulatorSubmission>>;

export type Measurement = Capture["measurement"];

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.mintDeclaredAssetLimit;
  const category = harness.catalogue.categories.mintDeclaredAssetLimit;
  expectRegisteredChainParity({
    registered,
    applied: applyMintDeclaredAssetLimitScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const applied = familyStepsFromRegisteredChain(
    registered.steps,
    MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
  );
  const contracts: MintDeclaredAssetLimitContracts = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  // Reference scripts are published only after the block under dispute is
  // set up: the funder's publications must not consume the harness nonce.
  return {
    applied,
    contracts,
    catalogue: harness.catalogue,
    category,
    references: undefined as unknown as readonly [UTxO, UTxO, UTxO, UTxO],
    certificateReference: undefined as unknown as UTxO,
  };
};

type Registered = Awaited<ReturnType<typeof registeredContracts>>;

/** Publishes the four applied steps and the certificate mint by reference. */
export const publishFamilyReferences = async (
  harness: Harness,
  registered: Registered,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of registered.applied.entries())
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `mint-declared-lifecycle-${index.toString()}`,
        })
      ).utxo,
    );
  registered.references = references as unknown as readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
  registered.certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: "mint-declared-lifecycle-certificate",
    })
  ).utxo;
  return registered.references;
};

const policy = (byte: number) => Buffer.alloc(28, byte);

export const singleton = (byte: number) =>
  encodeMidgardMintPolicyItem({
    policyId: policy(byte),
    assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
  });

/** `count` canonical two-byte asset names in ascending order. */
export const wide = (byte: number, count: number) =>
  encodeMidgardMintPolicyItem({
    policyId: policy(byte),
    assets: Array.from({ length: count }, (_, index) => ({
      assetName: Buffer.from([index >> 8, index & 255]),
      quantity: 1n,
    })),
  });

/**
 * A policy item whose canonical map header declares `count` (>= 256) assets
 * over `padding` body bytes the machine never reads when the header decides.
 */
export const declaring = (byte: number, count: number, padding: number) =>
  Buffer.concat([
    Buffer.from([0x82, 0x58, 0x1c]),
    policy(byte),
    Buffer.from([0xb9, count >> 8, count & 255]),
    Buffer.alloc(Math.max(1, padding), 0),
  ]);

/** Pads the declaring target so the field-5 preimage is exactly `total` bytes. */
export const fieldOfExactly = (
  prefix: readonly Buffer[],
  targetByte: number,
  declaredCount: number,
  total: number,
) => {
  let padding = 1;
  let field = encodeMidgardFieldPreimage([
    ...prefix,
    declaring(targetByte, declaredCount, padding),
  ]);
  for (let attempt = 0; attempt < 3 && field.length !== total; attempt += 1) {
    padding += total - field.length;
    field = encodeMidgardFieldPreimage([
      ...prefix,
      declaring(targetByte, declaredCount, padding),
    ]);
  }
  expect(field).toHaveLength(total);
  return field;
};

export const mintTx = (mintField: Buffer, fee: bigint) => {
  const base = makeNativeTx({ spendInputCbors: [], fee });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: { ...base.body, mintPreimageCbor: mintField },
    witnessSet: base.witnessSet,
  });
  return {
    nativeTx,
    id: computeMidgardNativeTxId(nativeTx).toString("hex"),
    compactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex"),
    sourceCbor: l2TransactionSourceCborV1(nativeTx),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
    ).toString("hex"),
    mintField,
    commitmentHex: midgardFieldCommitment(mintField).toString("hex"),
  };
};

type MintTx = ReturnType<typeof mintTx>;

/** One accepted block over several L2 transactions, with a proof for each. */
export const acceptedBlock = async (txs: readonly MintTx[]) => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const tx of txs)
    await trie.insert(
      Buffer.from(tx.id, "hex"),
      Buffer.from(tx.sourceCbor, "hex"),
    );
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusions = [];
  for (const tx of txs) {
    const proof = await trie.prove(Buffer.from(tx.id, "hex"));
    const proofCbor = proof.toCBOR().toString("hex");
    inclusions.push({
      nativeTxId: tx.id,
      nativeTx: nativeTxFromCoreCompact(tx.nativeTx.compact),
      nativeTxCompactCbor: tx.compactCbor,
      l2TransactionSourceCbor: tx.sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proofCbor, Proof),
      txMembershipProofCbor: proofCbor,
    });
  }
  return { transactionsRoot, inclusions };
};

export const acceptedEvidence = (tx: MintTx, policyIndex: number) =>
  prepareMintDeclaredAssetLimitEvidence({
    finding: { subject: acceptedVerdictSubject(tx.id), policyIndex },
    fieldPreimage: tx.mintField,
    committedFieldHashHex: tx.commitmentHex,
  });

/** Publishes a field's carriage (and, when certified, its certificate). */
export const publishCarriage = async (
  harness: Harness,
  registered: Registered,
  tx: Pick<MintTx, "id" | "compactCbor"> &
    Partial<Pick<MintTx, "witnessSetCompactCbor">>,
  items: readonly Buffer[],
  label: string,
  sourceKind: 0n | 1n,
) => {
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: sourceKind,
    fieldIndex: 5,
    anchorTxId: tx.id,
    nativeTxCompactCbor: tx.compactCbor,
    itemCbors: items,
    owner: harness.proverSigner.paymentKeyHash,
    publish: true,
    label,
  });
  const carriage = await captureEmulatorSubmission(harness.emulator, () =>
    publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label,
    }),
  );
  const certificate =
    planned.plan.tier === "Certified"
      ? await captureEmulatorSubmission(harness.emulator, () =>
          certifyFaultProofFieldCarriage({
            lucid: harness.proverLucid,
            network,
            signer: harness.proverSigner,
            planned,
            certificatePolicyId:
              harness.contracts.fieldPreimageCertificate.policyId,
            certificateMintingScript:
              harness.contracts.fieldPreimageCertificate.mintingScript,
            certificateReferenceScriptUtxo: registered.certificateReference,
            chunkUtxos: carriage.result,
            compactCbor: tx.compactCbor,
            witnessSetCompactCbor: tx.witnessSetCompactCbor!,
          }),
        )
      : undefined;
  return { planned, carriage, certificate };
};

export const progress = (message: string) => {
  if (process.env.MIDGARD_PRINT_FIT === "1")
    console.info(`[mint-declared-lifecycle] ${message}`);
};

export const measuredFit = createMeasuredFitRecorder(
  "mint-declared-asset-limit",
  "lifecycle",
  "62 policies with 1000-asset first policy, exact 32768-byte certified field and 192-unit fold; both directions",
);

export const printLedger = (
  label: string,
  rows: readonly (readonly [string, Measurement])[],
) => {
  rows.forEach(([name, measurement], index) =>
    measuredFit.record(
      `${label}/${index}-${name}`,
      measurement,
      measurement.executionMemory === 0n ? "publication" : "lifecycle",
    ),
  );
  if (process.env.MIDGARD_PRINT_FIT === "1")
    console.info(
      `[${label}] ${JSON.stringify(rows, (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      )}`,
    );
};

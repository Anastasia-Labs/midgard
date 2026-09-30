import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardRedeemerWitnessItem,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  midgardNativeTxProofFieldPreimageLengths,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { CommittedFieldShapeContracts } from "../../src/committed-field-shape/contracts.js";
import { type CommittedFieldShapeCatalogueCategory } from "../../src/committed-field-shape/submit-common.js";
import { encodeL2TransactionSourceValue } from "../../src/prepare-double-spend.js";
import { resolveProverSigner } from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { l2TransactionSourceCbor as l2TransactionSourceCborV1 } from "./emulator/native-tx.js";
import { setupFraudulentBlock } from "./submit-init-emulator-fixtures.js";
import {
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

export const EMPTY = Buffer.from("80", "hex");

export type CommittedFieldShapeEmulatorHarness = Awaited<
  ReturnType<typeof makeCommittedFieldShapeEmulatorHarness>
>;

/** Builds the real two-step chain plus a third, initially empty, wallet. */
export const makeCommittedFieldShapeEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realCommittedFieldShape: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const committedFieldShape = harness.contracts.committedFieldShape;
  const category = harness.catalogue.categories.committedFieldShape as
    | CommittedFieldShapeCatalogueCategory
    | undefined;
  if (committedFieldShape === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the committed-field-shape contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.committedFieldShape
  ) {
    throw new Error("Unexpected committed-field-shape category id");
  }
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return {
    ...harness,
    committedFieldShape,
    category,
    outsiderLucid,
    outsiderSigner,
  };
};

/** Both step validators are always published and consumed by reference. */
export const publishCommittedFieldShapeReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: CommittedFieldShapeContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const publications: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript,
      label: `committed-field-shape step-0${(index + 1).toString()}`,
    });
    publications.push(utxo);
  }
  return publications as unknown as readonly [UTxO, UTxO];
};

export type CommittedFieldShapeScenarioKind =
  | "wrong-stride"
  | "honest"
  | "field-item-width-illegal"
  | "redeemer-canonicity"
  | "non-envelope";

export type CommittedFieldShapeScenario = {
  readonly kind: CommittedFieldShapeScenarioKind;
  readonly canonicalTx: MidgardNativeTxCanonical | null;
  readonly fullTx: MidgardNativeTxFull | null;
  readonly nativeTxId: string;
  readonly compactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly inclusion: SubmitStep01TxInclusion;
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly setup: Awaited<ReturnType<typeof setupFraudulentBlock>>;
};

const invalidNonEnvelopeCanonical = (): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY,
    referenceInputsPreimageCbor: EMPTY,
    outputsPreimageCbor: Buffer.from("8041", "hex"),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY,
    requiredSignersPreimageCbor: EMPTY,
    mintPreimageCbor: EMPTY,
    scriptIntegrityHash: Buffer.alloc(32),
    auxiliaryDataHash: Buffer.alloc(32),
    networkId: 0n,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY,
    scriptTxWitsPreimageCbor: EMPTY,
    redeemerTxWitsPreimageCbor: EMPTY,
  },
});

export const committedFieldShapeScenarioMaterial = (
  kind: CommittedFieldShapeScenarioKind,
): {
  readonly canonicalTx: MidgardNativeTxCanonical | null;
  readonly fullTx: MidgardNativeTxFull | null;
  readonly compact: ReturnType<typeof deriveMidgardNativeTxCompact>;
  readonly l2TransactionSourceCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
} => {
  if (kind === "non-envelope") {
    const invalid = invalidNonEnvelopeCanonical();
    const compact = deriveMidgardNativeTxCompact(
      invalid.body,
      invalid.witnessSet,
      invalid.validity,
    );
    const nativeTxId = computeMidgardNativeTxId(compact).toString("hex");
    return {
      canonicalTx: null,
      fullTx: null,
      compact,
      l2TransactionSourceCbor: encodeL2TransactionSourceValue({
        txId: nativeTxId,
        proofSource: {
          compactCbor: encodeMidgardNativeTxCompact(compact),
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
            deriveMidgardNativeTxWitnessSetCompact(invalid.witnessSet),
          ),
          fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
            midgardNativeTxProofFieldPreimageLengths({
              body: invalid.body,
              witnessSet: invalid.witnessSet,
            }),
          ),
        },
      }),
      fieldIndex: 2,
      committedPreimage: Buffer.from(invalid.body.outputsPreimageCbor),
    };
  }
  if (kind === "field-item-width-illegal") {
    const oversizedOutput = encodeMidgardTxOutput({
      address: Buffer.from(`60${"00".repeat(28)}`, "hex"),
      value: { lovelace: 2_000_000n, assets: new Map() },
      datum: {
        kind: "inline",
        cbor: Buffer.concat([
          Buffer.from("5f", "hex"),
          ...Array.from({ length: 256 }, () =>
            Buffer.concat([Buffer.from("5840", "hex"), Buffer.alloc(64)]),
          ),
          Buffer.from("ff", "hex"),
        ]),
      },
    });
    const fullTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbors: [oversizedOutput],
    });
    return {
      canonicalTx: fullTx,
      fullTx,
      compact: fullTx.compact,
      l2TransactionSourceCbor: l2TransactionSourceCborV1(fullTx),
      fieldIndex: 2,
      committedPreimage: Buffer.from(fullTx.body.outputsPreimageCbor),
    };
  }
  if (kind === "redeemer-canonicity") {
    // Exercise certified carriage at the real retained-DA frontier. The first
    // item has a valid redeemer envelope but a non-minimal Plutus integer;
    // retaining the full 224-item field proves that exact-coordinate access
    // remains bounded independently of unrelated trailing witnesses.
    const redeemerItems = Array.from({ length: 224 }, (_, index) =>
      encodeMidgardRedeemerWitnessItem({
        purpose: "Spend",
        index: BigInt(index),
        redeemerCbor:
          index === 0
            ? Buffer.from("1800", "hex")
            : Buffer.concat([Buffer.from("5840", "hex"), Buffer.alloc(64)]),
        executionUnits: { memory: 1_000_000n, steps: 1_000_000n },
      }),
    );
    const redeemerPreimage = encodeMidgardFieldPreimage(redeemerItems);
    const fullTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      redeemerTxWitsPreimageCbor: redeemerPreimage,
    });
    return {
      canonicalTx: fullTx,
      fullTx,
      compact: fullTx.compact,
      l2TransactionSourceCbor: l2TransactionSourceCborV1(fullTx),
      fieldIndex: 8,
      committedPreimage: Buffer.from(
        fullTx.witnessSet.redeemerTxWitsPreimageCbor,
      ),
    };
  }
  const spendItem =
    kind === "wrong-stride"
      ? Buffer.from("deadbeef", "hex")
      : Buffer.alloc(38, 0xa5);
  const fullTx = makeNativeTx({ spendInputCbors: [spendItem], fee: 7n });
  return {
    canonicalTx: fullTx,
    fullTx,
    compact: fullTx.compact,
    l2TransactionSourceCbor: encodeL2TransactionSourceValue({
      txId: computeMidgardNativeTxId(fullTx).toString("hex"),
      proofSource: {
        compactCbor: encodeMidgardNativeTxCompact(fullTx.compact),
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
          deriveMidgardNativeTxWitnessSetCompact(fullTx.witnessSet),
        ),
        fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
          midgardNativeTxProofFieldPreimageLengths({
            body: fullTx.body,
            witnessSet: fullTx.witnessSet,
          }),
        ),
      },
    }),
    fieldIndex: 0,
    committedPreimage: Buffer.from(fullTx.body.spendInputsPreimageCbor),
  };
};

/** Commits the chosen real shape as a one-leaf transactions MPF and block. */
export const setupCommittedFieldShapeScenario = async ({
  harness,
  kind,
}: {
  readonly harness: CommittedFieldShapeEmulatorHarness;
  readonly kind: CommittedFieldShapeScenarioKind;
}): Promise<CommittedFieldShapeScenario> => {
  const material = committedFieldShapeScenarioMaterial(kind);
  const nativeTxId = computeMidgardNativeTxId(material.compact).toString("hex");
  const compact = encodeMidgardNativeTxCompact(material.compact);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(material.l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusion: SubmitStep01TxInclusion = {
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(material.compact),
    nativeTxCompactCbor: compact.toString("hex"),
    l2TransactionSourceCbor: material.l2TransactionSourceCbor,
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(proof.toCBOR().toString("hex"), SDK.Proof),
    txMembershipProofCbor: proof.toCBOR().toString("hex"),
  };
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture: {
      transactionsRoot,
      l2TransactionCount: 1n,
      ...(kind === "redeemer-canonicity" ? { headerDurationMs: 60_000 } : {}),
    },
  });
  return {
    kind,
    canonicalTx: material.canonicalTx,
    fullTx: material.fullTx,
    nativeTxId,
    compactCbor: compact.toString("hex"),
    fieldIndex: material.fieldIndex,
    committedPreimage: material.committedPreimage,
    inclusion,
    transactionsRoot,
    l2TransactionCount: 1n,
    setup,
  };
};

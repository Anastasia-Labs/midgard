import {
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCarriageBounds,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { type Script, type UTxO } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import type {
  MissingSignatureFinding,
  MissingSignatureProverDeps,
  MissingSignatureProverEvent,
  MissingSignatureProverPolicy,
} from "../../src/missing-signature/index.js";
import {
  MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
  MissingSignatureProvability,
} from "../../src/missing-signature/index.js";
import {
  buildDecodingBlockFixture,
  type DecodingBlockFixture,
} from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const MISSING_SIGNATURE_TARGET_VKEY = "11".repeat(32);

export const MISSING_SIGNATURE_TARGET_HASH = SDK.missingSignatureVkeyHash(
  MISSING_SIGNATURE_TARGET_VKEY,
);

/** First 103-byte-stride field-7 vector that crosses the tier-2 ceiling. */
export const MISSING_SIGNATURE_FIRST_CERTIFIED_WITNESS_COUNT =
  Math.floor(
    (midgardFieldCarriageBounds.maxPublishableCarriageBytes - 3) / 103,
  ) + 1;

/** First field-7 vector that is too large for tier 1 and must publish. */
export const MISSING_SIGNATURE_FIRST_RAW_WITNESS_COUNT =
  Math.floor(
    (midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes - 3) / 103,
  ) + 1;

/** Widest canonical field-7 vector admitted by the 32,768-byte field cap. */
export const MISSING_SIGNATURE_MAX_ADMISSIBLE_WITNESS_COUNT = Math.floor(
  (midgardFieldCarriageBounds.maxTransactionAggregateFieldBytes - 3) / 103,
);

const decoyWitness = (index: number): SDK.MidgardAddressWitness => {
  const vkey = Buffer.alloc(32);
  vkey.writeUInt32BE(index + 1, 28);
  return {
    verification_key: vkey.toString("hex"),
    signature: Buffer.alloc(64, (index % 254) + 1).toString("hex"),
  };
};

export const buildMissingSignatureSubject = ({
  honest = false,
  decoyWitnessCount = 0,
}: {
  readonly honest?: boolean;
  readonly decoyWitnessCount?: number;
} = {}): {
  readonly nativeTx: MidgardNativeTxFull;
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly SDK.MidgardAddressWitness[];
  readonly witnessSetCompact: SDK.NativeTxWitnessSetCompact;
} => {
  const addrTxWits: SDK.MidgardAddressWitness[] = [
    ...Array.from({ length: decoyWitnessCount }, (_unused, index) =>
      decoyWitness(index),
    ),
    ...(honest
      ? [
          {
            verification_key: MISSING_SIGNATURE_TARGET_VKEY,
            signature: "ff".repeat(64),
          },
        ]
      : []),
  ];
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: encodeCbor([
        Buffer.from(MISSING_SIGNATURE_TARGET_HASH, "hex"),
      ]),
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: SDK.encodeAddressWitnessPreimage(addrTxWits),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    nativeTx,
    requiredSignerHashes: [MISSING_SIGNATURE_TARGET_HASH],
    addrTxWits,
    witnessSetCompact: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
  };
};

export const makeMissingSignatureEmulatorHarness = async ({
  useScalusEvaluator = false,
}: {
  /**
   * The default Aiken/WASM evaluator is the faster of the two by a wide
   * margin on the multi-scan frontier and, since `@lucid-evolution/uplc`
   * 0.2.23, no longer leaks its arena per evaluation. Scalus remains
   * available for measurements that want a second evaluator's reading.
   */
  readonly useScalusEvaluator?: boolean;
} = {}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMissingSignature: true,
      alwaysFraudProofCatalogue: true,
    },
    // Lucid's preflight evaluator only; Emulator still independently runs
    // every submitted script through phase two.
    ...(useScalusEvaluator
      ? { lucidOptions: { evaluator: createScalusEvaluator() } }
      : {}),
  });
  const missingSignature = harness.contracts.missingSignature;
  const category = harness.catalogue.categories.missingSignature;
  if (missingSignature === undefined || category === undefined) {
    throw new Error(
      "missing-signature harness contracts/category were omitted",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingSignature
  ) {
    throw new Error("unexpected missing-signature category id");
  }
  return { ...harness, missingSignature, category };
};

export const publishMissingSignatureReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >["missingSignature"];
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const publications: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `missing-signature step-0${(index + 1).toString()}`,
    });
    publications.push(utxo);
  }
  return publications as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

export type MissingSignatureScenario = {
  readonly subject: ReturnType<typeof buildMissingSignatureSubject>;
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
};

export const setupMissingSignatureScenario = async ({
  harness,
  honest = false,
  decoyWitnessCount = 0,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly honest?: boolean;
  readonly decoyWitnessCount?: number;
}): Promise<MissingSignatureScenario> => {
  const subject = buildMissingSignatureSubject({
    honest,
    decoyWitnessCount,
  });
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return { subject, block, setup };
};

export const missingSignatureFinding = (
  scenario: MissingSignatureScenario,
): MissingSignatureFinding => ({
  headerHash: scenario.setup.headerHash,
  eventKey: {
    L2TransactionEventKey: { tx_id: scenario.block.nativeTxId },
  },
  fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  txId: scenario.block.nativeTxId,
  nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
  accusedRequiredSignerIndex: 0n,
  accusedRequiredSignerHash: MISSING_SIGNATURE_TARGET_HASH,
  resolvedVkey: MISSING_SIGNATURE_TARGET_VKEY,
  committedWitnessSetHash:
    scenario.subject.nativeTx.compact.transactionWitnessSetHash.toString("hex"),
  provability: MissingSignatureProvability.MissingWitness,
  estimatedThreadTxCount:
    5 +
    Math.floor(
      Math.max(0, scenario.subject.addrTxWits.length - 1) /
        SDK.MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
    ),
});

export const MISSING_SIGNATURE_EMULATOR_PROVER_POLICY: MissingSignatureProverPolicy =
  {
    ...MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
    minSettlementDepth: 0n,
    maxThreadBudgetLovelace: null,
  };

export const missingSignatureProverDeps = ({
  harness,
  scenario,
  referenceScriptUtxos,
  journal,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly scenario: MissingSignatureScenario;
  readonly referenceScriptUtxos: MissingSignatureProverDeps["referenceScriptUtxos"];
  readonly journal?: (event: MissingSignatureProverEvent) => void;
}): MissingSignatureProverDeps => ({
  lucid: harness.proverLucid,
  blueprint: harness.realBlueprint,
  network,
  contracts: harness.missingSignature,
  category: harness.category,
  catalogue: {
    policyId: harness.contracts.fraudProofCatalogue.policyId,
    spendingScriptAddress:
      harness.contracts.fraudProofCatalogue.spendingScriptAddress,
    root: harness.catalogue.root,
  },
  signer: harness.proverSigner,
  evidence: {
    txInclusion: async () => {
      if (scenario.block.txInclusion === null) {
        throw new Error("normal missing-signature fixture has no inclusion");
      }
      return scenario.block.txInclusion;
    },
    subjectTx: async () => ({
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      requiredSignerHashes: scenario.subject.requiredSignerHashes,
      addrTxWits: scenario.subject.addrTxWits,
      witnessSetCompact: scenario.subject.witnessSetCompact,
    }),
  },
  observations: {},
  journal: journal ?? (() => undefined),
  policy: MISSING_SIGNATURE_EMULATOR_PROVER_POLICY,
  referenceScriptUtxos,
  witnessReferenceScripts: harness.witnessReferenceScripts,
});

import { type MidgardLedgerOutputReferenceScriptLanguage } from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  encodeMidgardTxInputCanonical,
  faultProofStepRedeemerSchema,
  type MidgardTxInput,
} from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  Lucid,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { NativeScriptDecodingContracts } from "../../src/native-script-decoding/contracts.js";
import {
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
  type NativeScriptDecodingProverDeps,
  type NativeScriptDecodingProverEvent,
  type NativeScriptDecodingProverPolicy,
} from "../../src/native-script-decoding/prover.js";
import { resolveProverSigner } from "../../src/runtime.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.build-decoding-block-fixture.js";
import {
  buildDecodingLedgerFixture,
  type DecodingBlockFixture,
  type DecodingLedgerFixture,
  decodingSubjectTransaction,
} from "./native-script-decoding-emulator.build-decoding-ledger-fixture.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network as emulatorNetwork,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/**
 * The decoding-family harness: the real six-validator chain built from the
 * regenerated blueprint and registered in its canonical production catalogue
 * category.
 */
export const makeDecodingEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNativeScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
    // The #545 published-chunk carriage withdraws from the merkelized
    // verifier's reward account, which must be registered before any step
    // takes that route.
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const decoding = harness.contracts.nativeScriptDecoding;
  const category = harness.catalogue.categories.nativeScriptDecoding;
  if (decoding === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the native-script-decoding contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.nativeScriptDecoding
  ) {
    throw new Error("Unexpected native-script-decoding catalogue category id");
  }
  // The adversarial suite needs a THIRD party — a wallet that is neither the
  // funder nor the prover, and that must never be able to drive or cancel
  // somebody else's thread. It starts empty; `fundDecodingOutsider` fills
  // it once the setup transaction has consumed the harness nonce UTxO.
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network: emulatorNetwork,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return { ...harness, decoding, category, outsiderLucid, outsiderSigner };
};

/**
 * Publishes all six custody validators as reference scripts.
 */
export const publishDecodingReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: NativeScriptDecodingContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `native-script-decoding step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// The scenario: a committed block plus the pre-state ledger that resolves the
// accused outpoint, standing on the emulator and ready for Init.
// ---------------------------------------------------------------------------

/** The accused outpoint every scenario files, fixed so ids stay readable. */
export const DECODING_ACCUSED_TX_ID = "ab".repeat(32);

export type DecodingScenarioSource =
  | { readonly kind: "normal" }
  | {
      readonly kind: "forced";
      readonly verdict: SDK.OperatorVerdict;
      readonly orderKey?: SDK.OutputReference;
    };

export type DecodingScenario = {
  readonly ledger: DecodingLedgerFixture;
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly subjectFieldInputs: readonly MidgardTxInput[];
  /** Where the accused outpoint landed after the canonical §5.3 sort. */
  readonly accusedOrdinal: number;
  readonly accusedSourceKind: bigint;
  readonly referenceScriptItemBytes: Buffer;
};

/**
 * Commits the disputed block on the emulator over a pre-state ledger holding
 * the accused outpoint's descriptor. `accusedSourceKind` picks the §2.5 field
 * the accused ordinal indexes (0 = spend inputs, 1 = reference inputs). With
 * no decoys the accused outpoint sits at ordinal 0 of that field; decoys are
 * sorted in canonically, and `accusedOrdinal` reports where it landed.
 */
export const setupDecodingScenario = async ({
  harness,
  referenceScriptItemBytes,
  referenceScriptLanguage = 0,
  source,
  accusedSourceKind = 1n,
  accusedOutputIndex = 0,
  decoyTransactionCount = 0,
  decoySubjectInputCount = 0,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>;
  readonly referenceScriptItemBytes: Buffer;
  readonly referenceScriptLanguage?: Exclude<
    MidgardLedgerOutputReferenceScriptLanguage,
    -1
  >;
  readonly source: DecodingScenarioSource;
  readonly accusedSourceKind?: bigint;
  readonly accusedOutputIndex?: number;
  /** Extra committed L2 transactions, so the transactions trie proves in steps. */
  readonly decoyTransactionCount?: number;
  /**
   * Extra fabricated outpoints committed in the subject field beside the
   * accused one, so a test can grow the field's §5.1 preimage past the §8.4
   * tier-1 bound and let size alone select tier-2 carriage.
   */
  readonly decoySubjectInputCount?: number;
}): Promise<DecodingScenario> => {
  const { emulator, funderLucid, contracts, catalogue, nonceUtxo } = harness;
  const ledger = await buildDecodingLedgerFixture({
    txIdHex: DECODING_ACCUSED_TX_ID,
    outputIndex: accusedOutputIndex,
    referenceScriptItemBytes,
    referenceScriptLanguage,
  });
  const accused: MidgardTxInput = {
    tx_id: DECODING_ACCUSED_TX_ID,
    output_index: BigInt(accusedOutputIndex),
  };
  const subjectFieldInputs = [
    accused,
    ...Array.from(
      { length: decoySubjectInputCount },
      (_, index): MidgardTxInput => ({
        tx_id: (index + 1).toString(16).padStart(64, "0"),
        output_index: 0n,
      }),
    ),
  ].sort((left, right) =>
    Buffer.compare(
      encodeMidgardTxInputCanonical(left),
      encodeMidgardTxInputCanonical(right),
    ),
  );
  const accusedOrdinal = subjectFieldInputs.findIndex(
    (input) =>
      input.tx_id === accused.tx_id &&
      input.output_index === accused.output_index,
  );
  const subjectFieldCbors = subjectFieldInputs.map(
    encodeMidgardTxInputCanonical,
  );
  const nativeTx = decodingSubjectTransaction(
    accusedSourceKind === 0n
      ? { spendInputCbors: subjectFieldCbors, fee: 1_000n }
      : { referenceInputCbors: subjectFieldCbors, fee: 1_000n },
  );
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
      1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey: funderKeyHash,
    startTime,
    priorLedgerRoot: ledger.rootHex,
    decoyTransactionCount,
    subject:
      source.kind === "normal"
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: source.orderKey ?? {
              transactionId: "cd".repeat(32),
              outputIndex: 0n,
            },
            verdict: source.verdict,
          },
  });
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: block.header,
  });
  return {
    ledger,
    block,
    setup,
    subjectFieldInputs,
    accusedOrdinal,
    accusedSourceKind,
    referenceScriptItemBytes,
  };
};

// ---------------------------------------------------------------------------
// The §4.3 proving core, wired to a scenario
// ---------------------------------------------------------------------------

/** The emulator has no L1 depth or maturity to observe; both gates are off. */
export const DECODING_EMULATOR_PROVER_POLICY: NativeScriptDecodingProverPolicy =
  {
    ...NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
    minSettlementDepth: 0n,
    maturityGuardFactor: 0,
    maxThreadBudgetLovelace: null,
  };

/**
 * The proving core's dependencies for a scenario: every §4.3 evidence
 * callback is answered from the fixture, so what the core drives on chain is
 * exactly the committed material.
 */
export const decodingProverDeps = ({
  harness,
  scenario,
  referenceScriptItemBytes,
  referenceScriptUtxos,
  journal,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>;
  readonly scenario: DecodingScenario;
  /** `null` for the routes that never scan an item (§7.2, contradiction). */
  readonly referenceScriptItemBytes: Uint8Array | null;
  readonly referenceScriptUtxos?: NativeScriptDecodingProverDeps["referenceScriptUtxos"];
  readonly journal?: (event: NativeScriptDecodingProverEvent) => void;
}): NativeScriptDecodingProverDeps => ({
  lucid: harness.proverLucid,
  blueprint: harness.realBlueprint,
  network: emulatorNetwork,
  contracts: harness.decoding,
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
      const inclusion = scenario.block.txInclusion;
      if (inclusion === null) {
        throw new Error("this scenario's source is forced; it binds no leaf");
      }
      return inclusion;
    },
    reconstruction: async () => scenario.block.reconstruction,
    subjectTx: async () => ({
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
    }),
    descriptor: async () => ({
      descriptorCbor: scenario.ledger.descriptorCbor,
      referenceScriptItemBytes,
    }),
    ledgerTrie: async () => scenario.ledger.trie,
  },
  observations: {},
  journal: journal ?? (() => undefined),
  policy: DECODING_EMULATOR_PROVER_POLICY,
  referenceScriptUtxos,
  witnessReferenceScripts: harness.witnessReferenceScripts,
});

/**
 * Every step's spend redeemer shares the `Cancel` head; the raw builders
 * below never encode a `Continue` through this schema, so the argument
 * schema is irrelevant.
 */
const RawCancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());

export type RawCancelSpendRedeemer = Data.Static<
  typeof RawCancelSpendRedeemerSchema
>;

export const RawCancelSpendRedeemer = asDataType<RawCancelSpendRedeemer>(
  RawCancelSpendRedeemerSchema,
);

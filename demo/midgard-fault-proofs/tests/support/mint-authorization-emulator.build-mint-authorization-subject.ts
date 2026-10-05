import {
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { type Script, type UTxO } from "@lucid-evolution/lucid";

import type { MintAuthorizationContracts } from "../../src/mint-authorization/contracts.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
} from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { registerChunkedVerifyRewardAccount } from "./emulator/emulator-context.js";
import { publishFaultProofWitnessReferenceScripts } from "./emulator/reference-scripts.js";
import {
  fieldPreimageOf,
  type MintAuthorizationSubject,
} from "./mint-authorization-emulator.large-mint-item-cbors.js";
import {
  buildDecodingBlockFixture,
  buildDecodingLedgerFixture,
  type DecodingBlockFixture,
  type DecodingLedgerFixture,
} from "./native-script-decoding-emulator.js";
import { createExactRequestReusingScalusEvaluator } from "./scalus-exact-request-evaluator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

/**
 * Materialises the committed native transaction from its four door preimages.
 * `validity: "TxIsInvalid"` builds the §2.4.3(d) negative — an honestly
 * recorded no-op the family must never convict.
 */
export const buildMintAuthorizationSubject = ({
  mintItemCbors,
  scriptWitnessItemCbors = [],
  addrWitnessItemCbors = [],
  referenceInputItemCbors = [],
  validity = "TxIsValid",
}: {
  readonly mintItemCbors: readonly string[];
  readonly scriptWitnessItemCbors?: readonly string[];
  readonly addrWitnessItemCbors?: readonly string[];
  readonly referenceInputItemCbors?: readonly string[];
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): MintAuthorizationSubject => {
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: fieldPreimageOf(referenceInputItemCbors),
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: fieldPreimageOf(mintItemCbors),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: fieldPreimageOf(addrWitnessItemCbors),
      scriptTxWitsPreimageCbor: fieldPreimageOf(scriptWitnessItemCbors),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    nativeTx,
    witnessSetCompact: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
    mintItemCbors,
    scriptWitnessItemCbors,
    addrWitnessItemCbors,
    referenceInputItemCbors,
  };
};

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeMintAuthorizationEmulatorHarness = async ({
  useScalusEvaluator = true,
}: {
  /**
   * Scalus stays the default because two size-at-maximum cases
   * (`mint-authorization-maximum-lifecycle`, direction B of
   * `submit-init-emulator-mint-authorization-size-forced-carriage`) are
   * calibrated against its budget reading: the Aiken/WASM evaluator reports
   * the same spend roughly 3.5k memory units OVER `maxTxExMem`, so the two
   * evaluators disagree at the limit and the disagreement is an open owner
   * question, not something a harness default may paper over. Long
   * lifecycles that are not at the memory limit opt into Aiken/WASM, which
   * is 2–3× faster and, since `@lucid-evolution/uplc` 0.2.23, no longer
   * leaks its arena per evaluation.
   */
  readonly useScalusEvaluator?: boolean;
} = {}) => {
  const harness = await makeFaultProofEmulatorHarness({
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
    contractOptions: {
      realMintAuthorization: true,
      alwaysFraudProofCatalogue: false,
    },
    ...(useScalusEvaluator
      ? {
          lucidOptions: {
            evaluator: createExactRequestReusingScalusEvaluator(),
          },
        }
      : {}),
  });
  const family = harness.contracts.mintAuthorization;
  const category = harness.catalogue.categories.mintAuthorization;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the mint-authorization contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.mintAuthorization
  ) {
    throw new Error("Unexpected mint-authorization catalogue category id");
  }
  return { ...harness, family, category };
};

export type MintAuthorizationHarness = Awaited<
  ReturnType<typeof makeMintAuthorizationEmulatorHarness>
>;

export type MintAuthorizationScenario = {
  readonly subject: MintAuthorizationSubject;
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>> & {
    readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  };
};

/**
 * Commits a header whose transition trace carries the accused transaction as
 * an accepted L2 event and its dense transition step, ready for Init.
 */
export const setupMintAuthorizationScenario = async ({
  harness,
  subject,
  priorLedgerRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
  transformBlock,
}: {
  readonly harness: MintAuthorizationHarness;
  readonly subject: MintAuthorizationSubject;
  /** The block's pre-state ledger root; the step-04 ResolveNext trie root. */
  readonly priorLedgerRoot?: string;
  readonly transformBlock?: (
    block: DecodingBlockFixture,
  ) => Promise<DecodingBlockFixture>;
}): Promise<MintAuthorizationScenario> => {
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: harness.proverLucid,
      realBlueprint: harness.realBlueprint,
      computationThreadMintingScript:
        harness.family.computationThread.mintingScript,
      fraudProofMintingScript: harness.family.fraudProof.mintingScript,
    });
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  let block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
  if (transformBlock !== undefined) block = await transformBlock(block);
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return {
    subject,
    block,
    setup: { ...setup, witnessReferenceScripts },
  };
};

/** Publishes all five step validators as reference scripts (deployment shape). */
export const publishMintAuthorizationReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: MintAuthorizationContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `mint-authorization step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
};

// ---------------------------------------------------------------------------
// The step-04 ResolveNext pre-state ledger trie (one reference input)
// ---------------------------------------------------------------------------

/**
 * A pre-state ledger trie holding one scanned outpoint's descriptor under its
 * §5.3 38-byte key, its reference script hashing to something OTHER than the
 * accused policy so the absence claim holds. The block's `priorLedgerRoot`
 * must be set to the returned `rootHex`.
 */
export const buildMintAuthorizationLedgerFixture = async ({
  txIdHex,
  outputIndex,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
}): Promise<DecodingLedgerFixture> =>
  buildDecodingLedgerFixture({
    txIdHex,
    outputIndex,
    referenceScriptItemBytes: Buffer.from("a1b2c3d4", "hex"),
    referenceScriptLanguage: 0,
  });

// ---------------------------------------------------------------------------
// Raw guard-bypassing builder — a step-02 mint-door open against TAMPERED
// predeployed carriage, so the on-chain §8.8 commitment re-hash refuses. The
// honest builders locate publications BY CONTENT and can never reach tampered
// bytes, so the adversarial path constructs the RawUtxo opening directly.
// ---------------------------------------------------------------------------

/** Flips one byte of a field preimage — a publication the door must reject. */
export const tamperFieldPreimageBytes = (preimage: Buffer): Buffer => {
  const tampered = Buffer.from(preimage);
  const index = tampered.length - 1;
  tampered[index] = tampered[index]! ^ 0xff;
  return tampered;
};

/**
 * Publishes a bytes-only inline-datum UTxO carrying `bytes` at the prover's
 * own address, in the exact §8 publication datum shape the door reads, and
 * returns the confirmed UTxO. Used only to plant TAMPERED carriage.
 */
export const publishRawFieldPreimageCarriage = async ({
  lucid,
  signer,
  bytes,
}: {
  readonly lucid: MintAuthorizationHarness["proverLucid"];
  readonly signer: ResolvedProverSigner;
  readonly bytes: Buffer;
}): Promise<UTxO> => {
  signer.selectWallet(lucid);
  const datum = SDK.fieldPreimagePublicationDatumCbor(bytes);
  const { coinsPerUtxoByte } = await resolveProtocolParameters(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .pay.ToAddressWithData(
      signer.address,
      { kind: "inline", value: datum },
      {
        lovelace: minimumLovelaceForInlineDatumOutput({
          address: signer.address,
          datum,
          coinsPerUtxoByte,
        }),
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const published = (await lucid.utxosAt(signer.address)).find(
    (utxo) => utxo.datum === datum,
  );
  if (published === undefined) {
    throw new Error("raw tampered publication did not land at prover address");
  }
  return published;
};
